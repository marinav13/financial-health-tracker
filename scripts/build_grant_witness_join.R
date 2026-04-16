# =========================================================================
# build_grant_witness_join.R
# =========================================================================
#
# PURPOSE: Retrieve terminated/frozen federal research grants from Grant Witness
#          and match them to institutions in the financial tracker.
#
# DOMAIN: Grant witness = tracking disrupted (terminated/frozen) federal
#         research grants from 5 agencies: NIH, NSF, EPA, SAMHSA, CDC.
#         These disruptions indicate loss of research funding and can signal
#         institutional financial stress.
#
# INPUTS:
#   - Financial tracker CSV (latest year + historical data for trends)
#   - Grant Witness CSV files (downloaded from external service):
#     * nih_terminations.csv, nsf_terminations.csv, epa_terminations.csv,
#       samhsa_terminations.csv, cdc_terminations.csv
#   - Manual inclusion/override lists (optional, for curation):
#     * manual_include.csv, manual_match_overrides.csv
#   - USAspending sensitivity filter (output of build_grant_witness_usaspending_sensitivity.R)
#
# OUTPUTS:
#   - grant_witness_grant_level_joined.csv
#     (one row per grant with matched institution and financial metrics)
#   - grant_witness_institution_summary_long.csv
#     (institution-agency-level disrupted grant totals in long format)
#   - grant_witness_institution_summary.csv
#     (institution-level summary with agency columns pivoted wide)
#   - grant_witness_higher_ed_institution_summary.csv
#     (filtered to likely higher-ed institutions only)
#   - grant_witness_unmatched_for_review.csv
#     (unmatched organizations for manual curation)
#   - grant_witness_likely_higher_ed_unmatched_for_review.csv
#     (higher-ed-like unmatched organizations for easier review)
#   - grant_witness_excluded_pass_through_grants.csv
#     (grants filtered as pass-through/grantmaker organizations)
#   - grant_witness_excluded_risky_continuation_grants.csv
#     (grants filtered by USAspending sensitivity analysis)
#
# WORKFLOW:
#   1. Download or cache all 5 Grant Witness agency files
#   2. Load financial tracker and normalize institution names
#   3. Build multi-level matching lookups:
#     * Exact: norm_name + city + state
#     * Fallback 1: norm_name + state only
#     * Fallback 2: simplified_name + state (with alias variants)
#     * Manual overrides: institution-specific curation rules
#   4. Read and standardize all grant records (normalize fields, parse dates)
#   5. Apply matching cascade: city+state -> state -> aliases -> manual -> display name
#   6. Filter out obvious non-higher-ed and pass-through organizations
#   7. Filter out "risky continuation" grants (positive post-termination activity)
#   8. Build institution summaries (long and wide format)
#   9. Write outputs as CSVs
#
# KEY MATCHING STRATEGY:
#   Priority 1: City + normalized name + state (highest confidence)
#   Priority 2: Normalized name + state fallback
#   Priority 3: Alias name + state fallback (for institutions with variant names)
#   Priority 4: Manual name override (curator-specified matches)
#   Priority 5: Manual display name override (for display-name-only matches)
#   Priority 6: Manual include (high-confidence unmatched, curator-flagged)
#   Priority 7: Likely higher-ed unmatched (named similar to colleges)
#   Priority 8: Other unmatched (for review)
#
# DOMAIN CONCEPTS:
#   - status_bucket: Classifies grant status into comparable buckets across agencies
#   - currently_disrupted: TRUE if grant is currently in a terminated/frozen state
#   - pass_through_or_grantmaker: Grant where recipient organization passes money
#     to other entities (not direct research). These are filtered out.
#   - likely_higher_ed: Heuristic scoring based on institution name keywords
#   - award_remaining: Amount still available under the grant (not yet spent)

main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args          <- parse_cli_args(cli_args)
  ipeds         <- load_ipeds_paths()
  ipeds_layout  <- ipeds$ipeds_layout
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)
  has_flag      <- function(flag)                 arg_has(args, flag)

  ensure_packages(c("dplyr", "readr", "stringr", "tidyr"))

  # -----------------------------------------------------------------------
  # Parse command-line arguments
  # -----------------------------------------------------------------------

  financial_input <- get_arg_value(
    "--financial-input",
    ipeds_layout(root = ".")$dataset_csv
  )
  output_prefix <- get_arg_value(
    "--output-prefix",
    file.path(getwd(), "data_pipelines", "grant_witness", "grant_witness")
  )
  cache_dir <- get_arg_value(
    "--cache-dir",
    file.path(getwd(), "data_pipelines", "grant_witness", "cache")
  )
  manual_include_path <- get_arg_value(
    "--manual-include",
    file.path(getwd(), "data_pipelines", "grant_witness", "manual_include.csv")
  )
  manual_match_overrides_path <- get_arg_value(
    "--manual-match-overrides",
    file.path(getwd(), "data_pipelines", "grant_witness", "manual_match_overrides.csv")
  )
  usaspending_filter_path <- get_arg_value(
    "--usaspending-filter",
    file.path(getwd(), "data_pipelines", "grant_witness", "analysis", "grant_witness_usaspending_risky_continuation_filter.csv")
  )
  skip_download <- has_flag("--skip-download")
  skip_usaspending_filter <- has_flag("--skip-usaspending-filter")

  if (!file.exists(financial_input)) {
    stop("Financial input file not found: ", financial_input)
  }
  if (!skip_usaspending_filter && !file.exists(usaspending_filter_path)) {
    stop(
      "Risky continuation filter file not found: ",
      usaspending_filter_path,
      ". Run scripts/build_grant_witness_usaspending_sensitivity.R first."
    )
  }

  dir.create(dirname(output_prefix), recursive = TRUE, showWarnings = FALSE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # -----------------------------------------------------------------------
  # Define Grant Witness agency files to download
  # -----------------------------------------------------------------------
  # These are the raw Grant Witness agency files that feed the research-cuts
  # join. They are cached on disk so weekly refreshes can fall back cleanly.

  downloads <- c(
    nih = "https://files.grant-witness.us/nih_terminations.csv",
    nsf = "https://files.grant-witness.us/nsf_terminations.csv",
    epa = "https://files.grant-witness.us/epa_terminations.csv",
    samhsa = "https://files.grant-witness.us/samhsa_terminations.csv",
    cdc = "https://files.grant-witness.us/cdc_terminations.csv"
  )

  cached_paths <- file.path(cache_dir, paste0(names(downloads), "_terminations.csv"))
  names(cached_paths) <- names(downloads)

  # -----------------------------------------------------------------------
  # Load helper functions for text normalization and institution matching
  # -----------------------------------------------------------------------
  # The matching logic depends on very aggressive name normalization because
  # legal entity names, abbreviations, and punctuation vary across sources.

  source(file.path(getwd(), "scripts", "shared", "grant_witness_helpers.R"))
  # Text normalisation utilities (normalize_name, prettify_text,
  # simplify_institution_name, classify_status_bucket, maybe_download, etc.)
  # are in scripts/shared/grant_witness_helpers.R

  # -----------------------------------------------------------------------
  # Download all Grant Witness agency files (or skip if already cached)
  # -----------------------------------------------------------------------

  for (agency in names(downloads)) {
    maybe_download(downloads[[agency]], cached_paths[[agency]], skip_download = skip_download)
  }

  # -----------------------------------------------------------------------
  # Load financial tracker and prepare matching lookups
  # -----------------------------------------------------------------------

  message("Reading financial tracker data ...")
  financial_all <- readr::read_csv(financial_input, show_col_types = FALSE, progress = FALSE)
  latest_year <- suppressWarnings(max(financial_all$year, na.rm = TRUE))
  financial_latest <- financial_all |>
    dplyr::filter(year == latest_year) |>
    dplyr::transmute(
      unitid = as.character(unitid),
      tracker_institution_name = institution_name,
      tracker_city = prettify_location_text(city),
      tracker_state = prettify_location_text(as.character(state)),
      tracker_control_label = control_label,
      tracker_category = category,
      norm_name = normalize_name(institution_name),
      city_norm = normalize_city(city),
      state_full = prettify_location_text(as.character(state))
    )

  # Lookup table: Match by city + normalized name + state (highest confidence)
  city_lookup <- financial_latest |>
    dplyr::add_count(norm_name, state_full, city_norm, name = "candidate_count") |>
    dplyr::filter(!is.na(city_norm), city_norm != "", candidate_count == 1) |>
    dplyr::select(-candidate_count)

  # Fallback lookup: Match by normalized name + state only
  fallback_lookup <- financial_latest |>
    dplyr::add_count(norm_name, state_full, name = "candidate_count") |>
    dplyr::filter(candidate_count == 1) |>
    dplyr::select(-candidate_count)

  # -----------------------------------------------------------------------
  # Build institution name aliases (handle naming variations)
  # -----------------------------------------------------------------------
  # Each row maps a tracker institution name (regex) to an alternative name
  # that may appear in grant data. This allows matching despite punctuation,
  # abbreviations, or legal entity name differences.

  alias_seed <- financial_latest |>
    dplyr::transmute(
      unitid,
      tracker_institution_name,
      tracker_city,
      tracker_state,
      tracker_control_label,
      tracker_category,
      alias = tracker_institution_name
    )

  # Explicit aliases: institution-specific name variants known to appear in grants
  tracker_name_aliases <- tibble::tribble(
    ~tracker_pattern,                                         ~alias,
    "^University of Pittsburgh-Pittsburgh Campus$",           "University of Pittsburgh",
    "^University of Pittsburgh-Pittsburgh Campus$",           "University of Pittsburgh at Pittsburgh",
    "^University of Michigan-Ann Arbor$",                     "University of Michigan",
    "^University of Michigan-Ann Arbor$",                     "Regents of the University of Michigan - Ann Arbor",
    "^University of Michigan-Ann Arbor$",                     "Regents of the University of Michigan",
    "^Washington University in St Louis$",                    "Washington University",
    "^Washington University in St Louis$",                    "Washington University, The",
    "^Ohio State University-Main Campus$",                    "The Ohio State University",
    "^Ohio State University-Main Campus$",                    "Ohio State University, The",
    "^Boston University$",                                    "Trustees of Boston University",
    "^Harvard University$",                                   "President and Fellows of Harvard College",
    "^Virginia Polytechnic Institute and State University$",  "Virginia Polytechnic Inst and St Univ"
  )

  alias_core_cols <- c("unitid", "tracker_institution_name", "tracker_city",
                       "tracker_state", "tracker_control_label", "tracker_category")

  # Build alias variants: generic "-Main Campus" stripping + explicit aliases
  alias_variants <- dplyr::bind_rows(
    # Generic: strip "-Main Campus" suffix from all institution names
    financial_latest |>
      dplyr::transmute(
        dplyr::across(dplyr::all_of(alias_core_cols)),
        alias = stringr::str_replace(tracker_institution_name,
                                     regex("-[[:space:]]*Main Campus$", ignore_case = TRUE), "")
      ),
    # Explicit aliases from lookup table
    purrr::map_dfr(seq_len(nrow(tracker_name_aliases)), function(i) {
      matched <- financial_latest[
        stringr::str_detect(
          financial_latest$tracker_institution_name,
          stringr::regex(tracker_name_aliases$tracker_pattern[[i]], ignore_case = TRUE)
        ), ,
        drop = FALSE
      ]
      if (nrow(matched) == 0L) return(tibble::tibble())
      dplyr::transmute(matched,
        dplyr::across(dplyr::all_of(alias_core_cols)),
        alias = tracker_name_aliases$alias[[i]]
      )
    })
  )

  alias_lookup <- dplyr::bind_rows(alias_seed, alias_variants) |>
    dplyr::filter(!is.na(alias), trimws(alias) != "") |>
    dplyr::mutate(
      alias_norm = simplify_institution_name(alias),
      tracker_city = prettify_location_text(tracker_city),
      state_full = tracker_state
    ) |>
    dplyr::distinct(alias_norm, state_full, unitid, .keep_all = TRUE) |>
    dplyr::add_count(alias_norm, state_full, name = "candidate_count") |>
    dplyr::filter(candidate_count == 1) |>
    dplyr::select(-candidate_count) |>
    dplyr::distinct(alias_norm, state_full, .keep_all = TRUE)

  # -----------------------------------------------------------------------
  # Load manual inclusion/override lists for curator-driven curation
  # -----------------------------------------------------------------------

  # Manual include: List of organizations curator has flagged as "include in dataset"
  # (typically high-confidence matches that automated matching missed)
  manual_include <- if (file.exists(manual_include_path)) {
    readr::read_csv(manual_include_path, show_col_types = FALSE, progress = FALSE) |>
      dplyr::transmute(
        organization_name,
        organization_state = null_if_empty(organization_state),
        include_in_dataset = toupper(as.character(include_in_dataset %||% "TRUE")) == "TRUE"
      )
  } else {
    data.frame(
      organization_name = character(),
      organization_state = character(),
      include_in_dataset = logical(),
      stringsAsFactors = FALSE
    )
  }

  # Manual match overrides: Curator-specified UNITID matches for organization names
  manual_match_overrides <- if (file.exists(manual_match_overrides_path)) {
    readr::read_csv(manual_match_overrides_path, show_col_types = FALSE, progress = FALSE) |>
      dplyr::transmute(
        organization_name,
        organization_name_display = prettify_institution_name(organization_name),
        organization_state = null_if_empty(organization_state),
        override_unitid = as.character(unitid),
        override_tracker_institution_name = institution_name_override,
        override_tracker_state = state_override,
        override_tracker_control_label = control_label_override,
        override_tracker_category = category_override,
        override_likely_higher_ed = TRUE
      ) |>
      # Enrich with metadata from financial tracker
      dplyr::left_join(
        financial_latest |>
          dplyr::select(
            unitid,
            financial_tracker_institution_name = tracker_institution_name,
            financial_tracker_city = tracker_city,
            financial_tracker_state = tracker_state,
            financial_tracker_control_label = tracker_control_label,
            financial_tracker_category = tracker_category
          ),
        by = c("override_unitid" = "unitid")
      ) |>
      dplyr::mutate(
        override_tracker_institution_name = dplyr::coalesce(override_tracker_institution_name, financial_tracker_institution_name),
        override_tracker_city = financial_tracker_city,
        override_tracker_state = dplyr::coalesce(override_tracker_state, financial_tracker_state),
        override_tracker_control_label = dplyr::coalesce(override_tracker_control_label, financial_tracker_control_label),
        override_tracker_category = dplyr::coalesce(override_tracker_category, financial_tracker_category)
      ) |>
      dplyr::select(-dplyr::starts_with("financial_tracker_"))
  } else {
    data.frame(
      organization_name = character(),
      organization_name_display = character(),
      organization_state = character(),
      override_unitid = character(),
      override_tracker_institution_name = character(),
      override_tracker_city = character(),
      override_tracker_state = character(),
      override_tracker_control_label = character(),
      override_tracker_category = character(),
      override_likely_higher_ed = logical(),
      stringsAsFactors = FALSE
    )
  }

  # -----------------------------------------------------------------------
  # Helper: Read CSV with UTF-8 encoding
  # -----------------------------------------------------------------------

  read_csv_utf8 <- function(path) {
    readr::read_csv(
      path,
      show_col_types = FALSE,
      progress = FALSE,
      locale = readr::locale(encoding = "UTF-8")
    )
  }

  # -----------------------------------------------------------------------
  # Helper: Standardize and normalize each grant record
  # -----------------------------------------------------------------------
  # This function takes raw Grant Witness data and normalizes fields to
  # consistent types, formats, and naming. It extracts institution-relevant
  # information and computes heuristic fields like "likely_higher_ed".

  standardize_grants <- function(agency, df) {
    out <- standardize_grant_witness_rows(
      agency = agency,
      df = df,
      source_file_name = basename(cached_paths[[agency]])
    )

    out |>
      dplyr::mutate(
        organization_name = null_if_empty(organization_name),
        organization_state = prettify_location_text(null_if_empty(organization_state)),
        organization_city = prettify_location_text(null_if_empty(organization_city)),
        organization_type = null_if_empty(organization_type),
        project_title = null_if_empty(prettify_text(project_title)),
        project_abstract = null_if_empty(project_abstract),
        start_date = null_if_empty(start_date),
        original_end_date = null_if_empty(original_end_date),
        termination_date = null_if_empty(termination_date),
        reinstatement_date = null_if_empty(reinstatement_date),
        source_url = null_if_empty(source_url),
        detail_url = null_if_empty(detail_url),
        status_bucket = classify_status_bucket(agency, status),
        currently_disrupted = is_currently_disrupted(agency, status),
        organization_name_display = prettify_institution_name(organization_name),
        norm_name = normalize_name(organization_name),
        simplified_norm_name = simplify_institution_name(organization_name),
        organization_city_norm = normalize_city(organization_city),
        likely_higher_ed = is_likely_higher_ed_name(organization_name)
      )
  }

  # -----------------------------------------------------------------------
  # Read and standardize all Grant Witness files, apply matching cascade
  # -----------------------------------------------------------------------

  message("Reading and standardizing Grant Witness files ...")
  grants_joined <- dplyr::bind_rows(lapply(names(cached_paths), function(agency) {
    standardize_grants(agency, read_csv_utf8(cached_paths[[agency]]))
  })) |>
    # Apply matching priority 1: city + normalized name + state
    dplyr::left_join(
      city_lookup |>
        dplyr::rename(
          city_unitid = unitid,
          city_tracker_institution_name = tracker_institution_name,
          city_tracker_city = tracker_city,
          city_tracker_state = tracker_state,
          city_tracker_control_label = tracker_control_label,
          city_tracker_category = tracker_category
        ),
      by = c(
        "norm_name",
        "organization_state" = "state_full",
        "organization_city_norm" = "city_norm"
      )
    ) |>
    # Apply matching priority 2: normalized name + state fallback
    dplyr::left_join(
      fallback_lookup,
      by = c("norm_name", "organization_state" = "state_full")
    ) |>
    # Apply matching priority 3: alias name + state
    dplyr::left_join(
      alias_lookup |>
        dplyr::rename(
          alias_unitid = unitid,
          alias_tracker_institution_name = tracker_institution_name,
          alias_tracker_city = tracker_city,
          alias_tracker_state = tracker_state,
          alias_tracker_control_label = tracker_control_label,
          alias_tracker_category = tracker_category
        ),
      by = c("simplified_norm_name" = "alias_norm", "organization_state" = "state_full")
    ) |>
    # Apply matching priority 4: manual inclusion flag
    dplyr::left_join(
      manual_include,
      by = c("organization_name_display" = "organization_name", "organization_state")
    ) |>
    # Apply matching priority 5: manual name override
    dplyr::left_join(
      manual_match_overrides |>
        dplyr::select(-organization_name_display),
      by = c("organization_name", "organization_state")
    ) |>
    # Apply matching priority 6: manual display name override
    dplyr::left_join(
      manual_match_overrides |>
        dplyr::rename(
          display_override_unitid = override_unitid,
          display_override_tracker_institution_name = override_tracker_institution_name,
          display_override_tracker_city = override_tracker_city,
          display_override_tracker_state = override_tracker_state,
          display_override_tracker_control_label = override_tracker_control_label,
          display_override_tracker_category = override_tracker_category,
          display_override_likely_higher_ed = override_likely_higher_ed
        ) |>
        dplyr::select(
          organization_name_display,
          organization_state,
          display_override_unitid,
          display_override_tracker_institution_name,
          display_override_tracker_city,
          display_override_tracker_state,
          display_override_tracker_control_label,
          display_override_tracker_category,
          display_override_likely_higher_ed
        ),
      by = c("organization_name_display", "organization_state")
    ) |>
    # Coalesce all matching attempts into single columns
    dplyr::mutate(
      matched_unitid = dplyr::coalesce(city_unitid, unitid, alias_unitid, override_unitid, display_override_unitid),
      tracker_institution_name = dplyr::coalesce(city_tracker_institution_name, tracker_institution_name, alias_tracker_institution_name, override_tracker_institution_name, display_override_tracker_institution_name),
      tracker_city = dplyr::coalesce(city_tracker_city, tracker_city, alias_tracker_city, override_tracker_city, display_override_tracker_city),
      tracker_state = dplyr::coalesce(city_tracker_state, tracker_state, alias_tracker_state, override_tracker_state, display_override_tracker_state),
      tracker_control_label = dplyr::coalesce(city_tracker_control_label, tracker_control_label, alias_tracker_control_label, override_tracker_control_label, display_override_tracker_control_label),
      tracker_category = dplyr::coalesce(city_tracker_category, tracker_category, alias_tracker_category, override_tracker_category, display_override_tracker_category),
      likely_higher_ed = dplyr::if_else(
        is_noncampus_medical_or_foundation_name(organization_name),
        FALSE,
        dplyr::coalesce(override_likely_higher_ed, display_override_likely_higher_ed, include_in_dataset, likely_higher_ed)
      ),
      # Detect pass-through/grantmaker keywords in project title/abstract
      pass_through_keyword_match = vapply(
        seq_len(dplyr::n()),
        function(i) detect_pass_through_phrase(project_title[[i]], project_abstract[[i]]),
        character(1)
      ),
      # Extract award ID from USAspending URL (for cross-reference validation)
      award_id_string = stringr::str_match(source_url, "award/([^/?#]+)")[, 2],
      is_pass_through_or_grantmaker = !is.na(pass_through_keyword_match),
      # Record which matching method succeeded for this grant
      match_method = dplyr::case_when(
        !is.na(city_unitid) ~ "normalized_name_city_state",
        is.na(city_unitid) & !is.na(unitid) ~ "normalized_name_state_fallback",
        is.na(city_unitid) & is.na(unitid) & !is.na(alias_unitid) ~ "alias_name_state_fallback",
        is.na(city_unitid) & is.na(unitid) & is.na(alias_unitid) & !is.na(override_unitid) ~ "manual_name_override",
        is.na(city_unitid) & is.na(unitid) & is.na(alias_unitid) & is.na(override_unitid) & !is.na(display_override_unitid) ~ "manual_display_name_override",
        include_in_dataset %in% TRUE ~ "manual_include_unmatched",
        likely_higher_ed ~ "likely_higher_ed_unmatched",
        TRUE ~ "unmatched"
      ),
      in_financial_tracker = !is.na(matched_unitid)
    ) |>
    dplyr::select(
      agency,
      source_file,
      grant_id,
      grant_id_core,
      status,
      status_bucket,
      currently_disrupted,
      organization_name,
      organization_name_display,
      organization_state,
      organization_city,
      organization_type,
      project_title,
      project_abstract,
      is_pass_through_or_grantmaker,
      pass_through_keyword_match,
      start_date,
      original_end_date,
      termination_date,
      reinstatement_date,
      award_value,
      award_outlaid,
      award_remaining,
      remaining_field,
      award_id_string,
      source_url,
      detail_url,
      likely_higher_ed,
      matched_unitid,
      tracker_institution_name,
      tracker_city,
      tracker_state,
      tracker_control_label,
      tracker_category,
      match_method,
      in_financial_tracker
    )

  # -----------------------------------------------------------------------
  # Deduplication: Keep one best row per award/institution pair
  # -----------------------------------------------------------------------
  # This prevents inflated counts when a many-to-many join produces duplicates

  grants_joined <- grants_joined |>
    dplyr::mutate(
      match_priority = dplyr::case_when(
        match_method == "normalized_name_city_state" ~ 1L,
        match_method == "normalized_name_state_fallback" ~ 2L,
        match_method == "alias_name_state_fallback" ~ 3L,
        match_method == "manual_name_override" ~ 4L,
        match_method == "manual_display_name_override" ~ 5L,
        match_method == "manual_include_unmatched" ~ 6L,
        match_method == "likely_higher_ed_unmatched" ~ 7L,
        TRUE ~ 8L
      ),
      grant_match_key = dplyr::if_else(
        !is.na(matched_unitid) & trimws(matched_unitid) != "",
        paste(agency, grant_id, matched_unitid, sep = "|"),
        paste(agency, grant_id, organization_name_display, organization_state, sep = "|")
      )
    ) |>
    dplyr::arrange(match_priority) |>
    dplyr::group_by(grant_match_key) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(-match_priority, -grant_match_key) |>
    # Remove grants with negative remaining amount (fully spent)
    dplyr::filter(!(currently_disrupted & !is.na(award_remaining) & award_remaining <= 0))

  # -----------------------------------------------------------------------
  # Filter 1: Remove obvious pass-through/grantmaker awards
  # -----------------------------------------------------------------------
  # These are organizations receiving money to pass to others, not direct research.
  # Keep an audit file for review.

  excluded_pass_through_grants <- grants_joined |>
    dplyr::filter(currently_disrupted, is_pass_through_or_grantmaker) |>
    dplyr::arrange(dplyr::desc(award_remaining), organization_name_display, project_title, grant_id)

  grants_joined <- grants_joined |>
    dplyr::filter(!(currently_disrupted & is_pass_through_or_grantmaker))

  # -----------------------------------------------------------------------
  # Filter 2: Apply USAspending sensitivity filter
  # -----------------------------------------------------------------------
  # Proposal G exclusions are calculated in build_grant_witness_usaspending_sensitivity.R
  # These are grants showing positive post-termination continuation/revision activity,
  # suggesting money may still be flowing despite Grant Witness marking them disrupted.

  if (skip_usaspending_filter) {
    usaspending_filter_ids <- tibble::tibble(award_id_string = character())
    excluded_risky_continuation_grants <- grants_joined[0, ]
  } else {
    usaspending_filter_ids <- readr::read_csv(usaspending_filter_path, show_col_types = FALSE) |>
      dplyr::transmute(award_id_string = as.character(award_id_string)) |>
      dplyr::filter(!is.na(award_id_string), trimws(award_id_string) != "") |>
      dplyr::distinct()

    excluded_risky_continuation_grants <- grants_joined |>
      dplyr::filter(currently_disrupted, !is.na(award_id_string)) |>
      dplyr::semi_join(usaspending_filter_ids, by = "award_id_string") |>
      dplyr::arrange(dplyr::desc(award_remaining), organization_name_display, project_title, grant_id)

    grants_joined <- grants_joined |>
      dplyr::filter(!(currently_disrupted & !is.na(award_id_string) & award_id_string %in% usaspending_filter_ids$award_id_string))
  }

  # -----------------------------------------------------------------------
  # Build institution summary tables (after all grant-level exclusions)
  # -----------------------------------------------------------------------
  # This ensures that totals on the page and in downloads stay in sync.

  institution_summary_long <- grants_joined |>
    dplyr::filter(currently_disrupted, !is.na(organization_name), !is.na(organization_state)) |>
    dplyr::mutate(
      institution_key = dplyr::coalesce(
        paste0("unitid:", matched_unitid),
        paste0("name_state:", normalize_name(organization_name), "|", organization_state)
      ),
      display_name = dplyr::coalesce(tracker_institution_name, organization_name),
      display_name = prettify_institution_name(display_name),
      display_city = dplyr::coalesce(tracker_city, organization_city),
      display_city = prettify_location_text(display_city),
      display_state = dplyr::coalesce(tracker_state, organization_state)
    ) |>
    dplyr::group_by(
      institution_key,
      matched_unitid,
      display_name,
      display_city,
      display_state,
      tracker_control_label,
      tracker_category,
      agency
    ) |>
    dplyr::summarise(
      likely_higher_ed = any(likely_higher_ed, na.rm = TRUE),
      disrupted_grants = dplyr::n(),
      disrupted_award_remaining = sum(award_remaining, na.rm = TRUE),
      largest_single_grant_remaining = safe_max(award_remaining),
      .groups = "drop"
    ) |>
    dplyr::mutate(largest_single_grant_remaining = as.numeric(largest_single_grant_remaining))

  # Pivot institution summary to wide format (one row per institution)
  institution_summary_wide <- institution_summary_long |>
    dplyr::select(
      institution_key,
      matched_unitid,
      display_name,
      display_city,
      display_state,
      tracker_control_label,
      tracker_category,
      likely_higher_ed,
      agency,
      disrupted_grants,
      disrupted_award_remaining
    ) |>
    tidyr::pivot_wider(
      names_from = agency,
      values_from = c(disrupted_grants, disrupted_award_remaining),
      names_glue = "{agency}_{.value}",
      values_fill = 0
    )

  # Add total columns across agencies
  grant_cols <- grep("_disrupted_grants$", names(institution_summary_wide), value = TRUE)
  amount_cols <- grep("_disrupted_award_remaining$", names(institution_summary_wide), value = TRUE)
  if (length(grant_cols) > 0) {
    institution_summary_wide$total_disrupted_grants <- rowSums(institution_summary_wide[, grant_cols, drop = FALSE], na.rm = TRUE)
  } else {
    institution_summary_wide$total_disrupted_grants <- integer(nrow(institution_summary_wide))
  }
  if (length(amount_cols) > 0) {
    institution_summary_wide$total_disrupted_award_remaining <- rowSums(institution_summary_wide[, amount_cols, drop = FALSE], na.rm = TRUE)
  } else {
    institution_summary_wide$total_disrupted_award_remaining <- numeric(nrow(institution_summary_wide))
  }

  institution_summary_wide <- institution_summary_wide |>
    dplyr::arrange(dplyr::desc(total_disrupted_award_remaining), display_name)

  # -----------------------------------------------------------------------
  # Identify unmatched grants for manual review
  # -----------------------------------------------------------------------

  unmatched_for_review <- grants_joined |>
    dplyr::filter(currently_disrupted, !in_financial_tracker) |>
    dplyr::count(agency, organization_name = organization_name_display, organization_state, organization_city, organization_type, likely_higher_ed, match_method, wt = award_remaining, name = "disrupted_award_remaining") |>
    dplyr::arrange(dplyr::desc(disrupted_award_remaining), organization_name)

  # -----------------------------------------------------------------------
  # Prepare output file paths
  # -----------------------------------------------------------------------

  grant_path <- paste0(output_prefix, "_grant_level_joined.csv")
  summary_long_path <- paste0(output_prefix, "_institution_summary_long.csv")
  summary_path <- paste0(output_prefix, "_institution_summary.csv")
  higher_ed_summary_path <- paste0(output_prefix, "_higher_ed_institution_summary.csv")
  unmatched_path <- paste0(output_prefix, "_unmatched_for_review.csv")
  likely_higher_ed_unmatched_path <- paste0(output_prefix, "_likely_higher_ed_unmatched_for_review.csv")
  excluded_pass_through_path <- paste0(output_prefix, "_excluded_pass_through_grants.csv")
  excluded_risky_continuation_path <- paste0(output_prefix, "_excluded_risky_continuation_grants.csv")

  # -----------------------------------------------------------------------
  # Filter to higher-ed institutions for main outputs
  # -----------------------------------------------------------------------

  higher_ed_summary <- institution_summary_wide |>
    dplyr::filter(!is.na(matched_unitid) | likely_higher_ed)

  likely_higher_ed_unmatched <- unmatched_for_review |>
    dplyr::filter(likely_higher_ed)

  # Prepare review-ready summary for likely-higher-ed unmatched
  likely_higher_ed_review_ready <- likely_higher_ed_unmatched |>
    dplyr::group_by(organization_name, organization_state) |>
    dplyr::summarise(
      agencies = paste(sort(unique(stats::na.omit(as.character(agency)))), collapse = "; "),
      disrupted_award_remaining = sum(disrupted_award_remaining, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(disrupted_award_remaining), organization_name)

  # -----------------------------------------------------------------------
  # Safety check: ensure outputs are not empty
  # -----------------------------------------------------------------------

  if (nrow(grants_joined) == 0 || nrow(higher_ed_summary) == 0) {
    stop("Grant Witness refresh produced empty outputs; existing published files were left unchanged.")
  }

  # -----------------------------------------------------------------------
  # Write all CSV outputs
  # -----------------------------------------------------------------------

  write_csv_atomic(grants_joined, grant_path)
  write_csv_atomic(institution_summary_long, summary_long_path)
  write_csv_atomic(institution_summary_wide, summary_path)
  write_csv_atomic(higher_ed_summary, higher_ed_summary_path)
  write_csv_atomic(unmatched_for_review, unmatched_path)
  write_csv_atomic(likely_higher_ed_unmatched, likely_higher_ed_unmatched_path)
  write_csv_atomic(likely_higher_ed_review_ready, paste0(output_prefix, "_likely_higher_ed_review_ready.csv"))
  write_csv_atomic(excluded_pass_through_grants, excluded_pass_through_path)
  write_csv_atomic(excluded_risky_continuation_grants, excluded_risky_continuation_path)

  # -----------------------------------------------------------------------
  # Log completion
  # -----------------------------------------------------------------------

  cat(sprintf("Saved grant-level data to %s\n", grant_path))
  cat(sprintf("Saved institution summary (long) to %s\n", summary_long_path))
  cat(sprintf("Saved institution summary to %s\n", summary_path))
  cat(sprintf("Saved higher-ed institution summary to %s\n", higher_ed_summary_path))
  cat(sprintf("Saved unmatched review file to %s\n", unmatched_path))
  cat(sprintf("Saved likely higher-ed unmatched review file to %s\n", likely_higher_ed_unmatched_path))
  cat(sprintf("Saved likely higher-ed review-ready file to %s\n", paste0(output_prefix, "_likely_higher_ed_review_ready.csv")))
  cat(sprintf("Saved excluded pass-through grants file to %s\n", excluded_pass_through_path))
  cat(sprintf("Saved excluded risky continuation grants file to %s\n", excluded_risky_continuation_path))
}

if (sys.nframe() == 0) {
  main()
}
