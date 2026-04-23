# =========================================================================
# build_grant_witness_join.R
# =========================================================================
#
# PURPOSE: Retrieve terminated/frozen federal research grants from Grant Witness
#          and match them to institutions in the financial tracker.
#
# DOMAIN: Grant witness = tracking disrupted (terminated/frozen) federal
#         research grants from 5 agencies: NIH, NSF, EPA, SAMHSA, CDC.
#
# INPUTS:
#   - Financial tracker CSV
#   - Grant Witness CSV files (nih, nsf, epa, samhsa, cdc terminations)
#   - Manual inclusion/override lists (optional)
#   - Manual amount corrections from live USAspending spot checks (optional)
#   - USAspending sensitivity filter
#
# OUTPUTS:
#   - grant_witness_grant_level_joined.csv
#   - grant_witness_institution_summary_long.csv
#   - grant_witness_institution_summary.csv
#   - grant_witness_higher_ed_institution_summary.csv
#   - grant_witness_unmatched_for_review.csv
#   - grant_witness_likely_higher_ed_unmatched_for_review.csv
#   - grant_witness_excluded_pass_through_grants.csv
#   - grant_witness_excluded_risky_continuation_grants.csv
#
# WORKFLOW:
#   1. Download or cache all 5 Grant Witness agency files
#   2. Load financial tracker and normalize institution names
#   3. Build multi-level matching lookups
#   4. Read and standardize all grant records
#   5. Apply matching cascade
#   6. Filter out non-higher-ed and pass-through organizations
#   7. Filter out "risky continuation" grants
#   8. Build institution summaries
#   9. Write outputs as CSVs

main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args          <- parse_cli_args(cli_args)
  ipeds         <- load_ipeds_paths()
  ipeds_layout  <- ipeds$ipeds_layout
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)
  has_flag      <- function(flag)                 arg_has(args, flag)

  ensure_packages(c("dplyr", "readr", "stringr", "tidyr"))

  # -----------------------------------------------------------------------
  # PARSE COMMAND-LINE ARGUMENTS
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
  amount_corrections_path <- get_arg_value(
    "--amount-corrections",
    file.path(getwd(), "data_pipelines", "grant_witness", "manual_amount_corrections.csv")
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
  # DEFINE GRANT WITNESS AGENCY FILES
  downloads <- c(
    nih = "https://files.grant-witness.us/nih_terminations.csv",
    nsf = "https://files.grant-witness.us/nsf_terminations.csv",
    epa = "https://files.grant-witness.us/epa_terminations.csv",
    samhsa = "https://files.grant-witness.us/samhsa_terminations.csv",
    cdc = "https://files.grant-witness.us/cdc_terminations.csv"
  )

  cached_paths <- file.path(cache_dir, paste0(names(downloads), "_terminations.csv"))
  names(cached_paths) <- names(downloads)

  source(file.path(getwd(), "scripts", "shared", "grant_witness_helpers.R"))

  # -----------------------------------------------------------------------
  # DOWNLOAD ALL GRANT WITNESS AGENCY FILES
  for (agency in names(downloads)) {
    maybe_download(downloads[[agency]], cached_paths[[agency]], skip_download = skip_download)
  }

  # -----------------------------------------------------------------------
  # LOAD FINANCIAL TRACKER AND PREPARE MATCHING LOOKUPS
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

  # Lookup table: Match by city + normalized name + state
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
  # BUILD INSTITUTION NAME ALIASES
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

  # Explicit aliases for institutions with variant names in grant data
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

  # Build alias variants: strip "-Main Campus" + explicit aliases
  alias_variants <- dplyr::bind_rows(
    financial_latest |>
      dplyr::transmute(
        dplyr::across(dplyr::all_of(alias_core_cols)),
        alias = stringr::str_replace(tracker_institution_name,
                                     regex("-[[:space:]]*Main Campus$", ignore_case = TRUE), "")
      ),
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
  # LOAD MANUAL INCLUSION/OVERRIDE LISTS
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

  manual_match_overrides <- if (file.exists(manual_match_overrides_path)) {
    mo_raw <- readr::read_csv(manual_match_overrides_path, show_col_types = FALSE, progress = FALSE)
    if (!"organization_city" %in% names(mo_raw)) {
      mo_raw$organization_city <- NA_character_
    }
    mo <- mo_raw |>
      dplyr::transmute(
        organization_name,
        organization_name_display = prettify_institution_name(organization_name),
        organization_state = null_if_empty(organization_state),
        organization_city = prettify_location_text(null_if_empty(organization_city)),
        override_unitid = as.character(unitid),
        override_tracker_institution_name = institution_name_override,
        override_tracker_state = state_override,
        override_tracker_control_label = control_label_override,
        override_tracker_category = category_override,
        override_likely_higher_ed = TRUE
      ) |>
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

    # Validate: check for stale override unitids not in current IPEDS data
    stale_overrides <- mo |>
      dplyr::filter(!is.na(override_unitid), !override_unitid %in% financial_latest$unitid)
    if (nrow(stale_overrides) > 0) {
      warning(sprintf("%d manual override unitid(s) not found in current IPEDS data: %s",
        nrow(stale_overrides), paste(stale_overrides$override_unitid, collapse=", ")))
    }
    mo
  } else {
    data.frame(
      organization_name = character(),
      organization_name_display = character(),
      organization_state = character(),
      organization_city = character(),
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

  amount_corrections <- if (file.exists(amount_corrections_path)) {
    ac_raw <- readr::read_csv(
      amount_corrections_path,
      col_types = readr::cols(.default = readr::col_character()),
      progress = FALSE
    )
    if (!"award_id_string" %in% names(ac_raw)) {
      stop("Amount corrections file must include award_id_string: ", amount_corrections_path)
    }

    duplicate_corrections <- ac_raw |>
      dplyr::mutate(award_id_string = trimws(as.character(award_id_string))) |>
      dplyr::filter(!is.na(award_id_string), award_id_string != "") |>
      dplyr::count(award_id_string, name = "n") |>
      dplyr::filter(n > 1)
    if (nrow(duplicate_corrections) > 0) {
      stop(
        "Duplicate award_id_string values in amount corrections file: ",
        paste(duplicate_corrections$award_id_string, collapse = ", ")
      )
    }

    pick_correction_col <- function(df, col) {
      if (col %in% names(df)) as.character(df[[col]]) else rep(NA_character_, nrow(df))
    }

    ac_raw |>
      dplyr::transmute(
        award_id_string = trimws(as.character(award_id_string)),
        correction_award_value = suppressWarnings(as.numeric(pick_correction_col(ac_raw, "award_value"))),
        correction_award_outlaid = suppressWarnings(as.numeric(pick_correction_col(ac_raw, "award_outlaid"))),
        correction_award_remaining = suppressWarnings(as.numeric(pick_correction_col(ac_raw, "award_remaining"))),
        correction_remaining_field = dplyr::na_if(trimws(pick_correction_col(ac_raw, "remaining_field")), "")
      ) |>
      dplyr::filter(!is.na(award_id_string), award_id_string != "")
  } else {
    data.frame(
      award_id_string = character(),
      correction_award_value = numeric(),
      correction_award_outlaid = numeric(),
      correction_award_remaining = numeric(),
      correction_remaining_field = character(),
      stringsAsFactors = FALSE
    )
  }

  # -----------------------------------------------------------------------
  # HELPER: Read CSV with UTF-8 encoding
  read_csv_utf8 <- function(path) {
    readr::read_csv(
      path,
      show_col_types = FALSE,
      progress = FALSE,
      locale = readr::locale(encoding = "UTF-8")
    )
  }

  # -----------------------------------------------------------------------
  # HELPER: Standardize and normalize each grant record
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
  # READ AND STANDARDIZE ALL GRANT WITNESS FILES
  message("Reading and standardizing Grant Witness files ...")
  grants_joined <- dplyr::bind_rows(lapply(names(cached_paths), function(agency) {
    standardize_grants(agency, read_csv_utf8(cached_paths[[agency]]))
  })) |>
    # Matching priority 1: city + normalized name + state
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
    # Matching priority 2: normalized name + state fallback
    dplyr::left_join(
      fallback_lookup,
      by = c("norm_name", "organization_state" = "state_full")
    ) |>
    # Matching priority 3: alias name + state
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
    # Matching priority 4: manual inclusion flag
    dplyr::left_join(
      manual_include,
      by = c("organization_name_display" = "organization_name", "organization_state")
    ) |>
    # Matching priority 5: manual name override
    dplyr::left_join(
      manual_match_overrides |>
        dplyr::filter(!is.na(organization_city)) |>
        dplyr::distinct(organization_name, organization_state, organization_city, .keep_all = TRUE) |>
        dplyr::select(
          organization_name,
          organization_state,
          organization_city,
          city_override_unitid = override_unitid,
          city_override_tracker_institution_name = override_tracker_institution_name,
          city_override_tracker_city = override_tracker_city,
          city_override_tracker_state = override_tracker_state,
          city_override_tracker_control_label = override_tracker_control_label,
          city_override_tracker_category = override_tracker_category,
          city_override_likely_higher_ed = override_likely_higher_ed
        ),
      by = c("organization_name", "organization_state", "organization_city")
    ) |>
    dplyr::left_join(
      manual_match_overrides |>
        dplyr::filter(is.na(organization_city)) |>
        dplyr::distinct(organization_name, organization_state, .keep_all = TRUE) |>
        dplyr::select(-organization_name_display, -organization_city),
      by = c("organization_name", "organization_state")
    ) |>
    # Matching priority 6: manual display name override
    dplyr::left_join(
      manual_match_overrides |>
        dplyr::filter(!is.na(organization_city)) |>
        dplyr::distinct(organization_name_display, organization_state, organization_city, .keep_all = TRUE) |>
        dplyr::rename(
          display_city_override_unitid = override_unitid,
          display_city_override_tracker_institution_name = override_tracker_institution_name,
          display_city_override_tracker_city = override_tracker_city,
          display_city_override_tracker_state = override_tracker_state,
          display_city_override_tracker_control_label = override_tracker_control_label,
          display_city_override_tracker_category = override_tracker_category,
          display_city_override_likely_higher_ed = override_likely_higher_ed
        ) |>
        dplyr::select(
          organization_name_display,
          organization_state,
          organization_city,
          display_city_override_unitid,
          display_city_override_tracker_institution_name,
          display_city_override_tracker_city,
          display_city_override_tracker_state,
          display_city_override_tracker_control_label,
          display_city_override_tracker_category,
          display_city_override_likely_higher_ed
        ),
      by = c("organization_name_display", "organization_state", "organization_city")
    ) |>
    dplyr::left_join(
      manual_match_overrides |>
        dplyr::filter(is.na(organization_city)) |>
        dplyr::distinct(organization_name_display, organization_state, .keep_all = TRUE) |>
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
      matched_unitid = dplyr::coalesce(city_unitid, unitid, alias_unitid, city_override_unitid, override_unitid, display_city_override_unitid, display_override_unitid),
      tracker_institution_name = dplyr::coalesce(city_tracker_institution_name, tracker_institution_name, alias_tracker_institution_name, city_override_tracker_institution_name, override_tracker_institution_name, display_city_override_tracker_institution_name, display_override_tracker_institution_name),
      tracker_city = dplyr::coalesce(city_tracker_city, tracker_city, alias_tracker_city, city_override_tracker_city, override_tracker_city, display_city_override_tracker_city, display_override_tracker_city),
      tracker_state = dplyr::coalesce(city_tracker_state, tracker_state, alias_tracker_state, city_override_tracker_state, override_tracker_state, display_city_override_tracker_state, display_override_tracker_state),
      tracker_control_label = dplyr::coalesce(city_tracker_control_label, tracker_control_label, alias_tracker_control_label, city_override_tracker_control_label, override_tracker_control_label, display_city_override_tracker_control_label, display_override_tracker_control_label),
      tracker_category = dplyr::coalesce(city_tracker_category, tracker_category, alias_tracker_category, city_override_tracker_category, override_tracker_category, display_city_override_tracker_category, display_override_tracker_category),
      likely_higher_ed = dplyr::if_else(
        is_noncampus_medical_or_foundation_name(organization_name),
        FALSE,
        dplyr::coalesce(city_override_likely_higher_ed, override_likely_higher_ed, display_city_override_likely_higher_ed, display_override_likely_higher_ed, include_in_dataset, likely_higher_ed)
      ),
      pass_through_keyword_match = vapply(
        seq_len(dplyr::n()),
        function(i) detect_pass_through_phrase(project_title[[i]], project_abstract[[i]]),
        character(1)
      ),
      award_id_string = stringr::str_match(source_url, "award/([^/?#]+)")[, 2],
      is_pass_through_or_grantmaker = !is.na(pass_through_keyword_match),
      match_method = dplyr::case_when(
        !is.na(city_unitid) ~ "normalized_name_city_state",
        is.na(city_unitid) & !is.na(unitid) ~ "normalized_name_state_fallback",
        is.na(city_unitid) & is.na(unitid) & !is.na(alias_unitid) ~ "alias_name_state_fallback",
        is.na(city_unitid) & is.na(unitid) & is.na(alias_unitid) & !is.na(city_override_unitid) ~ "manual_name_city_override",
        is.na(city_unitid) & is.na(unitid) & is.na(alias_unitid) & is.na(city_override_unitid) & !is.na(override_unitid) ~ "manual_name_override",
        is.na(city_unitid) & is.na(unitid) & is.na(alias_unitid) & is.na(city_override_unitid) & is.na(override_unitid) & !is.na(display_city_override_unitid) ~ "manual_display_name_city_override",
        is.na(city_unitid) & is.na(unitid) & is.na(alias_unitid) & is.na(city_override_unitid) & is.na(override_unitid) & is.na(display_city_override_unitid) & !is.na(display_override_unitid) ~ "manual_display_name_override",
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

  if (nrow(amount_corrections) > 0) {
    grants_joined <- grants_joined |>
      dplyr::left_join(amount_corrections, by = "award_id_string") |>
      dplyr::mutate(
        award_value = dplyr::coalesce(correction_award_value, award_value),
        award_outlaid = dplyr::coalesce(correction_award_outlaid, award_outlaid),
        award_remaining = dplyr::coalesce(correction_award_remaining, award_remaining),
        remaining_field = dplyr::coalesce(correction_remaining_field, remaining_field)
      ) |>
      dplyr::select(
        -correction_award_value,
        -correction_award_outlaid,
        -correction_award_remaining,
        -correction_remaining_field
      )
  }

  # -----------------------------------------------------------------------
  # DEDUPLICATION: Keep one best row per award/institution pair
  grants_joined <- grants_joined |>
    dplyr::mutate(
      match_priority = dplyr::case_when(
        match_method == "normalized_name_city_state" ~ 1L,
        match_method == "normalized_name_state_fallback" ~ 2L,
        match_method == "alias_name_state_fallback" ~ 3L,
        match_method == "manual_name_city_override" ~ 4L,
        match_method == "manual_name_override" ~ 5L,
        match_method == "manual_display_name_city_override" ~ 6L,
        match_method == "manual_display_name_override" ~ 7L,
        match_method == "manual_include_unmatched" ~ 8L,
        match_method == "likely_higher_ed_unmatched" ~ 9L,
        TRUE ~ 10L
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
  # FILTER 1: Remove pass-through/grantmaker awards
  excluded_pass_through_grants <- grants_joined |>
    dplyr::filter(currently_disrupted, is_pass_through_or_grantmaker) |>
    dplyr::arrange(dplyr::desc(award_remaining), organization_name_display, project_title, grant_id)

  grants_joined <- grants_joined |>
    dplyr::filter(!(currently_disrupted & is_pass_through_or_grantmaker))

  # -----------------------------------------------------------------------
  # FILTER 2: Apply USAspending sensitivity filter
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
  # BUILD INSTITUTION SUMMARY TABLES
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

  # Pivot to wide format (one row per institution)
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
  # IDENTIFY UNMATCHED GRANTS FOR MANUAL REVIEW
  unmatched_for_review <- grants_joined |>
    dplyr::filter(currently_disrupted, !in_financial_tracker) |>
    dplyr::count(agency, organization_name = organization_name_display, organization_state, organization_city, organization_type, likely_higher_ed, match_method, wt = award_remaining, name = "disrupted_award_remaining") |>
    dplyr::arrange(dplyr::desc(disrupted_award_remaining), organization_name)

  # -----------------------------------------------------------------------
  # PREPARE OUTPUT FILE PATHS
  grant_path <- paste0(output_prefix, "_grant_level_joined.csv")
  summary_long_path <- paste0(output_prefix, "_institution_summary_long.csv")
  summary_path <- paste0(output_prefix, "_institution_summary.csv")
  higher_ed_summary_path <- paste0(output_prefix, "_higher_ed_institution_summary.csv")
  unmatched_path <- paste0(output_prefix, "_unmatched_for_review.csv")
  likely_higher_ed_unmatched_path <- paste0(output_prefix, "_likely_higher_ed_unmatched_for_review.csv")
  excluded_pass_through_path <- paste0(output_prefix, "_excluded_pass_through_grants.csv")
  excluded_risky_continuation_path <- paste0(output_prefix, "_excluded_risky_continuation_grants.csv")

  # -----------------------------------------------------------------------
  # FILTER TO HIGHER-ED INSTITUTIONS FOR MAIN OUTPUTS
  higher_ed_summary <- institution_summary_wide |>
    dplyr::filter(!is.na(matched_unitid) | likely_higher_ed)

  likely_higher_ed_unmatched <- unmatched_for_review |>
    dplyr::filter(likely_higher_ed)

  likely_higher_ed_review_ready <- likely_higher_ed_unmatched |>
    dplyr::group_by(organization_name, organization_state) |>
    dplyr::summarise(
      agencies = paste(sort(unique(stats::na.omit(as.character(agency)))), collapse = "; "),
      disrupted_award_remaining = sum(disrupted_award_remaining, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(disrupted_award_remaining), organization_name)

  # -----------------------------------------------------------------------
  # SAFETY CHECK
  if (nrow(grants_joined) == 0 || nrow(higher_ed_summary) == 0) {
    stop("Grant Witness refresh produced empty outputs; existing published files were left unchanged.")
  }

  # -----------------------------------------------------------------------
  # WRITE ALL CSV OUTPUTS
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
  # LOG COMPLETION
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
