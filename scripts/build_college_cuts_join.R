# =========================================================================
# build_college_cuts_join.R
# =========================================================================
#
# PURPOSE: Retrieve college cuts/layoffs/closures from the public
#          College Cuts API (college-cuts.com/api/cuts) and match them
#          to the financial tracker dataset.
#
# DOMAIN: College cuts = layoffs, program cuts, hiring freezes, campus/department
#         closures, and institution closures announced by universities.
#
# INPUTS:
#   - Financial tracker CSV (latest year data with institution info and metrics)
#   - College Cuts public API: GET /api/cuts?status=confirmed (no auth required)
#
# OUTPUTS:
#   - college_cuts_financial_tracker_cut_level_joined.csv
#   - college_cuts_financial_tracker_institution_summary.csv
#   - college_cuts_financial_tracker_financial_trends.csv
#   - college_cuts_financial_tracker_unmatched_for_review.csv
#   - college_cuts_financial_tracker_workbook.xlsx
#
# WORKFLOW:
#   1. Fetch all confirmed cuts from the public API (paginated)
#   2. Load financial tracker and normalize institution names
#   3. Build matching lookup tables (fuzzy name+state match)
#   4. Join cuts to financial tracker
#   5. Calculate financial_warning_count (6 red flags in latest year)
#   6. Build institution-level summary
#   7. Extract financial trends for all matched institutions
#   8. Generate review file for unmatched cuts
#   9. Write outputs as CSVs and Excel workbook
#
# MATCHING STRATEGY:
#   Priority 1: Normalized name + state fallback
#   Priority 2: Unmatched (flagged for manual review)

main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args          <- parse_cli_args(cli_args)
  ipeds         <- load_ipeds_paths()
  ipeds_layout  <- ipeds$ipeds_layout
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)

  ensure_packages(c("dplyr", "httr2", "openxlsx", "purrr", "readr", "stringr", "tidyr"))

  # -----------------------------------------------------------------------
  # PARSE COMMAND-LINE ARGUMENTS
  financial_input <- get_arg_value(
    "--financial-input",
    ipeds_layout(root = ".")$dataset_csv
  )
  output_prefix <- get_arg_value(
    "--output-prefix",
    file.path(getwd(), "data_pipelines", "college_cuts", "college_cuts_financial_tracker")
  )

  if (!file.exists(financial_input)) {
    stop("Financial input file not found: ", financial_input)
  }
  dir.create(dirname(output_prefix), recursive = TRUE, showWarnings = FALSE)

  # -----------------------------------------------------------------------
  # HELPER: Convert state abbreviations to full names
  state_lookup <- c(
    setNames(state.name, state.abb),
    DC = "District of Columbia",
    PR = "Puerto Rico",
    VI = "Virgin Islands",
    GU = "Guam",
    MP = "Northern Mariana Islands",
    AS = "American Samoa",
    PW = "Palau",
    FM = "Federated States of Micronesia",
    MH = "Marshall Islands"
  )

  abbr_to_state <- function(x) {
    x_chr <- as.character(x)
    out <- unname(state_lookup[toupper(x_chr)])
    out[is.na(out)] <- x_chr[is.na(out)]
    out
  }

  # -----------------------------------------------------------------------
  # HELPER: Normalize institution names for fuzzy matching
  # Strips common IPEDS name decorations so API names match IPEDS entries:
  #   - Leading "The " and "SUNY " (IPEDS adds these; API omits them)
  #   - Trailing " Main Campus" (IPEDS appends this; API uses the bare name)
  # Examples:
  #   "Ohio University-Main Campus"   → "ohio university"
  #   "The University of Texas at Dallas" → "university of texas at dallas"
  #   "SUNY Buffalo State University"     → "buffalo state university"
  normalize_name <- function(x) {
    x |>
      as.character() |>
      stringr::str_to_lower() |>
      stringr::str_remove("^the +") |>
      stringr::str_remove("^suny +") |>
      stringr::str_replace_all("&", " and ") |>
      stringr::str_replace_all("[^a-z0-9 ]", " ") |>
      stringr::str_remove("\\s+main campus$") |>
      # Expand "st" abbreviation so "College of St. Scholastica" matches
      # "The College of Saint Scholastica" in IPEDS.
      stringr::str_replace_all("\\bst\\b", "saint") |>
      stringr::str_squish()
  }

  # -----------------------------------------------------------------------
  # HELPER: Sum values, return NA if all inputs are NA
  safe_sum <- function(x) {
    if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  }

  # -----------------------------------------------------------------------
  # LOAD FINANCIAL TRACKER AND PREPARE FOR MATCHING
  message("Reading financial tracker data ...")
  financial_all <- readr::read_csv(financial_input, show_col_types = FALSE, progress = FALSE)
  latest_year <- suppressWarnings(max(financial_all$year, na.rm = TRUE))
  financial_latest <- financial_all |>
    dplyr::filter(year == latest_year) |>
    dplyr::mutate(
      norm_name = normalize_name(institution_name),
      state_full = as.character(state)
    )

  # Build fallback lookup: unique normalized-name + state → unitid.
  # Only keeps entries where the name+state pair is unambiguous (one candidate).
  fallback_lookup <- financial_latest |>
    dplyr::transmute(
      unitid_candidate = unitid,
      fallback_tracker_institution_name = institution_name,
      norm_name,
      state_full
    ) |>
    dplyr::add_count(norm_name, state_full, name = "candidate_count") |>
    dplyr::filter(candidate_count == 1) |>
    dplyr::select(-candidate_count)

  # ---------------------------------------------------------------------------
  # PRIMARY LOOKUP: Supabase-exported institution name → unitid mapping
  # ---------------------------------------------------------------------------
  # This CSV is generated by scripts/import_supabase_institution_mapping.py.
  # It maps the API institution name (as it appears in college-cuts.com) directly
  # to an IPEDS unitid, bypassing name-normalization entirely for known schools.
  # Run the import script whenever new institutions are added to Supabase.
  supabase_mapping_path <- file.path(getwd(), "data_pipelines", "college_cuts",
                                     "supabase_institution_unitid_mapping.csv")
  supabase_lookup <- if (file.exists(supabase_mapping_path)) {
    raw <- readr::read_csv(supabase_mapping_path, show_col_types = FALSE, progress = FALSE)
    if (nrow(raw) > 0 &&
        all(c("institution_name_api", "unitid", "state_full",
              "tracker_institution_name") %in% names(raw))) {
      message("  Using Supabase mapping: ", nrow(raw), " institutions")
      raw |>
        dplyr::filter(!is.na(unitid), nzchar(institution_name_api)) |>
        dplyr::transmute(
          norm_name  = normalize_name(institution_name_api),
          state_full = as.character(state_full),
          unitid_candidate = as.integer(unitid),
          fallback_tracker_institution_name = tracker_institution_name
        )
    } else {
      message("  Supabase mapping file empty or wrong schema — skipping")
      tibble::tibble(norm_name = character(), state_full = character(),
                     unitid_candidate = integer(),
                     fallback_tracker_institution_name = character())
    }
  } else {
    message("  No Supabase mapping file found at ", supabase_mapping_path)
    tibble::tibble(norm_name = character(), state_full = character(),
                   unitid_candidate = integer(),
                   fallback_tracker_institution_name = character())
  }

  # Manual overrides for API names that can't be auto-matched by name normalization.
  # The API often uses shorter names than IPEDS (e.g. "Columbia University" vs
  # "Columbia University in the City of New York"). These overrides map the
  # normalized API name + state to the correct IPEDS unitid.
  # These supplement the lookup; existing entries are not replaced.
  manual_aliases <- tibble::tibble(
    norm_name  = c(
      # Texas
      "texas a and m university",
      "university of texas hsch",
      "university of texas health science center at houston",
      # New York
      "columbia university",
      # Indiana (API uses bare system name → map to flagship)
      "indiana university",
      # New Jersey (API uses bare system name → map to flagship)
      "rutgers university",
      # Rhode Island
      "johnson and wales university",
      # California
      "california polytechnic saint university",   # after st→saint expansion
      "california polytechnic state university",   # alternate without expansion
      # Washington
      "washington state university",
      # Tennessee
      "vanderbilt university",
      # Minnesota (API "University of Minnesota" → Twin Cities flagship)
      "university of minnesota",
      # Oklahoma (API uses bare name, IPEDS appends "-Norman Campus")
      "university of oklahoma"
    ),
    state_full = c(
      "Texas", "Texas", "Texas",
      "New York",
      "Indiana",
      "New Jersey",
      "Rhode Island",
      "California", "California",
      "Washington",
      "Tennessee",
      "Minnesota",
      "Oklahoma"
    ),
    unitid_candidate = c(
      228723L, 229300L, 229300L,
      190150L,
      151351L,
      186380L,
      217235L,
      110422L, 110422L,
      236939L,
      221999L,
      174066L,
      207500L
    ),
    fallback_tracker_institution_name = c(
      "Texas A&M University-College Station",
      "The University of Texas Health Science Center at Houston",
      "The University of Texas Health Science Center at Houston",
      "Columbia University in the City of New York",
      "Indiana University-Bloomington",
      "Rutgers University-New Brunswick",
      "Johnson & Wales University-Providence",
      "California Polytechnic State University-San Luis Obispo",
      "California Polytechnic State University-San Luis Obispo",
      "Washington State University",
      "Vanderbilt University",
      "University of Minnesota-Twin Cities",
      "University of Oklahoma-Norman Campus"
    )
  )
  fallback_lookup <- dplyr::bind_rows(
    fallback_lookup,
    manual_aliases |>
      dplyr::anti_join(fallback_lookup, by = c("norm_name", "state_full"))
  )

  # Wire in Supabase mapping as the highest-priority matching source.
  # Supabase rows shadow any IPEDS name-normalization match for the same
  # (norm_name, state_full) pair, ensuring curated unitids are preferred.
  if (nrow(supabase_lookup) > 0) {
    fallback_lookup <- dplyr::bind_rows(
      supabase_lookup,
      fallback_lookup |>
        dplyr::anti_join(supabase_lookup, by = c("norm_name", "state_full"))
    )
  }

  # -----------------------------------------------------------------------
  # FETCH CUTS DATA FROM PUBLIC API
  # college-cuts.com/api/cuts — no authentication required
  message("Fetching confirmed college cuts from college-cuts.com public API ...")

  fetch_all_api_cuts <- function(status = "confirmed", limit = 100L) {
    page <- 1L
    all_rows <- list()
    repeat {
      url <- paste0(
        "https://college-cuts.com/api/cuts",
        "?status=", utils::URLencode(status, reserved = TRUE),
        "&limit=", limit,
        "&page=", page
      )
      resp <- httr2::request(url) |>
        httr2::req_timeout(30) |>
        httr2::req_error(is_error = function(r) FALSE) |>
        httr2::req_perform()
      if (httr2::resp_status(resp) != 200L) {
        stop("College Cuts API returned HTTP ", httr2::resp_status(resp),
             " — cannot continue without cuts data.", call. = FALSE)
      }
      body <- httr2::resp_body_json(resp)
      all_rows <- c(all_rows, body$data)
      if (page >= body$totalPages || length(body$data) == 0L) break
      page <- page + 1L
    }
    all_rows
  }

  # Build a human-readable cut label.
  # Prefers the API's own programName when it is non-empty and not just a copy
  # of the institution name (the API sometimes sets programName = institution).
  # Falls back to a label constructed from cutType + affected counts.
  make_program_name <- function(api_program_name, institution_name, cut_type,
                                faculty_affected, students_affected) {
    fa <- suppressWarnings(as.integer(faculty_affected))
    sa <- suppressWarnings(as.integer(students_affected))
    generic_label <- switch(
      cut_type %||% "",
      institution_closure = "Institution closure",
      department_closure  = "Department closure",
      hiring_freeze       = "Hiring freeze",
      staff_layoff = paste0(
        "Staff layoff",
        if (!is.na(fa) && fa > 0L) paste0(" (", fa, " positions affected)") else ""
      ),
      program_suspension = paste0(
        "Programs suspended",
        if (!is.na(sa) && sa > 0L) paste0(" (", sa, " students affected)") else ""
      ),
      stringr::str_to_title(gsub("_", " ", cut_type %||% "Unknown"))
    )
    api_name <- trimws(api_program_name %||% "")
    inst_name <- trimws(institution_name %||% "")
    # Use the API program name unless it's blank or just echoes the institution
    if (nzchar(api_name) && !identical(tolower(api_name), tolower(inst_name))) {
      api_name
    } else {
      generic_label
    }
  }

  api_records <- fetch_all_api_cuts("confirmed")
  message("Fetched ", length(api_records), " confirmed cuts from API")

  cuts_raw <- dplyr::bind_rows(lapply(api_records, function(x) {
    fa <- if (!is.null(x$facultyAffected))  suppressWarnings(as.integer(x$facultyAffected))  else NA_integer_
    sa <- if (!is.null(x$studentsAffected)) suppressWarnings(as.integer(x$studentsAffected)) else NA_integer_
    inst <- x$institution %||% ""
    tibble::tibble(
      id                           = x$id %||% NA_character_,
      program_name                 = make_program_name(x$programName, inst, x$cutType, fa, sa),
      cut_type                     = x$cutType %||% NA_character_,
      announcement_date            = x$announcementDate %||% NA_character_,
      effective_term               = x$effectiveTerm %||% NA_character_,
      status                       = x$status %||% NA_character_,
      students_affected            = sa,
      faculty_affected             = fa,
      cip_code                     = x$cipCode %||% NA_character_,
      notes                        = x$notes %||% NA_character_,
      institution_id               = NA_character_,
      institution_name_collegecuts = if (nzchar(inst)) inst else NA_character_,
      institution_city             = NA_character_,
      institution_state_abbr       = x$state %||% NA_character_,
      institution_state_full       = abbr_to_state(x$state %||% ""),
      institution_control          = x$control %||% NA_character_,
      institution_url              = NA_character_,
      institution_unitid           = NA_integer_,
      source_id                    = NA_character_,
      source_url_full              = x$sourceUrl %||% NA_character_,
      source_title                 = NA_character_,
      source_publication_name      = x$sourcePublication %||% NA_character_,
      source_published_at          = NA_character_
    )
  })) |>
    dplyr::filter(
      !is.na(institution_name_collegecuts),
      nzchar(institution_name_collegecuts)
    ) |>
    dplyr::mutate(norm_name = normalize_name(institution_name_collegecuts)) |>
    dplyr::left_join(
      fallback_lookup,
      by = c("norm_name", "institution_state_full" = "state_full")
    ) |>
    dplyr::mutate(
      matched_unitid = unitid_candidate,
      match_method = dplyr::if_else(
        !is.na(unitid_candidate),
        "normalized_name_state_fallback",
        "unmatched"
      )
    )

  # -----------------------------------------------------------------------
  # SELECT FINANCIAL FIELDS TO ATTACH TO CUT RECORDS
  latest_fields <- c(
    "unitid",
    "institution_name",
    "institution_unique_name",
    "year",
    "control_label",
    "sector",
    "level",
    "urbanization",
    "category",
    "state",
    "city",
    "enrollment_headcount_total",
    "enrollment_headcount_undergrad",
    "enrollment_headcount_graduate",
    "pct_international_all",
    "pct_international_undergraduate",
    "pct_international_graduate",
    "international_enrollment_pct_change_5yr",
    "international_enrollment_pct_change_10yr",
    "share_grad_students",
    "staff_headcount_total",
    "staff_headcount_instructional",
    "staff_total_headcount_pct_change_5yr",
    "staff_instructional_headcount_pct_change_5yr",
    "revenue_total",
    "expenses_total",
    "loss_amount",
    "ended_2024_at_loss",
    "losses_last_3_of_5",
    "loss_years_last_5",
    "loss_years_last_10",
    "revenue_10pct_drop_last_3_of_5",
    "revenue_pct_change_5yr",
    "revenue_decreased_5yr",
    "enrollment_decline_last_3_of_5",
    "enrollment_pct_change_5yr",
    "enrollment_decreased_5yr",
    "net_tuition_per_fte",
    "net_tuition_per_fte_change_5yr",
    "tuition_dependence_pct",
    "discount_rate",
    "discount_pct_change_5yr",
    "federal_grants_contracts_pell_adjusted",
    "federal_grants_contracts_pell_adjusted_pct_core_revenue",
    "federal_grants_contracts_pell_adjusted_pct_change_5yr",
    "state_funding",
    "state_funding_pct_core_revenue",
    "state_funding_pct_change_5yr",
    "endowment_value",
    "endowment_pct_change_5yr",
    "liquidity",
    "liquidity_percentile_private_nfp",
    "leverage",
    "leverage_percentile_private_nfp",
    "loan_pct_undergrad_federal_latest",
    "loan_count_undergrad_federal_latest",
    "loan_avg_undergrad_federal_latest"
  )

  financial_latest_slim <- financial_latest |>
    dplyr::select(dplyr::all_of(latest_fields))

  tracker_cols <- setdiff(names(financial_latest_slim), "unitid")
  names(financial_latest_slim)[match(tracker_cols, names(financial_latest_slim))] <- paste0("tracker_", tracker_cols)

  # -----------------------------------------------------------------------
  # JOIN CUTS TO FINANCIAL TRACKER AND COMPUTE FINANCIAL WARNING COUNT
  cuts_joined <- cuts_raw |>
    dplyr::left_join(financial_latest_slim, by = c("matched_unitid" = "unitid")) |>
    dplyr::mutate(
      in_financial_tracker = !is.na(tracker_institution_name),
      announcement_year = suppressWarnings(as.integer(format(as.Date(announcement_date), "%Y"))),
      financial_warning_count = rowSums(
        cbind(
          tracker_enrollment_decline_last_3_of_5 == "Yes",
          tracker_revenue_10pct_drop_last_3_of_5 == "Yes",
          tracker_losses_last_3_of_5 == "Yes",
          tracker_ended_2024_at_loss == "Yes",
          tracker_enrollment_decreased_5yr == "Yes",
          tracker_revenue_decreased_5yr == "Yes"
        ),
        na.rm = TRUE
      )
    ) |>
    dplyr::select(
      cut_id = id,
      program_name,
      cut_type,
      announcement_date,
      announcement_year,
      effective_term,
      status,
      students_affected,
      faculty_affected,
      cip_code,
      notes,
      institution_id,
      institution_name_collegecuts,
      institution_city,
      institution_state_abbr,
      institution_state_full,
      institution_control,
      institution_url,
      institution_unitid,
      matched_unitid,
      match_method,
      in_financial_tracker,
      financial_warning_count,
      tracker_institution_name,
      tracker_control_label,
      tracker_sector,
      tracker_level,
      tracker_urbanization,
      tracker_category,
      tracker_enrollment_headcount_total,
      tracker_enrollment_pct_change_5yr,
      tracker_enrollment_decline_last_3_of_5,
      tracker_revenue_total,
      tracker_expenses_total,
      tracker_revenue_pct_change_5yr,
      tracker_revenue_decreased_5yr,
      tracker_revenue_10pct_drop_last_3_of_5,
      tracker_loss_amount,
      tracker_ended_2024_at_loss,
      tracker_losses_last_3_of_5,
      tracker_loss_years_last_5,
      tracker_loss_years_last_10,
      tracker_net_tuition_per_fte,
      tracker_net_tuition_per_fte_change_5yr,
      tracker_tuition_dependence_pct,
      tracker_staff_headcount_total,
      tracker_staff_headcount_instructional,
      tracker_staff_total_headcount_pct_change_5yr,
      tracker_staff_instructional_headcount_pct_change_5yr,
      tracker_pct_international_all,
      tracker_pct_international_undergraduate,
      tracker_pct_international_graduate,
      tracker_international_enrollment_pct_change_5yr,
      tracker_international_enrollment_pct_change_10yr,
      tracker_share_grad_students,
      tracker_federal_grants_contracts_pell_adjusted_pct_core_revenue,
      tracker_federal_grants_contracts_pell_adjusted_pct_change_5yr,
      tracker_state_funding_pct_core_revenue,
      tracker_state_funding_pct_change_5yr,
      tracker_endowment_value,
      tracker_endowment_pct_change_5yr,
      tracker_discount_rate,
      tracker_discount_pct_change_5yr,
      tracker_liquidity,
      tracker_liquidity_percentile_private_nfp,
      tracker_leverage,
      tracker_leverage_percentile_private_nfp,
      tracker_loan_pct_undergrad_federal_latest,
      tracker_loan_count_undergrad_federal_latest,
      tracker_loan_avg_undergrad_federal_latest,
      source_id,
      source_url = source_url_full,
      source_title,
      source_publication = source_publication_name,
      source_published_at
    ) |>
    dplyr::arrange(dplyr::desc(announcement_date), institution_name_collegecuts, program_name)

  # -----------------------------------------------------------------------
  # BUILD INSTITUTION-LEVEL SUMMARY (COLLAPSE CUTS PER INSTITUTION)
  collapse_unique_values <- function(x) {
    vals <- unique(stats::na.omit(as.character(x)))
    if (length(vals) == 0) NA_character_ else paste(sort(vals), collapse = "; ")
  }

  cuts_institutions_summary <- cuts_joined |>
    dplyr::group_by(matched_unitid, institution_name_collegecuts, institution_state_full) |>
    dplyr::summarise(
      in_financial_tracker = dplyr::first(in_financial_tracker),
      match_method = dplyr::first(match_method),
      institution_control = dplyr::first(institution_control),
      institution_city = dplyr::first(institution_city),
      institution_url = dplyr::first(institution_url),
      tracker_institution_name = dplyr::first(tracker_institution_name),
      tracker_control_label = dplyr::first(tracker_control_label),
      cut_records = dplyr::n(),
      cut_types = collapse_unique_values(cut_type),
      latest_cut_announcement_date = suppressWarnings(max(as.Date(announcement_date), na.rm = TRUE)),
      first_cut_announcement_date = suppressWarnings(min(as.Date(announcement_date), na.rm = TRUE)),
      total_students_affected_known = safe_sum(students_affected),
      total_faculty_affected_known = safe_sum(faculty_affected),
      staff_layoff_records = sum(cut_type == "staff_layoff", na.rm = TRUE),
      program_suspension_records = sum(cut_type == "program_suspension", na.rm = TRUE),
      department_closure_records = sum(cut_type == "department_closure", na.rm = TRUE),
      campus_closure_records = sum(cut_type == "campus_closure", na.rm = TRUE),
      institution_closure_records = sum(cut_type == "institution_closure", na.rm = TRUE),
      teach_out_records = sum(cut_type == "teach_out", na.rm = TRUE),
      financial_warning_count = max(financial_warning_count, na.rm = TRUE),
      tracker_enrollment_pct_change_5yr = dplyr::first(tracker_enrollment_pct_change_5yr),
      tracker_enrollment_decline_last_3_of_5 = dplyr::first(tracker_enrollment_decline_last_3_of_5),
      tracker_revenue_pct_change_5yr = dplyr::first(tracker_revenue_pct_change_5yr),
      tracker_revenue_decreased_5yr = dplyr::first(tracker_revenue_decreased_5yr),
      tracker_revenue_10pct_drop_last_3_of_5 = dplyr::first(tracker_revenue_10pct_drop_last_3_of_5),
      tracker_ended_2024_at_loss = dplyr::first(tracker_ended_2024_at_loss),
      tracker_losses_last_3_of_5 = dplyr::first(tracker_losses_last_3_of_5),
      tracker_loss_years_last_10 = dplyr::first(tracker_loss_years_last_10),
      tracker_net_tuition_per_fte_change_5yr = dplyr::first(tracker_net_tuition_per_fte_change_5yr),
      tracker_staff_total_headcount_pct_change_5yr = dplyr::first(tracker_staff_total_headcount_pct_change_5yr),
      tracker_pct_international_all = dplyr::first(tracker_pct_international_all),
      tracker_international_enrollment_pct_change_5yr = dplyr::first(tracker_international_enrollment_pct_change_5yr),
      tracker_federal_grants_contracts_pell_adjusted_pct_core_revenue = dplyr::first(tracker_federal_grants_contracts_pell_adjusted_pct_core_revenue),
      tracker_state_funding_pct_core_revenue = dplyr::first(tracker_state_funding_pct_core_revenue),
      tracker_tuition_dependence_pct = dplyr::first(tracker_tuition_dependence_pct),
      tracker_endowment_value = dplyr::first(tracker_endowment_value),
      tracker_endowment_pct_change_5yr = dplyr::first(tracker_endowment_pct_change_5yr),
      tracker_discount_pct_change_5yr = dplyr::first(tracker_discount_pct_change_5yr),
      tracker_liquidity_percentile_private_nfp = dplyr::first(tracker_liquidity_percentile_private_nfp),
      tracker_leverage_percentile_private_nfp = dplyr::first(tracker_leverage_percentile_private_nfp),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      latest_cut_announcement_date = as.character(latest_cut_announcement_date),
      first_cut_announcement_date = as.character(first_cut_announcement_date)
    ) |>
    dplyr::arrange(dplyr::desc(financial_warning_count), dplyr::desc(cut_records), institution_name_collegecuts)

  # -----------------------------------------------------------------------
  # EXTRACT FINANCIAL TRENDS FOR MATCHED INSTITUTIONS
  matched_unitids <- cuts_joined |>
    dplyr::filter(!is.na(matched_unitid), in_financial_tracker) |>
    dplyr::distinct(matched_unitid) |>
    dplyr::pull(matched_unitid)

  trends_summary <- cuts_institutions_summary |>
    dplyr::filter(!is.na(matched_unitid), in_financial_tracker) |>
    dplyr::arrange(matched_unitid, dplyr::desc(cut_records), dplyr::desc(financial_warning_count)) |>
    dplyr::group_by(matched_unitid) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      unitid = matched_unitid,
      cut_records,
      cut_types,
      latest_cut_announcement_date,
      financial_warning_count
    )

  financial_trends <- financial_all |>
    dplyr::filter(unitid %in% matched_unitids) |>
    dplyr::select(
      unitid,
      institution_name,
      year,
      enrollment_headcount_total,
      enrollment_headcount_undergrad,
      enrollment_headcount_graduate,
      pct_international_all,
      pct_international_undergraduate,
      pct_international_graduate,
      staff_headcount_total,
      staff_headcount_instructional,
      revenue_total,
      expenses_total,
      loss_amount,
      net_tuition_per_fte,
      state_funding,
      federal_grants_contracts_pell_adjusted,
      endowment_value,
      discount_rate,
      liquidity,
      leverage
    ) |>
    dplyr::left_join(trends_summary, by = "unitid") |>
    dplyr::arrange(institution_name, year)

  # -----------------------------------------------------------------------
  # IDENTIFY UNMATCHED CUTS FOR MANUAL REVIEW
  unmatched_for_review <- cuts_joined |>
    dplyr::filter(!in_financial_tracker) |>
    dplyr::select(
      cut_id,
      program_name,
      cut_type,
      announcement_date,
      institution_name_collegecuts,
      institution_city,
      institution_state_abbr,
      institution_state_full,
      institution_control,
      institution_unitid,
      matched_unitid,
      match_method,
      source_url,
      source_publication
    ) |>
    dplyr::arrange(institution_name_collegecuts, dplyr::desc(announcement_date))

  # -----------------------------------------------------------------------
  # BUILD OVERVIEW SUMMARY TABLE
  overview <- tibble::tibble(
    metric = c(
      "Cuts records",
      "Distinct institutions in cuts data",
      "Institutions with direct UNITID from CollegeCuts",
      "Institutions matched by normalized name/state fallback",
      "Institutions unmatched to financial tracker",
      "Institutions matched to financial tracker",
      "Latest financial tracker year used"
    ),
    value = c(
      nrow(cuts_joined),
      dplyr::n_distinct(cuts_joined$institution_name_collegecuts),
      sum(cuts_joined$match_method == "collegecuts_institutions.unitid", na.rm = TRUE),
      sum(cuts_joined$match_method == "normalized_name_state_fallback", na.rm = TRUE),
      dplyr::n_distinct(unmatched_for_review$institution_name_collegecuts),
      dplyr::n_distinct(cuts_joined$matched_unitid[cuts_joined$in_financial_tracker]),
      latest_year
    )
  )

  # -----------------------------------------------------------------------
  # PREPARE OUTPUT FILE PATHS
  cut_level_csv <- paste0(output_prefix, "_cut_level_joined.csv")
  institution_csv <- paste0(output_prefix, "_institution_summary.csv")
  trends_csv <- paste0(output_prefix, "_financial_trends.csv")
  unmatched_csv <- paste0(output_prefix, "_unmatched_for_review.csv")
  workbook_xlsx <- paste0(output_prefix, "_workbook.xlsx")

  if (nrow(cuts_joined) == 0 || nrow(cuts_institutions_summary) == 0) {
    stop("College cuts refresh produced empty outputs; existing published files were left unchanged.")
  }

  # -----------------------------------------------------------------------
  # WRITE CSV OUTPUTS
  message("Writing outputs ...")
  write_csv_atomic(cuts_joined, cut_level_csv)
  write_csv_atomic(cuts_institutions_summary, institution_csv)
  write_csv_atomic(financial_trends, trends_csv)
  write_csv_atomic(unmatched_for_review, unmatched_csv)

  # -----------------------------------------------------------------------
  # WRITE EXCEL WORKBOOK
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "overview")
  openxlsx::addWorksheet(wb, "cuts_joined")
  openxlsx::addWorksheet(wb, "inst_summary")
  openxlsx::addWorksheet(wb, "fin_trends")
  openxlsx::addWorksheet(wb, "unmatched")

  openxlsx::writeData(wb, "overview", overview)
  openxlsx::writeData(wb, "cuts_joined", cuts_joined)
  openxlsx::writeData(wb, "inst_summary", cuts_institutions_summary)
  openxlsx::writeData(wb, "fin_trends", financial_trends)
  openxlsx::writeData(wb, "unmatched", unmatched_for_review)
  workbook_tmp <- paste0(workbook_xlsx, ".tmp")
  if (file.exists(workbook_tmp)) file.remove(workbook_tmp)
  openxlsx::saveWorkbook(wb, workbook_tmp, overwrite = TRUE)
  if (file.exists(workbook_xlsx)) file.remove(workbook_xlsx)
  file.rename(workbook_tmp, workbook_xlsx)

  # -----------------------------------------------------------------------
  # LOG COMPLETION
  message("Saved: ", cut_level_csv)
  message("Saved: ", institution_csv)
  message("Saved: ", trends_csv)
  message("Saved: ", unmatched_csv)
  message("Saved: ", workbook_xlsx)

  invisible(
    list(
      overview = overview,
      cuts_joined = cuts_joined,
      institution_summary = cuts_institutions_summary,
      financial_trends = financial_trends,
      unmatched_for_review = unmatched_for_review
    )
  )
}

if (sys.nframe() == 0) {
  main()
}
