# =========================================================================
# build_college_cuts_join.R
# =========================================================================
#
# PURPOSE: Retrieve college cuts/layoffs/closures from CollegeCuts database
#          (Supabase) and match them to the financial tracker dataset.
#
# DOMAIN: College cuts = layoffs, program cuts, hiring freezes, campus/department
#         closures, and institution closures announced by universities.
#
# INPUTS:
#   - Financial tracker CSV (latest year data with institution info and metrics)
#   - CollegeCuts Supabase tables:
#     * institutions  (metadata about each school in the cuts database)
#     * program_cuts  (individual cut announcements and details)
#     * sources       (article/publication metadata)
#
# OUTPUTS:
#   - college_cuts_financial_tracker_cut_level_joined.csv
#   - college_cuts_financial_tracker_institution_summary.csv
#   - college_cuts_financial_tracker_financial_trends.csv
#   - college_cuts_financial_tracker_unmatched_for_review.csv
#   - college_cuts_financial_tracker_workbook.xlsx
#
# WORKFLOW:
#   1. Fetch institutions, program_cuts, and sources tables from Supabase
#   2. Load financial tracker and normalize institution names
#   3. Build matching lookup tables (direct UNITID, fuzzy name+state match)
#   4. Join cuts to institutions, then to financial tracker
#   5. Calculate financial_warning_count (6 red flags in latest year)
#   6. Build institution-level summary
#   7. Extract financial trends for all matched institutions
#   8. Generate review file for unmatched cuts
#   9. Write outputs as CSVs and Excel workbook
#
# MATCHING STRATEGY:
#   Priority 1: Direct UNITID from CollegeCuts institutions table
#   Priority 2: Normalized name + state fallback
#   Priority 3: Unmatched (flagged for manual review)

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
  cache_dir <- get_arg_value(
    "--cache-dir",
    file.path(getwd(), "data_pipelines", "college_cuts", "cache")
  )
  cuts_csv <- get_arg_value(
    "--cuts-csv",
    file.path(getwd(), "data_pipelines", "college_cuts", "college_cuts_export.csv")
  )

  # -----------------------------------------------------------------------
  # SUPABASE API CREDENTIALS (optional - CSV fallback is available)
  # If credentials are set, use Supabase.
  # Otherwise, will try CSV fallback if available.
  supabase_url <- Sys.getenv("COLLEGE_CUTS_SUPABASE_URL", unset = "")
  supabase_key <- Sys.getenv("COLLEGE_CUTS_SUPABASE_ANON_KEY", unset = "")

  # Note: Supabase credential check moved to later in script to allow CSV fallback

  if (!file.exists(financial_input)) {
    stop("Financial input file not found: ", financial_input)
  }
  dir.create(dirname(output_prefix), recursive = TRUE, showWarnings = FALSE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # -----------------------------------------------------------------------
  # HELPER: Build Supabase REST API URL
  build_url <- function(table_name) {
    paste0(
      supabase_url,
      "/rest/v1/",
      utils::URLencode(table_name, reserved = TRUE)
    )
  }

  # -----------------------------------------------------------------------
  # HELPER: Fetch table from Supabase with cache fallback
  fetch_table_csv <- function(table_name, select = "*", order = NULL, page_size = 1000) {
    cache_path <- file.path(cache_dir, paste0(table_name, ".csv"))
    start <- 0L
    pieces <- list()
    live_result <- tryCatch({
      repeat {
        req <- httr2::request(build_url(table_name)) |>
          httr2::req_headers(
            apikey = supabase_key,
            Authorization = paste("Bearer", supabase_key),
            Accept = "text/csv",
            Prefer = "count=exact",
            Range = paste0(start, "-", start + page_size - 1L),
            `Range-Unit` = "items"
          ) |>
          httr2::req_url_query(select = select)

        if (!is.null(order)) {
          req <- httr2::req_url_query(req, order = order)
        }

        resp <- httr2::req_perform(req)
        body <- httr2::resp_body_string(resp)

        if (!nzchar(trimws(body))) {
          break
        }

        batch <- readr::read_csv(I(body), show_col_types = FALSE, progress = FALSE)
        if (nrow(batch) == 0) {
          break
        }

        pieces[[length(pieces) + 1L]] <- batch

        if (nrow(batch) < page_size) {
          break
        }

        start <- start + page_size
      }

      dplyr::bind_rows(pieces)
    }, error = function(e) e)

    if (!inherits(live_result, "error") && nrow(live_result) > 0) {
      write_csv_atomic(live_result, cache_path)
      return(live_result)
    }

    if (file.exists(cache_path)) {
      message("Falling back to cached College Cuts table for ", table_name)
      return(readr::read_csv(cache_path, show_col_types = FALSE, progress = FALSE))
    }

    if (inherits(live_result, "error")) {
      stop("Failed to fetch College Cuts table ", table_name, ": ", live_result$message)
    }

    stop("College Cuts table ", table_name, " returned no rows and no cache was available.")
  }

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
  normalize_name <- function(x) {
    x |>
      as.character() |>
      stringr::str_to_lower() |>
      stringr::str_replace_all("&", " and ") |>
      stringr::str_replace_all("[^a-z0-9 ]", " ") |>
      stringr::str_squish()
  }

  # -----------------------------------------------------------------------
  # HELPER: Sum values, return NA if all inputs are NA
  safe_sum <- function(x) {
    if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  }

  # -----------------------------------------------------------------------
  # LOAD FINANCIAL TRACKER AND PREPARE FOR MATCHING
  # (Loaded before the fetch so both Supabase and API paths can use
  # fallback_lookup for IPEDS name+state matching.)
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

  # -----------------------------------------------------------------------
  # FETCH CUTS DATA — Supabase (if credentials set) or public API (default)
  if (nzchar(supabase_url) && nzchar(supabase_key)) {
    # -------------------------------------------------------------------
    # SUPABASE PATH: fetch three tables and join into cuts_raw
    message("Fetching CollegeCuts tables from Supabase ...")
    institutions <- fetch_table_csv("institutions", order = "name.asc")
    program_cuts <- fetch_table_csv("program_cuts", order = "announcement_date.desc,id.asc")
    sources <- fetch_table_csv("sources", order = "published_at.desc,id.asc")

    cuts_raw <- program_cuts |>
      dplyr::left_join(
        institutions |>
          dplyr::rename(
            institution_name_collegecuts = name,
            institution_city = city,
            institution_state_abbr = state,
            institution_control = control,
            institution_url = url,
            institution_unitid = unitid,
            institution_latitude = latitude,
            institution_longitude = longitud,
            institution_created_at = created_at
          ),
        by = c("institution_id" = "id")
      ) |>
      dplyr::left_join(
        sources |>
          dplyr::rename(
            source_url_full = url,
            source_title = title,
            source_publication_name = publication,
            source_published_at = published_at,
            source_created_at = created_at
          ),
        by = c("source_id" = "id")
      ) |>
      dplyr::mutate(
        institution_state_full = abbr_to_state(institution_state_abbr),
        norm_name = normalize_name(institution_name_collegecuts)
      ) |>
      dplyr::left_join(
        fallback_lookup,
        by = c("norm_name", "institution_state_full" = "state_full")
      ) |>
      dplyr::mutate(
        matched_unitid = dplyr::coalesce(institution_unitid, unitid_candidate),
        match_method = dplyr::case_when(
          !is.na(institution_unitid) ~ "collegecuts_institutions.unitid",
          is.na(institution_unitid) & !is.na(unitid_candidate) ~ "normalized_name_state_fallback",
          TRUE ~ "unmatched"
        )
      )

  } else {
    # -------------------------------------------------------------------
    # PUBLIC API PATH: college-cuts.com/api/cuts (no auth required)
    # Fetches all confirmed cuts, then runs the same IPEDS name+state match
    # as the Supabase path so tracker_category and other fields are populated.
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

    # Build a human-readable cut label from cutType + affected counts.
    # The API's programName field mirrors the institution name for all cut types,
    # so we construct our own label from the structured fields instead.
    make_program_name <- function(cut_type, faculty_affected, students_affected) {
      fa <- suppressWarnings(as.integer(faculty_affected))
      sa <- suppressWarnings(as.integer(students_affected))
      switch(
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
    }

    api_records <- fetch_all_api_cuts("confirmed")
    message("Fetched ", length(api_records), " confirmed cuts from API")

    cuts_raw <- dplyr::bind_rows(lapply(api_records, function(x) {
      fa <- if (!is.null(x$facultyAffected))  suppressWarnings(as.integer(x$facultyAffected))  else NA_integer_
      sa <- if (!is.null(x$studentsAffected)) suppressWarnings(as.integer(x$studentsAffected)) else NA_integer_
      tibble::tibble(
        id                           = x$id %||% NA_character_,
        program_name                 = make_program_name(x$cutType, fa, sa),
        cut_type                     = x$cutType %||% NA_character_,
        announcement_date            = x$announcementDate %||% NA_character_,
        effective_term               = x$effectiveTerm %||% NA_character_,
        status                       = x$status %||% NA_character_,
        students_affected            = sa,
        faculty_affected             = fa,
        cip_code                     = x$cipCode %||% NA_character_,
        notes                        = x$notes %||% NA_character_,
        institution_id               = NA_character_,
        institution_name_collegecuts = x$institution %||% NA_character_,
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
  }

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
      institut