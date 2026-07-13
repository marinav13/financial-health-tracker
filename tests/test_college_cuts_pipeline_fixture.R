if (!exists("run_test", mode = "function")) {
  source(file.path(getwd(), "tests", "test_support.R"))
}

run_test("College Cuts join pipeline fixture", function() {
  fixture_root <- tempfile("college-cuts-fixture-")
  dir.create(fixture_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  dirs <- c(
    file.path(fixture_root, "scripts"),
    file.path(fixture_root, "scripts", "shared"),
    file.path(fixture_root, "data_pipelines"),
    file.path(fixture_root, "data_pipelines", "college_cuts"),
    file.path(fixture_root, "data_pipelines", "college_cuts", "cache")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  for (nm in c("shared/utils.R", "shared/ipeds_paths.R", "shared/contracts.R",
               "shared/name_normalization.R")) {
    file.copy(
      file.path(root, "scripts", nm),
      file.path(fixture_root, "scripts", nm),
      overwrite = TRUE
    )
  }
  file.copy(
    file.path(root, "scripts", "build_college_cuts_join.R"),
    file.path(fixture_root, "scripts", "build_college_cuts_join.R"),
    overwrite = TRUE
  )

  financial_input <- file.path(fixture_root, "fixture_financial.csv")
  output_prefix <- file.path(fixture_root, "data_pipelines", "college_cuts", "fixture_college_cuts")
  cache_dir <- file.path(fixture_root, "data_pipelines", "college_cuts", "cache")

  financial_df <- data.frame(
    unitid = "100",
    institution_name = "Example University",
    institution_unique_name = "Example University | Boston | Massachusetts",
    year = 2024,
    control_label = "Public",
    sector = "Public, 4-year or above",
    level = "4-year",
    urbanization = "City",
    category = "Degree-granting, primarily baccalaureate or above",
    state = "Massachusetts",
    city = "Boston",
    enrollment_headcount_total = 120,
    enrollment_headcount_undergrad = 100,
    enrollment_headcount_graduate = 20,
    pct_international_all = 0.10,
    pct_international_undergraduate = 0.08,
    pct_international_graduate = 0.20,
    international_enrollment_pct_change_5yr = 15,
    international_enrollment_pct_change_10yr = 25,
    share_grad_students = 0.17,
    staff_headcount_total = 60,
    staff_headcount_instructional = 15,
    staff_total_headcount_pct_change_5yr = -2,
    staff_instructional_headcount_pct_change_5yr = -1,
    revenue_total = 1050,
    expenses_total = 950,
    loss_amount = 0,
    ended_2024_at_loss = "No",
    losses_last_3_of_5 = "No",
    loss_years_last_5 = 0,
    loss_years_last_10 = 0,
    revenue_10pct_drop_last_3_of_5 = "No",
    revenue_pct_change_5yr = -3,
    revenue_decreased_5yr = "Yes",
    enrollment_decline_last_3_of_5 = "Yes",
    enrollment_pct_change_5yr = -5,
    enrollment_decreased_5yr = "Yes",
    net_tuition_per_fte = 3000,
    net_tuition_per_fte_change_5yr = 2,
    tuition_dependence_pct = 30,
    discount_rate = 0.25,
    discount_pct_change_5yr = 1,
    federal_grants_contracts_pell_adjusted = 200,
    federal_grants_contracts_pell_adjusted_pct_core_revenue = 0.25,
    federal_grants_contracts_pell_adjusted_pct_change_5yr = 6,
    state_funding = 150,
    state_funding_pct_core_revenue = 0.12,
    state_funding_pct_change_5yr = 4,
    endowment_value = 800,
    endowment_pct_change_5yr = 3,
    liquidity = 0.9,
    liquidity_percentile_private_nfp = 40,
    leverage = 0.3,
    leverage_percentile_private_nfp = 55,
    loan_pct_undergrad_federal_latest = 25,
    loan_count_undergrad_federal_latest = 40,
    loan_avg_undergrad_federal_latest = 10000,
    stringsAsFactors = FALSE
  )
  readr::write_csv(financial_df, financial_input, na = "")

  readr::write_csv(
    data.frame(
      id = 1,
      name = "Example University",
      city = "Boston",
      state = "MA",
      control = "Public",
      url = "https://example.org",
      unitid = NA_character_,
      latitude = NA_real_,
      longitud = NA_real_,
      created_at = "2024-01-01T00:00:00Z",
      stringsAsFactors = FALSE
    ),
    file.path(cache_dir, "institutions.csv"),
    na = ""
  )

  readr::write_csv(
    data.frame(
      id = 10,
      institution_id = 1,
      source_id = 100,
      program_name = "Humanities",
      cut_type = "staff_layoff",
      announcement_date = "2024-02-15",
      effective_term = "Fall 2024",
      status = "announced",
      students_affected = NA_integer_,
      faculty_affected = 12,
      cip_code = "24.0101",
      notes = "Twelve faculty positions affected",
      stringsAsFactors = FALSE
    ),
    file.path(cache_dir, "program_cuts.csv"),
    na = ""
  )

  readr::write_csv(
    data.frame(
      id = 100,
      url = "https://example.org/cuts",
      title = "Budget update",
      publication = "Example News",
      published_at = "2024-02-15",
      created_at = "2024-02-15T12:00:00Z",
      stringsAsFactors = FALSE
    ),
    file.path(cache_dir, "sources.csv"),
    na = ""
  )

  old_url <- Sys.getenv("COLLEGE_CUTS_SUPABASE_URL", unset = NA_character_)
  old_key <- Sys.getenv("COLLEGE_CUTS_SUPABASE_ANON_KEY", unset = NA_character_)
  on.exit({
    if (is.na(old_url)) Sys.unsetenv("COLLEGE_CUTS_SUPABASE_URL") else Sys.setenv(COLLEGE_CUTS_SUPABASE_URL = old_url)
    if (is.na(old_key)) Sys.unsetenv("COLLEGE_CUTS_SUPABASE_ANON_KEY") else Sys.setenv(COLLEGE_CUTS_SUPABASE_ANON_KEY = old_key)
  }, add = TRUE)
  Sys.setenv(
    COLLEGE_CUTS_SUPABASE_URL = "http://127.0.0.1:9",
    COLLEGE_CUTS_SUPABASE_ANON_KEY = "fixture-key"
  )

  # Create required input files that build_college_cuts_join.R reads from getwd().
  # Copy the real manual_aliases.csv so readr parses unitid_candidate as double
  # (an empty stub would be read as character, causing bind_rows type conflicts).
  file.copy(
    file.path(root, "data_pipelines", "college_cuts", "manual_aliases.csv"),
    file.path(fixture_root, "data_pipelines", "college_cuts", "manual_aliases.csv"),
    overwrite = TRUE
  )
  readr::write_csv(
    data.frame(
      institution_name_api = "Example University",
      unitid = 100,
      state_full = "Massachusetts",
      tracker_institution_name = "Example University",
      match_source = "supabase_mapping",
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "college_cuts", "supabase_institution_unitid_mapping.csv"),
    na = ""
  )

  setwd(fixture_root)
  cuts_env <- new.env(parent = globalenv())
  sys.source(file.path(fixture_root, "scripts", "build_college_cuts_join.R"), envir = cuts_env)
  # Write a minimal stub API cuts CSV so the fixture doesn't hit the live API
  api_cuts_csv <- file.path(fixture_root, "data_pipelines", "college_cuts", "fixture_api_cuts.csv")
  readr::write_csv(
    data.frame(
      id                           = "10",
      program_name                 = "Twelve faculty positions affected (Staff Layoff)",
      cut_type                     = "staff_layoff",
      announcement_date            = "2024-02-15",
      effective_term               = "Fall 2024",
      status                       = "confirmed",
      students_affected            = NA_integer_,
      faculty_affected             = 12L,
      cip_code                     = "24.0101",
      notes                        = "Twelve faculty positions affected",
      institution_id               = NA_character_,
      institution_name_collegecuts = "Example University",
      institution_city             = "Boston",
      institution_state_abbr       = "MA",
      institution_state_full       = "Massachusetts",
      institution_control          = "Public",
      institution_url              = NA_character_,
      institution_unitid           = NA_integer_,
      source_id                    = NA_character_,
      source_url_full              = "https://example.org/cuts",
      source_title                 = NA_character_,
      source_publication_name      = "Example News",
      source_published_at          = NA_character_,
      stringsAsFactors = FALSE
    ),
    api_cuts_csv,
    na = ""
  )

  cuts_env$main(c(
    "--financial-input", financial_input,
    "--output-prefix", output_prefix,
    "--cache-dir", cache_dir,
    "--api-cuts-csv", api_cuts_csv
  ))

  cut_level_path <- paste0(output_prefix, "_cut_level_joined.csv")
  summary_path <- paste0(output_prefix, "_institution_summary.csv")
  trends_path <- paste0(output_prefix, "_financial_trends.csv")
  unmatched_path <- paste0(output_prefix, "_unmatched_for_review.csv")

  assert_true(file.exists(cut_level_path), "Cut-level joined output should exist.")
  assert_true(file.exists(summary_path), "Institution summary output should exist.")
  assert_true(file.exists(trends_path), "Financial trends output should exist.")
  assert_true(file.exists(unmatched_path), "Unmatched review output should exist.")

  cuts_joined <- readr::read_csv(cut_level_path, show_col_types = FALSE)
  summary_df <- readr::read_csv(summary_path, show_col_types = FALSE)
  trends_df <- readr::read_csv(trends_path, show_col_types = FALSE)
  unmatched_df <- readr::read_csv(unmatched_path, show_col_types = FALSE)

  assert_identical(nrow(cuts_joined), 1L)
  assert_identical(as.character(cuts_joined$matched_unitid[[1]]), "100")
  assert_identical(cuts_joined$match_method[[1]], "supabase_mapping")
  assert_true(isTRUE(cuts_joined$in_financial_tracker[[1]]), "Fixture cut row should match into the financial tracker.")
  assert_identical(cuts_joined$tracker_institution_name[[1]], "Example University")

  assert_identical(nrow(summary_df), 1L)
  assert_identical(summary_df$cut_records[[1]], 1)
  assert_identical(summary_df$staff_layoff_records[[1]], 1)
  assert_identical(summary_df$tracker_control_label[[1]], "Public")

  assert_identical(nrow(trends_df), 1L)
  assert_identical(as.character(trends_df$unitid[[1]]), "100")
  assert_identical(nrow(unmatched_df), 0L)
})

run_test("College Cuts join falls back to cached cut-level snapshot when API is unavailable", function() {
  fixture_root <- tempfile("college-cuts-cache-fallback-")
  dir.create(fixture_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  dirs <- c(
    file.path(fixture_root, "scripts"),
    file.path(fixture_root, "scripts", "shared"),
    file.path(fixture_root, "data_pipelines"),
    file.path(fixture_root, "data_pipelines", "college_cuts"),
    file.path(fixture_root, "data_pipelines", "college_cuts", "cache")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  for (nm in c("shared/utils.R", "shared/ipeds_paths.R", "shared/contracts.R",
               "shared/name_normalization.R")) {
    file.copy(
      file.path(root, "scripts", nm),
      file.path(fixture_root, "scripts", nm),
      overwrite = TRUE
    )
  }
  file.copy(
    file.path(root, "scripts", "build_college_cuts_join.R"),
    file.path(fixture_root, "scripts", "build_college_cuts_join.R"),
    overwrite = TRUE
  )

  financial_input <- file.path(fixture_root, "fixture_financial.csv")
  output_prefix <- file.path(fixture_root, "data_pipelines", "college_cuts", "fixture_cached")
  fallback_snapshot <- paste0(output_prefix, "_cut_level_joined.csv")

  financial_df <- data.frame(
    unitid = "179159",
    institution_name = "Saint Louis University",
    institution_unique_name = "Saint Louis University | St. Louis | Missouri",
    year = 2024,
    control_label = "Private not-for-profit",
    sector = "Private not-for-profit, 4-year or above",
    level = "Four or more years",
    urbanization = "City",
    category = "Degree-granting, primarily baccalaureate or above",
    state = "Missouri",
    city = "St. Louis",
    enrollment_headcount_total = 15000,
    enrollment_headcount_undergrad = 11000,
    enrollment_headcount_graduate = 4000,
    pct_international_all = 0.2,
    pct_international_undergraduate = 0.05,
    pct_international_graduate = 0.45,
    international_enrollment_pct_change_5yr = 10,
    international_enrollment_pct_change_10yr = 20,
    share_grad_students = 0.27,
    staff_headcount_total = 3000,
    staff_headcount_instructional = 1000,
    staff_total_headcount_pct_change_5yr = -5,
    staff_instructional_headcount_pct_change_5yr = -4,
    revenue_total = 700000000,
    expenses_total = 690000000,
    loss_amount = 10000000,
    ended_2024_at_loss = "Yes",
    losses_last_3_of_5 = "Yes",
    loss_years_last_5 = 3,
    loss_years_last_10 = 4,
    revenue_10pct_drop_last_3_of_5 = "No",
    revenue_pct_change_5yr = -2,
    revenue_decreased_5yr = "Yes",
    enrollment_decline_last_3_of_5 = "Yes",
    enrollment_pct_change_5yr = -3,
    enrollment_decreased_5yr = "Yes",
    net_tuition_per_fte = 20000,
    net_tuition_per_fte_change_5yr = -1,
    tuition_dependence_pct = 40,
    discount_rate = 0.45,
    discount_pct_change_5yr = 2,
    federal_grants_contracts_pell_adjusted = 100000000,
    federal_grants_contracts_pell_adjusted_pct_core_revenue = 0.20,
    federal_grants_contracts_pell_adjusted_pct_change_5yr = 5,
    state_funding = 5000000,
    state_funding_pct_core_revenue = 0.01,
    state_funding_pct_change_5yr = 1,
    endowment_value = 1000000000,
    endowment_pct_change_5yr = 3,
    liquidity = 1.1,
    liquidity_percentile_private_nfp = 65,
    leverage = 0.4,
    leverage_percentile_private_nfp = 55,
    loan_pct_undergrad_federal_latest = 30,
    loan_count_undergrad_federal_latest = 5000,
    loan_avg_undergrad_federal_latest = 7000,
    stringsAsFactors = FALSE
  )
  readr::write_csv(financial_df, financial_input, na = "")

  file.copy(
    file.path(root, "data_pipelines", "college_cuts", "manual_aliases.csv"),
    file.path(fixture_root, "data_pipelines", "college_cuts", "manual_aliases.csv"),
    overwrite = TRUE
  )
  readr::write_csv(
    data.frame(
      institution_name_api = "Saint Louis University",
      unitid = 179159,
      state_full = "Missouri",
      tracker_institution_name = "Saint Louis University",
      match_source = "supabase_mapping",
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "college_cuts", "supabase_institution_unitid_mapping.csv"),
    na = ""
  )

  readr::write_csv(
    data.frame(
      cut_id = c("cached-cut-1", "cached-cut-2"),
      program_name = c("80 vacant positions eliminated", "Additional staff cuts"),
      cut_type = c("staff_layoff", "staff_layoff"),
      announcement_date = c("2026-07-01", "2026-07-02"),
      announcement_year = c(2026, 2026),
      effective_term = c("2026-2027", "2026-2027"),
      status = c("confirmed", "confirmed"),
      students_affected = c(NA_integer_, NA_integer_),
      faculty_affected = c(80L, 5L),
      cip_code = c(NA_character_, NA_character_),
      notes = c("Cached fallback row", "[staff] Additional cached fallback row"),
      institution_id = c(NA_character_, NA_character_),
      institution_name_collegecuts = c("Saint Louis University", "Saint Louis University"),
      institution_city = c(NA_character_, NA_character_),
      institution_state_abbr = c("MO", "MO"),
      institution_state_full = c("Missouri", "Missouri"),
      institution_control = c("Private non-profit", "Private non-profit"),
      institution_url = c(NA_character_, NA_character_),
      institution_unitid = c(NA_integer_, NA_integer_),
      matched_unitid = c(179159L, 179159L),
      match_method = c("supabase_mapping", "supabase_mapping"),
      in_financial_tracker = c(TRUE, TRUE),
      financial_warning_count = c(0, 0),
      tracker_institution_name = c("Saint Louis University", "Saint Louis University"),
      tracker_control_label = c("Private not-for-profit", "Private not-for-profit"),
      tracker_sector = c("Private not-for-profit, 4-year or above", "Private not-for-profit, 4-year or above"),
      tracker_level = c("Four or more years", "Four or more years"),
      tracker_urbanization = c("City", "City"),
      tracker_category = c("Degree-granting, primarily baccalaureate or above", "Degree-granting, primarily baccalaureate or above"),
      tracker_enrollment_headcount_total = c(15000, 15000),
      tracker_enrollment_pct_change_5yr = c(-3, -3),
      tracker_enrollment_decline_last_3_of_5 = c("Yes", "Yes"),
      tracker_revenue_total = c(700000000, 700000000),
      tracker_expenses_total = c(690000000, 690000000),
      tracker_revenue_pct_change_5yr = c(-2, -2),
      tracker_revenue_decreased_5yr = c("Yes", "Yes"),
      tracker_revenue_10pct_drop_last_3_of_5 = c("No", "No"),
      tracker_loss_amount = c(10000000, 10000000),
      tracker_ended_2024_at_loss = c("Yes", "Yes"),
      tracker_losses_last_3_of_5 = c("Yes", "Yes"),
      tracker_loss_years_last_5 = c(3, 3),
      tracker_loss_years_last_10 = c(4, 4),
      tracker_net_tuition_per_fte = c(20000, 20000),
      tracker_net_tuition_per_fte_change_5yr = c(-1, -1),
      tracker_tuition_dependence_pct = c(40, 40),
      tracker_staff_headcount_total = c(3000, 3000),
      tracker_staff_headcount_instructional = c(1000, 1000),
      tracker_staff_total_headcount_pct_change_5yr = c(-5, -5),
      tracker_staff_instructional_headcount_pct_change_5yr = c(-4, -4),
      tracker_pct_international_all = c(0.2, 0.2),
      tracker_pct_international_undergraduate = c(0.05, 0.05),
      tracker_pct_international_graduate = c(0.45, 0.45),
      tracker_international_enrollment_pct_change_5yr = c(10, 10),
      tracker_international_enrollment_pct_change_10yr = c(20, 20),
      tracker_share_grad_students = c(0.27, 0.27),
      tracker_federal_grants_contracts_pell_adjusted_pct_core_revenue = c(0.20, 0.20),
      tracker_federal_grants_contracts_pell_adjusted_pct_change_5yr = c(5, 5),
      tracker_state_funding_pct_core_revenue = c(0.01, 0.01),
      tracker_state_funding_pct_change_5yr = c(1, 1),
      tracker_endowment_value = c(1000000000, 1000000000),
      tracker_endowment_pct_change_5yr = c(3, 3),
      tracker_discount_rate = c(0.45, 0.45),
      tracker_discount_pct_change_5yr = c(2, 2),
      tracker_liquidity = c(1.1, 1.1),
      tracker_liquidity_percentile_private_nfp = c(65, 65),
      tracker_leverage = c(0.4, 0.4),
      tracker_leverage_percentile_private_nfp = c(55, 55),
      tracker_loan_pct_undergrad_federal_latest = c(30, 30),
      tracker_loan_count_undergrad_federal_latest = c(5000, 5000),
      tracker_loan_avg_undergrad_federal_latest = c(7000, 7000),
      source_id = c(NA_character_, NA_character_),
      source_url = c("https://example.org/cached-cut", "https://example.org/cached-cut-2"),
      source_title = c(NA_character_, NA_character_),
      source_publication = c("Example News", "Example News"),
      source_published_at = c(NA_character_, NA_character_),
      stringsAsFactors = FALSE
    ),
    fallback_snapshot,
    na = ""
  )

  setwd(fixture_root)
  cuts_env <- new.env(parent = globalenv())
  sys.source(file.path(fixture_root, "scripts", "build_college_cuts_join.R"), envir = cuts_env)

  cuts_env$main(c(
    "--financial-input", financial_input,
    "--output-prefix", output_prefix,
    "--cuts-api-base-url", "http://127.0.0.1:9/api/cuts"
  ))

  cut_level_path <- paste0(output_prefix, "_cut_level_joined.csv")
  summary_path <- paste0(output_prefix, "_institution_summary.csv")

  assert_true(file.exists(cut_level_path), "Cut-level output should exist after cached fallback.")
  assert_true(file.exists(summary_path), "Institution summary should exist after cached fallback.")

  cuts_joined <- readr::read_csv(cut_level_path, show_col_types = FALSE)
  summary_df <- readr::read_csv(summary_path, show_col_types = FALSE)

  assert_identical(nrow(cuts_joined), 2L)
  assert_true(
    setequal(cuts_joined$cut_id, c("cached-cut-1", "cached-cut-2")),
    paste0("Expected cached fallback cut ids to round-trip. Got: ", paste(cuts_joined$cut_id, collapse = ", "))
  )
  assert_identical(cuts_joined$cut_id[[1]], "cached-cut-2")
  assert_identical(as.character(cuts_joined$matched_unitid[[1]]), "179159")
  assert_identical(cuts_joined$match_method[[1]], "supabase_mapping")
  assert_true(isTRUE(cuts_joined$in_financial_tracker[[1]]), "Cached fallback row should remain matched.")
  assert_identical(cuts_joined$financial_warning_count[[1]], 5)
  assert_true(
    !grepl("[staff]", cuts_joined$notes[[1]] %||% "", fixed = TRUE),
    paste0("Cached fallback notes should have [staff] tag stripped. Got: ", cuts_joined$notes[[1]])
  )
  assert_identical(nrow(summary_df), 1L)
  assert_identical(summary_df$cut_records[[1]], 2)
})

run_test("College Cuts join: duplicate unitid in financial tracker raises stop()", function() {
  fixture_root <- tempfile("college-cuts-dup-unitid-")
  dir.create(fixture_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  dirs <- c(
    file.path(fixture_root, "scripts"),
    file.path(fixture_root, "scripts", "shared"),
    file.path(fixture_root, "data_pipelines"),
    file.path(fixture_root, "data_pipelines", "college_cuts")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  for (nm in c("shared/utils.R", "shared/ipeds_paths.R", "shared/contracts.R",
               "shared/name_normalization.R")) {
    file.copy(
      file.path(root, "scripts", nm),
      file.path(fixture_root, "scripts", nm),
      overwrite = TRUE
    )
  }
  file.copy(
    file.path(root, "scripts", "build_college_cuts_join.R"),
    file.path(fixture_root, "scripts", "build_college_cuts_join.R"),
    overwrite = TRUE
  )

  # Financial tracker with two rows sharing the same unitid for the same year.
  # This should trigger the H13 pre-join cardinality guard before the API call.
  financial_input <- file.path(fixture_root, "fixture_financial_dup.csv")
  output_prefix   <- file.path(fixture_root, "data_pipelines", "college_cuts", "fixture_cc_dup")

  one_row <- data.frame(
    unitid = "100",
    institution_name = "Example University",
    institution_unique_name = "Example University | Boston | Massachusetts",
    year = 2024,
    control_label = "Public",
    sector = "Public, 4-year or above",
    level = "4-year",
    urbanization = "City",
    category = "Degree-granting, primarily baccalaureate or above",
    state = "Massachusetts",
    city = "Boston",
    enrollment_headcount_total = 120,
    enrollment_headcount_undergrad = 100,
    enrollment_headcount_graduate = 20,
    pct_international_all = 0.10,
    pct_international_undergraduate = 0.08,
    pct_international_graduate = 0.20,
    international_enrollment_pct_change_5yr = 15,
    international_enrollment_pct_change_10yr = 25,
    share_grad_students = 0.17,
    staff_headcount_total = 60,
    staff_headcount_instructional = 15,
    staff_total_headcount_pct_change_5yr = -2,
    staff_instructional_headcount_pct_change_5yr = -1,
    revenue_total = 1050,
    expenses_total = 950,
    loss_amount = 0,
    ended_2024_at_loss = "No",
    losses_last_3_of_5 = "No",
    loss_years_last_5 = 0,
    loss_years_last_10 = 0,
    revenue_10pct_drop_last_3_of_5 = "No",
    revenue_pct_change_5yr = -3,
    revenue_decreased_5yr = "Yes",
    enrollment_decline_last_3_of_5 = "Yes",
    enrollment_pct_change_5yr = -5,
    enrollment_decreased_5yr = "Yes",
    net_tuition_per_fte = 3000,
    net_tuition_per_fte_change_5yr = 2,
    tuition_dependence_pct = 30,
    discount_rate = 0.25,
    discount_pct_change_5yr = 1,
    federal_grants_contracts_pell_adjusted = 200,
    federal_grants_contracts_pell_adjusted_pct_core_revenue = 0.25,
    federal_grants_contracts_pell_adjusted_pct_change_5yr = 6,
    state_funding = 150,
    state_funding_pct_core_revenue = 0.12,
    state_funding_pct_change_5yr = 4,
    endowment_value = 800,
    endowment_pct_change_5yr = 3,
    liquidity = 0.9,
    liquidity_percentile_private_nfp = 40,
    leverage = 0.3,
    leverage_percentile_private_nfp = 55,
    loan_pct_undergrad_federal_latest = 25,
    loan_count_undergrad_federal_latest = 40,
    loan_avg_undergrad_federal_latest = 10000,
    stringsAsFactors = FALSE
  )
  # Bind two identical rows — same unitid, same year
  financial_df <- rbind(one_row, one_row)
  readr::write_csv(financial_df, financial_input, na = "")

  setwd(fixture_root)
  cuts_env <- new.env(parent = globalenv())
  sys.source(
    file.path(fixture_root, "scripts", "build_college_cuts_join.R"),
    envir = cuts_env
  )

  error_msg <- NULL
  tryCatch(
    cuts_env$main(c(
      "--financial-input", financial_input,
      "--output-prefix", output_prefix
    )),
    error = function(e) { error_msg <<- conditionMessage(e) }
  )

  assert_true(
    !is.null(error_msg),
    "main() should stop() when financial tracker has duplicate unitids for the same year."
  )
  assert_true(
    grepl("duplicate unitid", error_msg, ignore.case = TRUE),
    paste0("Error message should mention 'duplicate unitid'. Got: ", error_msg)
  )
})
run_test("Bracket tags are stripped from notes in the CSV ingest path", function() {
  fixture_root <- tempfile("cc-bracket-strip-")
  dir.create(fixture_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  dirs <- c(
    file.path(fixture_root, "scripts"),
    file.path(fixture_root, "scripts", "shared"),
    file.path(fixture_root, "data_pipelines"),
    file.path(fixture_root, "data_pipelines", "college_cuts"),
    file.path(fixture_root, "data_pipelines", "college_cuts", "cache")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  for (nm in c("shared/utils.R", "shared/ipeds_paths.R", "shared/contracts.R",
               "shared/name_normalization.R")) {
    file.copy(file.path(root, "scripts", nm),
              file.path(fixture_root, "scripts", nm), overwrite = TRUE)
  }
  file.copy(file.path(root, "scripts", "build_college_cuts_join.R"),
            file.path(fixture_root, "scripts", "build_college_cuts_join.R"), overwrite = TRUE)

  financial_df <- data.frame(
    unitid = "100", institution_name = "Example University",
    institution_unique_name = "Example University | Boston | MA",
    year = 2024, control_label = "Public", sector = "Public, 4-year or above",
    level = "4-year", urbanization = "City",
    category = "Degree-granting, primarily baccalaureate or above",
    state = "Massachusetts", city = "Boston",
    enrollment_headcount_total = 120, enrollment_headcount_undergrad = 100,
    enrollment_headcount_graduate = 20, pct_international_all = 0.10,
    pct_international_undergraduate = 0.08, pct_international_graduate = 0.20,
    international_enrollment_pct_change_5yr = 15, international_enrollment_pct_change_10yr = 25,
    share_grad_students = 0.17, staff_headcount_total = 55, staff_headcount_instructional = 14,
    staff_total_headcount_pct_change_5yr = -1, staff_instructional_headcount_pct_change_5yr = -1,
    revenue_total = 1000, expenses_total = 900, loss_amount = NA, ended_2024_at_loss = "No",
    losses_last_3_of_5 = "No", loss_years_last_5 = 0, loss_years_last_10 = 0,
    revenue_10pct_drop_last_3_of_5 = "No", revenue_pct_change_5yr = -2,
    revenue_decreased_5yr = "No", enrollment_decline_last_3_of_5 = "No",
    enrollment_pct_change_5yr = -4, enrollment_decreased_5yr = "No",
    net_tuition_per_fte = 8, net_tuition_per_fte_change_5yr = 1,
    tuition_dependence_pct = 29, discount_rate = 0.28, discount_pct_change_5yr = 4,
    federal_grants_contracts_pell_adjusted = 190,
    federal_grants_contracts_pell_adjusted_pct_core_revenue = 0.20,
    federal_grants_contracts_pell_adjusted_pct_change_5yr = 5,
    state_funding = 140, state_funding_pct_core_revenue = 0.10,
    state_funding_pct_change_5yr = 3, endowment_value = 800, endowment_pct_change_5yr = 2,
    liquidity = 1.2, liquidity_percentile_private_nfp = NA, leverage = 0.5,
    leverage_percentile_private_nfp = NA, loan_pct_undergrad_federal_latest = 20,
    loan_count_undergrad_federal_latest = 100, loan_avg_undergrad_federal_latest = 5000,
    stringsAsFactors = FALSE
  )
  financial_input <- file.path(fixture_root, "fixture_financial.csv")
  readr::write_csv(financial_df, financial_input, na = "")

  file.copy(file.path(root, "data_pipelines", "college_cuts", "manual_aliases.csv"),
            file.path(fixture_root, "data_pipelines", "college_cuts", "manual_aliases.csv"),
            overwrite = TRUE)
  readr::write_csv(
    data.frame(institution_name_api = "Example University", unitid = 100,
               state_full = "Massachusetts", tracker_institution_name = "Example University",
               match_source = "supabase_mapping", stringsAsFactors = FALSE),
    file.path(fixture_root, "data_pipelines", "college_cuts", "supabase_institution_unitid_mapping.csv"),
    na = ""
  )

  output_prefix <- file.path(fixture_root, "data_pipelines", "college_cuts", "cc_bracket")
  api_cuts_csv  <- file.path(fixture_root, "data_pipelines", "college_cuts", "fixture_api_cuts.csv")
  readr::write_csv(
    data.frame(
      id = "20", program_name = "Staff layoff",
      cut_type = "staff_layoff", announcement_date = "2025-03-01",
      effective_term = NA_character_, status = "confirmed",
      students_affected = NA_integer_, faculty_affected = NA_integer_,
      cip_code = NA_character_,
      notes = "[staff] University cuts 20 positions amid budget deficit.",
      institution_id = NA_character_, institution_name_collegecuts = "Example University",
      institution_city = "Boston", institution_state_abbr = "MA",
      institution_state_full = "Massachusetts", institution_control = "Public",
      institution_url = NA_character_, institution_unitid = NA_integer_,
      source_id = NA_character_, source_url_full = "https://example.org/staff-cut",
      source_title = NA_character_, source_publication_name = "Example News",
      source_published_at = NA_character_, stringsAsFactors = FALSE
    ),
    api_cuts_csv, na = ""
  )

  setwd(fixture_root)
  cuts_env <- new.env(parent = globalenv())
  sys.source(file.path(fixture_root, "scripts", "build_college_cuts_join.R"), envir = cuts_env)
  cuts_env$main(c("--financial-input", financial_input, "--output-prefix", output_prefix,
                  "--api-cuts-csv", api_cuts_csv))

  cut_level_path <- paste0(output_prefix, "_cut_level_joined.csv")
  assert_true(file.exists(cut_level_path), "Cut-level output should exist.")
  cuts_joined <- readr::read_csv(cut_level_path, show_col_types = FALSE)
  assert_identical(nrow(cuts_joined), 1L)
  notes_val <- cuts_joined$notes[[1]]
  assert_true(
    !grepl("[staff]", notes_val %||% "", fixed = TRUE),
    paste0("notes should have [staff] tag stripped. Got: ", notes_val)
  )
  assert_true(
    grepl("University cuts 20 positions", notes_val %||% "", fixed = TRUE),
    paste0("Substantive notes text should be preserved. Got: ", notes_val)
  )
})
