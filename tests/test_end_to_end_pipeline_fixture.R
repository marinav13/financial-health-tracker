run_test("End-to-end canonical to web export fixture", function() {
  fixture_root <- tempfile("end-to-end-fixture-")
  dir.create(fixture_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  dirs <- c(
    file.path(fixture_root, "scripts"),
    file.path(fixture_root, "scripts", "shared"),
    file.path(fixture_root, "ipeds"),
    file.path(fixture_root, "ipeds", "cache"),
    file.path(fixture_root, "ipeds", "cache", "downloads"),
    file.path(fixture_root, "ipeds", "cache", "downloads", "dict"),
    file.path(fixture_root, "ipeds", "cache", "aux"),
    file.path(fixture_root, "ipeds", "cache", "aux", "extracted"),
    file.path(fixture_root, "ipeds", "derived"),
    file.path(fixture_root, "ipeds", "manifests"),
    file.path(fixture_root, "data"),
    file.path(fixture_root, "data", "schools"),
    file.path(fixture_root, "data", "downloads"),
    file.path(fixture_root, "data_pipelines", "college_cuts"),
    file.path(fixture_root, "data_pipelines", "accreditation"),
    file.path(fixture_root, "data_pipelines", "grant_witness"),
    file.path(fixture_root, "data_pipelines", "scorecard")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  file.copy(
    file.path(root, "scripts", "build_ipeds_canonical_dataset.R"),
    file.path(fixture_root, "scripts", "build_ipeds_canonical_dataset.R"),
    overwrite = TRUE
  )
  file.copy(
    file.path(root, "scripts", "build_web_exports.R"),
    file.path(fixture_root, "scripts", "build_web_exports.R"),
    overwrite = TRUE
  )
  file.copy(
    file.path(root, "scripts", "shared", "utils.R"),
    file.path(fixture_root, "scripts", "shared", "utils.R"),
    overwrite = TRUE
  )
  file.copy(
    file.path(root, "scripts", "shared", "ipeds_paths.R"),
    file.path(fixture_root, "scripts", "shared", "ipeds_paths.R"),
    overwrite = TRUE
  )
  file.copy(
    file.path(root, "scripts", "shared", "export_helpers.R"),
    file.path(fixture_root, "scripts", "shared", "export_helpers.R"),
    overwrite = TRUE
  )
  file.copy(
    file.path(root, "scripts", "shared", "contracts.R"),
    file.path(fixture_root, "scripts", "shared", "contracts.R"),
    overwrite = TRUE
  )
  file.copy(
    file.path(root, "scripts", "shared", "ipeds_row_builders.R"),
    file.path(fixture_root, "scripts", "shared", "ipeds_row_builders.R"),
    overwrite = TRUE
  )
  file.copy(
    file.path(root, "scripts", "shared", "ipeds_enrichment_helpers.R"),
    file.path(fixture_root, "scripts", "shared", "ipeds_enrichment_helpers.R"),
    overwrite = TRUE
  )
  file.copy(
    file.path(root, "scripts", "shared", "ipeds_sector_benchmarks.R"),
    file.path(fixture_root, "scripts", "shared", "ipeds_sector_benchmarks.R"),
    overwrite = TRUE
  )

  ipeds_helpers_lines <- readLines(file.path(root, "scripts", "shared", "ipeds_helpers.R"), warn = FALSE)
  ipeds_helpers_override <- c(
    ipeds_helpers_lines,
    "",
    "ensure_dictionary_archive <- function(table_name, out_file, year = 2024) {",
    "  invisible(out_file)",
    "}",
    "",
    "get_frequency_lookup <- function(dictionary_archive, table_name, var_name, extract_root) {",
    "  switch(",
    "    var_name,",
    "    SECTOR = c('4' = 'Public, 4-year or above'),",
    "    ICLEVEL = c('4' = 'Four or more years'),",
    "    ACT = c('1' = 'Active - institution active'),",
    "    CYACTIVE = c('1' = 'Yes'),",
    "    HBCU = c('2' = 'No'),",
    "    TRIBAL = c('2' = 'No'),",
    "    GROFFER = c('1' = 'Yes'),",
    "    INSTCAT = c('A' = 'Degree-granting, primarily baccalaureate or above'),",
    "    LOCALE = c('11' = 'City'),",
    "    CARNEGIESAEC = c('9' = 'Higher access'),",
    "    CARNEGIESIZE = c('3' = 'Medium'),",
    "    CARNEGIEAPM = c('8' = 'Undergrad mix'),",
    "    CARNEGIEGPM = c('7' = 'Grad mix'),",
    "    RELAFFIL = c('10' = 'None'),",
    "    FORM_F = c('1' = 'GASB'),",
    "    character()",
    "  )",
    "}"
  )
  writeLines(ipeds_helpers_override, file.path(fixture_root, "scripts", "shared", "ipeds_helpers.R"), useBytes = TRUE)

  invisible(file.create(file.path(fixture_root, "ipeds", "cache", "downloads", "dict", "HD2024.zip")))
  invisible(file.create(file.path(fixture_root, "ipeds", "cache", "downloads", "dict", "IC2024.zip")))
  invisible(file.create(file.path(fixture_root, "ipeds", "cache", "downloads", "dict", "FLAGS2024.zip")))

  raw_path <- file.path(fixture_root, "fixture_raw.csv")
  catalog_path <- file.path(fixture_root, "fixture_catalog.csv")
  canonical_out <- file.path(fixture_root, "ipeds", "derived", "fixture_canonical.csv")
  extended_out <- file.path(fixture_root, "ipeds", "derived", "fixture_extended.csv")

  raw_df <- data.frame(
    unitid = c("100", "100"),
    year = c("2023", "2024"),
    control = c("1", "1"),
    sector = c("4", "4"),
    level = c("4", "4"),
    status = c("1", "1"),
    is_active = c("1", "1"),
    state = c("MA", "MA"),
    city = c("Boston", "Boston"),
    institution_name = c("Example University", "Example University"),
    urbanization = c("11", "11"),
    category = c("A", "A"),
    hbcu = c("2", "2"),
    tribal_college = c("2", "2"),
    grad_offering = c("1", "1"),
    reporting_model = c("1", "1"),
    access_earnings = c("9", "9"),
    size = c("3", "3"),
    grad_program_mix = c("7", "7"),
    undergrad_program_mix = c("8", "8"),
    religious_affiliation = c("10", "10"),
    all_programs_distance_education = c("2", "2"),
    fte_12_months = c("90", "100"),
    fte_undergrad = c("70", "80"),
    fte_graduate = c("20", "20"),
    fte_total_staff = c("48", "50"),
    fte_instructional = c("9", "10"),
    total_operating_nonoperating_revenues_gasb = c("900", "1000"),
    total_expenses_deductions_current_total_gasb = c("850", "900"),
    tuition_fees_after_discounts_allowances_gasb = c("260", "300"),
    federal_operating_grants_contracts_gasb = c("180", "200"),
    state_appropriations_gasb = c("140", "150"),
    assets = c("480", "500"),
    liabilities = c("190", "200"),
    unrestricted_public = c("230", "250"),
    discounts_allowances_applied_tuition_fees_gasb = c("90", "100"),
    value_endowment_assets_end_gasb = c("760", "800"),
    research_expenses_total_gasb = c("45", "50"),
    core_expenses_gasb = c("380", "400"),
    auxiliary_enterprises_revenue_gasb = c("90", "100"),
    hospital_services_revenue_gasb = c("45", "50"),
    independent_operations_revenue_gasb = c("20", "25"),
    pell_accounting_method = c("1", "1"),
    pell_grants = c("0", "0"),
    endowment_spending_distribution_current_use_gasb = c("-35", "-40"),
    endowment_assets_per_fte_gasb = c("8.4", "8"),
    admissions_yield = c("41", "42"),
    institution_name_latest = c("Example University", "Example University"),
    institution_unique_name_latest = c("Example University | Boston | Massachusetts", "Example University | Boston | Massachusetts"),
    state_latest = c("Massachusetts", "Massachusetts"),
    city_latest = c("Boston", "Boston"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(raw_df, raw_path, na = "")

  effy_extract_dir <- file.path(fixture_root, "ipeds", "cache", "aux", "extracted", "EFFY2024")
  dir.create(effy_extract_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(
    data.frame(
      UNITID = c("100", "100", "100"),
      EFFYALEV = c("1", "2", "12"),
      EFYTOTLT = c("120", "90", "30"),
      EFYNRALT = c("12", "8", "4"),
      stringsAsFactors = FALSE
    ),
    file.path(effy_extract_dir, "EFFY2024.csv"),
    na = ""
  )

  readr::write_csv(
    data.frame(
      year = 2024L,
      table_name = "EFFY2024",
      data_url = "https://example.org/EFFY2024.zip",
      stringsAsFactors = FALSE
    ),
    catalog_path,
    na = ""
  )

  readr::write_csv(
    data.frame(
      matched_unitid = "100",
      announcement_date = "2024-02-15",
      announcement_year = "2024",
      in_financial_tracker = "TRUE",
      tracker_institution_name = "Example University",
      institution_name_collegecuts = "Example University",
      institution_state_full = "Massachusetts",
      institution_city = "Boston",
      tracker_control_label = "Public",
      institution_control = "Public",
      tracker_category = "Degree-granting, primarily baccalaureate or above",
      faculty_affected = "12",
      notes = "Twelve faculty positions affected",
      source_title = "Budget update",
      program_name = "Humanities",
      cut_type = "faculty_layoff",
      status = "announced",
      effective_term = "Fall 2024",
      source_url = "https://example.org/cuts",
      source_publication = "Example News",
      source_published_at = "2024-02-15",
      students_affected = "",
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "college_cuts", "college_cuts_financial_tracker_cut_level_joined.csv"),
    na = ""
  )

  readr::write_csv(
    data.frame(
      unitid = "100",
      institution_name = "Example University",
      state = "Massachusetts",
      city = "Boston",
      control_label = "Public",
      category = "Degree-granting, primarily baccalaureate or above",
      accreditors = "MSCHE",
      latest_action_date = "2024-03-01",
      latest_action_year = "2024",
      action_labels = "Warning",
      active_actions = "Warning",
      has_active_warning = TRUE,
      has_active_warning_or_notice = TRUE,
      has_active_adverse_action = FALSE,
      action_count = 1L,
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "accreditation", "accreditation_tracker_institution_summary.csv"),
    na = ""
  )

  readr::write_csv(
    data.frame(
      unitid = "100",
      institution_name = "Example University",
      state = "Massachusetts",
      city = "Boston",
      control_label = "Public",
      category = "Degree-granting, primarily baccalaureate or above",
      accreditor = "MSCHE",
      action_type = "warning",
      action_label_raw = "Warning",
      action_status = "active",
      action_date = "2024-03-01",
      action_year = "2024",
      notes = "Public warning issued",
      source_url = "https://example.org/accreditation",
      source_title = "MSCHE action",
      source_page_url = "https://example.org/accreditation-page",
      source_page_modified = "2024-03-02",
      display_action = TRUE,
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "accreditation", "accreditation_tracker_actions_joined.csv"),
    na = ""
  )

  readr::write_csv(
    data.frame(
      accreditor = "MSCHE",
      source_url = "https://example.org/accreditation-page",
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "accreditation", "accreditation_tracker_source_coverage.csv"),
    na = ""
  )

  readr::write_csv(
    data.frame(
      matched_unitid = "100",
      display_name = "Example University",
      display_city = "Boston",
      display_state = "Massachusetts",
      tracker_control_label = "Public",
      tracker_category = "Degree-granting, primarily baccalaureate or above",
      likely_higher_ed = TRUE,
      total_disrupted_grants = 1L,
      total_disrupted_award_remaining = 50000,
      nih_disrupted_grants = 1L,
      nih_disrupted_award_remaining = 50000,
      nsf_disrupted_grants = 0L,
      nsf_disrupted_award_remaining = 0,
      epa_disrupted_grants = 0L,
      epa_disrupted_award_remaining = 0,
      samhsa_disrupted_grants = 0L,
      samhsa_disrupted_award_remaining = 0,
      cdc_disrupted_grants = 0L,
      cdc_disrupted_award_remaining = 0,
      institution_key = "unitid:100",
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "grant_witness", "grant_witness_higher_ed_institution_summary.csv"),
    na = ""
  )

  readr::write_csv(
    data.frame(
      matched_unitid = "100",
      tracker_institution_name = "Example University",
      tracker_state = "Massachusetts",
      currently_disrupted = "TRUE",
      likely_higher_ed = TRUE,
      agency = "nih",
      grant_id = "R01-1",
      grant_id_core = "R01",
      status = "terminated",
      organization_name = "Example University",
      organization_city = "Boston",
      organization_state = "Massachusetts",
      organization_type = "University",
      project_title = "Cancer research",
      project_abstract = "Research abstract",
      start_date = "2023-01-01",
      original_end_date = "2025-12-31",
      termination_date = "2024-04-01",
      award_value = "100000",
      award_outlaid = "50000",
      award_remaining = "50000",
      remaining_field = "award_remaining",
      source_url = "https://example.org/grant",
      detail_url = "https://example.org/grant-detail",
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "grant_witness", "grant_witness_grant_level_joined.csv"),
    na = ""
  )

  readr::write_csv(
    data.frame(
      unitid = "100",
      graduation_rate_6yr = "72",
      median_earnings_10yr = "56000",
      median_debt_completers = "21000",
      grad_plus_recipients = "6",
      grad_plus_disbursements_amt = "60000",
      grad_plus_disbursements_per_recipient = "10000",
      outcomes_data_available = TRUE,
      scorecard_data_updated = "2024-01-01",
      grad_plus_data_updated = "2024-01-01",
      ipeds_graduation_rate_year = "2024",
      ipeds_graduation_rate_label = "2024 cohort",
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "scorecard", "tracker_outcomes_joined.csv"),
    na = ""
  )

  readr::write_file('{"as_of_date":"2024-01-01","schools":{}}', file.path(fixture_root, "data", "closure_status_by_unitid.json"))
  readr::write_file('{"generated_at":"2024-01-01","schools":{}}', file.path(fixture_root, "data", "hcm2_by_unitid.json"))
  readr::write_file('{"generated_at":"2024-01-01","schools":{}}', file.path(fixture_root, "data", "federal_composite_scores_by_unitid.json"))

  setwd(fixture_root)

  canonical_env <- new.env(parent = globalenv())
  sys.source(file.path(fixture_root, "scripts", "build_ipeds_canonical_dataset.R"), envir = canonical_env)
  canonical_env$main(c(
    "--raw", raw_path,
    "--catalog", catalog_path,
    "--output", canonical_out,
    "--expanded-output", extended_out
  ))

  export_env <- new.env(parent = globalenv())
  sys.source(file.path(fixture_root, "scripts", "build_web_exports.R"), envir = export_env)
  export_env$main(c("--input", canonical_out, "--output-dir", fixture_root))

  school_file <- jsonlite::read_json(file.path(fixture_root, "data", "schools", "100.json"), simplifyVector = TRUE)
  download_csv <- readr::read_csv(file.path(fixture_root, "data", "downloads", "full_dataset.csv"), show_col_types = FALSE)
  schools_index <- jsonlite::read_json(file.path(fixture_root, "data", "schools_index.json"), simplifyVector = TRUE)

  assert_identical(school_file$profile$institution_name, "Example University")
  assert_equal(school_file$summary$tuition_dependence_pct, 30)
  assert_equal(school_file$summary$pct_international_all, 10)
  assert_equal(school_file$summary$graduation_rate_6yr, 72)
  assert_identical(nrow(download_csv), 1L)
  assert_identical(as.character(download_csv$year[[1]]), "2024")
  assert_identical(schools_index$institution_unique_name[[1]], "Example University | Boston | Massachusetts")
})
