run_test("Web export pipeline fixture", function() {
  fixture_root <- tempfile("web-export-fixture-")
  dir.create(fixture_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  dirs <- c(
    file.path(fixture_root, "data"),
    file.path(fixture_root, "data", "schools"),
    file.path(fixture_root, "data", "downloads"),
    file.path(fixture_root, "data_pipelines", "college_cuts"),
    file.path(fixture_root, "data_pipelines", "accreditation"),
    file.path(fixture_root, "data_pipelines", "grant_witness"),
    file.path(fixture_root, "data_pipelines", "scorecard")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  canonical_path <- file.path(fixture_root, "fixture_canonical.csv")
  canonical_df <- data.frame(
    unitid = c("100", "100"),
    institution_name = c("Example University", "Example University"),
    institution_unique_name = c("Example University | Boston | Massachusetts", "Example University | Boston | Massachusetts"),
    city = c("Boston", "Boston"),
    state = c("Massachusetts", "Massachusetts"),
    control_label = c("Public", "Public"),
    sector = c("Public, 4-year or above", "Public, 4-year or above"),
    category = c("Degree-granting, primarily baccalaureate or above", "Degree-granting, primarily baccalaureate or above"),
    urbanization = c("City", "City"),
    religious_affiliation = c(NA, NA),
    all_programs_distance_education = c("No", "No"),
    year = c("2023", "2024"),
    enrollment_pct_change_5yr = c("-4", "-5"),
    enrollment_decline_last_3_of_5 = c("Yes", "Yes"),
    revenue_pct_change_5yr = c("-2", "-3"),
    net_tuition_per_fte_change_5yr = c("1", "2"),
    staff_total_headcount_pct_change_5yr = c("-1", "-2"),
    staff_instructional_headcount_pct_change_5yr = c("-1", "-2"),
    students_per_instructional_staff_fte = c("11", "12"),
    sector_median_students_per_instructional_staff_fte = c("13", "13"),
    ended_year_at_loss = c("No", "No"),
    losses_last_3_of_5 = c("No", "No"),
    loss_years_last_10 = c("0", "0"),
    tuition_dependence_pct = c("29", "30"),
    sector_median_tuition_dependence_pct = c("28", "29"),
    tuition_dependence_vs_sector_median_sentence = c("Sample sentence 2023", "Sample sentence 2024"),
    share_grad_students = c("0.24", "0.25"),
    research_expense = c("40", "50"),
    research_expense_per_fte = c("0.4", "0.5"),
    research_expense_pct_core_expenses = c("0.08", "0.10"),
    sector_research_spending_n = c("10", "10"),
    sector_research_spending_positive_n = c("8", "8"),
    sector_research_spending_reporting_share_pct = c("80", "80"),
    sector_median_research_expense_per_fte_positive = c("0.45", "0.45"),
    pct_international_all = c("0.09", "0.10"),
    pct_international_undergraduate = c("0.07", "0.08"),
    pct_international_graduate = c("0.18", "0.20"),
    international_student_count_change_5yr = c("8", "10"),
    international_enrollment_pct_change_5yr = c("12", "15"),
    international_students_sentence = c("In 2023, 9% of students were international.", "In 2024, 10% of students were international."),
    federal_loan_pct_most_recent = c("20", "25"),
    federal_grants_contracts_pell_adjusted_pct_core_revenue = c("0.20", "0.25"),
    state_funding_pct_core_revenue = c("0.10", "0.12"),
    federal_grants_contracts_pell_adjusted_pct_change_5yr = c("5", "6"),
    state_funding_pct_change_5yr = c("3", "4"),
    endowment_pct_change_5yr = c("2", "3"),
    endowment_spending_current_use_pct_core_revenue = c("0.05", "0.06"),
    revenue_total = c("1000", "1050"),
    expenses_total = c("900", "950"),
    revenue_total_adjusted = c("950", "1000"),
    expenses_total_adjusted = c("850", "900"),
    net_tuition_per_fte_adjusted = c("2.9", "3.0"),
    enrollment_headcount_total = c("110", "120"),
    enrollment_nonresident_total = c("10", "12"),
    enrollment_nonresident_undergrad = c("7", "8"),
    enrollment_nonresident_graduate = c("3", "4"),
    staff_headcount_total = c("55", "60"),
    staff_headcount_instructional = c("14", "15"),
    endowment_value_adjusted = c("780", "800"),
    endowment_spending_current_use = c("35", "40"),
    federal_grants_contracts_pell_adjusted_adjusted = c("190", "200"),
    state_funding_adjusted = c("140", "150"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(canonical_df, canonical_path, na = "")

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

  readr::write_file("{}", file.path(fixture_root, "data", "closure_status_by_unitid.json"))
  readr::write_file("{}", file.path(fixture_root, "data", "hcm2_by_unitid.json"))
  readr::write_file("{}", file.path(fixture_root, "data", "federal_composite_scores_by_unitid.json"))

  export_env <- new.env(parent = globalenv())
  sys.source(file.path(root, "scripts", "build_web_exports.R"), envir = export_env)
  export_env$main(c("--input", canonical_path, "--output-dir", fixture_root))

  metadata <- jsonlite::read_json(file.path(fixture_root, "data", "metadata.json"), simplifyVector = TRUE)
  schools_index <- jsonlite::read_json(file.path(fixture_root, "data", "schools_index.json"), simplifyVector = TRUE)
  school_file <- jsonlite::read_json(file.path(fixture_root, "data", "schools", "100.json"), simplifyVector = TRUE)
  cuts_export <- jsonlite::read_json(file.path(fixture_root, "data", "college_cuts.json"), simplifyVector = TRUE)
  accreditation_export <- jsonlite::read_json(file.path(fixture_root, "data", "accreditation.json"), simplifyVector = TRUE)
  research_export <- jsonlite::read_json(file.path(fixture_root, "data", "research_funding.json"), simplifyVector = TRUE)
  cuts_index <- jsonlite::read_json(file.path(fixture_root, "data", "college_cuts_index.json"), simplifyVector = TRUE)
  accred_index <- jsonlite::read_json(file.path(fixture_root, "data", "accreditation_index.json"), simplifyVector = TRUE)
  research_index <- jsonlite::read_json(file.path(fixture_root, "data", "research_funding_index.json"), simplifyVector = TRUE)
  download_csv <- readr::read_csv(file.path(fixture_root, "data", "downloads", "full_dataset.csv"), show_col_types = FALSE)

  # ── metadata schema ─────────────────────────────────────────────────────────
  assert_identical(metadata$title, "College Financial Health Tracker")
  assert_true(!is.null(metadata$generated_at) && nzchar(metadata$generated_at),
    "metadata$generated_at should be a non-empty date string.")
  assert_true(!is.null(metadata$files) && is.list(metadata$files),
    "metadata$files should be a named list.")
  required_meta_files <- c("scho