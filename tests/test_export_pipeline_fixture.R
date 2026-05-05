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
    year = c("2024", "2025"),
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
    tuition_dependence_vs_sector_median_sentence = c("Sample sentence 2024", "Sample sentence 2025"),
    discount_rate = c("0.28", "0.32"),
    discount_pct_change_5yr = c("4", "6"),
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
    international_students_sentence = c("In 2024, 9% of students were international.", "In 2025, 10% of students were international."),
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
    # Stale outcomes fields simulate rerunning the export after a previous
    # outcomes join. The exporter should replace these, not create .x/.y pairs.
    graduation_rate_6yr = c("11", "12"),
    median_earnings_10yr = c("11111", "22222"),
    median_debt_completers = c("3333", "4444"),
    outcomes_data_available = c(FALSE, FALSE),
    scorecard_data_updated = c("stale", "stale"),
    ipeds_graduation_rate_year = c("stale", "stale"),
    ipeds_graduation_rate_label = c("stale", "stale"),
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
      latest_action_date = "2026-04-24",
      latest_action_year = "2026",
      action_labels = "Program Addition; Substantive Change; Voluntarily Surrendered Accreditation",
      active_actions = "program_addition; adverse_action",
      has_active_warning = FALSE,
      has_active_warning_or_notice = FALSE,
      has_active_adverse_action = TRUE,
      action_count = 3L,
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "accreditation", "accreditation_tracker_institution_summary.csv"),
    na = ""
  )

  readr::write_csv(
    data.frame(
      unitid = c("100", "100", "100", "100"),
      institution_name = c("Example University", "Example University", "Example University", "Example University"),
      state = c("Massachusetts", "Massachusetts", "Massachusetts", "Massachusetts"),
      city = c("Boston", "Boston", "Boston", "Boston"),
      control_label = c("Public", "Public", "Public", "Public"),
      category = c(
        "Degree-granting, primarily baccalaureate or above",
        "Degree-granting, primarily baccalaureate or above",
        "Degree-granting, primarily baccalaureate or above",
        "Degree-granting, primarily baccalaureate or above"
      ),
      accreditor = c("MSCHE", "MSCHE", "MSCHE", "MSCHE"),
      action_type = c("adverse_action", "program_addition", "program_addition", "monitoring"),
      action_label_raw = c(
        paste0(
          "Staff acted on behalf of the Commission to acknowledge receipt of the notification, ",
          "dated April 2, 2026, of the institution's intent to change their primary accreditor, ",
          "voluntarily surrender accreditation, and terminate membership. ",
          "To accept the institution's request to voluntarily surrender its accreditation and intent to ",
          "terminate its membership effective May 31, 2026."
        ),
        "Program Addition",
        "Substantive Change Request Approved",
        paste0(
          "To acknowledge receipt of the monitoring report requested by the Commission action of June 27, 2024. ",
          "To reaffirm accreditation because the institution is now in compliance with Standard V (Educational Effectiveness Assessment). ",
          "To request a monitoring report, due March 2, 2026, demonstrating sustainability of corrective measures."
        )
      ),
      action_status = c("active", "routine", "routine", "active"),
      action_date = c("2026-04-24", "2026-05-01", "2026-05-15", "2025-06-26"),
      action_year = c("", "2026", "2026", "2025"),
      notes = c(
        "Voluntary surrender of accreditation",
        "Routine program addition should not display",
        "Routine substantive change approval should not display",
        "Monitoring report follow-up should lose to same-day DAPIP row"
      ),
      source_url = c(
        "https://example.org/accreditation",
        "https://example.org/program-addition-hidden",
        "https://example.org/program-addition-public",
        "https://example.org/msche-monitoring"
      ),
      source_title = c("MSCHE action", "MSCHE hidden routine action", "MSCHE visible-but-routine action", "MSCHE monitoring action"),
      source_page_url = c("https://example.org/accreditation-page", "https://example.org/accreditation-page", "https://example.org/accreditation-page", "https://example.org/accreditation-page"),
      source_page_modified = c("2024-03-02", "2024-03-02", "2024-03-02", "2024-03-02"),
      display_action = c(TRUE, FALSE, TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "accreditation", "accreditation_tracker_actions_joined.csv"),
    na = ""
  )

  hybrid_scraper_key <- build_accreditation_action_source_key(
    unitid = "100",
    institution_name = "Example University",
    accreditor = "MSCHE",
    action_type = "adverse_action",
    action_label = paste0(
      "Staff acted on behalf of the Commission to acknowledge receipt of the notification, ",
      "dated April 2, 2026, of the institution's intent to change their primary accreditor, ",
      "voluntarily surrender accreditation, and terminate membership. ",
      "To accept the institution's request to voluntarily surrender its accreditation and intent to ",
      "terminate its membership effective May 31, 2026."
    ),
    action_date = "2026-04-24",
    source_url = "https://example.org/accreditation",
    source_page_url = "https://example.org/accreditation-page"
  )
  hybrid_dapip_key <- build_accreditation_action_source_key(
    unitid = "100",
    institution_name = "Example University",
    accreditor = "MSCHE",
    action_type = "adverse_action",
    action_label = "Voluntary Withdrawal Received",
    action_date = "2026-04-06",
    source_page_url = "https://ope.ed.gov/dapip/#/institution-profile/100001",
    file_id = "0"
  )
  warning_dapip_key <- build_accreditation_action_source_key(
    unitid = "100",
    institution_name = "Example University",
    accreditor = "MSCHE",
    action_type = "warning",
    action_label = "Warning",
    action_date = "2025-12-08",
    source_page_url = "https://ope.ed.gov/dapip/#/institution-profile/100001",
    file_id = "22474"
  )
  same_day_dapip_key <- build_accreditation_action_source_key(
    unitid = "100",
    institution_name = "Example University",
    accreditor = "MSCHE",
    action_type = "removed",
    action_label = "Accreditation Reaffirmed: Warning Removed",
    action_date = "2025-06-26",
    source_page_url = "https://ope.ed.gov/dapip/#/institution-profile/100001",
    file_id = "0"
  )
  same_day_scraper_key <- build_accreditation_action_source_key(
    unitid = "100",
    institution_name = "Example University",
    accreditor = "MSCHE",
    action_type = "monitoring",
    action_label = paste0(
      "To acknowledge receipt of the monitoring report requested by the Commission action of June 27, 2024. ",
      "To reaffirm accreditation because the institution is now in compliance with Standard V (Educational Effectiveness Assessment). ",
      "To request a monitoring report, due March 2, 2026, demonstrating sustainability of corrective measures."
    ),
    action_date = "2025-06-26",
    source_url = "https://example.org/msche-monitoring",
    source_page_url = "https://example.org/accreditation-page"
  )

  readr::write_csv(
    data.frame(
      unitid = c("100", "100", "100"),
      institution_name_raw = c("Example University", "Example University", "Example University"),
      institution_state_raw = c("Massachusetts", "Massachusetts", "Massachusetts"),
      accreditor = c("Middle States Commission on Higher Education", "Middle States Commission on Higher Education", "Middle States Commission on Higher Education"),
      action_type = c("adverse_action", "warning", "removed"),
      action_label_raw = c("Voluntary Withdrawal Received", "Warning", "Accreditation Reaffirmed: Warning Removed"),
      action_status = c("active", "active", "resolved"),
      action_date = c("2026-04-06", "2025-12-08", "2025-06-26"),
      action_year = c("2026", "2025", "2025"),
      action_scope = c("", "", ""),
      source_url = c("", "", ""),
      source_title = c("DAPIP Institutional Accreditation Action", "DAPIP Institutional Accreditation Action", "DAPIP Institutional Accreditation Action"),
      notes = c("Voluntary Withdrawal Received", "Warning", "Accreditation Reaffirmed: Warning Removed"),
      source_page_url = c("https://ope.ed.gov/dapip/#/institution-profile/100001", "https://ope.ed.gov/dapip/#/institution-profile/100001", "https://ope.ed.gov/dapip/#/institution-profile/100001"),
      source_page_modified = c("", "", ""),
      file_id = c("0", "22474", "0"),
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "accreditation", "dapip_action_rows_filtered.csv"),
    na = ""
  )

  readr::write_csv(
    data.frame(
      unitid = c("100", "100", "100", "100"),
      institution_name = c("Example University", "Example University", "Example University", "Example University"),
      accreditor = c("MSCHE", "MSCHE", "MSCHE", "MSCHE"),
      scraper_action_type = c("adverse_action", "", "monitoring", ""),
      scraper_action_label = c(
        paste0(
          "Staff acted on behalf of the Commission to acknowledge receipt of the notification, ",
          "dated April 2, 2026, of the institution's intent to change their primary accreditor, ",
          "voluntarily surrender accreditation, and terminate membership. ",
          "To accept the institution's request to voluntarily surrender its accreditation and intent to ",
          "terminate its membership effective May 31, 2026."
        ),
        "",
        paste0(
          "To acknowledge receipt of the monitoring report requested by the Commission action of June 27, 2024. ",
          "To reaffirm accreditation because the institution is now in compliance with Standard V (Educational Effectiveness Assessment). ",
          "To request a monitoring report, due March 2, 2026, demonstrating sustainability of corrective measures."
        ),
        ""
      ),
      scraper_action_date = c("2026-04-24", "", "2025-06-26", ""),
      dapip_action_type = c("adverse_action", "warning", "", "removed"),
      dapip_action_label = c("Voluntary Withdrawal Received", "Warning", "", "Accreditation Reaffirmed: Warning Removed"),
      dapip_action_date = c("2026-04-06", "2025-12-08", "", "2025-06-26"),
      audit_result = c("match", "dapip_only", "scraper_only", "dapip_only"),
      date_delta_days = c("18", "", "", ""),
      scraper_source_url = c("https://example.org/accreditation", "", "https://example.org/msche-monitoring", ""),
      dapip_source_page_url = c("https://ope.ed.gov/dapip/#/institution-profile/100001", "https://ope.ed.gov/dapip/#/institution-profile/100001", "", "https://ope.ed.gov/dapip/#/institution-profile/100001"),
      dapip_file_id = c("0", "22474", "", "0"),
      notes = c("public_action_code", "public_action_code", "", "public_action_code"),
      scraper_public_keep = c(TRUE, FALSE, TRUE, FALSE),
      scraper_public_reason = c("closure_teachout_or_exit_signal", "routine_or_non_reader_signal", "monitoring_or_notice_signal", "routine_or_non_reader_signal"),
      dapip_public_keep = c(TRUE, TRUE, FALSE, TRUE),
      dapip_public_reason = c("closure_teachout_or_exit_signal", "core_sanction_signal", "", "left_distress_stage"),
      public_table_strategy = c("hybrid_keep", "dapip_backed_keep", "scraper_backed_keep", "dapip_backed_keep"),
      hybrid_candidate = c(TRUE, FALSE, FALSE, FALSE),
      hybrid_reason = c("scraper_detail_enriches_generic_or_fileless_dapip", "", "", ""),
      public_action_family = c("withdrawal_or_loss", "warning", "monitoring_or_notice", "removed"),
      scraper_source_key = c(hybrid_scraper_key, "", same_day_scraper_key, ""),
      dapip_source_key = c(hybrid_dapip_key, warning_dapip_key, "", same_day_dapip_key),
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "accreditation", "dapip_vs_scraper_audit.csv"),
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
  assert_identical(as.integer(metadata$latest_year), 2025L)
  assert_true(!is.null(metadata$files) && is.list(metadata$files),
    "metadata$files should be a named list.")
  required_meta_files <- c("schools_index", "college_cuts", "accreditation",
    "research_funding", "download", "closure_status", "hcm2", "federal_composite_scores")
  for (f in required_meta_files) {
    assert_true(f %in% names(metadata$files),
      sprintf("metadata$files should contain '%s'.", f))
    assert_true(is.character(metadata$files[[f]]) && nzchar(metadata$files[[f]]),
      sprintf("metadata$files[['%s']] should be a non-empty string path.", f))
  }

  # ── schools_index schema ────────────────────────────────────────────────────
  assert_identical(nrow(schools_index), 1L)
  assert_identical(schools_index$institution_unique_name[[1]], "Example University | Boston | Massachusetts")

  # No duplicate unitids — duplicate keys would break per-school JSON lookups.
  dup_unitids <- schools_index$unitid[duplicated(schools_index$unitid)]
  assert_true(length(dup_unitids) == 0L,
    sprintf("schools_index should have no duplicate unitids (found: %s).",
      paste(unique(dup_unitids), collapse = ", ")))

  # Required top-level fields on each school entry.
  required_index_fields <- c("unitid", "institution_name", "institution_unique_name",
    "state", "city", "control_label", "category")
  for (f in required_index_fields) {
    assert_true(f %in% names(schools_index),
      sprintf("schools_index should have column '%s'.", f))
  }

  # All unitids in schools_index should correspond to actual school JSON files.
  for (uid in schools_index$unitid) {
    school_path <- file.path(fixture_root, "data", "schools", paste0(uid, ".json"))
    assert_true(file.exists(school_path),
      sprintf("School JSON file should exist for unitid %s.", uid))
  }

  # ── Index file schemas ───────────────────────────────────────────────────────
  # Index files are used by the frontend for filtering/search — they must be arrays.
  for (idx_list in list(
        list(obj = cuts_index, name = "cuts_index"),
        list(obj = accred_index, name = "accreditation_index"),
        list(obj = research_index, name = "research_funding_index"))) {
    idx <- idx_list$obj
    nm <- idx_list$name
    assert_true(is.list(idx) && length(idx) > 0,
      sprintf("%s should be a non-empty list.", nm))
  }

  # ── Individual school JSON schema ────────────────────────────────────────────
  first_cut_index <- cuts_index[[1]]
  assert_true("landing_cuts" %in% names(first_cut_index),
    "cuts_index entries should expose landing_cuts for the landing-page table.")
  assert_true(is.list(first_cut_index$landing_cuts),
    "cuts_index$landing_cuts should be a list.")
  if (length(first_cut_index$landing_cuts) > 0) {
    first_cut_row_names <- if (is.data.frame(first_cut_index$landing_cuts)) {
      names(first_cut_index$landing_cuts)
    } else {
      names(first_cut_index$landing_cuts[[1]])
    }
    assert_true(all(c("program_name", "announcement_date", "announcement_year", "source_url") %in% first_cut_row_names),
      "landing_cuts rows should include the compact row-level fields used by cuts.html.")
  }

  first_accred_index <- accred_index[[1]]
  assert_true("landing_actions" %in% names(first_accred_index),
    "accreditation_index entries should expose landing_actions for the landing-page table.")
  assert_true(is.list(first_accred_index$landing_actions),
    "accreditation_index$landing_actions should be a list.")
  if (length(first_accred_index$landing_actions) > 0) {
    first_action_row_names <- if (is.data.frame(first_accred_index$landing_actions)) {
      names(first_accred_index$landing_actions)
    } else {
      names(first_accred_index$landing_actions[[1]])
    }
    assert_true(all(c("accreditor", "action_label", "action_date", "source_url") %in% first_action_row_names),
      "landing_actions rows should include the compact row-level fields used by accreditation.html.")
  }

  assert_identical(school_file$profile$institution_name, "Example University")

  # Top-level sections must be present.
  required_sections <- c("unitid", "generated_at", "profile", "summary")
  for (sec in required_sections) {
    assert_true(sec %in% names(school_file),
      sprintf("School JSON should have top-level field '%s'.", sec))
  }

  # unitid must be a non-empty character string.
  assert_true(is.character(school_file$unitid) && nzchar(school_file$unitid),
    "school_file$unitid should be a non-empty character string.")
  # generated_at must be a date string.
  assert_true(is.character(school_file$generated_at) && nzchar(school_file$generated_at),
    "school_file$generated_at should be a non-empty date string.")

  # profile section — all fields must be non-empty strings.
  required_profile_fields <- c("institution_name", "institution_unique_name",
    "state", "city", "control_label", "sector", "category")
  for (f in required_profile_fields) {
    assert_true(f %in% names(school_file$profile),
      sprintf("school_file$profile should have field '%s'.", f))
    val <- school_file$profile[[f]]
    assert_true(!is.null(val) && (is.character(val) || is.na(val)) && (!is.character(val) || nzchar(val)),
      sprintf("school_file$profile$%s should be a non-empty string or NA.", f))
  }

  # urbanization is required even if NA.
  assert_true("urbanization" %in% names(school_file$profile),
    "school_file$profile should have 'urbanization'.")

  # summary section — required fields present.
  required_summary_fields <- c("enrollment_pct_change_5yr", "revenue_pct_change_5yr",
    "tuition_dependence_pct", "ended_year_at_loss", "losses_last_3_of_5",
    "latest_year", "share_grad_students", "federal_grants_contracts_pell_adjusted_pct_core_revenue",
    "state_funding_pct_core_revenue", "endowment_pct_change_5yr", "unfunded_discount_rate", "unfunded_discount_pct_change_5yr")
  for (f in required_summary_fields) {
    assert_true(f %in% names(school_file$summary),
      sprintf("school_file$summary should have field '%s'.", f))
  }

  # Numeric summary fields must actually be numeric (not character or null).
  numeric_summary_fields <- c("tuition_dependence_pct", "graduation_rate_6yr",
    "pct_international_all", "share_grad_students",
    "federal_grants_contracts_pell_adjusted_pct_core_revenue",
    "state_funding_pct_core_revenue")
  for (f in numeric_summary_fields) {
    if (f %in% names(school_file$summary)) {
      val <- school_file$summary[[f]]
      assert_true(is.numeric(val) || is.integer(val) || is.na(val),
        sprintf("school_file$summary$%s should be numeric or NA (got %s).", f, typeof(val)))
    }
  }

  # series section — year-series arrays used by the site charts.
  assert_true("series" %in% names(school_file),
    "school_file should have a 'series' section.")
  required_series_fields <- c("revenue_total_adjusted", "expenses_total_adjusted",
    "enrollment_headcount_total", "endowment_value_adjusted", "unfunded_discount_rate")
  for (f in required_series_fields) {
    assert_true(f %in% names(school_file$series),
      sprintf("school_file$series should have field '%s'.", f))
    # Each series entry should be a list with at least 'year' and 'value'.
    entry <- school_file$series[[f]]
    assert_true(is.list(entry) && "year" %in% names(entry),
      sprintf("school_file$series$%s should be a list with a 'year' array.", f))
  }
  assert_identical(as.integer(school_file$summary$latest_year), 2025L)
  assert_identical(as.integer(school_file$series$revenue_total_adjusted$year), c(2024L, 2025L))
  assert_identical(as.integer(school_file$series$expenses_total_adjusted$year), c(2024L, 2025L))

  # ── Section export schemas ───────────────────────────────────────────────────
  assert_equal(school_file$summary$tuition_dependence_pct, 30)
  assert_equal(school_file$summary$unfunded_discount_rate, 32)
  assert_equal(school_file$summary$unfunded_discount_pct_change_5yr, 6)
  assert_equal(school_file$summary$graduation_rate_6yr, 72)
  assert_equal(school_file$summary$median_earnings_10yr, 56000)
  assert_identical(as.integer(school_file$series$unfunded_discount_rate$year), c(2024L, 2025L))
  assert_equal(as.numeric(school_file$series$unfunded_discount_rate$value), c(28, 32))

  # ── college_cuts.json ────────────────────────────────────────────────────────
  assert_identical(length(cuts_export$schools), 1L)
  school_cuts <- cuts_export$schools[[1]]
  # NOTE: with simplifyVector=TRUE and 1 school, $schools[[1]] returns the school object
  # (which may be a data.frame row or a list depending on how jsonlite simplified it).
  # Use the same adaptive approach for cuts.
  if (is.data.frame(school_cuts)) {
    assert_identical(school_cuts$cut_count, 1L)
    assert_true("unitid" %in% names(school_cuts),
      "cuts_export school entry should have 'unitid'.")
    assert_true("latest_cut_date" %in% names(school_cuts),
      "cuts_export school entry should have 'latest_cut_date'.")
    # cuts is a list column inside the school row; access via $cuts[[1]].
    cuts_df <- school_cuts$cuts[[1]]
    assert_true(!is.null(cuts_df) && is.data.frame(cuts_df),
      "cuts should be a data.frame (accessed via $cuts[[1]] for simplified data).")
  } else {
    assert_identical(school_cuts$cut_count, 1L)
    assert_true("unitid" %in% names(school_cuts),
      "cuts_export school entry should have 'unitid'.")
    assert_true("latest_cut_date" %in% names(school_cuts),
      "cuts_export school entry should have 'latest_cut_date'.")
    assert_true("cuts" %in% names(school_cuts) && is.data.frame(school_cuts$cuts),
      "cuts_export school entry should have a 'cuts' data.frame.")
    cuts_df <- school_cuts$cuts
  }
  assert_true(nrow(cuts_df) > 0,
    "cuts_export should have at least one cut record.")
  for (f in c("announcement_date", "cut_type", "status", "program_name")) {
    assert_true(f %in% names(cuts_df),
      sprintf("cuts data.frame should have column '%s'.", f))
  }

  # ── accreditation.json ──────────────────────────────────────────────────────
  assert_identical(length(accreditation_export$schools), 1L)
  school_accred <- accreditation_export$schools[[1]]
  assert_identical(school_accred$latest_status$action_count, 3L)
  assert_identical(school_accred$latest_status$latest_action_date, "2026-04-24")
  assert_true(grepl("voluntarily surrender", school_accred$latest_status$action_labels, ignore.case = TRUE),
    "Latest accreditation status labels should retain the hybrid voluntary-surrender text.")
  assert_true("latest_status" %in% names(school_accred),
    "accreditation_export school entry should have 'latest_status'.")
  for (f in c("accreditors", "action_labels", "has_active_warning", "has_active_adverse_action", "action_count")) {
    assert_true(f %in% names(school_accred$latest_status),
      sprintf("latest_status should have field '%s'.", f))
  }
  assert_true(!is.null(school_accred$actions) && is.data.frame(school_accred$actions),
    "accreditation_export actions should be a data.frame.")
  assert_identical(nrow(school_accred$actions), 3L)
  assert_identical(school_accred$actions$action_label[[1]], paste0(
    "Staff acted on behalf of the Commission to acknowledge receipt of the notification, ",
    "dated April 2, 2026, of the institution's intent to change their primary accreditor, ",
    "voluntarily surrender accreditation, and terminate membership. ",
    "To accept the institution's request to voluntarily surrender its accreditation and intent to ",
    "terminate its membership effective May 31, 2026."
  ))
  assert_identical(school_accred$actions$action_label_short[[1]], "Voluntarily Surrendered Accreditation")
  assert_identical(school_accred$actions$action_date[[1]], "2026-04-24")
  assert_true(any(school_accred$actions$action_label_short == "Accreditation Reaffirmed: Warning Removed"),
    "Same-day DAPIP removal row should be retained in the export.")
  assert_true(!any(grepl("monitoring report requested by the Commission action of June 27, 2024", school_accred$actions$action_label, fixed = TRUE)),
    "Pure scraper same-day MSCHE monitoring row should be dropped when a DAPIP row exists on the same date.")
  assert_true(any(school_accred$actions$public_table_strategy == "dapip_backed_keep"),
    "Accreditation export should retain DAPIP-backed rows selected by the audited public-table strategy.")
  assert_true(any(school_accred$actions$public_table_strategy == "hybrid_keep"),
    "Accreditation export should retain hybrid rows selected by the audited public-table strategy.")
  assert_true("display_action" %in% names(school_accred$actions),
    "display_action should be exported with accreditation actions.")
  assert_true(isTRUE(school_accred$actions$display_action[[1]]),
    "displayed accreditation action should carry display_action=true.")
  assert_true(isTRUE(school_accred$latest_status$has_active_adverse_action),
    "voluntary surrender row should count as an active adverse action.")

  accred_index_row <- if (is.data.frame(accred_index)) accred_index[1, , drop = FALSE] else accred_index[[1]]
  if (is.data.frame(accred_index_row)) {
    assert_identical(accred_index_row$action_count[[1]], 3L)
    assert_identical(accred_index_row$latest_action_date[[1]], "2026-04-24")
    assert_identical(accred_index_row$latest_action_label[[1]], "Voluntarily Surrendered Accreditation")
  } else {
    assert_identical(accred_index_row$action_count, 3L)
    assert_identical(accred_index_row$latest_action_date, "2026-04-24")
    assert_identical(accred_index_row$latest_action_label, "Voluntarily Surrendered Accreditation")
  }

  # ── research_funding.json ───────────────────────────────────────────────────
  assert_identical(length(research_export$schools), 1L)
  school_research <- research_export$schools[[1]]
  # With simplifyVector=TRUE and 1 school: school_research is a list, grants is a data.frame.
  # grants[[1]] is the first column (character), not a row.
  grants_df <- school_research$grants
  assert_true(!is.null(grants_df) && is.data.frame(grants_df),
    "research_export grants should be a data.frame.")
  assert_true(nrow(grants_df) > 0,
    "research_export should have at least one grant record.")
  for (f in c("agency", "grant_id", "status")) {
    assert_true(f %in% names(grants_df),
      sprintf("grants data.frame should have column '%s'.", f))
  }

  # ── Download CSV ────────────────────────────────────────────────────────────
  assert_identical(nrow(download_csv), 1L)
  assert_identical(as.integer(download_csv$year[[1]]), as.integer(metadata$latest_year))
  assert_true("unitid" %in% names(download_csv), "Download CSV must have 'unitid' column.")
  assert_true("year" %in% names(download_csv), "Download CSV must have 'year' column.")
  assert_true(
    !any(grepl("\\.(x|y)$", names(download_csv))),
    paste("Download CSV should not contain suffixed join-collision columns:", paste(names(download_csv), collapse = ", "))
  )
  # The download CSV must have more columns than just unitid + year.
  assert_true(ncol(download_csv) > 5,
    sprintf("Download CSV should have substantial data columns (found %d).", ncol(download_csv)))
})

run_test("Web export pipeline drops generic HLC current-status rows when a dated action exists", function() {
  fixture_root <- tempfile("web-export-hlc-dedupe-")
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
    unitid = c("200", "200"),
    institution_name = c("Example HLC University", "Example HLC University"),
    institution_unique_name = c("Example HLC University | Springfield | Ohio", "Example HLC University | Springfield | Ohio"),
    city = c("Springfield", "Springfield"),
    state = c("Ohio", "Ohio"),
    control_label = c("Private not-for-profit", "Private not-for-profit"),
    sector = c("Private not-for-profit, 4-year or above", "Private not-for-profit, 4-year or above"),
    category = c("Degree-granting, primarily baccalaureate or above", "Degree-granting, primarily baccalaureate or above"),
    urbanization = c("City", "City"),
    religious_affiliation = c(NA, NA),
    all_programs_distance_education = c("No", "No"),
    year = c("2024", "2025"),
    enrollment_pct_change_5yr = c("-4", "-4"),
    enrollment_decline_last_3_of_5 = c("No", "No"),
    revenue_pct_change_5yr = c("1", "1"),
    net_tuition_per_fte_change_5yr = c("2", "2"),
    staff_total_headcount_pct_change_5yr = c("0", "0"),
    staff_instructional_headcount_pct_change_5yr = c("0", "0"),
    students_per_instructional_staff_fte = c("12", "12"),
    sector_median_students_per_instructional_staff_fte = c("13", "13"),
    ended_year_at_loss = c("No", "No"),
    losses_last_3_of_5 = c("No", "No"),
    loss_years_last_10 = c("0", "0"),
    tuition_dependence_pct = c("45", "45"),
    sector_median_tuition_dependence_pct = c("40", "40"),
    tuition_dependence_vs_sector_median_sentence = c("Sample sentence 2024", "Sample sentence 2025"),
    discount_rate = c("0.35", "0.36"),
    discount_pct_change_5yr = c("2", "2"),
    share_grad_students = c("0.15", "0.15"),
    research_expense = c("0", "0"),
    research_expense_per_fte = c("0", "0"),
    research_expense_pct_core_expenses = c("0", "0"),
    sector_research_spending_n = c("0", "0"),
    sector_research_spending_positive_n = c("0", "0"),
    sector_research_spending_reporting_share_pct = c("0", "0"),
    sector_median_research_expense_per_fte_positive = c("0", "0"),
    pct_international_all = c("0.01", "0.01"),
    pct_international_undergraduate = c("0.01", "0.01"),
    pct_international_graduate = c("0.02", "0.02"),
    international_student_count_change_5yr = c("0", "0"),
    international_enrollment_pct_change_5yr = c("0", "0"),
    international_students_sentence = c("In 2024, 1% of students were international.", "In 2025, 1% of students were international."),
    federal_loan_pct_most_recent = c("40", "40"),
    federal_grants_contracts_pell_adjusted_pct_core_revenue = c("0.10", "0.10"),
    state_funding_pct_core_revenue = c("0.02", "0.02"),
    federal_grants_contracts_pell_adjusted_pct_change_5yr = c("0", "0"),
    state_funding_pct_change_5yr = c("0", "0"),
    endowment_pct_change_5yr = c("0", "0"),
    endowment_spending_current_use_pct_core_revenue = c("0.01", "0.01"),
    revenue_total = c("100", "100"),
    expenses_total = c("90", "90"),
    revenue_total_adjusted = c("100", "100"),
    expenses_total_adjusted = c("90", "90"),
    net_tuition_per_fte_adjusted = c("3", "3"),
    enrollment_headcount_total = c("100", "100"),
    enrollment_nonresident_total = c("1", "1"),
    enrollment_nonresident_undergrad = c("1", "1"),
    enrollment_nonresident_graduate = c("0", "0"),
    staff_headcount_total = c("20", "20"),
    staff_headcount_instructional = c("8", "8"),
    endowment_value_adjusted = c("50", "50"),
    endowment_spending_current_use = c("1", "1"),
    federal_grants_contracts_pell_adjusted_adjusted = c("10", "10"),
    state_funding_adjusted = c("2", "2"),
    graduation_rate_6yr = c("60", "60"),
    median_earnings_10yr = c("50000", "50000"),
    median_debt_completers = c("20000", "20000"),
    outcomes_data_available = c(TRUE, TRUE),
    scorecard_data_updated = c("2024-01-01", "2024-01-01"),
    ipeds_graduation_rate_year = c("2024", "2024"),
    ipeds_graduation_rate_label = c("2024 cohort", "2024 cohort"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(canonical_df, canonical_path, na = "")

  readr::write_csv(
    data.frame(
      matched_unitid = "200",
      announcement_date = "2024-02-15",
      announcement_year = "2024",
      in_financial_tracker = "TRUE",
      tracker_institution_name = "Example HLC University",
      institution_name_collegecuts = "Example HLC University",
      institution_state_full = "Ohio",
      institution_city = "Springfield",
      tracker_control_label = "Private not-for-profit",
      institution_control = "Private not-for-profit",
      tracker_category = "Degree-granting, primarily baccalaureate or above",
      faculty_affected = "1",
      notes = "One faculty position affected",
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
      unitid = "200",
      institution_name = "Example HLC University",
      state = "Ohio",
      city = "Springfield",
      control_label = "Private not-for-profit",
      category = "Degree-granting, primarily baccalaureate or above",
      accreditors = "HLC",
      latest_action_date = "2026-04-28",
      latest_action_year = "2026",
      action_labels = paste(
        "On Probation",
        "Placed on Probation. The institution was notified of this action on November 11, 2025.",
        "Approved the institution's provisional plan and teach-out agreements.",
        sep = "; "
      ),
      active_actions = "probation; adverse_action",
      has_active_warning = FALSE,
      has_active_warning_or_notice = FALSE,
      has_active_adverse_action = TRUE,
      action_count = 3L,
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "accreditation", "accreditation_tracker_institution_summary.csv"),
    na = ""
  )

  readr::write_csv(
    data.frame(
      unitid = c("200", "200", "200"),
      institution_name = c("Example HLC University", "Example HLC University", "Example HLC University"),
      state = c("Ohio", "Ohio", "Ohio"),
      city = c("Springfield", "Springfield", "Springfield"),
      control_label = c("Private not-for-profit", "Private not-for-profit", "Private not-for-profit"),
      category = c(
        "Degree-granting, primarily baccalaureate or above",
        "Degree-granting, primarily baccalaureate or above",
        "Degree-granting, primarily baccalaureate or above"
      ),
      accreditor = c("HLC", "HLC", "HLC"),
      action_type = c("probation", "probation", "adverse_action"),
      action_label_raw = c(
        "On Probation",
        "Placed on Probation. The institution was notified of this action on November 11, 2025. Information was posted for the public on November 12, 2025.",
        "Approved the institution's provisional plan and teach-out agreements."
      ),
      action_status = c("active", "active", "active"),
      action_date = c("", "2025-11-01", "2024-11-01"),
      action_year = c("", "2025", "2024"),
      notes = c(
        "On Probation",
        "Placed on Probation",
        "Approved the institution's provisional plan and teach-out agreements."
      ),
      source_url = c(
        "https://www.hlcommission.org/institution/1610/",
        "https://www.hlcommission.org/for-students/accreditation-actions/november-2025-actions/",
        "https://www.hlcommission.org/for-students/accreditation-actions/november-2024/"
      ),
      source_title = c(
        "Accreditation Actions | The Higher Learning Commission - On Probation",
        "November 2025 Actions | The Higher Learning Commission",
        "November 2024 Actions | The Higher Learning Commission"
      ),
      source_page_url = c(
        "https://www.hlcommission.org/for-students/accreditation-actions/",
        "https://www.hlcommission.org/for-students/accreditation-actions/november-2025-actions/",
        "https://www.hlcommission.org/for-students/accreditation-actions/november-2024/"
      ),
      source_page_modified = c("2026-04-28", "2026-03-03", "2025-04-03"),
      display_action = c(TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "accreditation", "accreditation_tracker_actions_joined.csv"),
    na = ""
  )

  detailed_probation_key <- build_accreditation_action_source_key(
    unitid = "200",
    institution_name = "Example HLC University",
    accreditor = "HLC",
    action_type = "probation",
    action_label = "Placed on Probation. The institution was notified of this action on November 11, 2025. Information was posted for the public on November 12, 2025.",
    action_date = "2025-11-01",
    source_url = "https://www.hlcommission.org/for-students/accreditation-actions/november-2025-actions/",
    source_page_url = "https://www.hlcommission.org/for-students/accreditation-actions/november-2025-actions/"
  )
  teachout_key <- build_accreditation_action_source_key(
    unitid = "200",
    institution_name = "Example HLC University",
    accreditor = "HLC",
    action_type = "adverse_action",
    action_label = "Approved the institution's provisional plan and teach-out agreements.",
    action_date = "2024-11-01",
    source_url = "https://www.hlcommission.org/for-students/accreditation-actions/november-2024/",
    source_page_url = "https://www.hlcommission.org/for-students/accreditation-actions/november-2024/"
  )

  readr::write_csv(
    data.frame(
      unitid = character(),
      institution_name_raw = character(),
      institution_state_raw = character(),
      accreditor = character(),
      action_type = character(),
      action_label_raw = character(),
      action_status = character(),
      action_date = character(),
      action_year = character(),
      action_scope = character(),
      source_url = character(),
      source_title = character(),
      notes = character(),
      source_page_url = character(),
      source_page_modified = character(),
      file_id = character(),
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "accreditation", "dapip_action_rows_filtered.csv"),
    na = ""
  )

  readr::write_csv(
    data.frame(
      unitid = c("200", "200"),
      institution_name = c("Example HLC University", "Example HLC University"),
      accreditor = c("HLC", "HLC"),
      scraper_action_type = c("probation", "adverse_action"),
      scraper_action_label = c(
        "Placed on Probation. The institution was notified of this action on November 11, 2025. Information was posted for the public on November 12, 2025.",
        "Approved the institution's provisional plan and teach-out agreements."
      ),
      scraper_action_date = c("2025-11-01", "2024-11-01"),
      dapip_action_type = c("", ""),
      dapip_action_label = c("", ""),
      dapip_action_date = c("", ""),
      audit_result = c("scraper_only", "scraper_only"),
      date_delta_days = c("", ""),
      scraper_source_url = c(
        "https://www.hlcommission.org/for-students/accreditation-actions/november-2025-actions/",
        "https://www.hlcommission.org/for-students/accreditation-actions/november-2024/"
      ),
      dapip_source_page_url = c("", ""),
      dapip_file_id = c("", ""),
      notes = c("", ""),
      scraper_public_keep = c(TRUE, TRUE),
      scraper_public_reason = c("core_sanction_signal", "closure_teachout_or_exit_signal"),
      dapip_public_keep = c(FALSE, FALSE),
      dapip_public_reason = c("", ""),
      public_table_strategy = c("scraper_backed_keep", "scraper_backed_keep"),
      hybrid_candidate = c(FALSE, FALSE),
      hybrid_reason = c("", ""),
      public_action_family = c("probation", "withdrawal_or_loss"),
      scraper_source_key = c("stale-key-should-be-recomputed", teachout_key),
      dapip_source_key = c("", ""),
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "accreditation", "dapip_vs_scraper_audit.csv"),
    na = ""
  )

  readr::write_csv(
    data.frame(
      accreditor = "HLC",
      source_url = "https://www.hlcommission.org/for-students/accreditation-actions/",
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "accreditation", "accreditation_tracker_source_coverage.csv"),
    na = ""
  )

  readr::write_csv(
    data.frame(
      matched_unitid = "200",
      display_name = "Example HLC University",
      display_city = "Springfield",
      display_state = "Ohio",
      tracker_control_label = "Private not-for-profit",
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
      institution_key = "unitid:200",
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "grant_witness", "grant_witness_higher_ed_institution_summary.csv"),
    na = ""
  )

  readr::write_csv(
    data.frame(
      matched_unitid = "200",
      tracker_institution_name = "Example HLC University",
      tracker_state = "Ohio",
      agency = "nih",
      grant_id = "G-1",
      grant_id_core = "G-1",
      status = "terminated",
      organization_name = "Example HLC University",
      organization_city = "Springfield",
      organization_state = "Ohio",
      organization_type = "Higher education",
      project_title = "Example project",
      project_abstract = "Example abstract",
      start_date = "2024-01-01",
      original_end_date = "2025-12-31",
      termination_date = "2024-04-01",
      award_value = "100000",
      award_outlaid = "50000",
      award_remaining = "50000",
      remaining_field = "award_remaining",
      currently_disrupted = TRUE,
      likely_higher_ed = TRUE,
      source_url = "https://example.org/grant",
      detail_url = "https://example.org/grant-detail",
      stringsAsFactors = FALSE
    ),
    file.path(fixture_root, "data_pipelines", "grant_witness", "grant_witness_grant_level_joined.csv"),
    na = ""
  )

  readr::write_csv(
    data.frame(
      unitid = "200",
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

  export_env <- new.env(parent = globalenv())
  sys.source(file.path(root, "scripts", "build_web_exports.R"), envir = export_env)
  export_env$main(c("--input", canonical_path, "--output-dir", fixture_root))

  accreditation_export <- jsonlite::read_json(file.path(fixture_root, "data", "accreditation.json"), simplifyVector = TRUE)
  accred_index <- jsonlite::read_json(file.path(fixture_root, "data", "accreditation_index.json"), simplifyVector = TRUE)

  school_accred <- accreditation_export$schools[[1]]
  assert_identical(nrow(school_accred$actions), 2L)
  assert_true(!any(school_accred$actions$action_label == "On Probation"),
    "Generic HLC current-status row should be dropped when a dated probation action exists.")
  assert_true(any(grepl("^Placed on Probation", school_accred$actions$action_label)),
    "Detailed dated HLC probation row should be retained.")
  assert_identical(school_accred$latest_status$action_count, 2L)
  assert_identical(school_accred$latest_status$latest_action_date, "2025-11-01")

  accred_index_row <- if (is.data.frame(accred_index)) accred_index[1, , drop = FALSE] else accred_index[[1]]
  if (is.data.frame(accred_index_row)) {
    assert_identical(accred_index_row$action_count[[1]], 2L)
    assert_identical(accred_index_row$latest_action_date[[1]], "2025-11-01")
    assert_identical(accred_index_row$latest_action_label[[1]], "Placed on Probation")
    landing_actions <- accred_index_row$landing_actions[[1]]
  } else {
    assert_identical(accred_index_row$action_count, 2L)
    assert_identical(accred_index_row$latest_action_date, "2025-11-01")
    assert_identical(accred_index_row$latest_action_label, "Placed on Probation")
    landing_actions <- accred_index_row$landing_actions
  }
  if (is.data.frame(landing_actions)) {
    assert_true(!any(landing_actions$action_label == "On Probation"),
      "Landing actions should not include the generic HLC current-status duplicate.")
  } else {
    assert_true(!any(vapply(landing_actions, function(action) identical(action$action_label, "On Probation"), logical(1))),
      "Landing actions should not include the generic HLC current-status duplicate.")
  }
})
