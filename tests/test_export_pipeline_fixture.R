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
    "state_funding_pct_core_revenue", "endowment_pct_change_5yr")
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
    "enrollment_headcount_total", "endowment_value_adjusted")
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
  assert_equal(school_file$summary$graduation_rate_6yr, 72)
  assert_equal(school_file$summary$median_earnings_10yr, 56000)

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
  assert_identical(school_accred$latest_status$action_count, 1L)
  assert_true("latest_status" %in% names(school_accred),
    "accreditation_export school entry should have 'latest_status'.")
  for (f in c("accreditors", "action_labels", "has_active_warning", "action_count")) {
    assert_true(f %in% names(school_accred$latest_status),
      sprintf("latest_status should have field '%s'.", f))
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
