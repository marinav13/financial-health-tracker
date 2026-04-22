run_test("Grant Witness join pipeline fixture", function() {
  fixture_root <- tempfile("grant-witness-fixture-")
  dir.create(fixture_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  dirs <- c(
    file.path(fixture_root, "scripts"),
    file.path(fixture_root, "scripts", "shared"),
    file.path(fixture_root, "data_pipelines"),
    file.path(fixture_root, "data_pipelines", "grant_witness"),
    file.path(fixture_root, "data_pipelines", "grant_witness", "cache"),
    file.path(fixture_root, "data_pipelines", "grant_witness", "analysis")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  for (nm in c("shared/utils.R", "shared/ipeds_paths.R", "shared/grant_witness_helpers.R")) {
    file.copy(
      file.path(root, "scripts", nm),
      file.path(fixture_root, "scripts", nm),
      overwrite = TRUE
    )
  }
  file.copy(
    file.path(root, "scripts", "build_grant_witness_join.R"),
    file.path(fixture_root, "scripts", "build_grant_witness_join.R"),
    overwrite = TRUE
  )

  financial_input <- file.path(fixture_root, "fixture_financial.csv")
  output_prefix <- file.path(fixture_root, "data_pipelines", "grant_witness", "fixture_grant_witness")
  cache_dir <- file.path(fixture_root, "data_pipelines", "grant_witness", "cache")
  risky_filter_path <- file.path(
    fixture_root, "data_pipelines", "grant_witness", "analysis",
    "grant_witness_usaspending_risky_continuation_filter.csv"
  )

  financial_df <- data.frame(
    unitid = c("100", "200"),
    institution_name = c("Example University", "Sample College"),
    city = c("Boston", "Springfield"),
    state = c("Massachusetts", "Massachusetts"),
    control_label = c("Public", "Private not-for-profit"),
    category = c("R1", "Baccalaureate"),
    year = c(2024, 2024),
    stringsAsFactors = FALSE
  )
  readr::write_csv(financial_df, financial_input, na = "")

  nih_df <- data.frame(
    full_award_number = c("NIH-001", "NIH-002", "NIH-003"),
    core_award_number = c("NIH-001", "NIH-002", "NIH-003"),
    status = c("terminated", "terminated", "terminated"),
    org_name = c("Example University", "Sample College", "Sample College"),
    org_state = c("MA", "MA", "MA"),
    org_city = c("Boston", "Springfield", "Springfield"),
    org_type = c("University", "College", "College"),
    org_traits = c(NA, NA, NA),
    project_title = c("Immune Response Study", "Campus Health Study", "Corrected Zero Study"),
    abstract_text = c("Research abstract", "More research", "Stale amount"),
    targeted_start_date = c("2024-01-01", "2024-02-01", "2024-03-01"),
    targeted_end_date = c("2025-12-31", "2025-12-31", "2025-12-31"),
    termination_date = c("2025-01-15", "2025-01-20", "2025-02-01"),
    reinstated_est_date = c(NA, NA, NA),
    total_award = c(1000000, 500000, 48974),
    total_estimated_outlays = c(400000, 100000, 48973.15),
    total_estimated_remaining = c(600000, 400000, 0.85),
    usaspending_url = c(
      "https://www.usaspending.gov/award/AWARD1",
      "https://www.usaspending.gov/award/AWARD2",
      "https://www.usaspending.gov/award/AWARD3"
    ),
    reporter_url = c(
      "https://grant-witness.us/nih/1",
      "https://grant-witness.us/nih/2",
      "https://grant-witness.us/nih/3"
    ),
    stringsAsFactors = FALSE
  )
  readr::write_csv(nih_df, file.path(cache_dir, "nih_terminations.csv"), na = "")

  empty_specs <- list(
    nsf = c("grant_id", "status", "org_name", "org_state", "org_city", "award_type", "project_title",
            "abstract", "nsf_start_date", "usasp_start_date", "nsf_end_date", "usasp_end_date",
            "termination_date", "reinstatement_date", "estimated_budget", "estimated_outlays",
            "estimated_remaining", "usaspending_url", "nsf_url"),
    epa = c("grant_id", "status", "organization", "org_state", "org_city", "org_type", "project_title",
            "project_description", "start_date", "original_end_date", "termination_date",
            "reinstatement_date", "award_value", "award_outlaid", "award_remaining",
            "usaspending_url", "nggs_url"),
    samhsa = c("grant_id", "status", "org_name", "org_state", "org_city", "org_type", "title",
               "abstract", "project_start_date", "first_award_date", "project_original_end_date",
               "termination_date", "reinstatement_date", "award_value", "award_outlaid",
               "award_remaining", "usaspending_url", "taggs_url"),
    cdc = c("grant_id", "status", "org_name", "org_state", "org_city", "org_type", "title",
            "project_start_date", "project_original_end_date", "termination_date",
            "reinstatement_date", "award_value", "award_outlaid", "award_remaining",
            "usaspending_url", "taggs_url")
  )
  for (agency in names(empty_specs)) {
    empty_df <- as.data.frame(stats::setNames(replicate(length(empty_specs[[agency]]), character(), simplify = FALSE),
                                              empty_specs[[agency]]), stringsAsFactors = FALSE)
    readr::write_csv(empty_df, file.path(cache_dir, paste0(agency, "_terminations.csv")), na = "")
  }

  readr::write_csv(
    data.frame(award_id_string = "AWARD1", stringsAsFactors = FALSE),
    risky_filter_path,
    na = ""
  )
  amount_corrections_path <- file.path(
    fixture_root, "data_pipelines", "grant_witness", "manual_amount_corrections.csv"
  )
  readr::write_csv(
    data.frame(
      award_id_string = "AWARD3",
      award_value = 48973.15,
      award_outlaid = 48973.15,
      award_remaining = 0,
      remaining_field = "usaspending_live_total_obligation_minus_total_outlay",
      stringsAsFactors = FALSE
    ),
    amount_corrections_path,
    na = ""
  )

  setwd(fixture_root)
  join_env <- new.env(parent = globalenv())
  sys.source(file.path(fixture_root, "scripts", "build_grant_witness_join.R"), envir = join_env)
  join_env$main(c(
    "--financial-input", financial_input,
    "--output-prefix", output_prefix,
    "--cache-dir", cache_dir,
    "--amount-corrections", amount_corrections_path,
    "--usaspending-filter", risky_filter_path,
    "--skip-download"
  ))

  grant_path <- paste0(output_prefix, "_grant_level_joined.csv")
  higher_ed_summary_path <- paste0(output_prefix, "_higher_ed_institution_summary.csv")
  excluded_risky_path <- paste0(output_prefix, "_excluded_risky_continuation_grants.csv")

  assert_true(file.exists(grant_path), "Grant-level joined output should exist.")
  assert_true(file.exists(higher_ed_summary_path), "Higher-ed summary output should exist.")
  assert_true(file.exists(excluded_risky_path), "Excluded risky continuation output should exist.")

  grants_joined <- readr::read_csv(grant_path, show_col_types = FALSE)
  higher_ed_summary <- readr::read_csv(higher_ed_summary_path, show_col_types = FALSE)
  excluded_risky <- readr::read_csv(excluded_risky_path, show_col_types = FALSE)

  assert_equal(nrow(excluded_risky), 1L, "Fixture should exclude exactly one risky continuation grant.")
  assert_true("AWARD1" %in% excluded_risky$award_id_string, "Excluded risky file should contain AWARD1.")
  assert_true(!("AWARD1" %in% grants_joined$award_id_string), "Joined grants should not retain filtered award AWARD1.")
  assert_true(!("AWARD3" %in% grants_joined$award_id_string), "Joined grants should not retain amount-corrected zero award AWARD3.")
  assert_true("AWARD2" %in% grants_joined$award_id_string, "Joined grants should retain non-filtered award AWARD2.")
  assert_true(nrow(higher_ed_summary) == 1L, "Higher-ed summary should contain one retained institution.")
  assert_true("Sample College" %in% higher_ed_summary$display_name, "Retained summary row should be Sample College.")
  assert_true(higher_ed_summary$total_disrupted_grants[[1]] == 1, "Retained institution should have one disrupted grant.")
})
