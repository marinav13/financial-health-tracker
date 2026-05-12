if (!exists("run_test", mode = "function")) {
  source(file.path(getwd(), "tests", "test_support.R"))
}

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
    file.path(fixture_root, "data_pipelines", "grant_witness", "cache")
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
    full_award_number = c("NIH-001", "NIH-002", "NIH-003", "NIH-004"),
    core_award_number = c("NIH-001", "NIH-002", "NIH-003", "NIH-004"),
    status = c("terminated", "terminated", "terminated", "terminated"),
    org_name = c("Example University", "ALAMO COMMUNITY COLLEGE DISTRICT - St. Philip's College", "Sample College", "Sample College"),
    org_state = c("MA", "MA", "MA", "MA"),
    org_city = c("Boston", "Springfield", "Springfield", "Springfield"),
    org_type = c("University", "College", "College", "College"),
    org_traits = c(NA, NA, NA, NA),
    project_title = c("Immune Response Study", "Campus Health Study", "Corrected Zero Study", "Forced Other Study"),
    abstract_text = c("Research abstract", "More research", "Stale amount", "Force other match override"),
    targeted_start_date = c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01"),
    targeted_end_date = c("2025-12-31", "2025-12-31", "2025-12-31", "2025-12-31"),
    termination_date = c("2025-01-15", "2025-01-20", "2025-02-01", "2025-02-15"),
    reinstated_est_date = c(NA, NA, NA, NA),
    total_award = c(1000000, 500000, 48974, 200000),
    total_estimated_outlays = c(400000, 100000, 48973.15, 50000),
    total_estimated_remaining = c(600000, 400000, 0.85, 150000),
    usaspending_url = c(
      "https://www.usaspending.gov/award/AWARD1",
      "https://www.usaspending.gov/award/AWARD2",
      "https://www.usaspending.gov/award/AWARD3",
      "https://www.usaspending.gov/award/AWARD4"
    ),
    reporter_url = c(
      "https://grant-witness.us/nih/1",
      "https://grant-witness.us/nih/2",
      "https://grant-witness.us/nih/3",
      "https://grant-witness.us/nih/4"
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

  manual_include_path <- file.path(
    fixture_root, "data_pipelines", "grant_witness", "manual_include.csv"
  )
  readr::write_csv(
    data.frame(
      organization_name = c(
        "ALAMO COMMUNITY COLLEGE DISTRICT - St. Philip's College",
        "Sample College"
      ),
      organization_state = c("Massachusetts", "Massachusetts"),
      include_in_dataset = c("TRUE", "TRUE"),
      display_name_override = c(NA, "Sample Other College"),
      force_other = c(NA, "TRUE"),
      stringsAsFactors = FALSE
    ),
    manual_include_path,
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
    "--skip-download"
  ))

  grant_path <- paste0(output_prefix, "_grant_level_joined.csv")
  higher_ed_summary_path <- paste0(output_prefix, "_higher_ed_institution_summary.csv")
  excluded_risky_path <- paste0(output_prefix, "_excluded_risky_continuation_grants.csv")

  assert_true(file.exists(grant_path), "Grant-level joined output should exist.")
  assert_true(file.exists(higher_ed_summary_path), "Higher-ed summary output should exist.")
  assert_true(!file.exists(excluded_risky_path),
              "Risky continuation output should no longer be produced after the USAspending outlay filter was removed.")

  grants_joined <- readr::read_csv(grant_path, show_col_types = FALSE)
  higher_ed_summary <- readr::read_csv(higher_ed_summary_path, show_col_types = FALSE)

  assert_true("AWARD1" %in% grants_joined$award_id_string,
              "Joined grants should retain AWARD1 now that the USAspending outlay filter is gone.")
  assert_true("AWARD2" %in% grants_joined$award_id_string,
              "Joined grants should retain AWARD2.")
  assert_true("AWARD4" %in% grants_joined$award_id_string,
              "Joined grants should retain AWARD4 when a manual include forces the institution into other.")
  assert_true(!("AWARD3" %in% grants_joined$award_id_string),
              "Joined grants should not retain amount-corrected zero award AWARD3.")
  assert_identical(
    grants_joined$match_method[grants_joined$award_id_string == "AWARD2"][[1]],
    "manual_include_unmatched",
    "Manual include should survive case/display-name differences without falling back to likely_higher_ed_unmatched."
  )
  assert_identical(
    grants_joined$match_method[grants_joined$award_id_string == "AWARD4"][[1]],
    "manual_include_unmatched",
    "Force-other manual includes should suppress profile matches and keep the institution in other."
  )
  assert_true(
    is.na(grants_joined$matched_unitid[grants_joined$award_id_string == "AWARD4"][[1]]),
    "Force-other manual includes should clear matched_unitid."
  )
  assert_identical(
    grants_joined$display_name_override[grants_joined$award_id_string == "AWARD4"][[1]],
    "Sample Other College",
    "Manual include display name override should be carried through to the joined grant output."
  )
  assert_equal(nrow(higher_ed_summary), 3L,
               "Higher-ed summary should contain all three retained institutions.")
  assert_true(all(c("Example University", "Alamo Community College District - St. Philip's College", "Sample Other College") %in% higher_ed_summary$display_name),
              "Retained summary rows should include the matched institution, the manual-include-only institution, and the display-overridden force-other institution.")
  assert_equal(sum(higher_ed_summary$total_disrupted_grants), 3L,
               "Three disrupted grants should survive the pipeline (AWARD1 + AWARD2 + AWARD4).")
})
