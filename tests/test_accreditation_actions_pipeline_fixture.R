run_test("Accreditation actions pipeline fixture", function() {
  fixture_root <- tempfile("accreditation-fixture-")
  dir.create(fixture_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  dirs <- c(
    file.path(fixture_root, "scripts"),
    file.path(fixture_root, "scripts", "shared"),
    file.path(fixture_root, "data_pipelines"),
    file.path(fixture_root, "data_pipelines", "accreditation"),
    file.path(fixture_root, "data_pipelines", "accreditation", "cache")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  for (nm in c("shared/utils.R", "shared/ipeds_paths.R", "shared/accreditation_helpers.R")) {
    file.copy(
      file.path(root, "scripts", nm),
      file.path(fixture_root, "scripts", nm),
      overwrite = TRUE
    )
  }
  file.copy(
    file.path(root, "scripts", "build_accreditation_actions.R"),
    file.path(fixture_root, "scripts", "build_accreditation_actions.R"),
    overwrite = TRUE
  )

  scraper_lines <- readLines(file.path(root, "scripts", "shared", "accreditation_scrapers.R"), warn = FALSE)
  scraper_override <- c(
    scraper_lines,
    "",
    "fixture_action_tbl <- function() {",
    "  tibble::tibble(",
    "    institution_name_raw = c('Example University', 'Unknown Institute'),",
    "    institution_state_raw = c('MA', 'CA'),",
    "    accreditor = c('MSCHE', 'MSCHE'),",
    "    action_type = c('probation', 'notice'),",
    "    action_label_raw = c('Probation', 'Notice'),",
    "    action_status = c('active', 'active'),",
    "    action_date = as.Date(c('2024-06-01', '2024-05-01')),",
    "    action_year = c(2024L, 2024L),",
    "    source_url = c('https://example.org/action1', 'https://example.org/action2'),",
    "    source_title = c('Fixture action 1', 'Fixture action 2'),",
    "    notes = c(NA_character_, NA_character_),",
    "    source_page_url = c('https://example.org/page1', 'https://example.org/page2'),",
    "    source_page_modified = c('2024-06-02', '2024-05-02'),",
    "    last_seen_at = c('2024-06-03', '2024-05-03')",
    "  )",
    "}",
    "",
    "parse_msche <- function(cache_dir, refresh) fixture_action_tbl()",
    "parse_hlc <- function(cache_dir, refresh) fixture_action_tbl()[0, ]",
    "parse_sacscoc <- function(cache_dir, refresh) fixture_action_tbl()[0, ]",
    "parse_neche <- function(cache_dir, refresh) fixture_action_tbl()[0, ]",
    "parse_wscuc <- function(cache_dir, refresh) fixture_action_tbl()[0, ]"
  )
  writeLines(scraper_override, file.path(fixture_root, "scripts", "shared", "accreditation_scrapers.R"), useBytes = TRUE)

  financial_input <- file.path(fixture_root, "fixture_financial.csv")
  output_prefix <- file.path(fixture_root, "data_pipelines", "accreditation", "fixture_accreditation")

  financial_df <- data.frame(
    unitid = "100",
    institution_name = "Example University",
    year = 2024,
    control_label = "Public",
    sector = "Public, 4-year or above",
    level = "Four or more years",
    urbanization = "City",
    category = "R1",
    state = "Massachusetts",
    city = "Boston",
    enrollment_pct_change_5yr = -4.2,
    revenue_pct_change_5yr = -3.1,
    revenue_decreased_5yr = "Yes",
    enrollment_decreased_5yr = "Yes",
    revenue_10pct_drop_last_3_of_5 = "No",
    enrollment_decline_last_3_of_5 = "Yes",
    ended_2024_at_loss = "No",
    losses_last_3_of_5 = "No",
    loss_years_last_10 = 1,
    net_tuition_per_fte_change_5yr = 2.1,
    tuition_dependence_pct = 45,
    state_funding_pct_core_revenue = 0.15,
    federal_grants_contracts_pell_adjusted_pct_core_revenue = 0.12,
    pct_international_all = 0.08,
    staff_total_headcount_pct_change_5yr = -1.5,
    discount_pct_change_5yr = 0.5,
    endowment_pct_change_5yr = 6.2,
    liquidity = 0.4,
    leverage = 0.3,
    stringsAsFactors = FALSE
  )
  readr::write_csv(financial_df, financial_input, na = "")

  setwd(fixture_root)
  acc_env <- new.env(parent = globalenv())
  sys.source(file.path(fixture_root, "scripts", "build_accreditation_actions.R"), envir = acc_env)

  old_ci <- Sys.getenv("CI", unset = NA_character_)
  old_actions <- Sys.getenv("GITHUB_ACTIONS", unset = NA_character_)
  on.exit({
    if (is.na(old_ci)) Sys.unsetenv("CI") else Sys.setenv(CI = old_ci)
    if (is.na(old_actions)) Sys.unsetenv("GITHUB_ACTIONS") else Sys.setenv(GITHUB_ACTIONS = old_actions)
  }, add = TRUE)
  Sys.setenv(CI = "true")

  ci_err <- tryCatch(
    {
      acc_env$main(c(
        "--financial-input", financial_input,
        "--output-prefix", output_prefix,
        "--refresh", "false"
      ))
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  assert_true(
    !is.null(ci_err) && grepl("SCRAPER RETURNED 0 ROWS: HLC", ci_err, fixed = TRUE),
    "CI accreditation refresh should fail when an unexpected scraper returns zero rows."
  )

  acc_env$main(c(
    "--financial-input", financial_input,
    "--output-prefix", output_prefix,
    "--refresh", "false",
    "--allow-partial-accreditation"
  ))

  actions_path <- paste0(output_prefix, "_actions_joined.csv")
  summary_path <- paste0(output_prefix, "_institution_summary.csv")
  current_path <- paste0(output_prefix, "_current_status.csv")
  unmatched_path <- paste0(output_prefix, "_unmatched_for_review.csv")
  coverage_path <- paste0(output_prefix, "_source_coverage.csv")
  workbook_path <- paste0(output_prefix, "_workbook.xlsx")

  assert_true(file.exists(actions_path), "Accreditation actions output should exist.")
  assert_true(file.exists(summary_path), "Accreditation summary output should exist.")
  assert_true(file.exists(current_path), "Accreditation current-status output should exist.")
  assert_true(file.exists(unmatched_path), "Accreditation unmatched output should exist.")
  assert_true(file.exists(coverage_path), "Accreditation coverage output should exist.")
  assert_true(file.exists(workbook_path), "Accreditation workbook output should exist.")

  actions_df <- readr::read_csv(actions_path, show_col_types = FALSE)
  summary_df <- readr::read_csv(summary_path, show_col_types = FALSE)
  current_df <- readr::read_csv(current_path, show_col_types = FALSE)
  unmatched_df <- readr::read_csv(unmatched_path, show_col_types = FALSE)
  coverage_df <- readr::read_csv(coverage_path, show_col_types = FALSE)

  assert_equal(nrow(actions_df), 2L, "Fixture should produce two accreditation action rows.")
  assert_true("100" %in% as.character(actions_df$unitid), "Matched action should retain unitid 100.")
  assert_true(any(is.na(actions_df$unitid)), "Fixture should leave one unmatched action for review.")
  assert_true(nrow(summary_df) == 1L, "Fixture should produce one matched institution summary row.")
  assert_true(summary_df$has_active_warning[[1]], "Matched institution should have an active warning.")
  assert_true("Example University" %in% current_df$tracker_name, "Current status should include Example University.")
  assert_true(nrow(unmatched_df) == 1L, "Fixture should produce one unmatched institution for review.")
  assert_true(nrow(coverage_df) >= 1L, "Coverage output should contain at least one row.")
})
