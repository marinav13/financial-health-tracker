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

  for (nm in c("shared/utils.R", "shared/ipeds_paths.R", "shared/name_normalization.R", "shared/accreditation_helpers.R")) {
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
    "    institution_name_raw = c('Saint Augustine\u2019s University', 'Women\u2019s Institute of Torah Seminary & College', 'Unknown Institute'),",
    "    institution_state_raw = c('NC', 'MD', 'CA'),",
    "    accreditor = c('MSCHE', 'MSCHE', 'MSCHE'),",
    "    action_type = c('probation', 'notice', 'notice'),",
    "    action_label_raw = c('Probation', 'Notice', 'Notice'),",
    "    action_status = c('active', 'active', 'active'),",
    "    action_date = as.Date(c('2024-06-01', '2024-05-15', '2024-05-01')),",
    "    action_year = c(2024L, 2024L, 2024L),",
    "    source_url = c('https://example.org/action1', 'https://example.org/action2', 'https://example.org/action3'),",
    "    source_title = c('Fixture action 1', 'Fixture action 2', 'Fixture action 3'),",
    "    notes = c(NA_character_, NA_character_, NA_character_),",
    "    source_page_url = c('https://example.org/page1', 'https://example.org/page2', 'https://example.org/page3'),",
    "    source_page_modified = c('2024-06-02', '2024-05-16', '2024-05-02'),",
    "    last_seen_at = c('2024-06-03', '2024-05-17', '2024-05-03')",
    "  )",
    "}",
    "",
    "parse_msche <- function(cache_dir, refresh) fixture_action_tbl()",
    "parse_hlc <- function(cache_dir, refresh) fixture_action_tbl()[0, ]",
    "parse_sacscoc <- function(cache_dir, refresh) fixture_action_tbl()[0, ]",
    "parse_neche <- function(cache_dir, refresh) fixture_action_tbl()[0, ]",
    "parse_wscuc <- function(cache_dir, refresh) fixture_action_tbl()[0, ]",
    # parse_nwccu must be stubbed too; otherwise the test walks the live
    # NWCCU directory (134 institutions at Sys.sleep(0.5) each) and the test
    # becomes ~67 seconds + network-dependent.
    "parse_nwccu <- function(cache_dir, refresh) fixture_action_tbl()[0, ]"
  )
  writeLines(scraper_override, file.path(fixture_root, "scripts", "shared", "accreditation_scrapers.R"), useBytes = TRUE)

  financial_input <- file.path(fixture_root, "fixture_financial.csv")
  output_prefix <- file.path(fixture_root, "data_pipelines", "accreditation", "fixture_accreditation")

  financial_df <- data.frame(
    unitid = c("100", "200"),
    institution_name = c("Saint Augustine's University", "Women's Institute of Torah Seminary and College"),
    year = c(2024, 2024),
    control_label = c("Public", "Private"),
    sector = c("Public, 4-year or above", "Private not-for-profit, 4-year or above"),
    level = c("Four or more years", "Four or more years"),
    urbanization = c("City", "City"),
    category = c("R1", "Faith-based"),
    state = c("North Carolina", "Maryland"),
    city = c("Raleigh", "Baltimore"),
    enrollment_pct_change_5yr = c(-4.2, -1.1),
    revenue_pct_change_5yr = c(-3.1, 2.3),
    revenue_decreased_5yr = c("Yes", "No"),
    enrollment_decreased_5yr = c("Yes", "No"),
    revenue_10pct_drop_last_3_of_5 = c("No", "No"),
    enrollment_decline_last_3_of_5 = c("Yes", "No"),
    ended_2024_at_loss = c("No", "No"),
    losses_last_3_of_5 = c("No", "No"),
    loss_years_last_10 = c(1, 0),
    net_tuition_per_fte_change_5yr = c(2.1, 1.4),
    tuition_dependence_pct = c(45, 68),
    state_funding_pct_core_revenue = c(0.15, 0.02),
    federal_grants_contracts_pell_adjusted_pct_core_revenue = c(0.12, 0.01),
    pct_international_all = c(0.08, 0.03),
    staff_total_headcount_pct_change_5yr = c(-1.5, 0.2),
    discount_pct_change_5yr = c(0.5, 1.1),
    endowment_pct_change_5yr = c(6.2, 3.4),
    liquidity = c(0.4, 0.7),
    leverage = c(0.3, 0.2),
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

  assert_equal(nrow(actions_df), 3L, "Fixture should produce three accreditation action rows.")
  assert_true("100" %in% as.character(actions_df$unitid), "Curly-apostrophe Saint Augustine fixture should match unitid 100.")
  assert_true("200" %in% as.character(actions_df$unitid), "Curly-apostrophe Women's fixture should match unitid 200.")
  assert_true(any(is.na(actions_df$unitid)), "Fixture should leave one unmatched action for review.")
  assert_true(nrow(summary_df) == 2L, "Fixture should produce two matched institution summary rows.")
  assert_true(
    all(c("100", "200") %in% as.character(summary_df$unitid)),
    "Matched summary output should retain both apostrophe-variant tracker unitids."
  )
  assert_true("Saint Augustine's University" %in% current_df$tracker_name, "Current status should include Saint Augustine's University.")
  assert_true("Women's Institute of Torah Seminary and College" %in% current_df$tracker_name, "Current status should include Women's Institute of Torah Seminary and College.")
  assert_true(nrow(unmatched_df) == 1L, "Fixture should produce one unmatched institution for review.")
  assert_true(nrow(coverage_df) >= 1L, "Coverage output should contain at least one row.")
  assert_true("row_type" %in% names(coverage_df), "Coverage output should include row_type metadata.")
  assert_true(any(coverage_df$row_type == "action_count"), "Coverage output should preserve action-count rows.")
  assert_true(any(coverage_df$row_type == "fetch_summary"), "Coverage output should include fetch-summary rows.")
  fetch_rows <- coverage_df[coverage_df$row_type == "fetch_summary", , drop = FALSE]
  assert_true(all(fetch_rows$fetch_total_count == 0L),
    "Stubbed fixture scrapers should emit zero-fetch summary rows.")
})

run_test("Accreditation workbook sanitiser truncates overlong cells intentionally", function() {
  acc_env <- new.env(parent = globalenv())
  sys.source(file.path(root, "scripts", "build_accreditation_actions.R"), envir = acc_env)

  long_text <- paste(rep("x", 33050), collapse = "")
  result <- acc_env$sanitize_excel_workbook_frame(
    data.frame(
      notes = c(long_text, "ok"),
      stringsAsFactors = FALSE
    ),
    "actions"
  )

  assert_true(is.data.frame(result$data), "Workbook sanitiser should return a data frame.")
  assert_true(nchar(result$data$notes[[1]], type = "chars") <= 32767L,
    "Workbook sanitiser should keep text within Excel's cell limit.")
  assert_true(grepl("\\[truncated for Excel cell limit\\]$", result$data$notes[[1]]),
    "Workbook sanitiser should append an explicit truncation marker.")
  assert_equal(nrow(result$truncations), 1L,
    "Workbook sanitiser should record one sheet/column truncation entry.")
  assert_equal(result$truncations$sheet[[1]], "actions")
  assert_equal(result$truncations$column[[1]], "notes")
  assert_equal(result$truncations$count[[1]], 1L)
  assert_true(result$truncations$max_original_chars[[1]] > 32767L,
    "Workbook truncation log should retain original max character length.")
})
