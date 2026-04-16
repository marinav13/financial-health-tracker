run_test("Article workbook fixture", function() {
  # Use an absolute path before any setwd() to avoid fixture directory nesting.
  sys_tmp_root <- normalizePath(file.path(tempdir(), ".."), winslash = "/", mustWork = TRUE)
  fixture_root <- file.path(sys_tmp_root, paste0("workbook-fixture-", as.hexmode(sample.int(10^7, 1))))
  dir.create(fixture_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  # Set up the directory structure the workbook script expects.
  dir.create(file.path(fixture_root, "ipeds", "derived"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(fixture_root, "workbooks"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(fixture_root, "scripts", "shared"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(fixture_root, "data_pipelines", "federal_closure", "derived"), recursive = TRUE, showWarnings = FALSE)

  # The workbook calls read_required_closure_csv() for closure tabs.  Create minimal
  # stub files with the expected column names so the workbook does not error out.
  closure_cols <- c("unitid", "institution_name", "city", "state", "closure_date",
    "closure_type", "source", "notes")
  closure_stub <- data.frame(matrix(rep(NA_character_, length(closure_cols)), nrow = 1, ncol = length(closure_cols)))
  names(closure_stub) <- closure_cols
  lapply(
    c("running_closures.csv", "main_campus_closures.csv", "branch_campus_closures.csv",
      "mergers_consolidations.csv", "private_sector_federal_main_closures.csv"),
    function(f) readr::write_csv(closure_stub,
      file.path(fixture_root, "data_pipelines", "federal_closure", "derived", f), na = "")
  )

  # The workbook calls read_csv_if_exists() for accreditation and college_cuts data.
  # read_csv_if_exists() returns a 0-row data frame with no columns when the file
  # is absent, so we must provide stub files that have the right columns (0 rows is fine).
  # accreditation_tracker_institution_summary needs: unitid and other columns.
  # college_cuts needs: unitid.
  dir.create(file.path(fixture_root, "data_pipelines", "accreditation"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(fixture_root, "data_pipelines", "college_cuts"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(fixture_root, "data_pipelines", "federal_hcm"), recursive = TRUE, showWarnings = FALSE)

  # accreditation_summary needs all columns that the workbook subscripts with [..., cols].
  # Provide a 1-row stub with all those columns as NA.  The workbook's merge() then
  # produces 0 rows (no matching unitids) but does not error.
  accred_cols <- c("unitid", "tracker_name", "tracker_state", "accreditors",
    "action_types", "action_labels", "active_actions", "has_active_warning",
    "has_active_warning_or_notice", "has_active_adverse_action", "action_count",
    "latest_action_date", "latest_action_year")
  accred_stub <- data.frame(
    sapply(accred_cols, simplify = FALSE, USE.NAMES = TRUE,
           function(col) NA_character_),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  readr::write_csv(accred_stub,
    file.path(fixture_root, "data_pipelines", "accreditation", "accreditation_tracker_institution_summary.csv"), na = "")

  # college_cuts_summary reads college_cuts_financial_tracker_institution_summary.csv.
  # The workbook subsets it to these columns before merging.
  cuts_cols <- c("matched_unitid", "institution_name_collegecuts", "institution_state_full",
    "cut_records", "cut_types", "latest_cut_announcement_date", "first_cut_announcement_date",
    "total_students_affected_known", "total_faculty_affected_known",
    "staff_layoff_records", "program_suspension_records",
    "department_closure_records", "campus_closure_records", "institution_closure_records",
    "teach_out_records", "financial_warning_count")
  cuts_stub <- data.frame(
    sapply(cuts_cols, simplify = FALSE, USE.NAMES = TRUE,
           function(col) NA_character_),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  readr::write_csv(cuts_stub,
    file.path(fixture_root, "data_pipelines", "college_cuts", "college_cuts_financial_tracker_institution_summary.csv"), na = "")

  # Build the extended canonical dataset in a nested fixture workspace.
  # The workbook needs 5 years of data (2020-2024) because its internal
  # enrich_group() calls compute 5-year trend metrics per institution.
  canon_fixture <- file.path(sys_tmp_root, paste0("workbook-canon-", as.hexmode(sample.int(10^7, 1))))
  dir.create(canon_fixture, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(canon_fixture, recursive = TRUE, force = TRUE), add = TRUE)

  canon_dirs <- c(
    file.path(canon_fixture, "scripts", "shared"),
    file.path(canon_fixture, "ipeds", "cache", "downloads", "dict"),
    file.path(canon_fixture, "ipeds", "derived"),
    file.path(canon_fixture, "ipeds", "manifests")
  )
  invisible(lapply(canon_dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  # Copy the scripts the canonical builder needs.
  for (nm in c("build_ipeds_canonical_dataset.R", "shared/utils.R",
               "shared/ipeds_paths.R", "shared/contracts.R", "shared/ipeds_helpers.R")) {
    file.copy(file.path(root, "scripts", nm),
      file.path(canon_fixture, "scripts", nm), overwrite = TRUE)
  }

  # Write stub dictionary lookups so the canonical builder runs without real IPEDS files.
  ipeds_helpers_lines <- readLines(file.path(root, "scripts", "shared", "ipeds_helpers.R"), warn = FALSE)
  ipeds_helpers_override <- c(
    ipeds_helpers_lines, "",
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
  writeLines(ipeds_helpers_override,
    file.path(canon_fixture, "scripts", "shared", "ipeds_helpers.R"), useBytes = TRUE)

  # Stub archives for every table the canonical builder looks up by year.
  # HD, IC, FLAGS are required every year; other tables vary but the builder tries all.
  stub_tables <- c("HD", "IC", "FLAGS", "EFFY", "EFIA", "EAP",
                   "DRVEF", "DRVADM", "DRVHR", "DRVGR", "DRVF")
  for (yr in 2020:2024) {
    for (tbl in stub_tables) {
      stub_dir <- file.path(canon_fixture, "ipeds", "cache", "downloads", "dict",
        paste0(tbl, as.integer(yr), ".zip"))
      invisible(file.create(stub_dir))
    }
  }

  extended_out <- file.path(canon_fixture, "ipeds", "derived", "fixture_extended.csv")

  # Three institutions (one per control category), five years (2020-2024).
  # Public institution (unitid=100), Private not-for-profit (unitid=200),
  # Private for-profit (unitid=300).
  raw_df <- data.frame(
    unitid = c(rep("100", 5), rep("200", 5), rep("300", 5)),
    year   = rep(as.character(2020:2024), 3),
    # -- Institution identity (HD table) --
    control = c(rep("1", 5), rep("2", 5), rep("3", 5)),   # 1=Public, 2=Private NFP, 3=Private FP
    sector = c(rep("4", 5), rep("4", 5), rep("4", 5)),
    level  = c(rep("4", 5), rep("4", 5), rep("4", 5)),
    status = c(rep("1", 5), rep("1", 5), rep("1", 5)),
    is_active = c(rep("1", 5), rep("1", 5), rep("1", 5)),
    state  = c(rep("MA", 5), rep("NY", 5), rep("TX", 5)),
    city   = c(rep("Boston", 5), rep("New York", 5), rep("Dallas", 5)),
    institution_name = c(rep("Public U", 5), rep("Private NFP College", 5), rep("Private FP College", 5)),
    urbanization = c(rep("11", 5), rep("11", 5), rep("11", 5)),
    category = c(rep("A", 5), rep("A", 5), rep("A", 5)),
    hbcu = c(rep("2", 5), rep("2", 5), rep("2", 5)),
    tribal_college = c(rep("2", 5), rep("2", 5), rep("2", 5)),
    grad_offering = c(rep("1", 5), rep("1", 5), rep("1", 5)),
    reporting_model = c(rep("1", 5), rep("2", 5), rep("3", 5)),  # GASB / FASB / PFP
    access_earnings = c(rep("9", 5), rep("9", 5), rep("9", 5)),
    size = c(rep("3", 5), rep("3", 5), rep("3", 5)),
    grad_program_mix = c(rep("7", 5), rep("7", 5), rep("7", 5)),
    undergrad_program_mix = c(rep("8", 5), rep("8", 5), rep("8", 5)),
    religious_affiliation = c(rep("10", 5), rep("10", 5), rep("10", 5)),
    all_programs_distance_education = c(rep("2", 5), rep("2", 5), rep("2", 5)),
    # -- Enrollment (EFIA table) --
    fte_12_months = c("90", "92", "95", "98", "100",
                       "80", "82", "85", "88", "90",
                       "20", "21", "22", "22", "22"),
    fte_undergrad  = c("70", "72", "75", "78", "80",
                        "65", "67", "70", "73", "75",
                        "18", "18", "19", "19", "19"),
    fte_graduate   = c("20", "20", "20", "20", "20",
                       "15", "15", "15", "15", "15",
                       "2", "3", "3", "3", "3"),
    fte_total_staff = c("48", "49", "50", "51", "52",
                          "40", "41", "42", "43", "44",
                          "10", "10", "10", "11", "11"),
    fte_instructional = c("9", "9", "10", "10", "10",
                           "8", "8", "8", "8", "8",
                           "3", "3", "3", "3", "3"),
    # -- Finance (F1 table — GASB) --
    total_operating_nonoperating_revenues_gasb = c("900", "950", "1000", "1050", "1100",
                                                     "600", "620", "650", "680", "700",
                                                     "80", "82", "85", "88", "90"),
    total_expenses_deductions_current_total_gasb = c("850", "880", "900", "940", "980",
                                                       "620", "640", "660", "690", "720",
                                                       "100", "102", "105", "108", "110"),
    tuition_fees_after_discounts_allowances_gasb = c("260", "270", "280", "290", "300",
                                                     "200", "210", "220", "230", "240",
                                                     "30", "31", "32", "33", "34"),
    federal_operating_grants_contracts_gasb = c("180", "185", "190", "195", "200",
                                               "100", "105", "110", "115", "120",
                                               "10", "10", "11", "11", "12"),
    state_appropriations_gasb = c("140", "142", "145", "148", "150",
                                    "80", "82", "85", "87", "90",
                                    "0", "0", "0", "0", "0"),
    assets = c("480", "490", "500", "510", "520",
               "300", "310", "320", "330", "340",
               "20", "20", "21", "21", "22"),
    liabilities = c("190", "195", "200", "205", "210",
                    "120", "125", "130", "135", "140",
                    "8", "8", "9", "9", "9"),
    unrestricted_public = c("230", "235", "240", "245", "250",
                             "150", "155", "160", "165", "170",
                             NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    discounts_allowances_applied_tuition_fees_gasb = c("90", "92", "95", "97", "100",
                                                        "60", "62", "65", "67", "70",
                                                        "5", "5", "5", "6", "6"),
    value_endowment_assets_end_gasb = c("760", "770", "780", "790", "800",
                                         "500", "510", "520", "530", "540",
                                         "0", "0", "0", "0", "0"),
    research_expenses_total_gasb = c("45", "46", "47", "48", "50",
                                       "10", "10", "11", "11", "12",
                                       "0", "0", "0", "0", "0"),
    core_expenses_gasb = c("380", "385", "390", "395", "400",
                             "300", "305", "310", "315", "320",
                             "40", "41", "42", "43", "44"),
    auxiliary_enterprises_revenue_gasb = c("90", "92", "94", "96", "98",
                                            "60", "62", "64", "66", "68",
                                            "5", "5", "5", "6", "6"),
    hospital_services_revenue_gasb = c("45", "46", "47", "48", "50",
                                        "30", "31", "32", "33", "34",
                                        "0", "0", "0", "0", "0"),
    independent_operations_revenue_gasb = c("20", "21", "22", "23", "25",
                                            "10", "11", "12", "13", "14",
                                            "0", "0", "0", "0", "0"),
    pell_accounting_method = c("1", "1", "1", "1", "1",
                                "2", "2", "2", "2", "2",
                                "2", "2", "2", "2", "2"),
    pell_grants = c("0", "0", "0", "0", "0",
                     "50", "52", "55", "57", "60",
                     "5", "5", "6", "6", "7"),
    endowment_spending_distribution_current_use_gasb = c("-35", "-36", "-38", "-39", "-40",
                                                         "-20", "-21", "-22", "-23", "-24",
                                                         "0", "0", "0", "0", "0"),
    endowment_assets_per_fte_gasb = c("8.4", "8.3", "8.2", "8.1", "8.0",
                                       "6.2", "6.1", "6.0", "5.9", "5.8",
                                       "0", "0", "0", "0", "0"),
    admissions_yield = c("41", "41", "41", "42", "42",
                          "35", "35", "35", "36", "36",
                          "50", "50", "51", "51", "52"),
    institution_name_latest = c(rep("Public U", 5), rep("Private NFP College", 5), rep("Private FP College", 5)),
    institution_unique_name_latest = c(rep("Public U | Boston | Massachusetts", 5),
                                        rep("Private NFP College | New York | New York", 5),
                                        rep("Private FP College | Dallas | Texas", 5)),
    state_latest = c(rep("Massachusetts", 5), rep("New York", 5), rep("Texas", 5)),
    city_latest = c(rep("Boston", 5), rep("New York", 5), rep("Dallas", 5)),
    stringsAsFactors = FALSE
  )

  raw_path     <- file.path(canon_fixture, "fixture_raw.csv")
  catalog_path <- file.path(canon_fixture, "fixture_catalog.csv")
  readr::write_csv(raw_df, raw_path, na = "")
  readr::write_csv(
    data.frame(year = integer(), table_name = character(), data_url = character(),
      stringsAsFactors = FALSE),
    catalog_path, na = ""
  )

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(canon_fixture)

  canonical_env <- new.env(parent = globalenv())
  sys.source(file.path(canon_fixture, "scripts", "build_ipeds_canonical_dataset.R"), envir = canonical_env)
  canonical_env$main(c(
    "--raw", raw_path,
    "--catalog", catalog_path,
    "--output", file.path(canon_fixture, "ipeds", "derived", "fixture_canonical.csv"),
    "--expanded-output", extended_out
  ))

  assert_true(file.exists(extended_out),
    "Canonical fixture should have produced extended output CSV.")
  extended_df <- readr::read_csv(extended_out, show_col_types = FALSE)
  assert_true(nrow(extended_df) >= 15,
    "Extended fixture should have at least 15 rows (3 institutions x 5 years).")

  # ── Run the workbook in the fixture workspace ──────────────────────────────
  setwd(fixture_root)

  workbook_input  <- file.path(fixture_root, "fixture_workbook_input.csv")
  workbook_output <- file.path(fixture_root, "workbooks", "fixture_workbook.xls")
  readr::write_csv(extended_df, workbook_input, na = "")

  # Copy helpers the workbook needs.  ipeds_paths.R is sourced before CLI arg
  # parsing in build_article_workbook.R, so it must be present.  Use absolute
  # paths from the repo root (root) to avoid getting confused by setwd() calls
  # in earlier fixtures.
  for (nm in c("shared/utils.R", "shared/ipeds_paths.R",
               "shared/workbook_helpers.R", "shared/contracts.R")) {
    file.copy(file.path(root, "scripts", nm),
      file.path(fixture_root, "scripts", nm), overwrite = TRUE)
  }
  file.copy(file.path(root, "scripts", "build_article_workbook.R"),
    file.path(fixture_root, "scripts", "build_article_workbook.R"), overwrite = TRUE)

  workbook_env <- new.env(parent = globalenv())
  sys.source(file.path(fixture_root, "scripts", "build_article_workbook.R"), envir = workbook_env)
  workbook_env$main(c(
    "--input", workbook_input,
    "--output", workbook_output
  ))

  # ── Assertions ────────────────────────────────────────────────────────────
  assert_true(file.exists(workbook_output), "Workbook file should have been created.")

  # The workbook is SpreadsheetML.  Read it back as raw lines to assert on sheet names.
  lines <- readLines(workbook_output, warn = FALSE, encoding = "UTF-8")
  xml_body <- paste(lines, collapse = "\n")

  # <Worksheet> elements each have a ss:Name attribute with the sheet title.
  sheet_names <- stringr::str_extract_all(xml_body, '<Worksheet[^>]+ss:Name="([^"]+)"')[[1]]
  sheet_names <- stringr::str_replace_all(sheet_names, '<Worksheet[^>]+ss:Name="|"', "")
  sheet_names <- trimws(sheet_names)

  assert_true("Summary" %in% sheet_names, "Workbook should contain a 'Summary' sheet.")
  assert_true("ReportAnswers" %in% sheet_names,
    "Workbook should contain a 'ReportAnswers' sheet.")
  assert_true(length(sheet_names) > 5,
    sprintf("Workbook should have more than 5 sheets (found %d).", length(sheet_names)))

  # Spot-check a few expected sheets are present.
  for (expected in c("Summary", "ReportAnswers", "ResearchLeaders", "HighDebt",
                      "MultiSignal", "PrivateCloseRisk", "PublicFinBad50",
                      "PrivateFinBad50", "LossTuition", "StaffCutsYoY")) {
    assert_true(expected %in% sheet_names,
      sprintf("Workbook should contain '%s' sheet.", expected))
  }

  # Summary sheet should have data rows beyond just the header.
  summary_block <- stringr::str_extract(xml_body,
    '(?s)<Worksheet[^>]+ss:Name="Summary".*?</Worksheet>')
  assert_true(!is.na(summary_block) && nzchar(summary_block),
    "Summary sheet block should not be empty.")
  summary_rows <- stringr::str_extract_all(summary_block, "<Row>")[[1]]
  assert_true(length(summary_rows) > 4,
    sprintf("Summary sheet should have more than a header row (found %d).", length(summary_rows)))
})
