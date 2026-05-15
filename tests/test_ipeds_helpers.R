if (!exists("run_test", mode = "function")) {
  source(file.path(getwd(), "tests", "test_support.R"))
}

run_test("IPEDS sector benchmarks", function() {
  df <- data.frame(
    unitid = c("100", "200", "100", "200"),
    year = c(2024L, 2024L, 2025L, 2025L),
    control_label = c("Public", "Public", "Public", "Public"),
    tuition_dependence_pct = c(40, 60, 50, 70),
    enrollment_headcount_total = c(1000, 2000, 1100, 1900),
    enrollment_pct_change_5yr = c(-5, -10, -3, -8),
    research_expense_per_fte = c(0, 50, 100, NA),
    students_per_instructional_staff_fte = c(12, 18, 10, 20),
    stringsAsFactors = FALSE
  )

  enriched <- apply_sector_benchmarks(df)

  assert_true("sector_median_tuition_dependence_pct" %in% names(enriched))
  assert_true("sector_enrollment_change_sentence" %in% names(enriched))
  assert_equal(enriched$sector_median_tuition_dependence_pct[[1]], 50)
  assert_true(grepl("net tuition", enriched$tuition_dependence_vs_sector_median_sentence[[1]], fixed = TRUE))
})

run_test("IPEDS layout honors configurable default year range", function() {
  old_start <- Sys.getenv("IPEDS_START_YEAR", unset = NA_character_)
  old_end <- Sys.getenv("IPEDS_END_YEAR", unset = NA_character_)
  on.exit({
    if (is.na(old_start)) Sys.unsetenv("IPEDS_START_YEAR") else Sys.setenv(IPEDS_START_YEAR = old_start)
    if (is.na(old_end)) Sys.unsetenv("IPEDS_END_YEAR") else Sys.setenv(IPEDS_END_YEAR = old_end)
  }, add = TRUE)

  Sys.setenv(IPEDS_START_YEAR = "2015", IPEDS_END_YEAR = "2025")
  ipeds <- load_ipeds_paths()
  paths <- ipeds$ipeds_layout(root = root, output_stem = "fixture_ipeds")

  assert_true(grepl("fixture_ipeds_raw_2015_2025.csv", paths$raw_csv, fixed = TRUE))
  assert_true(grepl("fixture_ipeds_canonical_2015_2025.csv", paths$canonical_csv, fixed = TRUE))
})

run_test("IPEDS canonical row builder", function() {
  row <- data.frame(
    unitid = "100",
    year = 2024L,
    control = "1",
    sector = "4",
    level = "4",
    status = "1",
    is_active = "1",
    state = "MA",
    city = "Boston",
    institution_name = "Example University",
    urbanization = "11",
    category = "A",
    hbcu = "2",
    tribal_college = "2",
    grad_offering = "1",
    reporting_model = "1",
    access_earnings = "9",
    size = "3",
    grad_program_mix = "7",
    undergrad_program_mix = "8",
    religious_affiliation = "10",
    all_programs_distance_education = "2",
    fte_12_months = "100",
    fte_undergrad = "80",
    fte_graduate = "20",
    enrollment_headcount_total = "120",
    enrollment_headcount_undergrad = "90",
    enrollment_headcount_graduate = "30",
    enrollment_nonresident_total = "12",
    enrollment_nonresident_undergrad = "8",
    enrollment_nonresident_graduate = "4",
    fte_total_staff = "50",
    fte_instructional = "10",
    transfer_out_rate_bachelor = "15",
    staff_headcount_total = "60",
    staff_headcount_instructional = "15",
    loan_pct_undergrad_federal = "30",
    loan_avg_undergrad_federal = "12000",
    loan_count_undergrad_federal = "40",
    total_operating_nonoperating_revenues_gasb = "1000",
    total_expenses_deductions_current_total_gasb = "900",
    tuition_fees_after_discounts_allowances_gasb = "300",
    federal_operating_grants_contracts_gasb = "200",
    state_appropriations_gasb = "150",
    assets = "500",
    liabilities = "200",
    unrestricted_public = "250",
    discounts_allowances_applied_tuition_fees_gasb = "100",
    value_endowment_assets_end_gasb = "800",
    research_expenses_total_gasb = "50",
    core_expenses_gasb = "400",
    auxiliary_enterprises_revenue_gasb = "100",
    hospital_services_revenue_gasb = "50",
    independent_operations_revenue_gasb = "25",
    pell_accounting_method = "1",
    pell_grants = "0",
    endowment_spending_distribution_current_use_gasb = "-40",
    endowment_assets_per_fte_gasb = "8",
    admissions_yield = "42",
    stringsAsFactors = FALSE
  )

  built <- build_canonical_ipeds_row(
    row,
    decode_lookups = list(
      hd_sector_lookup = c("4" = "Public, 4-year or above"),
      hd_level_lookup = c("4" = "Four or more years"),
      hd_act_lookup = c("1" = "Active - institution active"),
      hd_active_lookup = c("1" = "Yes"),
      hd_hbcu_lookup = c("2" = "No"),
      hd_tribal_lookup = c("2" = "No"),
      hd_grad_offering_lookup = c("1" = "Yes"),
      hd_category_lookup = c("A" = "Degree-granting, primarily baccalaureate or above"),
      hd_locale_lookup = c("11" = "City"),
      hd_access_lookup = c("9" = "Higher access"),
      hd_size_lookup = c("3" = "Medium"),
      hd_ug_mix_lookup = c("8" = "Undergrad mix"),
      hd_grad_mix_lookup = c("7" = "Grad mix"),
      ic_religious_affiliation_lookup = c("10" = "None"),
      flags_form_lookup = c("1" = "GASB")
    )
  )

  assert_identical(nrow(built), 1L)
  assert_identical(built$institution_unique_name[[1]], "Example University | Boston | Massachusetts")
  assert_equal(built$revenue_total[[1]], 1000)
  assert_equal(built$loss_amount[[1]], 100)
  assert_equal(built$tuition_dependence_pct[[1]], 30)
  assert_equal(built$pct_international_all[[1]], 0.1)
  assert_equal(built$leverage[[1]], 0.4)
})

# ---------------------------------------------------------------------------
# get_frequency_lookup column-layout drift coverage
# ---------------------------------------------------------------------------
# NCES has shipped at least two header layouts for the IPEDS dictionary
# Frequencies sheet across releases:
#   * legacy (still found in older cached extracts on contributor machines):
#       VarName | VarNumber | TableName | CodeValue | valuelabel | Frequency
#   * current (Sept 2025 HD2024_dict.xlsx onward):
#       varNumber | VarName | CodeValue | ValueLabel | Frequency | Percent
#
# Both layouts share the same case-insensitive header tokens but at different
# column positions, which previously broke the canonical build silently when
# CI re-extracted a fresh dict zip (Refresh #41).  These tests pin both
# layouts so any future positional regression surfaces here instead of in a
# 46-minute IPEDS rebuild.

# Helper: build a tiny xlsx with a single Frequencies sheet matching the
# header layout passed in, then return a fake (empty) zip path plus the
# extract root.  Skips the actual zip step by pre-populating the expected
# extract directory; expand_zip_if_missing() short-circuits when the
# destination exists with files.
.write_freq_fixture <- function(fixture_root, table_name, freq_df) {
  extract_dir <- file.path(fixture_root, "extracted")
  table_dir <- file.path(extract_dir, paste0("dict_", table_name))
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  xlsx_path <- file.path(table_dir, paste0(tolower(table_name), ".xlsx"))

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Frequencies")
  openxlsx::writeData(wb, "Frequencies", freq_df, colNames = TRUE)
  openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)

  fake_zip <- file.path(fixture_root, paste0(table_name, "_dict.zip"))
  invisible(file.create(fake_zip))
  list(zip = fake_zip, extract_root = extract_dir)
}

run_test("get_frequency_lookup handles current NCES header layout (varNumber/VarName/CodeValue/ValueLabel)", function() {
  fixture_root <- tempfile("freq-lookup-current-")
  dir.create(fixture_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  freq_df <- data.frame(
    varNumber = c("10016", "10016", "10031", "10031", "10031"),
    VarName = c("STABBR", "STABBR", "ICLEVEL", "ICLEVEL", "ICLEVEL"),
    CodeValue = c("AL", "CA", "1", "2", "3"),
    ValueLabel = c("Alabama", "California",
                   "Four or more years",
                   "At least 2 but less than 4 years",
                   "Less than 2 years"),
    Frequency = c("79", "668", "2500", "1100", "400"),
    Percent = c("1.3", "11", "62", "27", "10"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  fixture <- .write_freq_fixture(fixture_root, "HD2024", freq_df)

  result <- get_frequency_lookup(fixture$zip, "HD2024", "ICLEVEL", fixture$extract_root)

  assert_true(length(result) == 3L)
  assert_identical(sort(names(result)), c("1", "2", "3"))
  assert_identical(unname(result[["1"]]), "Four or more years")
  assert_identical(unname(result[["2"]]), "At least 2 but less than 4 years")
})

run_test("get_frequency_lookup handles legacy NCES header layout (VarName at column A, valuelabel lowercase)", function() {
  fixture_root <- tempfile("freq-lookup-legacy-")
  dir.create(fixture_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  freq_df <- data.frame(
    VarName = c("ICLEVEL", "ICLEVEL", "STABBR"),
    VarNumber = c("10031", "10031", "10016"),
    TableName = c("HD2024", "HD2024", "HD2024"),
    CodeValue = c("1", "2", "AL"),
    valuelabel = c("Four or more years",
                   "At least 2 but less than 4 years",
                   "Alabama"),
    Frequency = c("2500", "1100", "79"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  fixture <- .write_freq_fixture(fixture_root, "HD2024", freq_df)

  result <- get_frequency_lookup(fixture$zip, "HD2024", "ICLEVEL", fixture$extract_root)

  assert_true(length(result) == 2L)
  assert_identical(sort(names(result)), c("1", "2"))
  assert_identical(unname(result[["1"]]), "Four or more years")
  assert_identical(unname(result[["2"]]), "At least 2 but less than 4 years")
})

run_test("get_frequency_lookup returns empty character() when var_name is not in the sheet", function() {
  fixture_root <- tempfile("freq-lookup-empty-")
  dir.create(fixture_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  freq_df <- data.frame(
    varNumber = c("10016"),
    VarName = c("STABBR"),
    CodeValue = c("AL"),
    ValueLabel = c("Alabama"),
    Frequency = c("79"),
    Percent = c("1.3"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  fixture <- .write_freq_fixture(fixture_root, "HD2024", freq_df)

  result <- get_frequency_lookup(fixture$zip, "HD2024", "ICLEVEL", fixture$extract_root)
  assert_true(length(result) == 0L)
  assert_true(is.character(result))
})
