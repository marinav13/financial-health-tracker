run_test("IPEDS canonical pipeline fixture", function() {
  fixture_root <- tempfile("canonical-fixture-")
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
    file.path(fixture_root, "ipeds", "derived"),
    file.path(fixture_root, "ipeds", "manifests")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  file.copy(
    file.path(root, "scripts", "build_ipeds_canonical_dataset.R"),
    file.path(fixture_root, "scripts", "build_ipeds_canonical_dataset.R"),
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

  readr::write_csv(
    data.frame(
      year = integer(),
      table_name = character(),
      data_url = character(),
      stringsAsFactors = FALSE
    ),
    catalog_path,
    na = ""
  )

  setwd(fixture_root)
  canonical_env <- new.env(parent = globalenv())
  sys.source(file.path(fixture_root, "scripts", "build_ipeds_canonical_dataset.R"), envir = canonical_env)
  canonical_env$main(c(
    "--raw", raw_path,
    "--catalog", catalog_path,
    "--output", canonical_out,
    "--expanded-output", extended_out
  ))

  assert_true(file.exists(canonical_out), "Expected canonical output CSV to exist.")
  assert_true(file.exists(extended_out), "Expected extended output CSV to exist.")

  canonical_df <- readr::read_csv(canonical_out, show_col_types = FALSE)
  extended_df <- readr::read_csv(extended_out, show_col_types = FALSE)

  assert_identical(nrow(canonical_df), 2L)
  assert_identical(sort(as.integer(canonical_df$year)), c(2023L, 2024L))
  assert_identical(unique(canonical_df$institution_unique_name), "Example University | Boston | Massachusetts")
  assert_identical(unique(canonical_df$control_label), "Public")
  assert_true(all(c("tuition_dependence_pct", "revenue_total_adjusted", "pct_international_all") %in% names(canonical_df)))
  assert_true(all(c("operating_margin", "government_funding_total") %in% names(extended_df)))
  row_2024 <- canonical_df[canonical_df$year == 2024, , drop = FALSE]
  assert_equal(row_2024$tuition_dependence_pct[[1]], 30)
  assert_equal(row_2024$loss_amount[[1]], 100)
  assert_true(is.na(row_2024$pct_international_all[[1]]))
})
