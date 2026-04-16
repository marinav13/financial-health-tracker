root <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)

source(file.path(root, "scripts", "shared", "utils.R"))

required_pkgs <- c("dplyr", "jsonlite", "purrr", "readr", "readxl", "stringr", "xml2", "httr2")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing_pkgs) > 0L) {
  stop(
    paste(
      "Smoke tests require these installed packages:",
      paste(missing_pkgs, collapse = ", ")
    ),
    call. = FALSE
  )
}
invisible(lapply(required_pkgs, library, character.only = TRUE))

source(file.path(root, "scripts", "shared", "export_helpers.R"))
source(file.path(root, "scripts", "shared", "accreditation_helpers.R"))
source(file.path(root, "scripts", "shared", "grant_witness_helpers.R"))
source(file.path(root, "scripts", "shared", "workbook_helpers.R"))
source(file.path(root, "scripts", "shared", "ipeds_helpers.R"))

failures <- character()
passes <- 0L

fail_test <- function(name, message) {
  failures <<- c(failures, name)
  cat(sprintf("FAIL %s\n%s\n", name, message))
}

pass_test <- function(name) {
  passes <<- passes + 1L
  cat(sprintf("PASS %s\n", name))
}

assert_true <- function(condition, message = "Assertion failed.") {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

assert_identical <- function(actual, expected, message = "Values differed.") {
  if (!identical(actual, expected)) {
    stop(
      sprintf(
        "%s\nExpected: %s\nActual: %s",
        message,
        paste(capture.output(str(expected)), collapse = " "),
        paste(capture.output(str(actual)), collapse = " ")
      ),
      call. = FALSE
    )
  }
}

assert_equal <- function(actual, expected, message = "Values were not equal.") {
  comparison <- all.equal(actual, expected, check.attributes = FALSE)
  if (!isTRUE(comparison)) {
    stop(
      sprintf("%s\n%s", message, paste(comparison, collapse = "\n")),
      call. = FALSE
    )
  }
}

run_test <- function(name, fn) {
  tryCatch(
    {
      fn()
      pass_test(name)
    },
    error = function(e) fail_test(name, conditionMessage(e))
  )
}

run_test("CLI arg helpers", function() {
  args <- c("--input", "file.csv", "--refresh", "TRUE", "--dry-run")
  assert_identical(get_arg(args, "--input"), "file.csv")
  assert_identical(get_arg(args, "--missing", "fallback"), "fallback")
  assert_true(arg_has(args, "--dry-run"))
  assert_true(!arg_has(args, "--force"))
})

run_test("Numeric helpers", function() {
  assert_equal(to_num(c("1,234", "", "NULL")), c(1234, NA_real_, NA_real_))
  assert_identical(safe_divide(10, 2), 5)
  assert_true(is.na(safe_divide(10, 0)))
  assert_identical(safe_pct_change(90, 100), -10)
})

run_test("File helpers", function() {
  temp_dir <- tempfile("helper-smoke-")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  sample_txt <- file.path(temp_dir, "sample.txt")
  writeLines("ok", sample_txt)
  require_existing_local_file(sample_txt, "sample text", "unused")
  assert_identical(NULL %||% "fallback", "fallback")
  assert_identical(NA %||% "fallback", "fallback")

  csv_path <- file.path(temp_dir, "sample.csv")
  write_csv_atomic(data.frame(col = 1, stringsAsFactors = FALSE), csv_path)
  assert_true(file.exists(csv_path), "Expected atomic CSV output to exist.")

  missing_message <- tryCatch(
    {
      require_existing_local_file(file.path(temp_dir, "missing.txt"), "missing text", "create it")
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  assert_true(grepl("Missing required local source file", missing_message, fixed = TRUE))
})

run_test("Collapse unique values", function() {
  assert_identical(collapse_unique_values(c("b", NA, "a", "b")), "a; b")
  assert_true(is.na(collapse_unique_values(c(NA, NA))))
})

run_test("Export helper basics", function() {
  temp_file <- tempfile("local-file-")
  writeLines("ok", temp_file)
  require_local_file(temp_file, "temp file", "unused")

  assert_identical(make_export_id("cut", "12345", "Name", "NY"), "12345")
  assert_identical(
    make_export_id("cut", "", "Example University", "NY"),
    "cut-example-university-ny"
  )
  assert_identical(
    normalize_display_institution_name("Arizona State University Campus Immersion"),
    "Arizona State University"
  )
  assert_identical(or_null(c("", "filled")), NA)
  assert_identical(normalize_control_label("private non-profit"), "Private not-for-profit")
  assert_identical(derive_positions_affected(NA, "The college laid off 23 employees", "", "", ""), 23L)
  assert_equal(
    build_series(data.frame(year = c(2024L, 2025L), value = c(1, NA_real_)), "value"),
    list(list(year = 2024L, value = 1))
  )
})

run_test("International sentence builder", function() {
  sentence <- build_international_students_sentence(2024, 0.12, 0.08, 0.21)
  assert_true(grepl("In 2024, 12", sentence, fixed = TRUE))
  assert_true(grepl("8", sentence, fixed = TRUE))
  assert_true(grepl("21", sentence, fixed = TRUE))
})

run_test("Accreditation text and classification", function() {
  assert_identical(unname(clean_text("<p>Hello<br>World</p>")), "Hello World")
  html <- paste0(
    "<html><head><title> Example Title </title>",
    "<meta property=\"article:modified_time\" content=\"2025-02-03T12:00:00Z\">",
    "</head></html>"
  )
  assert_identical(unname(extract_page_title(html)), "Example Title")
  assert_identical(unname(extract_page_modified_date(html)), "2025-02-03")
  assert_identical(state_name("MA"), "Massachusetts")
  parsed <- extract_name_state_from_item("Example College, Boston, MA")
  assert_identical(unname(parsed$institution_name_raw), "Example College")
  assert_identical(unname(parsed$institution_state_raw), "Massachusetts")
  assert_identical(classify_action("Placed on probation"), "probation")
  assert_identical(classify_status("Removed from probation"), "resolved")
  assert_true(has_public_action_keywords("Public Notice of Concern"))
})

run_test("Accreditation tracker matching", function() {
  actions_df <- data.frame(
    institution_name_normalized = c("example college", "name only match"),
    institution_state_normalized = c("Massachusetts", "Texas"),
    stringsAsFactors = FALSE
  )
  lookup_exact <- data.frame(
    matched_unitid = "100",
    tracker_name = "Example College",
    tracker_state = "Massachusetts",
    norm_name = "example college",
    state_match = "Massachusetts",
    stringsAsFactors = FALSE
  )
  lookup_name_only <- data.frame(
    matched_unitid = "200",
    tracker_name = "Name Only Match University",
    tracker_state = "California",
    norm_name = "name only match",
    stringsAsFactors = FALSE
  )

  matched <- match_institutions_to_tracker(actions_df, lookup_exact, lookup_name_only)
  assert_identical(matched$unitid[[1]], "100")
  assert_identical(matched$match_method[[1]], "normalized_name_plus_state")
  assert_identical(matched$unitid[[2]], "200")
  assert_identical(matched$match_method[[2]], "normalized_name_only")
})

run_test("Grant Witness name helpers", function() {
  assert_identical(
    prettify_institution_name("TEXAS A&M UNIVERSITY"),
    "Texas A&M University"
  )
  assert_identical(
    simplify_institution_name("Board of Trustees of Southern Illinois University"),
    "southern illinois university"
  )
  assert_true(
    grepl(
      "subaward",
      detect_pass_through_phrase("Regional grantmaker initiative", "Will administer subawards"),
      fixed = TRUE
    )
  )
  assert_identical(
    strip_legal_prefixes("Board of Trustees of Southern Illinois University"),
    "Southern Illinois University"
  )
  assert_identical(abbr_to_state("IL"), "Illinois")
  assert_identical(normalize_city("San Juan, PR"), "san juan pr")
  assert_true(is_currently_disrupted("nih", "Frozen Funding"))
  assert_true(!is_currently_disrupted("nsf", "Reinstated"))
  assert_equal(
    classify_status_bucket(
      c("nih", "cdc", "epa"),
      c("Unfrozen Funding", "At Risk", "Reinstated")
    ),
    c("not_currently_disrupted", "currently_disrupted", "not_currently_disrupted")
  )
})

run_test("Grant Witness standardization config", function() {
  sample_nsf <- data.frame(
    grant_id = "12345",
    status = "Terminated",
    org_name = "BOARD OF TRUSTEES OF SOUTHERN ILLINOIS UNIVERSITY",
    org_state = "IL",
    org_city = "Carbondale",
    award_type = "Research",
    project_title = "CAMPUS RESEARCH",
    abstract = "Study of campus finance",
    nsf_start_date = NA_character_,
    usasp_start_date = "2024-01-01",
    nsf_end_date = NA_character_,
    usasp_end_date = "2025-01-01",
    termination_date = "2024-10-01",
    reinstatement_date = NA_character_,
    estimated_budget = "$100,000",
    estimated_outlays = "50000",
    estimated_remaining = "50000",
    usaspending_url = "https://example.com/award/abc",
    nsf_url = "https://nsf.example/123",
    stringsAsFactors = FALSE
  )

  standardized <- standardize_grant_witness_rows("nsf", sample_nsf, "nsf_terminations.csv")
  assert_identical(standardized$grant_id[[1]], "12345")
  assert_identical(standardized$organization_state[[1]], "Illinois")
  assert_identical(standardized$start_date[[1]], "2024-01-01")
  assert_equal(standardized$award_value[[1]], 100000)
  assert_identical(standardized$remaining_field[[1]], "estimated_remaining")
  assert_identical(standardized$detail_url[[1]], "https://nsf.example/123")

  temp_file <- tempfile("grant-witness-cache-")
  writeLines("cached", temp_file)
  maybe_download("https://invalid.example/grants.csv", temp_file, skip_download = TRUE)
  assert_true(file.exists(temp_file), "Expected cached Grant Witness file to be reused.")
})

run_test("Export bundle helpers", function() {
  temp_dir <- tempfile("export-bundle-")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  school <- list(
    unitid = "123",
    financial_unitid = "123",
    has_financial_profile = TRUE,
    is_primary_tracker = TRUE,
    institution_name = "Arizona State University Campus Immersion",
    city = "Tempe",
    state = "AZ",
    control_label = "Public",
    category = "Research"
  )

  assert_identical(
    build_institution_unique_name(school$institution_name, school$city, school$state),
    "Arizona State University | Tempe | AZ"
  )

  picked <- pick_first_present(
    data.frame(primary = NA_character_, fallback = "kept", stringsAsFactors = FALSE),
    c("missing", "primary", "fallback")
  )
  assert_identical(picked[[1]], "kept")

  entry <- build_school_index_entry(school, extra_flag = TRUE)
  assert_identical(entry$institution_unique_name, "Arizona State University | Tempe | AZ")
  assert_true(isTRUE(entry$extra_flag))

  bundle_paths <- write_export_bundle(
    export_obj = list(schools = list(school)),
    data_dir = temp_dir,
    export_filename = "schools.json",
    index_filename = "schools-index.json",
    index_builder = function(one_school) list(extra_flag = TRUE)
  )

  assert_true(file.exists(bundle_paths$export_path))
  assert_true(file.exists(bundle_paths$index_path))

  index_rows <- jsonlite::read_json(bundle_paths$index_path, simplifyVector = TRUE)
  assert_identical(index_rows$institution_unique_name[[1]], "Arizona State University | Tempe | AZ")
  assert_true(isTRUE(index_rows$extra_flag[[1]]))
})

run_test("Workbook helper grouping", function() {
  group_list <- list(
    all = data.frame(flag = c("Yes", "No"), value = c("10", "20"), metric = c(1, 2), stringsAsFactors = FALSE),
    public = data.frame(flag = "Yes", value = "5", metric = 4, stringsAsFactors = FALSE),
    private_nfp = data.frame(flag = character(), value = character(), metric = numeric(), stringsAsFactors = FALSE),
    private_fp = data.frame(flag = "No", value = "7", metric = 3, stringsAsFactors = FALSE),
    bacc_public = data.frame(flag = "Yes", value = "8", metric = 6, stringsAsFactors = FALSE),
    bacc_private_nfp = data.frame(flag = "No", value = "9", metric = 1, stringsAsFactors = FALSE),
    bacc_private_fp = data.frame(flag = "Yes", value = "11", metric = 5, stringsAsFactors = FALSE)
  )

  counts <- count_by_group_from(group_list, function(df) yes_flag(df$flag))
  pcts <- pct_by_group_from(group_list, function(df) yes_flag(df$flag))
  medians <- numeric_stat_by_group(group_list, "value")
  tops <- top_metric_by_group_from(group_list, "metric")
  summary_rows <- make_count_pct_rows("Example metric", counts, pcts)

  assert_equal(unname(counts[["all"]]), 1)
  assert_equal(unname(pcts[["public"]]), 100)
  assert_equal(unname(medians[["all"]]), 15)
  assert_equal(tops$public$metric[[1]], 4)
  assert_identical(nrow(summary_rows), 2L)
})

run_test("Workbook helper row appends", function() {
  row_a <- make_group_row("Metric A", "count", c(all = 1, public = 2))
  row_b <- make_group_row("Metric B", "percent", c(all = 50, public = 40))
  combined <- append_rows(row_a, row_b)

  assert_identical(nrow(combined), 2L)
  assert_identical(combined$metric[[1]], "Metric A")
  assert_identical(combined$statistic[[2]], "percent")
})

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

cat(sprintf("\nShared helper smoke tests: %d passed, %d failed.\n", passes, length(failures)))
if (length(failures) > 0L) {
  stop("Smoke tests failed.", call. = FALSE)
}
