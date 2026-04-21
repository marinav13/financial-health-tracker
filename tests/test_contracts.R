# tests/test_contracts.R
#
# Unit tests for scripts/shared/contracts.R.
# test_support.R is expected to have already sourced contracts.R via
#   source(file.path(root, "scripts", "shared", "contracts.R"))
# which is added to test_support.R alongside the other shared helpers.

# ---------------------------------------------------------------------------
# assert_columns
# ---------------------------------------------------------------------------

run_test("assert_columns passes when all required columns present", function() {
  df <- data.frame(unitid = "1", year = 2024, revenue_total = 1e6)
  result <- assert_columns(df, c("unitid", "year"), "test df")
  assert_identical(result, df)
})

run_test("assert_columns stops on missing columns", function() {
  df <- data.frame(unitid = "1", year = 2024)
  err <- tryCatch(
    assert_columns(df, c("unitid", "year", "revenue_total", "expenses_total"), "my df"),
    error = function(e) conditionMessage(e)
  )
  assert_true(grepl("Contract violation in my df", err))
  assert_true(grepl("revenue_total", err))
  assert_true(grepl("expenses_total", err))
  assert_true(grepl("2 required", err))
})

run_test("assert_columns stops listing each missing column on its own line", function() {
  df <- data.frame(a = 1)
  err <- tryCatch(
    assert_columns(df, c("b", "c", "d"), "df"),
    error = function(e) conditionMessage(e)
  )
  assert_true(grepl("b", err) && grepl("c", err) && grepl("d", err))
})

# ---------------------------------------------------------------------------
# assert_no_duplicate_keys
# ---------------------------------------------------------------------------

run_test("assert_no_duplicate_keys passes for unique keys", function() {
  df <- data.frame(
    unitid = c("100", "100", "200"),
    year   = c(2023,  2024,  2024)
  )
  result <- assert_no_duplicate_keys(df, c("unitid", "year"), "test df")
  assert_identical(result, df)
})

run_test("assert_no_duplicate_keys stops on duplicate keys", function() {
  df <- data.frame(
    unitid = c("100", "100"),
    year   = c(2024,  2024)
  )
  err <- tryCatch(
    assert_no_duplicate_keys(df, c("unitid", "year"), "dup df"),
    error = function(e) conditionMessage(e)
  )
  assert_true(grepl("Contract violation in dup df", err))
  assert_true(grepl("unitid, year", err))
  assert_true(grepl("1 total", err))
})

run_test("assert_no_duplicate_keys skips gracefully when key col absent", function() {
  df <- data.frame(unitid = c("1", "1"))
  # 'year' is missing – should not error, just return df
  result <- assert_no_duplicate_keys(df, c("unitid", "year"), "df")
  assert_identical(result, df)
})

# ---------------------------------------------------------------------------
# assert_column_types
# ---------------------------------------------------------------------------

run_test("assert_column_types passes when types are correct", function() {
  df <- data.frame(
    unitid = c("100", "200"),
    year   = c(2023L, 2024L),
    stringsAsFactors = FALSE
  )
  result <- assert_column_types(df, list(unitid = is.character, year = is.integer), "df")
  assert_identical(result, df)
})

run_test("assert_column_types stops when type is wrong", function() {
  df <- data.frame(unitid = c(100L, 200L), year = c(2023L, 2024L))
  err <- tryCatch(
    assert_column_types(df, list(unitid = is.character), "df"),
    error = function(e) conditionMessage(e)
  )
  assert_true(grepl("Contract violation", err))
  assert_true(grepl("unitid", err))
})

run_test("assert_column_types skips columns not in df", function() {
  df <- data.frame(year = 2024L)
  # unitid absent – should not error
  result <- assert_column_types(df, list(unitid = is.character, year = is.integer), "df")
  assert_identical(result, df)
})

# ---------------------------------------------------------------------------
# validate_canonical_output
# ---------------------------------------------------------------------------

run_test("validate_canonical_output passes for a valid minimal canonical df", function() {
  df <- data.frame(
    unitid                  = "123",
    institution_name        = "Test U",
    institution_unique_name = "Test U | City | ST",
    year                    = 2024,
    control_label           = "Public",
    state                   = "MA",
    city                    = "Boston",
    enrollment_headcount_total = 5000,
    revenue_total           = 1e7,
    expenses_total          = 9.5e6,
    loss_amount             = NA_real_,
    net_tuition_total       = 4e6,
    tuition_dependence_pct  = 0.4,
    ended_2024_at_loss      = FALSE,
    losses_last_3_of_5      = FALSE,
    stringsAsFactors = FALSE
  )
  result <- validate_canonical_output(df)
  assert_identical(result, df)
})

run_test("validate_canonical_output stops when required column missing", function() {
  df <- data.frame(
    unitid = "123", year = 2024, control_label = "Public",
    stringsAsFactors = FALSE
  )
  err <- tryCatch(
    validate_canonical_output(df),
    error = function(e) conditionMessage(e)
  )
  assert_true(grepl("canonical IPEDS output", err))
  assert_true(grepl("institution_name", err))
})

run_test("validate_canonical_output stops on duplicate unitid+year", function() {
  base_row <- list(
    unitid = "123", institution_name = "X", institution_unique_name = "X|Y|Z",
    year = 2024, control_label = "Public", state = "MA", city = "Boston",
    enrollment_headcount_total = 1, revenue_total = 1, expenses_total = 1,
    loss_amount = NA, net_tuition_total = 1, tuition_dependence_pct = 0.3,
    ended_2024_at_loss = FALSE, losses_last_3_of_5 = FALSE
  )
  df <- rbind(as.data.frame(base_row, stringsAsFactors = FALSE),
              as.data.frame(base_row, stringsAsFactors = FALSE))
  err <- tryCatch(
    validate_canonical_output(df),
    error = function(e) conditionMessage(e)
  )
  assert_true(grepl("duplicate", err))
})

run_test("validate_canonical_output stops when unitid is not character", function() {
  df <- data.frame(
    unitid                  = 123L,           # integer – should be character
    institution_name        = "Test U",
    institution_unique_name = "Test U | City | ST",
    year                    = 2024,
    control_label           = "Public",
    state                   = "MA",
    city                    = "Boston",
    enrollment_headcount_total = 5000,
    revenue_total           = 1e7,
    expenses_total          = 9.5e6,
    loss_amount             = NA_real_,
    net_tuition_total       = 4e6,
    tuition_dependence_pct  = 0.4,
    ended_2024_at_loss      = FALSE,
    losses_last_3_of_5      = FALSE
  )
  err <- tryCatch(
    validate_canonical_output(df),
    error = function(e) conditionMessage(e)
  )
  assert_true(grepl("unitid", err))
})

# ---------------------------------------------------------------------------
# validate_workbook_input
# ---------------------------------------------------------------------------

run_test("validate_workbook_input passes for valid minimal workbook df", function() {
  df <- data.frame(
    unitid                     = "123",
    institution_name           = "Test U",
    year                       = 2024,
    control_label              = "Public",
    state                      = "MA",
    enrollment_headcount_total = 5000,
    revenue_total              = 1e7,
    expenses_total             = 9.5e6,
    tuition_dependence_pct     = 0.4,
    ended_2024_at_loss         = FALSE,
    stringsAsFactors = FALSE
  )
  result <- validate_workbook_input(df)
  assert_identical(result, df)
})

run_test("validate_workbook_input stops when required column absent", function() {
  df <- data.frame(unitid = "123", year = 2024, control_label = "Public",
                   state = "MA", stringsAsFactors = FALSE)
  err <- tryCatch(validate_workbook_input(df), error = function(e) conditionMessage(e))
  assert_true(grepl("article workbook input", err))
})

# ---------------------------------------------------------------------------
# validate_export_input
# ---------------------------------------------------------------------------

run_test("validate_export_input passes for valid minimal export df", function() {
  df <- data.frame(
    unitid                     = "123",
    institution_name           = "Test U",
    institution_unique_name    = "Test U | City | ST",
    year                       = 2024,
    control_label              = "Public",
    state                      = "MA",
    enrollment_headcount_total = 5000,
    revenue_total              = 1e7,
    expenses_total             = 9.5e6,
    tuition_dependence_pct     = 0.4,
    stringsAsFactors = FALSE
  )
  result <- validate_export_input(df)
  assert_identical(result, df)
})

run_test("validate_export_input stops when required column absent", function() {
  df <- data.frame(unitid = "123", year = 2024, institution_name = "X",
                   control_label = "Public", state = "MA",
                   stringsAsFactors = FALSE)
  err <- tryCatch(validate_export_input(df), error = function(e) conditionMessage(e))
  assert_true(grepl("web export input", err))
})

run_test("validate_export_input stops on duplicate unitid+year", function() {
  row <- data.frame(
    unitid = "123", institution_name = "X", institution_unique_name = "X|Y|Z",
    year = 2024, control_label = "Public", state = "MA",
    enrollment_headcount_total = 1, revenue_total = 1,
    expenses_total = 1, tuition_dependence_pct = 0.3,
    stringsAsFactors = FALSE
  )
  df <- rbind(row, row)
  err <- tryCatch(validate_export_input(df), error = function(e) conditionMessage(e))
  assert_true(grepl("duplicate", err))
})
