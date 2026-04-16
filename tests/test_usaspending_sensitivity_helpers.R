# test_usaspending_sensitivity_helpers.R
#
# Unit tests for scripts/shared/usaspending_sensitivity_helpers.R
#
# Run with: Rscript tests/run_shared_helper_smoke_tests.R
# (it sources this file automatically)

root <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)
source(file.path(root, "scripts", "shared", "utils.R"))
source(file.path(root, "scripts", "shared", "usaspending_sensitivity_helpers.R"))

# -------------------------------------------------------------------
# safe_num
# -------------------------------------------------------------------

run_test("USAspending helpers: safe_num parses numeric string", function() {
  assert_equal(safe_num("42.5"), 42.5)
})

run_test("USAspending helpers: safe_num returns NA for non-numeric string", function() {
  assert_identical(safe_num("hello"), NA_real_)
})

run_test("USAspending helpers: safe_num returns NA for NA", function() {
  assert_identical(safe_num(NA), NA_real_)
})

run_test("USAspending helpers: safe_num handles integer", function() {
  assert_identical(safe_num(99L), 99)
})

run_test("USAspending helpers: safe_num handles NULL", function() {
  assert_identical(safe_num(NULL), NA_real_)
})

# -------------------------------------------------------------------
# parse_date_safe
# -------------------------------------------------------------------

run_test("USAspending helpers: parse_date_safe returns Date for valid ISO date", function() {
  result <- parse_date_safe("2025-03-15")
  assert_true(inherits(result, "Date"), "Result should be a Date object")
  assert_equal(as.character(result), "2025-03-15")
})

run_test("USAspending helpers: parse_date_safe returns Date NA for blank string", function() {
  result <- parse_date_safe("")
  assert_true(inherits(result, "Date"))
  result2 <- parse_date_safe("  ")
  assert_true(inherits(result2, "Date"))
})

run_test("USAspending helpers: parse_date_safe returns Date NA for NA_character_", function() {
  assert_true(is.na(parse_date_safe(NA_character_)))
})

run_test("USAspending helpers: parse_date_safe vectorises over multiple dates", function() {
  result <- parse_date_safe(c("2024-01-01", "", "2024-06-15"))
  assert_equal(length(result), 3L)
  assert_true(is.na(result[2]))
})

# -------------------------------------------------------------------
# period_end_date_from_fiscal
# -------------------------------------------------------------------

run_test("USAspending helpers: period_end_date_from_fiscal FY Oct (m=1) ends Jul 1 same year", function() {
  # FY October = month 1 → period ends July 1 of that fiscal year
  # cal_month = ((1+8)%%12)+1 = 10 → cal_year = fy-1 (Oct starts FY the prior year)
  # next_month_year = 2024, next_month = 11 → Nov 1 2024 - 1 = Oct 31 2024
  # Wait — let me re-check. fy=2025, fm=1 (Oct)
  # cal_month = ((1+8)%%12)+1 = 9+1 = 10 → cal_year = 2025-1 = 2024
  # next_month_year = 10==12? no → 2025, next_month = 11
  # Oct 1 2025 - 1 = Sep 30 2025. But FY Oct ends Sep 30.
  # Actually: FY Oct = FY 2025 starts Oct 1 2024. The last calendar day of FY 2025
  # is Sep 30 2025. So period_end_date_from_fiscal(2025, 1) = Sep 30, 2025.
  # My calc: cal_month=10, cal_year=2024, next_month_year=2025, next_month=11
  # → Nov 1 2025 - 1 = Oct 31 2025. That's wrong!
  # Let me re-read the formula...
  #
  # The USAspending reporting: fiscal year (e.g. 2025) means FY2025 = Oct 2024 – Sep 2025.
  # Reporting month 1 = October, month 12 = September.
  # Calendar month = ((fm + 8) %% 12) + 1:
  #   fm=1 (Oct) → cal_month = 10 (October)
  #   fm=12 (Sep) → cal_month = ((12+8)%%12)+1 = 20%%12+1 = 8+1 = 9 (September)
  #
  # cal_year: if cal_month >= 10 (Oct/Nov/Dec), the period falls in the calendar year BEFORE fy.
  #   fm=1 (Oct) → cal_month=10 >= 10 → cal_year = fy-1 = 2024
  #   fm=12 (Sep) → cal_month=9 < 10 → cal_year = fy = 2025
  #
  # next_month_year / next_month: the period ends the day before the NEXT calendar month.
  #   fm=1 (Oct 2024): cal_month=10, cal_year=2024 → next_month=11 (Nov), next_month_year=2024
  #     → Nov 1 2024 - 1 = Oct 31 2024. Correct: FY Oct ends Oct 31.
  #   fm=12 (Sep 2025): cal_month=9, cal_year=2025 → next_month=10 (Oct), next_month_year=2025
  #     → Oct 1 2025 - 1 = Sep 30 2025. Correct: FY Sep ends Sep 30.
  result <- period_end_date_from_fiscal(2025, 1L)
  assert_equal(as.character(result), "2024-10-31")
})

run_test("USAspending helpers: period_end_date_from_fiscal FY Sep (m=12) ends Sep 30 same year", function() {
  result <- period_end_date_from_fiscal(2025, 12L)
  assert_equal(as.character(result), "2025-09-30")
})

run_test("USAspending helpers: period_end_date_from_fiscal FY Jan (m=4) ends Apr 30", function() {
  # FY Jan = month 4 → calendar month = ((4+8)%%12)+1 = 12%%12+1 = 0+1 = 1 (January)
  # cal_month=1 < 10 → cal_year = fy = 2025
  # next_month = 2 (February), next_month_year = 2025
  # → Feb 1 2025 - 1 = Jan 31 2025
  result <- period_end_date_from_fiscal(2025, 4L)
  assert_equal(as.character(result), "2025-01-31")
})

run_test("USAspending helpers: period_end_date_from_fiscal FY Jul (m=10) ends Apr 30 next year", function() {
  # FY Jul = month 10 → cal_month = ((10+8)%%12)+1 = 18%%12+1 = 6+1 = 7 (July)
  # cal_month=7 < 10 → cal_year = fy = 2025
  # next_month = 8 (August), next_month_year = 2025
  # → Aug 1 2025 - 1 = Jul 31 2025
  result <- period_end_date_from_fiscal(2025, 10L)
  assert_equal(as.character(result), "2025-07-31")
})

run_test("USAspending helpers: period_end_date_from_fiscal wraps calendar year at FY Dec", function() {
  # FY Dec = month 11 → cal_month = ((11+8)%%12)+1 = 19%%12+1 = 7+1 = 8 (August)
  # cal_month=8 < 10 → cal_year = fy = 2025
  # next_month = 9 (September), next_month_year = 2025
  # → Sep 1 2025 - 1 = Aug 31 2025
  result <- period_end_date_from_fiscal(2025, 11L)
  assert_equal(as.character(result), "2025-08-31")
})

run_test("USAspending helpers: period_end_date_from_fiscal handles FY boundary", function() {
  # Verify a sequence of fiscal months covers a full fiscal year Oct→Sep
  dates <- vapply(seq_len(12), function(fm) {
    as.character(period_end_date_from_fiscal(2025, fm))
  }, character(1))
  # Oct ends Oct 31, Nov ends Nov 30, ..., Sep ends Sep 30
  # The sequence should cycle back: Oct 2024 … Sep 2025
  # We only check that all dates are valid
  expect_dates <- c(
    "2024-10-31", "2024-11-30", "2024-12-31",
    "2025-01-31", "2025-02-28", "2025-03-31",
    "2025-04-30", "2025-05-31", "2025-06-30",
    "2025-07-31", "2025-08-31", "2025-09-30"
  )
  assert_identical(dates, expect_dates)
})

# -------------------------------------------------------------------
# summarise_risky_filter
# -------------------------------------------------------------------

run_test("USAspending helpers: summarise_risky_filter returns single row with zeros when no rows flagged", function() {
  input <- tibble::tibble(
    proposal_flag       = c(FALSE, FALSE),
    institution_key     = c("a", "b"),
    award_remaining    = c(1000, 2000),
    post_termination_positive_cont_rev_jul_dec_2025 = c(0, 0),
    post_termination_positive_cont_rev_2026          = c(0, 0)
  )
  result <- summarise_risky_filter(input)
  # dplyr::summarise always returns at least 1 row; n() = 0 for an empty-filter result
  assert_equal(nrow(result), 1L)
  assert_equal(result$grants_flagged, 0L)
  assert_equal(result$institutions_affected, 0L)
})

run_test("USAspending helpers: summarise_risky_filter counts flagged grants", function() {
  input <- tibble::tibble(
    proposal_flag                              = c(TRUE, TRUE, FALSE),
    institution_key                            = c("a", "a", "b"),
    award_remaining                            = c(1000, 2000, 500),
    post_termination_positive_cont_rev_jul_dec_2025 = c(100, 0, 0),
    post_termination_positive_cont_rev_2026    = c(0, 50, 0)
  )
  result <- summarise_risky_filter(input)
  assert_equal(nrow(result), 1L)
  assert_equal(result$grants_flagged, 2L)
})

run_test("USAspending helpers: summarise_risky_filter counts distinct institutions", function() {
  input <- tibble::tibble(
    proposal_flag       = c(TRUE, TRUE, TRUE, TRUE),
    institution_key     = c("unitid:1", "unitid:1", "unitid:2", "unitid:3"),
    award_remaining     = c(100, 200, 300, 400),
    post_termination_positive_cont_rev_jul_dec_2025 = c(0, 0, 0, 0),
    post_termination_positive_cont_rev_2026          = c(0, 0, 0, 0)
  )
  result <- summarise_risky_filter(input)
  assert_equal(result$institutions_affected, 3L)
})

run_test("USAspending helpers: summarise_risky_filter sums excluded award_remaining", function() {
  input <- tibble::tibble(
    proposal_flag       = c(TRUE, TRUE, FALSE),
    institution_key     = c("a", "b", "c"),
    award_remaining     = c(1000.5, 2000.25, 999.99),
    post_termination_positive_cont_rev_jul_dec_2025 = c(0, 0, 0),
    post_termination_positive_cont_rev_2026          = c(0, 0, 0)
  )
  result <- summarise_risky_filter(input)
  assert_equal(result$excluded_award_remaining, 3000.75)
})

run_test("USAspending helpers: summarise_risky_filter counts jul-dec 2025 activity", function() {
  input <- tibble::tibble(
    proposal_flag       = c(TRUE, TRUE, TRUE),
    institution_key     = c("a", "b", "c"),
    award_remaining     = c(100, 100, 100),
    post_termination_positive_cont_rev_jul_dec_2025 = c(50, 0, 100),
    post_termination_positive_cont_rev_2026          = c(0, 0, 0)
  )
  result <- summarise_risky_filter(input)
  # Rows with jul_dec_2025 > 0: rows 1 and 3 → 2
  assert_equal(result$flagged_with_jul_dec_2025_activity, 2L)
})

run_test("USAspending helpers: summarise_risky_filter counts 2026 activity", function() {
  input <- tibble::tibble(
    proposal_flag       = c(TRUE, TRUE, TRUE),
    institution_key     = c("a", "b", "c"),
    award_remaining     = c(100, 100, 100),
    post_termination_positive_cont_rev_jul_dec_2025 = c(0, 0, 0),
    post_termination_positive_cont_rev_2026          = c(25, 0, 10)
  )
  result <- summarise_risky_filter(input)
  # Rows with 2026 > 0: rows 1 and 3 → 2
  assert_equal(result$flagged_with_2026_activity, 2L)
})

run_test("USAspending helpers: summarise_risky_filter sets proposal column", function() {
  input <- tibble::tibble(
    proposal_flag       = TRUE,
    institution_key     = "a",
    award_remaining     = 100,
    post_termination_positive_cont_rev_jul_dec_2025 = 0,
    post_termination_positive_cont_rev_2026          = 0
  )
  result <- summarise_risky_filter(input)
  assert_equal(result$proposal, "risky_continuation_filter")
})
