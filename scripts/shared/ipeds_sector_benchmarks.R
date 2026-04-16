# ============================================================================
# scripts/shared/ipeds_sector_benchmarks.R
# ============================================================================
#
# PURPOSE:
#   Computes and applies sector-level benchmarks (medians, means, aggregates)
#   for comparing individual institutions against their peer group. Benchmarks
#   are computed from all institutions in the same sector (Public, Private
#   not-for-profit, Private for-profit) for each year.
#'
# WHERE IT FITS:
#   - Source this AFTER utils.R
#   - Use apply_sector_benchmarks() after individual institution enrichment
#   - Called once per data load to attach sector comparison columns
#
# KEY CONCEPTS:
#   - Sector: control_label (Public, Private not-for-profit, Private for-profit)
#   - Benchmark types: medians (typical value), means (average), counts (participation)
#   - Relative comparison: institution's metric vs. sector median (above/below/same)
#   - Narrative: human-readable comparison sentences for reports
#
# IPEDS VARIABLES USED:
#   - unitid: unique college ID
#   - control_label: institution type (Public/Private not-for-profit/Private for-profit)
#   - FTE: Full-Time Equivalent enrollment (for per-student metrics)
#   - Tuition dependence: percentage of revenue from net tuition
#   - Research spending: research expense per FTE student
#   - Student-to-staff ratio: FTE students / instructional staff FTE
#
# ============================================================================

# ---------------------------------------------------------------------------
# Tuition Dependence Benchmarks
# ---------------------------------------------------------------------------

#' Compute Sector Tuition Dependence Benchmarks
#'
#' Calculates median and mean tuition dependence (% of revenue from net tuition)
#' for each sector and year. Used to contextualize individual institutions'
#' reliance on tuition revenue against sector norms.
#'
#' @param df Data frame with one row per institution-year (all sectors)
#'
#' @return Data frame, one row per (control_label, year) combination:
#'   - sector_median_tuition_dependence_pct: median tuition % for sector
#'   - sector_mean_tuition_dependence_pct: mean tuition % for sector
#'   - sector_tuition_dependence_n: count of institutions reporting data
#'
#' @details
#'   Groups by control_label and year, then computes statistics on
#'   tuition_dependence_pct column. Returns NA when all values in a
#'   sector-year are NA (no data available).
#'
compute_sector_tuition_benchmarks <- function(df) {
  df %>%
    # Group by sector and year to compute sector-wide statistics
    group_by(control_label, year) %>%
    summarise(
      # Median tuition dependence: the "middle value" institution
      # (50th percentile), more robust to outliers than mean
      sector_median_tuition_dependence_pct = if (all(is.na(tuition_dependence_pct))) NA_real_ else stats::median(tuition_dependence_pct, na.rm = TRUE),
      # Mean tuition dependence: the average across the sector
      sector_mean_tuition_dependence_pct = if (all(is.na(tuition_dependence_pct))) NA_real_ else mean(tuition_dependence_pct, na.rm = TRUE),
      # Count of institutions with valid tuition dependence data
      sector_tuition_dependence_n = sum(!is.na(tuition_dependence_pct)),
      .groups = "drop"
    )
}

# ---------------------------------------------------------------------------
# Sector Enrollment Benchmarks
# ---------------------------------------------------------------------------

#' Compute Sector Enrollment Benchmarks
#'
#' Calculates total sector enrollment (national aggregate) and 5-year
#' enrollment trend for each sector and year. Shows how the overall sector
#' is growing or shrinking.
#'
#' @param df Data frame with one row per institution-year
#'
#' @return Data frame, one row per (control_label, year) combination:
#'   - sector_enrollment_total_national: total 12-month unduplicated headcount
#'     for all institutions in sector
#'   - sector_enrollment_pct_change_5yr_national: % change in sector total
#'     over past 5 years
#'
#' @details
#'   Two-step process:
#'     1. Group by sector and year, sum total enrollment
#'     2. Arrange by year, compute YoY lag and 5-year % change
#'
compute_sector_enrollment_benchmarks <- function(df) {
  df %>%
    # Aggregate enrollment across all institutions in each sector-year
    group_by(control_label, year) %>%
    summarise(
      # Sum of 12-month unduplicated headcount across all sector institutions
      sector_enrollment_total_national = sum(enrollment_headcount_total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Sort by sector and year for lag computation
    group_by(control_label) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      # Compute 5-year % change: (current enrollment - enrollment 5 years ago) / enrollment 5 years ago * 100
      # lag(x, 5) returns the value from 5 rows prior (5 years prior in this context)
      sector_enrollment_pct_change_5yr_national = safe_pct_change(
        sector_enrollment_total_national,
        dplyr::lag(sector_enrollment_total_national, 5)
      )
    ) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# Sector Research Spending Benchmarks
# ---------------------------------------------------------------------------

#' Compute Sector Research Spending Benchmarks
#'
#' Calculates the median research spending per FTE for institutions in each
#' sector that report research expense data. Also tracks what percentage of
#' institutions in the sector report positive research spending.
#'
#' @param df Data frame with one row per institution-year
#'
#' @return Data frame, one row per (control_label, unitid, year) combination
#'   (preserves unitid for left_join):
#'   - sector_research_spending_n: count of institutions in sector reporting
#'     research expense data
#'   - sector_research_spending_positive_n: count reporting positive amount
#'   - sector_research_spending_reporting_share_pct: % of reporters with
#'     positive values (0-100)
#'   - sector_median_research_expense_per_fte_positive: median per-FTE research
#'     spending among those with positive values
#'
#' @details
#'   Filters to institutions with positive research spending only when computing
#'   the median, to exclude zero values that skew the benchmark. Most teaching
#'   institutions have zero research spending and shouldn't pull down the median.
#'
compute_sector_research_benchmarks <- function(df) {
  df %>%
    # Group by sector and year to compute sector-wide statistics
    group_by(control_label, year) %>%
    mutate(
      # Count institutions in sector with research data (including zeros)
      sector_research_spending_n = sum(!is.na(research_expense_per_fte)),
      # Count institutions in sector with positive research spending
      sector_research_spending_positive_n = sum(!is.na(research_expense_per_fte) & research_expense_per_fte > 0),
      # % of reporting institutions that have positive research spending
      sector_research_spending_reporting_share_pct = ifelse(
        sector_research_spending_n > 0,
        round(100 * sector_research_spending_positive_n / sector_research_spending_n, 1),
        NA_real_
      ),
      # Median per-FTE research spending, considering only positive values
      sector_median_research_expense_per_fte_positive = ifelse(
        sector_research_spending_positive_n > 0,
        stats::median(research_expense_per_fte[!is.na(research_expense_per_fte) & research_expense_per_fte > 0]),
        NA_real_
      )
    ) %>%
    ungroup() %>%
    # Keep only the sector-level benchmark columns, retain unitid/year for joining
    transmute(
      unitid,
      year,
      sector_research_spending_n,
      sector_research_spending_positive_n,
      sector_research_spending_reporting_share_pct,
      sector_median_research_expense_per_fte_positive
    )
}

# ---------------------------------------------------------------------------
# Sector Staffing Benchmarks
# ---------------------------------------------------------------------------

#' Compute Sector Student-to-Instructional-Staff Benchmarks
#'
#' Calculates median student-to-instructional-staff ratios for each sector.
#' Lower ratios indicate more instructor time available per student.
#' Used to contextualize individual institution staffing efficiency.
#'
#' @param df Data frame with one row per institution-year
#'
#' @return Data frame, one row per (control_label, year) combination:
#'   - sector_median_students_per_instructional_staff_fte: median ratio
#'     (students per instructor FTE, considering only positive values)
#'
#' @details
#'   Filters to positive values only before computing median, to exclude
#'   institutions with zero or invalid staffing data that would skew results.
#'
compute_sector_staffing_benchmarks <- function(df) {
  df %>%
    # Group by sector and year to compute sector-wide statistics
    group_by(control_label, year) %>%
    summarise(
      # Median FTE students per instructional staff FTE
      # Only considers institutions with positive values to avoid zeros/invalid data
      sector_median_students_per_instructional_staff_fte = if (
        all(is.na(students_per_instructional_staff_fte) | students_per_instructional_staff_fte <= 0)
      ) {
        # All values are NA or non-positive: return NA
        NA_real_
      } else {
        # Compute median of positive values only
        stats::median(
          students_per_instructional_staff_fte[
            !is.na(students_per_instructional_staff_fte) & students_per_instructional_staff_fte > 0
          ],
          na.rm = TRUE
        )
      },
      .groups = "drop"
    )
}

# ---------------------------------------------------------------------------
# Apply All Sector Benchmarks
# ---------------------------------------------------------------------------

#' Apply Sector Benchmarks to Individual Institutions
#'
#' Master function that computes all sector benchmarks and joins them to
#' individual institution data, enabling peer comparisons. Also generates
#' narrative sentences describing how an institution compares to its sector.
#'
#' FLOW:
#'   1. Compute four types of sector benchmarks
#'   2. Remove any existing sector benchmark columns (for idempotency)
#'   3. Left-join all benchmarks to individual institution rows
#'   4. Compute comparison metrics (difference from median, categorical assessment)
#'   5. Generate narrative sentences for dashboards/reports
#'
#' @param df Data frame with one row per institution-year
#'
#' @return Data frame with all original columns plus:
#'   - Sector benchmark columns (sector_median_*, sector_mean_*, etc.)
#'   - Comparison columns (tuition_dependence_vs_sector_median_*)
#'   - Narrative columns (tuition_dependence_vs_sector_median_sentence, etc.)
#'
#' @details
#'   Joins on (control_label, year) for tuition/enrollment/staffing benchmarks.
#'   Joins on (unitid, year) for research benchmarks (research spans sectors
#'   differently). All metrics are computed fresh to avoid stale data.
#'
apply_sector_benchmarks <- function(df) {
  # Compute all sector benchmarks from the provided data
  sector_tuition_dependence_benchmarks <- compute_sector_tuition_benchmarks(df)
  sector_enrollment_benchmarks <- compute_sector_enrollment_benchmarks(df)
  sector_research_benchmarks <- compute_sector_research_benchmarks(df)
  sector_staffing_benchmarks <- compute_sector_staffing_benchmarks(df)

  # Start with original data, remove any existing sector benchmark columns
  # (ensures fresh computation and avoids duplicates)
  df %>%
    select(-any_of(c(
      "sector_median_tuition_dependence_pct",
      "sector_mean_tuition_dependence_pct",
      "sector_tuition_dependence_n",
      "sector_enrollment_total_national",
      "sector_enrollment_pct_change_5yr_national",
      "sector_research_spending_n",
      "sector_research_spending_positive_n",
      "sector_research_spending_reporting_share_pct",
      "sector_median_research_expense_per_fte_positive",
      "sector_median_students_per_instructional_staff_fte",
      "tuition_dependence_vs_sector_median_pct_points",
      "tuition_dependence_relative_to_sector_median",
      "tuition_dependence_vs_sector_median_sentence",
      "sector_enrollment_change_sentence"
    ))) %>%
    # Join tuition benchmarks by sector and year
    left_join(sector_tuition_dependence_benchmarks, by = c("control_label", "year")) %>%
    # Join enrollment benchmarks by sector and year
    left_join(sector_enrollment_benchmarks, by = c("control_label", "year")) %>%
    # Join research benchmarks by unitid and year (research is institution-specific)
    left_join(sector_research_benchmarks, by = c("unitid", "year")) %>%
    # Join staffing benchmarks by sector and year
    left_join(sector_staffing_benchmarks, by = c("control_label", "year")) %>%
    mutate(
      # ---------------------
      # Tuition Dependence Comparisons
      # ---------------------

      # Percentage point difference: this institution's tuition % minus sector median
      tuition_dependence_vs_sector_median_pct_points = tuition_dependence_pct - sector_median_tuition_dependence_pct,

      # Categorical assessment: is this institution above/below/at sector median?
      # Uses a 5 percentage point tolerance for "about the same"
      tuition_dependence_relative_to_sector_median = case_when(
        is.na(tuition_dependence_pct) | is.na(sector_median_tuition_dependence_pct) ~ NA_character_,
        # Institution's tuition % is within 5 points of sector median
        abs(tuition_dependence_pct - sector_median_tuition_dependence_pct) < 0.05 ~ "About the same as sector median",
        # Institution's tuition % exceeds sector median
        tuition_dependence_pct > sector_median_tuition_dependence_pct ~ "Above sector median",
        # Institution's tuition % is below sector median
        tuition_dependence_pct < sector_median_tuition_dependence_pct ~ "Below sector median",
        TRUE ~ NA_character_
      ),

      # Narrative sentence: institution's tuition dependence and comparison
      # E.g.: "This college got 45% of its revenue from net tuition in 2023,
      #        above the median of 40% for Public colleges."
      tuition_dependence_vs_sector_median_sentence = case_when(
        is.na(tuition_dependence_pct) | is.na(sector_median_tuition_dependence_pct) ~ NA_character_,
        TRUE ~ paste0(
          "This college got ",
          round(tuition_dependence_pct),
          "% of its revenue from net tuition in ",
          year,
          ", ",
          tolower(case_when(
            abs(tuition_dependence_pct - sector_median_tuition_dependence_pct) < 0.05 ~ "about the same as",
            tuition_dependence_pct > sector_median_tuition_dependence_pct ~ "above",
            tuition_dependence_pct < sector_median_tuition_dependence_pct ~ "below",
            TRUE ~ "compared with"
          )),
          " the median of ",
          round(sector_median_tuition_dependence_pct),
          "% for ",
          tolower(control_label),
          " colleges."
        )
      ),

      # ---------------------
      # Sector Enrollment Change Narrative
      # ---------------------

      # Narrative sentence comparing institution's enrollment trend to sector trend
      # E.g.: "12-month unduplicated headcount changed by -2.5% over the past five years.
      #        That compares with -1.2% for Public institutions in this tracker."
      sector_enrollment_change_sentence = ifelse(
        is.na(enrollment_pct_change_5yr) | is.na(sector_enrollment_pct_change_5yr_national),
        NA_character_,
        paste0(
          "12-month unduplicated headcount changed by ",
          round(enrollment_pct_change_5yr, 1),
          "% over the past five years. That compares with ",
          round(sector_enrollment_pct_change_5yr_national, 1),
          "% for ",
          tolower(control_label),
          " institutions in this tracker."
        )
      )
    )
}
