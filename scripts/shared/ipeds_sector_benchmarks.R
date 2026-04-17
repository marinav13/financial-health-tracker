# ============================================================================
# scripts/shared/ipeds_sector_benchmarks.R
# ============================================================================
#
# Computes and applies sector-level benchmarks (medians, means, aggregates)
# for comparing individual institutions against their peer group. Benchmarks
# are computed from all institutions in the same sector (Public, Private
# not-for-profit, Private for-profit) for each year. Source AFTER utils.R.
#
# Benchmark types: medians (middle value), means (average), counts (participation).
# Institutions are compared to sector median to determine above/below/same.
#
# ============================================================================

# ---------------------------------------------------------------------------
# Tuition Dependence Benchmarks
# ---------------------------------------------------------------------------

# Computes median and mean tuition dependence (% of revenue from net tuition)
# for each sector and year, plus the count of institutions with data.
compute_sector_tuition_benchmarks <- function(df) {
  df %>%
    group_by(control_label, year) %>%
    summarise(
      sector_median_tuition_dependence_pct = if (all(is.na(tuition_dependence_pct))) NA_real_ else stats::median(tuition_dependence_pct, na.rm = TRUE),
      sector_mean_tuition_dependence_pct = if (all(is.na(tuition_dependence_pct))) NA_real_ else mean(tuition_dependence_pct, na.rm = TRUE),
      sector_tuition_dependence_n = sum(!is.na(tuition_dependence_pct)),
      .groups = "drop"
    )
}

# ---------------------------------------------------------------------------
# Sector Enrollment Benchmarks
# ---------------------------------------------------------------------------

# Computes total sector enrollment (national aggregate) and 5-year enrollment
# trend for each sector, showing how the overall sector is growing or shrinking.
compute_sector_enrollment_benchmarks <- function(df) {
  df %>%
    group_by(control_label, year) %>%
    summarise(
      sector_enrollment_total_national = sum(enrollment_headcount_total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(control_label) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
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

# Computes median research spending per FTE for institutions in each sector
# that report positive research expense data, plus the percentage of reporters.
compute_sector_research_benchmarks <- function(df) {
  df %>%
    group_by(control_label, year) %>%
    mutate(
      sector_research_spending_n = sum(!is.na(research_expense_per_fte)),
      sector_research_spending_positive_n = sum(!is.na(research_expense_per_fte) & research_expense_per_fte > 0),
      sector_research_spending_reporting_share_pct = ifelse(
        sector_research_spending_n > 0,
        round(100 * sector_research_spending_positive_n / sector_research_spending_n, 1),
        NA_real_
      ),
      sector_median_research_expense_per_fte_positive = ifelse(
        sector_research_spending_positive_n > 0,
        stats::median(research_expense_per_fte[!is.na(research_expense_per_fte) & research_expense_per_fte > 0]),
        NA_real_
      )
    ) %>%
    ungroup() %>%
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

# Computes median student-to-instructional-staff ratios for each sector.
# Lower ratios indicate more instructor time available per student.
compute_sector_staffing_benchmarks <- function(df) {
  df %>%
    group_by(control_label, year) %>%
    summarise(
      sector_median_students_per_instructional_staff_fte = if (
        all(is.na(students_per_instructional_staff_fte) | students_per_instructional_staff_fte <= 0)
      ) {
        NA_real_
      } else {
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

# Master function that computes all sector benchmarks and joins them to
# individual institution data, enabling peer comparisons. Also generates
# narrative sentences describing how an institution compares to its sector.
apply_sector_benchmarks <- function(df) {
  sector_tuition_dependence_benchmarks <- compute_sector_tuition_benchmarks(df)
  sector_enrollment_benchmarks <- compute_sector_enrollment_benchmarks(df)
  sector_research_benchmarks <- compute_sector_research_benchmarks(df)
  sector_staffing_benchmarks <- compute_sector_staffing_benchmarks(df)

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
    left_join(sector_tuition_dependence_benchmarks, by = c("control_label", "year")) %>%
    left_join(sector_enrollment_benchmarks, by = c("control_label", "year")) %>%
    left_join(sector_research_benchmarks, by = c("unitid", "year")) %>%
    left_join(sector_staffing_benchmarks, by = c("control_label", "year")) %>%
    mutate(
      tuition_dependence_vs_sector_median_pct_points = tuition_dependence_pct - sector_median_tuition_dependence_pct,

      tuition_dependence_relative_to_sector_median = case_when(
        is.na(tuition_dependence_pct) | is.na(sector_median_tuition_dependence_pct) ~ NA_character_,
        abs(tuition_dependence_pct - sector_median_tuition_dependence_pct) < 0.05 ~ "About the same as sector median",
        tuition_dependence_pct > sector_median_tuition_dependence_pct ~ "Above sector median",
        tuition_dependence_pct < sector_median_tuition_dependence_pct ~ "Below sector median",
        TRUE ~ NA_character_
      ),

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
