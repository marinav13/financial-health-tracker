library(readr)
library(dplyr)

safe_pct_change <- function(new_value, old_value) {
  ifelse(
    is.na(new_value) | is.na(old_value) | old_value == 0,
    NA_real_,
    ((new_value - old_value) / abs(old_value)) * 100
  )
}

add_benchmarks <- function(df) {
  benchmarks <- df %>%
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

  df %>%
    left_join(benchmarks, by = c("control_label", "year")) %>%
    mutate(
      sector_enrollment_change_sentence = ifelse(
        is.na(enrollment_pct_change_5yr) | is.na(sector_enrollment_pct_change_5yr_national),
        NA_character_,
        paste0(
          "Enrollment changed ",
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

paths <- c(
  "./looker_studio/ipeds_financial_health_looker_ready_2014_2024.csv",
  "./looker_studio/ipeds_financial_health_looker_ready_2014_2024_rebuilt2.csv",
  "./reporting/ipeds_financial_health_reporting_2014_2024_rebuilt2.csv"
)

for (path in paths) {
  if (!file.exists(path)) next
  df <- read_csv(path, show_col_types = FALSE, guess_max = 100000)
  if (!all(c("control_label", "year", "enrollment_headcount_total", "enrollment_pct_change_5yr") %in% names(df))) next
  df <- add_benchmarks(df)
  write_csv(df, path, na = "")
  cat("updated", path, "\n")
}
