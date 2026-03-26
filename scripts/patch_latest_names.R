library(readr)
library(dplyr)

normalize_names <- function(df) {
  df %>%
    mutate(
      unitid = as.character(unitid),
      year = suppressWarnings(as.integer(year))
    ) %>%
    group_by(unitid) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      institution_name_latest = {
        idx <- which(!is.na(institution_name) & institution_name != "")
        if (length(idx) == 0) rep(NA_character_, n()) else rep(institution_name[idx[length(idx)]], n())
      },
      city_latest = {
        idx <- which(!is.na(city) & city != "")
        if (length(idx) == 0) rep(NA_character_, n()) else rep(city[idx[length(idx)]], n())
      },
      state_latest = {
        idx <- which(!is.na(state) & state != "")
        if (length(idx) == 0) rep(NA_character_, n()) else rep(state[idx[length(idx)]], n())
      }
    ) %>%
    ungroup() %>%
    mutate(
      institution_name = coalesce(institution_name_latest, institution_name),
      city = coalesce(city_latest, city),
      state = coalesce(state_latest, state),
      institution_unique_name = ifelse(
        is.na(institution_name),
        institution_unique_name,
        paste(institution_name, city, state, sep = " | ")
      )
    ) %>%
    select(-institution_name_latest, -city_latest, -state_latest)
}

looker_path <- "./looker_studio/ipeds_financial_health_looker_ready_2014_2024.csv"
report_path <- "./reporting/ipeds_financial_health_reporting_2014_2024.csv"

looker <- read_csv(looker_path, show_col_types = FALSE, guess_max = 100000)
report <- read_csv(report_path, show_col_types = FALSE, guess_max = 100000)

looker <- normalize_names(looker)
report <- normalize_names(report)

write_csv(looker, looker_path, na = "")
write_csv(report, report_path, na = "")

cat("patched latest display names in looker and reporting outputs\n")
