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

dataset_path <- "./ipeds/ipeds_financial_health_dataset_2014_2024.csv"

dataset <- read_csv(dataset_path, show_col_types = FALSE, guess_max = 100000)
dataset <- normalize_names(dataset)

write_csv(dataset, dataset_path, na = "")

cat("patched latest display names in canonical IPEDS dataset\n")
