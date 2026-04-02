library(readr)
library(dplyr)

df <- read_csv(
  "./ipeds/ipeds_financial_health_dataset_2014_2024.csv",
  show_col_types = FALSE,
  guess_max = 100000
)

latest <- df %>%
  filter(
    year == 2024,
    !is.na(loan_pct_undergrad_federal_latest),
    !is.na(loan_count_undergrad_federal_latest),
    loan_pct_undergrad_federal_latest > 0
  ) %>%
  mutate(
    denom_est = loan_count_undergrad_federal_latest / (loan_pct_undergrad_federal_latest / 100)
  )

weighted_rate <- sum(latest$loan_count_undergrad_federal_latest, na.rm = TRUE) /
  sum(latest$denom_est, na.rm = TRUE) * 100

cat("institutions:", nrow(latest), "\n")
cat("weighted_rate_pct:", round(weighted_rate, 1), "\n")
