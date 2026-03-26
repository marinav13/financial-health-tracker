main <- function(cli_args = NULL) {
args <- if (is.null(cli_args)) commandArgs(trailingOnly = TRUE) else cli_args

get_arg_value <- function(flag, default) {
  idx <- match(flag, args)
  if (!is.na(idx) && idx < length(args)) {
    return(args[[idx + 1L]])
  }
  default
}

input_csv <- get_arg_value("--input", "./reporting/ipeds_financial_health_reporting_2014_2024.csv")
output_dir <- get_arg_value("--output-dir", ".")

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
  library(readr)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

root <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
input_path <- normalizePath(input_csv, winslash = "/", mustWork = TRUE)
data_dir <- file.path(root, "data")
schools_dir <- file.path(data_dir, "schools")
downloads_dir <- file.path(data_dir, "downloads")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(schools_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(downloads_dir, recursive = TRUE, showWarnings = FALSE)

to_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NULL")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", "", x, fixed = TRUE)))
}

null_if_empty <- function(x) {
  x <- as.character(x %||% "")
  x <- trimws(x)
  ifelse(x == "", NA_character_, x)
}

scale_ratio_to_pct <- function(x) {
  ifelse(is.na(x), NA_real_, x * 100)
}

write_json_file <- function(x, path) {
  jsonlite::write_json(x, path = path, pretty = TRUE, auto_unbox = TRUE, na = "null")
}

build_series <- function(df, value_col, scale = 1) {
  keep <- !is.na(df[[value_col]])
  if (!any(keep)) return(list())
  rows <- df[keep, c("year", value_col), drop = FALSE]
  lapply(seq_len(nrow(rows)), function(i) {
    list(
      year = as.integer(rows$year[[i]]),
      value = unname(rows[[value_col]][[i]]) * scale
    )
  })
}

build_school_file <- function(df) {
  latest <- df %>% filter(year == max(year, na.rm = TRUE)) %>% slice(1)

  list(
    unitid = as.character(latest$unitid[[1]]),
    generated_at = as.character(Sys.Date()),
    profile = list(
      institution_name = latest$institution_name[[1]],
      institution_unique_name = latest$institution_unique_name[[1]],
      state = latest$state[[1]],
      city = latest$city[[1]],
      control_label = latest$control_label[[1]],
      sector = latest$sector[[1]],
      category = latest$category[[1]],
      urbanization = latest$urbanization[[1]],
      religious_affiliation = latest$religious_affiliation[[1]],
      all_programs_distance_education = latest$all_programs_distance_education[[1]]
    ),
    summary = list(
      enrollment_pct_change_5yr = latest$enrollment_pct_change_5yr[[1]],
      revenue_pct_change_5yr = latest$revenue_pct_change_5yr[[1]],
      net_tuition_per_fte_change_5yr = latest$net_tuition_per_fte_change_5yr[[1]],
      staff_total_headcount_pct_change_5yr = latest$staff_total_headcount_pct_change_5yr[[1]],
      staff_instructional_headcount_pct_change_5yr = latest$staff_instructional_headcount_pct_change_5yr[[1]],
      ended_year_at_loss = latest$ended_year_at_loss[[1]],
      losses_last_3_of_5 = latest$losses_last_3_of_5[[1]],
      loss_years_last_10 = latest$loss_years_last_10[[1]],
      tuition_dependence_pct = latest$tuition_dependence_pct[[1]],
      sector_median_tuition_dependence_pct = latest$sector_median_tuition_dependence_pct[[1]],
      tuition_dependence_vs_sector_median_sentence = null_if_empty(latest$tuition_dependence_vs_sector_median_sentence[[1]]),
      pct_international_all = scale_ratio_to_pct(latest$pct_international_all[[1]]),
      international_students_sentence = null_if_empty(latest$international_students_sentence[[1]]),
      federal_loan_pct_most_recent = latest$federal_loan_pct_most_recent[[1]],
      federal_grants_contracts_pell_adjusted_pct_core_revenue = scale_ratio_to_pct(latest$federal_grants_contracts_pell_adjusted_pct_core_revenue[[1]]),
      state_funding_pct_core_revenue = scale_ratio_to_pct(latest$state_funding_pct_core_revenue[[1]]),
      federal_grants_contracts_pell_adjusted_pct_change_5yr = latest$federal_grants_contracts_pell_adjusted_pct_change_5yr[[1]],
      state_funding_pct_change_5yr = latest$state_funding_pct_change_5yr[[1]],
      endowment_pct_change_5yr = latest$endowment_pct_change_5yr[[1]]
    ),
    series = list(
      revenue_total_adjusted = build_series(df, "revenue_total_adjusted"),
      expenses_total_adjusted = build_series(df, "expenses_total_adjusted"),
      net_tuition_per_fte_adjusted = build_series(df, "net_tuition_per_fte_adjusted"),
      enrollment_headcount_total = build_series(df, "enrollment_headcount_total"),
      enrollment_nonresident_total = build_series(df, "enrollment_nonresident_total"),
      staff_headcount_total = build_series(df, "staff_headcount_total"),
      staff_headcount_instructional = build_series(df, "staff_headcount_instructional"),
      endowment_value_adjusted = build_series(df, "endowment_value_adjusted"),
      federal_grants_contracts_pell_adjusted_adjusted = build_series(df, "federal_grants_contracts_pell_adjusted_adjusted"),
      state_funding_adjusted = build_series(df, "state_funding_adjusted")
    )
  )
}

df <- readr::read_csv(input_path, show_col_types = FALSE)

numeric_cols <- c(
  "year","enrollment_pct_change_5yr","revenue_pct_change_5yr","net_tuition_per_fte_change_5yr",
  "staff_total_headcount_pct_change_5yr","staff_instructional_headcount_pct_change_5yr","loss_years_last_10",
  "tuition_dependence_pct","sector_median_tuition_dependence_pct","pct_international_all",
  "federal_loan_pct_most_recent","federal_grants_contracts_pell_adjusted_pct_core_revenue",
  "state_funding_pct_core_revenue","federal_grants_contracts_pell_adjusted_pct_change_5yr",
  "state_funding_pct_change_5yr","endowment_pct_change_5yr","revenue_total_adjusted",
  "expenses_total_adjusted","net_tuition_per_fte_adjusted","enrollment_headcount_total",
  "enrollment_nonresident_total","staff_headcount_total","staff_headcount_instructional",
  "endowment_value_adjusted","federal_grants_contracts_pell_adjusted_adjusted","state_funding_adjusted"
)

for (nm in intersect(numeric_cols, names(df))) {
  df[[nm]] <- to_num(df[[nm]])
}

df <- df %>% arrange(unitid, year)
latest_2024 <- df %>% filter(year == 2024)

schools_index <- latest_2024 %>%
  transmute(
    unitid = as.character(unitid),
    institution_name = institution_name,
    institution_unique_name = institution_unique_name,
    state = state,
    city = city,
    control_label = control_label,
    category = category,
    urbanization = urbanization,
    religious_affiliation = religious_affiliation
  ) %>%
  arrange(institution_name)

rankings <- list(
  generated_at = as.character(Sys.Date()),
  lists = list(
    enrollment_decline_5yr = latest_2024 %>%
      filter(!is.na(enrollment_pct_change_5yr)) %>%
      arrange(enrollment_pct_change_5yr) %>%
      slice_head(n = 25) %>%
      transmute(unitid = as.character(unitid), institution_name, value = round(enrollment_pct_change_5yr, 1)),
    staff_cuts_5yr = latest_2024 %>%
      filter(!is.na(staff_total_headcount_pct_change_5yr)) %>%
      arrange(staff_total_headcount_pct_change_5yr) %>%
      slice_head(n = 25) %>%
      transmute(unitid = as.character(unitid), institution_name, value = round(staff_total_headcount_pct_change_5yr, 1)),
    federal_dependence = latest_2024 %>%
      filter(!is.na(federal_grants_contracts_pell_adjusted_pct_core_revenue)) %>%
      arrange(desc(federal_grants_contracts_pell_adjusted_pct_core_revenue)) %>%
      slice_head(n = 25) %>%
      transmute(unitid = as.character(unitid), institution_name, value = round(federal_grants_contracts_pell_adjusted_pct_core_revenue * 100, 1)),
    private_closure_risk = latest_2024 %>%
      filter(control_label == "Private not-for-profit", !is.na(loss_years_last_10)) %>%
      arrange(desc(loss_years_last_10), revenue_pct_change_5yr, enrollment_pct_change_5yr) %>%
      slice_head(n = 25) %>%
      transmute(unitid = as.character(unitid), institution_name, value = as.integer(loss_years_last_10))
  )
)

metadata <- list(
  generated_at = as.character(Sys.Date()),
  title = "College Financial Health Tracker",
  dataset = "IPEDS Financial Health Reporting Dataset",
  methodology_note = "This website prototype uses the filtered public-facing reporting dataset from the project repo.",
  files = list(
    schools_index = "data/schools_index.json",
    rankings = "data/rankings.json",
    schools = "data/schools/{unitid}.json",
    download = "data/downloads/full_dataset.csv"
  )
)

write_json_file(schools_index, file.path(data_dir, "schools_index.json"))
write_json_file(rankings, file.path(data_dir, "rankings.json"))
write_json_file(metadata, file.path(data_dir, "metadata.json"))
readr::write_csv(latest_2024, file.path(downloads_dir, "full_dataset.csv"), na = "")

by_school <- split(df, df$unitid)
for (unitid in names(by_school)) {
  school_json <- build_school_file(by_school[[unitid]])
  write_json_file(school_json, file.path(schools_dir, paste0(unitid, ".json")))
}

cat(sprintf("Saved schools index to %s\n", file.path(data_dir, "schools_index.json")))
cat(sprintf("Saved rankings to %s\n", file.path(data_dir, "rankings.json")))
cat(sprintf("Saved metadata to %s\n", file.path(data_dir, "metadata.json")))
cat(sprintf("Saved school files to %s\n", schools_dir))
cat(sprintf("Saved download CSV to %s\n", file.path(downloads_dir, "full_dataset.csv")))

invisible(TRUE)
}

if (sys.nframe() == 0) {
  main()
}
