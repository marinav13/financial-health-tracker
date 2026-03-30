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

cuts_path <- file.path(root, "college_cuts", "college_cuts_financial_tracker_cut_level_joined.csv")
accreditation_summary_path <- file.path(root, "accreditation", "accreditation_tracker_institution_summary.csv")
accreditation_actions_path <- file.path(root, "accreditation", "accreditation_tracker_actions_joined.csv")
accreditation_coverage_path <- file.path(root, "accreditation", "accreditation_tracker_source_coverage.csv")

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

or_null <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA)
  value <- x[[1]]
  if (is.character(value)) {
    value <- trimws(value)
    if (identical(value, "")) return(NA)
  }
  value
}

or_null_date <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA)
  value <- x[[1]]
  if (inherits(value, "Date")) {
    if (is.na(value)) return(NA)
    return(as.character(value))
  }
  if (is.character(value)) {
    value <- trimws(value)
    if (identical(value, "")) return(NA)
  }
  value
}

make_export_id <- function(prefix, unitid, institution_name, state) {
  raw_unitid <- trimws(as.character(unitid %||% ""))
  if (!identical(raw_unitid, "")) return(raw_unitid)
  base <- paste(institution_name %||% "", state %||% "", sep = " | ")
  normalized <- tolower(base)
  normalized <- gsub("[^a-z0-9]+", "-", normalized)
  normalized <- gsub("^-+|-+$", "", normalized)
  paste0(prefix, "-", normalized)
}

normalize_control_label <- function(x) {
  value <- trimws(as.character(x %||% ""))
  if (!nzchar(value)) return(NA_character_)
  dplyr::case_when(
    grepl("^public$", value, ignore.case = TRUE) ~ "Public",
    grepl("private (?:non-profit|not-for-profit)", value, ignore.case = TRUE) ~ "Private not-for-profit",
    grepl("private for-profit", value, ignore.case = TRUE) ~ "Private for-profit",
    TRUE ~ value
  )
}

is_primary_bachelors_category <- function(x) {
  value <- as.character(x %||% "")
  grepl("primarily baccalaureate or above", value, ignore.case = TRUE) &
    !grepl("not primarily baccalaureate or above", value, ignore.case = TRUE)
}

derive_positions_affected <- function(faculty_affected, notes, source_title, program_name, cut_type) {
  explicit <- to_num(faculty_affected)
  if (!is.na(explicit) && explicit > 0) return(as.integer(explicit))

  text <- paste(notes %||% "", source_title %||% "", program_name %||% "", sep = " ")
  text <- gsub(",", "", text, fixed = TRUE)
  text <- trimws(text)
  if (!nzchar(text)) return(NA_integer_)

  layoff_signal <- grepl("layoff|laid off|positions|position|employees|employee|staff members|faculty", text, ignore.case = TRUE) ||
    grepl("staff_layoff|faculty_layoff", as.character(cut_type %||% ""), ignore.case = TRUE)
  if (!layoff_signal) return(NA_integer_)

  patterns <- c(
    "cutting\\s+([0-9]{1,4})\\s+positions",
    "lays? off\\s+([0-9]{1,4})",
    "laid off\\s+([0-9]{1,4})",
    "([0-9]{1,4})\\s+positions\\s+affected",
    "([0-9]{1,4})\\s+(?:employees|employee|staff members|staff|faculty(?: members)?)"
  )
  for (pattern in patterns) {
    matched <- regexec(pattern, text, ignore.case = TRUE)
    value <- regmatches(text, matched)[[1]]
    if (length(value) >= 2) {
      parsed <- suppressWarnings(as.integer(value[2]))
      if (!is.na(parsed) && parsed > 0) return(parsed)
    }
  }

  NA_integer_
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

build_cuts_export <- function() {
  if (!file.exists(cuts_path)) return(NULL)

  cuts <- readr::read_csv(cuts_path, show_col_types = FALSE) %>%
    mutate(
      matched_unitid = as.character(matched_unitid),
      announcement_date = as.character(announcement_date),
      announcement_year = as.character(announcement_year),
      in_financial_tracker = as.character(in_financial_tracker),
      export_unitid = vapply(
        seq_len(n()),
        function(i) make_export_id(
          "cut",
          matched_unitid[[i]],
          tracker_institution_name[[i]] %||% institution_name_collegecuts[[i]],
          institution_state_full[[i]]
        ),
        character(1)
      ),
      institution_name_display = dplyr::coalesce(tracker_institution_name, institution_name_collegecuts),
      city_display = institution_city,
      state_display = institution_state_full,
      control_label_display = vapply(
        dplyr::coalesce(tracker_control_label, institution_control),
        normalize_control_label,
        character(1)
      ),
      category_display = tracker_category,
      has_financial_profile = !is.na(matched_unitid) & matched_unitid != "" & in_financial_tracker == "TRUE",
      is_primary_tracker = has_financial_profile & is_primary_bachelors_category(category_display),
      positions_affected = vapply(
        seq_len(n()),
        function(i) derive_positions_affected(
          faculty_affected[[i]],
          notes[[i]],
          source_title[[i]],
          program_name[[i]],
          cut_type[[i]]
        ),
        integer(1)
      )
    )

  if (nrow(cuts) == 0) return(NULL)

  recent <- cuts %>%
    arrange(desc(announcement_date), desc(announcement_year)) %>%
    slice_head(n = 25) %>%
    transmute(
      unitid = export_unitid,
      financial_unitid = ifelse(has_financial_profile, matched_unitid, NA_character_),
      institution_name = institution_name_display,
      city = city_display,
      state = state_display,
      control_label = control_label_display,
      category = category_display,
      is_primary_tracker = is_primary_tracker,
      has_financial_profile = has_financial_profile,
      announcement_date = announcement_date,
      announcement_year = announcement_year,
      program_name = program_name,
      cut_type = cut_type,
      status = status,
      effective_term = effective_term,
      positions_affected = positions_affected,
      notes = notes,
      source_url = source_url,
      source_title = source_title,
      source_publication = source_publication
    )

  schools <- lapply(split(cuts, cuts$export_unitid), function(df) {
    df <- df %>% arrange(desc(announcement_date), desc(announcement_year))
    latest <- df %>% slice(1)
    list(
      unitid = as.character(latest$export_unitid[[1]]),
      financial_unitid = if (isTRUE(latest$has_financial_profile[[1]])) latest$matched_unitid[[1]] else NA_character_,
      has_financial_profile = isTRUE(latest$has_financial_profile[[1]]),
      is_primary_tracker = isTRUE(latest$is_primary_tracker[[1]]),
      institution_name = latest$institution_name_display[[1]],
      city = latest$city_display[[1]],
      state = latest$state_display[[1]],
      control_label = latest$control_label_display[[1]],
      category = latest$category_display[[1]],
      latest_cut_date = or_null(latest$announcement_date),
      latest_cut_label = or_null(latest$program_name),
      cut_count = nrow(df),
      cuts = lapply(seq_len(nrow(df)), function(i) {
        list(
          announcement_date = or_null(df$announcement_date[i]),
          announcement_year = or_null(df$announcement_year[i]),
          program_name = or_null(df$program_name[i]),
          cut_type = or_null(df$cut_type[i]),
          status = or_null(df$status[i]),
          effective_term = or_null(df$effective_term[i]),
          students_affected = or_null(df$students_affected[i]),
          faculty_affected = or_null(df$faculty_affected[i]),
          positions_affected = or_null(df$positions_affected[i]),
          notes = or_null(df$notes[i]),
          source_url = or_null(df$source_url[i]),
          source_title = or_null(df$source_title[i]),
          source_publication = or_null(df$source_publication[i]),
          source_published_at = or_null(df$source_published_at[i])
        )
      })
    )
  })

  list(
    generated_at = as.character(Sys.Date()),
    recent = recent,
    schools = schools
  )
}

build_accreditation_export <- function() {
  if (!file.exists(accreditation_summary_path) || !file.exists(accreditation_actions_path)) return(NULL)

  normalize_accreditor_name <- function(x) {
    dplyr::case_when(
      is.na(x) ~ x,
      x == "WSCUC" ~ "WASC Senior College and University Commission",
      TRUE ~ x
    )
  }

  summary_df <- readr::read_csv(accreditation_summary_path, show_col_types = FALSE) %>%
    mutate(
      unitid = as.character(unitid),
      accreditors = normalize_accreditor_name(accreditors),
      latest_action_date = na_if(as.character(latest_action_date), ""),
      latest_action_year = na_if(as.character(latest_action_year), "")
    )
  actions_df <- readr::read_csv(accreditation_actions_path, show_col_types = FALSE) %>%
    mutate(
      unitid = as.character(unitid),
      accreditor = normalize_accreditor_name(accreditor),
      action_date = na_if(as.character(action_date), ""),
      action_year = na_if(as.character(action_year), ""),
      source_page_modified = na_if(as.character(source_page_modified), ""),
      display_action = if ("display_action" %in% names(.)) as.logical(display_action) else TRUE
    )
  coverage_df <- if (file.exists(accreditation_coverage_path)) {
    readr::read_csv(accreditation_coverage_path, show_col_types = FALSE) %>%
      mutate(accreditor = normalize_accreditor_name(accreditor))
  } else {
    tibble::tibble()
  }

  make_accreditation_export_id <- function(unitid, institution_name, state, accreditor) {
    raw_unitid <- trimws(as.character(unitid %||% ""))
    if (!identical(raw_unitid, "")) return(raw_unitid)
    base <- paste(institution_name %||% "", state %||% "", accreditor %||% "", sep = " | ")
    normalized <- tolower(base)
    normalized <- gsub("[^a-z0-9]+", "-", normalized)
    normalized <- gsub("^-+|-+$", "", normalized)
    paste0("accred-", normalized)
  }

  pick_first_present <- function(df, candidates) {
    present <- candidates[candidates %in% names(df)]
    if (!length(present)) return(rep(NA_character_, nrow(df)))
    values <- lapply(present, function(col) as.character(df[[col]]))
    Reduce(function(x, y) dplyr::coalesce(x, y), values)
  }

  summary_df <- summary_df %>%
    mutate(
      export_institution_name = pick_first_present(cur_data(), c("institution_name", "tracker_name", "institution_name_raw")),
      export_state = pick_first_present(cur_data(), c("state", "tracker_state", "institution_state_raw")),
      export_city = pick_first_present(cur_data(), c("city", "tracker_city")),
      export_control_label = pick_first_present(cur_data(), c("control_label", "tracker_control")),
      export_category = pick_first_present(cur_data(), c("category", "tracker_category")),
      export_unitid = vapply(
        seq_len(n()),
        function(i) make_accreditation_export_id(unitid[[i]], export_institution_name[[i]], export_state[[i]], accreditors[[i]]),
        character(1)
      )
    )
  actions_df <- actions_df %>%
    mutate(
      export_institution_name = pick_first_present(cur_data(), c("institution_name", "tracker_name", "institution_name_raw")),
      export_state = pick_first_present(cur_data(), c("state", "tracker_state", "institution_state_raw")),
      export_city = pick_first_present(cur_data(), c("city", "tracker_city")),
      export_control_label = pick_first_present(cur_data(), c("control_label", "tracker_control")),
      export_category = pick_first_present(cur_data(), c("category", "tracker_category")),
      export_unitid = vapply(
        seq_len(n()),
        function(i) make_accreditation_export_id(unitid[[i]], export_institution_name[[i]], export_state[[i]], accreditor[[i]]),
        character(1)
      ),
      has_financial_profile = !is.na(unitid) & unitid != "",
      is_primary_tracker = has_financial_profile & is_primary_bachelors_category(export_category)
    ) %>%
    filter(!is.na(export_unitid), export_unitid != "")

  actions_df <- actions_df %>%
    filter(display_action %in% TRUE)

  covered_accreditors <- sort(unique(c(summary_df$accreditors, actions_df$accreditor)))
  covered_accreditors <- covered_accreditors[!is.na(covered_accreditors) & covered_accreditors != ""]

  not_covered <- list(
    list(name = "ACCJC", url = "https://accjc.org/", note = "Not yet integrated into the tracker workflow.")
  )

  schools <- lapply(split(actions_df, actions_df$export_unitid), function(df) {
    df <- df %>% arrange(desc(action_date), desc(action_year))
    summary_row <- summary_df %>% filter(export_unitid == df$export_unitid[[1]]) %>% slice(1)
    latest <- if (nrow(summary_row) > 0) summary_row else df %>% slice(1)

    sources <- df %>%
      distinct(accreditor, source_title, source_url, source_page_url, .keep_all = FALSE)

    list(
      unitid = as.character(df$export_unitid[[1]]),
      financial_unitid = if (isTRUE(df$has_financial_profile[[1]])) as.character(df$unitid[[1]]) else NA_character_,
      has_financial_profile = isTRUE(df$has_financial_profile[[1]]),
      is_primary_tracker = isTRUE(df$is_primary_tracker[[1]]),
      institution_name = or_null(latest$export_institution_name),
      city = or_null(latest$export_city),
      state = or_null(latest$export_state),
      control_label = or_null(latest$export_control_label),
      category = or_null(latest$export_category),
      latest_status = list(
        accreditors = or_null(latest$accreditors),
        action_labels = or_null(latest$action_labels),
        active_actions = or_null(latest$active_actions),
        has_active_warning = or_null(latest$has_active_warning),
        has_active_warning_or_notice = or_null(latest$has_active_warning_or_notice),
        has_active_adverse_action = or_null(latest$has_active_adverse_action),
        latest_action_date = or_null_date(latest$latest_action_date %||% latest$action_date),
        latest_action_year = or_null(latest$latest_action_year %||% latest$action_year),
        action_count = or_null(latest$action_count)
      ),
      actions = lapply(seq_len(nrow(df)), function(i) {
        list(
          accreditor = or_null(df$accreditor[i]),
          action_type = or_null(df$action_type[i]),
          action_label = or_null(df$action_label_raw[i]),
          action_status = or_null(df$action_status[i]),
          action_date = or_null_date(df$action_date[i]),
          action_year = or_null(df$action_year[i]),
          notes = or_null(df$notes[i]),
          source_url = or_null(df$source_url[i]),
          source_title = or_null(df$source_title[i]),
          source_page_url = or_null(df$source_page_url[i]),
          source_page_modified = or_null(df$source_page_modified[i]),
          has_financial_profile = isTRUE(df$has_financial_profile[i]),
          is_primary_tracker = isTRUE(df$is_primary_tracker[i])
        )
      }),
      sources = lapply(seq_len(nrow(sources)), function(i) {
        list(
          accreditor = or_null(sources$accreditor[i]),
          source_title = or_null(sources$source_title[i]),
          source_url = or_null(sources$source_url[i]),
          source_page_url = or_null(sources$source_page_url[i])
        )
      })
    )
  })

  list(
    generated_at = as.character(Sys.Date()),
    covered_accreditors = covered_accreditors,
    source_coverage = coverage_df,
    not_covered = not_covered,
    schools = schools
  )
}

build_school_file <- function(df) {
  latest <- df %>% filter(year == max(year, na.rm = TRUE)) %>% slice(1)
  sector_key <- latest$sector[[1]]

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
      enrollment_decline_last_3_of_5 = latest$enrollment_decline_last_3_of_5[[1]],
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
      pct_international_undergraduate = scale_ratio_to_pct(latest$pct_international_undergraduate[[1]]),
      pct_international_graduate = scale_ratio_to_pct(latest$pct_international_graduate[[1]]),
      international_student_count_change_5yr = latest$international_student_count_change_5yr[[1]],
      international_enrollment_pct_change_5yr = latest$international_enrollment_pct_change_5yr[[1]],
      international_students_sentence = null_if_empty(latest$international_students_sentence[[1]]),
      federal_loan_pct_most_recent = latest$federal_loan_pct_most_recent[[1]],
      sector_avg_federal_loan_pct_most_recent = unname(sector_loan_benchmarks[[sector_key]] %||% NA_real_),
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
      enrollment_nonresident_undergrad = build_series(df, "enrollment_nonresident_undergrad"),
      enrollment_nonresident_graduate = build_series(df, "enrollment_nonresident_graduate"),
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
  "pct_international_undergraduate","pct_international_graduate","international_student_count_change_5yr",
  "international_enrollment_pct_change_5yr",
  "federal_loan_pct_most_recent","federal_grants_contracts_pell_adjusted_pct_core_revenue",
  "state_funding_pct_core_revenue","federal_grants_contracts_pell_adjusted_pct_change_5yr",
  "state_funding_pct_change_5yr","endowment_pct_change_5yr","revenue_total_adjusted",
  "expenses_total_adjusted","net_tuition_per_fte_adjusted","enrollment_headcount_total",
  "enrollment_nonresident_total","enrollment_nonresident_undergrad","enrollment_nonresident_graduate",
  "staff_headcount_total","staff_headcount_instructional",
  "endowment_value_adjusted","federal_grants_contracts_pell_adjusted_adjusted","state_funding_adjusted"
)

for (nm in intersect(numeric_cols, names(df))) {
  df[[nm]] <- to_num(df[[nm]])
}

df <- df %>% arrange(unitid, year)
latest_2024 <- df %>% filter(year == 2024)
sector_loan_benchmarks <- latest_2024 %>%
  group_by(sector) %>%
  summarise(value = mean(federal_loan_pct_most_recent, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(sector), !is.na(value)) %>%
  { stats::setNames(.$value, .$sector) }

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

metadata <- list(
  generated_at = as.character(Sys.Date()),
  title = "College Financial Health Tracker",
  dataset = "IPEDS Financial Health Reporting Dataset",
  methodology_note = "This website prototype uses the filtered public-facing reporting dataset from the project repo.",
  files = list(
    schools_index = "data/schools_index.json",
    college_cuts_index = "data/college_cuts_index.json",
    accreditation_index = "data/accreditation_index.json",
    schools = "data/schools/{unitid}.json",
    download = "data/downloads/full_dataset.csv",
    college_cuts = "data/college_cuts.json",
    accreditation = "data/accreditation.json"
  )
)

write_json_file(schools_index, file.path(data_dir, "schools_index.json"))
write_json_file(metadata, file.path(data_dir, "metadata.json"))
readr::write_csv(latest_2024, file.path(downloads_dir, "full_dataset.csv"), na = "")

cuts_export <- build_cuts_export()
if (!is.null(cuts_export)) {
  write_json_file(cuts_export, file.path(data_dir, "college_cuts.json"))
  cuts_index <- lapply(cuts_export$schools, function(school) {
    list(
      unitid = school$unitid,
      financial_unitid = school$financial_unitid,
      has_financial_profile = school$has_financial_profile,
      is_primary_tracker = school$is_primary_tracker,
      institution_name = school$institution_name,
      institution_unique_name = paste(na.omit(c(school$institution_name, school$city, school$state)), collapse = " | "),
      state = school$state,
      city = school$city,
      control_label = school$control_label,
      category = school$category,
      latest_cut_date = school$latest_cut_date,
      latest_cut_label = school$latest_cut_label,
      cut_count = school$cut_count
    )
  })
  write_json_file(cuts_index, file.path(data_dir, "college_cuts_index.json"))
}

accreditation_export <- build_accreditation_export()
if (!is.null(accreditation_export)) {
  write_json_file(accreditation_export, file.path(data_dir, "accreditation.json"))
  accreditation_index <- lapply(accreditation_export$schools, function(school) {
    latest_action_label <- if (!is.null(school$actions) && length(school$actions) > 0) {
      school$actions[[1]]$action_label
    } else {
      school$latest_status$action_labels
    }
    list(
      unitid = school$unitid,
      financial_unitid = school$financial_unitid,
      has_financial_profile = school$has_financial_profile,
      is_primary_tracker = school$is_primary_tracker,
      institution_name = school$institution_name,
      institution_unique_name = paste(na.omit(c(school$institution_name, school$city, school$state)), collapse = " | "),
      state = school$state,
      city = school$city,
      control_label = school$control_label,
      category = school$category,
      latest_action_date = school$latest_status$latest_action_date,
      latest_action_label = latest_action_label,
      action_count = school$latest_status$action_count
    )
  })
  write_json_file(accreditation_index, file.path(data_dir, "accreditation_index.json"))
}

by_school <- split(df, df$unitid)
for (unitid in names(by_school)) {
  school_json <- build_school_file(by_school[[unitid]])
  write_json_file(school_json, file.path(schools_dir, paste0(unitid, ".json")))
}

cat(sprintf("Saved schools index to %s\n", file.path(data_dir, "schools_index.json")))
cat(sprintf("Saved metadata to %s\n", file.path(data_dir, "metadata.json")))
cat(sprintf("Saved school files to %s\n", schools_dir))
cat(sprintf("Saved download CSV to %s\n", file.path(downloads_dir, "full_dataset.csv")))
if (!is.null(cuts_export)) cat(sprintf("Saved college cuts export to %s\n", file.path(data_dir, "college_cuts.json")))
if (!is.null(accreditation_export)) cat(sprintf("Saved accreditation export to %s\n", file.path(data_dir, "accreditation.json")))
if (!is.null(cuts_export)) cat(sprintf("Saved college cuts index to %s\n", file.path(data_dir, "college_cuts_index.json")))
if (!is.null(accreditation_export)) cat(sprintf("Saved accreditation index to %s\n", file.path(data_dir, "accreditation_index.json")))

invisible(TRUE)
}

if (sys.nframe() == 0) {
  main()
}
