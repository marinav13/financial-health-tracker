main <- function(cli_args = NULL) {
args <- if (is.null(cli_args)) commandArgs(trailingOnly = TRUE) else cli_args

get_arg_value <- function(flag, default) {
  idx <- match(flag, args)
  if (!is.na(idx) && idx < length(args)) {
    return(args[[idx + 1L]])
  }
  default
}

raw_csv <- get_arg_value("--raw", "./raw_build/ipeds_financial_health_raw_2014_2024.csv")
catalog_csv <- get_arg_value("--catalog", "./raw_build/ipeds_financial_health_selected_file_catalog.csv")
output_csv <- get_arg_value("--output", "./looker_studio/ipeds_financial_health_looker_ready_2014_2024.csv")
expanded_output_csv <- get_arg_value("--expanded-output", "./reporting/ipeds_financial_health_reporting_2014_2024.csv")

user_lib <- Sys.getenv("R_LIBS_USER", unset = "")
if (!identical(user_lib, "")) {
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(unique(c(user_lib, .libPaths())))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(readxl)
  library(stringr)
  library(tidyr)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
raw_path <- normalizePath(raw_csv, winslash = "/", mustWork = TRUE)
catalog_path <- normalizePath(catalog_csv, winslash = "/", mustWork = TRUE)
output_path <- file.path(root, output_csv)
expanded_output_path <- file.path(root, expanded_output_csv)
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(expanded_output_path), recursive = TRUE, showWarnings = FALSE)
aux_root <- file.path(root, "looker_aux")
aux_data_root <- file.path(aux_root, "data")
aux_extract_root <- file.path(aux_root, "extracted")
dir.create(aux_root, recursive = TRUE, showWarnings = FALSE)
dir.create(aux_data_root, recursive = TRUE, showWarnings = FALSE)
dir.create(aux_extract_root, recursive = TRUE, showWarnings = FALSE)

to_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "NULL")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", "", x, fixed = TRUE)))
}

safe_pct_change <- function(new_value, old_value) {
  ifelse(
    is.na(new_value) | is.na(old_value) | old_value == 0,
    NA_real_,
    ((new_value - old_value) / abs(old_value)) * 100
  )
}

safe_divide <- function(numerator, denominator) {
  ifelse(
    is.na(numerator) | is.na(denominator) | denominator == 0,
    NA_real_,
    numerator / denominator
  )
}

cpi_u_annual_avg <- c(
  `2014` = 236.736,
  `2015` = 237.017,
  `2016` = 240.007,
  `2017` = 245.120,
  `2018` = 251.107,
  `2019` = 255.657,
  `2020` = 258.811,
  `2021` = 270.970,
  `2022` = 292.655,
  `2023` = 304.702,
  `2024` = 313.689
)

inflate_to_base_year <- function(value, year, base_year = 2024) {
  year_key <- as.character(year)
  base_key <- as.character(base_year)
  ifelse(
    is.na(value) | is.na(year) |
      !(year_key %in% names(cpi_u_annual_avg)) |
      !(base_key %in% names(cpi_u_annual_avg)),
    NA_real_,
    value * (unname(cpi_u_annual_avg[[base_key]]) / unname(cpi_u_annual_avg[[year_key]]))
  )
}

zero_if_null <- function(x) {
  ifelse(is.na(x), 0, x)
}

sum_if_any <- function(values) {
  if (all(is.na(values))) return(NA_real_)
  sum(values, na.rm = TRUE)
}

get_control_label <- function(control_code) {
  code <- trimws(as.character(control_code %||% ""))
  dplyr::case_when(
    code == "1" ~ "Public",
    code == "2" ~ "Private not-for-profit",
    code == "3" ~ "Private for-profit",
    TRUE ~ as.character(control_code)
  )
}

decode_yes_no_field <- function(x) {
  code <- trimws(as.character(x %||% ""))
  dplyr::case_when(
    code == "1" ~ "Yes",
    code == "2" ~ "No",
    code == "-1" ~ "Not reported",
    code == "-2" ~ "Not applicable",
    code == "" ~ NA_character_,
    TRUE ~ as.character(x)
  )
}

state_lookup <- c(
  AL = "Alabama", AK = "Alaska", AZ = "Arizona", AR = "Arkansas", CA = "California", CO = "Colorado",
  CT = "Connecticut", DE = "Delaware", DC = "District of Columbia", FL = "Florida", GA = "Georgia",
  HI = "Hawaii", ID = "Idaho", IL = "Illinois", IN = "Indiana", IA = "Iowa", KS = "Kansas",
  KY = "Kentucky", LA = "Louisiana", ME = "Maine", MD = "Maryland", MA = "Massachusetts",
  MI = "Michigan", MN = "Minnesota", MS = "Mississippi", MO = "Missouri", MT = "Montana",
  NE = "Nebraska", NV = "Nevada", NH = "New Hampshire", NJ = "New Jersey", NM = "New Mexico",
  NY = "New York", NC = "North Carolina", ND = "North Dakota", OH = "Ohio", OK = "Oklahoma",
  OR = "Oregon", PA = "Pennsylvania", RI = "Rhode Island", SC = "South Carolina", SD = "South Dakota",
  TN = "Tennessee", TX = "Texas", UT = "Utah", VT = "Vermont", VA = "Virginia", WA = "Washington",
  WV = "West Virginia", WI = "Wisconsin", WY = "Wyoming", PR = "Puerto Rico", GU = "Guam",
  VI = "U.S. Virgin Islands", AS = "American Samoa", MP = "Northern Mariana Islands",
  FM = "Federated States of Micronesia", MH = "Marshall Islands", PW = "Palau"
)

get_state_name <- function(state_abbr) {
  vals <- as.character(state_abbr)
  keys <- toupper(trimws(ifelse(is.na(vals), "", vals)))
  out <- vals
  matched <- keys %in% names(state_lookup)
  out[matched] <- unname(state_lookup[keys[matched]])
  out
}

excluded_state_codes <- c("PR", "GU", "VI", "AS", "MP", "FM", "MH", "PW")

download_if_missing <- function(url, out_file) {
  if (file.exists(out_file)) return(invisible(out_file))
  url <- gsub("&amp;", "&", as.character(url), fixed = TRUE)
  utils::download.file(url, destfile = out_file, mode = "wb", quiet = TRUE)
  out_file
}

expand_zip_if_missing <- function(zip_path, destination_path) {
  if (dir.exists(destination_path)) return(invisible(destination_path))
  dir.create(destination_path, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zip_path, exdir = destination_path)
  destination_path
}

find_first_file <- function(path, pattern) {
  hits <- list.files(path, pattern = pattern, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(hits) == 0) return(NA_character_)
  hits[[1]]
}

get_frequency_lookup <- function(dictionary_archive, table_name, var_name) {
  expanded <- file.path(aux_extract_root, paste0("dict_", table_name))
  expand_zip_if_missing(dictionary_archive, expanded)
  xlsx_path <- find_first_file(expanded, "\\.xlsx$")
  if (is.na(xlsx_path)) return(character())
  sheets <- tryCatch(readxl::excel_sheets(xlsx_path), error = function(e) character())
  freq_sheet <- sheets[tolower(sheets) == "frequencies"][1]
  if (length(freq_sheet) == 0 || is.na(freq_sheet)) return(character())
  rows <- suppressMessages(readxl::read_excel(xlsx_path, sheet = freq_sheet, col_names = FALSE, .name_repair = "minimal"))
  if (ncol(rows) == 0) return(character())
  bad_names <- is.na(names(rows)) | names(rows) == ""
  names(rows)[bad_names] <- paste0("X", which(bad_names))
  names(rows)[seq_len(min(5, ncol(rows)))] <- c("A", "B", "C", "D", "E")[seq_len(min(5, ncol(rows)))]
  if (!all(c("A", "D", "E") %in% names(rows))) return(character())
  rows <- rows %>%
    mutate(
      A = trimws(as.character(A)),
      D = trimws(as.character(D)),
      E = trimws(as.character(E))
    ) %>%
    filter(A == var_name, !is.na(D), D != "", !is.na(E), E != "")
  stats::setNames(rows$E, rows$D)
}

decode_value <- function(code, lookup) {
  key <- trimws(as.character(code %||% ""))
  if (!length(lookup) || is.na(key) || key == "") return(ifelse(key == "", NA_character_, as.character(code)))
  if (key %in% names(lookup)) lookup[[key]] else as.character(code)
}

latest_non_null <- function(df, field) {
  vals <- df %>%
    filter(!is.na(.data[[field]])) %>%
    arrange(desc(year)) %>%
    select(year, value = all_of(field)) %>%
    slice(1)
  if (nrow(vals) == 0) return(list(year = NA_real_, value = NA_real_))
  list(year = vals$year[[1]], value = vals$value[[1]])
}

count_decline_years <- function(years, values, start_year, end_year, threshold_pct = 0) {
  lookup <- stats::setNames(values, years)
  count <- 0
  for (yr in start_year:end_year) {
    old_val <- unname(lookup[as.character(yr)])
    new_val <- unname(lookup[as.character(yr + 1)])
    pct <- safe_pct_change(new_val, old_val)
    if (!is.na(pct) && pct <= threshold_pct) count <- count + 1
  }
  count
}

count_negative_years <- function(years, values, target_years, threshold = 0) {
  lookup <- stats::setNames(values, years)
  sum(purrr::map_lgl(target_years, ~ {
    val <- unname(lookup[as.character(.x)])
    !is.na(val) && val < threshold
  }))
}

loss_frequency <- function(years, values, end_year, window_years) {
  lookup <- stats::setNames(values, years)
  start_year <- end_year - window_years + 1
  sum(vapply(start_year:end_year, function(yr) {
    val <- unname(lookup[as.character(yr)])
    !is.na(val) && val < 0
  }, logical(1)))
}

raw_rows <- suppressMessages(readr::read_csv(raw_path, show_col_types = FALSE, guess_max = 100000))
catalog <- suppressMessages(readr::read_csv(catalog_path, show_col_types = FALSE, guess_max = 10000))

raw_rows <- raw_rows %>%
  mutate(
    unitid = as.character(unitid),
    year = as.integer(to_num(year))
  )

if ("state" %in% names(raw_rows)) {
  raw_rows <- raw_rows %>% filter(!(state %in% excluded_state_codes))
}

if ("transfer_out_rate_bachelor" %in% names(raw_rows)) {
  raw_rows <- raw_rows %>% select(-transfer_out_rate_bachelor)
}

if (!("F1H01" %in% names(raw_rows))) raw_rows$F1H01 <- NA_character_
if (!("F2H01" %in% names(raw_rows))) raw_rows$F2H01 <- NA_character_

catalog <- catalog %>%
  mutate(
    year = as.integer(to_num(year)),
    table_name = as.character(table_name)
  )

endowment_value_by_key <- raw_rows %>%
  mutate(
    control_label = get_control_label(control),
    endowment_value = case_when(
      control_label == "Public" ~ coalesce(to_num(value_endowment_assets_end_gasb), to_num(F1H01)),
      control_label == "Private not-for-profit" ~ coalesce(to_num(value_endowment_end_fasb), to_num(F2H01)),
      TRUE ~ NA_real_
    ),
    row_key = paste(unitid, year, sep = "|")
  ) %>%
  filter(!is.na(unitid), !is.na(year), !is.na(endowment_value)) %>%
  select(row_key, endowment_value)

hd2024_dict <- file.path(root, "raw_build", "downloads", "dict", "HD2024.zip")
ic2024_dict <- file.path(root, "raw_build", "downloads", "dict", "IC2024.zip")
flags2024_dict <- file.path(root, "raw_build", "downloads", "dict", "FLAGS2024.zip")
hd_sector_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "SECTOR") else character()
hd_level_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "ICLEVEL") else character()
hd_act_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "ACT") else character()
hd_active_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "CYACTIVE") else character()
hd_hbcu_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "HBCU") else character()
hd_tribal_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "TRIBAL") else character()
hd_grad_offering_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "GROFFER") else character()
hd_category_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "INSTCAT") else character()
hd_locale_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "LOCALE") else character()
hd_access_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "CARNEGIESAEC") else character()
hd_size_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "CARNEGIESIZE") else character()
hd_ug_mix_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "CARNEGIEAPM") else character()
hd_grad_mix_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "CARNEGIEGPM") else character()
ic_religious_affiliation_lookup <- if (file.exists(ic2024_dict)) get_frequency_lookup(ic2024_dict, "IC2024", "RELAFFIL") else character()
flags_form_lookup <- if (file.exists(flags2024_dict)) get_frequency_lookup(flags2024_dict, "FLAGS2024", "FORM_F") else character()

load_catalog_table <- function(entry) {
  table_name <- entry$table_name[[1]]
  year <- entry$year[[1]]
  shared_zip_path <- file.path(root, "raw_build", "downloads", "data", paste0(table_name, ".zip"))
  zip_path <- if (file.exists(shared_zip_path)) shared_zip_path else file.path(aux_data_root, paste0(table_name, ".zip"))
  extract_path <- file.path(aux_extract_root, table_name)
  if (!file.exists(zip_path)) {
    download_ok <- tryCatch({
      download_if_missing(entry$data_url[[1]], zip_path)
      TRUE
    }, error = function(e) FALSE)
    if (!download_ok) return(NULL)
  }
  expand_zip_if_missing(zip_path, extract_path)
  csv_file <- find_first_file(extract_path, "\\.csv$")
  if (is.na(csv_file)) return(NULL)
  list(year = year, table_name = table_name, data = suppressMessages(readr::read_csv(csv_file, show_col_types = FALSE, guess_max = 100000)))
}

eap_tables_df <- catalog %>% filter(str_detect(table_name, "^EAP\\d{4}$"))
effy_tables_df <- catalog %>% filter(str_detect(table_name, "^EFFY\\d{4}$"))
sfa_tables_df <- catalog %>% filter(str_detect(table_name, "^SFA\\d{4}$"))
drvf_tables_df <- catalog %>% filter(str_detect(table_name, "^DRVF\\d{4}$"))

eap_tables <- split(eap_tables_df, seq_len(nrow(eap_tables_df)))
effy_tables <- split(effy_tables_df, seq_len(nrow(effy_tables_df)))
sfa_tables <- split(sfa_tables_df, seq_len(nrow(sfa_tables_df)))
drvf_tables <- split(drvf_tables_df, seq_len(nrow(drvf_tables_df)))

eap_by_year_unit <- list()
for (entry in eap_tables) {
  loaded <- load_catalog_table(entry)
  if (is.null(loaded)) next
  table_lookup <- loaded$data %>%
    transmute(
      UNITID = as.character(UNITID),
      EAPCAT = as.character(EAPCAT),
      EAPTOT = to_num(EAPTOT)
    ) %>%
    filter(EAPCAT %in% c("10000", "21000")) %>%
    mutate(
      staff_headcount_total = if_else(EAPCAT == "10000", EAPTOT, NA_real_),
      staff_headcount_instructional = if_else(EAPCAT == "21000", EAPTOT, NA_real_)
    ) %>%
    group_by(UNITID) %>%
    summarise(
      staff_headcount_total = suppressWarnings(max(staff_headcount_total, na.rm = TRUE)),
      staff_headcount_instructional = suppressWarnings(max(staff_headcount_instructional, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(across(c(staff_headcount_total, staff_headcount_instructional), ~ ifelse(is.infinite(.x), NA_real_, .x)))
  eap_by_year_unit[[as.character(loaded$year)]] <- table_lookup
}

effy_by_year_unit <- list()
for (entry in effy_tables) {
  loaded <- load_catalog_table(entry)
  if (is.null(loaded)) next
  dat <- loaded$data %>% mutate(UNITID = as.character(UNITID))
  if ("EFFYALEV" %in% names(dat)) {
    dat <- dat %>% mutate(level_code = as.character(EFFYALEV))
  } else if ("LSTUDY" %in% names(dat)) {
    dat <- dat %>% mutate(level_code = dplyr::case_when(
      as.character(LSTUDY) == "999" ~ "1",
      as.character(LSTUDY) == "1" ~ "2",
      as.character(LSTUDY) == "3" ~ "12",
      TRUE ~ as.character(LSTUDY)
    ))
  } else {
    dat <- dat %>% mutate(level_code = NA_character_)
  }
  table_lookup <- dat %>%
    transmute(
      UNITID,
      level_code,
      EFYTOTLT = to_num(EFYTOTLT),
      EFYNRALT = to_num(EFYNRALT)
    ) %>%
    filter(level_code %in% c("1", "2", "12")) %>%
    group_by(UNITID) %>%
    summarise(
      enrollment_headcount_total = EFYTOTLT[level_code == "1"][1] %||% NA_real_,
      enrollment_headcount_undergrad = EFYTOTLT[level_code == "2"][1] %||% NA_real_,
      enrollment_headcount_graduate = EFYTOTLT[level_code == "12"][1] %||% NA_real_,
      enrollment_nonresident_total = EFYNRALT[level_code == "1"][1] %||% NA_real_,
      enrollment_nonresident_undergrad = EFYNRALT[level_code == "2"][1] %||% NA_real_,
      enrollment_nonresident_graduate = EFYNRALT[level_code == "12"][1] %||% NA_real_,
      .groups = "drop"
    )
  effy_by_year_unit[[as.character(loaded$year)]] <- table_lookup
}

sfa_by_year_unit <- list()
for (entry in sfa_tables) {
  loaded <- load_catalog_table(entry)
  if (is.null(loaded)) next
  table_lookup <- loaded$data %>%
    transmute(
      UNITID = as.character(UNITID),
      loan_pct_undergrad_federal = if ("UFLOANP" %in% names(loaded$data)) to_num(UFLOANP) else NA_real_,
      loan_avg_undergrad_federal = if ("UFLOANA" %in% names(loaded$data)) to_num(UFLOANA) else NA_real_,
      loan_count_undergrad_federal = if ("UFLOANN" %in% names(loaded$data)) to_num(UFLOANN) else NA_real_
    )
  sfa_by_year_unit[[as.character(loaded$year)]] <- table_lookup
}

drvf_by_year_unit <- list()
for (entry in drvf_tables) {
  loaded <- load_catalog_table(entry)
  if (is.null(loaded)) next
  table_lookup <- loaded$data %>%
    transmute(
      UNITID = as.character(UNITID),
      core_revenue = dplyr::coalesce(if ("F1CORREV" %in% names(loaded$data)) to_num(F1CORREV) else NA_real_, if ("F2CORREV" %in% names(loaded$data)) to_num(F2CORREV) else NA_real_),
      gov_grants_contracts_pct_core_revenue_gasb = if ("F1GVGCPC" %in% names(loaded$data)) to_num(F1GVGCPC) else NA_real_,
      gov_grants_contracts_pct_core_revenue_fasb = if ("F2GVGCPC" %in% names(loaded$data)) to_num(F2GVGCPC) else NA_real_,
      state_appropriations_pct_core_revenue_gasb = if ("F1STAPPC" %in% names(loaded$data)) to_num(F1STAPPC) else NA_real_
    )
  drvf_by_year_unit[[as.character(loaded$year)]] <- table_lookup
}

transfer_out_by_year_unit <- list()

for (yr in sort(unique(raw_rows$year[raw_rows$year >= 2020]))) {
  table_name <- paste0("DRVGR", yr)
  zip_path <- file.path(root, "raw_build", "downloads", "data", paste0(table_name, ".zip"))
  extract_path <- file.path(root, "raw_build", "downloads", "extracted", paste0("data_", table_name))
  if (!file.exists(zip_path)) next
  expand_zip_if_missing(zip_path, extract_path)
  csv_file <- find_first_file(extract_path, "\\.csv$")
  if (is.na(csv_file)) next
  dat <- suppressMessages(readr::read_csv(csv_file, show_col_types = FALSE, guess_max = 100000))
  names(dat) <- toupper(names(dat))
  if (!all(c("UNITID", "GBATRRT") %in% names(dat))) next
  transfer_out_by_year_unit[[as.character(yr)]] <- dat %>%
    transmute(
      UNITID = as.character(UNITID),
      transfer_out_rate_bachelor = to_num(GBATRRT)
    )
}

for (yr in 2014:2023) {
  if (as.character(yr) %in% names(transfer_out_by_year_unit)) next
  gr_zip <- file.path(root, "raw_build", "downloads", "data", paste0("GR", yr, ".zip"))
  gr_extract <- file.path(root, "raw_build", "downloads", "extracted", paste0("data_GR", yr))
  if (!file.exists(gr_zip)) next
  expand_zip_if_missing(gr_zip, gr_extract)
  gr_csv <- find_first_file(gr_extract, "\\.csv$")
  if (is.na(gr_csv)) next
  gr <- suppressMessages(readr::read_csv(gr_csv, show_col_types = FALSE, guess_max = 100000))
  if (!all(c("UNITID", "GRTYPE", "GRTOTLT") %in% names(gr))) next
  transfer_out_by_year_unit[[as.character(yr)]] <- gr %>%
    transmute(
      UNITID = as.character(UNITID),
      GRTYPE = as.character(GRTYPE),
      GRTOTLT = to_num(GRTOTLT)
    ) %>%
    filter(GRTYPE %in% c("8", "16")) %>%
    group_by(UNITID) %>%
    summarise(
      bachelor_adjusted_cohort = GRTOTLT[GRTYPE == "8"][1] %||% NA_real_,
      bachelor_transfer_out_count = GRTOTLT[GRTYPE == "16"][1] %||% NA_real_,
      .groups = "drop"
    ) %>%
    mutate(
      transfer_out_rate_bachelor = safe_divide(bachelor_transfer_out_count, bachelor_adjusted_cohort) * 100
    ) %>%
    select(UNITID, transfer_out_rate_bachelor)
}

bind_aux_year <- function(aux_list) {
  if (length(aux_list) == 0) return(tibble::tibble(unitid = character(), year = integer()))
  purrr::imap_dfr(aux_list, function(tbl, yr) {
    if (is.null(tbl) || nrow(tbl) == 0) return(tibble::tibble(unitid = character(), year = integer()))
    tbl %>%
      mutate(year = as.integer(yr), unitid = as.character(UNITID)) %>%
      select(-any_of("UNITID"))
  })
}

eap_long <- bind_aux_year(eap_by_year_unit)
effy_long <- bind_aux_year(effy_by_year_unit)
sfa_long <- bind_aux_year(sfa_by_year_unit)
drvf_long <- bind_aux_year(drvf_by_year_unit)
transfer_out_long <- bind_aux_year(transfer_out_by_year_unit)

for (nm in c(
  "enrollment_headcount_total", "enrollment_headcount_undergrad", "enrollment_headcount_graduate",
  "enrollment_nonresident_total", "enrollment_nonresident_undergrad", "enrollment_nonresident_graduate"
)) {
  if (!(nm %in% names(effy_long))) effy_long[[nm]] <- NA_real_
}
for (nm in c("staff_headcount_total", "staff_headcount_instructional")) {
  if (!(nm %in% names(eap_long))) eap_long[[nm]] <- NA_real_
}

raw_enriched <- raw_rows %>%
  left_join(effy_long, by = c("unitid", "year")) %>%
  left_join(eap_long, by = c("unitid", "year")) %>%
  left_join(sfa_long, by = c("unitid", "year")) %>%
  left_join(drvf_long, by = c("unitid", "year")) %>%
  left_join(transfer_out_long, by = c("unitid", "year"))

latest_name_lookup <- raw_enriched %>%
  mutate(state_full_latest = get_state_name(state)) %>%
  arrange(unitid, desc(year)) %>%
  group_by(unitid) %>%
  summarise(
    institution_name_latest = {
      idx <- which(!is.na(institution_name) & institution_name != "")
      if (length(idx) == 0) NA_character_ else institution_name[idx[1]]
    },
    city_latest = {
      idx <- which(!is.na(city) & city != "")
      if (length(idx) == 0) NA_character_ else city[idx[1]]
    },
    state_latest = {
      idx <- which(!is.na(state_full_latest) & state_full_latest != "")
      if (length(idx) == 0) NA_character_ else state_full_latest[idx[1]]
    },
    .groups = "drop"
  ) %>%
  mutate(
    institution_unique_name_latest = dplyr::case_when(
      is.na(institution_name_latest) ~ NA_character_,
      TRUE ~ paste(
        dplyr::coalesce(institution_name_latest, ""),
        dplyr::coalesce(city_latest, ""),
        dplyr::coalesce(state_latest, ""),
        sep = " | "
      ) %>%
        stringr::str_replace_all("\\s*\\|\\s*$", "") %>%
        stringr::str_replace_all("\\|\\s*\\|", "|")
    )
  )

raw_enriched <- raw_enriched %>%
  left_join(latest_name_lookup, by = "unitid")

prepared_rows <- purrr::map_dfr(seq_len(nrow(raw_enriched)), function(i) {
  row <- raw_enriched[i, , drop = FALSE]
  year <- row$year[[1]]
  unitid <- as.character(row$unitid[[1]])
  row_key <- paste(unitid, year, sep = "|")
  control_label <- get_control_label(row$control[[1]])

  sector_decoded <- decode_value(row$sector[[1]], hd_sector_lookup)
  level_decoded <- decode_value(row$level[[1]], hd_level_lookup)
  status_decoded <- decode_value(row$status[[1]], hd_act_lookup)
  is_active_decoded <- decode_value(row$is_active[[1]], hd_active_lookup)
  state_full <- get_state_name(row$state[[1]])

  if (!(control_label %in% c("Public", "Private not-for-profit", "Private for-profit"))) return(NULL)
  if (!identical(level_decoded, "Four or more years")) return(NULL)
  if (!is.na(sector_decoded) && str_detect(sector_decoded, "^Administrative Unit")) return(NULL)
  if (!is.na(is_active_decoded) && !(is_active_decoded %in% c("Yes", "Imputed as active"))) return(NULL)

  fte12 <- to_num(row$fte_12_months[[1]])
  revenue <- if (identical(control_label, "Public")) {
    to_num(row$total_operating_nonoperating_revenues_gasb[[1]])
  } else if (identical(control_label, "Private not-for-profit")) {
    to_num(row$total_revenues_investment_return_fasb[[1]])
  } else if (identical(control_label, "Private for-profit")) {
    to_num(row$total_revenues_investment_return_pfp[[1]])
  } else {
    NA_real_
  }
  expenses <- if (identical(control_label, "Public")) {
    to_num(row[["total_expenses_deductions_current_total_gasb"]][[1]])
  } else if (identical(control_label, "Private not-for-profit")) {
    to_num(row[["total_expenses_fasb"]][[1]])
  } else if (identical(control_label, "Private for-profit")) {
    to_num(row[["total_expenses_total_amount_pfp"]][[1]])
  } else {
    NA_real_
  }
  net_tuition_total <- if (identical(control_label, "Public")) {
    to_num(row$tuition_fees_after_discounts_allowances_gasb[[1]])
  } else if (identical(control_label, "Private not-for-profit")) {
    (to_num(row$tuition_and_fees_fasb[[1]]) + zero_if_null(to_num(row$allowances_applied_to_tuition_fasb[[1]]))) -
      (zero_if_null(to_num(row$institutional_grants_funded_fasb[[1]])) + zero_if_null(to_num(row$institutional_grants_unfunded_fasb[[1]])))
  } else if (identical(control_label, "Private for-profit")) {
    (to_num(row$tuition_fees_pfp[[1]]) + zero_if_null(to_num(row$discounts_allowances_applied_tuition_fees_pfp[[1]]))) -
      zero_if_null(to_num(row$institutional_grants_pfp[[1]]))
  } else {
    NA_real_
  }
  federal_funding <- if (identical(control_label, "Public")) {
    to_num(row$federal_operating_grants_contracts_gasb[[1]])
  } else if (identical(control_label, "Private not-for-profit")) {
    to_num(row$federal_grants_contracts_fasb[[1]])
  } else if (identical(control_label, "Private for-profit")) {
    to_num(row$federal_grants_contracts_pfp[[1]])
  } else {
    NA_real_
  }
  state_funding <- if (identical(control_label, "Public")) {
    to_num(row$state_appropriations_gasb[[1]])
  } else if (identical(control_label, "Private not-for-profit")) {
    to_num(row$state_approps_fasb[[1]])
  } else if (identical(control_label, "Private for-profit")) {
    to_num(row$state_appropriations_pfp[[1]])
  } else {
    NA_real_
  }
  assets <- to_num(row$assets[[1]])
  liabilities <- to_num(row$liabilities[[1]])
  unrestricted_assets <- if (identical(control_label, "Public")) {
    to_num(row$unrestricted_public[[1]])
  } else if (identical(control_label, "Private not-for-profit")) {
    to_num(row$total_unrestricted_net_assets_fasb[[1]])
  } else {
    NA_real_
  }
  gross_tuition <- if (identical(control_label, "Public")) {
    zero_if_null(to_num(row$discounts_allowances_applied_tuition_fees_gasb[[1]])) + zero_if_null(to_num(row$tuition_fees_after_discounts_allowances_gasb[[1]]))
  } else if (identical(control_label, "Private not-for-profit")) {
    zero_if_null(to_num(row$tuition_and_fees_fasb[[1]])) + zero_if_null(to_num(row$allowances_applied_to_tuition_fasb[[1]]))
  } else {
    NA_real_
  }
  discount_rate <- if (identical(control_label, "Public")) {
    safe_divide(to_num(row$discounts_allowances_applied_tuition_fees_gasb[[1]]), gross_tuition)
  } else if (identical(control_label, "Private not-for-profit")) {
    safe_divide(to_num(row$institutional_grants_unfunded_fasb[[1]]), gross_tuition)
  } else {
    NA_real_
  }
  endowment_value <- endowment_value_by_key %>%
    filter(row_key == !!row_key) %>%
    pull(endowment_value) %>%
    .[1] %||% if (identical(control_label, "Public")) to_num(row$value_endowment_assets_end_gasb[[1]]) else to_num(row$value_endowment_end_fasb[[1]])
  government_funding <- if (is.na(federal_funding) && is.na(state_funding)) NA_real_ else zero_if_null(federal_funding) + zero_if_null(state_funding)
  core_revenue <- dplyr::coalesce(
    to_num(row$core_revenue[[1]]),
    if (identical(control_label, "Public")) {
      total_rev <- to_num(row$total_operating_nonoperating_revenues_gasb[[1]])
      aux_rev <- to_num(row$auxiliary_enterprises_revenue_gasb[[1]])
      hosp_rev <- to_num(row$hospital_services_revenue_gasb[[1]])
      indep_rev <- to_num(row$independent_operations_revenue_gasb[[1]])
      if (is.na(total_rev)) NA_real_ else total_rev - zero_if_null(aux_rev) - zero_if_null(hosp_rev) - zero_if_null(indep_rev)
    } else if (identical(control_label, "Private not-for-profit")) {
      total_rev <- to_num(row$total_revenues_investment_return_fasb[[1]])
      aux_rev <- to_num(row$auxiliary_enterprises_revenue_fasb[[1]])
      hosp_rev <- to_num(row$hospital_revenue_fasb[[1]])
      indep_rev <- to_num(row$independent_operations_revenue_fasb[[1]])
      if (is.na(total_rev)) NA_real_ else total_rev - zero_if_null(aux_rev) - zero_if_null(hosp_rev) - zero_if_null(indep_rev)
    } else {
      NA_real_
    }
  )
  gov_grants_fasb <- dplyr::coalesce(
    to_num(row$gov_grants_fasb[[1]]),
    if (identical(control_label, "Private not-for-profit")) {
      safe_divide(
        sum_if_any(c(
          to_num(row$federal_grants_contracts_fasb[[1]]),
          to_num(row$state_grants_contracts_fasb[[1]]),
          to_num(row$local_grants_contracts_fasb[[1]])
        )),
        core_revenue
      )
    } else {
      NA_real_
    }
  )
  state_approps_percent_core_gasb <- dplyr::coalesce(
    to_num(row$state_approps_percent_core_gasb[[1]]),
    if (identical(control_label, "Public")) safe_divide(to_num(row$state_appropriations_gasb[[1]]), core_revenue) else NA_real_
  )
  state_revenue_fte_fasb <- dplyr::coalesce(
    to_num(row$state_revenue_fte_fasb[[1]]),
    if (identical(control_label, "Private not-for-profit")) safe_divide(to_num(row$state_approps_fasb[[1]]), fte12) else NA_real_
  )
  gov_revenue_fte_fasb <- dplyr::coalesce(
    to_num(row$gov_revenue_fte_fasb[[1]]),
    if (identical(control_label, "Private not-for-profit")) {
      safe_divide(
        sum_if_any(c(
          to_num(row$federal_grants_contracts_fasb[[1]]),
          to_num(row$state_grants_contracts_fasb[[1]]),
          to_num(row$local_grants_contracts_fasb[[1]])
        )),
        fte12
      )
    } else {
      NA_real_
    }
  )
  loss_amount <- if (is.na(revenue) || is.na(expenses)) NA_real_ else revenue - expenses
  pell_accounting_method <- trimws(as.character(row$pell_accounting_method[[1]] %||% ""))
  pell_grants <- to_num(row$pell_grants[[1]])
  federal_adj <- if (identical(control_label, "Public")) {
    federal_funding
  } else if (identical(control_label, "Private not-for-profit")) {
    if (is.na(federal_funding)) NA_real_ else if (identical(pell_accounting_method, "2")) pmax(federal_funding - zero_if_null(pell_grants), 0) else federal_funding
  } else {
    NA_real_
  }

  revenue_adjusted <- inflate_to_base_year(revenue, year)
  expenses_adjusted <- inflate_to_base_year(expenses, year)
  loss_amount_adjusted <- inflate_to_base_year(loss_amount, year)
  net_tuition_total_adjusted <- inflate_to_base_year(net_tuition_total, year)
  federal_funding_adjusted <- inflate_to_base_year(federal_funding, year)
  federal_adj_adjusted <- inflate_to_base_year(federal_adj, year)
  state_funding_adjusted <- inflate_to_base_year(state_funding, year)
  government_funding_adjusted <- inflate_to_base_year(government_funding, year)
  core_revenue_adjusted <- inflate_to_base_year(core_revenue, year)
  endowment_value_adjusted <- inflate_to_base_year(endowment_value, year)
  assets_adjusted <- inflate_to_base_year(assets, year)
  liabilities_adjusted <- inflate_to_base_year(liabilities, year)

  tibble::tibble(
    unitid = unitid,
    institution_name = dplyr::coalesce(row$institution_name_latest[[1]], row$institution_name[[1]]),
    institution_unique_name = dplyr::coalesce(
      row$institution_unique_name_latest[[1]],
      paste(na.omit(c(row$institution_name[[1]], row$city[[1]], state_full)), collapse = " | ")
    ),
    year = year,
    control_label = control_label,
    state = dplyr::coalesce(row$state_latest[[1]], state_full),
    city = dplyr::coalesce(row$city_latest[[1]], row$city[[1]]),
    urbanization = decode_value(row$urbanization[[1]], hd_locale_lookup),
    sector = sector_decoded,
    level = level_decoded,
    category = decode_value(row$category[[1]], hd_category_lookup),
    institution_status = status_decoded,
    is_active = is_active_decoded,
    hbcu = decode_value(row$hbcu[[1]], hd_hbcu_lookup),
    tribal_college = decode_value(row$tribal_college[[1]], hd_tribal_lookup),
    grad_offering = decode_value(row$grad_offering[[1]], hd_grad_offering_lookup),
    reporting_model = decode_value(row$reporting_model[[1]], flags_form_lookup),
    access_earnings = decode_value(row$access_earnings[[1]], hd_access_lookup),
    size = decode_value(row$size[[1]], hd_size_lookup),
    grad_program_mix = decode_value(row$grad_program_mix[[1]], hd_grad_mix_lookup),
    undergrad_program_mix = decode_value(row$undergrad_program_mix[[1]], hd_ug_mix_lookup),
    religious_affiliation = decode_value(row$religious_affiliation[[1]], ic_religious_affiliation_lookup),
    all_programs_distance_education = decode_yes_no_field(row$all_programs_distance_education[[1]]),
    fte_12_months = fte12,
    fte_undergrad = to_num(row$fte_undergrad[[1]]),
    fte_graduate = to_num(row$fte_graduate[[1]]),
    enrollment_headcount_total = to_num(row$enrollment_headcount_total[[1]]),
    enrollment_headcount_undergrad = to_num(row$enrollment_headcount_undergrad[[1]]),
    enrollment_headcount_graduate = to_num(row$enrollment_headcount_graduate[[1]]),
    enrollment_nonresident_total = to_num(row$enrollment_nonresident_total[[1]]),
    enrollment_nonresident_undergrad = to_num(row$enrollment_nonresident_undergrad[[1]]),
    enrollment_nonresident_graduate = to_num(row$enrollment_nonresident_graduate[[1]]),
    staff_fte_total = to_num(row$fte_total_staff[[1]]),
    staff_fte_instructional = to_num(row$fte_instructional[[1]]),
    transfer_out_rate_bachelor = to_num(row$transfer_out_rate_bachelor[[1]]),
    staff_headcount_total = to_num(row$staff_headcount_total[[1]]),
    staff_headcount_instructional = to_num(row$staff_headcount_instructional[[1]]),
    loan_pct_undergrad_federal = to_num(row$loan_pct_undergrad_federal[[1]]),
    loan_avg_undergrad_federal = to_num(row$loan_avg_undergrad_federal[[1]]),
    loan_count_undergrad_federal = to_num(row$loan_count_undergrad_federal[[1]]),
    revenue_total = revenue,
    revenue_total_adjusted = revenue_adjusted,
    expenses_total = expenses,
    expenses_total_adjusted = expenses_adjusted,
    loss_amount = loss_amount,
    loss_amount_adjusted = loss_amount_adjusted,
    ended_year_at_loss = dplyr::case_when(is.na(loss_amount) ~ NA_character_, loss_amount < 0 ~ "Yes", TRUE ~ "No"),
    operating_margin = ifelse(is.na(revenue) | revenue == 0 | is.na(expenses), NA_real_, ((revenue - expenses) / revenue) * 100),
    net_tuition_total = net_tuition_total,
    net_tuition_total_adjusted = net_tuition_total_adjusted,
    net_tuition_per_fte = safe_divide(net_tuition_total, fte12),
    net_tuition_per_fte_adjusted = safe_divide(net_tuition_total_adjusted, fte12),
    tuition_dependence_ratio = safe_divide(net_tuition_total, revenue),
    tuition_dependence_pct = safe_divide(net_tuition_total, revenue) * 100,
    admissions_yield = to_num(row$admissions_yield[[1]]),
    federal_funding = federal_funding,
    federal_funding_adjusted = federal_funding_adjusted,
    federal_grants_contracts_pell_adjusted = federal_adj,
    federal_grants_contracts_pell_adjusted_adjusted = federal_adj_adjusted,
    federal_grants_contracts_pell_adjusted_pct_core_revenue = safe_divide(federal_adj, core_revenue),
    state_funding = state_funding,
    state_funding_adjusted = state_funding_adjusted,
    core_revenue = core_revenue,
    core_revenue_adjusted = core_revenue_adjusted,
    state_funding_pct_core_revenue = safe_divide(state_funding, core_revenue),
    state_approps_percent_core_gasb = state_approps_percent_core_gasb,
    gov_grants_fasb = gov_grants_fasb,
    state_revenue_fte_fasb = state_revenue_fte_fasb,
    gov_revenue_fte_fasb = gov_revenue_fte_fasb,
    gov_grants_contracts_pct_core_revenue_gasb = to_num(row$gov_grants_contracts_pct_core_revenue_gasb[[1]]),
    gov_grants_contracts_pct_core_revenue_fasb = to_num(row$gov_grants_contracts_pct_core_revenue_fasb[[1]]),
    state_appropriations_pct_core_revenue_gasb = to_num(row$state_appropriations_pct_core_revenue_gasb[[1]]),
    government_funding_total = government_funding,
    government_funding_total_adjusted = government_funding_adjusted,
    government_funding_pct_total_revenue = safe_divide(government_funding, revenue),
    endowment_value = endowment_value,
    endowment_value_adjusted = endowment_value_adjusted,
    endowment_assets_per_fte_gasb = to_num(row$endowment_assets_per_fte_gasb[[1]]),
    endowment_assets_per_fte_fasb = to_num(row$endowment_assets_per_fte_fasb[[1]]),
    endowment_assets_per_fte = ifelse(control_label == "Public", to_num(row$endowment_assets_per_fte_gasb[[1]]), to_num(row$endowment_assets_per_fte_fasb[[1]])),
    endowment_assets_per_fte_adjusted = safe_divide(endowment_value_adjusted, fte12),
    leverage = safe_divide(liabilities, assets),
    liquidity = safe_divide(unrestricted_assets, assets),
    discount_rate = discount_rate,
    assets = assets,
    assets_adjusted = assets_adjusted,
    liabilities = liabilities,
    liabilities_adjusted = liabilities_adjusted
  ) %>%
    mutate(
      pct_international_all = safe_divide(enrollment_nonresident_total, enrollment_headcount_total),
      pct_international_undergraduate = safe_divide(enrollment_nonresident_undergrad, enrollment_headcount_undergrad),
      pct_international_graduate = safe_divide(enrollment_nonresident_graduate, enrollment_headcount_graduate)
    )
})

effy_backfill <- effy_long %>%
  mutate(
    enrollment_headcount_total = dplyr::coalesce(.data$enrollment_headcount_total, NA_real_),
    enrollment_headcount_undergrad = dplyr::coalesce(.data$enrollment_headcount_undergrad, NA_real_),
    enrollment_headcount_graduate = dplyr::coalesce(.data$enrollment_headcount_graduate, NA_real_),
    enrollment_nonresident_total = dplyr::coalesce(.data$enrollment_nonresident_total, NA_real_),
    enrollment_nonresident_undergrad = dplyr::coalesce(.data$enrollment_nonresident_undergrad, NA_real_),
    enrollment_nonresident_graduate = dplyr::coalesce(.data$enrollment_nonresident_graduate, NA_real_)
  ) %>%
  select(
    unitid, year,
    enrollment_headcount_total_bf = enrollment_headcount_total,
    enrollment_headcount_undergrad_bf = enrollment_headcount_undergrad,
    enrollment_headcount_graduate_bf = enrollment_headcount_graduate,
    enrollment_nonresident_total_bf = enrollment_nonresident_total,
    enrollment_nonresident_undergrad_bf = enrollment_nonresident_undergrad,
    enrollment_nonresident_graduate_bf = enrollment_nonresident_graduate
  )

eap_backfill <- eap_long %>%
  mutate(
    staff_headcount_total = dplyr::coalesce(.data$staff_headcount_total, NA_real_),
    staff_headcount_instructional = dplyr::coalesce(.data$staff_headcount_instructional, NA_real_)
  ) %>%
  select(
    unitid, year,
    staff_headcount_total_bf = staff_headcount_total,
    staff_headcount_instructional_bf = staff_headcount_instructional
  )

prepared_rows <- prepared_rows %>%
  left_join(effy_backfill, by = c("unitid", "year")) %>%
  left_join(eap_backfill, by = c("unitid", "year")) %>%
  mutate(
    enrollment_headcount_total = coalesce(enrollment_headcount_total, enrollment_headcount_total_bf),
    enrollment_headcount_undergrad = coalesce(enrollment_headcount_undergrad, enrollment_headcount_undergrad_bf),
    enrollment_headcount_graduate = coalesce(enrollment_headcount_graduate, enrollment_headcount_graduate_bf),
    enrollment_nonresident_total = coalesce(enrollment_nonresident_total, enrollment_nonresident_total_bf),
    enrollment_nonresident_undergrad = coalesce(enrollment_nonresident_undergrad, enrollment_nonresident_undergrad_bf),
    enrollment_nonresident_graduate = coalesce(enrollment_nonresident_graduate, enrollment_nonresident_graduate_bf),
    staff_headcount_total = coalesce(staff_headcount_total, staff_headcount_total_bf),
    staff_headcount_instructional = coalesce(staff_headcount_instructional, staff_headcount_instructional_bf)
  ) %>%
  select(-ends_with("_bf"))

for (nm in c("staff_headcount_total", "staff_headcount_instructional")) {
  if (!(nm %in% names(prepared_rows))) prepared_rows[[nm]] <- NA_real_
}

enrich_group <- function(df) {
  df <- df %>% arrange(year)
  years <- df$year
  enroll_lookup <- stats::setNames(df$enrollment_headcount_total, years)
  enroll_fte_lookup <- stats::setNames(df$fte_12_months, years)
  staff_fte_lookup <- stats::setNames(df$staff_fte_total, years)
  staff_instr_fte_lookup <- stats::setNames(df$staff_fte_instructional, years)
  staff_head_lookup <- stats::setNames(df$staff_headcount_total, years)
  staff_instr_head_lookup <- stats::setNames(df$staff_headcount_instructional, years)
  revenue_lookup <- stats::setNames(df$revenue_total, years)
  revenue_adjusted_lookup <- stats::setNames(df$revenue_total_adjusted, years)
  op_margin_lookup <- stats::setNames(df$operating_margin, years)
  net_tuition_lookup <- stats::setNames(df$net_tuition_total, years)
  net_tuition_adjusted_lookup <- stats::setNames(df$net_tuition_total_adjusted, years)
  net_tuition_fte_lookup <- stats::setNames(df$net_tuition_per_fte, years)
  net_tuition_fte_adjusted_lookup <- stats::setNames(df$net_tuition_per_fte_adjusted, years)
  yield_lookup <- stats::setNames(df$admissions_yield, years)
  discount_lookup <- stats::setNames(df$discount_rate, years)
  government_lookup <- stats::setNames(df$government_funding_total, years)
  government_adjusted_lookup <- stats::setNames(df$government_funding_total_adjusted, years)
  endowment_lookup <- stats::setNames(df$endowment_value, years)
  endowment_adjusted_lookup <- stats::setNames(df$endowment_value_adjusted, years)
  intl_lookup <- stats::setNames(df$enrollment_nonresident_total, years)
  loss_lookup <- stats::setNames(df$loss_amount, years)
  loss_adjusted_lookup <- stats::setNames(df$loss_amount_adjusted, years)
  federal_adj_lookup <- stats::setNames(df$federal_grants_contracts_pell_adjusted, years)
  federal_adj_adjusted_lookup <- stats::setNames(df$federal_grants_contracts_pell_adjusted_adjusted, years)
  state_lookup_year <- stats::setNames(df$state_funding, years)
  state_adjusted_lookup_year <- stats::setNames(df$state_funding_adjusted, years)
  transfer_out_lookup <- stats::setNames(df$transfer_out_rate_bachelor, years)
  loan_pct_latest <- latest_non_null(df, "loan_pct_undergrad_federal")
  loan_count_latest <- latest_non_null(df, "loan_count_undergrad_federal")
  loan_avg_latest <- latest_non_null(df, "loan_avg_undergrad_federal")
  loss2024 <- unname(loss_lookup["2024"])

  df %>%
    rowwise() %>%
    mutate(
      enrollment_pct_change_5yr = safe_pct_change(enrollment_headcount_total, unname(enroll_lookup[as.character(year - 5)])),
      enrollment_decreased_5yr = case_when(is.na(enrollment_pct_change_5yr) ~ NA_character_, enrollment_pct_change_5yr < 0 ~ "Yes", TRUE ~ "No"),
      enroll_fte_pct_change_5yr = safe_pct_change(fte_12_months, unname(enroll_fte_lookup[as.character(year - 5)])),
      enrollment_decline_last_3_of_5 = ifelse(count_decline_years(years, df$enrollment_headcount_total, year - 5, year - 1, 0) >= 3, "Yes", "No"),
      enroll_fte_decline_last_3_of_5 = ifelse(count_decline_years(years, df$fte_12_months, year - 5, year - 1, 0) >= 3, "Yes", "No"),
      staff_total_pct_change_5yr = safe_pct_change(staff_fte_total, unname(staff_fte_lookup[as.character(year - 5)])),
      staff_instructional_fte_pct_change_5yr = safe_pct_change(staff_fte_instructional, unname(staff_instr_fte_lookup[as.character(year - 5)])),
      staff_total_headcount_pct_change_5yr = safe_pct_change(staff_headcount_total, unname(staff_head_lookup[as.character(year - 5)])),
      staff_instructional_headcount_pct_change_5yr = safe_pct_change(staff_headcount_instructional, unname(staff_instr_head_lookup[as.character(year - 5)])),
      revenue_pct_change_5yr_nominal = safe_pct_change(revenue_total, unname(revenue_lookup[as.character(year - 5)])),
      revenue_pct_change_5yr = safe_pct_change(revenue_total_adjusted, unname(revenue_adjusted_lookup[as.character(year - 5)])),
      revenue_pct_change_5yr_adjusted = revenue_pct_change_5yr,
      revenue_decreased_5yr = case_when(is.na(revenue_pct_change_5yr) ~ NA_character_, revenue_pct_change_5yr < 0 ~ "Yes", TRUE ~ "No"),
      revenue_change_1yr_nominal = safe_pct_change(revenue_total, unname(revenue_lookup[as.character(year - 1)])),
      revenue_change_1yr = safe_pct_change(revenue_total_adjusted, unname(revenue_adjusted_lookup[as.character(year - 1)])),
      enrollment_change_1yr = safe_pct_change(enrollment_headcount_total, unname(enroll_lookup[as.character(year - 1)])),
      staff_change_1yr = safe_pct_change(staff_headcount_total, unname(staff_head_lookup[as.character(year - 1)])),
      revenue_10pct_drop_last_3_of_5 = ifelse(count_decline_years(years, df$revenue_total_adjusted, year - 5, year - 1, -10) >= 3, "Yes", "No"),
      losses_last_3_of_5 = ifelse(count_negative_years(years, df$operating_margin, (year - 4):year, 0) >= 3, "Yes", "No"),
      net_tuition_pct_change_5yr_nominal = safe_pct_change(net_tuition_total, unname(net_tuition_lookup[as.character(year - 5)])),
      net_tuition_pct_change_5yr = safe_pct_change(net_tuition_total_adjusted, unname(net_tuition_adjusted_lookup[as.character(year - 5)])),
      net_tuition_pct_change_5yr_adjusted = net_tuition_pct_change_5yr,
      net_tuition_per_fte_change_5yr_nominal = safe_pct_change(net_tuition_per_fte, unname(net_tuition_fte_lookup[as.character(year - 5)])),
      net_tuition_per_fte_change_5yr = safe_pct_change(net_tuition_per_fte_adjusted, unname(net_tuition_fte_adjusted_lookup[as.character(year - 5)])),
      net_tuition_per_fte_change_5yr_adjusted = net_tuition_per_fte_change_5yr,
      yield_pct_change_5yr = safe_pct_change(admissions_yield, unname(yield_lookup[as.character(year - 5)])),
      discount_pct_change_5yr = safe_pct_change(discount_rate, unname(discount_lookup[as.character(year - 5)])),
      government_funding_pct_change_5yr_nominal = safe_pct_change(government_funding_total, unname(government_lookup[as.character(year - 5)])),
      government_funding_pct_change_5yr = safe_pct_change(government_funding_total_adjusted, unname(government_adjusted_lookup[as.character(year - 5)])),
      government_funding_pct_change_5yr_adjusted = government_funding_pct_change_5yr,
      federal_grants_contracts_pell_adjusted_pct_change_5yr_nominal = safe_pct_change(federal_grants_contracts_pell_adjusted, unname(federal_adj_lookup[as.character(year - 5)])),
      federal_grants_contracts_pell_adjusted_pct_change_5yr = safe_pct_change(federal_grants_contracts_pell_adjusted_adjusted, unname(federal_adj_adjusted_lookup[as.character(year - 5)])),
      federal_grants_contracts_pell_adjusted_pct_change_5yr_adjusted = federal_grants_contracts_pell_adjusted_pct_change_5yr,
      state_funding_pct_change_5yr_nominal = safe_pct_change(state_funding, unname(state_lookup_year[as.character(year - 5)])),
      state_funding_pct_change_5yr = safe_pct_change(state_funding_adjusted, unname(state_adjusted_lookup_year[as.character(year - 5)])),
      state_funding_pct_change_5yr_adjusted = state_funding_pct_change_5yr,
      endowment_pct_change_5yr_nominal = safe_pct_change(endowment_value, unname(endowment_lookup[as.character(year - 5)])),
      endowment_pct_change_5yr = safe_pct_change(endowment_value_adjusted, unname(endowment_adjusted_lookup[as.character(year - 5)])),
      endowment_pct_change_5yr_adjusted = endowment_pct_change_5yr,
      international_enrollment_change_10yr = enrollment_nonresident_total - unname(intl_lookup[as.character(year - 10)]),
      international_enrollment_pct_change_10yr = safe_pct_change(enrollment_nonresident_total, unname(intl_lookup[as.character(year - 10)])),
      international_enrollment_increase_10yr = case_when(is.na(unname(intl_lookup[as.character(year - 10)])) | is.na(enrollment_nonresident_total) ~ NA_character_, enrollment_nonresident_total > unname(intl_lookup[as.character(year - 10)]) ~ "Yes", TRUE ~ "No"),
      international_student_count_change_5yr = enrollment_nonresident_total - unname(intl_lookup[as.character(year - 5)]),
      international_enrollment_pct_change_5yr = safe_pct_change(enrollment_nonresident_total, unname(intl_lookup[as.character(year - 5)])),
      international_enrollment_increase_5yr = case_when(is.na(unname(intl_lookup[as.character(year - 5)])) | is.na(enrollment_nonresident_total) ~ NA_character_, enrollment_nonresident_total > unname(intl_lookup[as.character(year - 5)]) ~ "Yes", TRUE ~ "No"),
      transfer_out_rate_bachelor_change_5yr = transfer_out_rate_bachelor - unname(transfer_out_lookup[as.character(year - 5)]),
      transfer_out_rate_bachelor_increase_5yr = case_when(
        is.na(unname(transfer_out_lookup[as.character(year - 5)])) | is.na(transfer_out_rate_bachelor) ~ NA_character_,
        transfer_out_rate_bachelor > unname(transfer_out_lookup[as.character(year - 5)]) ~ "Yes",
        TRUE ~ "No"
      ),
      transfer_out_rate_bachelor_change_10yr = transfer_out_rate_bachelor - unname(transfer_out_lookup[as.character(year - 10)]),
      transfer_out_rate_bachelor_increase_10yr = case_when(
        is.na(unname(transfer_out_lookup[as.character(year - 10)])) | is.na(transfer_out_rate_bachelor) ~ NA_character_,
        transfer_out_rate_bachelor > unname(transfer_out_lookup[as.character(year - 10)]) ~ "Yes",
        TRUE ~ "No"
      ),
      share_grad_students = safe_divide(enrollment_headcount_graduate, enrollment_headcount_total),
      loan_year_latest = dplyr::coalesce(loan_pct_latest$year, loan_count_latest$year, loan_avg_latest$year),
      loan_pct_undergrad_federal_latest = loan_pct_latest$value,
      loan_count_undergrad_federal_latest = loan_count_latest$value,
      loan_avg_undergrad_federal_latest = loan_avg_latest$value,
      federal_loan_pct_most_recent = loan_pct_latest$value,
      federal_loan_count_most_recent = loan_count_latest$value,
      federal_loan_avg_most_recent = loan_avg_latest$value,
      ended_2024_at_loss = case_when(is.na(loss2024) ~ NA_character_, loss2024 < 0 ~ "Yes", TRUE ~ "No"),
      loss_amount_2024 = loss2024,
      loss_amount_2024_adjusted = unname(loss_adjusted_lookup["2024"]),
      loss_years_last_10 = loss_frequency(years, df$loss_amount, year, 10),
      loss_years_last_5 = loss_frequency(years, df$loss_amount, year, 5)
    ) %>%
    ungroup() %>%
    mutate(
      international_students_sentence = case_when(
        is.na(pct_international_all) ~ NA_character_,
        !is.na(pct_international_undergraduate) & !is.na(pct_international_graduate) ~ paste0(round(pct_international_all * 100, 1), "% of students are international. That includes ", round(pct_international_undergraduate * 100, 1), "% of undergraduates and ", round(pct_international_graduate * 100, 1), "% of graduate students."),
        TRUE ~ paste0(round(pct_international_all * 100, 1), "% of students are international.")
      ),
      enrollment_change_sentence = ifelse(is.na(enrollment_pct_change_5yr), NA_character_, paste0("12-month unduplicated headcount changed by ", round(enrollment_pct_change_5yr, 1), "% over the past five years.")),
      revenue_change_sentence = ifelse(is.na(revenue_pct_change_5yr), NA_character_, paste0("Total revenue changed by ", round(revenue_pct_change_5yr, 1), "% over the past five years.")),
      staffing_change_sentence = ifelse(is.na(staff_total_headcount_pct_change_5yr), NA_character_, paste0("Total staff headcount changed by ", round(staff_total_headcount_pct_change_5yr, 1), "% over the past five years.")),
      federal_grants_contracts_dependence_sentence = ifelse(is.na(federal_grants_contracts_pell_adjusted_pct_core_revenue), NA_character_, paste0(round(federal_grants_contracts_pell_adjusted_pct_core_revenue * 100, 1), "% of core revenue came from Pell-adjusted federal grants and contracts.")),
      state_funding_sentence = ifelse(is.na(state_funding_pct_core_revenue), NA_character_, paste0(round(state_funding_pct_core_revenue * 100, 1), "% of core revenue came from state appropriations."))
    )
}

sorted_rows <- prepared_rows %>%
  group_by(unitid) %>%
  group_modify(~ enrich_group(.x)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(
    liquidity_percentile_private_nfp = ifelse(control_label == "Private not-for-profit" & !is.na(liquidity), round(percent_rank(liquidity) * 100, 1), NA_real_),
    leverage_percentile_private_nfp = ifelse(control_label == "Private not-for-profit" & !is.na(leverage), round(percent_rank(leverage) * 100, 1), NA_real_)
  ) %>%
  ungroup() %>%
  arrange(year, unitid)

eligible_unitids_2024 <- sorted_rows %>%
  filter(year == 2024) %>%
  filter(
    is.na(is_active) | is_active %in% c("Yes", "Imputed as active"),
    institution_status %in% c(
      "Active - institution active",
      "New (active) - added during the current year",
      "Restore (active) - restored to the current universe"
    ),
    category == "Degree-granting, primarily baccalaureate or above",
    is.na(all_programs_distance_education) | all_programs_distance_education != "Yes"
  ) %>%
  pull(unitid) %>%
  unique()

sorted_rows <- sorted_rows %>%
  filter(unitid %in% eligible_unitids_2024)

sector_tuition_dependence_benchmarks <- sorted_rows %>%
  group_by(control_label, year) %>%
  summarise(
    sector_median_tuition_dependence_pct = if (all(is.na(tuition_dependence_pct))) NA_real_ else stats::median(tuition_dependence_pct, na.rm = TRUE),
    sector_mean_tuition_dependence_pct = if (all(is.na(tuition_dependence_pct))) NA_real_ else mean(tuition_dependence_pct, na.rm = TRUE),
    sector_tuition_dependence_n = sum(!is.na(tuition_dependence_pct)),
    .groups = "drop"
  )

sector_enrollment_benchmarks <- sorted_rows %>%
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

sorted_rows <- sorted_rows %>%
  left_join(sector_tuition_dependence_benchmarks, by = c("control_label", "year")) %>%
  left_join(sector_enrollment_benchmarks, by = c("control_label", "year")) %>%
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
        "This college gets ",
        round(tuition_dependence_pct, 1),
        "% of its revenue from net tuition, ",
        tolower(case_when(
          abs(tuition_dependence_pct - sector_median_tuition_dependence_pct) < 0.05 ~ "about the same as",
          tuition_dependence_pct > sector_median_tuition_dependence_pct ~ "above",
          tuition_dependence_pct < sector_median_tuition_dependence_pct ~ "below",
          TRUE ~ "compared with"
        )),
        " the median of ",
        round(sector_median_tuition_dependence_pct, 1),
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

looker_columns <- c(
  "unitid","institution_name","institution_unique_name","year","control_label","state","city","sector","level",
  "urbanization","category","institution_status","is_active","hbcu","tribal_college","grad_offering",
  "reporting_model","access_earnings","size","grad_program_mix","undergrad_program_mix","religious_affiliation","fte_12_months",
  "all_programs_distance_education",
  "fte_undergrad","fte_graduate","enrollment_headcount_total","enrollment_headcount_undergrad",
  "enrollment_headcount_graduate","enrollment_nonresident_total","enrollment_nonresident_undergrad",
  "enrollment_nonresident_graduate","pct_international_all","pct_international_undergraduate",
  "pct_international_graduate","share_grad_students",
  "international_student_count_change_5yr","international_enrollment_pct_change_5yr",
  "international_enrollment_increase_5yr","international_enrollment_change_10yr",
  "international_enrollment_increase_10yr","transfer_out_rate_bachelor","transfer_out_rate_bachelor_change_5yr",
  "transfer_out_rate_bachelor_increase_5yr","staff_fte_total","staff_fte_instructional",
  "staff_headcount_total","staff_headcount_instructional","revenue_total","revenue_total_adjusted","expenses_total",
  "expenses_total_adjusted","loss_amount","loss_amount_adjusted",
  "ended_year_at_loss","losses_last_3_of_5","loss_years_last_5","loss_years_last_10","net_tuition_total",
  "net_tuition_total_adjusted","net_tuition_per_fte","net_tuition_per_fte_adjusted","net_tuition_per_fte_change_5yr",
  "net_tuition_per_fte_change_5yr_adjusted","tuition_dependence_ratio","tuition_dependence_pct",
  "sector_median_tuition_dependence_pct","sector_mean_tuition_dependence_pct","sector_tuition_dependence_n",
  "tuition_dependence_vs_sector_median_pct_points","tuition_dependence_relative_to_sector_median",
  "tuition_dependence_vs_sector_median_sentence",
  "discount_rate","discount_pct_change_5yr","federal_grants_contracts_pell_adjusted",
  "federal_grants_contracts_pell_adjusted_adjusted",
  "federal_grants_contracts_pell_adjusted_pct_core_revenue","federal_grants_contracts_pell_adjusted_pct_change_5yr",
  "federal_grants_contracts_pell_adjusted_pct_change_5yr_adjusted",
  "state_approps_percent_core_gasb","gov_grants_fasb","state_revenue_fte_fasb","gov_revenue_fte_fasb",
  "state_funding","state_funding_adjusted","state_funding_pct_core_revenue",
  "state_funding_pct_change_5yr","state_funding_pct_change_5yr_adjusted","endowment_value","endowment_value_adjusted","endowment_assets_per_fte_gasb","endowment_assets_per_fte_fasb","endowment_assets_per_fte",
  "endowment_assets_per_fte_adjusted","endowment_pct_change_5yr","endowment_pct_change_5yr_adjusted","liquidity","liquidity_percentile_private_nfp","leverage",
  "leverage_percentile_private_nfp","loan_year_latest","loan_pct_undergrad_federal_latest",
  "loan_count_undergrad_federal_latest","loan_avg_undergrad_federal_latest","federal_loan_pct_most_recent",
  "federal_loan_count_most_recent","federal_loan_avg_most_recent","enrollment_decline_last_3_of_5",
  "enrollment_pct_change_5yr","enrollment_decreased_5yr","staff_total_pct_change_5yr",
  "staff_instructional_fte_pct_change_5yr","staff_total_headcount_pct_change_5yr",
  "staff_instructional_headcount_pct_change_5yr","revenue_10pct_drop_last_3_of_5","revenue_change_1yr",
  "revenue_change_1yr_nominal","revenue_pct_change_5yr",
  "revenue_pct_change_5yr_nominal",
  "revenue_pct_change_5yr_adjusted","revenue_decreased_5yr","ended_2024_at_loss","loss_amount_2024","loss_amount_2024_adjusted","international_enrollment_pct_change_10yr",
  "sector_enrollment_total_national","sector_enrollment_pct_change_5yr_national",
  "net_tuition_pct_change_5yr_nominal","net_tuition_per_fte_change_5yr_nominal",
  "government_funding_pct_change_5yr_nominal","federal_grants_contracts_pell_adjusted_pct_change_5yr_nominal",
  "state_funding_pct_change_5yr_nominal","endowment_pct_change_5yr_nominal"
)

reporting_columns <- unique(c(
  looker_columns,
  "operating_margin","admissions_yield","yield_pct_change_5yr","core_revenue","core_revenue_adjusted",
  "gov_grants_contracts_pct_core_revenue_gasb","gov_grants_contracts_pct_core_revenue_fasb",
  "state_appropriations_pct_core_revenue_gasb","government_funding_total","government_funding_total_adjusted",
  "government_funding_pct_total_revenue","government_funding_pct_change_5yr","government_funding_pct_change_5yr_adjusted","assets","assets_adjusted","liabilities","liabilities_adjusted",
  "revenue_change_1yr","enrollment_change_1yr","staff_change_1yr","net_tuition_pct_change_5yr","net_tuition_pct_change_5yr_adjusted",
  "enroll_fte_pct_change_5yr","enroll_fte_decline_last_3_of_5","staff_total_headcount_pct_change_5yr",
  "staff_instructional_headcount_pct_change_5yr","international_enrollment_pct_change_10yr",
  "loan_pct_undergrad_federal","loan_avg_undergrad_federal","loan_count_undergrad_federal","federal_funding","federal_funding_adjusted",
  "transfer_out_rate_bachelor","transfer_out_rate_bachelor_change_5yr","transfer_out_rate_bachelor_increase_5yr",
  "transfer_out_rate_bachelor_change_10yr","transfer_out_rate_bachelor_increase_10yr",
  "revenue_change_1yr_nominal","revenue_pct_change_5yr_nominal","net_tuition_pct_change_5yr_nominal",
  "net_tuition_per_fte_change_5yr_nominal","government_funding_pct_change_5yr_nominal",
  "federal_grants_contracts_pell_adjusted_pct_change_5yr_nominal","state_funding_pct_change_5yr_nominal",
  "endowment_pct_change_5yr_nominal",
  "loss_amount_2024","ended_2024_at_loss"
))

looker_ready <- sorted_rows %>% select(any_of(looker_columns))
reporting_ready <- sorted_rows %>% select(any_of(reporting_columns))

Encoding(names(looker_ready)) <- "UTF-8"
Encoding(names(reporting_ready)) <- "UTF-8"
looker_ready[] <- lapply(looker_ready, function(col) {
  if (is.character(col)) enc2utf8(col) else col
})
reporting_ready[] <- lapply(reporting_ready, function(col) {
  if (is.character(col)) enc2utf8(col) else col
})

readr::write_csv(looker_ready, output_path, na = "")
readr::write_csv(reporting_ready, expanded_output_path, na = "")

cat(sprintf("Saved Looker-ready dataset to %s\n", output_path))
cat(sprintf("Saved reporting dataset to %s\n", expanded_output_path))
invisible(list(
  looker_ready = output_path,
  reporting_ready = expanded_output_path
))
}

if (sys.nframe() == 0) {
  result <- main()
  quit(save = "no", status = 0)
}
