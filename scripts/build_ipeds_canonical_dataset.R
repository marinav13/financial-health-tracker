main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args                     <- parse_cli_args(cli_args)
  ipeds                    <- load_ipeds_paths()
  ipeds_layout             <- ipeds$ipeds_layout
  ensure_ipeds_layout_dirs <- ipeds$ensure_ipeds_layout_dirs
  get_arg_value            <- function(flag, default = NULL) get_arg(args, flag, default)

  # This script takes the wide raw IPEDS extract and turns it into the
  # cleaned, decoded canonical dataset used by the website, workbook, and
  # downstream exports.
  setup_r_libs()
  ensure_packages(c("dplyr", "purrr", "readr", "readxl", "stringr", "tidyr"))

  source(file.path(getwd(), "scripts", "shared", "ipeds_helpers.R"))

  default_paths        <- ipeds_layout(root = ".", output_stem = "ipeds_financial_health", start_year = 2014L, end_year = 2024L)
  raw_csv              <- get_arg_value("--raw",             default_paths$raw_csv)
  catalog_csv          <- get_arg_value("--catalog",         default_paths$selected_file_catalog_csv)
  output_csv           <- get_arg_value("--output",          default_paths$canonical_csv)
  expanded_output_csv  <- get_arg_value("--expanded-output", default_paths$dataset_csv)

# Set up paths for the input raw file, the selected-file catalog, and
# the canonical processed dataset output used by the website and workbook.
root <- normalizePath(".", winslash = "/", mustWork = TRUE)
raw_path <- normalizePath(raw_csv, winslash = "/", mustWork = TRUE)
catalog_path <- normalizePath(catalog_csv, winslash = "/", mustWork = TRUE)
output_path <- normalizePath(output_csv, winslash = "/", mustWork = FALSE)
expanded_output_path <- normalizePath(expanded_output_csv, winslash = "/", mustWork = FALSE)
resolved_paths <- ipeds_layout(root = root, output_stem = "ipeds_financial_health", start_year = 2014L, end_year = 2024L)
ensure_ipeds_layout_dirs(resolved_paths)
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(expanded_output_path), recursive = TRUE, showWarnings = FALSE)
aux_root <- resolved_paths$cache_aux_dir
aux_data_root <- resolved_paths$cache_aux_data_dir
aux_extract_root <- resolved_paths$cache_aux_extract_dir


# Utility functions (as_positive_spend, inflate_to_base_year, decode helpers,
# download helpers, time-series helpers) are in scripts/shared/ipeds_helpers.R


# Load the raw tracker build plus the file catalog that tells us where to find
# supplemental IPEDS tables such as EAP, EFFY, SFA, and DRVF.
raw_rows <- suppressMessages(readr::read_csv(
  raw_path,
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
))
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
if (!("F1H02" %in% names(raw_rows))) raw_rows$F1H02 <- NA_character_
if (!("F2H01" %in% names(raw_rows))) raw_rows$F2H01 <- NA_character_
if (!("F2H02" %in% names(raw_rows))) raw_rows$F2H02 <- NA_character_

catalog <- catalog %>%
  mutate(
    year = as.integer(to_num(year)),
    table_name = as.character(table_name)
  )

# Decode 2024 HD / IC / FLAGS labels once and reuse them throughout the build
# so the output contains readable categories rather than numeric codes.
hd2024_dict <- file.path(root, "ipeds", "cache", "downloads", "dict", "HD2024.zip")
ic2024_dict <- file.path(root, "ipeds", "cache", "downloads", "dict", "IC2024.zip")
flags2024_dict <- file.path(root, "ipeds", "cache", "downloads", "dict", "FLAGS2024.zip")
try(ensure_dictionary_archive("HD2024", hd2024_dict), silent = TRUE)
try(ensure_dictionary_archive("IC2024", ic2024_dict), silent = TRUE)
try(ensure_dictionary_archive("FLAGS2024", flags2024_dict), silent = TRUE)
hd_sector_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "SECTOR", aux_extract_root) else character()
hd_level_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "ICLEVEL", aux_extract_root) else character()
hd_act_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "ACT", aux_extract_root) else character()
hd_active_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "CYACTIVE", aux_extract_root) else character()
hd_hbcu_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "HBCU", aux_extract_root) else character()
hd_tribal_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "TRIBAL", aux_extract_root) else character()
hd_grad_offering_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "GROFFER", aux_extract_root) else character()
hd_category_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "INSTCAT", aux_extract_root) else character()
hd_locale_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "LOCALE", aux_extract_root) else character()
hd_access_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "CARNEGIESAEC", aux_extract_root) else character()
hd_size_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "CARNEGIESIZE", aux_extract_root) else character()
hd_ug_mix_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "CARNEGIEAPM", aux_extract_root) else character()
hd_grad_mix_lookup <- if (file.exists(hd2024_dict)) get_frequency_lookup(hd2024_dict, "HD2024", "CARNEGIEGPM", aux_extract_root) else character()
ic_religious_affiliation_lookup <- if (file.exists(ic2024_dict)) get_frequency_lookup(ic2024_dict, "IC2024", "RELAFFIL", aux_extract_root) else character()
flags_form_lookup <- if (file.exists(flags2024_dict)) get_frequency_lookup(flags2024_dict, "FLAGS2024", "FORM_F", aux_extract_root) else character()

load_catalog_table <- function(entry) {
  table_name <- entry$table_name[[1]]
  year <- entry$year[[1]]
  shared_zip_path <- file.path(root, "ipeds", "cache", "downloads", "data", paste0(table_name, ".zip"))
  extract_path <- file.path(aux_extract_root, table_name)
  legacy_extract_path <- file.path(root, "ipeds", "aux", "extracted", table_name)
  if (dir.exists(extract_path) && length(list.files(extract_path, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)) > 0) {
    csv_file <- find_first_file(extract_path, "\\.csv$")
    if (!is.na(csv_file)) {
      return(list(
        year = year,
        table_name = table_name,
        data = suppressMessages(readr::read_csv(csv_file, show_col_types = FALSE, guess_max = 100000))
      ))
    }
  }
  if (dir.exists(legacy_extract_path) && length(list.files(legacy_extract_path, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)) > 0) {
    csv_file <- find_first_file(legacy_extract_path, "\\.csv$")
    if (!is.na(csv_file)) {
      return(list(
        year = year,
        table_name = table_name,
        data = suppressMessages(readr::read_csv(csv_file, show_col_types = FALSE, guess_max = 100000))
      ))
    }
  }

  zip_candidates <- c(
    shared_zip_path,
    file.path(aux_data_root, paste0(table_name, ".zip")),
    file.path(root, "ipeds", "aux", "data", paste0(table_name, ".zip"))
  )
  zip_path <- zip_candidates[file.exists(zip_candidates)][1]
  if (length(zip_path) == 0 || is.na(zip_path)) {
    download_ok <- tryCatch({
      zip_path <- shared_zip_path
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

# Pull supporting IPEDS tables into year-by-year lookup tables keyed by UNITID.
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
  zip_path <- file.path(root, "ipeds", "cache", "downloads", "data", paste0(table_name, ".zip"))
  extract_path <- file.path(root, "ipeds", "cache", "downloads", "extracted", paste0("data_", table_name))
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
  gr_zip <- file.path(root, "ipeds", "cache", "downloads", "data", paste0("GR", yr, ".zip"))
  gr_extract <- file.path(root, "ipeds", "cache", "downloads", "extracted", paste0("data_GR", yr))
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

load_finance_research_year <- function(year, year_catalog) {
  aliases <- list(
    F1A = year_catalog %>% filter(str_detect(table_name, "^F\\d{4}_F1A$")) %>% slice(1) %>% pull(table_name) %||% NA_character_,
    F2 = year_catalog %>% filter(str_detect(table_name, "^F\\d{4}_F2$")) %>% slice(1) %>% pull(table_name) %||% NA_character_,
    F3 = year_catalog %>% filter(str_detect(table_name, "^F\\d{4}_F3$")) %>% slice(1) %>% pull(table_name) %||% NA_character_,
    DRVF = year_catalog %>% filter(str_detect(table_name, "^DRVF\\d{4}$")) %>% slice(1) %>% pull(table_name) %||% NA_character_
  )
  field_map <- list(
    F1A = c("UNITID", "F1C021"),
    F2 = c("UNITID", "F2E021"),
    F3 = c("UNITID", "F3E02A1"),
    DRVF = c("UNITID", "F1COREXP", "F2COREXP", "F3COREXP")
  )

  purrr::imap_dfr(aliases, function(table_name, alias) {
    if (is.na(table_name) || identical(table_name, "")) return(tibble::tibble())
  data_folder <- file.path(root, "ipeds", "cache", "downloads", "extracted", paste0("data_", table_name))
    csv_path <- find_first_file(data_folder, "\\.csv$")
    required_fields <- field_map[[alias]]
    if (is.na(csv_path)) {
      return(tibble::tibble())
    }
    header <- suppressMessages(readr::read_csv(csv_path, show_col_types = FALSE, n_max = 0))
    available_fields <- intersect(required_fields, names(header))
    if (!("UNITID" %in% available_fields) || length(available_fields) <= 1) {
      return(tibble::tibble())
    }
    tbl <- suppressMessages(
      readr::read_csv(csv_path, show_col_types = FALSE, col_select = any_of(available_fields))
    ) %>%
      mutate(unitid = as.character(UNITID), year = as.integer(year))

    if (identical(alias, "DRVF")) {
      tbl %>%
        transmute(
          unitid,
          year,
          core_expenses = dplyr::coalesce(
            if ("F1COREXP" %in% names(.)) to_num(F1COREXP) else NA_real_,
            if ("F2COREXP" %in% names(.)) to_num(F2COREXP) else NA_real_,
            if ("F3COREXP" %in% names(.)) to_num(F3COREXP) else NA_real_
          )
        )
    } else {
      value_field <- setdiff(available_fields, "UNITID")[1]
      tbl %>%
        transmute(
          unitid,
          year,
          research_expense = to_num(.data[[value_field]])
        )
    }
  }) %>%
    group_by(unitid, year) %>%
    summarise(
      research_expense = if ("research_expense" %in% names(.)) {
        dplyr::coalesce(research_expense[which(!is.na(research_expense))[1]], NA_real_)
      } else {
        NA_real_
      },
      core_expenses = if ("core_expenses" %in% names(.)) {
        dplyr::coalesce(core_expenses[which(!is.na(core_expenses))[1]], NA_real_)
      } else {
        NA_real_
      },
      .groups = "drop"
    )
}

eap_long <- bind_aux_year(eap_by_year_unit)
effy_long <- bind_aux_year(effy_by_year_unit)
sfa_long <- bind_aux_year(sfa_by_year_unit)
drvf_long <- bind_aux_year(drvf_by_year_unit)
transfer_out_long <- bind_aux_year(transfer_out_by_year_unit)
finance_research_long <- purrr::map_dfr(sort(unique(catalog$year)), function(yr) {
  year_catalog <- catalog %>% filter(year == yr)
  if (nrow(year_catalog) == 0) {
    return(tibble::tibble(unitid = character(), year = integer(), research_expense = numeric(), core_expenses = numeric()))
  }
  load_finance_research_year(yr, year_catalog)
})

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
  left_join(transfer_out_long, by = c("unitid", "year")) %>%
  left_join(finance_research_long, by = c("unitid", "year"))

# Freeze the latest institution name/city/state across all years so the site
# uses one consistent display name for each school.
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

# Older raw years can be sparse on HD metadata. Carry the latest known
# institution attributes across the full time series for each UNITID so the
# canonical build applies one consistent cohort definition to every year.
metadata_fill_cols <- c(
  "institution_name", "city", "state", "zip", "county", "longitude", "latitude",
  "status", "date_closed", "is_active", "multi_institution_campus_org",
  "id_number_multi", "name_multi", "opeid", "region", "hbcu", "tribal_college",
  "sector", "level", "control", "degree_granting_status", "highest_degree",
  "category", "grad_offering", "urbanization", "size", "undergrad_program_mix",
  "grad_program_mix", "control_or_affiliation", "religious_affiliation",
  "has_full_time_first_time_undergrad", "all_programs_distance_education",
  "reporting_model", "pell_accounting_method"
)
metadata_fill_cols <- intersect(metadata_fill_cols, names(raw_enriched))
if (length(metadata_fill_cols) > 0) {
  raw_enriched <- raw_enriched %>%
    arrange(unitid, year) %>%
    group_by(unitid) %>%
    tidyr::fill(dplyr::all_of(metadata_fill_cols), .direction = "updown") %>%
    ungroup()
}

# Keep a lightweight unitid/year backfill table from the auxiliary joins so the
# canonical fallback path can still restore enrollment, staffing, and loan
# fields even when older source files have gaps that can be filled from nearby years.
aux_backfill_rows <- raw_enriched %>%
  transmute(
    unitid = as.character(unitid),
    year = as.integer(year),
    enrollment_headcount_total = to_num(enrollment_headcount_total),
    enrollment_headcount_undergrad = to_num(enrollment_headcount_undergrad),
    enrollment_headcount_graduate = to_num(enrollment_headcount_graduate),
    enrollment_nonresident_total = to_num(enrollment_nonresident_total),
    enrollment_nonresident_undergrad = to_num(enrollment_nonresident_undergrad),
    enrollment_nonresident_graduate = to_num(enrollment_nonresident_graduate),
    staff_headcount_total = to_num(staff_headcount_total),
    staff_headcount_instructional = to_num(staff_headcount_instructional),
    research_expense = dplyr::coalesce(
      if ("research_expenses_total_gasb" %in% names(.)) to_num(research_expenses_total_gasb) else NA_real_,
      if ("research_expenses_total_fasb" %in% names(.)) to_num(research_expenses_total_fasb) else NA_real_,
      if ("research_expenses_total_pfp" %in% names(.)) to_num(research_expenses_total_pfp) else NA_real_,
      if ("research_expense" %in% names(.)) to_num(research_expense) else NA_real_,
      if ("F1C021" %in% names(.)) to_num(F1C021) else NA_real_,
      if ("F2E021" %in% names(.)) to_num(F2E021) else NA_real_,
      if ("F3E02A1" %in% names(.)) to_num(F3E02A1) else NA_real_
    ),
    core_expenses = dplyr::coalesce(
      if ("core_expenses_gasb" %in% names(.)) to_num(core_expenses_gasb) else NA_real_,
      if ("core_expenses_fasb" %in% names(.)) to_num(core_expenses_fasb) else NA_real_,
      if ("core_expenses_pfp" %in% names(.)) to_num(core_expenses_pfp) else NA_real_,
      if ("core_expenses" %in% names(.)) to_num(core_expenses) else NA_real_,
      if ("F1COREXP" %in% names(.)) to_num(F1COREXP) else NA_real_,
      if ("F2COREXP" %in% names(.)) to_num(F2COREXP) else NA_real_,
      if ("F3COREXP" %in% names(.)) to_num(F3COREXP) else NA_real_
    ),
    loan_pct_undergrad_federal = if ("loan_pct_undergrad_federal" %in% names(.)) to_num(.data$loan_pct_undergrad_federal) else NA_real_,
    loan_avg_undergrad_federal = if ("loan_avg_undergrad_federal" %in% names(.)) to_num(.data$loan_avg_undergrad_federal) else NA_real_,
    loan_count_undergrad_federal = if ("loan_count_undergrad_federal" %in% names(.)) to_num(.data$loan_count_undergrad_federal) else NA_real_
  ) %>%
  distinct()

decode_lookups <- list(
  hd_sector_lookup = hd_sector_lookup,
  hd_level_lookup = hd_level_lookup,
  hd_act_lookup = hd_act_lookup,
  hd_active_lookup = hd_active_lookup,
  hd_hbcu_lookup = hd_hbcu_lookup,
  hd_tribal_lookup = hd_tribal_lookup,
  hd_grad_offering_lookup = hd_grad_offering_lookup,
  hd_category_lookup = hd_category_lookup,
  hd_locale_lookup = hd_locale_lookup,
  hd_access_lookup = hd_access_lookup,
  hd_size_lookup = hd_size_lookup,
  hd_ug_mix_lookup = hd_ug_mix_lookup,
  hd_grad_mix_lookup = hd_grad_mix_lookup,
  ic_religious_affiliation_lookup = ic_religious_affiliation_lookup,
  flags_form_lookup = flags_form_lookup
)

prepared_rows <- purrr::map_dfr(seq_len(nrow(raw_enriched)), function(i) {
  build_canonical_ipeds_row(raw_enriched[i, , drop = FALSE], decode_lookups = decode_lookups)
})


effy_backfill <- effy_long %>%
  {
    required_cols <- c(
      "enrollment_headcount_total",
      "enrollment_headcount_undergrad",
      "enrollment_headcount_graduate",
      "enrollment_nonresident_total",
      "enrollment_nonresident_undergrad",
      "enrollment_nonresident_graduate"
    )
    missing_cols <- setdiff(required_cols, names(.))
    for (nm in missing_cols) .[[nm]] <- NA_real_
    .
  } %>%
  mutate(
    enrollment_headcount_total = dplyr::coalesce(.data$enrollment_headcount_total, NA_real_),
    enrollment_headcount_undergrad = dplyr::coalesce(.data$enrollment_headcount_undergrad, NA_real_),
    enrollment_headcount_graduate = dplyr::coalesce(.data$enrollment_headcount_graduate, NA_real_),
    enrollment_nonresident_total = dplyr::coalesce(.data$enrollment_nonresident_total, NA_real_),
    enrollment_nonresident_undergrad = dplyr::coalesce(.data$enrollment_nonresident_undergrad, NA_real_),
    enrollment_nonresident_graduate = dplyr::coalesce(.data$enrollment_nonresident_graduate, NA_real_)
  ) %>%
  select(
    any_of(c("unitid", "year")),
    enrollment_headcount_total_bf = enrollment_headcount_total,
    enrollment_headcount_undergrad_bf = enrollment_headcount_undergrad,
    enrollment_headcount_graduate_bf = enrollment_headcount_graduate,
    enrollment_nonresident_total_bf = enrollment_nonresident_total,
    enrollment_nonresident_undergrad_bf = enrollment_nonresident_undergrad,
    enrollment_nonresident_graduate_bf = enrollment_nonresident_graduate
  )

eap_backfill <- eap_long %>%
  {
    required_cols <- c("staff_headcount_total", "staff_headcount_instructional")
    missing_cols <- setdiff(required_cols, names(.))
    for (nm in missing_cols) .[[nm]] <- NA_real_
    .
  } %>%
  mutate(
    staff_headcount_total = dplyr::coalesce(.data$staff_headcount_total, NA_real_),
    staff_headcount_instructional = dplyr::coalesce(.data$staff_headcount_instructional, NA_real_)
  ) %>%
  select(
    any_of(c("unitid", "year")),
    staff_headcount_total_bf = staff_headcount_total,
    staff_headcount_instructional_bf = staff_headcount_instructional
  )

# Fill gaps in a few enrollment/staff fields from the auxiliary EFFY and EAP
# tables after the main row build. These joins are purely backfills.
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
    staff_headcount_instructional = coalesce(staff_headcount_instructional, staff_headcount_instructional_bf),
    pct_international_all = safe_divide(enrollment_nonresident_total, enrollment_headcount_total),
    pct_international_undergraduate = safe_divide(enrollment_nonresident_undergrad, enrollment_headcount_undergrad),
    pct_international_graduate = safe_divide(enrollment_nonresident_graduate, enrollment_headcount_graduate)
  ) %>%
  select(-ends_with("_bf"))

for (nm in c("staff_headcount_total", "staff_headcount_instructional")) {
  if (!(nm %in% names(prepared_rows))) prepared_rows[[nm]] <- NA_real_
}

# enrich_group() is defined in scripts/shared/ipeds_helpers.R

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


# The canonical dataset now depends only on the refactored IPEDS collector.
# Fail fast if the raw extract does not span the requested multi-year window.
if (length(unique(sorted_rows$year)) <= 1) {
  stop(
    "The collected IPEDS raw dataset only contains a single year. ",
    "Fix the collector input/extraction step instead of falling back to a legacy dataset."
  )
}

# The public-facing finance tracker is defined by the 2024 cohort. Once we know
# which 2024 schools meet that universe rule, we keep their full time series.
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

sorted_rows <- apply_sector_benchmarks(sorted_rows)

# These final column lists define the canonical IPEDS dataset used by the
# website/workbook plus a broader extended export for analysis.
canonical_columns <- c(
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
  "transfer_out_rate_bachelor_increase_5yr","research_expense","research_expense_per_fte","research_expense_pct_core_expenses","core_expenses",
  "sector_research_spending_n","sector_research_spending_positive_n","sector_research_spending_reporting_share_pct","sector_median_research_expense_per_fte_positive","staff_fte_total","staff_fte_instructional",
  "students_per_instructional_staff_fte","sector_median_students_per_instructional_staff_fte",
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
  "state_funding_pct_change_5yr","state_funding_pct_change_5yr_adjusted","endowment_value","endowment_value_adjusted","endowment_spending_current_use","endowment_spending_current_use_adjusted","endowment_spending_current_use_pct_core_revenue","endowment_assets_per_fte_gasb","endowment_assets_per_fte_fasb","endowment_assets_per_fte",
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

extended_columns <- unique(c(
  canonical_columns,
  "operating_margin","admissions_yield","yield_pct_change_5yr","core_revenue","core_revenue_adjusted",
  "gov_grants_contracts_pct_core_revenue_gasb","gov_grants_contracts_pct_core_revenue_fasb",
  "state_appropriations_pct_core_revenue_gasb","government_funding_total","government_funding_total_adjusted",
  "government_funding_pct_total_revenue","government_funding_pct_change_5yr","government_funding_pct_change_5yr_adjusted","assets","assets_adjusted","liabilities","liabilities_adjusted",
  "revenue_change_1yr","enrollment_change_1yr","staff_change_1yr","net_tuition_pct_change_5yr","net_tuition_pct_change_5yr_adjusted",
  "enroll_fte_pct_change_5yr","enroll_fte_decline_last_3_of_5","staff_total_headcount_pct_change_5yr",
  "staff_instructional_headcount_pct_change_5yr","international_enrollment_pct_change_10yr",
  "research_expense","research_expense_per_fte","research_expense_pct_core_expenses","core_expenses","sector_research_spending_n","sector_research_spending_positive_n","sector_research_spending_reporting_share_pct","sector_median_research_expense_per_fte_positive",
  "loan_pct_undergrad_federal","loan_avg_undergrad_federal","loan_count_undergrad_federal","federal_funding","federal_funding_adjusted",
  "transfer_out_rate_bachelor","transfer_out_rate_bachelor_change_5yr","transfer_out_rate_bachelor_increase_5yr",
  "transfer_out_rate_bachelor_change_10yr","transfer_out_rate_bachelor_increase_10yr",
  "revenue_change_1yr_nominal","revenue_pct_change_5yr_nominal","net_tuition_pct_change_5yr_nominal",
  "net_tuition_per_fte_change_5yr_nominal","government_funding_pct_change_5yr_nominal",
  "federal_grants_contracts_pell_adjusted_pct_change_5yr_nominal","state_funding_pct_change_5yr_nominal",
  "endowment_pct_change_5yr_nominal",
  "loss_amount_2024","ended_2024_at_loss"
))

canonical_dataset <- sorted_rows %>% select(any_of(canonical_columns))
extended_dataset <- sorted_rows %>% select(any_of(extended_columns))

Encoding(names(canonical_dataset)) <- "UTF-8"
Encoding(names(extended_dataset)) <- "UTF-8"
canonical_dataset[] <- lapply(canonical_dataset, function(col) {
  if (is.character(col)) enc2utf8(col) else col
})
extended_dataset[] <- lapply(extended_dataset, function(col) {
  if (is.character(col)) enc2utf8(col) else col
})

readr::write_csv(canonical_dataset, output_path, na = "")
if (normalizePath(output_path, winslash = "/", mustWork = FALSE) != normalizePath(expanded_output_path, winslash = "/", mustWork = FALSE)) {
  readr::write_csv(extended_dataset, expanded_output_path, na = "")
}

cat(sprintf("Saved canonical IPEDS dataset to %s\n", output_path))
if (normalizePath(output_path, winslash = "/", mustWork = FALSE) != normalizePath(expanded_output_path, winslash = "/", mustWork = FALSE)) {
  cat(sprintf("Saved extended IPEDS dataset to %s\n", expanded_output_path))
}
invisible(list(
  canonical_dataset = output_path,
  extended_dataset = expanded_output_path
))
}

if (sys.nframe() == 0) {
  result <- main()
  quit(save = "no", status = 0)
}
