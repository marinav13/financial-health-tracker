main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args                     <- parse_cli_args(cli_args)
  ipeds                    <- load_ipeds_paths()
  ipeds_layout             <- ipeds$ipeds_layout
  ensure_ipeds_layout_dirs <- ipeds$ensure_ipeds_layout_dirs
  get_arg_value            <- function(flag, default = NULL) get_arg(args, flag, default)

  # This script is the single source-of-truth IPEDS collector. It downloads only
  # the annual source tables and dictionaries we need, resolves the requested
  # variables year by year, and writes one wide raw-but-decoded institution-year
  # dataset for the canonical build.
  setup_r_libs()
  ensure_packages(c("dplyr", "purrr", "readr", "readxl", "stringr", "tidyr", "xml2"))
  source(file.path(getwd(), "scripts", "shared", "ipeds_helpers.R"))
  source(file.path(getwd(), "scripts", "shared", "ipeds_collector_helpers.R"))
  # Shared: download_if_missing, expand_zip_if_missing, find_first_file,
  # sum_if_any are in scripts/shared/ipeds_helpers.R
  # Collector: get_eap_fte, get_eap_headcount, lookup_row, lookup_number,
  # lookup_string, rebuild_core_revenue_* are in scripts/shared/ipeds_collector_helpers.R

  start_year    <- as.integer(get_arg_value("--start-year",    "2014"))
  end_year      <- as.integer(get_arg_value("--end-year",      "2024"))
  output_stem   <- get_arg_value("--output-stem", "ipeds_financial_health")
  # Pass --force-rebuild TRUE to ignore per-year caches and rebuild everything
  force_rebuild <- identical(get_arg_value("--force-rebuild", "FALSE"), "TRUE")

excluded_state_codes <- c("PR", "GU", "VI", "AS", "MP", "FM", "MH", "PW")

paths <- ipeds_layout(
  root = ".",
  output_stem = output_stem,
  start_year = start_year,
  end_year = end_year
)

root <- paths$repo_root
ipeds_root <- paths$ipeds_root
raw_root <- paths$raw_dir
manifest_root <- paths$manifests_dir
cache_root <- paths$cache_dir
download_root <- paths$cache_downloads_dir
data_root <- paths$cache_data_dir
dict_root <- paths$cache_dict_dir
extract_root <- paths$cache_extract_dir
year_cache_dir <- paths$cache_year_dir
catalog_html <- paths$catalog_html
catalog_csv <- paths$selected_file_catalog_csv
dataset_csv <- paths$raw_csv
resolution_audit_csv <- paths$field_resolution_audit_csv
legacy_catalog_html <- paths$legacy_catalog_html

ensure_ipeds_layout_dirs(paths)

# ── helpers ────────────────────────────────────────────────────────────────

# Collector-specific helpers for HTML catalog parsing and field access.
# (download_if_missing, expand_zip_if_missing, find_first_file are in ipeds_helpers.R)

html_decode_clean <- function(x) {
  x <- iconv(as.character(x %||% ""), from = "", to = "UTF-8", sub = "")
  x <- gsub("<br\\s*/?>", " ", x, perl = TRUE)
  x <- gsub("<[^>]+>", "", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

get_catalog_rows <- function(html_path) {
  html <- paste(readLines(html_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  pattern <- paste0(
    '<tr class="idc_gridviewrow">\\s*<td[^>]*>(\\d{4})</td><td[^>]*>(.*?)</td><td[^>]*>(.*?)</td>',
    '<td[^>]*><a href="([^"]+)">([^<]+)</a></td><td[^>]*><a href="([^"]+)">([^<]+)</a></td>',
    '<td[^>]*>.*?</td><td[^>]*><a href="([^"]+)">Dictionary</a></td>'
  )
  m <- stringr::str_match_all(html, pattern)[[1]]
  if (nrow(m) == 0) return(tibble::tibble())
  base_url <- "https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=-1&sid=5588c647-c6be-4540-b2d1-283f6c31aee7&rtid=1"
  tibble::tibble(
    year = as.integer(m[, 2]),
    survey = html_decode_clean(m[, 3]),
    description = html_decode_clean(m[, 4]),
    table_name = html_decode_clean(m[, 6]),
    data_url = vapply(m[, 5], function(u) as.character(xml2::url_absolute(u, base_url)), character(1)),
    dictionary_url = vapply(m[, 8], function(u) as.character(xml2::url_absolute(u, base_url)), character(1))
  )
}

get_varlist <- function(dictionary_archive, table_name) {
  expanded  <- file.path(extract_root, paste0("dict_", table_name))
  expand_zip_if_missing(dictionary_archive, expanded)
  xlsx_path <- find_first_file(expanded, "\\.xlsx$")
  if (is.na(xlsx_path)) return(tibble::tibble())
  sheets    <- readxl::excel_sheets(xlsx_path)
  var_sheet <- sheets[tolower(sheets) == "varlist"][1]
  if (length(var_sheet) == 0 || is.na(var_sheet)) return(tibble::tibble())
  raw <- suppressMessages(readxl::read_excel(xlsx_path, sheet = var_sheet, col_names = FALSE, .name_repair = "minimal"))
  if (ncol(raw) == 0) return(tibble::tibble())
  bad_names <- is.na(names(raw)) | names(raw) == ""
  names(raw)[bad_names] <- paste0("X", which(bad_names))
  names(raw)[seq_len(min(7, ncol(raw)))] <- c("A", "B", "C", "D", "E", "F", "G")[seq_len(min(7, ncol(raw)))]
  raw %>%
    mutate(across(everything(), as.character)) %>%
    filter("A" %in% names(.), "B" %in% names(.)) %>%
    filter(str_detect(A, "^\\d+$"), !is.na(B), B != "") %>%
    transmute(
      var_number     = A,
      var_name       = B,
      data_type      = if ("C" %in% names(raw)) C else NA_character_,
      field_width    = if ("D" %in% names(raw)) D else NA_character_,
      format         = if ("E" %in% names(raw)) E else NA_character_,
      imputation_var = if ("F" %in% names(raw)) F else NA_character_,
      var_title      = if ("G" %in% names(raw)) G else NA_character_
    )
}

find_var_match <- function(varlist, patterns) {
  for (pattern in patterns) {
    hit <- varlist %>% filter(str_detect(var_title, pattern) | str_detect(var_name, pattern)) %>% slice(1)
    if (nrow(hit) > 0) {
      return(list(
        var_name        = hit$var_name[[1]],
        var_title       = hit$var_title[[1]] %||% NA_character_,
        matched_pattern = pattern
      ))
    }
  }
  list(var_name = NA_character_, var_title = NA_character_, matched_pattern = NA_character_)
}

resolve_year_table_aliases <- function(year_catalog, table_alias_patterns) {
  lapply(table_alias_patterns, function(pat) {
    hit <- year_catalog[stringr::str_detect(year_catalog$table_name, pat), , drop = FALSE]
    if (nrow(hit) == 0) NA_character_ else hit$table_name[[1L]]
  })
}

load_year_tables_and_dictionaries <- function(aliases, year_catalog, data_root, dict_root, extract_root) {
  data_tables <- list()
  dictionaries <- list()

  for (alias in names(aliases)) {
    table_name <- aliases[[alias]]
    if (is.na(table_name) || identical(table_name, "")) next

    entry <- year_catalog %>% filter(table_name == !!table_name) %>% slice(1)
    data_archive <- file.path(data_root, paste0(table_name, ".zip"))
    data_folder <- file.path(extract_root, paste0("data_", table_name))
    dict_archive <- file.path(dict_root, paste0(table_name, ".zip"))

    download_if_missing(entry$data_url[[1]], data_archive)
    expand_zip_if_missing(data_archive, data_folder)

    dict_ok <- tryCatch({
      download_if_missing(entry$dictionary_url[[1]], dict_archive)
      TRUE
    }, error = function(e) FALSE)

    dictionaries[[alias]] <- if (dict_ok && file.exists(dict_archive)) {
      get_varlist(dict_archive, table_name)
    } else {
      tibble::tibble()
    }

    csv_file <- find_first_file(data_folder, "\\.csv$")
    if (!is.na(csv_file)) {
      tbl <- suppressMessages(readr::read_csv(csv_file, show_col_types = FALSE, guess_max = 2000))
      names(tbl) <- toupper(names(tbl))
      if ("UNITID" %in% names(tbl)) {
        data_tables[[alias]] <- tbl %>% mutate(UNITID = as.character(UNITID))
      }
    }
  }

  list(data_tables = data_tables, dictionaries = dictionaries)
}

resolve_year_fields <- function(field_specs, dictionaries, data_tables, exact_field_overrides, aliases, year) {
  resolved_fields <- list()
  resolution_audit <- list()

  for (spec in field_specs) {
    alias <- spec$table
    match_info <- list(var_name = NA_character_, var_title = NA_character_, matched_pattern = NA_character_)
    if (!is.null(dictionaries[[alias]]) && nrow(dictionaries[[alias]]) > 0) {
      match_info <- find_var_match(dictionaries[[alias]], spec$patterns)
    }

    exact_override <- exact_field_overrides[[spec$output]] %||% NA_character_
    if ((is.na(match_info$var_name) || identical(match_info$var_name, "")) &&
        !is.na(exact_override) &&
        exact_override %in% names(data_tables[[alias]] %||% data.frame())) {
      match_info <- list(
        var_name = exact_override,
        var_title = NA_character_,
        matched_pattern = "exact_code_override"
      )
    }

    resolved_fields[[spec$output]] <- match_info$var_name
    resolution_audit[[length(resolution_audit) + 1L]] <- tibble::tibble(
      year = year,
      table_alias = alias,
      table_name = aliases[[alias]] %||% alias,
      output = spec$output,
      resolved_var_name = match_info$var_name,
      resolved_var_title = match_info$var_title,
      matched_pattern = match_info$matched_pattern
    )
  }

  list(
    resolved_fields = resolved_fields,
    resolution_audit = dplyr::bind_rows(resolution_audit)
  )
}

build_eap_indexes <- function(eap_raw) {
  indexes <- list(total = list(), instructional = list())

  if (is.null(eap_raw) || !all(c("OCCUPCAT", "EAPCAT", "FACSTAT") %in% names(eap_raw))) {
    return(indexes)
  }

  eap_100 <- eap_raw[eap_raw$OCCUPCAT == "100" &
                       eap_raw$EAPCAT == "10000" &
                       eap_raw$FACSTAT == "0", ]
  eap_210 <- eap_raw[eap_raw$OCCUPCAT == "210" &
                       eap_raw$EAPCAT == "21000" &
                       eap_raw$FACSTAT == "0", ]

  if (nrow(eap_100) > 0) indexes$total <- split(eap_100, eap_100$UNITID)
  if (nrow(eap_210) > 0) indexes$instructional <- split(eap_210, eap_210$UNITID)
  indexes
}

build_effy_index <- function(effy_raw) {
  if (is.null(effy_raw) || !all(c("UNITID", "EFYTOTLT", "EFYNRALT") %in% names(effy_raw))) {
    return(list())
  }

  effy_prepped <- effy_raw %>% mutate(UNITID = as.character(UNITID))
  if ("EFFYALEV" %in% names(effy_prepped)) {
    effy_prepped <- effy_prepped %>% mutate(level_code = as.character(EFFYALEV))
  } else if ("LSTUDY" %in% names(effy_prepped)) {
    effy_prepped <- effy_prepped %>% mutate(level_code = dplyr::case_when(
      as.character(LSTUDY) == "999" ~ "1",
      as.character(LSTUDY) == "1" ~ "2",
      as.character(LSTUDY) == "3" ~ "12",
      TRUE ~ as.character(LSTUDY)
    ))
  } else {
    effy_prepped <- effy_prepped %>% mutate(level_code = NA_character_)
  }

  effy_lookup <- effy_prepped %>%
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

  if (nrow(effy_lookup) == 0) return(list())
  split(effy_lookup, effy_lookup$UNITID)
}

# safe_divide() is in utils.R; sum_if_any() is in ipeds_helpers.R

# ── catalog ────────────────────────────────────────────────────────────────
# Build the filtered IPEDS file catalog that drives all later downloads and
# dictionary lookups for the requested year range.
if (!file.exists(catalog_html)) {
  if (file.exists(legacy_catalog_html)) {
    file.copy(legacy_catalog_html, catalog_html, overwrite = TRUE)
  }
}

if (!file.exists(catalog_html)) {
  download_if_missing(
    "https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=-1&sid=5588c647-c6be-4540-b2d1-283f6c31aee7&rtid=1",
    catalog_html
  )
}

target_table_regex <- c(
  "^HD\\d{4}$", "^IC\\d{4}$", "^EFFY\\d{4}$", "^EFIA\\d{4}$", "^FLAGS\\d{4}$", "^COST1_\\d{4}$",
  "^SAL\\d{4}_IS$", "^SAL\\d{4}_NIS$", "^EAP\\d{4}$", "^F\\d{4}_F1A$", "^F\\d{4}_F2$", "^F\\d{4}_F3$",
  "^DRVCOST\\d{4}$", "^DRVEF\\d{4}$", "^DRVEF12\\d{4}$", "^DRVF\\d{4}$", "^DRVHR\\d{4}$",
  "^DRVADM\\d{4}$", "^DRVOM\\d{4}$", "^DRVGR\\d{4}$", "^ADM\\d{4}$", "^SFA\\d{4}$"
)

catalog <- get_catalog_rows(catalog_html) %>%
  filter(year >= start_year, year <= end_year) %>%
  filter(purrr::map_lgl(table_name, ~ any(stringr::str_detect(.x, target_table_regex)))) %>%
  arrange(year, table_name)

# Manual patches for entries that sometimes go missing from the catalog.
# To add a future patch: append a row to this tibble.  No other code changes.
catalog_patches <- tibble::tibble(
  year           = c(2020L,                2020L),
  survey         = "12-Month Enrollment",
  description    = c(
    "12-month unduplicated headcount by race/ethnicity, gender and level of student: 2019-20",
    "12-month full-time equivalent enrollment and instructional activity: 2019-20"
  ),
  table_name     = c("EFFY2020",           "EFIA2020"),
  data_url       = c(
    "https://nces.ed.gov/ipeds/datacenter/data/EFFY2020.zip",
    "https://nces.ed.gov/ipeds/datacenter/data/EFIA2020.zip"
  ),
  dictionary_url = c(
    "https://nces.ed.gov/ipeds/datacenter/EFFY2020_STATA",
    "https://nces.ed.gov/ipeds/datacenter/EFIA2020_STATA"
  )
)

patches_needed <- catalog_patches[
  catalog_patches$year >= start_year &
    catalog_patches$year <= end_year &
    !catalog_patches$table_name %in% catalog$table_name, ,
  drop = FALSE
]
if (nrow(patches_needed) > 0) {
  catalog <- dplyr::bind_rows(catalog, patches_needed)
}

catalog <- arrange(catalog, year, table_name)
readr::write_csv(catalog, catalog_csv, na = "")
catalog_by_year <- split(catalog, catalog$year)

# ── field specs ────────────────────────────────────────────────────────────
# Define the output fields we want in the raw tracker and the dictionary
# patterns used to resolve each variable name year by year.
field_specs <- list(
  list(output = "institution_name", table = "HD", patterns = c("^Institution \\(entity\\) name$", "^Institution name$")),
  list(output = "city", table = "HD", patterns = c("^City location of institution$")),
  list(output = "state", table = "HD", patterns = c("^State abbreviation$")),
  list(output = "zip", table = "HD", patterns = c("^ZIP code$", "^Zip code$")),
  list(output = "county", table = "HD", patterns = c("^County name$")),
  list(output = "longitude", table = "HD", patterns = c("^Longitude location of institution$")),
  list(output = "latitude", table = "HD", patterns = c("^Latitude location of institution$")),
  list(output = "status", table = "HD", patterns = c("^Status of institution$")),
  list(output = "date_closed", table = "HD", patterns = c("^Date institution closed$")),
  list(output = "is_active", table = "HD", patterns = c("^Institution is active in current year$")),
  list(output = "multi_institution_campus_org", table = "HD", patterns = c("^Multi-institution or multi-campus organization$")),
  list(output = "id_number_multi", table = "HD", patterns = c("^Identification number of multi-institution or multi-campus organization$")),
  list(output = "name_multi", table = "HD", patterns = c("^Name of multi-institution or multi-campus organization$")),
  list(output = "opeid", table = "HD", patterns = c("^Office of Postsecondary Education \\(OPE\\) ID Number$")),
  list(output = "region", table = "HD", patterns = c("Bureau of Economic Analysis \\(BEA\\) regions")),
  list(output = "hbcu", table = "HD", patterns = c("Historically Black College or University")),
  list(output = "tribal_college", table = "HD", patterns = c("^Tribal college$")),
  list(output = "sector", table = "HD", patterns = c("^Sector of institution$")),
  list(output = "level", table = "HD", patterns = c("^Level of institution$")),
  list(output = "control", table = "HD", patterns = c("^Control of institution$")),
  list(output = "degree_granting_status", table = "HD", patterns = c("^Degree-granting status$")),
  list(output = "highest_degree", table = "HD", patterns = c("^Highest degree offered$")),
  list(output = "category", table = "HD", patterns = c("^Institutional category$")),
  list(output = "grad_offering", table = "HD", patterns = c("^Graduate offering$")),
  list(output = "urbanization", table = "HD", patterns = c("Urban-centric locale")),
  list(output = "access_earnings", table = "HD", patterns = c("Student Access and Earnings")),
  list(output = "size", table = "HD", patterns = c("Institutional Size")),
  list(output = "undergrad_program_mix", table = "HD", patterns = c("Undergraduate Academic Program Mix")),
  list(output = "grad_program_mix", table = "HD", patterns = c("Graduate Academic Program Mix")),
  list(output = "control_or_affiliation", table = "IC", patterns = c("^Institutional control or affiliation$")),
  list(output = "religious_affiliation", table = "IC", patterns = c("^Religious affiliation$")),
  list(output = "has_full_time_first_time_undergrad", table = "IC", patterns = c("Full time first-time degree/certificate-seeking undergraduate students enrolled")),
  list(output = "all_programs_distance_education", table = "IC", patterns = c("^All programs offered completely via distance education$")),
  list(output = "reporting_model", table = "FLAGS", patterns = c("Identifies reporting standards GASB, FASB, or modified FASB")),
  list(output = "pell_accounting_method_fasb", table = "FLAGS", patterns = c("^Account for Pell grants as pass through transactions or as federal grant revenues to the institution \\(FASB\\s+institutions\\)\\?$", "^Account for Pell grants as pass through transactions or as federal grant revenues to the institution \\(FASB institutions\\)\\?$")),
  list(output = "pell_accounting_method_pfp", table = "FLAGS", patterns = c("^Account for Pell grants as pass through transactions or as federal grant revenues to the institution \\(private-for-profit institutions\\)\\?$")),
  list(output = "admissions_yield", table = "DRVADM", patterns = c("^Admissions yield")),
  list(output = "fte_12_months", table = "DRVEF12", patterns = c("^12-month full-time equivalent enrollment$", "^12-month full-time equivalent enrollment \\(FTE\\)$")),
  list(output = "fte_undergrad", table = "EFIA", patterns = c("^Reported full-time equivalent \\(FTE\\) undergraduate enrollment", "^Reported FTE undergraduate enrollment$")),
  list(output = "fte_graduate", table = "EFIA", patterns = c("^Reported full-time equivalent \\(FTE\\) graduate enrollment", "^Reported FTE graduate enrollment$")),
  list(output = "fte_total_staff", table = "DRVHR", patterns = c("^Total full-time-equivalent staff$", "^Total FTE staff$")),
  list(output = "fte_instructional", table = "DRVHR", patterns = c("^Instructional staff, full-time-equivalent$", "^Instructional FTE$")),
  list(output = "transfer_out_rate_bachelor", table = "DRVGR", patterns = c("^Transfer-out rate - Bachelor cohort$", "^Transfer-out rate, bachelor cohort$")),
  list(output = "state_approps_percent_core_gasb", table = "DRVF", patterns = c("State appropriations as percent of core revenues.*GASB")),
  list(output = "gov_grants_fasb", table = "DRVF", patterns = c("Government grants and contracts as a percent of core revenues.*FASB")),
  list(output = "state_revenue_fte_fasb", table = "DRVF", patterns = c("Revenues from state appropriations per FTE.*FASB")),
  list(output = "gov_revenue_fte_fasb", table = "DRVF", patterns = c("Revenues from government grants and contracts per FTE.*FASB")),
  list(output = "research_expenses_pct_core_expenses_gasb", table = "DRVF", patterns = c("^Research expenses as a percent of total core expenses.*GASB")),
  list(output = "research_expenses_pct_core_expenses_fasb", table = "DRVF", patterns = c("^Research expenses as a percent of total core expenses.*FASB")),
  list(output = "research_expenses_pct_core_expenses_pfp", table = "DRVF", patterns = c("^Research expenses as a percent of total core expenses.*for-profit")),
  list(output = "research_expenses_per_fte_gasb", table = "DRVF", patterns = c("^Research expenses per FTE.*GASB")),
  list(output = "research_expenses_per_fte_fasb", table = "DRVF", patterns = c("^Research expenses per FTE.*FASB")),
  list(output = "research_expenses_per_fte_pfp", table = "DRVF", patterns = c("^Research expenses per FTE.*for-profit")),
  list(output = "core_expenses_gasb", table = "DRVF", patterns = c("^Total core expenses.*GASB")),
  list(output = "core_expenses_fasb", table = "DRVF", patterns = c("^Total core expenses.*FASB")),
  list(output = "core_expenses_pfp", table = "DRVF", patterns = c("^Total core expenses.*for-profit")),
  list(output = "endowment_assets_per_fte_gasb", table = "DRVF", patterns = c("Endowment assets \\(year end\\) per FTE enrollment.*GASB")),
  list(output = "endowment_assets_per_fte_fasb", table = "DRVF", patterns = c("Endowment assets \\(year end\\) per FTE enrollment.*FASB")),
  list(output = "tuition_fees_after_discounts_allowances_gasb", table = "F1A", patterns = c("^Tuition and fees, after deducting discounts and allowances$")),
  list(output = "federal_operating_grants_contracts_gasb", table = "F1A", patterns = c("^Federal operating grants and contracts$")),
  list(output = "state_appropriations_gasb", table = "F1A", patterns = c("^State appropriations$")),
  list(output = "total_operating_nonoperating_revenues_gasb", table = "F1A", patterns = c("^Total revenues and other additions$", "^Total operating and nonoperating revenues$")),
  list(output = "discounts_allowances_applied_tuition_fees_gasb", table = "F1A", patterns = c("Discounts and allowances applied to tuition and fees")),
  list(output = "research_expenses_total_gasb", table = "F1A", patterns = c("^Research\\s*-\\s*Current year total$", "^Research - Current year total$")),
  list(output = "total_expenses_deductions_current_total_gasb", table = "F1A", patterns = c("^Total expenses and other deductions$", "^Total expenses and deductions.? Current year total$", "^Total expenses and deductions - Current year total$")),
  list(output = "total_assets_gasb", table = "F1A", patterns = c("^Total assets")),
  list(output = "total_liabilities_gasb", table = "F1A", patterns = c("^Total liabilities")),
  list(output = "unrestricted_public", table = "F1A", patterns = c("^Unrestricted$", "^Unrestricted net position$")),
  list(output = "value_endowment_assets_beginning_gasb", table = "F1A", patterns = c("Value of endowment assets at beginning of fiscal year", "Value of endowment assets at the beginning of the fiscal year")),
  list(output = "value_endowment_assets_end_gasb", table = "F1A", patterns = c("Value of endowment assets at end of fiscal year", "Value of endowment assets at the end of the fiscal year")),
  list(output = "new_gifts_additions_gasb", table = "F1A", patterns = c("New gifts and additions")),
  list(output = "endowment_net_investment_return_gasb", table = "F1A", patterns = c("Endowment net investment return")),
  list(output = "endowment_spending_distribution_current_use_gasb", table = "F1A", patterns = c("Spending distribution for current use")),
  list(output = "total_assets_fasb", table = "F2", patterns = c("^Total assets$")),
  list(output = "total_liabilities_fasb", table = "F2", patterns = c("^Total liabilities$")),
  list(output = "total_unrestricted_net_assets_fasb", table = "F2", patterns = c("^Total unrestricted net assets$")),
  list(output = "institutional_grants_funded_fasb", table = "F2", patterns = c("^Institutional grants \\(funded\\)$")),
  list(output = "institutional_grants_unfunded_fasb", table = "F2", patterns = c("^Institutional grants \\(unfunded\\)$")),
  list(output = "allowances_applied_to_tuition_fasb", table = "F2", patterns = c("^Allowances applied to tuition and fees$")),
  list(output = "pell_grants", table = "F2", patterns = c("^Pell grants$")),
  list(output = "tuition_and_fees_fasb", table = "F2", patterns = c("^Tuition and fees \\(total\\)$", "^Tuition and fees - Total$")),
  list(output = "state_approps_fasb", table = "F2", patterns = c("^State appropriations.? Total$", "^State appropriations - Total$")),
  list(output = "federal_grants_contracts_fasb", table = "F2", patterns = c("^Federal grants and contracts.? Total$", "^Federal grants and contracts - Total$")),
  list(output = "research_expenses_total_fasb", table = "F2", patterns = c("^Research-Total amount$", "^Research - Total amount$")),
  list(output = "total_revenues_investment_return_fasb", table = "F2", patterns = c("^Total revenues and investment return.? Total$", "^Total revenues and investment return - Total$")),
  list(output = "total_expenses_fasb", table = "F2", patterns = c("^Total expenses$", "^Total expenses.? Total amount$", "^Total expenses - Total amount$")),
  list(output = "unrestricted_operating_rev_fasb", table = "F2", patterns = c("^Total unrestricted operating revenues$")),
  list(output = "value_endowment_beginning_fasb", table = "F2", patterns = c("Value of endowment assets at beginning of fiscal year", "Value of endowment assets at the beginning of the fiscal year")),
  list(output = "value_endowment_end_fasb", table = "F2", patterns = c("Value of endowment assets at end of fiscal year", "Value of endowment assets at the end of the fiscal year")),
  list(output = "new_endowment_gifts_fasb", table = "F2", patterns = c("New gifts and additions")),
  list(output = "endowment_net_investment_return_fasb", table = "F2", patterns = c("Endowment net investment return")),
  list(output = "spending_distribution_for_current_use_fasb", table = "F2", patterns = c("Spending distribution for current use")),
  list(output = "total_assets_pfp", table = "F3", patterns = c("^Total assets$")),
  list(output = "total_liabilities_pfp", table = "F3", patterns = c("^Total liabilities$")),
  list(output = "institutional_grants_pfp", table = "F3", patterns = c("^Institutional grants$")),
  list(output = "discounts_allowances_applied_tuition_fees_pfp", table = "F3", patterns = c("Discounts and allowances applied to tuition and fees", "Allowances applied to tuition and fees")),
  list(output = "tuition_fees_pfp", table = "F3", patterns = c("^Tuition and fees$")),
  list(output = "federal_grants_contracts_pfp", table = "F3", patterns = c("^Federal grants and contracts", "^Federal appropriations, grants and contracts")),
  list(output = "research_expenses_total_pfp", table = "F3", patterns = c("^Research-Total amount$", "^Research - Total amount$")),
  list(output = "state_appropriations_pfp", table = "F3", patterns = c("^State appropriations", "^State appropriations, grants and contracts")),
  list(output = "total_revenues_investment_return_pfp", table = "F3", patterns = c("^Total revenues and investment return$")),
  list(output = "total_revenues_pfp", table = "F3", patterns = c("^Total revenues$")),
  list(output = "total_expenses_total_amount_pfp", table = "F3", patterns = c("^Total expenses$", "^Total expenses.? Total amount$", "^Total expenses-Total amount$"))
)

# A few finance variables are more reliable to resolve by exact IPEDS code than
# by dictionary title text, especially the research and core-expense fields.
  exact_field_overrides <- list(
    tuition_fees_after_discounts_allowances_gasb = "F1B01",
    federal_operating_grants_contracts_gasb = "F1B04",
    state_appropriations_gasb = "F1B17",
    total_operating_nonoperating_revenues_gasb = "F1D01",
    total_expenses_deductions_current_total_gasb = "F1D02",
    tuition_and_fees_fasb = "F2D01",
    pell_grants = "F2D06",
    federal_grants_contracts_fasb = "F2D05",
    state_approps_fasb = "F2D04",
    total_revenues_investment_return_fasb = "F2B01",
    total_expenses_fasb = "F2B02",
    tuition_fees_pfp = "F3D01",
    federal_grants_contracts_pfp = "F3D04",
    state_appropriations_pfp = "F3D05",
    total_revenues_investment_return_pfp = "F3B01",
    total_expenses_total_amount_pfp = "F3B02",
    research_expenses_total_gasb = "F1C021",
    value_endowment_assets_beginning_gasb = "F1H01",
    value_endowment_assets_end_gasb = "F1H02",
    new_gifts_additions_gasb = "F1H03A",
    endowment_net_investment_return_gasb = "F1H03B",
    endowment_spending_distribution_current_use_gasb = "F1H03C",
    research_expenses_total_fasb = "F2E021",
    value_endowment_beginning_fasb = "F2H01",
    value_endowment_end_fasb = "F2H02",
    new_endowment_gifts_fasb = "F2H03A",
    endowment_net_investment_return_fasb = "F2H03B",
    spending_distribution_for_current_use_fasb = "F2H03C",
    research_expenses_total_pfp = "F3E02A1",
  core_expenses_gasb = "F1COREXP",
  core_expenses_fasb = "F2COREXP",
  core_expenses_pfp = "F3COREXP",
  research_expenses_pct_core_expenses_gasb = "F1RSRCPC",
  research_expenses_pct_core_expenses_fasb = "F2RSRCPC",
  research_expenses_pct_core_expenses_pfp = "F3RSRCPC",
  research_expenses_per_fte_gasb = "F1RSRCFT",
  research_expenses_per_fte_fasb = "F2RSRCFT",
  research_expenses_per_fte_pfp = "F3RSRCFT"
)

# ── main year loop ─────────────────────────────────────────────────────────
all_rows             <- list()
resolution_audit_rows <- list()
required_year_cache_cols <- c(
  "enrollment_headcount_total",
  "enrollment_headcount_undergrad",
  "enrollment_headcount_graduate",
  "enrollment_nonresident_total",
  "enrollment_nonresident_undergrad",
  "enrollment_nonresident_graduate",
  "staff_headcount_total",
  "staff_headcount_instructional"
)
year_table_alias_patterns <- c(
  HD = "^HD\\d{4}$",
  IC = "^IC\\d{4}$",
  FLAGS = "^FLAGS\\d{4}$",
  EFFY = "^EFFY\\d{4}$",
  EFIA = "^EFIA\\d{4}$",
  EAP = "^EAP\\d{4}$",
  DRVEF12 = "^DRVEF12\\d{4}$",
  DRVADM = "^DRVADM\\d{4}$",
  DRVHR = "^DRVHR\\d{4}$",
  DRVGR = "^DRVGR\\d{4}$",
  DRVF = "^DRVF\\d{4}$",
  F1A = "^F\\d{4}_F1A$",
  F2 = "^F\\d{4}_F2$",
  F3 = "^F\\d{4}_F3$"
)

for (year in start_year:end_year) {
  # Build each year separately so we can cache the result and avoid
  # recomputing years that already exist locally.
  year_cache_csv <- file.path(year_cache_dir, sprintf("%s_year_%d.csv", output_stem, year))

  # ── Use cached year if available and not force-rebuilding ──────────────
  if (!force_rebuild && file.exists(year_cache_csv)) {
    cat(sprintf("[%d] Loading from cache...\n", year))
    cached <- suppressMessages(readr::read_csv(year_cache_csv, show_col_types = FALSE))
    missing_cache_cols <- setdiff(required_year_cache_cols, names(cached))
    if (length(missing_cache_cols) == 0) {
      if ("unitid" %in% names(cached)) cached <- cached %>% mutate(unitid = as.character(unitid))
      if ("year" %in% names(cached)) cached <- cached %>% mutate(year = suppressWarnings(as.integer(year)))
      all_rows[[length(all_rows) + 1L]] <- cached
      next
    }
    cat(sprintf(
      "[%d] Rebuilding cache because required columns are missing: %s\n",
      year,
      paste(missing_cache_cols, collapse = ", ")
    ))
  }

  year_catalog <- catalog_by_year[[as.character(year)]]
  if (is.null(year_catalog) || nrow(year_catalog) == 0) next

  cat(sprintf("[%d] Processing...\n", year))

  # Canonical alias → regex pattern mapping.  Add a row here to support a
  # new IPEDS table without touching the download/index/field-match loops.
  aliases <- resolve_year_table_aliases(year_catalog, year_table_alias_patterns)
  year_assets <- load_year_tables_and_dictionaries(
    aliases = aliases,
    year_catalog = year_catalog,
    data_root = data_root,
    dict_root = dict_root,
    extract_root = extract_root
  )
  data_tables <- year_assets$data_tables
  dictionaries <- year_assets$dictionaries

  field_resolution <- resolve_year_fields(
    field_specs = field_specs,
    dictionaries = dictionaries,
    data_tables = data_tables,
    exact_field_overrides = exact_field_overrides,
    aliases = aliases,
    year = year
  )
  resolved_fields <- field_resolution$resolved_fields
  resolution_audit_rows[[length(resolution_audit_rows) + 1L]] <- field_resolution$resolution_audit

  hd_table <- data_tables[["HD"]]
  if (is.null(hd_table) || nrow(hd_table) == 0) next

  # ── Pre-index every table by UNITID for O(1) lookup ──────────────────────
  # This replaces the original filter(UNITID == !!unitid) per-row scan,
  # which was O(n) per lookup and the main performance bottleneck.
  table_index <- lapply(data_tables, function(tbl) split(tbl, tbl$UNITID))

  # Pre-filter and pre-index EAP for the two occupation categories we need
  eap_indexes <- build_eap_indexes(data_tables[["EAP"]])
  eap_100_index <- eap_indexes$total
  eap_210_index <- eap_indexes$instructional
  rm(eap_indexes)

  # EFFY stores enrollment in multiple rows per institution, so collapse it to
  # one row per UNITID before the main unit loop.
  effy_index <- build_effy_index(data_tables[["EFFY"]])

  unitids   <- hd_table$UNITID
  year_rows <- vector("list", length(unitids))

  for (i in seq_along(unitids)) {
    unitid <- unitids[[i]]

    get_row <- function(alias) {
      idx <- table_index[[alias]]
      if (is.null(idx)) return(NULL)
      row <- idx[[unitid]]
      if (is.null(row) || nrow(row) == 0) return(NULL)
      row[1L, , drop = FALSE]
    }

    hd     <- get_row("HD");   ic      <- get_row("IC");    flags  <- get_row("FLAGS")
    efia   <- get_row("EFIA"); drvef12 <- get_row("DRVEF12"); drvadm <- get_row("DRVADM")
    drvhr  <- get_row("DRVHR"); drvgr  <- get_row("DRVGR"); drvf   <- get_row("DRVF")
    f1     <- get_row("F1A");  f2      <- get_row("F2");    f3     <- get_row("F3")
    effy   <- effy_index[[unitid]]

    institution_name <- get_string(hd, resolved_fields[["institution_name"]])
    city             <- get_string(hd, resolved_fields[["city"]])
    state            <- get_string(hd, resolved_fields[["state"]])

    fte_undergrad <- get_number(efia, resolved_fields[["fte_undergrad"]])
    fte_graduate  <- get_number(efia, resolved_fields[["fte_graduate"]])
    if (is.na(fte_undergrad)) {
      fte_undergrad <- first_non_null(c(get_number(efia, "EFTEUG"), get_number(efia, "FTEUG")))
    }
    if (is.na(fte_graduate)) {
      fte_graduate <- first_non_null(c(get_number(efia, "EFTEGD"), get_number(efia, "FTEGD")))
    }

    fte_12_months <- get_number(drvef12, resolved_fields[["fte_12_months"]])
    if (is.na(fte_12_months) && (!is.na(fte_undergrad) || !is.na(fte_graduate))) {
      fte_12_months <- dplyr::coalesce(fte_undergrad, 0) + dplyr::coalesce(fte_graduate, 0)
    }

    get_eap_fte <- function(occupcat) {
      idx <- if (occupcat == "100") eap_100_index else eap_210_index
      hit <- idx[[unitid]]
      if (is.null(hit) || nrow(hit) == 0) return(NA_real_)
      hit <- hit[1L, , drop = FALSE]
      ft  <- get_number(hit, "EAPFT")
      pt  <- get_number(hit, "EAPPT")
      if (is.na(ft) && is.na(pt)) return(NA_real_)
      dplyr::coalesce(ft, 0) + dplyr::coalesce(pt, 0) / 3
    }

    get_eap_headcount <- function(occupcat) {
      idx <- if (occupcat == "100") eap_100_index else eap_210_index
      hit <- idx[[unitid]]
      if (is.null(hit) || nrow(hit) == 0) return(NA_real_)
      hit <- hit[1L, , drop = FALSE]
      get_number(hit, "EAPTOT")
    }

    fte_total_staff_rebuilt    <- get_eap_total_fte(unitid, eap_100_index)
    fte_instructional_rebuilt  <- get_eap_instructional_fte(unitid, eap_210_index)
    staff_headcount_total_rebuilt <- get_eap_total_headcount(unitid, eap_100_index)
    staff_headcount_instructional_rebuilt <- get_eap_instructional_headcount(unitid, eap_210_index)

    auxiliary_enterprises_revenue_gasb    <- get_number(f1, "F1B05")
    hospital_services_revenue_gasb        <- get_number(f1, "F1B06")
    independent_operations_revenue_gasb   <- get_number(f1, "F1B07")
    core_revenue_gasb_rebuilt <- if (is.na(get_number(f1, "F1D01"))) {
      NA_real_
    } else {
      get_number(f1, "F1D01") -
        dplyr::coalesce(auxiliary_enterprises_revenue_gasb, 0) -
        dplyr::coalesce(hospital_services_revenue_gasb, 0) -
        dplyr::coalesce(independent_operations_revenue_gasb, 0)
    }

    # Only use state/local FASB government-grant fields when they resolve from
    # the year-specific dictionary. Hard-coding nearby F2 columns is brittle and
    # can misclassify Pell grants or other aid lines as state/local revenue.
    state_grants_contracts_fasb <- if (!is.null(resolved_fields[["state_grants_contracts_fasb"]])) {
      get_number(f2, resolved_fields[["state_grants_contracts_fasb"]])
    } else {
      NA_real_
    }
    local_grants_contracts_fasb <- if (!is.null(resolved_fields[["local_grants_contracts_fasb"]])) {
      get_number(f2, resolved_fields[["local_grants_contracts_fasb"]])
    } else {
      NA_real_
    }
    auxiliary_enterprises_revenue_fasb <- get_number(f2, "F2D12")
    hospital_revenue_fasb              <- get_number(f2, "F2D13")
    independent_operations_revenue_fasb <- get_number(f2, "F2D14")
    gov_grants_contracts_total_fasb <- sum_if_any(c(
      get_number(f2, resolved_fields[["federal_grants_contracts_fasb"]]),
      state_grants_contracts_fasb,
      local_grants_contracts_fasb
    ))
    core_revenue_fasb_rebuilt <- if (is.na(get_number(f2, resolved_fields[["total_revenues_investment_return_fasb"]]))) {
      NA_real_
    } else {
      get_number(f2, resolved_fields[["total_revenues_investment_return_fasb"]]) -
        dplyr::coalesce(auxiliary_enterprises_revenue_fasb, 0) -
        dplyr::coalesce(hospital_revenue_fasb, 0) -
        dplyr::coalesce(independent_operations_revenue_fasb, 0)
    }

    row <- tibble::tibble(
      unitid = unitid,
      institution_name = institution_name,
      institution_unique_name = paste(na.omit(c(institution_name, city, state)), collapse = " | "),
      year = year,
      access_earnings = get_string(hd, resolved_fields[["access_earnings"]]),
      admissions_yield = get_number(drvadm, resolved_fields[["admissions_yield"]]),
      city = city,
      state = state,
      zip = get_string(hd, resolved_fields[["zip"]]),
      county = get_string(hd, resolved_fields[["county"]]),
      longitude = get_number(hd, resolved_fields[["longitude"]]),
      latitude = get_number(hd, resolved_fields[["latitude"]]),
      status = get_string(hd, resolved_fields[["status"]]),
      date_closed = get_string(hd, resolved_fields[["date_closed"]]),
      is_active = get_string(hd, resolved_fields[["is_active"]]),
      multi_institution_campus_org = get_string(hd, resolved_fields[["multi_institution_campus_org"]]),
      id_number_multi = get_string(hd, resolved_fields[["id_number_multi"]]),
      name_multi = get_string(hd, resolved_fields[["name_multi"]]),
      opeid = get_string(hd, resolved_fields[["opeid"]]),
      region = get_string(hd, resolved_fields[["region"]]),
      hbcu = get_string(hd, resolved_fields[["hbcu"]]),
      tribal_college = get_string(hd, resolved_fields[["tribal_college"]]),
      sector = get_string(hd, resolved_fields[["sector"]]),
      level = get_string(hd, resolved_fields[["level"]]),
      control = get_string(hd, resolved_fields[["control"]]),
      degree_granting_status = get_string(hd, resolved_fields[["degree_granting_status"]]),
      highest_degree = get_string(hd, resolved_fields[["highest_degree"]]),
      category = get_string(hd, resolved_fields[["category"]]),
      grad_offering = get_string(hd, resolved_fields[["grad_offering"]]),
      urbanization = get_string(hd, resolved_fields[["urbanization"]]),
      size = get_string(hd, resolved_fields[["size"]]),
      undergrad_program_mix = get_string(hd, resolved_fields[["undergrad_program_mix"]]),
      grad_program_mix = get_string(hd, resolved_fields[["grad_program_mix"]]),
      control_or_affiliation = get_string(ic, resolved_fields[["control_or_affiliation"]]),
      religious_affiliation = get_string(ic, resolved_fields[["religious_affiliation"]]),
      has_full_time_first_time_undergrad = get_string(ic, resolved_fields[["has_full_time_first_time_undergrad"]]),
      all_programs_distance_education = get_string(ic, resolved_fields[["all_programs_distance_education"]]),
      reporting_model = get_string(flags, resolved_fields[["reporting_model"]]),
      fte_12_months = fte_12_months,
      fte_undergrad = fte_undergrad,
      fte_graduate = fte_graduate,
      enrollment_headcount_total = if (is.null(effy) || nrow(effy) == 0) NA_real_ else effy$enrollment_headcount_total[[1]],
      enrollment_headcount_undergrad = if (is.null(effy) || nrow(effy) == 0) NA_real_ else effy$enrollment_headcount_undergrad[[1]],
      enrollment_headcount_graduate = if (is.null(effy) || nrow(effy) == 0) NA_real_ else effy$enrollment_headcount_graduate[[1]],
      enrollment_nonresident_total = if (is.null(effy) || nrow(effy) == 0) NA_real_ else effy$enrollment_nonresident_total[[1]],
      enrollment_nonresident_undergrad = if (is.null(effy) || nrow(effy) == 0) NA_real_ else effy$enrollment_nonresident_undergrad[[1]],
      enrollment_nonresident_graduate = if (is.null(effy) || nrow(effy) == 0) NA_real_ else effy$enrollment_nonresident_graduate[[1]],
      fte_total_staff = dplyr::coalesce(get_number(drvhr, resolved_fields[["fte_total_staff"]]), fte_total_staff_rebuilt),
      fte_instructional = dplyr::coalesce(get_number(drvhr, resolved_fields[["fte_instructional"]]), fte_instructional_rebuilt),
      staff_headcount_total = staff_headcount_total_rebuilt,
      staff_headcount_instructional = staff_headcount_instructional_rebuilt,
      transfer_out_rate_bachelor = get_number(drvgr, resolved_fields[["transfer_out_rate_bachelor"]]),
      state_approps_percent_core_gasb = dplyr::coalesce(
        get_number(drvf, resolved_fields[["state_approps_percent_core_gasb"]]),
        safe_divide(get_number(f1, resolved_fields[["state_appropriations_gasb"]]), core_revenue_gasb_rebuilt)
      ),
      gov_grants_fasb = dplyr::coalesce(
        get_number(drvf, resolved_fields[["gov_grants_fasb"]]),
        safe_divide(gov_grants_contracts_total_fasb, core_revenue_fasb_rebuilt)
      ),
      state_revenue_fte_fasb = dplyr::coalesce(
        get_number(drvf, resolved_fields[["state_revenue_fte_fasb"]]),
        safe_divide(get_number(f2, resolved_fields[["state_approps_fasb"]]), fte_12_months)
      ),
      gov_revenue_fte_fasb = dplyr::coalesce(
        get_number(drvf, resolved_fields[["gov_revenue_fte_fasb"]]),
        safe_divide(gov_grants_contracts_total_fasb, fte_12_months)
      ),
      research_expenses_pct_core_expenses_gasb = get_number(drvf, resolved_fields[["research_expenses_pct_core_expenses_gasb"]]),
      research_expenses_pct_core_expenses_fasb = get_number(drvf, resolved_fields[["research_expenses_pct_core_expenses_fasb"]]),
      research_expenses_pct_core_expenses_pfp = get_number(drvf, resolved_fields[["research_expenses_pct_core_expenses_pfp"]]),
      research_expenses_per_fte_gasb = get_number(drvf, resolved_fields[["research_expenses_per_fte_gasb"]]),
      research_expenses_per_fte_fasb = get_number(drvf, resolved_fields[["research_expenses_per_fte_fasb"]]),
      research_expenses_per_fte_pfp = get_number(drvf, resolved_fields[["research_expenses_per_fte_pfp"]]),
      core_expenses_gasb = get_number(drvf, resolved_fields[["core_expenses_gasb"]]),
      core_expenses_fasb = get_number(drvf, resolved_fields[["core_expenses_fasb"]]),
      core_expenses_pfp = get_number(drvf, resolved_fields[["core_expenses_pfp"]]),
      endowment_assets_per_fte_gasb = dplyr::coalesce(
        get_number(drvf, resolved_fields[["endowment_assets_per_fte_gasb"]]),
        safe_divide(dplyr::coalesce(get_number(f1, resolved_fields[["value_endowment_assets_end_gasb"]]), get_number(f1, "F1H02")), fte_12_months)
      ),
      endowment_assets_per_fte_fasb = dplyr::coalesce(
        get_number(drvf, resolved_fields[["endowment_assets_per_fte_fasb"]]),
        safe_divide(dplyr::coalesce(get_number(f2, resolved_fields[["value_endowment_end_fasb"]]), get_number(f2, "F2H02")), fte_12_months)
      ),
      tuition_fees_after_discounts_allowances_gasb = get_number(f1, resolved_fields[["tuition_fees_after_discounts_allowances_gasb"]]),
      federal_operating_grants_contracts_gasb = get_number(f1, resolved_fields[["federal_operating_grants_contracts_gasb"]]),
      state_appropriations_gasb = get_number(f1, resolved_fields[["state_appropriations_gasb"]]),
      total_operating_nonoperating_revenues_gasb = dplyr::coalesce(
        get_number(f1, resolved_fields[["total_operating_nonoperating_revenues_gasb"]]),
        get_number(f1, "F1D01"),
        get_number(f1, "F1B27"),
        get_number(f1, "F1B25")
      ),
      discounts_allowances_applied_tuition_fees_gasb = get_number(f1, resolved_fields[["discounts_allowances_applied_tuition_fees_gasb"]]),
      research_expenses_total_gasb = get_number(f1, resolved_fields[["research_expenses_total_gasb"]]),
      total_expenses_deductions_current_total_gasb = dplyr::coalesce(
        get_number(f1, resolved_fields[["total_expenses_deductions_current_total_gasb"]]),
        get_number(f1, "F1D02"),
        get_number(f1, "F1C191")
      ),
      total_assets_gasb = get_number(f1, resolved_fields[["total_assets_gasb"]]),
      total_liabilities_gasb = get_number(f1, resolved_fields[["total_liabilities_gasb"]]),
      unrestricted_public = get_number(f1, resolved_fields[["unrestricted_public"]]),
      value_endowment_assets_beginning_gasb = dplyr::coalesce(get_number(f1, resolved_fields[["value_endowment_assets_beginning_gasb"]]), get_number(f1, "F1H01"), get_number(f1, "F1H02")),
      value_endowment_assets_end_gasb = dplyr::coalesce(get_number(f1, resolved_fields[["value_endowment_assets_end_gasb"]]), get_number(f1, "F1H02"), get_number(f1, "F1H01")),
      new_gifts_additions_gasb = dplyr::coalesce(get_number(f1, resolved_fields[["new_gifts_additions_gasb"]]), get_number(f1, "F1B22")),
      endowment_net_investment_return_gasb = get_number(f1, resolved_fields[["endowment_net_investment_return_gasb"]]),
      endowment_spending_distribution_current_use_gasb = get_number(f1, resolved_fields[["endowment_spending_distribution_current_use_gasb"]]),
      total_assets_fasb = get_number(f2, resolved_fields[["total_assets_fasb"]]),
      total_liabilities_fasb = get_number(f2, resolved_fields[["total_liabilities_fasb"]]),
      total_unrestricted_net_assets_fasb = get_number(f2, resolved_fields[["total_unrestricted_net_assets_fasb"]]),
      institutional_grants_funded_fasb = get_number(f2, resolved_fields[["institutional_grants_funded_fasb"]]),
      institutional_grants_unfunded_fasb = get_number(f2, resolved_fields[["institutional_grants_unfunded_fasb"]]),
      allowances_applied_to_tuition_fasb = get_number(f2, resolved_fields[["allowances_applied_to_tuition_fasb"]]),
      pell_grants = get_number(f2, resolved_fields[["pell_grants"]]),
      pell_accounting_method = first_non_null(c(get_string(flags, resolved_fields[["pell_accounting_method_fasb"]]), get_string(flags, resolved_fields[["pell_accounting_method_pfp"]]))),
      tuition_and_fees_fasb = get_number(f2, resolved_fields[["tuition_and_fees_fasb"]]),
      state_approps_fasb = get_number(f2, resolved_fields[["state_approps_fasb"]]),
      federal_grants_contracts_fasb = get_number(f2, resolved_fields[["federal_grants_contracts_fasb"]]),
      state_grants_contracts_fasb = state_grants_contracts_fasb,
      local_grants_contracts_fasb = local_grants_contracts_fasb,
      total_revenues_investment_return_fasb = get_number(f2, resolved_fields[["total_revenues_investment_return_fasb"]]),
      research_expenses_total_fasb = get_number(f2, resolved_fields[["research_expenses_total_fasb"]]),
      total_expenses_fasb = dplyr::coalesce(
        get_number(f2, resolved_fields[["total_expenses_fasb"]]),
        get_number(f2, "F2B02"),
        get_number(f2, "F2I07"),
        get_number(f2, "F2E131"),
        get_number(f2, "F2D18"),
        get_number(f2, "F2D16")
      ),
      unrestricted_operating_rev_fasb = get_number(f2, resolved_fields[["unrestricted_operating_rev_fasb"]]),
      value_endowment_beginning_fasb = dplyr::coalesce(get_number(f2, resolved_fields[["value_endowment_beginning_fasb"]]), get_number(f2, "F2H01"), get_number(f2, "F2H02")),
      value_endowment_end_fasb = dplyr::coalesce(get_number(f2, resolved_fields[["value_endowment_end_fasb"]]), get_number(f2, "F2H02"), get_number(f2, "F2H01")),
      new_endowment_gifts_fasb = get_number(f2, resolved_fields[["new_endowment_gifts_fasb"]]),
      endowment_net_investment_return_fasb = get_number(f2, resolved_fields[["endowment_net_investment_return_fasb"]]),
      spending_distribution_for_current_use_fasb = get_number(f2, resolved_fields[["spending_distribution_for_current_use_fasb"]]),
      auxiliary_enterprises_revenue_gasb = auxiliary_enterprises_revenue_gasb,
      hospital_services_revenue_gasb = hospital_services_revenue_gasb,
      independent_operations_revenue_gasb = independent_operations_revenue_gasb,
      auxiliary_enterprises_revenue_fasb = auxiliary_enterprises_revenue_fasb,
      hospital_revenue_fasb = hospital_revenue_fasb,
      independent_operations_revenue_fasb = independent_operations_revenue_fasb,
      total_assets_pfp = get_number(f3, resolved_fields[["total_assets_pfp"]]),
      total_liabilities_pfp = get_number(f3, resolved_fields[["total_liabilities_pfp"]]),
      institutional_grants_pfp = get_number(f3, resolved_fields[["institutional_grants_pfp"]]),
      discounts_allowances_applied_tuition_fees_pfp = get_number(f3, resolved_fields[["discounts_allowances_applied_tuition_fees_pfp"]]),
      tuition_fees_pfp = get_number(f3, resolved_fields[["tuition_fees_pfp"]]),
      federal_grants_contracts_pfp = get_number(f3, resolved_fields[["federal_grants_contracts_pfp"]]),
      research_expenses_total_pfp = get_number(f3, resolved_fields[["research_expenses_total_pfp"]]),
      state_appropriations_pfp = get_number(f3, resolved_fields[["state_appropriations_pfp"]]),
      total_revenues_investment_return_pfp = dplyr::coalesce(
        get_number(f3, resolved_fields[["total_revenues_investment_return_pfp"]]),
        get_number(f3, "F3B01"),
        get_number(f3, "F3D09")
      ),
      total_revenues_pfp = get_number(f3, resolved_fields[["total_revenues_pfp"]]),
      total_expenses_total_amount_pfp = dplyr::coalesce(
        get_number(f3, resolved_fields[["total_expenses_total_amount_pfp"]]),
        get_number(f3, "F3B02"),
        get_number(f3, "F3E071")
      )
    ) %>%
      mutate(
        assets      = dplyr::coalesce(total_assets_gasb,      total_assets_fasb,      total_assets_pfp),
        liabilities = dplyr::coalesce(total_liabilities_gasb, total_liabilities_fasb, total_liabilities_pfp)
      )

    year_rows[[i]] <- row
  } # end unitid loop

  # ── Save year cache and free memory ────────────────────────────────────
  year_df <- dplyr::bind_rows(year_rows)
  year_df <- year_df %>%
    mutate(
      unitid = as.character(unitid),
      year = suppressWarnings(as.integer(year))
    )
  readr::write_csv(year_df, year_cache_csv, na = "")
  all_rows[[length(all_rows) + 1L]] <- year_df

  rm(data_tables, table_index, eap_100_index, eap_210_index, year_rows, year_df)
  gc(verbose = FALSE)

} # end year loop

# Combine all annual caches into the final wide raw dataset, and write the
# resolution audit so we can inspect how every field was matched.
all_rows_df <- dplyr::bind_rows(all_rows) %>%
  filter(!state %in% excluded_state_codes) %>%
  arrange(year, unitid)

resolution_audit_df <- if (length(resolution_audit_rows) == 0) {
  tibble::tibble(
    year               = integer(),
    table_alias        = character(),
    table_name         = character(),
    output             = character(),
    resolved_var_name  = character(),
    resolved_var_title = character(),
    matched_pattern    = character()
  )
} else {
  dplyr::bind_rows(resolution_audit_rows)
}

# Write outputs atomically via .tmp intermediaries so a partial run never
# leaves a half-written file that downstream scripts silently accept.
tmp_dataset <- paste0(dataset_csv, ".tmp")
tmp_audit   <- paste0(resolution_audit_csv, ".tmp")
readr::write_csv(all_rows_df,        tmp_dataset, na = "")
readr::write_csv(resolution_audit_df, tmp_audit,   na = "")
file.rename(tmp_dataset, dataset_csv)
file.rename(tmp_audit,   resolution_audit_csv)

cat(sprintf("Saved catalog to %s\n",              catalog_csv))
cat(sprintf("Saved dataset to %s\n",              dataset_csv))
cat(sprintf("Saved field resolution audit to %s\n", resolution_audit_csv))

invisible(list(
  dataset                = dataset_csv,
  field_resolution_audit = resolution_audit_csv
))
}

if (sys.nframe() == 0) {
  result <- main()
  quit(save = "no", status = 0)
}
