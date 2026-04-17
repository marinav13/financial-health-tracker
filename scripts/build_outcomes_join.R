main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args          <- parse_cli_args(cli_args)
  ipeds         <- load_ipeds_paths()
  ipeds_layout  <- ipeds$ipeds_layout
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)

  # This script joins the latest College Scorecard earnings/debt values and the
  # latest IPEDS graduation-rate file onto the 2024 finance tracker cohort.
  # By default it expects those source files to already exist in the local
  # scorecard cache so the build is reproducible without a live download.
  ensure_packages(c("dplyr", "readr", "readxl", "stringr"))
  source(file.path(getwd(), "scripts", "shared", "export_helpers.R"))

  download_missing <- identical(get_arg_value("--download-missing", "FALSE"), "TRUE")
  input_csv <- get_arg_value("--input", ipeds_layout(root = ".")$dataset_csv)
  output_dir <- get_arg_value("--output-dir", ".")

  root <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  input_path <- normalizePath(input_csv, winslash = "/", mustWork = TRUE)
  outcomes_root <- file.path(root, "data_pipelines", "scorecard")
  cache_dir <- file.path(outcomes_root, "cache")
  dir.create(outcomes_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  scorecard_url <- "https://ed-public-download.scorecard.network/downloads/Most-Recent-Cohorts-Institution_03232026.zip"
  scorecard_zip <- file.path(cache_dir, "Most-Recent-Cohorts-Institution_03232026.zip")
  scorecard_csv <- file.path(cache_dir, "Most-Recent-Cohorts-Institution.csv")

  drvgr_url <- "https://nces.ed.gov/ipeds/data-generator?year=2024&tableName=DRVGR2024&HasRV=0&type=csv&t=639095446135801851"
  drvgr_zip <- file.path(cache_dir, "DRVGR2024.zip")
  drvgr_csv <- file.path(cache_dir, "drvgr2024.csv")

  grad_plus_url <- "https://studentaid.gov/sites/default/files/fsawg/datacenter/library/dl-dashboard-ay2025-2026-q2.xls"
  grad_plus_xls <- file.path(cache_dir, "dl-dashboard-ay2025-2026-q2.xls")

  hd2024_url <- "https://nces.ed.gov/ipeds/datacenter/data/HD2024.zip"
  hd2024_zip <- file.path(cache_dir, "HD2024.zip")
  hd2024_csv <- file.path(cache_dir, "hd2024.csv")
  hd2024_local_csv <- file.path(root, "ipeds", "cache", "downloads", "extracted", "data_HD2024", "hd2024.csv")

  # Helpers for downloading, extraction, and numeric cleanup.
  download_if_needed <- function(url, path) {
    if (file.exists(path)) return(invisible(path))
    if (!download_missing) {
      stop(
        paste0(
          "Missing required local source file: ", path, "\n",
          "Re-run with --download-missing TRUE if you want the script to fetch it."
        ),
        call. = FALSE
      )
    }
    utils::download.file(url, destfile = path, mode = "wb", quiet = TRUE)
    invisible(path)
  }

  resolve_hd2024_csv <- function() {
    if (file.exists(hd2024_csv)) return(hd2024_csv)
    if (file.exists(hd2024_local_csv)) return(hd2024_local_csv)
    download_if_needed(hd2024_url, hd2024_zip)
    extract_first_matching_file_from_zip(hd2024_zip, "^hd2024\\.csv$", cache_dir)
  }

  to_num <- function(x) {
    values <- trimws(as.character(x %||% ""))
    values[values %in% c("", "NA", "NULL", "PrivacySuppressed", "PS", "NULL", "NaN")] <- NA_character_
    suppressWarnings(as.numeric(gsub(",", "", values, fixed = TRUE)))
  }

  download_if_needed(scorecard_url, scorecard_zip)
  download_if_needed(drvgr_url, drvgr_zip)
  download_if_needed(grad_plus_url, grad_plus_xls)
  require_existing_local_file(
    scorecard_zip,
    "College Scorecard",
    "Place the file in data_pipelines/scorecard/cache/ or re-run with --download-missing TRUE."
  )
  require_existing_local_file(
    drvgr_zip,
    "IPEDS graduation rates",
    "Place the file in data_pipelines/scorecard/cache/ or re-run with --download-missing TRUE."
  )
  require_existing_local_file(
    grad_plus_xls,
    "Grad PLUS dashboard",
    "Place the file in data_pipelines/scorecard/cache/ or re-run with --download-missing TRUE."
  )

  scorecard_csv_path <- extract_single_file_from_zip(scorecard_zip, basename(scorecard_csv), cache_dir)
  drvgr_csv_path <- extract_single_file_from_zip(drvgr_zip, basename(drvgr_csv), cache_dir)
  hd2024_csv_path <- resolve_hd2024_csv()

# Limit the outcomes join to the latest tracker cohort so downstream exports can
# attach these values directly to each finance-page school record.
tracker <- readr::read_csv(input_path, show_col_types = FALSE) %>%
  mutate(unitid = as.character(unitid)) %>%
  filter(year == 2024) %>%
  distinct(unitid, .keep_all = TRUE)

scorecard <- readr::read_csv(scorecard_csv_path, show_col_types = FALSE) %>%
  transmute(
    unitid = as.character(UNITID),
    scorecard_institution_name = INSTNM,
    scorecard_city = CITY,
    scorecard_state = STABBR,
    median_earnings_10yr = to_num(MD_EARN_WNE_P10),
    median_debt_completers = to_num(GRAD_DEBT_MDN)
  )

drvgr <- readr::read_csv(drvgr_csv_path, show_col_types = FALSE) %>%
  transmute(
    unitid = as.character(UNITID),
    graduation_rate_6yr = to_num(GBA6RTT)
  )

grad_plus_col_names <- c(
  "ope_id", "school", "state", "zip_code", "school_type",
  "sub_recipients", "sub_loans_n", "sub_loans_amt", "sub_disb_n", "sub_disb_amt",
  "unsub_ug_recipients", "unsub_ug_loans_n", "unsub_ug_loans_amt", "unsub_ug_disb_n", "unsub_ug_disb_amt",
  "unsub_gr_recipients", "unsub_gr_loans_n", "unsub_gr_loans_amt", "unsub_gr_disb_n", "unsub_gr_disb_amt",
  "parent_plus_recipients", "parent_plus_loans_n", "parent_plus_loans_amt", "parent_plus_disb_n", "parent_plus_disb_amt",
  "grad_plus_recipients", "grad_plus_loans_originated_n", "grad_plus_loans_originated_amt",
  "grad_plus_disbursements_n", "grad_plus_disbursements_amt"
)

fsa_grad_plus <- readxl::read_excel(
  grad_plus_xls,
  sheet = "Award Year Summary",
  skip = 3,
  col_names = grad_plus_col_names,
  col_types = "text"
) %>%
  filter(!is.na(ope_id), str_detect(ope_id, "^\\d+")) %>%
  transmute(
    opeid_int = as.integer(ope_id),
    grad_plus_school = school,
    grad_plus_school_type = school_type,
    grad_plus_recipients = to_num(grad_plus_recipients),
    grad_plus_loans_originated_n = to_num(grad_plus_loans_originated_n),
    grad_plus_loans_originated_amt = to_num(grad_plus_loans_originated_amt),
    grad_plus_disbursements_n = to_num(grad_plus_disbursements_n),
    grad_plus_disbursements_amt = to_num(grad_plus_disbursements_amt)
  )

hd2024 <- readr::read_csv(hd2024_csv_path, show_col_types = FALSE) %>%
  rename_with(str_to_lower) %>%
  transmute(
    unitid = as.character(unitid),
    opeid_int = as.integer(opeid)
  ) %>%
  filter(!is.na(opeid_int)) %>%
  distinct(unitid, .keep_all = TRUE)

grad_plus <- fsa_grad_plus %>%
  left_join(hd2024, by = "opeid_int") %>%
  filter(!is.na(unitid)) %>%
  transmute(
    unitid,
    grad_plus_recipients,
    grad_plus_loans_originated_n,
    grad_plus_loans_originated_amt,
    grad_plus_disbursements_n,
    grad_plus_disbursements_amt,
    grad_plus_disbursements_per_recipient = safe_divide(grad_plus_disbursements_amt, grad_plus_recipients)
  ) %>%
  distinct(unitid, .keep_all = TRUE)

# Join Scorecard earnings/debt plus IPEDS graduation rates onto the selected
# tracker universe, then write the intermediate files used by web exports.
  scorecard_selected <- tracker %>%
    select(
      unitid,
      institution_name,
      institution_unique_name,
      city,
      state,
      control_label,
      sector,
      category,
      urbanization,
      all_programs_distance_education
    ) %>%
    left_join(scorecard, by = "unitid") %>%
    left_join(drvgr, by = "unitid") %>%
    left_join(grad_plus, by = "unitid") %>%
    mutate(
      institution_name = normalize_display_institution_name(coalesce(institution_name, scorecard_institution_name)),
      city = coalesce(city, scorecard_city),
      scorecard_institution_name = normalize_display_institution_name(scorecard_institution_name),
      state = coalesce(state, scorecard_state),
      outcomes_data_available = !is.na(median_earnings_10yr) | !is.na(median_debt_completers) | !is.na(graduation_rate_6yr) | !is.na(grad_plus_recipients) | !is.na(grad_plus_disbursements_amt),
      scorecard_data_updated = "2026-03-23",
      grad_plus_data_updated = "AY 2025-2026 Q2",
      ipeds_graduation_rate_year = 2024L,
      ipeds_graduation_rate_label = "Bachelor degree within 6 years, total"
    ) %>%
    arrange(institution_name)

  joined_out_path <- file.path(outcomes_root, "tracker_outcomes_joined.csv")
  readr::write_csv(scorecard_selected, joined_out_path, na = "")
  cat(sprintf("Saved joined tracker outcomes to %s\n", joined_out_path))

  invisible(TRUE)
}

if (sys.nframe() == 0) {
  main()
}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    