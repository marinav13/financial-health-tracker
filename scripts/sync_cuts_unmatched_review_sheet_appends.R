main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args <- parse_cli_args(cli_args)
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)
  has_flag <- function(flag) arg_has(args, flag)

  ensure_packages(c("dplyr", "readr", "digest", "googlesheets4"))
  source(file.path(getwd(), "scripts", "shared", "editorial_review_helpers.R"))
  source(file.path(getwd(), "scripts", "shared", "google_sheets_helpers.R"))

  # This tab is one-way triage only. When an editor confirms that an unmatched
  # discovered row is a tracked school under an unrecognized name, the durable
  # fix belongs in data_pipelines/college_cuts/manual_aliases.csv so future
  # discovery runs resolve the unitid before staging.
  discovered_path <- get_arg_value(
    "--discovered",
    file.path(getwd(), "data_pipelines", "college_cuts", "discovered_cut_candidates.csv")
  )
  leads_path <- get_arg_value(
    "--leads",
    file.path(getwd(), "data_pipelines", "college_cuts", "discovery", "leads.csv")
  )
  default_sheet <- Sys.getenv(
    "COLLEGE_CUTS_REVIEW_SHEET_ID",
    unset = Sys.getenv("ACCREDITATION_REVIEW_SHEET_ID", unset = NA_character_)
  )
  sheet_id_or_url <- get_arg_value("--sheet", default_sheet)
  sheet_tab <- get_arg_value(
    "--tab",
    Sys.getenv("COLLEGE_CUTS_UNMATCHED_REVIEW_SHEET_TAB", unset = "cuts_unmatched_review")
  )
  auth_json <- get_arg_value("--auth-json", Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = NA_character_))
  email <- get_arg_value("--email", NA_character_)
  cache_dir <- get_arg_value("--cache", file.path(getwd(), ".secrets", "googlesheets4"))
  verbose <- has_flag("--verbose")

  sheet_target <- trimws(as.character(sheet_id_or_url %||% ""))
  if (!nzchar(sheet_target)) {
    stop("Provide --sheet <Google Sheet URL or ID> or set the cuts review sheet id environment variable.", call. = FALSE)
  }

  if (!file.exists(discovered_path)) {
    message("No discovered cuts file at ", discovered_path, " - skipping unmatched cuts sheet append.")
    return(invisible(list(appended = 0L, tab = sheet_tab)))
  }

  discovered <- read_college_cuts_review_candidates(discovered_path)
  leads <- if (file.exists(leads_path)) {
    readr::read_csv(
      leads_path,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    )
  } else {
    data.frame(stringsAsFactors = FALSE)
  }

  authenticate_google_sheets(
    auth_json = auth_json,
    email = email,
    cache_dir = cache_dir,
    scopes = "spreadsheets",
    verbose = verbose
  )

  sheet_target <- extract_google_sheet_id(sheet_target)
  tab_exists <- google_sheet_tab_exists(sheet_target, sheet_tab)
  sheet_rows <- if (tab_exists) {
    read_google_sheet_table(
      ss = sheet_target,
      sheet_name = sheet_tab,
      verbose = verbose
    )
  } else {
    data.frame(stringsAsFactors = FALSE)
  }

  appended <- 0L
  if (!tab_exists || !nrow(sheet_rows)) {
    initial_rows <- build_college_cuts_unmatched_review_sheet_rows(
      discovered,
      leads_df = leads
    )
    if (!tab_exists) {
      ensure_google_sheet_tab(sheet_target, sheet_tab)
    }
    if (verbose) {
      message("Writing initial unmatched cuts review tab: ", sheet_tab)
    }
    googlesheets4::sheet_write(
      data = initial_rows[, COLLEGE_CUTS_UNMATCHED_REVIEW_SHEET_COLUMNS, drop = FALSE],
      ss = sheet_target,
      sheet = sheet_tab
    )
    appended <- nrow(initial_rows)
  } else {
    assert_college_cuts_unmatched_review_sheet_header(sheet_rows)
    assert_review_sheet_header_order(
      sheet_rows,
      COLLEGE_CUTS_UNMATCHED_REVIEW_SHEET_COLUMNS,
      sheet_tab
    )
    append_rows <- build_college_cuts_unmatched_review_sheet_append_rows(
      discovered,
      sheet_rows,
      leads_df = leads
    )
    if (nrow(append_rows)) {
      googlesheets4::sheet_append(
        ss = sheet_target,
        data = append_rows[, COLLEGE_CUTS_UNMATCHED_REVIEW_SHEET_COLUMNS, drop = FALSE],
        sheet = sheet_tab
      )
    }
    appended <- nrow(append_rows)
  }

  message(sprintf("college cuts unmatched review sheet rows appended: %d (tab: %s)", appended, sheet_tab))
  invisible(list(appended = appended, tab = sheet_tab))
}

if (sys.nframe() == 0) {
  main()
}
