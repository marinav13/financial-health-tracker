main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args <- parse_cli_args(cli_args)
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)
  has_flag <- function(flag) arg_has(args, flag)

  ensure_packages(c("dplyr", "readr", "digest", "jsonlite", "googlesheets4"))
  source(file.path(getwd(), "scripts", "shared", "editorial_review_helpers.R"))
  source(file.path(getwd(), "scripts", "shared", "google_sheets_helpers.R"))

  pipeline <- trim_text(get_arg_value("--pipeline", ""))
  if (!(pipeline %in% c("accreditation", "cuts"))) {
    stop("Provide --pipeline accreditation or --pipeline cuts.", call. = FALSE)
  }
  is_accreditation <- identical(pipeline, "accreditation")

  overrides_path <- get_arg_value(
    "--overrides",
    if (is_accreditation) {
      file.path(getwd(), "data_pipelines", "accreditation", "editorial_overrides.csv")
    } else {
      file.path(getwd(), "data_pipelines", "college_cuts", "editorial_overrides.csv")
    }
  )
  candidates_path <- get_arg_value(
    "--candidates",
    if (is_accreditation) {
      file.path(getwd(), "data_pipelines", "accreditation", "accreditation_review_candidates.csv")
    } else {
      file.path(getwd(), "data_pipelines", "college_cuts", "college_cuts_review_candidates.csv")
    }
  )
  tracker_index_path <- get_arg_value("--tracker-index", file.path(getwd(), "data", "schools_index.json"))
  default_sheet <- if (is_accreditation) {
    Sys.getenv("ACCREDITATION_REVIEW_SHEET_ID", unset = NA_character_)
  } else {
    Sys.getenv(
      "COLLEGE_CUTS_REVIEW_SHEET_ID",
      unset = Sys.getenv("ACCREDITATION_REVIEW_SHEET_ID", unset = NA_character_)
    )
  }
  sheet_id_or_url <- get_arg_value("--sheet", default_sheet)
  default_tab <- if (is_accreditation) {
    Sys.getenv("ACCREDITATION_REVIEW_SHEET_TAB", unset = "accreditation_review")
  } else {
    Sys.getenv("COLLEGE_CUTS_REVIEW_SHEET_TAB", unset = "college_cuts_review")
  }
  sheet_tab <- get_arg_value("--tab", default_tab)
  auth_json <- get_arg_value("--auth-json", Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = NA_character_))
  email <- get_arg_value("--email", NA_character_)
  cache_dir <- get_arg_value("--cache", file.path(getwd(), ".secrets", "googlesheets4"))
  verbose <- has_flag("--verbose")

  sheet_target <- trimws(as.character(sheet_id_or_url %||% ""))
  if (!nzchar(sheet_target)) {
    stop("Provide --sheet <Google Sheet URL or ID> or set the review sheet id environment variable.", call. = FALSE)
  }

  require_existing_local_file(
    overrides_path,
    sprintf("%s editorial overrides", pipeline),
    "Run the staging script first so editorial_overrides.csv exists."
  )
  require_existing_local_file(
    candidates_path,
    sprintf("%s review candidates", pipeline),
    "Run the export build first so the review candidates CSV exists."
  )

  if (is_accreditation) {
    overrides <- read_accreditation_editorial_overrides(overrides_path)
    candidates <- read_accreditation_review_candidates(candidates_path)
    sheet_staged <- filter_accreditation_overrides_for_review_sheet(
      overrides,
      candidate_action_ids = candidates$action_id
    )
    tracker_unitids <- NULL
  } else {
    require_existing_local_file(
      tracker_index_path,
      "tracker schools index",
      "Run the export build first so data/schools_index.json exists."
    )
    tracker_index <- jsonlite::fromJSON(tracker_index_path, simplifyVector = TRUE)
    tracker_unitids <- trim_text(as.character(tracker_index$unitid %||% character()))
    tracker_unitids <- unique(tracker_unitids[nzchar(tracker_unitids)])
    if (!length(tracker_unitids)) {
      stop(sprintf("Tracker schools index did not yield any unitids. Input: %s", tracker_index_path), call. = FALSE)
    }
    overrides <- read_college_cuts_editorial_overrides(overrides_path)
    candidates <- read_college_cuts_review_candidates(candidates_path)
    sheet_staged <- filter_college_cuts_overrides_for_review_sheet(
      overrides,
      candidate_cut_ids = candidates$cut_id,
      tracker_unitids = tracker_unitids
    )
  }

  authenticate_google_sheets(
    auth_json = auth_json,
    email = email,
    cache_dir = cache_dir,
    scopes = "spreadsheets",
    verbose = verbose
  )

  sheet_target <- extract_google_sheet_id(sheet_target)
  sheet_rows <- read_google_sheet_table(
    ss = sheet_target,
    sheet_name = sheet_tab,
    verbose = verbose
  )

  appended <- 0L
  if (!nrow(sheet_rows)) {
    initial_rows <- if (is_accreditation) {
      format_accreditation_review_sheet_headers(build_accreditation_review_sheet_rows(sheet_staged))
    } else {
      format_college_cuts_sheet_headers(
        build_college_cuts_review_sheet_rows(sheet_staged, tracker_unitids = tracker_unitids)
      )
    }
    if (verbose) {
      message("Writing initial review sheet tab: ", sheet_tab)
    }
    googlesheets4::sheet_write(data = initial_rows, ss = sheet_target, sheet = sheet_tab)
    appended <- nrow(initial_rows)
  } else if (is_accreditation) {
    assert_accreditation_review_sheet_header(sheet_rows)
    assert_review_sheet_header_order(
      sheet_rows,
      ACCREDITATION_REVIEW_SHEET_COLUMNS,
      sheet_tab,
      normalizer = normalize_accreditation_review_sheet_headers
    )
    coerced <- coerce_accreditation_review_sheet_rows(sheet_rows)
    append_rows <- build_accreditation_review_sheet_append_rows(sheet_staged, coerced)
    if (nrow(append_rows)) {
      googlesheets4::sheet_append(
        ss = sheet_target,
        data = format_accreditation_review_sheet_headers(
          append_rows[, ACCREDITATION_REVIEW_SHEET_COLUMNS, drop = FALSE]
        ),
        sheet = sheet_tab
      )
    }
    appended <- nrow(append_rows)
  } else {
    assert_college_cuts_review_sheet_header(sheet_rows)
    assert_review_sheet_header_order(
      sheet_rows,
      COLLEGE_CUTS_REVIEW_SHEET_COLUMNS,
      sheet_tab,
      normalizer = normalize_college_cuts_sheet_headers
    )
    coerced <- coerce_college_cuts_review_sheet_rows(sheet_rows)
    append_rows <- build_college_cuts_review_sheet_append_rows(
      sheet_staged,
      coerced,
      tracker_unitids = tracker_unitids
    )
    if (nrow(append_rows)) {
      googlesheets4::sheet_append(
        ss = sheet_target,
        data = format_college_cuts_sheet_headers(
          append_rows[, COLLEGE_CUTS_REVIEW_SHEET_COLUMNS, drop = FALSE]
        ),
        sheet = sheet_tab
      )
    }
    appended <- nrow(append_rows)
  }

  message(sprintf("%s review sheet rows appended: %d (tab: %s)", pipeline, appended, sheet_tab))
  invisible(list(pipeline = pipeline, appended = appended, tab = sheet_tab))
}

if (sys.nframe() == 0) {
  main()
}
