main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args <- parse_cli_args(cli_args)
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)
  has_flag <- function(flag) arg_has(args, flag)

  ensure_packages(c("googlesheets4", "jsonlite"))
  source(file.path(getwd(), "scripts", "shared", "editorial_review_helpers.R"))
  source(file.path(getwd(), "scripts", "shared", "google_sheets_helpers.R"))

  input_path <- get_arg_value(
    "--input",
    file.path(getwd(), "data_pipelines", "college_cuts", "editorial_overrides.csv")
  )
  tracker_index_path <- get_arg_value(
    "--tracker-index",
    file.path(getwd(), "data", "schools_index.json")
  )
  sheet_id_or_url <- get_arg_value(
    "--sheet",
    Sys.getenv(
      "COLLEGE_CUTS_REVIEW_SHEET_ID",
      unset = Sys.getenv("ACCREDITATION_REVIEW_SHEET_ID", unset = NA_character_)
    )
  )
  sheet_tab <- get_arg_value("--tab", Sys.getenv("COLLEGE_CUTS_REVIEW_SHEET_TAB", unset = "college_cuts_review"))
  auth_json <- get_arg_value("--auth-json", Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = NA_character_))
  email <- get_arg_value("--email", NA_character_)
  cache_dir <- get_arg_value("--cache", file.path(getwd(), ".secrets", "googlesheets4"))
  verbose <- has_flag("--verbose")

  require_existing_local_file(
    input_path,
    "college cuts editorial overrides",
    "Run `Rscript ./scripts/stage_college_cuts_review.R --verbose` first so editorial_overrides.csv exists."
  )
  require_existing_local_file(
    tracker_index_path,
    "tracker schools index",
    "Run `Rscript ./scripts/build_web_exports.R --only cuts` first so data/schools_index.json exists."
  )

  sheet_target <- trimws(as.character(sheet_id_or_url %||% ""))
  if (!nzchar(sheet_target)) {
    stop(
      paste(
        "Provide --sheet <Google Sheet URL or ID> or set",
        "COLLEGE_CUTS_REVIEW_SHEET_ID / ACCREDITATION_REVIEW_SHEET_ID."
      ),
      call. = FALSE
    )
  }

  tracker_index <- jsonlite::fromJSON(tracker_index_path, simplifyVector = TRUE)
  tracker_unitids <- trim_text(as.character(tracker_index$unitid %||% character()))
  tracker_unitids <- unique(tracker_unitids[nzchar(tracker_unitids)])
  if (!length(tracker_unitids)) {
    stop(
      paste(
        "Tracker schools index did not yield any unitids.",
        sprintf("Input: %s", tracker_index_path)
      ),
      call. = FALSE
    )
  }

  local_overrides <- read_college_cuts_editorial_overrides(input_path)
  local_overrides <- filter_college_cuts_overrides_for_tracker_scope(
    local_overrides,
    tracker_unitids = tracker_unitids,
    context = "College cuts editorial overrides"
  )
  rewritten_rows <- build_college_cuts_review_sheet_rows(local_overrides, tracker_unitids = tracker_unitids)

  authenticate_google_sheets(
    auth_json = auth_json,
    email = email,
    cache_dir = cache_dir,
    scopes = "spreadsheets",
    verbose = verbose
  )

  sheet_target <- extract_google_sheet_id(sheet_target)
  googlesheets4::sheet_write(
    data = format_college_cuts_sheet_headers(
      rewritten_rows[, COLLEGE_CUTS_REVIEW_SHEET_COLUMNS, drop = FALSE]
    ),
    ss = sheet_target,
    sheet = sheet_tab
  )

  message("College cuts review sheet rows written: ", nrow(rewritten_rows))
  if (verbose) print(utils::head(rewritten_rows, 3L))

  invisible(list(
    rows_written = nrow(rewritten_rows),
    tab = sheet_tab
  ))
}

if (sys.nframe() == 0) {
  main()
}
