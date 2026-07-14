#
# One-time Google Sheet schema migration utility for closure_flags_review.
#
main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args <- parse_cli_args(cli_args)
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)
  has_flag <- function(flag) arg_has(args, flag)

  ensure_packages(c("googlesheets4"))
  source(file.path(getwd(), "scripts", "shared", "editorial_review_helpers.R"))
  source(file.path(getwd(), "scripts", "shared", "google_sheets_helpers.R"))

  sheet_id_or_url <- get_arg_value(
    "--sheet",
    Sys.getenv(
      "COLLEGE_CUTS_REVIEW_SHEET_ID",
      unset = Sys.getenv("ACCREDITATION_REVIEW_SHEET_ID", unset = NA_character_)
    )
  )
  sheet_tab <- get_arg_value(
    "--tab",
    Sys.getenv("COLLEGE_CUTS_CLOSURE_FLAGS_REVIEW_SHEET_TAB", unset = "closure_flags_review")
  )
  auth_json <- get_arg_value("--auth-json", Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = NA_character_))
  email <- get_arg_value("--email", NA_character_)
  cache_dir <- get_arg_value("--cache", file.path(getwd(), ".secrets", "googlesheets4"))
  verbose <- has_flag("--verbose")

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

  authenticate_google_sheets(
    auth_json = auth_json,
    email = email,
    cache_dir = cache_dir,
    scopes = "spreadsheets",
    verbose = verbose
  )

  sheet_target <- extract_google_sheet_id(sheet_target)
  raw_rows <- read_google_sheet_table(
    ss = sheet_target,
    sheet_name = sheet_tab,
    verbose = verbose
  )
  migrated_rows <- coerce_college_cuts_closure_flags_review_sheet_rows(raw_rows)

  googlesheets4::sheet_write(
    data = migrated_rows[, COLLEGE_CUTS_CLOSURE_FLAGS_REVIEW_SHEET_COLUMNS, drop = FALSE],
    ss = sheet_target,
    sheet = sheet_tab
  )

  message("Closure flags review sheet rows before migration: ", nrow(raw_rows))
  message("Closure flags review sheet rows after migration: ", nrow(migrated_rows))
  if (verbose) print(utils::head(migrated_rows, 3L))

  invisible(list(
    before_rows = nrow(raw_rows),
    after_rows = nrow(migrated_rows),
    tab = sheet_tab
  ))
}

if (sys.nframe() == 0) {
  main()
}
