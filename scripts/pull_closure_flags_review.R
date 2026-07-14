pull_closure_flags_review <- function(output_path,
                                      sheet_id_or_url,
                                      sheet_tab = "closure_flags_review",
                                      auth_json = Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = NA_character_),
                                      email = NA_character_,
                                      cache_dir = file.path(getwd(), ".secrets", "googlesheets4"),
                                      verbose = FALSE) {
  ensure_packages(c("readr", "googlesheets4"))
  source(file.path(getwd(), "scripts", "shared", "editorial_review_helpers.R"))
  source(file.path(getwd(), "scripts", "shared", "google_sheets_helpers.R"))

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
  sheet_rows <- read_google_sheet_table(
    ss = sheet_target,
    sheet_name = sheet_tab,
    verbose = verbose
  )
  assert_college_cuts_closure_flags_review_sheet_header(sheet_rows)

  if (!nrow(sheet_rows)) {
    stop(
      sprintf(
        "Google Sheet tab `%s` is empty or missing. Refusing to overwrite local closure_flags_review.csv.",
        sheet_tab
      ),
      call. = FALSE
    )
  }

  coerced <- coerce_college_cuts_closure_flags_review_sheet_rows(sheet_rows)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write_csv_atomic(coerced, output_path)
  message("Pulled closure flags review sheet values into: ", output_path)
  invisible(list(
    sheet_rows = nrow(coerced),
    output = output_path
  ))
}

main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args <- parse_cli_args(cli_args)
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)
  has_flag <- function(flag) arg_has(args, flag)

  output_path <- get_arg_value(
    "--output",
    file.path(getwd(), "data_pipelines", "college_cuts", "closure_flags_review.csv")
  )
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

  pull_closure_flags_review(
    output_path = output_path,
    sheet_id_or_url = sheet_id_or_url,
    sheet_tab = sheet_tab,
    auth_json = auth_json,
    email = email,
    cache_dir = cache_dir,
    verbose = verbose
  )
}

if (sys.nframe() == 0) {
  main()
}
