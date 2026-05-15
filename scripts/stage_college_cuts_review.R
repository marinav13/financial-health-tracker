main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args <- parse_cli_args(cli_args)
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)
  has_flag <- function(flag) arg_has(args, flag)

  ensure_packages(c("dplyr", "readr", "digest"))
  source(file.path(getwd(), "scripts", "shared", "editorial_review_helpers.R"))

  input_path <- get_arg_value(
    "--input",
    file.path(getwd(), "data_pipelines", "college_cuts", "college_cuts_review_candidates.csv")
  )
  output_path <- get_arg_value(
    "--output",
    file.path(getwd(), "data_pipelines", "college_cuts", "editorial_overrides.csv")
  )
  first_seen <- get_arg_value("--first-seen", as.character(Sys.Date()))
  sheet_id_or_url <- get_arg_value("--sheet", Sys.getenv("ACCREDITATION_REVIEW_SHEET_ID", unset = NA_character_))
  sheet_tab <- get_arg_value("--tab", Sys.getenv("COLLEGE_CUTS_REVIEW_SHEET_TAB", unset = "college_cuts_review"))
  auth_json <- get_arg_value("--auth-json", Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = NA_character_))
  email <- get_arg_value("--email", NA_character_)
  cache_dir <- get_arg_value("--cache", file.path(getwd(), ".secrets", "googlesheets4"))
  rewrite_sheet <- has_flag("--rewrite-sheet")
  verbose <- has_flag("--verbose")

  require_existing_local_file(
    input_path,
    "college cuts review candidates",
    "Run `Rscript ./scripts/build_web_exports.R --input ...` first so the review candidates CSV exists."
  )

  candidates <- read_college_cuts_review_candidates(input_path)
  existing <- if (file.exists(output_path)) {
    read_college_cuts_editorial_overrides(output_path)
  } else {
    empty_college_cuts_editorial_overrides()
  }

  staged <- stage_college_cuts_editorial_overrides(
    candidates = candidates,
    existing = existing,
    first_seen = first_seen
  )

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write_csv_atomic(staged, output_path)

  new_count <- nrow(staged) - nrow(existing)
  sheet_append_count <- 0L

  sheet_target <- trimws(as.character(sheet_id_or_url %||% ""))
  if (nzchar(sheet_target)) {
    ensure_packages(c("googlesheets4"))
    source(file.path(getwd(), "scripts", "shared", "google_sheets_helpers.R"))

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
    assert_college_cuts_review_sheet_header(sheet_rows)
    sheet_rows <- coerce_college_cuts_editorial_overrides(sheet_rows)
    sheet_append_rows <- build_college_cuts_review_sheet_append_rows(staged, sheet_rows)
    sheet_append_count <- nrow(sheet_append_rows)

    if (isTRUE(rewrite_sheet)) {
      if (verbose) {
        message("Rewriting college cuts review sheet tab: ", sheet_tab)
      }
      googlesheets4::sheet_write(
        data = format_college_cuts_sheet_headers(
          staged[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]
        ),
        ss = sheet_target,
        sheet = sheet_tab
      )
      sheet_append_count <- nrow(staged)
    } else if (!nrow(sheet_rows)) {
      if (verbose) {
        message("Writing initial college cuts review sheet tab: ", sheet_tab)
      }
      googlesheets4::sheet_write(
        data = format_college_cuts_sheet_headers(
          staged[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]
        ),
        ss = sheet_target,
        sheet = sheet_tab
      )
    } else if (nrow(sheet_append_rows) > 0L) {
      if (verbose) {
        message("Appending ", nrow(sheet_append_rows), " college cuts review row(s) to tab: ", sheet_tab)
      }
      googlesheets4::sheet_append(
        ss = sheet_target,
        data = format_college_cuts_sheet_headers(
          sheet_append_rows[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]
        ),
        sheet = sheet_tab
      )
    } else if (verbose) {
      message("No new college cuts review rows to append to Google Sheet.")
    }
  }

  if (verbose) {
    message("College cuts review candidates: ", nrow(candidates))
    message("Editorial overrides before staging: ", nrow(existing))
  }
  message("Editorial overrides after staging: ", nrow(staged))
  message("New rows appended: ", new_count)
  if (nzchar(sheet_target)) {
    message("Google Sheet rows appended: ", sheet_append_count)
  }
  invisible(list(
    candidates = nrow(candidates),
    existing = nrow(existing),
    staged = nrow(staged),
    appended = new_count,
    sheet_appended = sheet_append_count,
    output = output_path
  ))
}

if (sys.nframe() == 0) {
  main()
}
