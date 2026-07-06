pull_college_cuts_overrides <- function(input_path,
                                        output_path = input_path,
                                        sheet_id_or_url,
                                        candidates_path = file.path(getwd(), "data_pipelines", "college_cuts", "college_cuts_review_candidates.csv"),
                                        sheet_tab = "college_cuts_review",
                                        auth_json = Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = NA_character_),
                                        email = NA_character_,
                                        cache_dir = file.path(getwd(), ".secrets", "googlesheets4"),
                                        verbose = FALSE) {
  ensure_packages(c("dplyr", "readr", "googlesheets4"))
  source(file.path(getwd(), "scripts", "shared", "editorial_review_helpers.R"))
  source(file.path(getwd(), "scripts", "shared", "google_sheets_helpers.R"))

  require_existing_local_file(
    input_path,
    "college cuts editorial overrides",
    "Run `Rscript ./scripts/stage_college_cuts_review.R --verbose` first so editorial_overrides.csv exists."
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

  local_overrides <- read_college_cuts_editorial_overrides(input_path)

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

  if (!nrow(sheet_rows)) {
    stop(
      sprintf(
        "Google Sheet tab `%s` is empty or missing. Refusing to overwrite local editorial_overrides.csv.",
        sheet_tab
      ),
      call. = FALSE
    )
  }

  candidate_ids <- character()
  if (!is.null(candidates_path) && file.exists(candidates_path)) {
    candidate_ids <- read_college_cuts_review_candidates(candidates_path)$cut_id
  }
  filtered_sheet <- drop_stale_college_cuts_sheet_rows(
    sheet_rows = sheet_rows,
    local_cut_ids = local_overrides$cut_id,
    candidate_cut_ids = candidate_ids
  )
  if (verbose && nrow(filtered_sheet$dropped_rows) > 0L) {
    message(
      "Ignoring stale sheet-only scraper row(s) that are absent from both local overrides and current candidates: ",
      nrow(filtered_sheet$dropped_rows)
    )
  }
  sheet_rows <- filtered_sheet$kept_rows

  merged <- merge_college_cuts_review_sheet_editor_columns(
    overrides = local_overrides,
    sheet_rows = sheet_rows,
    allow_editor_added_rows = allow_editor_added_rows
  )

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write_csv_atomic(merged, output_path)

  if (verbose) {
    message("Local editorial overrides rows: ", nrow(local_overrides))
    message("Google Sheet review rows: ", nrow(sheet_rows))
  }
  message("Pulled college cuts review sheet values into: ", output_path)
  invisible(list(
    local_rows = nrow(local_overrides),
    sheet_rows = nrow(sheet_rows),
    output = output_path
  ))
}

main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args <- parse_cli_args(cli_args)
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)
  has_flag <- function(flag) arg_has(args, flag)

  input_path <- get_arg_value(
    "--input",
    file.path(getwd(), "data_pipelines", "college_cuts", "editorial_overrides.csv")
  )
  output_path <- get_arg_value("--output", input_path)
  candidates_path <- get_arg_value(
    "--candidates",
    file.path(getwd(), "data_pipelines", "college_cuts", "college_cuts_review_candidates.csv")
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
  allow_editor_added_rows <- has_flag("--allow-editor-added-rows")

  pull_college_cuts_overrides(
    input_path = input_path,
    output_path = output_path,
    sheet_id_or_url = sheet_id_or_url,
    candidates_path = candidates_path,
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
