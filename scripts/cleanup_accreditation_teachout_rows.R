main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args <- parse_cli_args(cli_args)
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)
  has_flag <- function(flag) arg_has(args, flag)

  ensure_packages(c("dplyr", "readr"))
  source(file.path(getwd(), "scripts", "shared", "editorial_review_helpers.R"))

  overrides_input_path <- get_arg_value(
    "--overrides-input",
    file.path(getwd(), "data_pipelines", "accreditation", "editorial_overrides.csv")
  )
  overrides_output_path <- get_arg_value("--overrides-output", overrides_input_path)
  review_candidates_input_path <- get_arg_value(
    "--review-candidates-input",
    file.path(getwd(), "data_pipelines", "accreditation", "accreditation_review_candidates.csv")
  )
  review_candidates_output_path <- get_arg_value("--review-candidates-output", review_candidates_input_path)
  sheet_id_or_url <- get_arg_value("--sheet", Sys.getenv("ACCREDITATION_REVIEW_SHEET_ID", unset = NA_character_))
  sheet_tab <- get_arg_value("--tab", Sys.getenv("ACCREDITATION_REVIEW_SHEET_TAB", unset = "accreditation_review"))
  auth_json <- get_arg_value("--auth-json", Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = NA_character_))
  email <- get_arg_value("--email", NA_character_)
  cache_dir <- get_arg_value("--cache", file.path(getwd(), ".secrets", "googlesheets4"))
  verbose <- has_flag("--verbose")
  dry_run <- has_flag("--dry-run")

  removed_overrides <- 0L
  removed_review_candidates <- 0L
  removed_sheet_rows <- 0L

  if (file.exists(overrides_input_path)) {
    overrides <- read_accreditation_editorial_overrides(overrides_input_path)
    override_sheet_view <- build_accreditation_review_sheet_rows(overrides)
    drop_overrides <- compute_accreditation_teachout_process_mask(
      override_sheet_view,
      action_type_col = "action_type",
      action_label_raw_col = "action_label_raw",
      action_label_short_col = "generated_statement"
    )
    removed_overrides <- sum(drop_overrides & !(overrides$inactive %in% TRUE))
    # Tombstone instead of delete: the row keeps its id (so the strict pull
    # guard and staging dedup still know it) but is inactive for publication
    # and excluded from every sheet-facing view.
    cleaned_overrides <- overrides
    cleaned_overrides$inactive[drop_overrides] <- TRUE
    cleaned_overrides$inactive_reason[drop_overrides] <- "teachout_cleanup"
    cleaned_overrides <- cleaned_overrides[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]

    if (!isTRUE(dry_run)) {
      dir.create(dirname(overrides_output_path), recursive = TRUE, showWarnings = FALSE)
      write_csv_atomic(cleaned_overrides, overrides_output_path)
    }
  }

  if (file.exists(review_candidates_input_path)) {
    review_candidates <- read_accreditation_review_candidates(review_candidates_input_path)
    drop_review_candidates <- compute_accreditation_teachout_process_mask(
      review_candidates,
      action_type_col = "action_type",
      action_label_raw_col = "action_label_raw",
      action_label_short_col = "generated_statement"
    )
    removed_review_candidates <- sum(drop_review_candidates)
    cleaned_review_candidates <- review_candidates[!drop_review_candidates, ACCREDITATION_REVIEW_CANDIDATE_COLUMNS, drop = FALSE]

    if (!isTRUE(dry_run)) {
      dir.create(dirname(review_candidates_output_path), recursive = TRUE, showWarnings = FALSE)
      write_csv_atomic(cleaned_review_candidates, review_candidates_output_path)
    }
  }

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
    assert_accreditation_review_sheet_header(sheet_rows)
    sheet_rows <- coerce_accreditation_review_sheet_rows(sheet_rows)

    drop_sheet_rows <- compute_accreditation_teachout_process_mask(
      sheet_rows,
      action_type_col = "action_type",
      action_label_raw_col = "action_label_raw",
      action_label_short_col = "generated_statement"
    )
    removed_sheet_rows <- sum(drop_sheet_rows)
    cleaned_sheet_rows <- sheet_rows[!drop_sheet_rows, ACCREDITATION_REVIEW_SHEET_COLUMNS, drop = FALSE]

    if (!isTRUE(dry_run)) {
      googlesheets4::sheet_write(
        data = format_accreditation_review_sheet_headers(cleaned_sheet_rows),
        ss = sheet_target,
        sheet = sheet_tab
      )
    }
  }

  if (isTRUE(dry_run)) {
    message("Dry run only: no files or Google Sheet tabs were rewritten.")
  }
  message("Tombstoned teach-out rows in local editorial overrides: ", removed_overrides)
  message("Removed teach-out rows from local review candidates: ", removed_review_candidates)
  if (nzchar(sheet_target)) {
    message("Removed teach-out rows from Google Sheet tab: ", removed_sheet_rows)
  }

  invisible(list(
    removed_overrides = removed_overrides,
    removed_review_candidates = removed_review_candidates,
    removed_sheet_rows = removed_sheet_rows
  ))
}

if (sys.nframe() == 0) {
  main()
}
