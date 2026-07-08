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
  force_discard <- has_flag("--force-discard-decisions")

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
  inactive_rows <- local_overrides$inactive %in% TRUE
  if (any(inactive_rows)) {
    message("Excluding ", sum(inactive_rows), " tombstoned (inactive) row(s) from the rewrite payload.")
    local_overrides <- local_overrides[!inactive_rows, , drop = FALSE]
  }
  rewritten_rows <- build_college_cuts_review_sheet_rows(local_overrides, tracker_unitids = tracker_unitids)

  authenticate_google_sheets(
    auth_json = auth_json,
    email = email,
    cache_dir = cache_dir,
    scopes = "spreadsheets",
    verbose = verbose
  )

  sheet_target <- extract_google_sheet_id(sheet_target)

  # Never blind-overwrite the live tab: inspect it first and refuse to
  # destroy or revert rows that carry editorial decisions (see the
  # 2026-07-06/07 wipe post-mortem in docs/PHASE0_PIPELINE_AUDIT.md).
  guard_result <- tryCatch({
    current_sheet_rows <- read_google_sheet_table(
      ss = sheet_target,
      sheet_name = sheet_tab,
      verbose = verbose
    )
    if (nrow(current_sheet_rows)) {
      find_review_rows_lost_by_rewrite(
        coerce_college_cuts_review_sheet_rows(current_sheet_rows),
        rewritten_rows,
        id_column = "cut_id"
      )
    } else {
      NULL
    }
  }, error = function(e) e)
  if (inherits(guard_result, "error")) {
    if (!isTRUE(force_discard)) {
      stop(sprintf(paste(
        "Could not inspect current tab `%s` before rewriting (%s).",
        "Refusing to overwrite a tab whose contents cannot be verified.",
        "Re-run with --force-discard-decisions to overwrite anyway."
      ), sheet_tab, conditionMessage(guard_result)), call. = FALSE)
    }
    message(sprintf(
      "--force-discard-decisions: current tab could not be inspected (%s); overwriting anyway.",
      conditionMessage(guard_result)
    ))
  } else if (!is.null(guard_result) && nrow(guard_result) > 0L) {
    dump_path <- file.path(getwd(), "data_pipelines", "college_cuts", "rewrite_blocked_decision_rows.csv")
    dir.create(dirname(dump_path), recursive = TRUE, showWarnings = FALSE)
    write_csv_atomic(guard_result, dump_path)
    sample_ids <- paste(utils::head(unique(trim_text(guard_result$cut_id)), 5L), collapse = ", ")
    if (!isTRUE(force_discard)) {
      stop(sprintf(paste(
        "Refusing to rewrite tab `%s`: %d sheet row(s) carry review decisions the rewrite would discard or revert.",
        "Their current values were saved to %s.",
        "Sample cut_id values: %s.",
        "Re-run with --force-discard-decisions to overwrite them anyway."
      ), sheet_tab, nrow(guard_result), dump_path, sample_ids), call. = FALSE)
    }
    message(sprintf(
      "--force-discard-decisions: overwriting %d decision-carrying sheet row(s); previous values saved to %s.",
      nrow(guard_result), dump_path
    ))
  }

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
