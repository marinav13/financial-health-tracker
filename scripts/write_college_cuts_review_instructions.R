main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args <- parse_cli_args(cli_args)
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)
  has_flag <- function(flag) arg_has(args, flag)

  ensure_packages(c("googlesheets4", "readr"))
  source(file.path(getwd(), "scripts", "shared", "google_sheets_helpers.R"))

  sheet_id_or_url <- get_arg_value("--sheet", Sys.getenv("ACCREDITATION_REVIEW_SHEET_ID", unset = NA_character_))
  tab_name <- get_arg_value("--tab", "college_cuts_review_instructions")
  auth_json <- get_arg_value("--auth-json", Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = NA_character_))
  email <- get_arg_value("--email", NA_character_)
  cache_dir <- get_arg_value("--cache", file.path(getwd(), ".secrets", "googlesheets4"))
  verbose <- has_flag("--verbose")

  sheet_target <- trimws(as.character(sheet_id_or_url %||% ""))
  if (!nzchar(sheet_target)) {
    stop(
      "Provide --sheet <Google Sheet URL or ID> or set ACCREDITATION_REVIEW_SHEET_ID.",
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

  instructions <- data.frame(
    section = c(
      "Start here",
      "Start here",
      "Direct edits",
      "Direct edits",
      "What to edit",
      "What to edit",
      "What to edit",
      "Statuses",
      "Statuses",
      "Statuses",
      "Manual rows",
      "Manual rows",
      "Publishing",
      "IDs",
      "If something looks wrong"
    ),
    question = c(
      "What is this sheet for?",
      "What should I do first on a new row?",
      "How should I edit reviewed values?",
      "Which columns are the live reviewed values?",
      "Fix the public cut description",
      "Fix the date or year",
      "Fix the cut type or source fields",
      "unreviewed / in_review",
      "approved",
      "needs_revision / reject",
      "Can I add rows myself?",
      "What fields are required for a manual row?",
      "Do I need to click a publish button every time?",
      "When can cut_id be blank?",
      "Should I change IDs on existing rows?"
    ),
    answer = c(
      "Each row is one college cut item. The visible row is the review surface for what should publish to the site.",
      "Check institution_name, state, announcement_date, announcement_year, cut_type, cut_description, source_url, and source_publication. Then set the right review_status.",
      "Edit the visible fields directly. Do not use hidden helper columns or old editor_* columns.",
      "The reviewed values are unitid, institution_name, state, announcement_date, announcement_year, cut_type, cut_description, source_url, and source_publication.",
      "Edit cut_description directly when the public wording is inaccurate, unclear, or too long.",
      "Edit announcement_date and announcement_year directly when the staged values are materially wrong and you can confirm the correction from the source.",
      "Edit cut_type, source_url, and source_publication directly when the staged values are wrong or a better public source should be used.",
      "Brand-new row or a row still being checked. It is not ready for the site yet.",
      "Ready for the site. Approved rows are the rows that should publish once the gate is enabled.",
      "Use needs_revision if more work is needed. Use reject if the row should never publish.",
      "Yes. Add the row at the bottom, leave cut_id blank, and set row_origin = manual.",
      "Manual rows require institution_name, state, announcement_date, cut_type, cut_description, source_url, and source_publication. unitid is optional if it is not known yet.",
      "Yes. There is no live sheet-to-GitHub auto-publish path anymore.",
      "Only manual rows may leave cut_id blank. IDs are generated on pull for manual rows.",
      "No. Do not edit cut_id on existing rows."
    ),
    stringsAsFactors = FALSE
  )

  googlesheets4::sheet_write(
    data = instructions,
    ss = sheet_target,
    sheet = tab_name
  )

  if (verbose) {
    message("Wrote college cuts review instructions tab: ", tab_name)
  }
  invisible(list(sheet = sheet_target, tab = tab_name, rows = nrow(instructions)))
}

if (sys.nframe() == 0) {
  main()
}
