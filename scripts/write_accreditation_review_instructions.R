main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args <- parse_cli_args(cli_args)
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)
  has_flag <- function(flag) arg_has(args, flag)

  ensure_packages(c("googlesheets4", "readr"))
  source(file.path(getwd(), "scripts", "shared", "google_sheets_helpers.R"))

  sheet_id_or_url <- get_arg_value("--sheet", Sys.getenv("ACCREDITATION_REVIEW_SHEET_ID", unset = NA_character_))
  tab_name <- get_arg_value("--tab", "accreditation_review_instructions")
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
      "What to edit",
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
      "Fix the public wording",
      "Fix the date",
      "Fix the action type or raw action text",
      "Fix the source link or source title",
      "unreviewed",
      "in_review",
      "approved",
      "Can I add rows myself?",
      "What fields are required for a manual row?",
      "Do I need to click a publish button every time?",
      "When can action_id be blank?",
      "Should I change IDs on existing rows?"
    ),
    answer = c(
      "Each row is one accreditation action. The visible row is the review surface for what should publish to the site.",
      "Check unitid, institution_name, accreditor, action_date, action_type, action_label_raw, generated_statement, source_url, and source_title. Then set the right review_status.",
      "Edit the visible fields directly. Do not use hidden helper columns or old editor_* columns.",
      "Edit generated_statement directly when the public wording is unclear, too long, inaccurate, or just needs copy editing.",
      "Edit action_date directly when the staged date is materially wrong and you can confirm the correction from the source.",
      "Edit action_type and action_label_raw directly when the staged classification or raw action text needs correction.",
      "Edit source_url and source_title directly when the link target or caption is wrong.",
      "Brand-new row. It is not ready for the site yet.",
      "You are checking it now. Leave notes if needed.",
      "Ready for the site. Approved rows are eligible to publish.",
      "Yes. Add the row at the bottom, leave action_id blank, and set row_origin = manual.",
      "Manual rows require institution_name, accreditor, action_date, action_type, action_label_raw, generated_statement, source_url, and source_title. unitid is optional if it is not known yet.",
      "Yes. There is no live sheet-to-GitHub auto-publish path anymore.",
      "Only manual rows may leave action_id blank. IDs are generated on pull for manual rows.",
      "No. Do not edit action_id on existing rows."
    ),
    stringsAsFactors = FALSE
  )

  googlesheets4::sheet_write(
    data = instructions,
    ss = sheet_target,
    sheet = tab_name
  )

  if (verbose) {
    message("Wrote accreditation review instructions tab: ", tab_name)
  }
  invisible(list(sheet = sheet_target, tab = tab_name, rows = nrow(instructions)))
}

if (sys.nframe() == 0) {
  main()
}
