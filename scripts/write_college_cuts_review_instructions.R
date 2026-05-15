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
      "What to edit",
      "What to edit",
      "What to edit",
      "What to edit",
      "What not to edit",
      "Statuses",
      "Statuses",
      "Statuses",
      "Current rows",
      "Urgent fixes",
      "Automatic publishing",
      "Duplicates",
      "If something looks wrong"
    ),
    question = c(
      "What is this sheet for?",
      "What should I do first on a new row?",
      "Fix the public cut description",
      "Fix the date",
      "Fix the cut type",
      "Fix the source information",
      "Do not edit these columns",
      "unreviewed / in_review",
      "approved",
      "needs_revision / reject",
      "Can I still edit old approved rows?",
      "How do I make a quick copy edit live?",
      "Do I need to click a publish button every time?",
      "What if two rows seem to describe the same cut?",
      "What if the cut description is wrong or too long?"
    ),
    answer = c(
      "Each row is one college cut item from the joined cuts pipeline. Review it before it becomes the version shown on the site.",
      "Check the cut description, date, cut type, and source fields. Then set the right review_status.",
      "Use editor_cut_description when the visible cut description is inaccurate, unclear, too long, or just needs copy editing.",
      "Use editor_announcement_date only when the staged date is materially wrong and you can confirm the correction from the source.",
      "Use editor_cut_type only when the staged type is materially wrong and you can confirm the correction from the source.",
      "Use editor_source_publication to improve the visible news outlet name. Use editor_source_title for better internal metadata, and editor_source_url when the link target itself is wrong or a better public source should be used.",
      "Do not edit cut_id, unitid, institution_name, state, announcement_date, announcement_year, cut_type, cut_description, source_url, source_title, source_publication, row_origin, first_seen, or grandfathered directly.",
      "Brand-new row or a row still being checked. It is not ready for the site yet.",
      "Ready for the site. Approved rows are the rows that should publish once the gate is enabled.",
      "Use needs_revision if more work is needed. Use reject if the row should never publish.",
      "Yes. grandfathered = TRUE only means the row was pre-approved during rollout. It does not lock the row.",
      "Edit the editor_* fields, leave review_status = approved, and the auto-publish system should pick it up on the next cycle. Manual GitHub publish is still available as a backup.",
      "Usually no. The normal path is automatic once the Apps Script automation is turned on.",
      "Compare them carefully. If they are the same cut, keep the cleaner or more canonical row as the long-term record and reject the duplicate after the replacement is approved.",
      "Use editor_cut_description for the corrected public wording, add a note in editor_notes if helpful, and keep review_status off approved until the row is truly ready."
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
