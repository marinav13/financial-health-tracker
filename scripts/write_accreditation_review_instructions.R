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
      "Start here",
      "What to edit",
      "What to edit",
      "What to edit",
      "What to edit",
      "What not to edit",
      "Statuses",
      "Statuses",
      "Statuses",
      "Statuses",
      "Current rows",
      "Urgent fixes",
      "Automatic publishing",
      "Automatic publishing",
      "Duplicates",
      "Manual rows",
      "If something looks wrong"
    ),
    question = c(
      "What is this sheet for?",
      "What should I do first on a new row?",
      "What is the main editing field?",
      "Fix the public wording",
      "Fix the date",
      "Fix the action type",
      "Fix the source link or source title",
      "Do not edit these columns",
      "unreviewed",
      "in_review",
      "approved",
      "needs_revision / reject",
      "Can I still edit old approved rows?",
      "How do I make a quick copy edit live?",
      "Do I need to click a publish button every time?",
      "What triggers auto-publish?",
      "What if a new scraper row duplicates a manual row?",
      "Can I add rows myself?",
      "What if the generated statement is blank or obviously wrong?"
    ),
    answer = c(
      "Each row is one accreditation action. Review it before it appears on the public site.",
      "Check the generated statement, source, date, and type. Then set the right review_status.",
      "Use editor_action_label_short. This is the main human-written label that can appear on the site.",
      "Use editor_action_label_short when the generated statement is unclear, too long, inaccurate, or just needs copy editing.",
      "Use editor_action_date only when the staged date is materially wrong and you can confirm the correction from the source.",
      "Use editor_action_type only when the staged type is materially wrong and you can confirm the correction from the source.",
      "Use editor_source_title to improve the caption. Use editor_source_url when the link target itself is wrong or a better public source should be used.",
      "Do not edit action_id, unitid, institution_name, accreditor, action_label_raw, generated_statement, source_url, source_title, row_origin, first_seen, or grandfathered.",
      "Brand-new row. It is not ready for the site yet.",
      "You are checking it now. Leave notes if needed.",
      "Ready for the site. Approved rows are eligible to publish.",
      "Use needs_revision if more work is needed. Use reject if the row should never publish.",
      "Yes. grandfathered = TRUE only means the row was pre-approved during rollout. It does not lock the row.",
      "Edit the editor_* fields, leave review_status = approved, and the auto-publish system should pick it up on the next cycle. Manual GitHub publish is still available as a backup.",
      "Usually no. The normal path is automatic.",
      "The sheet marks itself dirty when an approved row's publish-relevant fields change, or when review_status changes. A background trigger checks every ~15 minutes and starts the GitHub publish workflow if needed.",
      "Compare the rows carefully. If they are the same action, keep the scraper-backed row as the long-term record and mark the older manual row reject after the replacement is approved.",
      "Only with the manual-row workflow. If you are not sure, ask the maintainer before adding rows directly.",
      "Flag it in editor_notes, fix the editor fields you can confirm, and keep review_status off approved until the row is truly ready."
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
