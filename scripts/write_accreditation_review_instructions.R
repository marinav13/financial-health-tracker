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
      "Purpose",
      "What appears here",
      "System columns",
      "Editor columns",
      "Review status",
      "When to edit the statement",
      "When to edit date or type",
      "When to edit source fields",
      "Grandfathered rows",
      "Duplicate handling",
      "Manual rows",
      "Publishing rule",
      "Do not change",
      "Hotfix workflow"
    ),
    guidance = c(
      "This tab explains how to review rows in accreditation_review before they publish to the site.",
      "Each row is one accreditation action staged from scraper or DAPIP-backed pipeline output.",
      "Columns A through L are pipeline-owned system columns. Treat them as read-only.",
      paste(
        "Editors are expected to use review_status, editor_action_label_short,",
        "editor_action_date, editor_action_type, editor_source_url, editor_source_title,",
        "editor_notes, reviewer, and reviewed_at."
      ),
      paste(
        "Use unreviewed for new rows, in_review while checking, approved when ready to publish,",
        "needs_revision if more work is required, and reject for rows that should never publish."
      ),
      paste(
        "Use editor_action_label_short when the generated statement is wrong, unclear, too long,",
        "or needs copy-editing. This is the main human-written visible label."
      ),
      paste(
        "Use editor_action_date or editor_action_type only when the staged values are materially wrong",
        "and the corrected values are supported by the source."
      ),
      paste(
        "Use editor_source_title to improve the public source caption. Use editor_source_url when the",
        "link target itself is wrong or a better public source page should be used."
      ),
      paste(
        "grandfathered = TRUE means the row was pre-approved during rollout. It does not lock the row.",
        "Editors can still update editor_action_label_short, editor_action_date, editor_action_type,",
        "editor_source_url, editor_source_title, editor_notes, reviewer, and reviewed_at while leaving",
        "review_status as approved."
      ),
      paste(
        "If a later scraper-backed or DAPIP-backed row appears to duplicate a manual editor-added row,",
        "compare both carefully. Approve the canonical replacement, then mark the older manual row reject.",
        "Do not assume the system will auto-merge duplicates."
      ),
      paste(
        "For manual rows, leave action_id blank in the editor workflow only if the dedicated manual-row",
        "sync path is in use. Otherwise ask the maintainer before adding rows directly."
      ),
      "Only rows with review_status = approved are eligible to publish once the gate is enabled.",
      paste(
        "Do not edit action_id, unitid, institution_name, accreditor, action_label_raw, generated_statement,",
        "row_origin, or first_seen directly."
      ),
      paste(
        "For urgent fixes on already published rows: update the editor columns, set reviewer and reviewed_at,",
        "leave review_status as approved, and trigger the publish workflow if needed."
      )
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
