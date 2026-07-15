main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args <- parse_cli_args(cli_args)
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)
  has_flag <- function(flag) arg_has(args, flag)

  ensure_packages(c("googlesheets4", "readr"))
  source(file.path(getwd(), "scripts", "shared", "google_sheets_helpers.R"))

  sheet_id_or_url <- get_arg_value(
    "--sheet",
    Sys.getenv(
      "COLLEGE_CUTS_REVIEW_SHEET_ID",
      unset = Sys.getenv("ACCREDITATION_REVIEW_SHEET_ID", unset = NA_character_)
    )
  )
  tab_name <- get_arg_value("--tab", "college_cuts_review_instructions")
  auth_json <- get_arg_value("--auth-json", Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = NA_character_))
  email <- get_arg_value("--email", NA_character_)
  cache_dir <- get_arg_value("--cache", file.path(getwd(), ".secrets", "googlesheets4"))
  verbose <- has_flag("--verbose")

  sheet_target <- trimws(as.character(sheet_id_or_url %||% ""))
  if (!nzchar(sheet_target)) {
    stop(
      "Provide --sheet <Google Sheet URL or ID> or set COLLEGE_CUTS_REVIEW_SHEET_ID / ACCREDITATION_REVIEW_SHEET_ID.",
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
      "Statuses",
      "Statuses",
      "Statuses",
      "Where rows come from",
      "Where rows come from",
      "Where rows come from",
      "What to edit",
      "What to edit",
      "What to edit",
      "Manual rows",
      "Manual rows",
      "Unmatched tab",
      "Unmatched tab",
      "Unmatched tab",
      "Unmatched tab",
      "Closure flags tab",
      "Closure flags tab",
      "Closure flags tab",
      "IDs",
      "If something looks wrong"
    ),
    question = c(
      "What is this sheet for?",
      "What should I do first on a new row?",
      "unreviewed",
      "in_review",
      "approved / rejected",
      "What does row_origin mean?",
      "Why did the queue get so long in July 2026?",
      "Why do some rows mention model confidence?",
      "Fix the public wording",
      "Fix the date, type, or source",
      "Which cut_type values are allowed?",
      "Can I add rows myself?",
      "What fields are required for a manual row?",
      "What is the cuts_unmatched_review tab?",
      "How do I triage it?",
      "How do I promote an unmatched row for a school we DO track?",
      "What if the institution is not in the tracker at all?",
      "What is the closure_flags_review tab?",
      "What does badge_kind mean?",
      "What does flag_confirmed do?",
      "When can cut_id be blank?",
      "Who do I ask?"
    ),
    answer = c(
      "Each row is one staffing/program cut at a tracked institution. Approved rows publish to the site's cuts page and school profiles; nothing publishes without approval here.",
      "Verify the institution and unitid, the cut_type, the announcement date, the public text (edited_cut_text), and the source_url. Then set review_status.",
      "Brand-new row staged by the pipeline. Not on the site yet.",
      "You are checking it now. Leave reviewer_notes if useful.",
      "approved publishes with the next refresh; rejected permanently keeps it off the site. Both are useful - rejections tune the discovery filters.",
      "scraper = CollegeCuts API; manual / hechinger = added by a person; news_scan = our own news discovery (trade press + Google News); warn_notice = state WARN layoff filings.",
      "A one-time backfill swept news from Jan 2024 through mid-2026, staging roughly 100 historical candidates at once. The normal weekly flow is far smaller. Review in batches at your own pace.",
      "Discovery rows are pre-read by an AI model that drafts the summary and flags its confidence in the label (e.g. [model: high confidence]). It only pre-sorts; your review decides.",
      "Edit edited_cut_text directly. It becomes the public description when the row is approved.",
      "Edit announcement_date, cut_type, source_url, and source_publication directly when they are wrong and the source confirms the correction.",
      "staff_layoff, program_suspension, department_closure, campus_closure, institution_closure, hiring_freeze (also used for furloughs). institution_closure rows additionally flow to the closure_flags_review tab.",
      "Yes. Add the row at the bottom, leave cut_id blank, set row_origin = manual, and fill unitid, institution_name, state, announcement_date, cut_type, edited_cut_text, and source_url.",
      "unitid, institution_name, state, announcement_date, cut_type, edited_cut_text, source_url. IDs are generated on pull when cut_id is blank.",
      "Discovered cuts whose institution could not be matched to any of the ~1,890 tracked schools. Nothing on that tab can publish; it is a triage list, not a backlog.",
      "Skim for recognizable four-year colleges. Dismiss hospitals, community colleges, K-12, and foreign or sector-wide stories - note your call in resolution_status/resolution_notes. You do not need to clear every row.",
      "Do not copy the row by hand. Add one line to data_pipelines/college_cuts/manual_aliases.csv in the repo mapping the exact institution text from the unmatched row to the school's unitid (the unitid is in the school page URL). The next pipeline run re-matches it automatically and it appears on this tab as a normal unreviewed row - and that name matches forever after. Then mark the unmatched row resolved.",
      "It cannot be promoted. Cuts only display on tracked schools' pages, so out-of-scope entities have nowhere to render. Dismiss the row.",
      "One row per institution with a confirmed-or-pending institution-closure cut. It is the human gate for the site's closure/absorption badges: a badge only renders when flag_confirmed is TRUE here.",
      "closure = the institution announced it is shutting down. absorption = it is being absorbed by / merged into another institution and continues under the acquirer (e.g. NJCU into Kean). The badge wording on the site follows this column.",
      "TRUE renders the badge on the school's page at the next refresh; FALSE or blank keeps it off. You can revoke a badge at any time by flipping it back.",
      "Only manual rows may leave cut_id blank; the pull generates one. Never edit cut_id on existing rows - it is the row's identity across every system.",
      "Flag it to the data team rather than working around it. If a row looks corrupted or a column stops making sense, stop editing and report - the pipeline can restore state safely."
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
