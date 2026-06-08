if (!exists("run_test", mode = "function")) {
  source(file.path(getwd(), "tests", "test_support.R"))
}

run_test("Text trim helpers preserve zero-length vectors and normalize scalar NA values", function() {
  assert_identical(trim_text(character(0)), character(0))
  assert_identical(trim_optional_text(character(0)), character(0))
  assert_identical(derive_year_from_date_string(character(0)), character(0))
  assert_identical(trim_text(NA_character_), "")
  assert_true(is.na(trim_optional_text(NA_character_)))
  assert_true(is.na(derive_year_from_date_string(NA_character_)))
})

run_test("Accreditation legacy sheet rows normalize into the new visible schema", function() {
  legacy_rows <- data.frame(
    action_id = c("act-1", "act-2"),
    unitid = c("100", "101"),
    institution_name = c("Example U", "Example V"),
    accreditor = c("MSCHE", "HLC"),
    action_date = c("2026-04-24", "2026-05-01"),
    action_type = c("warning", "notice"),
    action_label_raw = c("Warning", "Notice"),
    generated_statement = c("Generated warning", "Generated notice"),
    source_url = c("https://example.org/one", "https://example.org/two"),
    source_title = c("Source one", "Source two"),
    row_origin = c("scraper", "scraper"),
    first_seen = c("2026-05-01", "2026-05-02"),
    review_status = c("approved", "unreviewed"),
    editor_action_label_short = c("Edited warning", NA_character_),
    editor_action_type = c("adverse_action", NA_character_),
    editor_source_url = c("https://example.org/editor-one", NA_character_),
    editor_notes = c("checked", NA_character_),
    reviewer = c("editor@example.org", NA_character_),
    reviewed_at = c("2026-05-03", NA_character_),
    grandfathered = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  normalized <- coerce_accreditation_review_sheet_rows(legacy_rows)
  assert_identical(nrow(normalized), 2L)
  assert_identical(normalized$generated_statement[[1]], "Edited warning")
  assert_identical(normalized$action_type[[1]], "adverse_action")
  assert_identical(normalized$source_url[[1]], "https://example.org/editor-one")
  assert_identical(normalized$reviewer_notes[[1]], "checked")
  assert_identical(normalized$review_status[[1]], "approved")
  assert_identical(normalized$review_status[[2]], "unreviewed")
})

run_test("Committed accreditation overrides in legacy CSV shape still coerce", function() {
  legacy_overrides_path <- file.path(root, "data_pipelines", "accreditation", "editorial_overrides.csv")
  legacy_overrides <- read.csv(legacy_overrides_path, stringsAsFactors = FALSE, check.names = FALSE)

  normalized <- coerce_accreditation_editorial_overrides(legacy_overrides)
  assert_true(nrow(normalized) >= 1L)
  assert_true("override_unitid" %in% names(normalized))
  assert_identical(normalized$source_row_origin[[1]], "scraper")
})

run_test("Committed college cuts overrides in legacy CSV shape still coerce", function() {
  legacy_overrides_path <- file.path(root, "data_pipelines", "college_cuts", "editorial_overrides.csv")
  legacy_overrides <- read.csv(legacy_overrides_path, stringsAsFactors = FALSE, check.names = FALSE)

  normalized <- coerce_college_cuts_editorial_overrides(legacy_overrides)
  assert_true(nrow(normalized) >= 1L)
  assert_true("override_unitid" %in% names(normalized))
  assert_identical(normalized$source_row_origin[[1]], "scraper")
})

run_test("College cuts sheet rows accept Hechinger as a human-authored row_origin", function() {
  sheet_rows <- data.frame(
    cut_id = "",
    unitid = "100",
    institution_name = "Example University",
    state = "Massachusetts",
    announcement_date = "2026-06-01",
    announcement_year = "2026",
    cut_type = "program_closure",
    cut_description = "History BA closure",
    source_url = "https://example.org/hechinger-cut",
    source_publication = "The Hechinger Report",
    row_origin = "Hechinger",
    review_status = "approved",
    stringsAsFactors = FALSE
  )

  normalized <- coerce_college_cuts_review_sheet_rows(
    sheet_rows,
    default_first_seen = "2026-06-06"
  )

  assert_identical(nrow(normalized), 1L)
  assert_identical(normalized$row_origin[[1]], "hechinger")
  assert_true(nzchar(trim_text(normalized$cut_id[[1]])))
  assert_identical(normalized$first_seen[[1]], "2026-06-06")
})

run_test("College cuts unsupported row_origin errors include the literal bad value", function() {
  bad_rows <- data.frame(
    cut_id = "cut-1",
    unitid = "100",
    institution_name = "Example University",
    state = "Massachusetts",
    announcement_date = "2026-06-01",
    announcement_year = "2026",
    cut_type = "layoff",
    cut_description = "Ten staff layoffs",
    source_url = "https://example.org/bad-origin",
    source_publication = "Example outlet",
    row_origin = "BadOrigin",
    review_status = "unreviewed",
    stringsAsFactors = FALSE
  )

  err <- tryCatch(
    {
      coerce_college_cuts_review_sheet_rows(bad_rows)
      NULL
    },
    error = identity
  )

  assert_true(!is.null(err), "Unsupported college cuts row_origin should fail validation.")
  assert_true(
    grepl("cut_id cut-1 has row_origin='BadOrigin'", conditionMessage(err), fixed = TRUE),
    "The validation error should include the literal bad row_origin value."
  )
  assert_true(
    grepl("Supported values: scraper, manual, hechinger", conditionMessage(err), fixed = TRUE),
    "The validation error should list the allowed college cuts row_origin values."
  )
})

run_test("Approved college cuts local overrides publish without sheet helpers", function() {
  cuts_df <- data.frame(
    cut_id = "cut-1",
    matched_unitid = "100",
    export_unitid = "100",
    institution_name_display = "Example University",
    state_display = "Alabama",
    announcement_date = "2026-04-24",
    announcement_year = 2026L,
    cut_type = "program_closure",
    program_name = "History BA",
    source_url = "https://example.org/pipeline-cut",
    source_title = "Pipeline source",
    source_publication = "Pipeline paper",
    stringsAsFactors = FALSE
  )

  overrides <- data.frame(
    cut_id = "cut-1",
    source_unitid = "100",
    source_institution_name = "Example University",
    source_state = "Alabama",
    source_announcement_date = "2026-04-24",
    source_announcement_year = "2026",
    source_cut_type = "program_closure",
    source_cut_description = "History BA",
    source_source_url = "https://example.org/pipeline-cut",
    source_source_title = "Pipeline source",
    source_source_publication = "Pipeline paper",
    source_row_origin = "scraper",
    override_unitid = "200",
    override_institution_name = "Corrected University",
    override_state = "Georgia",
    override_announcement_date = "2026-04-30",
    override_announcement_year = "2026",
    override_cut_type = "restructuring",
    override_cut_description = "Corrected History BA",
    override_source_url = "https://example.org/corrected-cut",
    override_source_title = "Corrected source",
    override_source_publication = "Corrected paper",
    first_seen = "2026-05-27",
    review_status = "approved",
    reviewer = "editor@example.org",
    reviewer_notes = "checked",
    reviewed_at = "2026-05-28",
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  )

  applied <- apply_college_cuts_editorial_overrides(cuts_df, overrides, enforce_review_gate = FALSE)

  assert_identical(nrow(applied), 1L)
  assert_identical(applied$matched_unitid[[1]], "200")
  assert_identical(applied$institution_name_display[[1]], "Corrected University")
  assert_identical(applied$state_display[[1]], "Georgia")
  assert_identical(applied$cut_type[[1]], "restructuring")
  assert_identical(applied$program_name[[1]], "Corrected History BA")
  assert_identical(applied$source_url[[1]], "https://example.org/corrected-cut")
  assert_identical(applied$source_title[[1]], "Corrected source")
  assert_identical(applied$source_publication[[1]], "Corrected paper")
})

run_test("College cuts visible-field edits merge from the sheet and publish", function() {
  cuts_df <- data.frame(
    cut_id = "cut-1",
    matched_unitid = "100",
    export_unitid = "100",
    institution_name_display = "Example University",
    state_display = "Alabama",
    announcement_date = "2026-04-24",
    announcement_year = 2026L,
    cut_type = "program_closure",
    program_name = "History BA",
    source_url = "https://example.org/pipeline-cut",
    source_title = "Pipeline source",
    source_publication = "Pipeline paper",
    is_primary_tracker = TRUE,
    stringsAsFactors = FALSE
  )

  candidates <- build_college_cuts_review_candidates(cuts_df, tracker_unitids = "100")
  staged <- stage_college_cuts_editorial_overrides(
    candidates,
    first_seen = "2026-05-27",
    tracker_unitids = "100"
  )
  edited_sheet_rows <- build_college_cuts_review_sheet_rows(staged, tracker_unitids = "100")
  edited_sheet_rows$unitid[[1]] <- "200"
  edited_sheet_rows$institution_name[[1]] <- "Corrected University"
  edited_sheet_rows$state[[1]] <- "Georgia"
  edited_sheet_rows$announcement_date[[1]] <- "2026-04-30"
  edited_sheet_rows$announcement_year[[1]] <- "2026"
  edited_sheet_rows$cut_type[[1]] <- "restructuring"
  edited_sheet_rows$cut_description[[1]] <- "Corrected History BA"
  edited_sheet_rows$source_url[[1]] <- "https://example.org/corrected-cut"
  edited_sheet_rows$source_publication[[1]] <- "Corrected paper"
  edited_sheet_rows$review_status[[1]] <- "approved"
  edited_sheet_rows$reviewer[[1]] <- "editor@example.org"
  edited_sheet_rows$reviewer_notes[[1]] <- "checked"
  edited_sheet_rows$reviewed_at[[1]] <- "2026-05-28"

  merged <- merge_college_cuts_review_sheet_editor_columns(
    staged,
    edited_sheet_rows,
    first_seen = "2026-05-27"
  )
  applied <- apply_college_cuts_editorial_overrides(cuts_df, merged, enforce_review_gate = FALSE)

  assert_identical(nrow(applied), 1L)
  assert_identical(applied$matched_unitid[[1]], "200")
  assert_identical(applied$institution_name_display[[1]], "Corrected University")
  assert_identical(applied$state_display[[1]], "Georgia")
  assert_identical(applied$announcement_date[[1]], "2026-04-30")
  assert_identical(applied$announcement_year[[1]], 2026L)
  assert_identical(applied$cut_type[[1]], "restructuring")
  assert_identical(applied$program_name[[1]], "Corrected History BA")
  assert_identical(applied$source_url[[1]], "https://example.org/corrected-cut")
  assert_identical(applied$source_publication[[1]], "Corrected paper")
})

run_test("Manual accreditation rows with blank ids get stable generated ids", function() {
  manual_rows <- data.frame(
    action_id = "",
    unitid = "",
    institution_name = "Manual University",
    accreditor = "MSCHE",
    action_date = "2026-06-01",
    action_type = "warning",
    action_label_raw = "Issued warning",
    generated_statement = "Issued warning",
    source_url = "https://example.org/manual-accreditation",
    source_title = "Manual source",
    row_origin = "manual",
    review_status = "approved",
    stringsAsFactors = FALSE
  )

  first <- coerce_accreditation_review_sheet_rows(manual_rows, default_first_seen = "2026-05-27")
  second <- coerce_accreditation_review_sheet_rows(manual_rows, default_first_seen = "2026-05-27")

  assert_identical(first$action_id[[1]], second$action_id[[1]])
  assert_identical(nchar(first$action_id[[1]]), 12L)
  assert_identical(first$first_seen[[1]], "2026-05-27")
})

run_test("Sheet-only manual accreditation rows merge without error", function() {
  local_overrides <- empty_accreditation_editorial_overrides()
  sheet_rows <- data.frame(
    action_id = "",
    unitid = "",
    institution_name = "Manual University",
    accreditor = "MSCHE",
    action_date = "2026-06-01",
    action_type = "warning",
    action_label_raw = "Issued warning",
    generated_statement = "Issued warning",
    source_url = "https://example.org/manual-accreditation",
    source_title = "Manual source",
    row_origin = "manual",
    first_seen = "2026-05-27",
    review_status = "approved",
    reviewer = "editor@example.org",
    reviewer_notes = "manual",
    reviewed_at = "2026-05-27",
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  )

  merged <- merge_accreditation_review_sheet_editor_columns(local_overrides, sheet_rows)
  assert_identical(nrow(merged), 1L)
  assert_identical(merged$source_row_origin[[1]], "manual")
  assert_identical(merged$review_status[[1]], "approved")
  assert_identical(merged$reviewer_notes[[1]], "manual")
})

run_test("Accreditation visible-field edits publish and approved manual rows append", function() {
  actions_df <- data.frame(
    export_unitid = "100",
    unitid = "100",
    export_institution_name = "Example University",
    accreditor = "MSCHE",
    action_date = "2026-04-24",
    action_type = "warning",
    action_label_raw = "Warning",
    action_label_short = "Generated warning",
    source_url = "https://example.org/pipeline-action",
    source_title = "Pipeline source",
    source_page_url = "https://example.org/pipeline-action",
    stringsAsFactors = FALSE
  )

  staged <- stage_accreditation_editorial_overrides(
    build_accreditation_review_candidates(actions_df),
    first_seen = "2026-05-27"
  )
  edited_sheet_rows <- data.frame(
    action_id = build_accreditation_review_sheet_rows(staged)$action_id[[1]],
    unitid = "200",
    institution_name = "Corrected University",
    accreditor = "MSCHE",
    action_date = "2026-04-30",
    action_type = "adverse_action",
    action_label_raw = "Warning",
    generated_statement = "Corrected warning statement",
    source_url = "https://example.org/corrected-action",
    source_title = "Corrected source",
    row_origin = "scraper",
    first_seen = "2026-05-27",
    review_status = "approved",
    reviewer = "editor@example.org",
    reviewer_notes = "checked",
    reviewed_at = "2026-05-28",
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  )
  manual_sheet_row <- data.frame(
    action_id = "",
    unitid = "",
    institution_name = "Manual University",
    accreditor = "NECHE",
    action_date = "2026-06-01",
    action_type = "warning",
    action_label_raw = "Issued warning",
    generated_statement = "Manual accreditation action",
    source_url = "https://example.org/manual-accreditation",
    source_title = "Manual source",
    row_origin = "manual",
    first_seen = "2026-05-27",
    review_status = "approved",
    reviewer = "editor@example.org",
    reviewer_notes = "manual",
    reviewed_at = "2026-05-28",
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  )

  merged <- merge_accreditation_review_sheet_editor_columns(
    staged,
    dplyr::bind_rows(edited_sheet_rows, manual_sheet_row),
    first_seen = "2026-05-27"
  )
  applied <- apply_accreditation_editorial_overrides(actions_df, merged, enforce_review_gate = FALSE)

  assert_identical(nrow(applied), 2L)
  assert_identical(applied$unitid[[1]], "200")
  assert_identical(applied$export_institution_name[[1]], "Corrected University")
  assert_identical(applied$action_type[[1]], "adverse_action")
  assert_identical(applied$action_label_short[[1]], "Corrected warning statement")
  assert_identical(applied$source_url[[1]], "https://example.org/corrected-action")
  assert_true(any(applied$row_origin == "manual"))
  assert_true(any(applied$action_label_short == "Manual accreditation action"))
})

run_test("Apply-only accreditation publish reuses the approved reviewed statement when the committed tracker row is poorer", function() {
  reviewed_actions_df <- data.frame(
    export_unitid = "198695",
    unitid = "198695",
    export_institution_name = "High Point University",
    accreditor = "SACSCOC",
    action_date = "2023-06-15",
    action_type = "notice",
    action_label_raw = paste(
      "Qubein: The following action regarding your institution was taken by the Board of Trustees",
      "of the Southern Association of Colleges and Schools Commission on Colleges (SACSCOC)",
      "during its meeting held on June 15, 2023: The SACSCOC Board of Trustees",
      "reviewed the institution's Referral Report from the submission of a Fifth-Year Interim",
      "Report in June 2022 and recommended that the institution be placed on Warning for",
      "twelve months for failure to comply with Core Requirement 12."
    ),
    action_label_short = "Recommended warning for twelve months for failure to comply with Core Requirement 12.1 (Student support services), Standard 8.2.a (Student outcomes: educational programs), and Standard 14.1 (Publication of accreditation status)",
    source_url = "https://ope.ed.gov/dapip/#/institution-profile/133562",
    source_title = "DAPIP Institutional Accreditation Action",
    source_page_url = "https://ope.ed.gov/dapip/#/institution-profile/133562",
    stringsAsFactors = FALSE
  )

  candidates <- build_accreditation_review_candidates(reviewed_actions_df)
  staged <- stage_accreditation_editorial_overrides(candidates, first_seen = "2026-05-27")
  sheet_rows <- build_accreditation_review_sheet_rows(staged)
  sheet_rows$review_status <- "approved"
  merged <- merge_accreditation_review_sheet_editor_columns(staged, sheet_rows, first_seen = "2026-05-27")

  poorer_actions_df <- reviewed_actions_df
  poorer_actions_df$action_label_short[[1]] <- "Recommended warning for twelve months for failure to comply with Core Requirement 12."

  applied <- apply_accreditation_editorial_overrides(
    poorer_actions_df,
    merged,
    enforce_review_gate = TRUE,
    allowed_action_ids = candidates$action_id,
    drop_unlisted = TRUE
  )

  assert_identical(nrow(applied), 1L)
  assert_identical(
    applied$action_label_short[[1]],
    reviewed_actions_df$action_label_short[[1]]
  )
})

run_test("Apply-only accreditation publish keeps approved scraper rows when recompute drops them", function() {
  reviewed_actions_df <- data.frame(
    export_unitid = "100",
    unitid = "100",
    export_institution_name = "Example University",
    accreditor = "MSCHE",
    action_date = "2026-04-24",
    action_type = "warning",
    action_label_raw = "Issued warning",
    action_label_short = "Reviewed warning statement",
    source_url = "https://example.org/reviewed-action",
    source_title = "Reviewed source",
    source_page_url = "https://example.org/reviewed-action",
    stringsAsFactors = FALSE
  )

  candidates <- build_accreditation_review_candidates(reviewed_actions_df)
  staged <- stage_accreditation_editorial_overrides(candidates, first_seen = "2026-05-27")
  sheet_rows <- build_accreditation_review_sheet_rows(staged)
  sheet_rows$review_status <- "approved"
  overrides <- merge_accreditation_review_sheet_editor_columns(staged, sheet_rows, first_seen = "2026-05-27")

  recomputed_actions_df <- reviewed_actions_df
  recomputed_actions_df$action_date[[1]] <- "2026-05-01"
  recomputed_actions_df$action_label_raw[[1]] <- "Different warning"
  recomputed_actions_df$action_label_short[[1]] <- "Different warning"
  recomputed_actions_df$source_url[[1]] <- "https://example.org/different-action"
  recomputed_actions_df$source_page_url[[1]] <- "https://example.org/different-action"

  applied <- apply_accreditation_editorial_overrides(
    recomputed_actions_df,
    overrides,
    enforce_review_gate = TRUE,
    allowed_action_ids = candidates$action_id,
    drop_unlisted = TRUE
  )

  assert_identical(nrow(applied), 1L)
  assert_identical(trim_text(applied$action_id[[1]]), trim_text(candidates$action_id[[1]]))
  assert_identical(applied$action_label_short[[1]], "Reviewed warning statement")
  assert_identical(applied$row_origin[[1]], "scraper")
  assert_identical(applied$source_url[[1]], "https://example.org/reviewed-action")
})

run_test("Manual accreditation rows append cleanly against production-shaped datetime columns", function() {
  actions_path <- file.path(root, "data_pipelines", "accreditation", "accreditation_tracker_actions_joined.csv")
  actions_df <- readr::read_csv(
    actions_path,
    show_col_types = FALSE,
    col_types = readr::cols(
      last_seen_at = readr::col_datetime(),
      .default = readr::col_character()
    )
  )[1, , drop = FALSE]
  export_institution_name <- dplyr::coalesce(
    trim_optional_text(actions_df$institution_name),
    trim_optional_text(actions_df$tracker_name),
    trim_optional_text(actions_df$institution_name_raw)
  )
  export_state <- dplyr::coalesce(
    trim_optional_text(actions_df$state),
    trim_optional_text(actions_df$tracker_state),
    trim_optional_text(actions_df$institution_state_raw)
  )
  export_unitid <- trim_optional_text(actions_df$unitid)
  if (is.na(export_unitid[[1]]) || !nzchar(export_unitid[[1]])) {
    name_slug <- tolower(trimws(export_institution_name[[1]] %||% ""))
    name_slug <- sub("^the +", "", name_slug)
    name_slug <- gsub("\\bst\\.?\\b", "saint", name_slug)
    name_slug <- gsub("&", "and", name_slug, fixed = TRUE)
    name_slug <- gsub("[^a-z0-9]+", "-", name_slug)
    name_slug <- gsub("^-+|-+$", "", name_slug)

    state_slug <- gsub("[^a-z0-9]+", "-", tolower(trimws(export_state[[1]] %||% "")))
    state_slug <- gsub("^-+|-+$", "", state_slug)

    accred_slug <- gsub("[^a-z0-9]+", "-", tolower(trimws(actions_df$accreditor[[1]] %||% "")))
    accred_slug <- gsub("^-+|-+$", "", accred_slug)

    export_unitid[[1]] <- paste0("accred-", paste(Filter(nzchar, c(name_slug, state_slug, accred_slug)), collapse = "--"))
  }
  actions_df$export_institution_name <- export_institution_name
  actions_df$export_unitid <- export_unitid
  actions_df$action_date <- trim_optional_text(actions_df$action_date)
  actions_df$action_year <- trim_optional_text(actions_df$action_year)
  actions_df$action_label_short <- if ("action_label_short" %in% names(actions_df)) {
    dplyr::coalesce(
      trim_optional_text(actions_df$action_label_short),
      trim_optional_text(actions_df$action_label_raw)
    )
  } else {
    trim_optional_text(actions_df$action_label_raw)
  }

  assert_true("last_seen_at" %in% names(actions_df))
  assert_true(inherits(actions_df$last_seen_at, "POSIXct"))

  merged <- merge_accreditation_review_sheet_editor_columns(
    empty_accreditation_editorial_overrides(),
    data.frame(
      action_id = "",
      unitid = "",
      institution_name = "Manual University",
      accreditor = "NECHE",
      action_date = "2026-06-01",
      action_type = "warning",
      action_label_raw = "Issued warning",
      generated_statement = "Manual accreditation action",
      source_url = "https://example.org/manual-accreditation",
      source_title = "Manual source",
      row_origin = "manual",
      review_status = "approved",
      stringsAsFactors = FALSE
    ),
    first_seen = "2026-05-27"
  )

  applied <- apply_accreditation_editorial_overrides(actions_df, merged, enforce_review_gate = FALSE)

  assert_identical(nrow(applied), 2L)
  assert_true(inherits(applied$last_seen_at, "POSIXct"))
  assert_true(is.na(applied$last_seen_at[[2]]))
  assert_identical(applied$row_origin[[2]], "manual")
})

run_test("Unreviewed manual accreditation rows stay excluded from exports", function() {
  actions_df <- data.frame(
    export_unitid = "100",
    unitid = "100",
    export_institution_name = "Example University",
    accreditor = "MSCHE",
    action_date = "2026-04-24",
    action_type = "warning",
    action_label_raw = "Warning",
    action_label_short = "Generated warning",
    source_url = "https://example.org/pipeline-action",
    source_title = "Pipeline source",
    stringsAsFactors = FALSE
  )
  merged <- merge_accreditation_review_sheet_editor_columns(
    empty_accreditation_editorial_overrides(),
    data.frame(
      action_id = "",
      unitid = "",
      institution_name = "Manual University",
      accreditor = "NECHE",
      action_date = "2026-06-01",
      action_type = "warning",
      action_label_raw = "Issued warning",
      generated_statement = "Manual accreditation action",
      source_url = "https://example.org/manual-accreditation",
      source_title = "Manual source",
      row_origin = "manual",
      review_status = "unreviewed",
      stringsAsFactors = FALSE
    ),
    first_seen = "2026-05-27"
  )
  applied <- apply_accreditation_editorial_overrides(actions_df, merged, enforce_review_gate = FALSE)
  assert_identical(nrow(applied), 1L)
})

run_test("Accreditation review gate accepts reject as a terminal decision but excludes it from output", function() {
  actions_df <- data.frame(
    export_unitid = c("100", "101"),
    unitid = c("100", "101"),
    export_institution_name = c("Example University", "Example College"),
    accreditor = c("MSCHE", "HLC"),
    action_date = c("2026-04-24", "2026-05-01"),
    action_type = c("warning", "probation"),
    action_label_raw = c("Warning", "Probation"),
    action_label_short = c("Generated warning", "Generated probation"),
    source_url = c("https://example.org/action-one", "https://example.org/action-two"),
    source_title = c("Source one", "Source two"),
    source_page_url = c("https://example.org/action-one", "https://example.org/action-two"),
    stringsAsFactors = FALSE
  )
  candidates <- build_accreditation_review_candidates(actions_df)
  staged <- stage_accreditation_editorial_overrides(candidates, first_seen = "2026-05-27")
  sheet_rows <- build_accreditation_review_sheet_rows(staged)
  sheet_rows$review_status <- c("approved", "reject")
  overrides <- merge_accreditation_review_sheet_editor_columns(staged, sheet_rows, first_seen = "2026-05-27")

  applied <- apply_accreditation_editorial_overrides(actions_df, overrides, enforce_review_gate = TRUE)

  assert_identical(nrow(applied), 1L)
  assert_identical(trim_text(applied$action_id[[1]]), trim_text(candidates$action_id[[1]]))
  assert_true(!(trim_text(candidates$action_id[[2]]) %in% trim_text(applied$action_id)))
})

run_test("Accreditation review gate still fails blank review decisions", function() {
  actions_df <- data.frame(
    export_unitid = c("100", "101"),
    unitid = c("100", "101"),
    export_institution_name = c("Example University", "Example College"),
    accreditor = c("MSCHE", "HLC"),
    action_date = c("2026-04-24", "2026-05-01"),
    action_type = c("warning", "probation"),
    action_label_raw = c("Warning", "Probation"),
    action_label_short = c("Generated warning", "Generated probation"),
    source_url = c("https://example.org/action-one", "https://example.org/action-two"),
    source_title = c("Source one", "Source two"),
    source_page_url = c("https://example.org/action-one", "https://example.org/action-two"),
    stringsAsFactors = FALSE
  )
  candidates <- build_accreditation_review_candidates(actions_df)
  staged <- stage_accreditation_editorial_overrides(candidates, first_seen = "2026-05-27")
  sheet_rows <- build_accreditation_review_sheet_rows(staged)
  sheet_rows$review_status <- c("approved", "")
  overrides <- merge_accreditation_review_sheet_editor_columns(staged, sheet_rows, first_seen = "2026-05-27")

  err <- tryCatch(
    {
      apply_accreditation_editorial_overrides(actions_df, overrides, enforce_review_gate = TRUE)
      NULL
    },
    error = identity
  )

  assert_true(!is.null(err), "Blank accreditation decisions should still fail the review gate.")
  assert_true(grepl("missing an editorial decision", conditionMessage(err), fixed = TRUE))
})

run_test("Unreviewed manual college cuts rows stay excluded from exports", function() {
  cuts_df <- data.frame(
    cut_id = "cut-1",
    matched_unitid = "100",
    export_unitid = "100",
    institution_name_display = "Example University",
    state_display = "Alabama",
    announcement_date = "2026-04-24",
    announcement_year = 2026L,
    cut_type = "program_closure",
    program_name = "History BA",
    source_url = "https://example.org/pipeline-cut",
    source_title = "Pipeline source",
    source_publication = "Pipeline paper",
    stringsAsFactors = FALSE
  )
  overrides <- data.frame(
    cut_id = "editor-manual-cut",
    source_unitid = "",
    source_institution_name = "Manual College",
    source_state = "Ohio",
    source_announcement_date = "2026-06-02",
    source_announcement_year = "2026",
    source_cut_type = "layoff",
    source_cut_description = "Ten staff layoffs",
    source_source_url = "https://example.org/manual-cut",
    source_source_title = "Manual source",
    source_source_publication = "Manual paper",
    source_row_origin = "manual",
    override_unitid = NA_character_,
    override_institution_name = NA_character_,
    override_state = NA_character_,
    override_announcement_date = NA_character_,
    override_announcement_year = NA_character_,
    override_cut_type = NA_character_,
    override_cut_description = NA_character_,
    override_source_url = NA_character_,
    override_source_title = NA_character_,
    override_source_publication = NA_character_,
    first_seen = "2026-05-27",
    review_status = "unreviewed",
    reviewer = NA_character_,
    reviewer_notes = NA_character_,
    reviewed_at = NA_character_,
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  )
  applied <- apply_college_cuts_editorial_overrides(cuts_df, overrides, enforce_review_gate = FALSE)
  assert_identical(nrow(applied), 1L)
})

run_test("College cuts review gate accepts reject as a terminal decision but excludes it from output", function() {
  cuts_df <- data.frame(
    cut_id = c("cut-1", "cut-2"),
    matched_unitid = c("100", "101"),
    export_unitid = c("100", "101"),
    institution_name_display = c("Example University", "Example College"),
    state_display = c("Alabama", "Georgia"),
    announcement_date = c("2026-04-24", "2026-05-01"),
    announcement_year = c(2026L, 2026L),
    cut_type = c("program_closure", "layoff"),
    program_name = c("History BA", "Ten staff layoffs"),
    source_url = c("https://example.org/cut-one", "https://example.org/cut-two"),
    source_title = c("Source one", "Source two"),
    source_publication = c("Paper one", "Paper two"),
    is_primary_tracker = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  candidates <- build_college_cuts_review_candidates(cuts_df, tracker_unitids = c("100", "101"))
  staged <- stage_college_cuts_editorial_overrides(
    candidates,
    first_seen = "2026-05-27",
    tracker_unitids = c("100", "101")
  )
  sheet_rows <- build_college_cuts_review_sheet_rows(staged, tracker_unitids = c("100", "101"))
  sheet_rows$review_status <- c("approved", "reject")
  overrides <- merge_college_cuts_review_sheet_editor_columns(staged, sheet_rows, first_seen = "2026-05-27")

  applied <- apply_college_cuts_editorial_overrides(cuts_df, overrides, enforce_review_gate = TRUE)

  assert_identical(nrow(applied), 1L)
  assert_identical(trim_text(applied$cut_id[[1]]), trim_text(candidates$cut_id[[1]]))
  assert_true(!(trim_text(candidates$cut_id[[2]]) %in% trim_text(applied$cut_id)))
})

run_test("Cross-source duplicate suppression in stage_accreditation_editorial_overrides", function() {
  make_override <- function(action_id, unitid, accreditor, action_date, action_type,
                            action_label_raw = "Warning", review_status = "approved") {
    data.frame(
      action_id = action_id,
      source_unitid = unitid,
      source_institution_name = "Example University",
      source_accreditor = accreditor,
      source_action_date = action_date,
      source_action_type = action_type,
      source_action_label_raw = action_label_raw,
      source_generated_statement = action_label_raw,
      source_source_url = "https://example.org/existing",
      source_source_title = "Existing source",
      source_row_origin = "scraper",
      override_unitid = NA_character_,
      override_institution_name = NA_character_,
      override_accreditor = NA_character_,
      override_action_date = NA_character_,
      override_action_type = NA_character_,
      override_action_label_raw = NA_character_,
      override_generated_statement = NA_character_,
      override_source_url = NA_character_,
      override_source_title = NA_character_,
      first_seen = "2025-12-01",
      review_status = review_status,
      reviewer = "MV",
      reviewer_notes = NA_character_,
      reviewed_at = "2025-12-01",
      grandfathered = FALSE,
      stringsAsFactors = FALSE
    )
  }

  make_candidate <- function(action_id, unitid, accreditor, action_date, action_type,
                             action_label_raw = "Warning") {
    data.frame(
      action_id = action_id,
      unitid = unitid,
      institution_name = "Example University",
      accreditor = accreditor,
      action_date = action_date,
      action_type = action_type,
      action_label_raw = action_label_raw,
      generated_statement = action_label_raw,
      source_url = "https://example.org/new",
      source_title = "New source",
      row_origin = "scraper",
      stringsAsFactors = FALSE
    )
  }

  # 1. Same unitid/accreditor/type within 7 days: suppressed
  existing <- make_override("existing-1", "100", "SACSCOC", "2025-12-07", "warning")
  candidate <- make_candidate("new-dup-1", "100", "SACSCOC", "2025-12-01", "warning", action_label_raw = "Updated warning summary")
  candidate$generated_statement <- "Updated warning summary"
  candidate$source_url <- "https://example.org/refreshed"
  candidate$source_title <- "Refreshed source"
  staged <- stage_accreditation_editorial_overrides(candidate, existing, first_seen = "2026-06-05")
  assert_true(!("new-dup-1" %in% staged$action_id), "7-day gap same event should be suppressed")
  assert_identical(trim_text(staged$source_action_label_raw[[1]]), "Updated warning summary")
  assert_identical(trim_text(staged$source_generated_statement[[1]]), "Updated warning summary")
  assert_identical(trim_text(staged$source_source_url[[1]]), "https://example.org/refreshed")

  # 2. 26-day gap within 30-day tolerance: suppressed
  existing2 <- make_override("existing-2", "200", "WSCUC", "2025-06-27", "monitoring")
  candidate2 <- make_candidate("new-dup-2", "200", "WSCUC", "2025-06-01", "notice")
  staged2 <- stage_accreditation_editorial_overrides(candidate2, existing2, first_seen = "2026-06-05")
  assert_true(!("new-dup-2" %in% staged2$action_id), "26-day gap notice/monitoring should be suppressed")

  # 3. Different institution at same date: NOT suppressed
  existing3 <- make_override("existing-3", "300", "SACSCOC", "2025-12-01", "warning")
  candidate3 <- make_candidate("new-diff-inst", "999", "SACSCOC", "2025-12-01", "warning")
  staged3 <- stage_accreditation_editorial_overrides(candidate3, existing3, first_seen = "2026-06-05")
  assert_true("new-diff-inst" %in% staged3$action_id, "Different unitid should not be suppressed")

  # 4. Same institution/accreditor, 45-day gap: NOT suppressed
  existing4 <- make_override("existing-4", "400", "HLC", "2025-12-01", "warning")
  candidate4 <- make_candidate("new-too-far", "400", "HLC", "2026-01-15", "warning")
  staged4 <- stage_accreditation_editorial_overrides(candidate4, existing4, first_seen = "2026-06-05")
  assert_true("new-too-far" %in% staged4$action_id, "45-day gap should not be suppressed")

  # 5. Same institution/accreditor/date but incompatible types (warning vs removed): NOT suppressed
  existing5 <- make_override("existing-5", "500", "SACSCOC", "2025-12-01", "removed")
  candidate5 <- make_candidate("new-diff-type", "500", "SACSCOC", "2025-12-01", "warning")
  staged5 <- stage_accreditation_editorial_overrides(candidate5, existing5, first_seen = "2026-06-05")
  assert_true("new-diff-type" %in% staged5$action_id, "Incompatible action types should not be suppressed")
})

run_test("HLC institution-page bare status rows are suppressed before staging", function() {
  hlc_candidates <- data.frame(
    action_id = c("hlc-on-prob", "hlc-real-action", "hlc-wrong-url"),
    unitid = c("100", "101", "102"),
    institution_name = c("Wittenberg University", "Some College", "Other College"),
    accreditor = c("HLC", "HLC", "HLC"),
    action_date = c("2025-11-01", "2025-11-01", "2025-11-01"),
    action_type = c("probation", "probation", "probation"),
    action_label_raw = c(
      "On Probation",
      "Placed on Probation for failure to comply",
      "On Probation"
    ),
    generated_statement = c("On Probation", "Placed on Probation", "On Probation"),
    source_url = c(
      "https://www.hlcommission.org/institution/12345/",
      "https://www.hlcommission.org/institution/12345/",
      "https://ope.ed.gov/dapip/"
    ),
    source_title = c("HLC Status", "HLC Action", "DAPIP"),
    row_origin = c("scraper", "scraper", "scraper"),
    stringsAsFactors = FALSE
  )

  staged <- stage_accreditation_editorial_overrides(hlc_candidates, first_seen = "2026-06-05")

  # "On Probation" from hlcommission.org/institution/ must be suppressed
  assert_true(!("hlc-on-prob" %in% staged$action_id))
  # Real probation action text from same URL should pass through
  assert_true("hlc-real-action" %in% staged$action_id)
  # "On Probation" from a different URL should pass through
  assert_true("hlc-wrong-url" %in% staged$action_id)
})

run_test("Accreditation review sheet append rows only include unseen action_ids", function() {
  actions_df <- data.frame(
    export_unitid = c("100", "101"),
    unitid = c("100", "101"),
    export_institution_name = c("Example University", "Example College"),
    accreditor = c("MSCHE", "HLC"),
    action_date = c("2026-04-24", "2026-05-01"),
    action_type = c("warning", "probation"),
    action_label_raw = c("Warning", "Placed on Probation for failure to comply."),
    action_label_short = c("Generated warning", "Placed on Probation"),
    source_url = c("https://example.org/action-one", "https://example.org/action-two"),
    source_title = c("Source one", "Source two"),
    source_page_url = c("https://example.org/action-one", "https://example.org/action-two"),
    stringsAsFactors = FALSE
  )

  candidates <- build_accreditation_review_candidates(actions_df)
  staged <- stage_accreditation_editorial_overrides(candidates, first_seen = "2026-05-27")
  local_sheet_rows <- build_accreditation_review_sheet_rows(staged)
  existing_sheet <- local_sheet_rows[1, , drop = FALSE]
  existing_sheet$review_status <- "approved"
  existing_sheet$reviewer <- "editor@example.org"
  existing_sheet$reviewed_at <- "2026-05-28"
  existing_sheet$generated_statement <- "Human-reviewed warning statement"

  append_rows <- build_accreditation_review_sheet_append_rows(staged, existing_sheet)

  assert_identical(nrow(append_rows), 1L)
  assert_identical(
    trim_text(append_rows$action_id[[1]]),
    trim_text(local_sheet_rows$action_id[[2]])
  )
})

run_test("Accreditation review candidates drop teach-out process rows but keep sanction rows that mention teach-out plans", function() {
  actions_df <- data.frame(
    export_unitid = c("100", "101", "102"),
    unitid = c("100", "101", "102"),
    export_institution_name = c("Process University", "Monitoring College", "Closure Institute"),
    accreditor = c("MSCHE", "MSCHE", "HLC"),
    action_date = c("2026-04-24", "2026-05-01", "2026-05-02"),
    action_type = c("other", "monitoring", "adverse_action"),
    action_label_raw = c(
      "Approved the institution's teach-out agreement with Sample University.",
      "Required teach-out plan and financial viability monitoring after Heightened Cash Monitoring (HCM2).",
      "Withdrawal of accreditation and required teach-out plan."
    ),
    action_label_short = c(
      "Approved the institution's teach-out agreement with Sample University.",
      "Required teach-out plan and financial viability monitoring after Heightened Cash Monitoring (HCM2).",
      "Withdrawal of accreditation and required teach-out plan."
    ),
    source_url = c(
      "https://example.org/process-row",
      "https://example.org/monitoring-row",
      "https://example.org/adverse-row"
    ),
    source_title = c("Process row", "Monitoring row", "Adverse row"),
    source_page_url = c(
      "https://example.org/process-row",
      "https://example.org/monitoring-row",
      "https://example.org/adverse-row"
    ),
    stringsAsFactors = FALSE
  )

  candidates <- build_accreditation_review_candidates(actions_df)

  assert_identical(nrow(candidates), 2L)
  assert_true(
    !any(grepl("teach-out agreement with sample university", tolower(candidates$action_label_raw), fixed = TRUE)),
    "Administrative teach-out agreement approvals should not be staged for editorial review."
  )
  assert_true(
    any(candidates$action_type == "monitoring"),
    "Monitoring rows that mention teach-out requirements should still be staged."
  )
  assert_true(
    any(candidates$action_type == "adverse_action"),
    "Adverse-action rows that mention teach-out requirements should still be staged."
  )
})

run_test("Accreditation review sheet selection excludes legacy teach-out process rows but keeps substantive sanctions", function() {
  overrides <- data.frame(
    action_id = c("keep-1", "drop-1"),
    unitid = c("100", "101"),
    institution_name = c("Example University", "Process University"),
    accreditor = c("HLC", "MSCHE"),
    action_date = c("2026-05-01", "2026-05-02"),
    action_type = c("probation", "other"),
    action_label_raw = c(
      "Placed on Probation and required a teach-out plan.",
      "Approved the institution's teach-out agreement with Sample University."
    ),
    generated_statement = c(
      "Placed on Probation and required a teach-out plan.",
      "Approved the institution's teach-out agreement with Sample University."
    ),
    source_url = c("https://example.org/probation-row", "https://example.org/process-row"),
    source_title = c("Probation row", "Process row"),
    row_origin = c("scraper", "scraper"),
    first_seen = c("2026-06-06", "2026-06-06"),
    review_status = c("approved", "approved"),
    stringsAsFactors = FALSE
  )

  filtered <- filter_accreditation_overrides_for_review_sheet(
    overrides,
    candidate_action_ids = c("keep-1", "drop-1")
  )

  assert_identical(nrow(filtered), 1L)
  assert_identical(trim_text(filtered$action_id[[1]]), "keep-1")
})

run_test("Accreditation teach-out classifier handles real production phrasings", function() {
  rows <- data.frame(
    action_type = c(
      "other",
      "adverse_action",
      "other",
      "adverse_action",
      "adverse_action",
      "adverse_action",
      "show_cause"
    ),
    action_label_raw = c(
      "Approved the institution’s teach-out plan to move the Harding School of Theology from the branch campus in Memphis, Tennessee, to the main campus in Searcy, Arkansas.",
      "Accepted Teach-Out Plans",
      "Approved the institution’s Provisional Plan for teach-out of an additional location: AU Challenger Learning Center for Science and Technology, 222 E. Church Street, Woodstock, IL 60098.",
      "Approved the teach-out of the George Williams College Location, 1350 Constance Blvd., Williams Bay, WI 53191.",
      "Approved the institution’s provisional plan to teach out students enrolled in the Doctor of Pharmacy degree program, including a teach-out arrangement with West Virginia University (Morgantown, WV).",
      "To require that the institution complete and submit for approval, by March 3, 2025, a teach-out plan and teach-out agreements as required by the Commission's Teach-Out Plans and Agreements Policy and Procedures and federal regulation 34 CFR 602.24(c)(2)(i) because the Secretary of Education has placed the institution on Heightened Cash Monitoring (HCM2).",
      "To request that the institution complete and submit for approval, by September 1, 2022, a comprehensive, implementable teach-out plan (Teach-Out Plans and Agreements Policy and Procedures)."
    ),
    generated_statement = c(
      "Approved the institution’s teach-out plan to move the Harding School of Theology from the branch campus in Memphis, Tennessee, to the main campus in Searcy, Arkansas.",
      "Accepted Teach-Out Plans",
      "Approved the institution’s Provisional Plan for teach-out of an additional location: AU Challenger Learning Center for Science and Technology, 222 E. Church Street, Woodstock, IL 60098.",
      "Approved the teach-out of the George Williams College Location, 1350 Constance Blvd., Williams Bay, WI 53191.",
      "Approved the institution’s provisional plan to teach out students enrolled in the Doctor of Pharmacy degree program, including a teach-out arrangement with West Virginia University (Morgantown, WV).",
      "Required teach-out plan and financial viability monitoring after Heightened Cash Monitoring (HCM2)",
      "Show cause with required comprehensive teach-out plan"
    ),
    stringsAsFactors = FALSE
  )

  actual <- compute_accreditation_teachout_process_mask(
    rows,
    action_type_col = "action_type",
    action_label_raw_col = "action_label_raw",
    action_label_short_col = "generated_statement"
  )

  expected <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  assert_identical(actual, expected)
})
