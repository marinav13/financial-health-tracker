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
  edited_sheet_rows$edited_cut_text[[1]] <- "Corrected History BA"
  edited_sheet_rows$raw_cut_text[[1]] <- "Part of 20 programs eliminated due to enrollment declines and budget deficits."
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
  assert_identical(applied$cut_label_override_effective[[1]], "Corrected History BA")
  assert_identical(applied$source_url[[1]], "https://example.org/corrected-cut")
  assert_identical(applied$source_publication[[1]], "Corrected paper")
})

run_test("Generic college cuts source descriptions prefill edited text from generated label without override churn", function() {
  generated_label <- "At least 16 faculty laid off, mostly in humanities, effective end of academic year (pay and benefits through August)."
  generated_summary <- paste(
    generated_label,
    "Board of trustees approved cuts in early February 2026.",
    "Driven by ongoing fiscal distress."
  )
  cuts_df <- data.frame(
    cut_id = "cut-generic",
    matched_unitid = "100",
    export_unitid = "100",
    institution_name_display = "Example University",
    state_display = "Ohio",
    announcement_date = "2026-02-20",
    announcement_year = 2026L,
    cut_type = "staff_layoff",
    program_name = "Staff layoff",
    generated_cut_label = generated_label,
    generated_cut_summary = generated_summary,
    source_url = "https://example.org/csu",
    source_title = "Pipeline source",
    source_publication = "Example Paper",
    is_primary_tracker = TRUE,
    stringsAsFactors = FALSE
  )

  candidates <- build_college_cuts_review_candidates(cuts_df, tracker_unitids = "100")
  staged <- stage_college_cuts_editorial_overrides(
    candidates,
    first_seen = "2026-05-27",
    tracker_unitids = "100"
  )
  sheet_rows <- build_college_cuts_review_sheet_rows(staged, tracker_unitids = "100")

  assert_identical(sheet_rows$edited_cut_text[[1]], generated_label)
  assert_identical(sheet_rows$raw_cut_text[[1]], generated_summary)

  sheet_rows$review_status[[1]] <- "approved"
  sheet_rows$reviewer[[1]] <- "editor@example.org"
  merged <- merge_college_cuts_review_sheet_editor_columns(
    staged,
    sheet_rows,
    first_seen = "2026-05-27"
  )

  assert_true(is.na(merged$override_cut_description[[1]]) || !nzchar(trimws(merged$override_cut_description[[1]] %||% "")))
  assert_true(is.na(merged$override_cut_label[[1]]) || !nzchar(trimws(merged$override_cut_label[[1]] %||% "")))

  applied <- apply_college_cuts_editorial_overrides(cuts_df, merged, enforce_review_gate = FALSE)
  assert_identical(applied$program_name[[1]], generated_label)
  assert_identical(applied$cut_label_override_effective[[1]], generated_label)
})

run_test("Stale sheet-only scraper college cuts rows drop before merge while human rows stay", function() {
  cuts_df <- data.frame(
    cut_id = "cut-current",
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
  current_sheet_row <- build_college_cuts_review_sheet_rows(staged, tracker_unitids = "100")
  current_sheet_row$review_status[[1]] <- "approved"

  stale_scraper_row <- current_sheet_row
  stale_scraper_row$cut_id[[1]] <- "cut-stale"
  stale_scraper_row$institution_name[[1]] <- "Stale University"

  stale_undecided_row <- stale_scraper_row
  stale_undecided_row$cut_id[[1]] <- "cut-stale-undecided"
  stale_undecided_row$review_status[[1]] <- "unreviewed"
  stale_undecided_row$reviewer[[1]] <- ""
  stale_undecided_row$reviewer_notes[[1]] <- ""
  stale_undecided_row$reviewed_at[[1]] <- ""

  human_sheet_row <- current_sheet_row
  human_sheet_row$cut_id[[1]] <- ""
  human_sheet_row$unitid[[1]] <- "200"
  human_sheet_row$institution_name[[1]] <- "Manual University"
  human_sheet_row$state[[1]] <- "Georgia"
  human_sheet_row$announcement_date[[1]] <- "2026-05-01"
  human_sheet_row$announcement_year[[1]] <- "2026"
  human_sheet_row$cut_type[[1]] <- "staff_layoff"
  human_sheet_row$display_categories[[1]] <- "Staff layoffs / furloughs"
  human_sheet_row$edited_cut_text[[1]] <- "Manual layoff row"
  human_sheet_row$raw_cut_text[[1]] <- "Manual layoff summary."
  human_sheet_row$source_url[[1]] <- "https://example.org/manual-cut"
  human_sheet_row$source_publication[[1]] <- "Manual paper"
  human_sheet_row$row_origin[[1]] <- "manual"
  human_sheet_row$first_seen[[1]] <- "2026-05-27"
  human_sheet_row$review_status[[1]] <- "approved"

  filtered <- drop_stale_college_cuts_sheet_rows(
    sheet_rows = dplyr::bind_rows(current_sheet_row, stale_scraper_row, stale_undecided_row, human_sheet_row),
    local_cut_ids = staged$cut_id,
    candidate_cut_ids = candidates$cut_id
  )

  assert_identical(nrow(filtered$dropped_rows), 1L)
  assert_identical(filtered$dropped_rows$cut_id[[1]], "cut-stale-undecided")
  assert_identical(nrow(filtered$quarantined_rows), 1L)
  assert_identical(filtered$quarantined_rows$cut_id[[1]], "cut-stale")
  assert_identical(nrow(filtered$kept_rows), 2L)

  merged <- merge_college_cuts_review_sheet_editor_columns(
    staged,
    filtered$kept_rows,
    first_seen = "2026-05-27"
  )

  assert_identical(nrow(merged), 2L)
  assert_true(any(trim_text(merged$source_row_origin) == "manual"))
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

run_test("Apply-only college cuts review gate withholds unreviewed rows instead of failing", function() {
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
  sheet_rows$review_status <- c("approved", "unreviewed")
  overrides <- merge_college_cuts_review_sheet_editor_columns(staged, sheet_rows, first_seen = "2026-05-27")

  applied <- apply_college_cuts_editorial_overrides(
    cuts_df,
    overrides,
    enforce_review_gate = TRUE,
    allowed_cut_ids = candidates$cut_id,
    drop_unlisted = TRUE,
    gate_mask = cuts_df$is_primary_tracker %in% TRUE
  )

  assert_identical(nrow(applied), 1L)
  assert_identical(trim_text(applied$cut_id[[1]]), trim_text(candidates$cut_id[[1]]))
  assert_true(!(trim_text(candidates$cut_id[[2]]) %in% trim_text(applied$cut_id)))
})

run_test("Apply-only college cuts review gate still fails true missing override rows", function() {
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
    source_url = "https://example.org/cut-one",
    source_title = "Source one",
    source_publication = "Paper one",
    is_primary_tracker = TRUE,
    stringsAsFactors = FALSE
  )

  overrides <- data.frame(
    cut_id = "other-cut",
    source_unitid = "101",
    source_institution_name = "Other College",
    source_state = "Georgia",
    source_announcement_date = "2026-05-01",
    source_announcement_year = "2026",
    source_cut_type = "layoff",
    source_cut_description = "Ten staff layoffs",
    source_source_url = "https://example.org/cut-two",
    source_source_title = "Source two",
    source_source_publication = "Paper two",
    source_row_origin = "scraper",
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
    review_status = "approved",
    reviewer = "editor@example.org",
    reviewer_notes = NA_character_,
    reviewed_at = "2026-05-28",
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  )

  err <- tryCatch(
    {
      apply_college_cuts_editorial_overrides(
        cuts_df,
        overrides,
        enforce_review_gate = TRUE,
        allowed_cut_ids = "cut-1",
        drop_unlisted = TRUE,
        gate_mask = TRUE
      )
      NULL
    },
    error = identity
  )

  assert_true(!is.null(err), "Missing cut override rows should still fail the apply-only review gate.")
  assert_true(grepl("missing editorial overrides", conditionMessage(err), fixed = TRUE))
})

run_test("Cross-source duplicate suppression in stage_accreditation_editorial_overrides", function() {
  make_override <- function(action_id, unitid, accreditor, action_date, action_type,
                            action_label_raw = "Warning due to financial reporting failures",
                            generated_statement = action_label_raw,
                            review_status = "approved",
                            reviewer = "MV",
                            reviewer_notes = NA_character_,
                            reviewed_at = "2025-12-01",
                            override_action_label_raw = NA_character_) {
    data.frame(
      action_id = action_id,
      source_unitid = unitid,
      source_institution_name = "Example University",
      source_accreditor = accreditor,
      source_action_date = action_date,
      source_action_type = action_type,
      source_action_label_raw = action_label_raw,
      source_generated_statement = generated_statement,
      source_source_url = "https://example.org/existing",
      source_source_title = "Existing source",
      source_row_origin = "scraper",
      override_unitid = NA_character_,
      override_institution_name = NA_character_,
      override_accreditor = NA_character_,
      override_action_date = NA_character_,
      override_action_type = NA_character_,
      override_action_label_raw = override_action_label_raw,
      override_generated_statement = NA_character_,
      override_source_url = NA_character_,
      override_source_title = NA_character_,
      first_seen = "2025-12-01",
      review_status = review_status,
      reviewer = reviewer,
      reviewer_notes = reviewer_notes,
      reviewed_at = reviewed_at,
      grandfathered = FALSE,
      stringsAsFactors = FALSE
    )
  }

  make_candidate <- function(action_id, unitid, accreditor, action_date, action_type,
                             action_label_raw = "Warning due to financial reporting failures",
                             generated_statement = action_label_raw) {
    data.frame(
      action_id = action_id,
      unitid = unitid,
      institution_name = "Example University",
      accreditor = accreditor,
      action_date = action_date,
      action_type = action_type,
      action_label_raw = action_label_raw,
      generated_statement = generated_statement,
      source_url = "https://example.org/new",
      source_title = "New source",
      row_origin = "scraper",
      stringsAsFactors = FALSE
    )
  }

  # 1. Same unitid/accreditor/type within 7 days with similar raw text:
  # suppressed and source fields refresh in place.
  existing <- make_override("existing-1", "100", "SACSCOC", "2025-12-07", "warning", review_status = "reject")
  candidate <- make_candidate(
    "new-dup-1",
    "100",
    "SACSCOC",
    "2025-12-01",
    "warning",
    action_label_raw = "Warning due to financial reporting failures and governance concerns",
    generated_statement = "Warning due to financial reporting failures and governance concerns"
  )
  candidate$source_url <- "https://example.org/refreshed"
  candidate$source_title <- "Refreshed source"
  staged <- stage_accreditation_editorial_overrides(candidate, existing, first_seen = "2026-06-05")
  assert_true(!("new-dup-1" %in% staged$action_id), "7-day gap same event should be suppressed")
  assert_identical(trim_text(staged$source_action_label_raw[[1]]), "Warning due to financial reporting failures and governance concerns")
  assert_identical(trim_text(staged$source_generated_statement[[1]]), "Warning due to financial reporting failures and governance concerns")
  assert_identical(trim_text(staged$source_source_url[[1]]), "https://example.org/refreshed")
  assert_identical(trim_text(staged$review_status[[1]]), "reject")

  # 2. A new notice candidate may still absorb an older monitoring row.
  existing2 <- make_override(
    "existing-2",
    "200",
    "WSCUC",
    "2025-06-27",
    "monitoring",
    action_label_raw = "Monitoring notice regarding financial responsibility concerns",
    generated_statement = "Monitoring notice regarding financial responsibility concerns"
  )
  candidate2 <- make_candidate(
    "new-dup-2",
    "200",
    "WSCUC",
    "2025-06-01",
    "notice",
    action_label_raw = "Notice regarding financial responsibility monitoring concerns",
    generated_statement = "Notice regarding financial responsibility monitoring concerns"
  )
  staged2 <- stage_accreditation_editorial_overrides(candidate2, existing2, first_seen = "2026-06-05")
  assert_true(!("new-dup-2" %in% staged2$action_id), "26-day gap notice/monitoring should be suppressed")

  # 3. The reverse direction must fail closed: monitoring follow-up rows do
  # not swallow an existing notice sanction.
  existing3 <- make_override(
    "existing-3",
    "300",
    "SACSCOC",
    "2025-12-27",
    "notice",
    action_label_raw = "Notice regarding financial responsibility monitoring concerns"
  )
  candidate3 <- make_candidate(
    "new-monitoring-followup",
    "300",
    "SACSCOC",
    "2025-12-01",
    "monitoring",
    action_label_raw = "Monitoring follow-up regarding financial responsibility concerns"
  )
  staged3 <- stage_accreditation_editorial_overrides(candidate3, existing3, first_seen = "2026-06-05")
  assert_true("new-monitoring-followup" %in% staged3$action_id, "Monitoring follow-up rows must not swallow a formal notice sanction")

  # 4. Same-type distinct actions within 30 days: NOT suppressed.
  existing4 <- make_override(
    "existing-4",
    "400",
    "MSCHE",
    "2025-12-01",
    "other",
    action_label_raw = "Grant Substantive Change: Ownership"
  )
  candidate4 <- make_candidate(
    "new-distinct-other",
    "400",
    "MSCHE",
    "2025-12-18",
    "other",
    action_label_raw = "To acknowledge receipt of notification that the merger of Keystone College into Keystone College, LLC occurred on May 30, 2025."
  )
  staged4 <- stage_accreditation_editorial_overrides(candidate4, existing4, first_seen = "2026-06-05")
  assert_true("new-distinct-other" %in% staged4$action_id, "Low-similarity same-type actions must stage as new rows")

  # 5. Same institution/accreditor, 45-day gap: NOT suppressed.
  existing4 <- make_override("existing-4", "400", "HLC", "2025-12-01", "warning")
  candidate4 <- make_candidate("new-too-far", "400", "HLC", "2026-01-15", "warning")
  staged5 <- stage_accreditation_editorial_overrides(candidate4, existing4, first_seen = "2026-06-05")
  assert_true("new-too-far" %in% staged5$action_id, "45-day gap should not be suppressed")

  # 6. Same institution/accreditor/date but incompatible types (warning vs
  # removed): NOT suppressed.
  existing5 <- make_override("existing-5", "500", "SACSCOC", "2025-12-01", "removed")
  candidate5 <- make_candidate("new-diff-type", "500", "SACSCOC", "2025-12-01", "warning")
  staged6 <- stage_accreditation_editorial_overrides(candidate5, existing5, first_seen = "2026-06-05")
  assert_true("new-diff-type" %in% staged6$action_id, "Incompatible action types should not be suppressed")

  # 7. Same institution/accreditor within 30 days but teach-out process vs
  # actual resignation: NOT suppressed.
  existing6 <- make_override(
    "existing-6",
    "151810",
    "HLC",
    "2025-12-01",
    "adverse_action",
    action_label_raw = "Approved the institution’s provisional plan and teach-out agreement with the following institution: University of Indianapolis, Indianapolis, IN"
  )
  existing6$source_generated_statement <- "Approved provisional plan and teach-out agreement with University of Indianapolis"
  candidate6 <- make_candidate(
    "new-martin-resignation",
    "151810",
    "HLC",
    "2025-12-31",
    "adverse_action",
    action_label_raw = "Martin University Voluntary Resignation of Accreditation Effective: December 31, 2025 Martin University in Indianapolis, Indiana, voluntarily resigned its accreditation with the Higher Learning Commission effective December 31, 2025."
  )
  candidate6$generated_statement <- "Voluntarily Surrendered Accreditation"
  staged7 <- stage_accreditation_editorial_overrides(candidate6, existing6, first_seen = "2026-06-08")
  assert_true(
    "new-martin-resignation" %in% staged7$action_id,
    "Teach-out process approvals must not suppress a later same-month resignation action."
  )
})

run_test("Cross-source duplicate suppression flips approved rows back to unreviewed on raw-text drift", function() {
  existing <- data.frame(
    action_id = "existing-approved",
    source_unitid = "100",
    source_institution_name = "Example University",
    source_accreditor = "SACSCOC",
    source_action_date = "2025-12-07",
    source_action_type = "warning",
    source_action_label_raw = "Warning due to financial reporting failures",
    source_generated_statement = "Warning due to financial reporting failures",
    source_source_url = "https://example.org/existing",
    source_source_title = "Existing source",
    source_row_origin = "scraper",
    override_unitid = NA_character_,
    override_institution_name = NA_character_,
    override_accreditor = NA_character_,
    override_action_date = NA_character_,
    override_action_type = NA_character_,
    override_action_label_raw = "Editor-facing public text",
    override_generated_statement = "Editor-facing public text",
    override_source_url = NA_character_,
    override_source_title = NA_character_,
    first_seen = "2025-12-01",
    review_status = "approved",
    reviewer = "MV",
    reviewer_notes = "Approved under old wording",
    reviewed_at = "2025-12-01",
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  )
  candidate <- data.frame(
    action_id = "new-approved-dup",
    unitid = "100",
    institution_name = "Example University",
    accreditor = "SACSCOC",
    action_date = "2025-12-01",
    action_type = "warning",
    action_label_raw = "Warning due to financial reporting failures and governance concerns",
    generated_statement = "Warning due to financial reporting failures and governance concerns",
    source_url = "https://example.org/refreshed",
    source_title = "Refreshed source",
    row_origin = "scraper",
    stringsAsFactors = FALSE
  )

  staged <- stage_accreditation_editorial_overrides(candidate, existing, first_seen = "2026-06-05")
  assert_true(!("new-approved-dup" %in% staged$action_id), "Approved duplicate rows should still fold into the existing override row")
  assert_identical(trim_text(staged$source_action_label_raw[[1]]), "Warning due to financial reporting failures and governance concerns")
  assert_identical(trim_text(staged$review_status[[1]]), "unreviewed")
  assert_identical(trim_text(staged$reviewer[[1]]), "")
  assert_identical(trim_text(staged$reviewer_notes[[1]]), "")
  assert_identical(trim_text(staged$reviewed_at[[1]]), "")
  assert_identical(trim_text(staged$override_action_label_raw[[1]]), "Editor-facing public text")
})

run_test("Cross-source duplicate suppression compares raw source text, not edited override text", function() {
  existing <- data.frame(
    action_id = "existing-raw-contract",
    source_unitid = "100",
    source_institution_name = "Example University",
    source_accreditor = "SACSCOC",
    source_action_date = "2025-12-07",
    source_action_type = "warning",
    source_action_label_raw = "Warning due to financial reporting failures",
    source_generated_statement = "Warning due to financial reporting failures",
    source_source_url = "https://example.org/existing",
    source_source_title = "Existing source",
    source_row_origin = "scraper",
    override_unitid = NA_character_,
    override_institution_name = NA_character_,
    override_accreditor = NA_character_,
    override_action_date = NA_character_,
    override_action_type = NA_character_,
    override_action_label_raw = "Completely different editor text",
    override_generated_statement = "Completely different editor text",
    override_source_url = NA_character_,
    override_source_title = NA_character_,
    first_seen = "2025-12-01",
    review_status = "approved",
    reviewer = "MV",
    reviewer_notes = "Reviewed",
    reviewed_at = "2025-12-01",
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  )
  candidate <- data.frame(
    action_id = "new-raw-contract",
    unitid = "100",
    institution_name = "Example University",
    accreditor = "SACSCOC",
    action_date = "2025-12-01",
    action_type = "warning",
    action_label_raw = "Warning due to financial reporting failures",
    generated_statement = "Warning due to financial reporting failures",
    source_url = "https://example.org/refreshed",
    source_title = "Refreshed source",
    row_origin = "scraper",
    stringsAsFactors = FALSE
  )

  staged <- stage_accreditation_editorial_overrides(candidate, existing, first_seen = "2026-06-05")
  assert_true(!("new-raw-contract" %in% staged$action_id), "The raw-vs-raw matcher should still suppress identical source text")
  assert_identical(trim_text(staged$review_status[[1]]), "approved")
  assert_identical(trim_text(staged$reviewer[[1]]), "MV")
  assert_identical(trim_text(staged$override_action_label_raw[[1]]), "Completely different editor text")
})

run_test("Apply-only accreditation review gate canonicalizes duplicate snapshot ids to existing override rows", function() {
  make_override <- function(action_id, unitid, accreditor, action_date, action_type,
                            action_label_raw = "Warning due to financial reporting failures", generated_statement = action_label_raw,
                            review_status = "approved") {
    data.frame(
      action_id = action_id,
      source_unitid = unitid,
      source_institution_name = "Example University",
      source_accreditor = accreditor,
      source_action_date = action_date,
      source_action_type = action_type,
      source_action_label_raw = action_label_raw,
      source_generated_statement = generated_statement,
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

  committed_candidates <- data.frame(
    action_id = "new-dup-1",
    unitid = "100",
    institution_name = "Example University",
    accreditor = "SACSCOC",
    action_date = "2025-12-01",
    action_type = "warning",
    action_label_raw = "Warning due to financial reporting failures and governance concerns",
    generated_statement = "Warning due to financial reporting failures and governance concerns",
    source_url = "https://example.org/refreshed",
    source_title = "Refreshed source",
    row_origin = "scraper",
    stringsAsFactors = FALSE
  )
  overrides <- make_override(
    "existing-1",
    "100",
    "SACSCOC",
    "2025-12-07",
    "warning",
    action_label_raw = "Warning due to financial reporting failures",
    generated_statement = "Warning due to financial reporting failures"
  )
  allowed_ids <- canonicalize_accreditation_review_gate_action_ids(committed_candidates, overrides)
  assert_identical(allowed_ids, "existing-1")

  actions_df <- data.frame(
    export_unitid = "100",
    unitid = "100",
    export_institution_name = "Example University",
    accreditor = "SACSCOC",
    action_date = "2025-12-01",
    action_type = "warning",
    action_label_raw = "Warning due to financial reporting failures and governance concerns",
    action_label_short = "Warning due to financial reporting failures and governance concerns",
    source_url = "https://example.org/refreshed",
    source_title = "Refreshed source",
    source_page_url = "https://example.org/refreshed",
    stringsAsFactors = FALSE
  )

  applied <- apply_accreditation_editorial_overrides(
    actions_df,
    overrides,
    enforce_review_gate = TRUE,
    allowed_action_ids = allowed_ids,
    drop_unlisted = TRUE,
    gate_mask = TRUE
  )

  assert_identical(nrow(applied), 1L)
  assert_identical(trim_text(applied$action_id[[1]]), "existing-1")
  assert_identical(trim_text(applied$action_label_short[[1]]), "Warning due to financial reporting failures")
})

run_test("Apply-only accreditation review gate does not canonicalize low-similarity or monitoring-follow-up rows", function() {
  overrides <- rbind(
    data.frame(
      action_id = "existing-other",
      source_unitid = "400",
      source_institution_name = "Example University",
      source_accreditor = "MSCHE",
      source_action_date = "2025-12-01",
      source_action_type = "other",
      source_action_label_raw = "Grant Substantive Change: Ownership",
      source_generated_statement = "Grant Substantive Change: Ownership",
      source_source_url = "https://example.org/ownership",
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
      review_status = "approved",
      reviewer = "MV",
      reviewer_notes = NA_character_,
      reviewed_at = "2025-12-01",
      grandfathered = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      action_id = "existing-notice",
      source_unitid = "300",
      source_institution_name = "Example University",
      source_accreditor = "SACSCOC",
      source_action_date = "2025-12-27",
      source_action_type = "notice",
      source_action_label_raw = "Notice regarding financial responsibility monitoring concerns",
      source_generated_statement = "Notice regarding financial responsibility monitoring concerns",
      source_source_url = "https://example.org/notice",
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
      review_status = "approved",
      reviewer = "MV",
      reviewer_notes = NA_character_,
      reviewed_at = "2025-12-01",
      grandfathered = FALSE,
      stringsAsFactors = FALSE
    )
  )

  committed_candidates <- rbind(
    data.frame(
      action_id = "new-distinct-other",
      unitid = "400",
      institution_name = "Example University",
      accreditor = "MSCHE",
      action_date = "2025-12-18",
      action_type = "other",
      action_label_raw = "To acknowledge receipt of notification that the merger of Keystone College into Keystone College, LLC occurred on May 30, 2025.",
      generated_statement = "To acknowledge receipt of notification that the merger of Keystone College into Keystone College, LLC occurred on May 30, 2025.",
      source_url = "https://example.org/merger",
      source_title = "Committed candidate",
      row_origin = "scraper",
      stringsAsFactors = FALSE
    ),
    data.frame(
      action_id = "new-monitoring-followup",
      unitid = "300",
      institution_name = "Example University",
      accreditor = "SACSCOC",
      action_date = "2025-12-01",
      action_type = "monitoring",
      action_label_raw = "Monitoring follow-up regarding financial responsibility concerns",
      generated_statement = "Monitoring follow-up regarding financial responsibility concerns",
      source_url = "https://example.org/monitoring",
      source_title = "Committed candidate",
      row_origin = "scraper",
      stringsAsFactors = FALSE
    )
  )

  allowed_ids <- canonicalize_accreditation_review_gate_action_ids(committed_candidates, overrides)
  assert_identical(
    sort(allowed_ids),
    sort(c("new-distinct-other", "new-monitoring-followup"))
  )
})

run_test("HLC institution-page status rows are exempt from staging and the apply-only review gate", function() {
  make_hlc_candidate <- function(action_id, action_label_raw) {
    data.frame(
      action_id = action_id,
      unitid = "204617",
      institution_name = "Ohio Dominican University",
      accreditor = "HLC",
      action_date = "2026-07-02",
      action_type = "notice",
      action_label_raw = action_label_raw,
      generated_statement = action_label_raw,
      source_url = "https://www.hlcommission.org/institution/ohio-dominican-university/",
      source_title = "HLC institution page",
      row_origin = "scraper",
      stringsAsFactors = FALSE
    )
  }
  badge_id <- compute_accreditation_action_id(
    "204617", "HLC", "2026-07-02", "On Notice",
    "204617", "Ohio Dominican University"
  )
  real_label <- "Placed on Notice by the Board of Trustees for financial reasons."
  real_id <- compute_accreditation_action_id(
    "204617", "HLC", "2026-07-02", real_label,
    "204617", "Ohio Dominican University"
  )
  badge_candidate <- make_hlc_candidate(badge_id, "On Notice")
  real_candidate <- make_hlc_candidate(real_id, real_label)
  candidates <- rbind(badge_candidate, real_candidate)

  assert_true(
    is_hlc_institution_status_page_row(
      badge_candidate$accreditor, badge_candidate$source_url, badge_candidate$action_label_raw
    ),
    "Bare status badge on an HLC institution page must match the mask"
  )
  assert_true(
    !is_hlc_institution_status_page_row(
      real_candidate$accreditor, real_candidate$source_url, real_candidate$action_label_raw
    ),
    "A real action label must not match the mask"
  )
  assert_true(
    !is_hlc_institution_status_page_row("MSCHE", badge_candidate$source_url, "On Notice"),
    "Non-HLC rows must not match the mask"
  )

  staged <- stage_accreditation_editorial_overrides(candidates, first_seen = "2026-07-07")
  assert_true(!(badge_id %in% staged$action_id), "Staging must suppress the badge row")
  assert_true(real_id %in% staged$action_id, "Staging must keep the real action")

  allowed_ids <- canonicalize_accreditation_review_gate_action_ids(candidates, staged)
  assert_true(!(badge_id %in% allowed_ids), "Gate must exempt the badge candidate")
  assert_true(real_id %in% allowed_ids, "Gate must keep the staged candidate")

  # Regression for the 2026-07-07 publish failure: an enforced gated export
  # whose committed candidates include a badge row must not stop() on the
  # badge row's missing override; it drops the badge action and publishes
  # the approved real action.
  staged$review_status[trim_text(staged$action_id) == real_id] <- "approved"
  staged$reviewer[trim_text(staged$action_id) == real_id] <- "MV"
  actions_df <- data.frame(
    export_unitid = c("204617", "204617"),
    unitid = c("204617", "204617"),
    export_institution_name = c("Ohio Dominican University", "Ohio Dominican University"),
    accreditor = c("HLC", "HLC"),
    action_date = c("2026-07-02", "2026-07-02"),
    action_type = c("notice", "notice"),
    action_label_raw = c("On Notice", real_label),
    action_label_short = c("On Notice", "Placed on Notice"),
    source_url = rep(badge_candidate$source_url, 2L),
    source_title = c("HLC institution page", "HLC institution page"),
    source_page_url = rep(badge_candidate$source_url, 2L),
    stringsAsFactors = FALSE
  )
  applied <- apply_accreditation_editorial_overrides(
    actions_df,
    staged,
    enforce_review_gate = TRUE,
    allowed_action_ids = allowed_ids,
    drop_unlisted = TRUE,
    gate_mask = c(TRUE, TRUE)
  )
  assert_identical(nrow(applied), 1L)
  assert_identical(trim_text(applied$action_id[[1]]), real_id)
})

run_test("Apply-only accreditation review gate does not canonicalize teach-out process rows to later resignation actions", function() {
  overrides <- data.frame(
    action_id = "existing-6",
    source_unitid = "151810",
    source_institution_name = "Martin University",
    source_accreditor = "HLC",
    source_action_date = "2025-12-01",
    source_action_type = "adverse_action",
    source_action_label_raw = "Approved the institution’s provisional plan and teach-out agreement with the following institution: University of Indianapolis, Indianapolis, IN",
    source_generated_statement = "Approved provisional plan and teach-out agreement with University of Indianapolis",
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
    review_status = "approved",
    reviewer = "MV",
    reviewer_notes = NA_character_,
    reviewed_at = "2025-12-01",
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  )

  committed_candidates <- data.frame(
    action_id = "new-martin-resignation",
    unitid = "151810",
    institution_name = "Martin University",
    accreditor = "HLC",
    action_date = "2025-12-31",
    action_type = "adverse_action",
    action_label_raw = "Martin University Voluntary Resignation of Accreditation Effective: December 31, 2025 Martin University in Indianapolis, Indiana, voluntarily resigned its accreditation with the Higher Learning Commission effective December 31, 2025.",
    generated_statement = "Voluntarily Surrendered Accreditation",
    source_url = "https://example.org/martin",
    source_title = "Martin source",
    row_origin = "scraper",
    stringsAsFactors = FALSE
  )

  allowed_ids <- canonicalize_accreditation_review_gate_action_ids(committed_candidates, overrides)
  assert_identical(allowed_ids, "new-martin-resignation")
})

run_test("Apply-only accreditation review gate still fails true missing override rows", function() {
  committed_candidates <- data.frame(
    action_id = "missing-1",
    unitid = "100",
    institution_name = "Example University",
    accreditor = "SACSCOC",
    action_date = "2025-12-01",
    action_type = "warning",
    action_label_raw = "Updated warning summary",
    generated_statement = "Updated warning summary",
    source_url = "https://example.org/refreshed",
    source_title = "Refreshed source",
    row_origin = "scraper",
    stringsAsFactors = FALSE
  )
  overrides <- data.frame(
    action_id = "existing-other",
    source_unitid = "200",
    source_institution_name = "Other University",
    source_accreditor = "HLC",
    source_action_date = "2025-11-01",
    source_action_type = "probation",
    source_action_label_raw = "Probation",
    source_generated_statement = "Probation",
    source_source_url = "https://example.org/existing-other",
    source_source_title = "Existing other source",
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
    review_status = "approved",
    reviewer = "MV",
    reviewer_notes = NA_character_,
    reviewed_at = "2025-12-01",
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  )
  allowed_ids <- canonicalize_accreditation_review_gate_action_ids(committed_candidates, overrides)
  assert_identical(allowed_ids, "missing-1")

  actions_df <- data.frame(
    export_unitid = "100",
    unitid = "100",
    export_institution_name = "Example University",
    accreditor = "SACSCOC",
    action_date = "2025-12-01",
    action_type = "warning",
    action_label_raw = "Updated warning summary",
    action_label_short = "Updated warning summary",
    source_url = "https://example.org/refreshed",
    source_title = "Refreshed source",
    source_page_url = "https://example.org/refreshed",
    stringsAsFactors = FALSE
  )

  err <- tryCatch(
    {
      apply_accreditation_editorial_overrides(
        actions_df,
        overrides,
        enforce_review_gate = TRUE,
        allowed_action_ids = allowed_ids,
        drop_unlisted = TRUE,
        gate_mask = TRUE
      )
      NULL
    },
    error = identity
  )

  assert_true(!is.null(err), "Missing override rows should still fail the apply-only review gate.")
  assert_true(grepl("missing editorial overrides", conditionMessage(err), fixed = TRUE))
})

run_test("Approved review-backed accreditation overrides export as visible rows", function() {
  actions_df <- data.frame(
    unitid = "151810",
    export_unitid = "151810",
    export_institution_name = "Martin University",
    institution_name = "Martin University",
    accreditor = "HLC",
    action_date = "2025-12-01",
    action_year = "2025",
    action_type = "adverse_action",
    action_label_raw = "Approved the institutionâ€™s provisional plan and teach-out agreement with the following institution: University of Indianapolis, Indianapolis, IN",
    action_label_short = "Approved provisional plan and teach-out agreement with University of Indianapolis",
    source_url = "https://example.org/existing",
    source_title = "Existing source",
    source_page_url = "https://example.org/existing",
    source_page_modified = NA_character_,
    display_action = TRUE,
    public_table_strategy = "scraper_backed_keep",
    hybrid_candidate = FALSE,
    hybrid_reason = NA_character_,
    has_financial_profile = TRUE,
    is_primary_tracker = TRUE,
    stringsAsFactors = FALSE
  )

  overrides <- data.frame(
    action_id = "91a619a88984",
    source_unitid = "151810",
    source_institution_name = "Martin University",
    source_accreditor = "HLC",
    source_action_date = "2025-12-31",
    source_action_type = "adverse_action",
    source_action_label_raw = "Martin University Voluntary Resignation of Accreditation Effective: December 31, 2025 Martin University in Indianapolis, Indiana, voluntarily resigned its accreditation with the Higher Learning Commission effective December 31, 2025.",
    source_generated_statement = "Voluntarily Surrendered Accreditation effective December 31, 2025. The institution has established a teach-out agreement that has been approved by HLC with University of Indianapolis in Indianapolis, Indiana.",
    source_source_url = "https://ope.ed.gov/dapip/#/institution-profile/115320",
    source_source_title = "DAPIP Institutional Accreditation Action",
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
    first_seen = "2026-06-06",
    review_status = "approved",
    reviewer = "MV",
    reviewer_notes = NA_character_,
    reviewed_at = NA_character_,
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  )

  applied <- apply_accreditation_editorial_overrides(
    actions_df,
    overrides,
    enforce_review_gate = FALSE
  )

  martin_row <- applied[trim_text(applied$action_id) == "91a619a88984", , drop = FALSE]
  assert_identical(nrow(martin_row), 1L)
  assert_true(isTRUE(martin_row$display_action[[1]]),
    "Approved review-backed accreditation rows should export with display_action=true.")
  assert_true(nzchar(trim_text(martin_row$action_label_raw[[1]])),
    "Review-backed accreditation rows should retain the raw action label.")
  assert_true(nzchar(trim_text(martin_row$action_label_short[[1]])),
    "Review-backed accreditation rows should retain the short action label.")
  assert_identical(trim_text(martin_row$source_url[[1]]), "https://ope.ed.gov/dapip/#/institution-profile/115320")
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
run_test("College cuts edited/raw text columns round-trip through stage->sheet->merge", function() {
  cuts_df <- data.frame(
    cut_id = "cut-1",
    matched_unitid = "100",
    export_unitid = "100",
    institution_name_display = "Example University",
    state_display = "Alabama",
    announcement_date = "2026-04-24",
    announcement_year = 2026L,
    cut_type = "department_closure",
    program_name = "Athletics department",
    generated_cut_label = "University closes athletics department",
    generated_cut_summary = "University closes athletics department and laid off 12 staff.",
    source_url = "https://example.org/cut",
    source_title = NA_character_,
    source_publication = "Example Paper",
    is_primary_tracker = TRUE,
    stringsAsFactors = FALSE
  )

  candidates <- build_college_cuts_review_candidates(cuts_df, tracker_unitids = "100")
  assert_identical("generated_cut_label" %in% names(candidates), TRUE)
  assert_identical("generated_cut_summary" %in% names(candidates), TRUE)
  assert_identical(candidates$generated_cut_label[[1]], "University closes athletics department")

  staged <- stage_college_cuts_editorial_overrides(candidates, first_seen = "2026-05-01", tracker_unitids = "100")
  assert_identical("source_generated_cut_label" %in% names(staged), TRUE)
  assert_identical(staged$source_generated_cut_label[[1]], "University closes athletics department")

  sheet_rows <- build_college_cuts_review_sheet_rows(staged, tracker_unitids = "100")
  assert_identical("display_categories" %in% names(sheet_rows), TRUE)
  assert_identical("edited_cut_text" %in% names(sheet_rows), TRUE)
  assert_identical("raw_cut_text" %in% names(sheet_rows), TRUE)
  assert_identical(sheet_rows$edited_cut_text[[1]], "Athletics department")
  assert_identical(sheet_rows$raw_cut_text[[1]], "University closes athletics department and laid off 12 staff.")
  assert_identical(sheet_rows$display_categories[[1]], "Athletics cuts; Staff layoffs / furloughs")

  sheet_rows$edited_cut_text[[1]] <- "Editor-revised short label"
  sheet_rows$review_status[[1]] <- "approved"
  sheet_rows$reviewer[[1]] <- "editor@example.org"

  merged <- merge_college_cuts_review_sheet_editor_columns(staged, sheet_rows, first_seen = "2026-05-01")
  assert_identical(merged$override_cut_description[[1]], "Editor-revised short label")
  assert_identical(merged$override_cut_label[[1]], "Editor-revised short label")
  assert_true(is.na(merged$override_cut_summary[[1]]) || !nzchar(trimws(merged$override_cut_summary[[1]] %||% "")))
})

run_test("Discovered college cuts candidates merge into review candidates and route unresolved rows to unmatched review", function() {
  candidate_rows <- data.frame(
    cut_id = c("existing-cut", "collision-cut"),
    unitid = c("100", "100"),
    institution_name = c("Example University", "Example University"),
    state = c("Alabama", "Alabama"),
    announcement_date = c("2026-04-24", "2026-04-25"),
    announcement_year = c("2026", "2026"),
    cut_type = c("staff_layoff", "staff_layoff"),
    program_name = c("Existing cut", "Collision cut"),
    generated_cut_label = c("Existing cut", "Collision cut"),
    generated_cut_summary = c("Existing cut summary", "Collision cut summary"),
    source_url = c("https://example.org/existing", "https://example.org/collision"),
    source_title = c("Existing story", "Collision story"),
    source_publication = c("Example Paper", "Example Paper"),
    row_origin = c("scraper", "scraper"),
    stringsAsFactors = FALSE
  )

  discovered_rows <- data.frame(
    cut_id = c("discovered-new", "collision-cut", "discovered-unmatched"),
    unitid = c("200", "100", ""),
    institution_name = c("Discovery University", "Example University", "Unknown College"),
    state = c("Ohio", "Alabama", "Texas"),
    announcement_date = c("2026-05-01", "2026-05-02", "2026-05-03"),
    announcement_year = c("2026", "2026", "2026"),
    cut_type = c("hiring_freeze", "staff_layoff", "program_suspension"),
    program_name = c("Discovery hiring freeze", "Duplicate collision", "Unresolved program cut"),
    generated_cut_label = c("Discovery hiring freeze", "Duplicate collision", "Unresolved program cut"),
    generated_cut_summary = c("Freeze announced for administrative hiring.", "Collision summary", "Programs suspended pending budget review."),
    source_url = c("https://example.org/discovery", "https://example.org/collision-2", "https://example.org/unmatched"),
    source_title = c("Discovery story", "Collision story 2", "Unmatched story"),
    source_publication = c("Discovery Wire", "Example Paper", "State Paper"),
    row_origin = c("news_scan", "news_scan", "warn_notice"),
    stringsAsFactors = FALSE
  )

  discovered_path <- tempfile("discovered_cuts_", fileext = ".csv")
  unmatched_path <- tempfile("discovered_unmatched_", fileext = ".csv")
  readr::write_csv(discovered_rows, discovered_path, na = "")

  merged <- merge_discovered_college_cuts_review_candidates(
    candidate_rows,
    discovered_path = discovered_path,
    unmatched_path = unmatched_path
  )

  assert_identical(nrow(merged), 3L)
  assert_true(
    setequal(merged$cut_id, c("existing-cut", "collision-cut", "discovered-new")),
    paste0("Expected existing ids plus one discovered id. Got: ", paste(merged$cut_id, collapse = ", "))
  )
  discovered_idx <- match("discovered-new", merged$cut_id)
  assert_identical(merged$row_origin[[discovered_idx]], "news_scan")
  assert_identical(merged$cut_type[[discovered_idx]], "hiring_freeze")

  unmatched_rows <- readr::read_csv(unmatched_path, show_col_types = FALSE)
  assert_identical(nrow(unmatched_rows), 1L)
  assert_identical(unmatched_rows$cut_id[[1]], "discovered-unmatched")
  assert_identical(unmatched_rows$institution_name_collegecuts[[1]], "Unknown College")
  assert_identical(unmatched_rows$match_method[[1]], "discovered_unmatched")
})

run_test("Discovered college cuts header mismatch warns and leaves generated candidates unchanged", function() {
  candidate_rows <- data.frame(
    cut_id = "existing-cut",
    unitid = "100",
    institution_name = "Example University",
    state = "Alabama",
    announcement_date = "2026-04-24",
    announcement_year = "2026",
    cut_type = "staff_layoff",
    program_name = "Existing cut",
    generated_cut_label = "Existing cut",
    generated_cut_summary = "Existing cut summary",
    source_url = "https://example.org/existing",
    source_title = "Existing story",
    source_publication = "Example Paper",
    row_origin = "scraper",
    stringsAsFactors = FALSE
  )
  bad_path <- tempfile("discovered_bad_", fileext = ".csv")
  readr::write_csv(
    data.frame(
      cut_id = "discovered-new",
      unitid = "200",
      institution_name = "Bad Header University",
      stringsAsFactors = FALSE
    ),
    bad_path,
    na = ""
  )

  warning_message <- NULL
  merged <- withCallingHandlers(
    merge_discovered_college_cuts_review_candidates(candidate_rows, bad_path),
    warning = function(w) {
      warning_message <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  assert_identical(nrow(merged), 1L)
  assert_identical(merged$cut_id[[1]], "existing-cut")
  assert_true(
    !is.null(warning_message) && grepl("header mismatch", warning_message, ignore.case = TRUE),
    paste0("Expected discovered header mismatch warning. Got: ", warning_message %||% "<none>")
  )
})

run_test("Missing discovered college cuts file leaves generated candidates unchanged", function() {
  candidate_rows <- data.frame(
    cut_id = "existing-cut",
    unitid = "100",
    institution_name = "Example University",
    state = "Alabama",
    announcement_date = "2026-04-24",
    announcement_year = "2026",
    cut_type = "staff_layoff",
    program_name = "Existing cut",
    generated_cut_label = "Existing cut",
    generated_cut_summary = "Existing cut summary",
    source_url = "https://example.org/existing",
    source_title = "Existing story",
    source_publication = "Example Paper",
    row_origin = "scraper",
    stringsAsFactors = FALSE
  )

  merged <- merge_discovered_college_cuts_review_candidates(
    candidate_rows,
    discovered_path = tempfile("missing_discovered_", fileext = ".csv")
  )

  assert_identical(nrow(merged), 1L)
  assert_identical(merged$cut_id[[1]], "existing-cut")
})

run_test("Legacy college cuts sheet without edited/raw names coerces cleanly", function() {
  legacy_sheet <- data.frame(
    cut_id = "cut-2",
    unitid = "200",
    institution_name = "Legacy University",
    state = "Ohio",
    announcement_date = "2025-09-01",
    announcement_year = "2025",
    cut_type = "staff_layoff",
    cut_description = "Staff layoff",
    source_url = "https://example.org/legacy",
    source_publication = "Legacy Paper",
    row_origin = "scraper",
    first_seen = "2025-09-05",
    review_status = "approved",
    reviewer = "editor@example.org",
    reviewer_notes = "ok",
    reviewed_at = "2025-09-06",
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  )

  coerced <- coerce_college_cuts_review_sheet_rows(legacy_sheet, default_first_seen = "2025-09-05")
  assert_identical(nrow(coerced), 1L)
  assert_identical("display_categories" %in% names(coerced), TRUE)
  assert_identical("edited_cut_text" %in% names(coerced), TRUE)
  assert_identical("raw_cut_text" %in% names(coerced), TRUE)
  assert_identical(coerced$display_categories[[1]], "Staff layoffs / furloughs")
  assert_identical(coerced$edited_cut_text[[1]], "Staff layoff")
  assert_true(is.na(coerced$raw_cut_text[[1]]) || !nzchar(trimws(coerced$raw_cut_text[[1]] %||% "")))
})

run_test("College cuts sheet rows with the legacy one-column shift repair before row_origin validation", function() {
  shifted_sheet <- data.frame(
    cut_id = "cut-shift",
    unitid = "179159",
    institution_name = "Saint Louis University",
    state = "Missouri",
    announcement_date = "2026-07-01",
    announcement_year = "2026",
    cut_type = "staff_layoff",
    display_categories = "80 vacant positions eliminated and filled faculty and staff cuts announced - FY2027 raises suspended",
    cut_description = "80 vacant positions eliminated and filled faculty and staff cuts announced - FY2027 raises suspended",
    cut_label = "80 vacant positions eliminated (35 open faculty + 45 open staff) plus unspecified cuts to filled faculty and staff positions.",
    cut_summary = "https://example.org/slu-cuts",
    source_url = "KSDK / St. Louis Business Journal",
    source_publication = "scraper",
    row_origin = "2026-07-06",
    first_seen = "unreviewed",
    review_status = NA_character_,
    reviewer = NA_character_,
    reviewer_notes = NA_character_,
    reviewed_at = "FALSE",
    grandfathered = NA,
    stringsAsFactors = FALSE
  )

  coerced <- coerce_college_cuts_review_sheet_rows(shifted_sheet, default_first_seen = "2026-07-06")

  assert_identical(nrow(coerced), 1L)
  assert_identical(coerced$edited_cut_text[[1]], shifted_sheet$cut_description[[1]])
  assert_identical(coerced$raw_cut_text[[1]], shifted_sheet$cut_label[[1]])
  assert_identical(coerced$source_url[[1]], shifted_sheet$cut_summary[[1]])
  assert_identical(coerced$source_publication[[1]], shifted_sheet$source_url[[1]])
  assert_identical(coerced$row_origin[[1]], "scraper")
  assert_identical(coerced$first_seen[[1]], "2026-07-06")
  assert_identical(coerced$review_status[[1]], "unreviewed")
  assert_identical(coerced$grandfathered[[1]], FALSE)
  assert_identical(coerced$display_categories[[1]], "Staff layoffs / furloughs")
})

run_test("reviewer_notes stays internal and is not exposed in public schema fields", function() {
  # reviewer_notes must NOT be routed to cut_label_public, cut_summary_public, or edited_cut_text
  staged <- empty_college_cuts_editorial_overrides()
  assert_true("reviewer_notes" %in% names(staged))
  assert_true(!("reviewer_notes" %in% c(
    unname(COLLEGE_CUTS_SHEET_SOURCE_MAP),
    unname(COLLEGE_CUTS_SHEET_OVERRIDE_MAP)
  )), "reviewer_notes must not be in any source or override map")
})

run_test("editor_cut_label alias maps to edited_cut_text in sheet coerce", function() {
  sheet_with_alias <- data.frame(
    cut_id = "cut-3",
    unitid = "300",
    institution_name = "Alias University",
    state = "Texas",
    announcement_date = "2026-01-01",
    announcement_year = "2026",
    cut_type = "program_closure",
    cut_description = "Programs suspended",
    editor_cut_label = "Alias-derived public label",
    source_url = "https://example.org/alias",
    source_publication = "Alias Paper",
    row_origin = "scraper",
    first_seen = "2026-01-02",
    review_status = "unreviewed",
    reviewer = NA_character_,
    reviewer_notes = NA_character_,
    reviewed_at = NA_character_,
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  )

  coerced <- coerce_college_cuts_review_sheet_rows(sheet_with_alias, default_first_seen = "2026-01-02")
  assert_identical(nrow(coerced), 1L)
  assert_identical(coerced$edited_cut_text[[1]], "Alias-derived public label")
})

run_test("Committed college cuts overrides CSV with new label/summary columns reads correctly", function() {
  overrides <- data.frame(
    cut_id = "cut-4",
    source_unitid = "400",
    source_institution_name = "New Schema U",
    source_state = "Michigan",
    source_announcement_date = "2026-03-01",
    source_announcement_year = "2026",
    source_cut_type = "staff_layoff",
    source_cut_description = "Staff layoff",
    source_generated_cut_label = "University lays off 25 staff in deficit response",
    source_generated_cut_summary = "University lays off 25 staff in deficit response. The move affects instructional and administrative staff.",
    source_source_url = "https://example.org/new-schema",
    source_source_title = NA_character_,
    source_source_publication = "Michigan News",
    source_row_origin = "scraper",
    override_unitid = NA_character_,
    override_institution_name = NA_character_,
    override_state = NA_character_,
    override_announcement_date = NA_character_,
    override_announcement_year = NA_character_,
    override_cut_type = NA_character_,
    override_cut_description = NA_character_,
    override_cut_label = "Revised short label",
    override_cut_summary = NA_character_,
    override_source_url = NA_character_,
    override_source_title = NA_character_,
    override_source_publication = NA_character_,
    first_seen = "2026-03-05",
    review_status = "approved",
    reviewer = "editor@example.org",
    reviewer_notes = "checked",
    reviewed_at = "2026-03-06",
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  )

  coerced <- coerce_college_cuts_editorial_overrides(overrides)
  assert_identical(nrow(coerced), 1L)
  assert_identical(coerced$source_generated_cut_label[[1]], "University lays off 25 staff in deficit response")
  assert_identical(coerced$override_cut_label[[1]], "Revised short label")
  assert_true(is.na(coerced$override_cut_summary[[1]]))
})

run_test("Grandfathered generic college cuts labels backfill from generated label only when untouched", function() {
  generated_label <- "At least 16 faculty laid off, mostly in humanities, effective end of academic year (pay and benefits through August)."
  overrides <- data.frame(
    cut_id = c("cut-repair", "cut-keep"),
    source_unitid = c("100", "101"),
    source_institution_name = c("Repair University", "Keep University"),
    source_state = c("Ohio", "Alabama"),
    source_announcement_date = c("2026-02-20", "2026-02-13"),
    source_announcement_year = c("2026", "2026"),
    source_cut_type = c("staff_layoff", "program_suspension"),
    source_cut_description = c("Staff layoff", "Programs suspended"),
    source_generated_cut_label = c(
      generated_label,
      "Board unanimously voted to eliminate 9 minors and 7 concentrations (16 total), saving ~$400K."
    ),
    source_generated_cut_summary = c(
      paste(generated_label, "Board approved cuts in early February 2026."),
      "Board unanimously voted to eliminate 9 minors and 7 concentrations (16 total), saving ~$400K."
    ),
    source_source_url = c("https://example.org/repair", "https://example.org/keep"),
    source_source_title = c(NA_character_, NA_character_),
    source_source_publication = c("Example Paper", "Example Paper"),
    source_row_origin = c("scraper", "scraper"),
    override_unitid = c(NA_character_, NA_character_),
    override_institution_name = c(NA_character_, NA_character_),
    override_state = c(NA_character_, NA_character_),
    override_announcement_date = c(NA_character_, NA_character_),
    override_announcement_year = c(NA_character_, NA_character_),
    override_cut_type = c(NA_character_, NA_character_),
    override_cut_description = c(NA_character_, "Already edited text"),
    override_cut_label = c("Staff layoff", "Already edited text"),
    override_cut_summary = c(NA_character_, NA_character_),
    override_source_url = c(NA_character_, NA_character_),
    override_source_title = c(NA_character_, NA_character_),
    override_source_publication = c(NA_character_, NA_character_),
    first_seen = c("2026-05-15", "2026-05-15"),
    review_status = c("approved", "approved"),
    reviewer = c("grandfathered", "grandfathered"),
    reviewer_notes = c(NA_character_, NA_character_),
    reviewed_at = c("2026-05-15", "2026-05-15"),
    grandfathered = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  repaired <- backfill_college_cuts_grandfathered_generic_labels(overrides)

  assert_identical(repaired$override_cut_description[[1]], generated_label)
  assert_identical(repaired$override_cut_label[[1]], generated_label)
  assert_identical(repaired$override_cut_description[[2]], "Already edited text")
  assert_identical(repaired$override_cut_label[[2]], "Already edited text")
})

run_test("allow_editor_added_rows imports sheet-only non-manual accreditation rows", function() {
  sheet_rows <- data.frame(
    action_id = "orphan1234ab",
    unitid = "100",
    institution_name = "Orphan University",
    accreditor = "MSCHE",
    action_date = "2026-06-15",
    action_type = "warning",
    action_label_raw = "Issued warning",
    generated_statement = "Scraped action stranded in the sheet",
    source_url = "https://example.org/orphan-action",
    source_title = "Orphan source",
    row_origin = "scraper",
    review_status = "approved",
    stringsAsFactors = FALSE
  )

  err <- tryCatch(
    {
      merge_accreditation_review_sheet_editor_columns(
        empty_accreditation_editorial_overrides(),
        sheet_rows,
        first_seen = "2026-07-06"
      )
      NULL
    },
    error = identity
  )
  assert_true(!is.null(err), "Strict merge should reject sheet-only non-manual accreditation rows.")
  assert_true(grepl("not present in editorial_overrides.csv", conditionMessage(err), fixed = TRUE))

  merged <- merge_accreditation_review_sheet_editor_columns(
    empty_accreditation_editorial_overrides(),
    sheet_rows,
    allow_editor_added_rows = TRUE,
    first_seen = "2026-07-06"
  )
  assert_identical(nrow(merged), 1L)
  assert_identical(trim_text(merged$action_id[[1]]), "orphan1234ab")
  assert_identical(merged$source_row_origin[[1]], "scraper")
  assert_identical(merged$review_status[[1]], "approved")
})

run_test("allow_editor_added_rows imports sheet-only non-human college cuts rows", function() {
  sheet_rows <- data.frame(
    cut_id = "orphancut1234",
    unitid = "100",
    institution_name = "Orphan College",
    state = "MA",
    announcement_date = "2026-06-15",
    announcement_year = "2026",
    cut_type = "layoffs",
    edited_cut_text = "College lays off 10 staff",
    raw_cut_text = "College lays off ten staff members",
    source_url = "https://example.org/orphan-cut",
    source_publication = "Example News",
    row_origin = "scraper",
    review_status = "approved",
    stringsAsFactors = FALSE
  )

  err <- tryCatch(
    {
      merge_college_cuts_review_sheet_editor_columns(
        empty_college_cuts_editorial_overrides(),
        sheet_rows,
        first_seen = "2026-07-06"
      )
      NULL
    },
    error = identity
  )
  assert_true(!is.null(err), "Strict merge should reject sheet-only non-human cuts rows.")
  assert_true(grepl("not present in editorial_overrides.csv", conditionMessage(err), fixed = TRUE))

  merged <- merge_college_cuts_review_sheet_editor_columns(
    empty_college_cuts_editorial_overrides(),
    sheet_rows,
    allow_editor_added_rows = TRUE,
    first_seen = "2026-07-06"
  )
  assert_identical(nrow(merged), 1L)
  assert_identical(trim_text(merged$cut_id[[1]]), "orphancut1234")
  assert_identical(merged$source_row_origin[[1]], "scraper")
  assert_identical(merged$review_status[[1]], "approved")
})

run_test("review_sheet_row_has_decision is fail-closed on status vocabulary and reviewer metadata", function() {
  statuses <- c("approved", "reject", "rejected", "hold", "unreviewed", "", NA_character_)
  mask <- review_sheet_row_has_decision(statuses)
  assert_identical(mask, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))

  with_reviewer <- review_sheet_row_has_decision(
    c("unreviewed", "unreviewed", ""),
    reviewer = c("MV", "", ""),
    reviewer_notes = c("", "checked", ""),
    reviewed_at = c("", "", NA_character_)
  )
  assert_identical(with_reviewer, c(TRUE, TRUE, FALSE))
})

run_test("Rewrite guard finds decision rows a tab rewrite would discard or revert", function() {
  make_sheet_row <- function(action_id, review_status, reviewer = "") {
    data.frame(
      action_id = action_id,
      unitid = "100",
      institution_name = "Example University",
      accreditor = "MSCHE",
      action_date = "2026-04-24",
      action_type = "warning",
      action_label_raw = "Warning",
      generated_statement = "Warning",
      source_url = "https://example.org/one",
      source_title = "Source",
      row_origin = "scraper",
      first_seen = "2026-05-01",
      review_status = review_status,
      reviewer = reviewer,
      reviewer_notes = "",
      reviewed_at = "",
      grandfathered = FALSE,
      stringsAsFactors = FALSE
    )
  }
  current <- dplyr::bind_rows(
    make_sheet_row("act-kept", "approved"),
    make_sheet_row("act-missing", "approved"),
    make_sheet_row("act-reverted", "reject"),
    make_sheet_row("act-undecided-missing", "unreviewed")
  )
  payload <- dplyr::bind_rows(
    make_sheet_row("act-kept", "approved"),
    make_sheet_row("act-reverted", "unreviewed")
  )
  lost <- find_review_rows_lost_by_rewrite(current, payload, id_column = "action_id")
  assert_identical(sort(trim_text(lost$action_id)), c("act-missing", "act-reverted"))

  none_lost <- find_review_rows_lost_by_rewrite(current[1, , drop = FALSE], payload, id_column = "action_id")
  assert_identical(nrow(none_lost), 0L)
})

run_test("Quarantine CSV appends dropped decision rows and dedupes within a day", function() {
  quarantine_path <- file.path(tempdir(), sprintf("review-quarantine-%d.csv", as.integer(Sys.time())))
  on.exit(unlink(quarantine_path), add = TRUE)
  rows <- data.frame(
    cut_id = c("cut-q1", "cut-q2"),
    institution_name = c("A University", "B College"),
    review_status = c("approved", "reject"),
    stringsAsFactors = FALSE
  )
  append_review_quarantine_rows(rows, quarantine_path, id_column = "cut_id")
  append_review_quarantine_rows(rows[1, , drop = FALSE], quarantine_path, id_column = "cut_id")
  saved <- readr::read_csv(
    quarantine_path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
  assert_identical(nrow(saved), 2L)
  assert_true(all(c("cut_id", "quarantined_at") %in% names(saved)))
})

run_test("Review sheet header order assert refuses mismatched tabs before positional appends", function() {
  make_empty <- function(columns) {
    as.data.frame(
      setNames(rep(list(character(0)), length(columns)), columns),
      stringsAsFactors = FALSE
    )
  }
  good <- make_empty(COLLEGE_CUTS_REVIEW_SHEET_COLUMNS)
  assert_review_sheet_header_order(
    good, COLLEGE_CUTS_REVIEW_SHEET_COLUMNS, "college_cuts_review",
    normalizer = normalize_college_cuts_sheet_headers
  )

  swapped <- good[, c(2L, 1L, seq(3L, ncol(good))), drop = FALSE]
  err <- tryCatch({
    assert_review_sheet_header_order(
      swapped, COLLEGE_CUTS_REVIEW_SHEET_COLUMNS, "college_cuts_review",
      normalizer = normalize_college_cuts_sheet_headers
    )
    NULL
  }, error = function(e) e)
  assert_true(!is.null(err), "Swapped header order must be refused")

  short <- good[, seq(1L, ncol(good) - 1L), drop = FALSE]
  err_short <- tryCatch({
    assert_review_sheet_header_order(
      short, COLLEGE_CUTS_REVIEW_SHEET_COLUMNS, "college_cuts_review",
      normalizer = normalize_college_cuts_sheet_headers
    )
    NULL
  }, error = function(e) e)
  assert_true(!is.null(err_short), "Missing trailing column must be refused")

  formatted <- make_empty(ACCREDITATION_REVIEW_SHEET_COLUMNS)
  names(formatted)[names(formatted) == "generated_statement"] <- "action_edited"
  names(formatted)[names(formatted) == "action_label_raw"] <- "action_raw"
  assert_review_sheet_header_order(
    formatted, ACCREDITATION_REVIEW_SHEET_COLUMNS, "accreditation_review",
    normalizer = normalize_accreditation_review_sheet_headers
  )
})

run_test("Apply-only gate splits ignored actions into new-this-week vs anomalies", function() {
  overrides <- data.frame(
    action_id = "existing-1",
    source_unitid = "100",
    source_institution_name = "Example University",
    source_accreditor = "SACSCOC",
    source_action_date = "2025-12-01",
    source_action_type = "warning",
    source_action_label_raw = "Warning",
    source_generated_statement = "Warning",
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
    review_status = "approved",
    reviewer = "MV",
    reviewer_notes = NA_character_,
    reviewed_at = "2025-12-01",
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  )
  make_action <- function(unitid, label) {
    data.frame(
      export_unitid = unitid,
      unitid = unitid,
      export_institution_name = "Example University",
      accreditor = "SACSCOC",
      action_date = "2025-12-01",
      action_type = "warning",
      action_label_raw = label,
      action_label_short = label,
      source_url = "https://example.org/existing",
      source_title = "Existing source",
      source_page_url = "https://example.org/existing",
      stringsAsFactors = FALSE
    )
  }
  # Row 1 joins the allowed override; rows 2-3 are unexpected. Different
  # unitids keep them clear of the cross-source matcher.
  actions_df <- dplyr::bind_rows(
    make_action("100", "Warning"),
    make_action("200", "New warning this week"),
    make_action("300", "Stranded action")
  )
  ids <- vapply(seq_len(nrow(actions_df)), function(i) {
    compute_accreditation_action_id(
      actions_df$unitid[[i]], actions_df$accreditor[[i]], actions_df$action_date[[i]],
      actions_df$action_label_raw[[i]], actions_df$export_unitid[[i]],
      actions_df$export_institution_name[[i]]
    )
  }, character(1))
  overrides$action_id <- ids[[1]]

  msgs <- character()
  warns <- character()
  applied <- withCallingHandlers(
    apply_accreditation_editorial_overrides(
      actions_df,
      overrides,
      enforce_review_gate = FALSE,
      allowed_action_ids = ids[[1]],
      drop_unlisted = TRUE,
      gate_mask = rep(TRUE, 3L),
      current_candidate_ids = ids[[2]]
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  assert_identical(nrow(applied), 1L)
  assert_true(any(grepl("withholding 1 new recomputed action", msgs)),
              "By-design bucket must be reported as withheld-pending-review")
  assert_true(any(grepl(sprintf("ANOMALY: 1 recomputed.*%s", ids[[3]]), warns)),
              "Anomaly bucket must warn with the stranded id")
  assert_true(!any(grepl(ids[[2]], warns)),
              "New-this-week id must not appear in the anomaly warning")

  # Without current_candidate_ids the legacy single-bucket message remains.
  msgs2 <- character()
  applied2 <- withCallingHandlers(
    apply_accreditation_editorial_overrides(
      actions_df,
      overrides,
      enforce_review_gate = FALSE,
      allowed_action_ids = ids[[1]],
      drop_unlisted = TRUE,
      gate_mask = rep(TRUE, 3L)
    ),
    message = function(m) {
      msgs2 <<- c(msgs2, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) invokeRestart("muffleWarning")
  )
  assert_identical(nrow(applied2), 1L)
  assert_true(any(grepl("ignoring 2 recomputed action", msgs2)),
              "Legacy message must remain when no current candidate ids are supplied")
})

make_tombstone_test_override <- function(action_id, unitid,
                                         review_status = "approved",
                                         inactive = FALSE,
                                         inactive_reason = NA_character_) {
  data.frame(
    action_id = action_id,
    source_unitid = unitid,
    source_institution_name = "Example University",
    source_accreditor = "SACSCOC",
    source_action_date = "2025-12-01",
    source_action_type = "warning",
    source_action_label_raw = "Warning",
    source_generated_statement = "Warning",
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
    inactive = inactive,
    inactive_reason = inactive_reason,
    stringsAsFactors = FALSE
  )
}

run_test("Coerce backfills tombstone columns on legacy override data", function() {
  legacy <- make_tombstone_test_override("act-legacy", "100")
  legacy$inactive <- NULL
  legacy$inactive_reason <- NULL
  coerced <- coerce_accreditation_editorial_overrides(legacy)
  assert_true(all(c("inactive", "inactive_reason") %in% names(coerced)))
  assert_identical(coerced$inactive, FALSE)
  assert_true(is.na(coerced$inactive_reason))

  # A stray reason on an active row is cleared.
  noisy <- make_tombstone_test_override("act-noisy", "100",
                                        inactive = FALSE,
                                        inactive_reason = "out_of_tracker_scope")
  coerced2 <- coerce_accreditation_editorial_overrides(noisy)
  assert_true(is.na(coerced2$inactive_reason))
})

run_test("Tracker scope filter tombstones and revives instead of deleting", function() {
  overrides <- dplyr::bind_rows(
    make_tombstone_test_override("act-in", "100"),
    make_tombstone_test_override("act-out", "999"),
    make_tombstone_test_override("act-revive", "100",
                                 inactive = TRUE,
                                 inactive_reason = "out_of_tracker_scope"),
    make_tombstone_test_override("act-teachout", "100",
                                 inactive = TRUE,
                                 inactive_reason = "teachout_cleanup")
  )
  msgs <- character()
  filtered <- withCallingHandlers(
    filter_accreditation_overrides_for_tracker_scope(overrides, tracker_unitids = "100"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  assert_identical(nrow(filtered), 4L)

  by_id <- function(id) filtered[trim_text(filtered$action_id) == id, , drop = FALSE]
  assert_true(!(by_id("act-in")$inactive %in% TRUE), "In-scope active row stays active")
  assert_true(by_id("act-out")$inactive %in% TRUE, "Out-of-scope row is tombstoned")
  assert_identical(trim_text(by_id("act-out")$inactive_reason), "out_of_tracker_scope")
  assert_true(!(by_id("act-revive")$inactive %in% TRUE),
              "Back-in-scope scope-tombstoned row is revived")
  assert_true(by_id("act-teachout")$inactive %in% TRUE,
              "Teachout tombstone is never revived by the scope filter")
  assert_true(any(grepl("tombstoning 1 out-of-scope", msgs)),
              "Tombstoning must be reported with a count")

  # An already-tombstoned out-of-roster manual row must not abort the filter.
  manual_out <- make_tombstone_test_override("act-manual-out", "999",
                                             inactive = TRUE,
                                             inactive_reason = "out_of_tracker_scope")
  manual_out$source_row_origin <- "manual"
  filtered2 <- filter_accreditation_overrides_for_tracker_scope(
    dplyr::bind_rows(make_tombstone_test_override("act-in2", "100"), manual_out),
    tracker_unitids = "100"
  )
  assert_identical(nrow(filtered2), 2L)
})

run_test("Tombstoned approved rows are never published or resurrected by apply", function() {
  active <- make_tombstone_test_override("id-active", "100")
  tombstoned <- make_tombstone_test_override("id-dead", "200",
                                             inactive = TRUE,
                                             inactive_reason = "teachout_cleanup")
  overrides <- dplyr::bind_rows(active, tombstoned)

  make_action <- function(unitid, label) {
    data.frame(
      export_unitid = unitid,
      unitid = unitid,
      export_institution_name = "Example University",
      accreditor = "SACSCOC",
      action_date = "2025-12-01",
      action_type = "warning",
      action_label_raw = label,
      action_label_short = label,
      source_url = "https://example.org/existing",
      source_title = "Existing source",
      source_page_url = "https://example.org/existing",
      stringsAsFactors = FALSE
    )
  }
  actions_df <- dplyr::bind_rows(
    make_action("100", "Warning"),
    make_action("200", "Warning")
  )
  ids <- vapply(seq_len(nrow(actions_df)), function(i) {
    compute_accreditation_action_id(
      actions_df$unitid[[i]], actions_df$accreditor[[i]], actions_df$action_date[[i]],
      actions_df$action_label_raw[[i]], actions_df$export_unitid[[i]],
      actions_df$export_institution_name[[i]]
    )
  }, character(1))
  overrides$action_id <- ids

  # Enforce mode: the tombstoned action passes the identity check (its id is
  # a known override) but is withheld from publication like a reject.
  applied <- apply_accreditation_editorial_overrides(
    actions_df,
    overrides,
    enforce_review_gate = TRUE,
    allowed_action_ids = ids,
    drop_unlisted = TRUE,
    gate_mask = c(TRUE, TRUE)
  )
  assert_identical(nrow(applied), 1L)
  assert_identical(trim_text(applied$action_id[[1]]), ids[[1]])

  # And a tombstoned override with no matching recomputed action must not
  # resurrect as a review-backed export row.
  applied2 <- apply_accreditation_editorial_overrides(
    actions_df[1, , drop = FALSE],
    overrides,
    enforce_review_gate = TRUE,
    allowed_action_ids = ids[[1]],
    drop_unlisted = TRUE,
    gate_mask = TRUE
  )
  assert_true(!(ids[[2]] %in% trim_text(applied2$action_id)),
              "Tombstoned override must not resurrect into the export")
})

run_test("Sheet-facing selection and staging both respect tombstones", function() {
  cuts_df <- data.frame(
    cut_id = "cut-live",
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

  # Tombstone the staged row, then re-stage the same candidate: it must NOT
  # come back as a new row (dedup sees the tombstone).
  staged$inactive <- TRUE
  staged$inactive_reason <- "teachout_cleanup"
  restaged <- stage_college_cuts_editorial_overrides(
    candidates,
    existing = staged,
    first_seen = "2026-06-03",
    tracker_unitids = "100"
  )
  assert_identical(nrow(restaged), 1L)
  assert_true(restaged$inactive %in% TRUE, "Tombstone survives re-staging")

  # The sheet-facing filter must exclude the tombstoned row so it can never
  # be appended (or re-appended) to the review tab.
  sheet_rows <- filter_college_cuts_overrides_for_review_sheet(
    restaged,
    candidate_cut_ids = candidates$cut_id,
    tracker_unitids = "100"
  )
  assert_identical(nrow(sheet_rows), 0L)
})

run_test("Cross-source fold never flips a tombstoned approved row to unreviewed", function() {
  tombstoned <- make_tombstone_test_override(
    "act-dead-approved", "100",
    review_status = "approved",
    inactive = TRUE,
    inactive_reason = "teachout_cleanup"
  )
  # Same institution/accreditor, 6 days apart, same type, reworded label:
  # similar enough to suppress, different enough that an active approved row
  # would flip to unreviewed.
  candidate <- data.frame(
    action_id = "new-reworded",
    unitid = "100",
    institution_name = "Example University",
    accreditor = "SACSCOC",
    action_date = "2025-12-07",
    action_type = "warning",
    action_label_raw = "Warning continued for good cause",
    generated_statement = "Warning continued for good cause",
    source_url = "https://example.org/new",
    source_title = "New source",
    row_origin = "scraper",
    stringsAsFactors = FALSE
  )
  tombstoned$source_action_label_raw <- "Warning continued"
  tombstoned$source_generated_statement <- "Warning continued"

  staged <- stage_accreditation_editorial_overrides(candidate, tombstoned, first_seen = "2026-07-08")
  assert_true(!("new-reworded" %in% trim_text(staged$action_id)),
              "Reworded duplicate must still be suppressed by the tombstoned row")
  row <- staged[trim_text(staged$action_id) == "act-dead-approved", , drop = FALSE]
  assert_identical(trim_text(row$review_status), "approved",
                   "Tombstoned approved row must keep its review_status")
  assert_identical(trim_text(row$reviewer), "MV",
                   "Tombstoned approved row must keep its reviewer metadata")
  assert_true(row$inactive %in% TRUE, "Row stays tombstoned")

  # Control: the same fold against an ACTIVE approved row does flip.
  active <- tombstoned
  active$inactive <- FALSE
  active$inactive_reason <- NA_character_
  staged2 <- stage_accreditation_editorial_overrides(candidate, active, first_seen = "2026-07-08")
  row2 <- staged2[trim_text(staged2$action_id) == "act-dead-approved", , drop = FALSE]
  assert_identical(trim_text(row2$review_status), "unreviewed",
                   "Active approved row with changed raw text must flip to unreviewed")
})

run_test("Stale sheet-only accreditation rows drop before merge; decisions quarantine; manual rows stay", function() {
  make_sheet_row <- function(action_id, row_origin = "scraper",
                             review_status = "unreviewed", reviewer = "") {
    data.frame(
      action_id = action_id,
      unitid = "100",
      institution_name = "Example University",
      accreditor = "MSCHE",
      action_date = "2026-04-24",
      action_type = "warning",
      action_label_raw = "Warning",
      generated_statement = "Warning",
      source_url = "https://example.org/one",
      source_title = "Source",
      row_origin = row_origin,
      first_seen = "2026-05-01",
      review_status = review_status,
      reviewer = reviewer,
      reviewer_notes = "",
      reviewed_at = "",
      grandfathered = FALSE,
      stringsAsFactors = FALSE
    )
  }
  sheet_rows <- dplyr::bind_rows(
    make_sheet_row("act-local", review_status = "approved", reviewer = "MV"),
    make_sheet_row("act-candidate-only"),
    make_sheet_row("act-stale-undecided"),
    make_sheet_row("act-stale-approved", review_status = "approved", reviewer = "MV"),
    make_sheet_row("act-manual-only", row_origin = "manual", review_status = "approved", reviewer = "MV")
  )

  filtered <- drop_stale_accreditation_sheet_rows(
    sheet_rows = sheet_rows,
    local_action_ids = "act-local",
    candidate_action_ids = "act-candidate-only"
  )

  kept_ids <- sort(trim_text(filtered$kept_rows$action_id))
  assert_identical(kept_ids, c("act-candidate-only", "act-local", "act-manual-only"))
  assert_identical(trim_text(filtered$dropped_rows$action_id), "act-stale-undecided")
  assert_identical(trim_text(filtered$quarantined_rows$action_id), "act-stale-approved")
})

run_test("Unmatched cuts review sheet rows are built only from unresolved discovered candidates", function() {
  discovered <- data.frame(
    cut_id = c("discovered-keep-1", "discovered-keep-2", "discovered-resolved", "manual-ignore"),
    unitid = c("", NA_character_, "123456", ""),
    institution_name = c("Alias University", "Warning College", "Resolved State", "Manual School"),
    state = c("Ohio", "Texas", "Georgia", "Florida"),
    announcement_date = c("2026-07-09", "2026-07-08", "2026-07-07", "2026-07-06"),
    announcement_year = c("2026", "2026", "2026", "2026"),
    cut_type = c("staff_layoff", "other", "program_suspension", "other"),
    program_name = c("Alias University laid off 12 staff.", "Warning College announced several cuts.", "Resolved program suspension.", "Manual row should not stage."),
    generated_cut_label = c(
      "Alias University laid off 12 staff. [model: high confidence]",
      "UNCLASSIFIED TYPE: Warning College announced several cuts. [model: low confidence]",
      "Resolved program suspension. [model: medium confidence]",
      "Manual row should not stage. [model: medium confidence]"
    ),
    generated_cut_summary = c(
      "Alias University laid off 12 staff.",
      "Warning College announced several cuts.",
      "Resolved program suspension.",
      "Manual row should not stage."
    ),
    source_url = c(
      "https://example.org/alias",
      "https://example.org/warn",
      "https://example.org/resolved",
      "https://example.org/manual"
    ),
    source_title = c("Alias title", "Warning title", "Resolved title", "Manual title"),
    source_publication = c("Alias Press", "Warning Weekly", "Resolved Journal", "Manual News"),
    row_origin = c("news_scan", "warn_notice", "news_scan", "manual"),
    stringsAsFactors = FALSE
  )
  leads <- data.frame(
    url = c("https://example.org/alias", "https://example.org/alias", "https://example.org/warn"),
    first_seen = c("2026-07-10", "2026-07-08", "2026-07-09"),
    stringsAsFactors = FALSE
  )

  rows <- build_college_cuts_unmatched_review_sheet_rows(discovered, leads_df = leads)
  assert_identical(trim_text(rows$unmatched_id), c("discovered-keep-1", "discovered-keep-2"))
  assert_identical(trim_text(rows$first_seen), c("2026-07-08", "2026-07-09"))
  assert_identical(trim_text(rows$institution_name_raw), c("Alias University", "Warning College"))
  assert_identical(trim_text(rows$confidence), c("high", "low"))
  assert_identical(trim_text(rows$summary), c(
    "Alias University laid off 12 staff.",
    "Warning College announced several cuts."
  ))
  assert_true(all(is.na(rows$resolution_status)))
  assert_true(all(is.na(rows$resolution_notes)))
})

run_test("Unmatched cuts review sheet appends dedupe against existing unmatched ids", function() {
  discovered <- data.frame(
    cut_id = c("discovered-keep-1", "discovered-keep-2"),
    unitid = c("", ""),
    institution_name = c("Alias University", "Warning College"),
    state = c("Ohio", "Texas"),
    announcement_date = c("2026-07-09", "2026-07-08"),
    announcement_year = c("2026", "2026"),
    cut_type = c("staff_layoff", "other"),
    program_name = c("Alias University laid off 12 staff.", "Warning College announced several cuts."),
    generated_cut_label = c(
      "Alias University laid off 12 staff. [model: high confidence]",
      "UNCLASSIFIED TYPE: Warning College announced several cuts. [model: low confidence]"
    ),
    generated_cut_summary = c(
      "Alias University laid off 12 staff.",
      "Warning College announced several cuts."
    ),
    source_url = c("https://example.org/alias", "https://example.org/warn"),
    source_title = c("Alias title", "Warning title"),
    source_publication = c("Alias Press", "Warning Weekly"),
    row_origin = c("news_scan", "warn_notice"),
    stringsAsFactors = FALSE
  )
  existing_sheet <- data.frame(
    unmatched_id = "discovered-keep-1",
    first_seen = "2026-07-08",
    institution_name_raw = "Alias University",
    state = "Ohio",
    cut_type = "staff_layoff",
    announcement_date = "2026-07-09",
    confidence = "high",
    summary = "Alias University laid off 12 staff.",
    source_url = "https://example.org/alias",
    source_publication = "Alias Press",
    resolution_status = "needs_alias",
    resolution_notes = "Add alias",
    stringsAsFactors = FALSE
  )
  leads <- data.frame(
    url = c("https://example.org/alias", "https://example.org/warn"),
    first_seen = c("2026-07-08", "2026-07-09"),
    stringsAsFactors = FALSE
  )

  append_rows <- build_college_cuts_unmatched_review_sheet_append_rows(
    discovered,
    existing_sheet,
    leads_df = leads
  )
  assert_identical(trim_text(append_rows$unmatched_id), "discovered-keep-2")
  assert_identical(trim_text(append_rows$confidence), "low")
})

run_test("Closure flags review sheet rows track one institution_closure candidate per school and carry confirmations forward", function() {
  cuts <- data.frame(
    cut_id = c("sou-1", "lime-1", "lime-older"),
    matched_unitid = c("210146", "218238", "218238"),
    export_unitid = c("210146", "218238", "218238"),
    institution_name_display = c("Southern Oregon University", "Limestone University", "Limestone University"),
    announcement_date = c("2026-05-09", "2025-04-29", "2024-12-15"),
    announcement_year = c(2026L, 2025L, 2024L),
    cut_type = c("department_closure", "institution_closure", "institution_closure"),
    program_name = c(
      "3 majors eliminated and 66 positions cut to close $5M deficit after Deloitte review",
      "Board of Trustees voted April 29, 2025 to close after nearly 180 years, citing an unresolvable $6M budget shortfall.",
      "Older closure row"
    ),
    cut_label_public = c(
      "3 majors eliminated and 66 positions cut to close $5M deficit after Deloitte review",
      "Board of Trustees voted April 29, 2025 to close after nearly 180 years, citing an unresolvable $6M budget shortfall.",
      "Older closure row"
    ),
    source_url = c(
      "https://example.org/sou",
      "https://example.org/limestone",
      "https://example.org/limestone-older"
    ),
    is_primary_tracker = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  existing_sheet <- data.frame(
    cut_id = "lime-1",
    unitid = "218238",
    institution_name = "Limestone University",
    badge_kind = "closure",
    announcement_date = "2025-04-29",
    source_url = "https://example.org/limestone",
    evidence_text = "Board of Trustees voted April 29, 2025 to close after nearly 180 years, citing an unresolvable $6M budget shortfall.",
    flag_confirmed = "TRUE",
    notes = "Confirmed by editor",
    first_seen = "2026-07-01",
    stringsAsFactors = FALSE
  )

  rows <- build_college_cuts_closure_flags_review_sheet_rows(
    cuts,
    existing_sheet = existing_sheet,
    first_seen_date = "2026-07-14"
  )
  assert_identical(trim_text(rows$cut_id), "lime-1")
  assert_true(isTRUE(rows$flag_confirmed[[1]]))
  assert_identical(trim_text(rows$notes), "Confirmed by editor")
  assert_identical(trim_text(rows$first_seen), "2026-07-01")
  assert_identical(trim_text(rows$badge_kind), "closure")
  assert_identical(trim_text(rows$source_url), "https://example.org/limestone")
})

run_test("Closure flags review sheet appends dedupe against existing cut ids", function() {
  candidates <- data.frame(
    cut_id = c("lime-1", "cornish-1"),
    unitid = c("218238", "235024"),
    institution_name = c("Limestone University", "Cornish College of the Arts"),
    badge_kind = c("closure", "absorption"),
    announcement_date = c("2025-04-29", "2025-03-27"),
    source_url = c("https://example.org/limestone", "https://example.org/cornish"),
    evidence_text = c(
      "Board of Trustees voted April 29, 2025 to close after nearly 180 years, citing an unresolvable $6M budget shortfall.",
      "Cornish College absorbed by Seattle University; all 354 employees laid off with potential rehiring by Seattle U"
    ),
    flag_confirmed = c("TRUE", ""),
    notes = c("Confirmed by editor", ""),
    first_seen = c("2026-07-01", "2026-07-14"),
    stringsAsFactors = FALSE
  )
  existing_sheet <- data.frame(
    cut_id = "lime-1",
    unitid = "218238",
    institution_name = "Limestone University",
    badge_kind = "closure",
    announcement_date = "2025-04-29",
    source_url = "https://example.org/limestone",
    evidence_text = "Board of Trustees voted April 29, 2025 to close after nearly 180 years, citing an unresolvable $6M budget shortfall.",
    flag_confirmed = "TRUE",
    notes = "Confirmed by editor",
    first_seen = "2026-07-01",
    stringsAsFactors = FALSE
  )

  append_rows <- build_college_cuts_closure_flags_review_sheet_append_rows(
    candidates,
    existing_sheet
  )
  assert_identical(trim_text(append_rows$cut_id), "cornish-1")
  assert_true(!isTRUE(append_rows$flag_confirmed[[1]]))
  assert_identical(trim_text(append_rows$badge_kind), "absorption")
  assert_identical(trim_text(append_rows$source_url), "https://example.org/cornish")
})

run_test("Closure flags review sheet generates ids and preserves manual confirmed rows", function() {
  candidates <- data.frame(
    cut_id = "lime-1",
    matched_unitid = "218238",
    export_unitid = "218238",
    institution_name_display = "Limestone University",
    announcement_date = "2025-04-29",
    announcement_year = "2025",
    cut_type = "institution_closure",
    source_url = "https://example.org/limestone",
    program_name = "Institution closure",
    cut_label_public = "Board of Trustees voted April 29, 2025 to close after nearly 180 years.",
    is_primary_tracker = TRUE,
    stringsAsFactors = FALSE
  )
  existing_sheet <- data.frame(
    cut_id = "",
    unitid = "185129",
    institution_name = "New Jersey City University",
    badge_kind = "absorption",
    announcement_date = "2026-07-01",
    source_url = "https://www.nj.com/education/2026/07/2-nj-universities-complete-massive-merger.html",
    evidence_text = "New Jersey City University absorbed into Kean University; merger completed July 1, 2026.",
    flag_confirmed = "TRUE",
    notes = "",
    first_seen = "2026-07-14",
    stringsAsFactors = FALSE
  )

  rows <- build_college_cuts_closure_flags_review_sheet_rows(
    candidates,
    existing_sheet = existing_sheet,
    first_seen_date = "2026-07-14"
  )

  assert_true(nrow(rows) == 2)
  njcu <- rows[trim_text(rows$unitid) == "185129", , drop = FALSE]
  assert_true(nrow(njcu) == 1)
  assert_true(nzchar(trim_text(njcu$cut_id[[1]])))
  assert_identical(trim_text(njcu$badge_kind[[1]]), "absorption")
  assert_true(isTRUE(njcu$flag_confirmed[[1]]))
})
