if (!exists("run_test", mode = "function")) {
  source(file.path(getwd(), "tests", "test_support.R"))
}

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

run_test("College cuts legacy sheet rows normalize into the new visible schema", function() {
  legacy_rows <- data.frame(
    cut_id = c("cut-1", "cut-2"),
    unitid = c("100", "101"),
    institution_name = c("Example U", "Example V"),
    state = c("Alabama", "Georgia"),
    announcement_date = c("2026-04-24", "2026-05-01"),
    announcement_year = c("2026", "2026"),
    cut_type = c("program_closure", "layoff"),
    program_name = c("History BA", "Faculty layoffs"),
    source_url = c("https://example.org/one", "https://example.org/two"),
    source_title = c("Source one", "Source two"),
    source_publication = c("Paper one", "Paper two"),
    row_origin = c("scraper", "scraper"),
    first_seen = c("2026-05-01", "2026-05-02"),
    review_status = c("approved", "unreviewed"),
    editor_cut_description = c("Edited History BA", NA_character_),
    editor_cut_type = c("restructuring", NA_character_),
    editor_source_publication = c("Edited paper", NA_character_),
    editor_notes = c("checked", NA_character_),
    reviewer = c("editor@example.org", NA_character_),
    reviewed_at = c("2026-05-03", NA_character_),
    grandfathered = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  normalized <- coerce_college_cuts_review_sheet_rows(legacy_rows)
  assert_identical(nrow(normalized), 2L)
  assert_identical(normalized$cut_description[[1]], "Edited History BA")
  assert_identical(normalized$cut_type[[1]], "restructuring")
  assert_identical(normalized$source_publication[[1]], "Edited paper")
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

run_test("Manual college cuts rows with blank ids get stable generated ids", function() {
  manual_rows <- data.frame(
    cut_id = "",
    unitid = "",
    institution_name = "Manual College",
    state = "Ohio",
    announcement_date = "2026-06-02",
    announcement_year = "",
    cut_type = "layoff",
    cut_description = "Ten staff layoffs",
    source_url = "https://example.org/manual-cut",
    source_publication = "Manual paper",
    row_origin = "manual",
    review_status = "approved",
    stringsAsFactors = FALSE
  )

  first <- coerce_college_cuts_review_sheet_rows(manual_rows, default_first_seen = "2026-05-27")
  second <- coerce_college_cuts_review_sheet_rows(manual_rows, default_first_seen = "2026-05-27")

  assert_identical(first$cut_id[[1]], second$cut_id[[1]])
  assert_true(startsWith(first$cut_id[[1]], "editor-"))
  assert_identical(first$announcement_year[[1]], "2026")
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

run_test("Sheet-only manual college cuts rows merge without error", function() {
  local_overrides <- empty_college_cuts_editorial_overrides()
  sheet_rows <- data.frame(
    cut_id = "",
    unitid = "",
    institution_name = "Manual College",
    state = "Ohio",
    announcement_date = "2026-06-02",
    announcement_year = "",
    cut_type = "layoff",
    cut_description = "Ten staff layoffs",
    source_url = "https://example.org/manual-cut",
    source_publication = "Manual paper",
    row_origin = "manual",
    first_seen = "2026-05-27",
    review_status = "approved",
    reviewer = "editor@example.org",
    reviewer_notes = "manual",
    reviewed_at = "2026-05-27",
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  )

  merged <- merge_college_cuts_review_sheet_editor_columns(local_overrides, sheet_rows)
  assert_identical(nrow(merged), 1L)
  assert_identical(merged$source_row_origin[[1]], "manual")
  assert_identical(merged$review_status[[1]], "approved")
  assert_identical(merged$source_cut_description[[1]], "Ten staff layoffs")
})

run_test("Staging preserves existing manual college cuts rows", function() {
  candidates <- build_college_cuts_review_candidates(data.frame(
    cut_id = "cut-1",
    matched_unitid = "100",
    export_unitid = "100",
    institution_name_display = "Pipeline University",
    state_display = "Alabama",
    announcement_date = "2026-04-24",
    announcement_year = "2026",
    cut_type = "program_closure",
    program_name = "History BA",
    source_url = "https://example.org/pipeline-cut",
    source_title = "Pipeline source",
    source_publication = "Pipeline paper",
    stringsAsFactors = FALSE
  ))

  existing <- merge_college_cuts_review_sheet_editor_columns(
    empty_college_cuts_editorial_overrides(),
    data.frame(
      cut_id = "",
      unitid = "",
      institution_name = "Manual College",
      state = "Ohio",
      announcement_date = "2026-06-02",
      announcement_year = "",
      cut_type = "layoff",
      cut_description = "Ten staff layoffs",
      source_url = "https://example.org/manual-cut",
      source_publication = "Manual paper",
      row_origin = "manual",
      review_status = "approved",
      stringsAsFactors = FALSE
    ),
    first_seen = "2026-05-27"
  )

  staged <- stage_college_cuts_editorial_overrides(candidates, existing = existing, first_seen = "2026-05-28")
  assert_identical(nrow(staged), 2L)
  assert_true(any(staged$source_row_origin == "manual"))
  assert_true(any(trim_text(staged$cut_id) == "cut-1"))
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

run_test("College cuts visible-field edits publish and approved manual rows append", function() {
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

  staged <- stage_college_cuts_editorial_overrides(
    build_college_cuts_review_candidates(cuts_df),
    first_seen = "2026-05-27"
  )
  edited_sheet_rows <- data.frame(
    cut_id = build_college_cuts_review_sheet_rows(staged)$cut_id[[1]],
    unitid = "200",
    institution_name = "Corrected University",
    state = "Georgia",
    announcement_date = "2026-04-30",
    announcement_year = "2026",
    cut_type = "restructuring",
    cut_description = "Corrected History BA",
    source_url = "https://example.org/corrected-cut",
    source_publication = "Corrected paper",
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
    cut_id = "",
    unitid = "",
    institution_name = "Manual College",
    state = "Ohio",
    announcement_date = "2026-06-02",
    announcement_year = "",
    cut_type = "layoff",
    cut_description = "Ten staff layoffs",
    source_url = "https://example.org/manual-cut",
    source_publication = "Manual paper",
    row_origin = "manual",
    first_seen = "2026-05-27",
    review_status = "approved",
    reviewer = "editor@example.org",
    reviewer_notes = "manual",
    reviewed_at = "2026-05-28",
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  )

  merged <- merge_college_cuts_review_sheet_editor_columns(
    staged,
    dplyr::bind_rows(edited_sheet_rows, manual_sheet_row),
    first_seen = "2026-05-27"
  )
  applied <- apply_college_cuts_editorial_overrides(cuts_df, merged, enforce_review_gate = FALSE)

  assert_identical(nrow(applied), 2L)
  assert_identical(applied$matched_unitid[[1]], "200")
  assert_identical(applied$institution_name_display[[1]], "Corrected University")
  assert_identical(applied$cut_type[[1]], "restructuring")
  assert_identical(applied$program_name[[1]], "Corrected History BA")
  assert_identical(applied$source_publication[[1]], "Corrected paper")
  assert_true(any(applied$row_origin == "manual"))
  assert_true(any(applied$program_name == "Ten staff layoffs"))
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
  merged <- merge_college_cuts_review_sheet_editor_columns(
    empty_college_cuts_editorial_overrides(),
    data.frame(
      cut_id = "",
      unitid = "",
      institution_name = "Manual College",
      state = "Ohio",
      announcement_date = "2026-06-02",
      announcement_year = "",
      cut_type = "layoff",
      cut_description = "Ten staff layoffs",
      source_url = "https://example.org/manual-cut",
      source_publication = "Manual paper",
      row_origin = "manual",
      review_status = "unreviewed",
      stringsAsFactors = FALSE
    ),
    first_seen = "2026-05-27"
  )
  applied <- apply_college_cuts_editorial_overrides(cuts_df, merged, enforce_review_gate = FALSE)
  assert_identical(nrow(applied), 1L)
})
