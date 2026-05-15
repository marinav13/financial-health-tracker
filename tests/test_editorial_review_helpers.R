if (!exists("run_test", mode = "function")) {
  source(file.path(getwd(), "tests", "test_support.R"))
}

run_test("Editorial review action ids normalize cosmetic text changes", function() {
  canonical_id <- compute_accreditation_action_id(
    unitid = "100",
    accreditor = "MSCHE",
    action_date = "2026-04-24",
    action_label_raw = "To accept the institution's request to voluntarily surrender its accreditation."
  )
  cosmetic_variant_id <- compute_accreditation_action_id(
    unitid = " 100 ",
    accreditor = "msche",
    action_date = "2026/04/24",
    action_label_raw = "To accept the institution s request to voluntarily surrender its accreditation"
  )

  assert_identical(canonical_id, cosmetic_variant_id)
  assert_identical(nchar(canonical_id), 12L)
})

run_test("Editorial review candidates derive from final accreditation action rows", function() {
  actions_df <- data.frame(
    export_unitid = "100",
    unitid = "100",
    export_institution_name = "Example University",
    accreditor = "MSCHE",
    action_date = "2026-04-24",
    action_type = "adverse_action",
    action_label_raw = "Voluntary Withdrawal Received",
    action_label_short = "Voluntarily surrendered accreditation",
    source_url = NA_character_,
    source_title = "MSCHE action",
    source_page_url = "https://example.org/accreditation-page",
    stringsAsFactors = FALSE
  )

  candidates <- build_accreditation_review_candidates(actions_df)
  assert_identical(nrow(candidates), 1L)
  assert_identical(candidates$institution_name[[1]], "Example University")
  assert_identical(candidates$generated_statement[[1]], "Voluntarily surrendered accreditation")
  assert_identical(candidates$source_url[[1]], "https://example.org/accreditation-page")
  assert_identical(candidates$row_origin[[1]], "scraper")
})

run_test("Stage accreditation review appends only new ids and preserves editor columns", function() {
  tmp_dir <- tempfile("stage-accreditation-review-")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  candidates_path <- file.path(tmp_dir, "accreditation_review_candidates.csv")
  overrides_path <- file.path(tmp_dir, "editorial_overrides.csv")

  existing_id <- compute_accreditation_action_id(
    unitid = "100",
    accreditor = "MSCHE",
    action_date = "2026-04-24",
    action_label_raw = "Voluntary Withdrawal Received"
  )
  new_id <- compute_accreditation_action_id(
    unitid = "100",
    accreditor = "MSCHE",
    action_date = "2025-06-26",
    action_label_raw = "Warning"
  )

  readr::write_csv(
    data.frame(
      action_id = c(existing_id, new_id),
      unitid = c("100", "100"),
      institution_name = c("Example University", "Example University"),
      accreditor = c("MSCHE", "MSCHE"),
      action_date = c("2026-04-24", "2025-06-26"),
      action_type = c("adverse_action", "warning"),
      action_label_raw = c("Voluntary Withdrawal Received", "Warning"),
      generated_statement = c("Voluntarily surrendered accreditation", "Warning"),
      source_url = c("https://example.org/accreditation", "https://example.org/warning"),
      source_title = c("MSCHE action", "Warning action"),
      row_origin = c("scraper", "scraper"),
      stringsAsFactors = FALSE
    ),
    candidates_path,
    na = ""
  )

  readr::write_csv(
    data.frame(
      action_id = existing_id,
      unitid = "100",
      institution_name = "Example University",
      accreditor = "MSCHE",
      action_date = "2026-04-24",
      action_type = "adverse_action",
      action_label_raw = "Voluntary Withdrawal Received",
      generated_statement = "",
      source_url = "https://example.org/accreditation",
      source_title = "MSCHE action",
      row_origin = "scraper",
      first_seen = "2026-05-01",
      review_status = "approved",
      editor_rewrite = "Editor-approved wording",
      editor_notes = "checked",
      reviewer = "editor@example.org",
      reviewed_at = "2026-05-02",
      grandfathered = FALSE,
      stringsAsFactors = FALSE
    ),
    overrides_path,
    na = ""
  )

  script_env <- new.env(parent = globalenv())
  sys.source(file.path(root, "scripts", "stage_accreditation_review.R"), envir = script_env)
  script_env$main(c(
    "--input", candidates_path,
    "--output", overrides_path,
    "--first-seen", "2026-05-15"
  ))
  script_env$main(c(
    "--input", candidates_path,
    "--output", overrides_path,
    "--first-seen", "2026-05-15"
  ))

  staged <- read_accreditation_editorial_overrides(overrides_path)
  staged_lines <- readLines(overrides_path, warn = FALSE)
  assert_identical(nrow(staged), 2L)
  assert_true(is.character(staged$first_seen),
    "first_seen should remain character after reading editorial_overrides.csv.")
  assert_true(any(grepl("2026-05-01", staged_lines, fixed = TRUE)),
    "editorial_overrides.csv should persist first_seen as an ISO date string.")
  assert_true(any(grepl("FALSE", staged_lines, fixed = TRUE)),
    "editorial_overrides.csv should persist grandfathered as TRUE/FALSE text.")
  assert_true(grepl("editor_action_label_short", staged_lines[[1]], fixed = TRUE),
    "editorial_overrides.csv should write the explicit editor override schema.")
  assert_true(!grepl("editor_rewrite", staged_lines[[1]], fixed = TRUE),
    "editorial_overrides.csv should migrate away from the legacy editor_rewrite header.")

  existing_row <- staged[staged$action_id == existing_id, , drop = FALSE]
  new_row <- staged[staged$action_id == new_id, , drop = FALSE]

  assert_identical(existing_row$review_status[[1]], "approved")
  assert_identical(existing_row$editor_action_label_short[[1]], "Editor-approved wording")
  assert_identical(existing_row$first_seen[[1]], "2026-05-01")
  assert_identical(existing_row$generated_statement[[1]], "Voluntarily surrendered accreditation")
  assert_true(is.na(existing_row$editor_action_date[[1]]))
  assert_true(is.na(existing_row$editor_action_type[[1]]))
  assert_true(is.na(existing_row$editor_source_url[[1]]))
  assert_true(is.na(existing_row$editor_source_title[[1]]))

  assert_identical(new_row$review_status[[1]], "unreviewed")
  assert_identical(new_row$first_seen[[1]], "2026-05-15")
  assert_true(is.na(new_row$editor_action_label_short[[1]]))
  assert_true(is.na(new_row$editor_action_date[[1]]))
  assert_true(is.na(new_row$editor_action_type[[1]]))
  assert_true(is.na(new_row$editor_source_url[[1]]))
  assert_true(is.na(new_row$editor_source_title[[1]]))
})

run_test("Accreditation review sheet append rows only include ids missing from the sheet", function() {
  existing_id <- compute_accreditation_action_id(
    unitid = "100",
    accreditor = "MSCHE",
    action_date = "2026-04-24",
    action_label_raw = "Voluntary Withdrawal Received"
  )
  new_id <- compute_accreditation_action_id(
    unitid = "100",
    accreditor = "MSCHE",
    action_date = "2025-06-26",
    action_label_raw = "Warning"
  )

  overrides <- coerce_accreditation_editorial_overrides(data.frame(
    action_id = c(existing_id, new_id),
    unitid = c("100", "100"),
    institution_name = c("Example University", "Example University"),
    accreditor = c("MSCHE", "MSCHE"),
    action_date = c("2026-04-24", "2025-06-26"),
    action_type = c("adverse_action", "warning"),
    action_label_raw = c("Voluntary Withdrawal Received", "Warning"),
    generated_statement = c("Voluntarily surrendered accreditation", "Placed on warning"),
    source_url = c("https://example.org/accreditation", "https://example.org/warning"),
    source_title = c("MSCHE action", "Warning action"),
    row_origin = c("scraper", "scraper"),
    first_seen = c("2026-05-01", "2026-05-15"),
    review_status = c("approved", "unreviewed"),
    grandfathered = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  ))
  sheet_rows <- coerce_accreditation_editorial_overrides(data.frame(
    action_id = existing_id,
    unitid = "100",
    institution_name = "Example University",
    accreditor = "MSCHE",
    action_date = "2026-04-24",
    action_type = "adverse_action",
    action_label_raw = "Voluntary Withdrawal Received",
    generated_statement = "Voluntarily surrendered accreditation",
    source_url = "https://example.org/accreditation",
    source_title = "MSCHE action",
    row_origin = "scraper",
    first_seen = "2026-05-01",
    review_status = "approved",
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  ))

  append_rows <- build_accreditation_review_sheet_append_rows(overrides, sheet_rows)
  assert_identical(nrow(append_rows), 1L)
  assert_identical(append_rows$action_id[[1]], new_id)
})

run_test("Google Sheet editor columns overwrite local editor fields only", function() {
  existing_id <- compute_accreditation_action_id(
    unitid = "100",
    accreditor = "MSCHE",
    action_date = "2026-04-24",
    action_label_raw = "Voluntary Withdrawal Received"
  )

  local_overrides <- coerce_accreditation_editorial_overrides(data.frame(
    action_id = existing_id,
    unitid = "100",
    institution_name = "Example University",
    accreditor = "MSCHE",
    action_date = "2026-04-24",
    action_type = "adverse_action",
    action_label_raw = "Voluntary Withdrawal Received",
    generated_statement = "Voluntarily surrendered accreditation",
    source_url = "https://example.org/accreditation",
    source_title = "MSCHE action",
    row_origin = "scraper",
    first_seen = "2026-05-01",
    review_status = "unreviewed",
    editor_action_label_short = NA_character_,
    editor_action_date = NA_character_,
    editor_source_url = NA_character_,
    reviewer = NA_character_,
    reviewed_at = NA_character_,
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  ))

  sheet_rows <- coerce_accreditation_editorial_overrides(data.frame(
    action_id = existing_id,
    unitid = "999",
    institution_name = "Do Not Trust Sheet System Columns",
    accreditor = "MSCHE",
    action_date = "1999-01-01",
    action_type = "warning",
    action_label_raw = "Wrong system text",
    generated_statement = "Wrong generated statement",
    source_url = "https://example.org/wrong-system-url",
    source_title = "Wrong source title",
    row_origin = "scraper",
    first_seen = "1999-01-01",
    review_status = "approved",
    editor_action_label_short = "Editor-approved wording",
    editor_action_date = "2026-04-30",
    editor_action_type = "warning",
    editor_source_url = "https://example.org/editor-source",
    editor_source_title = "Editor source title",
    editor_notes = "checked",
    reviewer = "editor@example.org",
    reviewed_at = "2026-05-02",
    grandfathered = TRUE,
    stringsAsFactors = FALSE
  ))

  merged <- merge_accreditation_review_sheet_editor_columns(local_overrides, sheet_rows)
  assert_identical(merged$unitid[[1]], "100")
  assert_identical(merged$institution_name[[1]], "Example University")
  assert_identical(merged$action_type[[1]], "adverse_action")
  assert_identical(merged$source_url[[1]], "https://example.org/accreditation")
  assert_identical(merged$first_seen[[1]], "2026-05-01")
  assert_identical(merged$grandfathered[[1]], FALSE)
  assert_identical(merged$review_status[[1]], "approved")
  assert_identical(merged$editor_action_label_short[[1]], "Editor-approved wording")
  assert_identical(merged$editor_action_date[[1]], "2026-04-30")
  assert_identical(merged$editor_action_type[[1]], "warning")
  assert_identical(merged$editor_source_url[[1]], "https://example.org/editor-source")
  assert_identical(merged$editor_source_title[[1]], "Editor source title")
})

run_test("Google Sheet pull fails loudly on unknown action ids", function() {
  local_overrides <- coerce_accreditation_editorial_overrides(data.frame(
    action_id = "known-id",
    unitid = "100",
    institution_name = "Example University",
    accreditor = "MSCHE",
    action_date = "2026-04-24",
    action_type = "adverse_action",
    action_label_raw = "Voluntary Withdrawal Received",
    generated_statement = "Voluntarily surrendered accreditation",
    source_url = "https://example.org/accreditation",
    source_title = "MSCHE action",
    row_origin = "scraper",
    first_seen = "2026-05-01",
    review_status = "unreviewed",
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  ))
  sheet_rows <- coerce_accreditation_editorial_overrides(data.frame(
    action_id = "editor-12345",
    unitid = "100",
    institution_name = "Example University",
    accreditor = "MSCHE",
    action_date = "2026-04-24",
    action_type = "adverse_action",
    action_label_raw = "Voluntary Withdrawal Received",
    generated_statement = "Voluntarily surrendered accreditation",
    source_url = "https://example.org/accreditation",
    source_title = "MSCHE action",
    row_origin = "editor",
    first_seen = "2026-05-01",
    review_status = "approved",
    grandfathered = FALSE,
    stringsAsFactors = FALSE
  ))

  err <- tryCatch(
    {
      merge_accreditation_review_sheet_editor_columns(local_overrides, sheet_rows)
      NULL
    },
    error = function(e) conditionMessage(e)
  )

  assert_true(
    !is.null(err) && grepl("Editor-added rows are not supported", err, fixed = TRUE),
    "Pull helper should fail clearly when the sheet contains unknown action_id values."
  )
})

run_test("Google Sheet header validation fails loudly when required columns are missing", function() {
  err <- tryCatch(
    {
      assert_accreditation_review_sheet_header(data.frame(
        action_id = "known-id",
        review_status = "approved",
        stringsAsFactors = FALSE
      ))
      NULL
    },
    error = function(e) conditionMessage(e)
  )

  assert_true(
    !is.null(err) && grepl("Google Sheet tab is missing required columns", err, fixed = TRUE),
    "Broken Google Sheet headers should fail with an explicit required-columns error."
  )
})

run_test("Grandfather helper approves current unreviewed rows without overwriting existing approvals", function() {
  overrides <- coerce_accreditation_editorial_overrides(data.frame(
    action_id = c("row-1", "row-2"),
    unitid = c("100", "101"),
    institution_name = c("Example U", "Example V"),
    accreditor = c("MSCHE", "HLC"),
    action_date = c("2026-04-24", "2026-05-01"),
    action_type = c("adverse_action", "warning"),
    action_label_raw = c("Voluntary Withdrawal Received", "Warning"),
    generated_statement = c("Voluntarily surrendered accreditation", "Placed on warning"),
    source_url = c("https://example.org/one", "https://example.org/two"),
    source_title = c("Source one", "Source two"),
    row_origin = c("scraper", "scraper"),
    first_seen = c("2026-05-01", "2026-05-01"),
    review_status = c("unreviewed", "approved"),
    reviewer = c(NA_character_, "editor@example.org"),
    reviewed_at = c(NA_character_, "2026-05-02"),
    grandfathered = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  ))

  grandfathered <- grandfather_accreditation_editorial_overrides(
    overrides,
    reviewed_at = "2026-05-15",
    reviewer = "grandfathered"
  )

  assert_identical(grandfathered$review_status[[1]], "approved")
  assert_identical(grandfathered$grandfathered[[1]], TRUE)
  assert_identical(grandfathered$reviewer[[1]], "grandfathered")
  assert_identical(grandfathered$reviewed_at[[1]], "2026-05-15")

  assert_identical(grandfathered$review_status[[2]], "approved")
  assert_identical(grandfathered$grandfathered[[2]], FALSE)
  assert_identical(grandfathered$reviewer[[2]], "editor@example.org")
  assert_identical(grandfathered$reviewed_at[[2]], "2026-05-02")
})

run_test("Editorial overrides helper applies approved edits and gate filtering", function() {
  actions_df <- data.frame(
    export_unitid = c("100", "101"),
    unitid = c("100", "101"),
    export_institution_name = c("Example University", "Example College"),
    accreditor = c("MSCHE", "HLC"),
    action_date = c("2026-04-24", "2025-06-26"),
    action_type = c("adverse_action", "warning"),
    action_label_raw = c("Voluntary Withdrawal Received", "Warning"),
    action_label_short = c("Voluntarily surrendered accreditation", "Placed on warning"),
    source_url = c("https://example.org/one", "https://example.org/two"),
    source_title = c("Source One", "Source Two"),
    stringsAsFactors = FALSE
  )

  overrides <- coerce_accreditation_editorial_overrides(data.frame(
    action_id = c(
      compute_accreditation_action_id("100", "MSCHE", "2026-04-24", "Voluntary Withdrawal Received"),
      compute_accreditation_action_id("101", "HLC", "2025-06-26", "Warning")
    ),
    unitid = c("100", "101"),
    institution_name = c("Example University", "Example College"),
    accreditor = c("MSCHE", "HLC"),
    action_date = c("2026-04-24", "2025-06-26"),
    action_type = c("adverse_action", "warning"),
    action_label_raw = c("Voluntary Withdrawal Received", "Warning"),
    generated_statement = c("Voluntarily surrendered accreditation", "Placed on warning"),
    source_url = c("https://example.org/one", "https://example.org/two"),
    source_title = c("Source One", "Source Two"),
    row_origin = c("scraper", "scraper"),
    first_seen = c("2026-05-01", "2026-05-01"),
    review_status = c("approved", "unreviewed"),
    editor_action_label_short = c("Voluntary surrender approved.", NA_character_),
    editor_action_date = c("2026-04-30", NA_character_),
    editor_action_type = c("other", NA_character_),
    editor_source_url = c("https://example.org/editor-one", NA_character_),
    editor_source_title = c("Edited Source One", NA_character_),
    grandfathered = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  ))

  ungated <- apply_accreditation_editorial_overrides(actions_df, overrides, enforce_review_gate = FALSE)
  assert_identical(nrow(ungated), 2L)
  assert_identical(ungated$action_label_short[[1]], "Voluntary surrender approved.")
  assert_identical(ungated$action_date[[1]], "2026-04-30")
  assert_identical(ungated$action_type[[1]], "other")
  assert_identical(ungated$source_url[[1]], "https://example.org/editor-one")
  assert_identical(ungated$source_title[[1]], "Edited Source One")

  gated <- apply_accreditation_editorial_overrides(actions_df, overrides, enforce_review_gate = TRUE)
  assert_identical(nrow(gated), 1L)
  assert_identical(gated$unitid[[1]], "100")
  assert_identical(gated$review_status[[1]], "approved")
})
