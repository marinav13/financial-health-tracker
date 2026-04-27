run_test("slug_institution_name normalisation rules", function() {
  # Basic lowercase + non-alnum → hyphen
  assert_identical(slug_institution_name("Example University"), "example-university")
  # Strip leading "The "
  assert_identical(slug_institution_name("The Ohio State University"), "ohio-state-university")
  # St / St. → saint
  assert_identical(slug_institution_name("St. John's University"), "saint-john-s-university")
  assert_identical(slug_institution_name("St Xavier University"),  "saint-xavier-university")
  # & → and
  assert_identical(slug_institution_name("Arts & Sciences College"), "arts-and-sciences-college")
  # Edge cases
  assert_identical(slug_institution_name(""), "")
  assert_identical(slug_institution_name(NA_character_), "")
})

run_test("make_export_id uses unitid when present, slug otherwise", function() {
  # Numeric unitid is returned as-is (no prefix)
  assert_identical(make_export_id("cut", "12345", "Name", "NY"), "12345")
  # Slug format: prefix-name-slug--state-slug
  assert_identical(
    make_export_id("cut", "", "Example University", "New York"),
    "cut-example-university--new-york"
  )
  # "--" separator keeps name and state visually distinct
  assert_identical(
    make_export_id("accred", NA_character_, "The Ohio State University", "Ohio"),
    "accred-ohio-state-university--ohio"
  )
  # St → saint propagated through make_export_id
  assert_identical(
    make_export_id("cut", "", "St. Mary's College", "California"),
    "cut-saint-mary-s-college--california"
  )
  # Missing state: just name slug
  assert_identical(
    make_export_id("cut", "", "Example College", ""),
    "cut-example-college"
  )
})

run_test("Export helper basics", function() {
  temp_file <- tempfile("local-file-")
  writeLines("ok", temp_file)
  require_local_file(temp_file, "temp file", "unused")

  assert_identical(make_export_id("cut", "12345", "Name", "NY"), "12345")
  assert_identical(
    make_export_id("cut", "", "Example University", "NY"),
    "cut-example-university--ny"
  )
  assert_identical(
    normalize_display_institution_name("Arizona State University Campus Immersion"),
    "Arizona State University"
  )
  assert_identical(or_null(c("", "filled")), NA)
  assert_identical(normalize_control_label("private non-profit"), "Private not-for-profit")
  assert_identical(derive_positions_affected(NA, "The college laid off 23 employees", "", "", ""), 23L)
  assert_equal(
    build_series(data.frame(year = c(2024L, 2025L), value = c(1, NA_real_)), "value"),
    list(list(year = 2024L, value = 1))
  )
})

run_test("International sentence builder", function() {
  sentence <- build_international_students_sentence(2024, 0.12, 0.08, 0.21)
  assert_true(grepl("In 2024, 12", sentence, fixed = TRUE))
  assert_true(grepl("8", sentence, fixed = TRUE))
  assert_true(grepl("21", sentence, fixed = TRUE))
})

run_test("Export bundle helpers", function() {
  temp_dir <- tempfile("export-bundle-")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  school <- list(
    unitid = "123",
    financial_unitid = "123",
    has_financial_profile = TRUE,
    is_primary_tracker = TRUE,
    institution_name = "Arizona State University Campus Immersion",
    city = "Tempe",
    state = "AZ",
    control_label = "Public",
    category = "Research"
  )

  assert_identical(
    build_institution_unique_name(school$institution_name, school$city, school$state),
    "Arizona State University | Tempe | AZ"
  )

  picked <- pick_first_present(
    data.frame(primary = NA_character_, fallback = "kept", stringsAsFactors = FALSE),
    c("missing", "primary", "fallback")
  )
  assert_identical(picked[[1]], "kept")

  entry <- build_school_index_entry(school, extra_flag = TRUE)
  assert_identical(entry$institution_unique_name, "Arizona State University | Tempe | AZ")
  assert_true(isTRUE(entry$extra_flag))

  bundle_paths <- write_export_bundle(
    export_obj = list(schools = list(school)),
    data_dir = temp_dir,
    export_filename = "schools.json",
    index_filename = "schools-index.json",
    index_builder = function(one_school) list(extra_flag = TRUE)
  )

  assert_true(file.exists(bundle_paths$export_path))
  assert_true(file.exists(bundle_paths$index_path))

  index_rows <- jsonlite::read_json(bundle_paths$index_path, simplifyVector = TRUE)
  assert_identical(index_rows$institution_unique_name[[1]], "Arizona State University | Tempe | AZ")
  assert_true(isTRUE(index_rows$extra_flag[[1]]))
})

run_test("write_json_file strips mid-file null bytes and validates JSON", function() {
  tmp <- tempfile("write_json_null_", fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  # Write a valid JSON file, then inject null bytes at the start, middle, and end
  writeLines('{"a":1,"b":2}', con = tmp)
  raw_clean <- readBin(tmp, raw(), n = file.info(tmp)$size)
  null_byte  <- as.raw(0x00)
  # Inject three null bytes: one at position 2, one in the middle, one at the end
  mid <- length(raw_clean) %/% 2L
  corrupted <- c(raw_clean[1L], null_byte, raw_clean[2L:mid],
                 null_byte, raw_clean[(mid + 1L):length(raw_clean)], null_byte)
  con <- file(tmp, "wb"); writeBin(corrupted, con); close(con)

  # Overwrite via write_json_file with a clean object — should succeed and parse cleanly
  write_json_file(list(a = 1L, b = 2L), tmp)
  result <- jsonlite::read_json(tmp)
  assert_identical(result$a, 1L)
  assert_identical(result$b, 2L)

  # Sanity-check: no null bytes remain in the written file
  final_bytes <- readBin(tmp, raw(), n = file.info(tmp)$size)
  assert_identical(sum(final_bytes == as.raw(0x00)), 0L)
})

# ---------------------------------------------------------------------------
# H8: JSON schema validation
# ---------------------------------------------------------------------------

run_test("validate_json_schema: valid object file passes", function() {
  tmp <- tempfile("schema_valid_obj_", fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  writeLines('{"generated_at":"2024-01-01","schools":{"100":{"unitid":"100","institution_name":"Example U","cuts":[]}}}', tmp)

  errs <- validate_json_schema(
    path                = tmp,
    required_top_keys   = c("generated_at", "schools"),
    path_to_entries     = list("schools"),
    required_entry_keys = c("unitid", "institution_name", "cuts")
  )
  assert_identical(length(errs), 0L)
})

run_test("validate_json_schema: valid top-level array passes", function() {
  tmp <- tempfile("schema_valid_arr_", fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  writeLines('[{"unitid":"100","institution_name":"Example U","state":"MA"}]', tmp)

  errs <- validate_json_schema(
    path                = tmp,
    required_top_keys   = character(0),
    path_to_entries     = list(),
    required_entry_keys = c("unitid", "institution_name", "state")
  )
  assert_identical(length(errs), 0L)
})

run_test("validate_json_schema: missing required top-level key is caught", function() {
  tmp <- tempfile("schema_missing_top_", fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  # 'generated_at' intentionally omitted
  writeLines('{"schools":{}}', tmp)

  errs <- validate_json_schema(
    path                = tmp,
    required_top_keys   = c("generated_at", "schools"),
    path_to_entries     = NULL,
    required_entry_keys = character(0)
  )
  assert_true(length(errs) >= 1L)
  assert_true(any(grepl("generated_at", errs)))
})

run_test("validate_json_schema: missing required entry key is caught", function() {
  tmp <- tempfile("schema_missing_entry_", fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  # entry missing the 'cuts' key
  writeLines('{"generated_at":"2024","schools":{"100":{"unitid":"100","institution_name":"Example U"}}}', tmp)

  errs <- validate_json_schema(
    path                = tmp,
    required_top_keys   = c("generated_at", "schools"),
    path_to_entries     = list("schools"),
    required_entry_keys = c("unitid", "institution_name", "cuts")
  )
  assert_true(length(errs) >= 1L)
  assert_true(any(grepl("cuts", errs)))
})

run_test("validate_json_schema: missing file returns error string", function() {
  errs <- validate_json_schema(
    path                = "/nonexistent/path/file.json",
    required_top_keys   = c("generated_at"),
    path_to_entries     = NULL,
    required_entry_keys = character(0)
  )
  assert_true(length(errs) >= 1L)
  assert_true(any(grepl("does not exist", errs)))
})

run_test("validate_json_schema: empty schools container skips entry check", function() {
  tmp <- tempfile("schema_empty_schools_", fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  # schools is an empty dict — no entry to validate against
  writeLines('{"generated_at":"2024","schools":{}}', tmp)

  errs <- validate_json_schema(
    path                = tmp,
    required_top_keys   = c("generated_at", "schools"),
    path_to_entries     = list("schools"),
    required_entry_keys = c("unitid", "institution_name", "cuts")
  )
  assert_identical(length(errs), 0L)
})

run_test("validate_all_export_schemas: passes against actual data directory", function() {
  # Resolve the data directory relative to the repo root.
  # This test only runs when the exported JSON files are present; it is skipped
  # silently when none of the known files exist (e.g., fresh CI before first
  # export build).
  data_dir <- file.path(root, "data")

  present <- vapply(EXPORT_SCHEMAS, function(s) {
    file.exists(file.path(data_dir, s$filename))
  }, logical(1L))

  if (!any(present)) {
    message("validate_all_export_schemas: no export files found in data/ — skipping live check")
    return(invisible(NULL))
  }

  # Should complete without stopping
  result <- tryCatch(
    validate_all_export_schemas(data_dir),
    error = function(e) conditionMessage(e)
  )
  assert_true(
    isTRUE(result),
    paste0("validate_all_export_schemas() failed against data/:\n", result)
  )
})


# ---------------------------------------------------------------------------
# derive_action_label_short — Phase 2 MSCHE summarization
# ---------------------------------------------------------------------------

run_test("derive_action_label_short: non-MSCHE passthrough", function() {
  # HLC and other accreditors must keep their scrape-time label verbatim.
  assert_identical(
    derive_action_label_short("warning", "On Probation", "HLC"),
    "On Probation"
  )
  assert_identical(
    derive_action_label_short("adverse_action",
      "Accepted Teach-Out Plans (Master of Social Work degree at its Bedford, Cape Cod, and Fall River locations)",
      "NECHE"),
    "Accepted Teach-Out Plans (Master of Social Work degree at its Bedford, Cape Cod, and Fall River locations)"
  )
  assert_identical(
    derive_action_label_short("warning",
      "denied reaffirmation, continued accreditation, and continued the University of Lynchburg on Warning for twelve months",
      "SACSCOC"),
    "denied reaffirmation, continued accreditation, and continued the University of Lynchburg on Warning for twelve months"
  )
})

run_test("derive_action_label_short: MSCHE pattern 1 — Approved Teach-Out Plan with extracted scope", function() {
  # DeSales (MSCHE) — closure of an additional location.
  assert_identical(
    derive_action_label_short(
      "adverse_action",
      "To approve the teach-out plan for the closure of the additional location at DeSales Institute of Philosophy and Religion, Bangalore, India.",
      "MSCHE"
    ),
    "Approved Teach-Out Plan (closure of the additional location at DeSales Institute of Philosophy and Religion, Bangalore, India)"
  )
  # Saint Rose (MSCHE) — multi-institution agreements connector "and agreements with".
  assert_identical(
    derive_action_label_short(
      "adverse_action",
      "To approve the teach-out plan and agreements with several institutions.",
      "MSCHE"
    ),
    "Approved Teach-Out Plan (several institutions)"
  )
})

run_test("derive_action_label_short: MSCHE pattern 1 NEGATIVE — 'teach-out plan ... not necessary' must NOT classify", function() {
  # Swarthmore-style voluntary surrender mentions teach-out only to say
  # one is not required. Must NOT bucket as Teach-Out Plan; should
  # match Pattern 2 (voluntary surrender) instead.
  swarthmore_text <- paste0(
    "Staff acted on behalf of the Commission to acknowledge receipt of the notification, ",
    "dated April 2, 2026, of the institution's intent to change their primary accreditor, ",
    "voluntarily surrender accreditation, and terminate membership. ",
    "To note that a teach-out plan and teach-out agreements are not necessary because ",
    "the institution will retain its degree-granting authority."
  )
  result <- derive_action_label_short("adverse_action", swarthmore_text, "MSCHE")
  assert_true(
    !grepl("Teach-Out Plan", result, fixed = TRUE),
    paste0("'teach-out ... not necessary' phrasing must not bucket as Teach-Out Plan. Got: ", result)
  )
  assert_identical(result, "Voluntarily Surrendered Accreditation")
})

run_test("derive_action_label_short: MSCHE pattern 2 — Voluntarily Surrendered Accreditation", function() {
  # Bard-style notification of intent to change accreditor.
  bard_text <- paste0(
    "Staff acted on behalf of the Commission to acknowledge receipt of the notification, ",
    "dated March 3, 2026, of the institution's intent to change their primary accreditor, ",
    "voluntarily surrender accreditation, and terminate membership."
  )
  assert_identical(
    derive_action_label_short("adverse_action", bard_text, "MSCHE"),
    "Voluntarily Surrendered Accreditation"
  )
  # Word-form variant ("voluntary surrender") should also match.
  assert_identical(
    derive_action_label_short(
      "adverse_action",
      "To accept the institution's request for voluntary surrender of its accreditation.",
      "MSCHE"
    ),
    "Voluntarily Surrendered Accreditation"
  )
})

run_test("derive_action_label_short: MSCHE pattern 3 — Warning with Standard reference when present", function() {
  # Saint Rose 2023-06-22.
  assert_identical(
    derive_action_label_short(
      "warning",
      "To warn the institution that its accreditation may be in jeopardy because of insufficient evidence that the institution is currently in compliance with Standard VI.",
      "MSCHE"
    ),
    "Warning (Standard VI)"
  )
  # No Standard reference -> bare "Warning"
  assert_identical(
    derive_action_label_short(
      "warning",
      "To warn the institution that its accreditation may be in jeopardy.",
      "MSCHE"
    ),
    "Warning"
  )
})

run_test("derive_action_label_short: MSCHE pattern 4 — Removed from Probation", function() {
  assert_identical(
    derive_action_label_short(
      "removed",
      "To remove the institution from Probation.",
      "MSCHE"
    ),
    "Removed from Probation"
  )
})

run_test("derive_action_label_short: MSCHE pattern 5 — Continued on Warning with duration", function() {
  # Word-form duration ("twelve months") normalizes to numeric ("12 months").
  assert_identical(
    derive_action_label_short(
      "warning",
      "To continue the institution on Warning for twelve months.",
      "MSCHE"
    ),
    "Continued on Warning (12 months)"
  )
  # Numeric duration passes through unchanged.
  assert_identical(
    derive_action_label_short(
      "warning",
      "To continue the institution on Warning for 6 months.",
      "MSCHE"
    ),
    "Continued on Warning (6 months)"
  )
  # No duration -> bare "Continued on Warning"
  assert_identical(
    derive_action_label_short(
      "warning",
      "To continue the institution on Warning.",
      "MSCHE"
    ),
    "Continued on Warning"
  )
})

run_test("derive_action_label_short: MSCHE fallback — strip 'acknowledge receipt of' preamble, return first remaining sentence", function() {
  # Common MSCHE shape: an "acknowledge receipt of <X>." preamble
  # followed by the substantive action sentence.
  text <- paste0(
    "To acknowledge receipt of the substantive change request requested by the Commission action of December 18, 2023. ",
    "To include the institutional closure within the institution's scope of accreditation."
  )
  assert_identical(
    derive_action_label_short("adverse_action", text, "MSCHE"),
    "To include the institutional closure within the institution's scope of accreditation."
  )
  # 'Staff acted on behalf of the Commission to acknowledge receipt of <X>.' prefix.
  text2 <- paste0(
    "Staff acted on behalf of the Commission to acknowledge receipt of the monitoring report. ",
    "The next evaluation visit is scheduled for 2032-2033."
  )
  assert_identical(
    derive_action_label_short("monitoring", text2, "MSCHE"),
    "The next evaluation visit is scheduled for 2032-2033."
  )
})

run_test("derive_action_label_short: 'withdraw the substantive change request' must NOT be summarized as adverse", function() {
  # Negative pin paired with the classify_action negative test in
  # test_accreditation_scrapers.R. The summarizer must not invent an
  # adverse-sounding label out of administrative withdrawal phrasing.
  text <- "To withdraw the substantive change request as requested by the institution received on February 20, 2024."
  result <- derive_action_label_short("other", text, "MSCHE")
  assert_true(
    !grepl("Teach-Out Plan|Surrendered|Withdrawal of Accreditation", result),
    paste0("Substantive-change withdrawal must not surface adverse summary keywords. Got: ", result)
  )
})

run_test("derive_action_label_short: empty raw label falls back to action_type or 'Action'", function() {
  assert_identical(
    derive_action_label_short("warning", NA_character_, "HLC"),
    "warning"
  )
  assert_identical(
    derive_action_label_short(NA_character_, "", "MSCHE"),
    "Action"
  )
})


# ---------------------------------------------------------------------------
# Phase 4: derive_action_label_short — merger + teach-out agreement patterns
# ---------------------------------------------------------------------------

run_test("derive_action_label_short Phase 4: Pattern 0a — Merger with named partner + effective date", function() {
  # Albany College of Pharmacy and Health Sciences row (MSCHE, Feb 26, 2026):
  # body explicitly names the merging partner and the effective date.
  albany_text <- paste0(
    "To acknowledge receipt of the complex substantive change request. ",
    "To include the change in legal status, form of control, and ownership ",
    "within the institution's scope of accreditation, effective June 1, 2026. ",
    "To note the complex substantive change request includes the merger of ",
    "Albany College of Pharmacy and Health Sciences with Russell Sage College, ",
    "effective June 1, 2026, the anticipated date of the transaction."
  )
  assert_identical(
    derive_action_label_short("adverse_action", albany_text, "MSCHE"),
    "Merger with Russell Sage College (effective June 1, 2026)"
  )
})

run_test("derive_action_label_short Phase 4: Pattern 0a fallback — Change of Legal Status when merger partner not named", function() {
  # Drexel-style row (MSCHE): change in legal status without a named
  # merging counterparty. Falls through to the broader change-of-status
  # bucket so the effective date still surfaces.
  drexel_text <- paste0(
    "To include the change in legal status, form of control, and ownership ",
    "within the institution's scope of accreditation, effective June 30, 2024."
  )
  assert_identical(
    derive_action_label_short("adverse_action", drexel_text, "MSCHE"),
    "Change of Legal Status (effective June 30, 2024)"
  )
})

run_test("derive_action_label_short Phase 4: Pattern 0b — Approved Teach-Out Agreement with named partner", function() {
  # Albany College of Pharmacy second action (MSCHE, Feb 26, 2026): the
  # teach-out AGREEMENT with a single named partner is meaningfully
  # different from a teach-out PLAN approval (which Pattern 1 catches
  # for multi-institution batches).
  albany_agreement <- paste0(
    "To acknowledge receipt of the teach-out plan and teach-out agreement. ",
    "To approve the teach-out plan. ",
    "To approve the teach-out agreement with Russell Sage College, Troy, NY. ",
    "To note that Albany College of Pharmacy and Health Sciences' accreditation ",
    "cease date will be determined after the United States Department of Education ",
    "has approved the merger application."
  )
  assert_identical(
    derive_action_label_short("adverse_action", albany_agreement, "MSCHE"),
    "Approved Teach-Out Agreement with Russell Sage College"
  )
})

run_test("derive_action_label_short Phase 4: agreement pattern does NOT swallow teach-out PLAN rows", function() {
  # Pattern 0b matches "approve the teach-out AGREEMENT with X" specifically;
  # the older "approve the teach-out PLAN for X" / "...plan and agreements
  # with X" shapes (Pattern 1) must continue to win for those rows.
  desales_text <- paste0(
    "To approve the teach-out plan for the closure of the additional location ",
    "at DeSales Institute of Philosophy and Religion, Bangalore, India."
  )
  assert_identical(
    derive_action_label_short("adverse_action", desales_text, "MSCHE"),
    "Approved Teach-Out Plan (closure of the additional location at DeSales Institute of Philosophy and Religion, Bangalore, India)"
  )
})


run_test("derive_action_label_short Phase 4 hotfix: 'Staff acted on behalf' preamble stripped unconditionally", function() {
  # St. Francis College row (MSCHE): the preamble has "Staff acted on behalf
  # of the Commission to REQUEST" — not "to acknowledge receipt of". The old
  # fallback only stripped the acknowledge-receipt shape, so action_label_short
  # retained the full preamble and the JS isTrackedAction procedural filter
  # (anchored at "^to request") could not match. Phase 4 hotfix strips the
  # staff-acted phrase regardless of the verb that follows.
  st_francis_text <- paste0(
    "Staff acted on behalf of the Commission to request a supplemental ",
    "information report, due April 13, 2026, providing information on key ",
    "data indicators (financial health and federal financial responsibility)."
  )
  result <- derive_action_label_short("other", st_francis_text, "MSCHE")
  assert_true(
    !grepl("^Staff acted on behalf", result, fixed = TRUE),
    paste0("Staff-acted preamble must be stripped from fallback output. Got: ", result)
  )
  # First-letter capitalization: post-strip the next char is "to" (lowercase
  # in the original); the helper should capitalize so the output reads as
  # a standalone sentence.
  assert_true(
    grepl("^To request a supplemental information report", result),
    paste0("Post-strip output should start with capitalized 'To request'. Got: ", result)
  )
})
