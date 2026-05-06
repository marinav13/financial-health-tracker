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
      "Denied reaffirmation, continued accreditation, and continued the University of Lynchburg on Warning for twelve months",
      "SACSCOC"),
    "Denied reaffirmation, continued accreditation, and continued the University of Lynchburg on Warning for twelve months"
  )
})

run_test("derive_action_label_short: non-MSCHE extracts substantive SACSCOC action from letter text", function() {
  text <- paste0(
    "Morrison-Shetlar: The following action regarding your institution was taken by the Board of Trustees ",
    "of the Southern Association of Colleges and Schools Commission on Colleges (SACSCOC) during its meeting held on December 7, 2025: ",
    "The SACSCOC Board of Trustees denied reaffirmation of accreditation, continued accreditation, and continued the institution on Warning. ",
    "For twelve months for failure to comply with Standards 8.2.a and 13.3."
  )
  assert_identical(
    derive_action_label_short("notice", text, "SACSCOC"),
    "Denied reaffirmation of accreditation, continued accreditation, and continued the institution on Warning. For twelve months for failure to comply with Standards 8.2.a and 13.3."
  )
})

run_test("derive_action_label_short: non-MSCHE falls back to compact label for garbled SACSCOC OCR text", function() {
  text <- paste0(
    "Becerra: Bo ard of Trustees of the was ta ke n by th e ring its g ac tio n re ga rd ing your institution ",
    "m is si on on C ol le ges (SACSCOC) du The followin d Schools Com tion of Colleges an Southern Associa ",
    "ne 12, 2025: meeting held on Ju n’s Referral Report, continued vi ew ed th e insti tu tio re to ard of Trustees ",
    "re elve months for failu The SAC SCOC Bo n on Warning fo r tw cial cred ita tio n."
  )
  assert_identical(
    derive_action_label_short("warning", text, "SACSCOC"),
    "Placed on Warning"
  )
})

run_test("derive_action_label_short: strips SACSCOC Special Committee boilerplate from sanction summary", function() {
  text <- paste0(
    "Placed on Probation for Good Cause for twelve months for failure to comply with Standard 13.3 (Financial responsibility), ",
    "Standard 13.4 (Control of finances), and Standard 13.6 (Federal and state responsibilities) of the Principles of accreditation. ",
    "A Special Committee has been authorized to visit the institution to determine compliance with the standards cited above."
  )
  assert_identical(
    derive_action_label_short("probation", text, "SACSCOC"),
    "Placed on Probation for Good Cause for twelve months for failure to comply with Standard 13.3 (financial responsibility), Standard 13.4 (Control of finances), and Standard 13.6 (Federal and state responsibilities) of the Principles of accreditation."
  )
})

run_test("derive_action_label_short: NECHE heightened monitoring uses justification detail from notes", function() {
  assert_identical(
    derive_action_label_short(
      "notice",
      "JOINT PRESS RELEASE New England Commission of Higher Education and Hampshire College",
      "NECHE",
      "Heightened Monitoring or Focused Review | In danger of being found not to meet the Commission's standards on Organization and Governance and Institutional Resources"
    ),
    "In danger of being found not to meet the Commission's standards on Organization and Governance and Institutional Resources. Placed on Heightened Monitoring or Focused Review."
  )
})

run_test("derive_action_label_short: NECHE heightened monitoring strips duplicated trailing punctuation", function() {
  assert_identical(
    derive_action_label_short(
      "notice",
      "JOINT PRESS RELEASE New England Commission of Higher Education and Hellenic College",
      "NECHE",
      "Heightened Monitoring or Focused Review | The institution is in danger of not meeting the Commission's standards on Planning and Evaluation and Institutional Resources."
    ),
    "The institution is in danger of not meeting the Commission's standards on Planning and Evaluation and Institutional Resources. Placed on Heightened Monitoring or Focused Review."
  )
})

run_test("derive_action_label_short: NECHE show cause uses institutional resources detail", function() {
  text <- paste0(
    "The Commission has reason to believe that Hampshire College may no longer meet the standard on Institutional Resources, ",
    "the College be given an opportunity to show cause at the Commission's June 2026 meeting why the institution should not be placed on probation ",
    "or why its accreditation should not be withdrawn."
  )
  assert_identical(
    derive_action_label_short("show_cause", text, "NECHE"),
    "Asked to Show Cause for possible Probation or Withdrawal over Standard 7 (Institutional Resources) concerns"
  )
})

run_test("derive_action_label_short: closure announcement drops leading dates from action text", function() {
  text <- paste0(
    "On April 23, 2026, Anna Maria College announced that its Board of Trustees had voted on April 22, 2026 ",
    "to cease academic operations at the end of the Spring 2026 semester."
  )
  assert_identical(
    derive_action_label_short("adverse_action", text, "NECHE"),
    "Anna Maria College announced that its Board of Trustees had voted to cease academic operations at the end of the Spring 2026 semester."
  )
})

run_test("extract_teachout_partners fixes collapsed state-name OCR joins", function() {
  text <- paste(
    "Approved the institution's provisional plan and teach-out agreements with the following institutions:",
    "Calvin University, Grand Rapids, MI",
    "Saint Xavier University, Chicago, ILWheaton College, Wheaton, IL"
  )
  partners <- .extract_teachout_partners(text)
  assert_true("Wheaton College" %in% partners)
  assert_true("Saint Xavier University" %in% partners)
})

run_test("derive_action_label_short: SACSCOC disclosure statement extracts warning reason from readable section", function() {
  text <- paste0(
    "Disclosure Statement Regarding the Status of Marymount University. ",
    "What is the accreditation status of Marymount University? Marymount University is accredited by the Southern Association of Colleges and Schools Commission on Colleges. ",
    "The institution was continued in accreditation and placed on Warning following the Board's review of a Referral Report following submission of the institution's Fifth-Year Interim Report in March 2024. ",
    "Why was Marymount University placed on Warning? Marymount University was continued in accreditation and placed on Warning because the SACSCOC Board of Trustees determined that the institution has not yet demonstrated compliance with Core Requirement 13.1 (Financial resources), Standard 13.3 (Financial responsibility), Standard 13.4 (Control of finances), and Standard 13.6 (Federal and state responsibilities) of the Principles of Accreditation. ",
    "A Special Committee was not authorized to visit the institution."
  )
  assert_identical(
    derive_action_label_short("warning", text, "SACSCOC"),
    "Placed on Warning for failure to comply with Core Requirement 13.1 (financial resources), Standard 13.3 (financial responsibility), Standard 13.4 (Control of finances), and Standard 13.6 (Federal and state responsibilities) of the Principles of accreditation."
  )
})

run_test("derive_action_label_short: SACSCOC warning clause strips lead-in and dates", function() {
  text <- paste0(
    "The SACSCOC Board of Trustees reviewed the institution's Special Report following receipt of unsolicited information from the U.S. Department of Education indicating the institutions federal composite score failed to meet the standards for financial responsibility for fiscal year ended June 30, 2023, ",
    "and placed the institution on Warning for twelve (12) months for failure to comply with Standard 13.1 (Financial resources) and Standard 13.3 (Financial responsibility) of the Principles of Accreditation (Principles). ",
    "The institution is requested to submit a First Monitoring Report due April 1, 2026."
  )
  assert_identical(
    derive_action_label_short("warning", text, "SACSCOC"),
    "Placed on Warning for twelve (12) months for failure to comply with Standard 13.1 (financial resources) and Standard 13.3 (financial responsibility) of the Principles of accreditation (Principles)."
  )
})

run_test("derive_action_label_short: SACSCOC long sanction clauses collapse to standards-concerning summary", function() {
  text <- paste0(
    "The SACSCOC Board of Trustees denied reaffirmation of accreditation, continued accreditation, and placed the institution on Warning. ",
    "For twelve months for failure to comply with Core Requirement 6.1 (Full-time faculty), Core Requirement 7.1 (Institutional planning), Core Requirement 13.1 (Financial resources), Core Requirement 13.2 (Financial documents), ",
    "Standard 4.2.a (Mission review), Standard 4.2.c (CEO evaluation/selection), Standard 6.2.b (Program faculty), Standard 8.2.a (Student outcomes: educational programs), Standard 8.2.c (Student outcomes: academic and student services), Standard 10.9 (Cooperative academic arrangements), Standard 13.3 (Financial responsibility), Standard 13.4 (Control of finances), Standard 13.5 (Control of sponsored research/external funds), and Standard 13.6 (Federal and state responsibilities)."
  )
  assert_identical(
    derive_action_label_short("warning", text, "SACSCOC"),
    "Denied reaffirmation of accreditation, continued accreditation, and placed the institution on Warning. For twelve months for failure to comply with standards concerning faculty, institutional planning, financial resources, financial documents, mission and CEO evaluation/selection"
  )
})

run_test("derive_action_label_short: SACSCOC monitoring review lead-in is stripped from sanction summary", function() {
  text <- paste0(
    "The SACSCOC Board of Trustees reviewed the institution’s Monitoring Report, and denied reaffirmation, continued accreditation, and continued the institution on Warning for twelve months for failure to comply with Core Requirement 4.1 (Governing board characteristics), ",
    "Core Requirement 13.1 (Financial resources), and Standard 13.3 (Financial responsibility)."
  )
  assert_identical(
    derive_action_label_short("warning", text, "SACSCOC"),
    "Denied reaffirmation, continued accreditation, and continued the institution on Warning for twelve months for failure to comply with Core Requirement 4.1 (Governing board characteristics), Core Requirement 13.1 (financial resources), and Standard 13.3 (financial responsibility)."
  )
})

run_test("derive_action_label_short: non-MSCHE shortens NECHE show-cause correspondence", function() {
  text <- paste0(
    "Jennifer Chrisler President Hampshire College 893 West Street Amherst, MA 01002-3359 Dear President Chrisler: ",
    "I write to inform you that at its meeting on March 5, 2026, the New England Commission of Higher Education considered the Financial Screening Response report submitted by Hampshire College and took the following action: ",
    "that, because the Commission has reason to believe that Hampshire College may no longer meet the standard on Institutional Resources, the College be given an opportunity to show cause at the Commission’s June 2026 meeting why the institution should not be placed on probation or why its accreditation should not be withdrawn."
  )
  assert_identical(
    derive_action_label_short("show_cause", text, "NECHE"),
    "Asked to Show Cause for possible Probation or Withdrawal over Standard 7 (Institutional Resources) concerns"
  )
})

run_test("derive_action_label_short: non-MSCHE shortens NECHE show-cause press release wording", function() {
  text <- paste0(
    "On March 5, 2026, the New England Commission of Higher Education (NECHE) took action to require Hampshire College ",
    "to show cause at the Commission's June 2026 meeting why the institution should not be placed on probation ",
    "or why its accreditation should not be withdrawn."
  )
  assert_identical(
    derive_action_label_short("show_cause", text, "NECHE"),
    "Asked to Show Cause for Probation or Withdrawal of Accreditation"
  )
})

run_test("NECHE helpers: Hampshire-style text yields institutional resources concern signature", function() {
  text <- paste0(
    "The Commission has reason to believe that Hampshire College may no longer meet the standard on Institutional Resources, ",
    "the College be given an opportunity to show cause at the Commission's June 2026 meeting why the institution should not be placed on probation ",
    "or why its accreditation should not be withdrawn."
  )
  assert_identical(
    extract_neche_standard_families(text),
    "institutional_resources"
  )
  assert_identical(
    get_neche_concern_signature(text),
    "institutional_resources"
  )
})

run_test("NECHE helpers: fallback label preserves formal standard name when no explicit concern is stated", function() {
  text <- paste0(
    "The Commission has reason to believe that Hampshire College may no longer meet the standard on Institutional Resources, ",
    "the College be given an opportunity to show cause at the Commission's June 2026 meeting why the institution should not be placed on probation ",
    "or why its accreditation should not be withdrawn."
  )
  assert_identical(
    .build_neche_standard_concern_label(text),
    "Standard 7 (Institutional Resources) concerns"
  )
})

run_test("NECHE helpers: unmapped standard family does not produce a compaction signature", function() {
  text <- "The Commission found the institution may not meet the standard on students and public disclosure."
  assert_true(is.na(get_neche_concern_signature(text)))
})

run_test("derive_action_label_short: HLC teach-out summary names counterpart institutions", function() {
  text <- paste0(
    "Approved the institution’s provisional for students enrolled in the following programs. ",
    "This includes approval of the teach-out agreements with the following institutions: ",
    "A.T. Still University of Health Science, Kirksville, MO Columbia College, Columbia, MO ",
    "Grand View University, Des Moines, IA Harris-Stowe State University, St. Louis, MO"
  )
  assert_identical(
    derive_action_label_short("adverse_action", text, "HLC"),
    "Approved provisional plan and teach-out agreements with A.T. Still University of Health Science, Columbia College, Grand View University, and others"
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
    "Merger of Albany College of Pharmacy and Health Sciences with Russell Sage College (effective June 1, 2026)"
  )
  # Symmetry check: the same MSCHE action body appears on Russell Sage's
  # accreditation page. Naming both institutions in the label ensures
  # the row reads correctly regardless of whose page it is shown on
  # (the prior "Merger with <Y>" shape rendered as a self-reference on
  # Y's own row).
  russell_sage_text <- paste0(
    "To note the complex substantive change request includes the merger of ",
    "Albany College of Pharmacy and Health Sciences with Russell Sage College, ",
    "effective June 1, 2026, the anticipated date of the transaction."
  )
  assert_identical(
    derive_action_label_short("adverse_action", russell_sage_text, "MSCHE"),
    "Merger of Albany College of Pharmacy and Health Sciences with Russell Sage College (effective June 1, 2026)"
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

run_test("derive_action_label_short Phase 4: Pattern 0a handles merger rows with comma after 'effective'", function() {
  saint_josephs_text <- paste0(
    "To acknowledge receipt of the complex substantive change request. ",
    "To include the change in legal status, form of control, and ownership ",
    "within the institution's scope of accreditation, effective January 3, 2024. ",
    "To note the complex substantive change request includes the merger of ",
    "Pennsylvania College of Health Sciences with Saint Joseph's University, ",
    "effective, January 3, 2024, the anticipated date of the transaction. ",
    "To note that Saint Joseph's University is the surviving institution."
  )
  assert_identical(
    derive_action_label_short("adverse_action", saint_josephs_text, "MSCHE"),
    "Merger of Pennsylvania College of Health Sciences with Saint Joseph's University (effective January 3, 2024)"
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


run_test("derive_action_label_short Phase 4 hotfix v3: Pattern 6 — Show Cause / Continued Show Cause with deadline", function() {
  # Metropolitan College of New York row (MSCHE, Nov 20, 2025): the
  # action has a multi-sentence body that begins with an
  # acknowledge-receipt preamble plus a "to note the follow-up team
  # visit" sentence. Without this pattern the fallback strips the
  # preamble and returns the follow-up-visit sentence, which the JS
  # procedural filter then drops -- erasing the show cause status from
  # the global table.
  metny_text <- paste0(
    "To acknowledge receipt of the show cause report. ",
    "To note the follow-up team visit by Commission representatives ",
    "to the main campus at 60 West Street on October 8-9, 2025. ",
    "To require the institution to continue to show cause by ",
    "February 27, 2026 to demonstrate why its accreditation should ",
    "not be withdrawn for non-compliance with Standards III, IV, V, and VII."
  )
  assert_identical(
    derive_action_label_short("show_cause", metny_text, "MSCHE"),
    "Continued Show Cause"
  )
  # First-time show cause (no "continue to") emits "Required to Show Cause".
  first_time_text <- paste0(
    "To require the institution to show cause by April 1, 2026 ",
    "to demonstrate why its accreditation should not be withdrawn."
  )
  assert_identical(
    derive_action_label_short("show_cause", first_time_text, "MSCHE"),
    "Required to Show Cause"
  )
  # Show cause without a captured deadline still surfaces the status.
  no_date_text <- "To require the institution to show cause why its accreditation should not be withdrawn."
  assert_identical(
    derive_action_label_short("show_cause", no_date_text, "MSCHE"),
    "Required to Show Cause"
  )
})

run_test("derive_action_label_short: MSCHE notification heading maps to non-compliance summary", function() {
  text <- "Loyack President Rider University 2083 Lawrenceville Road Lawrenceville, NJ 08648-3099 NOTIFICATION OF NON-COMPLIANCE PROBATION ACTION Dear Mr."
  assert_identical(
    derive_action_label_short("probation", text, "MSCHE"),
    "Non-Compliance Probation"
  )
})

run_test("derive_action_label_short Phase 4 hotfix: 'Staff acted on behalf' preamble stripped unconditionally", function() {
  # St. Francis College row (MSCHE): the preamble has "Staff acted on behalf
  # of the Commission to REQUEST" -- not "to acknowledge receipt of". The old
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

run_test("derive_action_label_short: HLC removed-from-notice file text becomes compact removal summary", function() {
  text <- paste0(
    "Summary of the Action: The Board determined that the Institution is no longer at risk of noncompliance ",
    "with the Criteria for Accreditation and removed the Institution from Notice. ",
    "The Institution meets Core Component 3.A with concerns."
  )
  assert_identical(
    derive_action_label_short("removed", text, "HLC"),
    "Accreditation Reaffirmed: Warning Removed"
  )
})

run_test("select_action_summary_source_text: MSCHE DAPIP notes override raw letter body", function() {
  raw <- paste0(
    "Burns: On behalf of the Middle States Commission on Higher Education, I am writing to inform you ",
    "that on June 27, 2024, the Commission acted as follows: To acknowledge receipt of the monitoring report."
  )
  assert_identical(
    .select_action_summary_source_text(
      raw,
      file_text_path = NA_character_,
      action_type = "warning",
      accreditor = "MSCHE",
      notes = "Probation or Equivalent or a More Severe Status: Warning"
    ),
    "Probation or Equivalent or a More Severe Status: Warning"
  )
})

run_test("select_action_summary_source_text: MSCHE correspondence wrappers prefer file text", function() {
  raw <- paste0(
    "Pullo: Notification of Non-Compliance Action On behalf of the Middle States Commission on Higher Education, ",
    "I am writing to inform you that on February 27, 2025, the Commission acted as follows: ",
    "To acknowledge receipt of the request by the institution to reconsider the adverse action to withdraw accreditation."
  )
  tmp <- tempfile("msche_file_text_", fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(
    paste0(
      "To acknowledge receipt of the request. ",
      "To require the institution to continue to show cause by September 2, 2025 ",
      "to demonstrate why its accreditation should not be withdrawn because of insufficient evidence ",
      "that the institution is in compliance with Standard VI (Planning, Resources, and Institutional Improvement)."
    ),
    tmp
  )
  assert_true(
    grepl(
      "continue to show cause by September 2, 2025",
      .select_action_summary_source_text(
        raw,
        file_text_path = tmp,
        action_type = "show_cause",
        accreditor = "MSCHE",
        notes = "public_action_code"
      ),
      ignore.case = TRUE
    )
  )
})

run_test("select_action_summary_source_text: HLC DAPIP notes override boilerplate file text", function() {
  raw <- paste0(
    "HLC Disclosure Obligations The Board action resulted in changes that will be reflected in the ",
    "Institution's Statement of Accreditation Status as well as the Institutional Status and Requirements Report."
  )
  assert_identical(
    .select_action_summary_source_text(
      raw,
      file_text_path = NA_character_,
      action_type = "removed",
      accreditor = "HLC",
      notes = "Accreditation Reaffirmed: Warning Removed"
    ),
    "Accreditation Reaffirmed: Warning Removed"
  )
})

run_test("select_action_summary_source_text: HLC sanction rows prefer richer cached letter text over generic notes", function() {
  raw <- "Summary of the Action: The Institution has been placed on Notice because it is at risk of being out of compliance with the Criteria for Accreditation."
  tmp <- tempfile("hlc_file_text_", fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(
    paste0(
      "July 8, 2025 BY CERTIFIED MAIL Brennan Randolph President Saint Mary-of-the-Woods College. ",
      "Summary of the Action: The Institution has been placed on Notice because it is at risk of being out of compliance with the Criteria for Accreditation. ",
      "The Institution meets Core Components 2.A, 2.C, 5.A, and 5.B with concerns. ",
      "The Institution does not meet Assumed Practice D.4. ",
      "Institutional Disclosure Obligation: HLC policy requires disclosure."
    ),
    tmp
  )
  selected <- .select_action_summary_source_text(
    raw,
    file_text_path = tmp,
    action_type = "warning",
    accreditor = "HLC",
    notes = "Probation or Equivalent or a More Severe Status: Warning | The Institution has been placed on Notice because it is at risk of being out of compliance with the Criteria for Accreditation."
  )
  assert_true(grepl("Core Components 2.A, 2.C, 5.A, and 5.B", selected, fixed = TRUE))
  assert_true(grepl("Assumed Practice D.4", selected, fixed = TRUE))
})

run_test("select_action_summary_source_kind: DAPIP file-text hint maps to pdf_body provenance", function() {
  assert_identical(
    .select_action_summary_source_kind(
      action_label_raw = "on probation for a period not to exceed two years because the Commission found that Hellenic College does not meet the standards on Institutional Resources.",
      file_text_path = NA_character_,
      action_type = "probation",
      accreditor = "NECHE",
      notes = "Probation or Equivalent or a More Severe Status: Probation",
      action_label_source_hint = "dapip_file_text"
    ),
    "pdf_body"
  )
})

run_test("get_accreditation_sanction_strength: notice and monitoring share compaction strength", function() {
  assert_identical(get_accreditation_sanction_strength("notice"), 1L)
  assert_identical(get_accreditation_sanction_strength("monitoring_or_notice"), 1L)
  assert_identical(get_accreditation_sanction_strength("warning"), 2L)
  assert_identical(get_accreditation_sanction_strength("show_cause"), 3L)
  assert_identical(get_accreditation_sanction_strength("probation"), 4L)
  assert_identical(get_accreditation_sanction_strength("adverse_action"), 5L)
})

run_test("source selection helper: MSCHE procedural wrapper stripping follows frontend procedural intent", function() {
  text <- paste0(
    "Staff acted on behalf of the Commission to acknowledge receipt of the monitoring report. ",
    "The next evaluation visit is scheduled for 2032-2033."
  )
  assert_identical(
    .strip_action_source_selection_wrapper(text, "MSCHE"),
    "The next evaluation visit is scheduled for 2032-2033."
  )
})

run_test("normalize_action_summary_text: repairs common mojibake punctuation from OCR/PDF text", function() {
  assert_identical(
    .normalize_action_summary_text("HLCâ€™s Criteria â€œfor Accreditationâ€ â€“ resources"),
    "HLC's Criteria \"for accreditation\" - resources"
  )
})

run_test("source selection helper: MSCHE standards-bearing rows earn a higher specificity score than procedural wrappers", function() {
  procedural_text <- paste0(
    "Staff acted on behalf of the Commission to acknowledge receipt of the monitoring report. ",
    "The next evaluation visit is scheduled for 2032-2033."
  )
  substantive_text <- paste0(
    "To place the institution on probation and note that the institution's accreditation is in jeopardy because of insufficient evidence ",
    "that the institution is currently in compliance with Standard V (Educational Effectiveness Assessment), Standard VI ",
    "(Planning, Resources, and Institutional Improvement), and Requirements of Affiliation 9, 11, and 12."
  )
  assert_true(
    get_action_summary_specificity_score(substantive_text, "MSCHE") >
      get_action_summary_specificity_score(procedural_text, "MSCHE"),
    "MSCHE standards-bearing row should outrank procedural wrapper text."
  )
})

run_test("source selection helper: WSCUC code labels stay low-specificity while letter excerpts score higher", function() {
  code_label <- "Probation or Equivalent or a More Severe Status: Warning"
  letter_excerpt <- paste0(
    "The Commission determined that Academy of Art was out of compliance with Standard 2, CFR 2.10, and Standard 3, CFR 3.4 specifically: ",
    "The institution has not developed realistic multi-year, scenario-based financial plans."
  )
  assert_identical(get_action_summary_specificity_score(code_label, "WSCUC"), 0L)
  assert_true(
    get_action_summary_specificity_score(letter_excerpt, "WSCUC") > 0L,
    "WSCUC letter excerpt with Standards/CFR references should score as substantive."
  )
})

run_test("normalize_accreditor_code: WASC Senior name maps to WSCUC", function() {
  assert_identical(
    normalize_accreditor_code("WASC Senior College and University Commission"),
    "WSCUC"
  )
})

run_test("source selection helper: thin WSCUC DAPIP code labels do not outrank scraper headings on tied specificity", function() {
  scraper_heading <- "Following an Accreditation Visit – issue Warning"
  dapip_code_label <- "Warning or Equivalent-Factors Affecting Academic Quality"
  assert_identical(
    get_action_summary_specificity_score(scraper_heading, "WSCUC"),
    get_action_summary_specificity_score(dapip_code_label, "WSCUC")
  )
})

run_test("select_action_summary_source_text: WSCUC boilerplate DAPIP raw text prefers cached letter text", function() {
  raw <- "At that meeting, the Commission acted to place Providence Christian College on Probation."
  letter_text <- paste0(
    "This letter serves as formal notification and official record of action taken concerning Providence Christian College. ",
    "At that meeting, the Commission acted to place Providence Christian College on Probation. ",
    "The Commission has determined that Providence Christian College is not in compliance with WSCUC Standards 3 and 4."
  )
  file_path <- tempfile(fileext = ".txt")
  writeLines(letter_text, file_path, useBytes = TRUE)
  on.exit(unlink(file_path), add = TRUE)

  assert_identical(
    .select_action_summary_source_text(
      raw,
      file_text_path = file_path,
      action_type = "probation",
      accreditor = "WSCUC"
    ),
    letter_text
  )
})

run_test("select_action_summary_source_text: WSCUC footer and procedural snippets prefer cached letter text", function() {
  hilo_raw <- "WSCUC is committed to an accreditation process that adds value to institutions while contributing to public accountability, and we thank you for your continued support of this process."
  hilo_file <- tempfile("wscuc_hilo_", fileext = ".txt")
  on.exit(unlink(hilo_file), add = TRUE)
  writeLines(
    paste0(
      "This letter serves as formal notification and official record of action taken concerning University of Hawaii at Hilo. ",
      "Actions 1. Receive the Special Visit team report 2. Remove Formal Notice of Concern."
    ),
    hilo_file
  )
  assert_true(
    grepl(
      "Remove Formal Notice of Concern",
      .select_action_summary_source_text(
        hilo_raw,
        file_text_path = hilo_file,
        action_type = "removed",
        accreditor = "WSCUC",
        notes = "Removal of Monitoring Status"
      ),
      ignore.case = TRUE
    )
  )

  ggu_raw <- "The Commission recognizes the significant improvements made while acknowledging that continued monitoring through a Notice of Concern is appropriate to ensure long-term institutional stability."
  ggu_file <- tempfile("wscuc_ggu_", fileext = ".txt")
  on.exit(unlink(ggu_file), add = TRUE)
  writeLines(
    paste0(
      "This letter serves as formal notification and official record of action taken concerning Golden Gate University. ",
      "At that meeting the Commission acted to continue Golden Gate University on Notice of Concern. ",
      "Standard at Risk of Non-Compliance and Requiring a Response GGU is at risk of non-compliance with Standard 3 (CFRs 3.4, 3.5)."
    ),
    ggu_file
  )
  assert_true(
    grepl(
      "Standard 3",
      .select_action_summary_source_text(
        ggu_raw,
        file_text_path = ggu_file,
        action_type = "notice",
        accreditor = "WSCUC",
        notes = "Heightened Monitoring or Focused Review"
      ),
      ignore.case = TRUE
    )
  )

  sdcc_raw <- "These actions were taken after reviewing SDCC's request for consideration of new evidence of compliance with WSCUC Standards pursuant to the institution's appeal of the withdrawal of its accreditation effective July 14, 2023."
  sdcc_file <- tempfile("wscuc_sdcc_", fileext = ".txt")
  on.exit(unlink(sdcc_file), add = TRUE)
  writeLines(
    paste0(
      "SDCC will remain on Show Cause. Actions 1. Receive the New Evidence Report 2. Continue the sanction of Show Cause. ",
      "Areas of Noncompliance The Commission determined that SDCC has not demonstrated compliance with Standard 3, specifically with CFR 3.4."
    ),
    sdcc_file
  )
  assert_true(
    grepl(
      "Continue the sanction of Show Cause",
      .select_action_summary_source_text(
        sdcc_raw,
        file_text_path = sdcc_file,
        action_type = "show_cause",
        accreditor = "WSCUC",
        notes = "Probation or Equivalent or a More Severe Status: Show Cause"
      ),
      ignore.case = TRUE
    )
  )
})

run_test("source selection helper: long procedural text does not beat shorter substantive text", function() {
  procedural_text <- paste0(
    "Staff acted on behalf of the Commission to acknowledge receipt of the monitoring report. ",
    "The next evaluation visit is scheduled for 2032-2033. ",
    "The institution remains responsible for all previously requested follow-up materials."
  )
  substantive_text <- "The Commission determined that Providence Christian College is not in compliance with WSCUC Standards 3 and 4."
  assert_true(
    get_action_summary_substantive_text_length(procedural_text, "MSCHE") >
      get_action_summary_substantive_text_length(substantive_text, "WSCUC"),
    "Procedural wrapper text should still be longer after stripping."
  )
  assert_true(
    get_action_summary_specificity_score(substantive_text, "WSCUC") >
      get_action_summary_specificity_score(procedural_text, "MSCHE"),
    "Shorter substantive text should outrank longer procedural text on specificity."
  )
})

run_test("derive_action_label_short: WSCUC Providence letter text yields standards-backed probation summary", function() {
  text <- paste0(
    "This letter serves as formal notification and official record of action taken concerning Providence Christian College. ",
    "At that meeting, the Commission acted to place Providence Christian College on Probation. ",
    "Non-Compliance with Standards: Deficiencies to be Addressed Providence Christian College is out of compliance with Standard 3 CFRs 3.4 and 3.7 because it lacks both a multi-year financial plan to guide activities and a strategic enrollment plan to increase enrollment, threatening ongoing fiscal sustainability. ",
    "Providence Christian College is out of compliance with Standard 4 CFRs 4.1-4.5 because it has not developed quality assurance processes including data collection, analysis, and dissemination, use of data in decision making, and strategic planning."
  )
  assert_identical(
    derive_action_label_short("probation", text, "WSCUC"),
    "Placed on Probation because it is out of compliance with Standards 3 and 4, CFRs 3.4, 3.7, 4.1, 4.2, 4.3, 4.4, and 4.5 on financial sustainability and quality assurance"
  )
})

run_test("derive_action_label_short: WSCUC Academy of Art letter text yields standards-backed warning summary", function() {
  text <- paste0(
    "This letter serves as formal notification and official record of action taken concerning Academy of Art University. ",
    "At that meeting the Commission removed the Notice of Concern and issued a Warning. ",
    "Areas of Noncompliance The Commission determined that Academy of Art was out of compliance with Standard 2, CFR 2.10, and Standard 3, CFR 3.4 specifically: ",
    "Standard 2, CFR 2.10: The institution demonstrates that students make reasonable progress toward and complete their degrees in a timely manner. ",
    "Standard 3, CFRs 3.4: Resource planning and development include realistic budgeting, enrollment management, and diversification of revenue sources. ",
    "The institution has not developed realistic multi-year, scenario-based financial plans."
  )
  assert_identical(
    derive_action_label_short("warning", text, "WSCUC"),
    "Removed Notice of Concern and issued a Warning because it is out of compliance with Standards 2 and 3, CFRs 2.10 and 3.4 on student completion and resource planning"
  )
})

run_test("derive_action_label_short: WSCUC San Diego Christian letter text yields standards-backed warning summary", function() {
  text <- paste0(
    "This letter serves as formal notification and official record of action taken concerning San Diego Christian College (SDCC). ",
    "At that meeting, the Commission acted to remove a Show Cause order and impose a Warning. ",
    "Non-Compliance with Standards: Deficiencies to be Addressed The Commission determined that SDCC has not demonstrated compliance with Standard 3, specifically with CFR 3.4: ",
    "The institution is financially stable and has unqualified independent financial audits and resources sufficient to ensure long-term viability. ",
    "Resource planning and development include realistic budgeting, enrollment management, and diversification of revenue sources. ",
    "SDCC needs to show evidence that it has met enrollment goals, has realistic plans to close budget deficits, and can ensure long-term fiscal viability."
  )
  assert_identical(
    derive_action_label_short("warning", text, "WSCUC"),
    "Removed Show Cause and issued a Warning because it has not demonstrated compliance with Standard 3, CFR 3.4 on financial sustainability and resource planning"
  )
})

run_test("derive_action_label_short: WSCUC CSU East Bay letter text yields notice summary with CFR detail", function() {
  text <- paste0(
    "This letter serves as formal notification and official record of action taken concerning California State University East Bay (CSUEB). ",
    "At that meeting the Commission decided to place CSUEB on Notice of Concern. ",
    "Standards at Risk of Non-Compliance and Requiring a Response Standard 3, CFR 3.4 Resource Planning and CFR 3.5 Fiscal Stability: ",
    "Develop, communicate, and implement budgetary plans in collaboration with stakeholders to ensure financial stability and long-term sustainability. ",
    "Develop a comprehensive strategic enrollment plan that includes multiple budget scenarios and options for financial sustainability."
  )
  assert_identical(
    derive_action_label_short("notice", text, "WSCUC"),
    "Issued a Notice of Concern over Standard 3, CFRs 3.4 and 3.5 on financial sustainability and resource planning"
  )
})

run_test("derive_action_label_short: WSCUC notice removal and continued-notice letters become compact summaries", function() {
  hilo_text <- paste0(
    "This letter serves as formal notification and official record of action taken concerning University of Hawaii at Hilo (UHH). ",
    "Actions 1. Receive the Special Visit team report 2. Remove Formal Notice of Concern 3. Schedule an Interim Report."
  )
  woodbury_removed_text <- paste0(
    "This letter serves as formal notification and official record of action taken concerning Woodbury University (WoodU). ",
    "Actions 1. Receive the Special Visit team report 2. Remove the Notice of Concern 3. Continue with previously scheduled reaffirmation review."
  )
  ggu_text <- paste0(
    "This letter serves as formal notification and official record of action taken concerning Golden Gate University (GGU). ",
    "At that meeting the Commission acted to continue Golden Gate University on Notice of Concern. ",
    "Standard at Risk of Non-Compliance and Requiring a Response GGU is at risk of non-compliance with Standard 3 (CFRs 3.4, 3.5). ",
    "The university faces concerning financial challenges: limited cash flow affecting operational flexibility, recurring operating deficits impacting financial stability, and revenue uncertainty creating vulnerability."
  )
  assert_identical(
    derive_action_label_short("removed", hilo_text, "WSCUC"),
    "Removed Notice of Concern"
  )
  assert_identical(
    derive_action_label_short("removed", woodbury_removed_text, "WSCUC"),
    "Removed Notice of Concern"
  )
  assert_identical(
    derive_action_label_short("notice", ggu_text, "WSCUC"),
    "Continued Notice of Concern because it is at risk of non-compliance with Standard 3, CFRs 3.4 and 3.5 on financial sustainability"
  )
})

run_test("derive_action_label_short: WSCUC Woodbury notice and SDCC continued show cause use standards-backed summaries", function() {
  woodbury_text <- paste0(
    "This letter serves as formal notification and official record of action taken concerning Woodbury University (WoodU). ",
    "Actions 1. Receive the Accreditation Visit team report 2. Reaffirm accreditation for a period of six years 3. Issue a Formal Notice of Concern. ",
    "Standard at Risk of Non-Compliance and Requiring a Response Woodbury University is in danger of being found out of compliance with Standard 3, CFRs 3.4 and 3.7. ",
    "The institution has experienced significant financial problems due to years of declining student enrollment that has contributed to operating expenses exceeding revenues. ",
    "In addition, significant changes have occurred in leadership and organizational structures."
  )
  sdcc_show_cause_text <- paste0(
    "This letter serves as formal notification and official record of action taken concerning San Diego Christian College (SDCC). ",
    "In November 2023 the Commission accepted SDCC's evidence of compliance with CFRs 2.10, 2.13, and 3.8, and found that SDCC remains out of compliance with CFR 3.4. ",
    "SDCC will remain on Show Cause. Actions 1. Receive the New Evidence Report 2. Continue the sanction of Show Cause. ",
    "Areas of Noncompliance The Commission determined that SDCC has not demonstrated compliance with Standard 3, specifically with CFR 3.4: ",
    "The institution is financially stable and has unqualified independent financial audits and resources sufficient to ensure long-term viability. ",
    "Resource planning and development include realistic budgeting, enrollment management, and diversification of revenue sources."
  )
  assert_identical(
    derive_action_label_short("notice", woodbury_text, "WSCUC"),
    "Issued a Notice of Concern over Standard 3, CFRs 3.4 and 3.7 on financial sustainability and leadership capacity"
  )
  assert_identical(
    derive_action_label_short("show_cause", sdcc_show_cause_text, "WSCUC"),
    "Continued Show Cause because it has not demonstrated compliance with Standard 3, CFR 3.4 on financial sustainability and resource planning"
  )
})

run_test("derive_action_label_short: WSCUC show-cause letters still summarize when OCR text is garbled", function() {
  sdcc_garbled_text <- paste0(
    "These actions were taken after reviewing SDCC's request for consideration of new evidence of compliance. ",
    "SDCC will remain on Show Cause. Actions 1. Receive the New Evidence Report 2. Continue the sanction of Show Cause. ",
    "Areas of Noncompliance The Commission determined that SDCC has not demonstrated compliance with Standard 3, specifically with CFR 3.4: ",
    "The institution is financially stable and has unqualified independent financial audits and resources sufficient to ensure long-term viability. ",
    "Resource planning and development include realistic budgeting, enrollment management, and diversification of revenue sources."
  )
  assert_identical(
    derive_action_label_short("show_cause", sdcc_garbled_text, "WSCUC"),
    "Continued Show Cause because it has not demonstrated compliance with Standard 3, CFR 3.4 on financial sustainability and resource planning"
  )
})

run_test("derive_action_label_short: WSCUC closure notes surface institutional closure", function() {
  assert_identical(
    derive_action_label_short(
      "adverse_action",
      "Loss of Accreditation or Preaccreditation: Voluntary Withdrawal",
      "WSCUC",
      "Loss of Accreditation or Preaccreditation: Voluntary Withdrawal | CC-ASU has officially closed its campus and is no longer accredited"
    ),
    "Institution closed and no longer accredited"
  )
})

run_test("extract_hlc_core_components: parses single-component interim report references from HLC letter text", function() {
  text <- paste0(
    "Summary of the Action: The Institution has been granted initial accreditation. ",
    "The Institution is required to submit an Interim Report regarding Core Component 4.B no later than October 15, 2025, ",
    "and an Interim Report regarding Core Component 5.B no later than October 15, 2025."
  )
  assert_identical(
    extract_hlc_core_components(text),
    c("4.B", "5.B")
  )
})

run_test("extract_hlc_core_components: parses multi-component findings from HLC summary text", function() {
  text <- paste0(
    "Summary of the Action: The Institution has been placed on Notice because it is at risk of being out of compliance with the Criteria for Accreditation. ",
    "The Institution meets Core Components 2.A, 2.C, 5.A, and 5.B with concerns. ",
    "The Institution does not meet Assumed Practice D.4."
  )
  assert_identical(
    extract_hlc_core_components(text),
    c("2.A", "2.C", "5.A", "5.B")
  )
})

run_test("extract_hlc_core_components: parses Criterion Five, Core Component 5.B rationale text", function() {
  text <- paste0(
    "Arkansas Baptist College (\"the Institution\") does not meet Criterion Five, Core Component 5.B, ",
    "\"the institution's resource base supports its educational offerings and its plans for maintaining and strengthening their quality in the future,\" ",
    "for the following reasons: The Institution's enrollment has declined from 990 in fall 2015 to approximately 467 in fall 2020."
  )
  assert_identical(
    extract_hlc_core_components(text),
    "5.B"
  )
})

run_test("extract_hlc_assumed_practices: parses Assumed Practice D.4 from HLC summary text", function() {
  text <- paste0(
    "Summary of the Action: The Institution has been placed on Notice because it is at risk of being out of compliance with the Criteria for Accreditation. ",
    "The Institution meets Core Components 2.A, 2.C, 5.A, and 5.B with concerns. ",
    "The Institution does not meet Assumed Practice D.4."
  )
  assert_identical(
    extract_hlc_assumed_practices(text),
    "D.4"
  )
})

run_test("extract_hlc_named_concern_phrases: parses production note-style HLC concern phrasing", function() {
  text <- paste0(
    "Probation or Equivalent or a More Severe Status: Probation | ",
    "HLC took this action because it determined that the institution does not meet HLC’s Criteria for Accreditation related to integrity: ",
    "ethical and responsible conduct and institutional effectiveness, resources and planning."
  )
  assert_identical(
    extract_hlc_named_concern_phrases(text),
    "integrity: ethical and responsible conduct and institutional effectiveness, resources and planning"
  )
})

run_test("extract_hlc_findings: returns component, assumed-practice, and named-concern bundles consistently", function() {
  text <- paste0(
    "Summary of the Action: The Institution has been placed on Notice because it is at risk of being out of compliance with the Criteria for Accreditation. ",
    "The Institution meets Core Components 2.A, 2.C, 5.A, and 5.B with concerns. ",
    "The Institution does not meet Assumed Practice D.4. ",
    "HLC took this action because it determined that the institution does not meet HLC’s Criteria for Accreditation related to integrity: ",
    "ethical and responsible conduct and institutional effectiveness, resources and planning."
  )
  findings <- extract_hlc_findings(text)
  assert_identical(findings$core_components, c("2.A", "2.C", "5.A", "5.B"))
  assert_identical(findings$assumed_practices, "D.4")
  assert_identical(
    findings$named_concerns,
    "integrity: ethical and responsible conduct and institutional effectiveness, resources and planning"
  )
})

run_test("derive_action_label_short: HLC DAPIP note-style warning retains concise reason", function() {
  text <- paste0(
    "Probation or Equivalent or a More Severe Status: Warning | ",
    "HLC took this action because it determined that the institution was at risk of being out of compliance with HLC requirements."
  )
  assert_identical(
    derive_action_label_short("warning", text, "HLC"),
    "Placed on Warning because the institution was at risk of being out of compliance with HLC requirements."
  )
})

run_test("derive_action_label_short: HLC Arkansas Baptist probation uses detailed findings from full letter text", function() {
  text <- paste0(
    "March 13, 2019 BY CERTIFIED MAIL Regina Favors, Interim President Arkansas Baptist College. ",
    "Summary of the Action: The Institution has been placed on Probation because it is out of compliance with the Criteria for Accreditation. ",
    "The Institution meets Core Component 5.C with concerns. The Institution does not meet Core Component 5.A. ",
    "The Institution is required to host a comprehensive evaluation no later than September 2020. ",
    "Board Rationale The Board based its action on the following findings made with regard to the Institution."
  )
  assert_identical(
    derive_action_label_short("probation", text, "HLC"),
    "Placed on Probation because it is out of compliance with Core Components 5.C and 5.A."
  )
})

run_test("derive_action_label_short: HLC Saint Mary-of-the-Woods notice names Core Components and Assumed Practice", function() {
  text <- paste0(
    "Probation or Equivalent or a More Severe Status: Warning | ",
    "The Institution has been placed on Notice because it is at risk of being out of compliance with the Criteria for Accreditation. ",
    "The Institution meets Core Components 2.A, 2.C, 5.A, and 5.B with concerns. ",
    "The Institution does not meet Assumed Practice D.4."
  )
  assert_identical(
    derive_action_label_short("warning", text, "HLC"),
    "Placed on Warning because it is at risk of being out of compliance with Core Components 2.A, 2.C, 5.A, and 5.B and Assumed Practice D.4."
  )
})

run_test("derive_action_label_short: HLC Ohio Christian full letter text resolves generic HLC criteria wording to Core Components", function() {
  text <- paste0(
    "March 4, 2020 BY CERTIFIED MAIL Dr. Jon Kulaga, President Ohio Christian University. ",
    "Summary of the Action: The Institution has been placed on Notice because it is at risk of being out of compliance with the Criteria for Accreditation. ",
    "The Institution meets Core Components 5.A and 5.C with Concerns with concerns and requires monitoring related to the Federal Compliance Requirements. ",
    "Board Rationale The Board based its action on the following findings made with regard to the Institution."
  )
  assert_identical(
    derive_action_label_short(
      "warning",
      text,
      "HLC",
      "Probation or Equivalent or a More Severe Status: Warning | HLC took this action because it determined that the institution is at risk of being out of compliance with HLC’s Criteria for Accreditation."
    ),
    "Placed on Warning because the institution is at risk of being out of compliance with Core Components 5.A and 5.C."
  )
})

run_test("derive_action_label_short: HLC Wittenberg probation names a single Core Component", function() {
  text <- paste0(
    "Probation or Equivalent or a More Severe Status: Probation | ",
    "The Institution has been placed on Probation because it is out of compliance with the Criteria for Accreditation. ",
    "The Institution does not meet Core Component 4.B."
  )
  assert_identical(
    derive_action_label_short("probation", text, "HLC"),
    "Placed on Probation because it is out of compliance with Core Component 4.B."
  )
})

run_test("derive_action_label_short: HLC Wheeling full letter text resolves generic HLC criteria wording to Core Components", function() {
  text <- paste0(
    "March 4, 2021 BY CERTIFIED MAIL Ginny R. Favede, President Wheeling University. ",
    "Summary of the Action: The Institution has been placed on Probation because it is out of compliance with the Criteria for Accreditation. ",
    "The Institution meets Core Components 2.C (sufficient board autonomy), 4.B (assessment of student learning), 4.C (persistence, retention and completion), and 5.D (institutional effectiveness) with concerns. ",
    "The Institution does not meet Core Components 5.A (resources) and 5.C (strategic planning). ",
    "Board Rationale The Board based its action on the following findings made with regard to the Institution."
  )
  assert_identical(
    derive_action_label_short(
      "probation",
      text,
      "HLC",
      "Probation or Equivalent or a More Severe Status: Probation | HLC took this action because it determined that the institution is out of compliance with HLC's Criteria for Accreditation."
    ),
    "Placed on Probation because the institution is out of compliance with Core Components 2.C, 4.B, 4.C, 5.D, 5.A, and 5.C."
  )
})

run_test("derive_action_label_short: HLC Wittenberg full letter text yields detailed probation summary", function() {
  text <- paste0(
    "November 11, 2025 BY CERTIFIED MAIL Dr. Christian Brady President Wittenberg University. ",
    "Summary of the Action: The Institution has been placed on Probation because it is out of compliance with the Criteria for Accreditation. ",
    "The Institution does not meet Core Component 4.B. The Institution is required to host a comprehensive evaluation for Probation no later than April 2027. ",
    "Board Rationale The Board based its action on the following findings made with regard to the Institution."
  )
  assert_identical(
    derive_action_label_short("probation", text, "HLC"),
    "Placed on Probation because it is out of compliance with Core Component 4.B."
  )
})

run_test("derive_action_label_short: HLC Wilberforce notice names multiple Core Components and Assumed Practices", function() {
  text <- paste0(
    "Probation or Equivalent or a More Severe Status: Warning | ",
    "The Institution has been placed on Notice because it is at risk of being out of compliance with the Criteria for Accreditation. ",
    "The Institution meets Core Components 3.C, 4.C, 5.B, and 5.C with concerns. ",
    "The Institution does not meet Assumed Practices D.3 and D.4."
  )
  assert_identical(
    derive_action_label_short("warning", text, "HLC"),
    "Placed on Warning because it is at risk of being out of compliance with Core Components 3.C, 4.C, 5.B, and 5.C and Assumed Practices D.3 and D.4."
  )
})

run_test("derive_action_label_short: HLC Wilberforce probation extension uses detailed findings from full letter text", function() {
  text <- paste0(
    "November 11, 2020 BY CERTIFIED MAIL Dr. Elfred Pinkard, President Wilberforce University. ",
    "Summary of the Action: The Board exercised its discretion to extend Probation beyond the maximum timeframe based on HLC's COVID-19 policy and because despite the Institution's progress, the Institution remains out of compliance with the Criteria for Accreditation. ",
    "The Institution meets Core Components 3.C, 4.C, and 5.C with concerns. The Institution does not meet Core Components 5.A and 5.D, and is out of conformity with Assumed Practices D.1 and D.2.1 ",
    "The Institution is required to host a focused visit no later than April 2021. ",
    "Institutional Disclosure Obligation: HLC policy2 requires that an Institution inform its constituencies."
  )
  assert_identical(
    derive_action_label_short("probation", text, "HLC"),
    "The Board exercised its discretion to extend Probation beyond the maximum timeframe based on HLC's COVID-19 policy and because despite the Institution's progress, the Institution remains out of compliance with Core Components 3.C, 4.C, 5.C, 5.A, and 5.D and Assumed Practices D.1 and D.2."
  )
})

run_test("derive_action_label_short: HLC Southwest Baptist probation preserves named concern phrasing", function() {
  text <- paste0(
    "Probation or Equivalent or a More Severe Status: Probation | ",
    "HLC took this action because it determined that the institution does not meet HLCâ€™s Criteria for Accreditation related to integrity: ",
    "ethical and responsible conduct and institutional effectiveness, resources and planning."
  )
  assert_identical(
    derive_action_label_short("probation", text, "HLC"),
    "Placed on Probation because the institution does not meet HLC's Criteria for Accreditation related to integrity: ethical and responsible conduct and institutional effectiveness, resources and planning."
  )
})

run_test("derive_action_label_short: HLC Harris-Stowe notice uses full-letter component detail", function() {
  text <- paste0(
    "November 15, 2022 BY CERTIFIED MAIL Dr. LaTonia Collins Smith, President Harris-Stowe State University. ",
    "Summary of the Action: The Institution has been placed on Notice because it is at risk of being out of compliance with the Criteria for Accreditation. ",
    "The Institution meets Core Components 2.A, 4.A, 4.B, 4.C, and 5.B with concerns. ",
    "The Institution is required to host a Notice Visit no later than April 2024. ",
    "Board Rationale The Board based its action on the following findings made with regard to the Institution."
  )
  assert_identical(
    derive_action_label_short("warning", text, "HLC"),
    "Placed on Warning because it is at risk of being out of compliance with Core Components 2.A, 4.A, 4.B, 4.C, and 5.B."
  )
})

run_test("derive_action_label_short: HLC removal notes outrank descriptive file text for warning removal", function() {
  text <- "Resources appear to be sufficient to support operations and deliver educational programs, but continued progress and improvement is needed."
  assert_identical(
    derive_action_label_short("removed", text, "HLC", "Accreditation Reaffirmed: Warning Removed"),
    "Accreditation Reaffirmed: Warning Removed"
  )
})

run_test("derive_action_label_short: HLC teach-out location summaries strip street addresses and ZIP codes", function() {
  text <- paste0(
    "Approved the institutionâ€™s teach-out plan for closing six additional locations: ",
    "Jacksonville, 7077 Bonneval Rd #114 , Jacksonville, FL 32216-4050 ",
    "Mesquite, 3737 Motley Drive, Mesquite, TX 75150 ",
    "JFTB Los Alamitos, 11206 Lexington Avenue, Suite 110, Los Alamitos, CA 90720 ",
    "NSB Kings Bay, 918 USS James Madison Rd, Kings Bay, GA 31547-2533 ",
    "Kansas City, 4240 Blue Ridge Blvd., Suite 400, Kansas City, MO 64133-1702 ",
    "Springfield, 3271 East Battlefield Road, Suite 250, Springfield, MO 65804"
  )
  assert_identical(
    derive_action_label_short("adverse_action", text, "HLC"),
    "Approved the institution's teach-out plan for closing six additional locations: Jacksonville, Mesquite, JFTB Los Alamitos, NSB Kings Bay, Kansas City, and Springfield"
  )
})

run_test("derive_action_label_short: HLC teach-out agreements keep partner summarization instead of location cleanup", function() {
  text <- paste0(
    "Approved the institutionÃ¢â‚¬â„¢s provisional plan to teach out students at the branch campus at Ann Arbor, 4090 Geddes Road, Ann Arbor, MI 48105, ",
    "including teach-out agreements with the following institutions: ",
    "Madonna University, Livonia, MI Lourdes University, Sylvania, OH Siena Heights University, Adrian, MI ",
    "Rochester Christian University, Rochester Hills, MI University of Detroit Mercy, Detroit, MI"
  )
  assert_identical(
    derive_action_label_short("adverse_action", text, "HLC"),
    "Approved teach-out agreements with Madonna University, Lourdes University, Siena Heights University, and others"
  )
})

run_test("derive_action_label_short: HLC single teach-out agreement trims provisional-plan boilerplate", function() {
  text <- "Approved the institution’s teach-out agreement with Westminster College in Fulton, Missouri, as an addition to the provisional plan approved by HLC’s Institutional Actions Council in April 2024. (Approved February 14, 2025)"
  assert_identical(
    derive_action_label_short("adverse_action", text, "HLC"),
    "Approved the institution’s teach-out agreement with Westminster College in Fulton, Missouri"
  )
})

run_test("derive_action_label_short: HLC plural teach-out agreements drop additions-to-provisional-plan boilerplate", function() {
  text <- "Approved the institution’s teach-out agreements with Warren Wilson College (North Carolina) and University of Wisconsin-Green Bay, as additions to the provisional plan approved by HLC’s Institutional Actions Council in March 2025. (Approved April 4, 2025)"
  assert_identical(
    derive_action_label_short("adverse_action", text, "HLC"),
    "Approved teach-out agreements with Warren Wilson College (North Carolina) and University of Wisconsin-Green Bay"
  )
})

run_test("derive_action_label_short: HLC teach-out plan for two generic locations omits plain city-only list", function() {
  text <- "Approved the institution’s teach-out plan for two additional locations: North Lauderdale, 955 Rock Island Road, North Lauderdale, FL 33068 Kendall, 9010 SW 137 Ave., Miami, FL 33176."
  assert_identical(
    derive_action_label_short("adverse_action", text, "HLC"),
    "Approved the institution’s teach-out plan for two additional locations:"
  )
})

run_test("derive_action_label_short: WSCUC Sonoma notice wording is normalized", function() {
  assert_identical(
    derive_action_label_short("notice", "Defer Action on Reaffirmation of accreditation/Issue a Notice of Concern", "WSCUC"),
    "Deferred action on reaffirmation of accreditation and issued a Notice of Concern"
  )
})

run_test("derive_action_label_short: WSCUC Sonoma scraper heading can use institution-specific DAPIP detail", function() {
  assert_identical(
    derive_action_label_short(
      "notice",
      "Defer Action on Reaffirmation of accreditation/Issue a Notice of Concern",
      "WSCUC",
      "Sonoma State University"
    ),
    "Issued a Notice of Concern over Standards 1 and 3, CFRs 1, 1.7, 3.11, and 3.4 on financial sustainability and shared governance"
  )
})

run_test("derive_action_label_short: HLC teach-out location summaries drop PO boxes as address text", function() {
  text <- "Approved the teach-out of an additional location: Pawnee Nation College, 891 Little Dee Drive, PO Box 390, Pawnee, OK 74058."
  assert_identical(
    derive_action_label_short("adverse_action", text, "HLC"),
    "Approved the teach-out of an additional location: Pawnee Nation College"
  )
})

run_test("derive_action_label_short: MSCHE DAPIP code labels become compact sanction summaries", function() {
  assert_identical(
    derive_action_label_short("warning", "Probation or Equivalent or a More Severe Status: Warning", "MSCHE"),
    "Placed on Warning"
  )
  assert_identical(
    derive_action_label_short("removed", "Accreditation Reaffirmed: Warning Removed", "MSCHE"),
    "Accreditation Reaffirmed: Warning Removed"
  )
})

run_test("derive_action_label_short: MSCHE HCM2 monitoring and teach-out request becomes compact summary", function() {
  text <- paste0(
    "Staff acted on behalf of the Commission to request that the monitoring report, due March 3, 2025, also provide evidence of ",
    "(1) compliance with any and all conditions established by the USDE Office of Federal Student Aid (FSA) relating to the institution's Heightened Cash Monitoring (HCM2) status ",
    "(Standard VI: Planning, Resources, and Institutional Improvement) and (2) an annual independent audit confirming financial viability with evidence of follow-up on any cited concerns. ",
    "To require that the institution complete and submit for approval, by March 3, 2025, a teach-out plan and teach-out agreements because the Secretary of Education has placed the institution on Heightened Cash Monitoring (HCM2)."
  )
  assert_identical(
    derive_action_label_short("adverse_action", text, "MSCHE"),
    "Required teach-out plan and financial viability monitoring after Heightened Cash Monitoring (HCM2)"
  )
})

run_test("derive_action_label_short: SACSCOC referral-report letters become compact summaries", function() {
  text <- paste0(
    "The Southern Association of Colleges and Schools Commission on Colleges (SACSCOC) Committee on Fifth-Year Interim Reports reviewed the institutionâ€™s compliance ",
    "with select standards of the Principles of Accreditation outlined in the SACSCOC Fifth-Year Interim Report. ",
    "Based only on those reviewed standards, the institution is requested to submit a Referral Report due April 1, 2026, addressing the following referenced standards of the Principles: ",
    "Standard 6.2.c (Program coordination) Standard 10.9 (Cooperative academic arrangements) ",
    "Standard 13.6 (Federal and state responsibilities) Standard 14.1 (Publication of accreditation status)."
  )
  assert_identical(
    derive_action_label_short("notice", text, "SACSCOC"),
    "Requested to submit a Referral Report documenting compliance with accreditation standards"
  )
})

run_test("derive_action_label_short: SACSCOC monitoring-report letters become compact summaries", function() {
  text <- paste0(
    "The institution is requested to submit a First Monitoring Report due April 1, 2026, addressing the following referenced standard of the Principles of accreditation: ",
    "Standard 13.3 (Financial responsibility)."
  )
  assert_identical(
    derive_action_label_short("notice", text, "SACSCOC"),
    "Requested to Submit a Monitoring Report on Financial Responsibility"
  )
})

run_test("derive_action_label_short: SACSCOC raw requested-monitoring rows use submit wording", function() {
  text <- "Requested a Monitoring Report in twelve (12) months to provide required oversight and resolution of compliance issues."
  assert_identical(
    derive_action_label_short("notice", text, "SACSCOC"),
    "Requested to Submit a Monitoring Report in twelve (12) months to provide required oversight and resolution of compliance issues."
  )
})

run_test("derive_action_label_short: SACSCOC letter-body monitoring rows use submit wording", function() {
  text <- paste0(
    "Sanford: The following action regarding your institution was taken by the Board of Trustees of the Southern Association of Colleges and Schools Commission on Colleges (SACSCOC) during its meeting held on December 7, 2025: ",
    "The SACSCOC Board of Trustees requested a Monitoring Report in twelve (12) months to provide required oversight and resolution of compliance issues."
  )
  assert_identical(
    derive_action_label_short("notice", text, "SACSCOC"),
    "Requested to Submit a Monitoring Report in twelve (12) months to provide required oversight and resolution of compliance issues."
  )
})

run_test("derive_action_label_short: SACSCOC warning letters keep full spaced standard references", function() {
  text <- paste0(
    "Qubein: The following action regarding your institution was taken by the Board of Trustees of the Southern Association of Colleges and Schools Commission on Colleges (SACSCOC) during its meeting held on June 15, 2023: ",
    "The SACSCOC Board of Trustees reviewed the institution's Referral Report from the submission of a Fifth-Year Interim Report in June 2022 and recommended that the institution be placed on Warning for twelve months for failure to comply with Core Requirement 12. 1 (Student support services), Standard 8. 2. a (Student outcomes: educational programs), and Standard 14. 1 (Publication of accreditation status) of the Principles of Accreditation. ",
    "A Special Committee was not authorized to visit the institution."
  )
  assert_identical(
    derive_action_label_short("notice", text, "SACSCOC"),
    "Recommended that the institution be placed on Warning for twelve months for failure to comply with Core Requirement 12.1 (Student support services), Standard 8.2.a (Student outcomes: educational programs), and Standard 14.1 (Publication of accreditation status) of the Principles of accreditation."
  )
})

run_test("derive_action_label_short: SACSCOC follow-up warning letters drop the report lead-in", function() {
  text <- paste0(
    "White: The following action regarding your institution was taken by the Board of Trustees of the Southern Association of Colleges and Schools Commission on Colleges (SACSCOC) during its meeting held on December 4, 2022: ",
    "The SACSCOC Board of Trustees reviewed the institution's Follow-Up Report requested in June 2018 that focused on finances and placed the institution on Warning for six (6) months for failure to comply with Core Requirement 13. 1 (Financial resources), Core Requirement 13. 2 (Financial documents), and Standard 13. 3 (Financial responsibility) of the Principles of Accreditation."
  )
  assert_identical(
    derive_action_label_short("notice", text, "SACSCOC"),
    "Placed on Warning for six (6) months for failure to comply with Core Requirement 13.1 (financial resources), Core Requirement 13.2 (financial documents), and Standard 13.3 (financial responsibility) of the Principles of accreditation."
  )
})

run_test("derive_action_label_short: SACSCOC committee removal letters summarize the non-compliance reasons", function() {
  text <- paste0(
    "At its meeting on December 7, 2024, the Committee on Compliance and Reports, Group C, of the Southern Association of Colleges and Schools Commission on Colleges (SACSCOC) Board of Trustees reviewed Saint Augustine’s University’s Fifth Monitoring Report, financial statements, the Report of a Special Committee, and the institution’s response to that Report. ",
    "The Committee recommended the removal of Saint Augustine’s University from membership for failure to comply with Core Requirement 4.1 (Governing board characteristics), Core Requirement 13.1 (Financial resources), Core Requirement 13.2 (Financial documents), Standard 13.3 (Financial responsibility), Standard 13.4 (Control of finances), Standard 13.5 (Control of sponsored research/external funds) and Standard 13.6 (Federal and state responsibilities) of the Principles of Accreditation. ",
    "The recommendation of the Committee on Compliance and Reports was approved by the SACSCOC Board of Trustees at its meeting on December 8, 2024."
  )
  assert_identical(
    derive_action_label_short("adverse_action", text, "SACSCOC"),
    "Removed from membership for failure to comply with standards concerning governance, financial resources, financial documents, financial responsibility, control of finances and sponsored research/external funds"
  )
})

run_test("derive_action_label_short: HLC notice retains reason from DAPIP summary", function() {
  text <- "Summary of the Action: The Institution has been placed on Notice because it is at risk of being out of compliance with the Criteria for Accreditation."
  assert_identical(
    derive_action_label_short("warning", text, "HLC"),
    "Placed on Warning because it is at risk of being out of compliance with the Criteria for Accreditation."
  )
})

run_test("derive_action_label_short: MSCHE show cause retains Standard VI reason", function() {
  text <- paste0(
    "To require the institution to show cause, by September 2, 2025, to demonstrate why its accreditation should not be withdrawn ",
    "because of insufficient evidence that the institution is in compliance with Standard VI (Planning, Resources, and Institutional Improvement)."
  )
  assert_identical(
    derive_action_label_short("show_cause", text, "MSCHE"),
    "Required to Show Cause because of insufficient evidence of compliance with Standard VI (Planning, Resources, and Institutional Improvement)"
  )
})

run_test("derive_action_label_short: SACSCOC disclosure statement retains injunction detail", function() {
  text <- paste0(
    "Disclosure Statement Regarding the Status of Saint Augustine's University. ",
    "On August 18, 2025, SACSCOC received litigation documents confirming that SAU was granted a temporary restraining order and preliminary injunction. ",
    "The order requires that SACSCOC reinstate the institution's accreditation status (accredited on Probation) pending the outcome of litigation."
  )
  assert_identical(
    derive_action_label_short("show_cause", text, "SACSCOC"),
    "Accreditation on Probation was reinstated pending litigation after a temporary restraining order and preliminary injunction."
  )
})

run_test("derive_action_label_short: SACSCOC removal letters become compact removal summaries", function() {
  text <- paste0(
    "January 6, 2026 Brother Chris Englert Interim President Christian Brothers University 650 East Parkway South Memphis, TN 38104 Dear Brother Englert: ",
    "The following action regarding your institution was taken by the Board of Trustees of the Southern Association of Colleges and Schools Commission on Colleges during its meeting held on December 7, 2025: ",
    "The SACSCOC Board of Trustees removed the institution from Probation for Good Cause."
  )
  assert_identical(
    derive_action_label_short("removed", text, "SACSCOC"),
    "Removed from Probation for Good Cause"
  )
})

run_test("derive_action_label_short: SACSCOC off-campus review letters drop the header boilerplate", function() {
  text <- paste0(
    "Khator: The following action regarding your institution was taken by the Board of Trustees of the Southern Association of Colleges and Schools Commission on Colleges (SACSCOC) during its meeting held on June 16, 2022: ",
    "The SACSCOC Board of Trustees continued accreditation following the review of an off- campus instructional site located at Dalian Maritime University in Liaoning Province, China (approved June 2021)."
  )
  assert_identical(
    derive_action_label_short("other", text, "SACSCOC"),
    "Continued accreditation following review of the off-campus instructional site at Dalian Maritime University in Liaoning Province, China."
  )
})

run_test("derive_action_label_short: SACSCOC denied-program letters put the action first", function() {
  text <- paste0(
    "Allen: The following actions regarding your institution were taken by the Board of Trustees of the Southern Association of Colleges and Schools Commission on Colleges (SACSCOC) during its meeting held on December 8, 2024: ",
    "The SACSCOC Board of Trustees denied approval of a Master of Education in Clinical Mental Health Counseling and a Master of Education in Professional School Counseling ",
    "(intended implementation: August 2025) because the institution did not provide an acceptable plan and supporting documentation to ensure that it has the capability to comply with the following standards of the Principles of Accreditation as they relate to the substantive change: ",
    "Core Requirement 13.1 (Financial resources) and Standard 13.3 (Financial responsibility)."
  )
  assert_identical(
    derive_action_label_short("other", text, "SACSCOC"),
    "Denied approval of a Master of Education in Clinical Mental Health Counseling and a Master of Education in Professional School Counseling (intended implementation: August 2025) because the institution did not provide an acceptable plan and supporting documentation to show compliance with financial resources and financial responsibility."
  )
})

run_test("is_sacscoc_public_table_row_to_drop: drops routine publication and programmatic rows", function() {
  assert_true(is_sacscoc_public_table_row_to_drop(
    "notice",
    "Requested Referral Report on Public information, Policies for awarding credit, and, Publication of accreditation status",
    "Standard 14.1 (Publication of accreditation status) The institution accurately represents its accreditation status."
  ))
  assert_true(is_sacscoc_public_table_row_to_drop(
    "other",
    "Denied approval of a Master of Education in Clinical Mental Health Counseling and a Master of Education in Professional School Counseling (intended implementation: August 2025) because the institution did not provide an acceptable plan and supporting documentation to show compliance with financial resources and financial responsibility.",
    "Denied approval of a Master of Education in Clinical Mental Health Counseling."
  ))
  assert_true(is_sacscoc_public_table_row_to_drop(
    "other",
    "Failureto documentcompliancewiththeCore Requirementatthetimeofits next review will result in the institution being placed on a sanction.",
    "Failureto documentcompliancewiththeCore Requirementatthetimeofits next review will result in the institution being placed on a sanction."
  ))
  assert_true(is_sacscoc_public_table_row_to_drop(
    "notice",
    "Failure to document compliance with the Core Requirement at the time of the board's review will result in your institution being placed on a sanction.",
    "Failure to document compliance with the Core Requirement at the time of the board's review will result in your institution being placed on a sanction."
  ))
  assert_true(is_sacscoc_public_table_row_to_drop(
    "notice",
    "This reviewwas conducted following the institution's failure to notify SACSCOCfollowing the late closure of the Harris Methodist Hospital.",
    "This reviewwas conducted following the institution's failure to notify SACSCOCfollowing the late closure of the Harris Methodist Hospital."
  ))
  assert_true(!is_sacscoc_public_table_row_to_drop(
    "warning",
    "Placed on Warning for failure to comply with financial resources and financial responsibility",
    "The institution was placed on Warning for failure to comply with Core Requirement 13.1 (Financial resources) and Standard 13.3 (Financial responsibility)."
  ))
})

run_test("derive_action_label_short: SACSCOC garbled OCR warning letters still surface the sanction", function() {
  text <- paste0(
    "Maurer: Thefollowing action regardingyourinstitution wastaken bythe BoardofTrustees ofthe Southern Association ofColleges and Schools Commission on Colleges (SACSCOC) durina its meeting held on December 4, 2020: ",
    "The Board ofTrustees denied reaffirmation, continued accreditation, and placed the institution on Warning for 12 months for failure to comply with Core Requirement (CR) 13."
  )
  assert_identical(
    derive_action_label_short("warning", text, "SACSCOC"),
    "Denied reaffirmation, continued accreditation, and placed the institution on Warning for 12 months"
  )
})

run_test("derive_action_label_short: SACSCOC DAPIP code labels become compact sanction summaries", function() {
  assert_identical(
    derive_action_label_short("warning", "Probation or Equivalent or a More Severe Status: Warning", "SACSCOC"),
    "Placed on Warning"
  )
  assert_identical(
    derive_action_label_short("probation", "Probation or Equivalent or a More Severe Status: Probation", "SACSCOC"),
    "Placed on Probation"
  )
})

run_test("derive_action_label_short: WSCUC labels drop special-visit boilerplate and use sentence case", function() {
  assert_identical(
    derive_action_label_short("notice", "Following a Special Visit - Issue a Formal Notice of Concern", "WSCUC"),
    "Issue a formal notice of concern"
  )
  assert_identical(
    derive_action_label_short("warning", "Following a Special Visit - Remove the Warning, Issue a Formal Notice of Concern, Reaffirm Accreditation for 6 Years", "WSCUC"),
    "Remove the warning, issue a formal notice of concern, reaffirm accreditation for 6 years"
  )
  assert_identical(
    derive_action_label_short("probation", "Placed on Probation because it is out of compliance with the Criteria for Accreditation.", "WSCUC"),
    "Placed on probation because it is out of compliance with the criteria for accreditation."
  )
})

run_test("derive_action_label_short: NWCCU warning and removals become compact summaries", function() {
  warning_text <- paste0(
    "Accreditation Accept the Report, Issue a Sanction of Warning on Recommendation 2: Spring 2020 Evaluation of Institutional Effectiveness. ",
    "The Commission finds that the following Recommendations are areas where Warner Pacific University is out of compliance with the NWCCU Standards for Accreditation."
  )
  warning_removed_text <- paste0(
    "Accreditation Accept the Report, Remove Sanction of Warning Status of Previous Recommendations Addressed in this Evaluation. ",
    "The Commission recommends that Cornish College of the Arts regularly monitor institutional finances."
  )
  show_cause_removed_text <- paste0(
    "Accreditation Reaffirm Accreditation: Remove the Sanction of Show Cause, Accept the Report. ",
    "The Commission recommends that Bastyr University engage in effective financial planning."
  )

  assert_identical(
    derive_action_label_short("warning", warning_text, "NWCCU"),
    "Placed on Warning"
  )
  assert_identical(
    derive_action_label_short("removed", warning_removed_text, "NWCCU"),
    "Accreditation Reaffirmed: Warning Removed"
  )
  assert_identical(
    derive_action_label_short("removed", show_cause_removed_text, "NWCCU"),
    "Removed from Show Cause"
  )
})

run_test("derive_action_label_short: HLC voluntary resignation rows become compact withdrawal summaries", function() {
  text <- paste0(
    "Public Disclosure: University of Arizona Voluntary Resignation of accreditation Effective: August 1, 2023 ",
    "University of Arizona in Tucson, Arizona, voluntarily resigned its accreditation with the Higher Learning Commission, effective August 1, 2023."
  )
  assert_identical(
    derive_action_label_short("adverse_action", text, "HLC"),
    "Voluntarily Surrendered Accreditation"
  )
})

run_test("derive_action_label_short: NECHE probation removal becomes compact removal summary", function() {
  text <- paste0(
    "At its meeting on March 5, 2021, the New England Commission of Higher Education (NECHE) voted to remove Hellenic College, Inc. ",
    "from probation for failure to meet the standards on Institutional Resources and Planning and Evaluation, and to remove the Notations ",
    "with respect to the standards on Organization and Governance and The Academic Program."
  )
  assert_identical(
    derive_action_label_short("removed", text, "NECHE"),
    "Accreditation Reaffirmed: Probation Removed"
  )
})

run_test("derive_action_label_short: NECHE show-cause press release can surface concern summary", function() {
  text <- paste0(
    "JOINT PRESS RELEASE New England Commission of Higher Education and Hellenic College, Inc. ",
    "At its meeting on May 31, 2019, the New England Commission of Higher Education (NECHE) voted to ask Hellenic College, Inc. ",
    "to show cause why it should not be placed on probation or have its accreditation withdrawn because the Commission had reason to believe that ",
    "Hellenic College, Inc. is not meeting the Commissionâ€™s standards on Planning and Evaluation and Institutional Resources."
  )
  assert_identical(
    derive_action_label_short("notice", text, "NECHE"),
    "Concerns Hellenic College may no longer meet Standard 2 (Planning and Evaluation) and Standard 7 (Institutional Resources)."
  )
})

run_test("derive_action_label_short: HLC teach-out additions retain counterpart institutions", function() {
  text <- paste0(
    "Approved the institutionâ€™s teach-out agreements with the following institutions as additions to the provisional plan approved by HLCâ€™s Institutional Actions Council in April 2026. ",
    "Illinois College, Jacksonville, IL Missouri Baptist University, St. Louis, MO ",
    "Washington University in St. Louis, St. Louis, MO"
  )
  assert_identical(
    derive_action_label_short("adverse_action", text, "HLC"),
    "Approved teach-out agreements with Illinois College, Missouri Baptist University, and Washington University in St. Louis"
  )
})

run_test("derive_action_label_short: HLC resolved concerns row becomes compact summary", function() {
  text <- paste0(
    "The Institution now meets without concerns Criterion Three, Core Component 3.A, “the institution’s degree programs are appropriate to higher education,” for the following reason: ",
    "As a result of program assessments, a review of mission alignment, and a minimal cost benefit, the Graduate Admissions Policy Committee recommended to the faculty that the Doctor of Counseling program, the sole program in question at the time of sanction, be discontinued."
  )
  assert_identical(
    derive_action_label_short("removed", text, "HLC"),
    "Concerns about Criterion Three, Core Component 3.A were resolved after discontinuing the Doctor of Counseling program."
  )
})
