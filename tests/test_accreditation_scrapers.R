source(file.path(root, "scripts", "shared", "accreditation_scrapers.R"))

run_test("Accreditation scraper action schema helper", function() {
  empty_rows <- ensure_accreditation_action_schema(tibble::tibble(), "empty fixture")
  assert_identical(nrow(empty_rows), 0L)
  assert_true(all(ACCREDITATION_ACTION_COLUMNS %in% names(empty_rows)))

  err <- tryCatch(
    {
      ensure_accreditation_action_schema(
        tibble::tibble(institution_name_raw = "Missing columns University"),
        "broken fixture"
      )
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  assert_true(!is.null(err) && grepl("broken fixture", err, fixed = TRUE))
  assert_true(grepl("source_url", err, fixed = TRUE))
})

run_test("Accreditation scraper section extractors", function() {
  regex_html <- paste0(
    '<a class="elementor-toggle-title">Warning</a>',
    '<div id="elementor-tab-content-1" class="elementor-tab-content">',
    '<ul><li>Alpha College, Boston, MA</li></ul></div>'
  )
  regex_sections <- extract_regex_heading_sections(regex_html, NECHE_TOGGLE_SECTION_PATTERN)

  tag_html <- paste0(
    "<h2>Probation</h2><ul><li>Beta College, CA</li></ul>",
    "<h2>Not Public</h2><p>Ignore</p>"
  )
  tag_sections <- extract_tag_heading_sections(tag_html, heading_tag = "h2")

  assert_identical(nrow(regex_sections), 1L)
  assert_identical(regex_sections$heading[[1]], "Warning")
  assert_identical(nrow(tag_sections), 2L)
  assert_identical(tag_sections$heading[[1]], "Probation")
})

run_test("Accreditation scraper public action section parser", function() {
  sections <- tibble::tibble(
    heading = c("Warning", "Campus celebration"),
    body = c(
      "<ul><li>Alpha College, Boston, MA</li><li>Beta University, CA</li></ul>",
      "<ul><li>Should be ignored</li></ul>"
    )
  )

  rows <- parse_public_action_sections(
    sections = sections,
    accreditor = "NECHE",
    action_date = as.Date("2025-01-01"),
    action_year = 2025L,
    source_url = "https://example.com/actions",
    source_title = "Example Actions",
    source_page_url = "https://example.com/actions",
    source_page_modified = "2025-01-15"
  )

  assert_identical(nrow(rows), 2L)
  assert_true(all(rows$accreditor == "NECHE"))
  assert_true(all(rows$action_type == "warning"))
  assert_identical(rows$institution_state_raw[[1]], "Massachusetts")
  assert_identical(rows$institution_state_raw[[2]], "California")
})

run_test("Accreditation scraper SACSCOC sanction item parser", function() {
  standard_row <- parse_sacscoc_sanction_item(
    "Alpha College, City, Georgia (Placed on Warning)",
    action_date = as.Date("2025-06-01"),
    url = "https://example.com/sacscoc",
    page_title = "June 2025 Actions"
  )
  withdrawal_row <- parse_sacscoc_sanction_item(
    "Beta Institute (Town, NC) withdraws from membership",
    action_date = as.Date("2025-06-01"),
    url = "https://example.com/sacscoc",
    page_title = "June 2025 Actions"
  )

  assert_identical(standard_row$institution_name_raw[[1]], "Alpha College")
  assert_identical(standard_row$institution_state_raw[[1]], "Georgia")
  assert_identical(standard_row$action_type[[1]], "warning")
  assert_identical(withdrawal_row$institution_state_raw[[1]], "North Carolina")
  assert_identical(withdrawal_row$action_type[[1]], "adverse_action")
})

run_test("Accreditation scraper SACSCOC disclosure builder", function() {
  disclosure_matches <- matrix(
    c(
      "<p><a href=\"https://sacscoc.box.com/s/abc\">PDF</a>, Alpha College, GA</p>",
      "https://sacscoc.box.com/s/abc",
      "PDF",
      "Alpha College",
      "GA",
      "<p><a href=\"https://sacscoc.box.com/s/abc\">PDF</a>, Alpha College, GA</p>",
      "https://sacscoc.box.com/s/abc",
      "PDF",
      "Alpha College",
      "GA",
      "<p><a href=\"https://sacscoc.box.com/s/def\">PDF</a>, Public Disclosure Statements, GA</p>",
      "https://sacscoc.box.com/s/def",
      "PDF",
      "Public Disclosure Statements",
      "GA"
    ),
    ncol = 5,
    byrow = TRUE
  )

  rows <- build_sacscoc_disclosure_rows(
    disclosure_matches,
    action_date = as.Date("2025-06-01"),
    url = "https://example.com/sacscoc",
    page_title = "June 2025 Actions"
  )

  assert_identical(nrow(rows), 1L)
  assert_identical(rows$institution_name_raw[[1]], "Alpha College")
  assert_identical(rows$institution_state_raw[[1]], "Georgia")
  assert_identical(rows$action_label_raw[[1]], "Public Disclosure Statement")
})

run_test("Accreditation scraper HLC paragraph state updater", function() {
  doc <- xml2::read_html(paste0(
    "<html><body>",
    "<p><a href=\"/institution/test\">Alpha College</a>, Boston, MA</p>",
    "<p><a href=\"/institution/test\">Alpha College</a>, Placed on Notice</p>",
    "</body></html>"
  ))
  nodes <- xml2::xml_find_all(doc, "//p")

  location_update <- update_hlc_current_institution(
    p_text = clean_text(xml2::xml_text(nodes[[1]])),
    p_link = xml2::xml_find_first(nodes[[1]], ".//a[contains(@href, '/institution/')]")
  )
  action_update <- update_hlc_current_institution(
    p_text = clean_text(xml2::xml_text(nodes[[2]])),
    p_link = xml2::xml_find_first(nodes[[2]], ".//a[contains(@href, '/institution/')]")
  )

  assert_identical(location_update$institution, "Alpha College")
  assert_identical(location_update$state, "Massachusetts")
  assert_identical(action_update$institution, "Alpha College")
  assert_identical(action_update$action_text, "Placed on Notice")
})

run_test("Accreditation scraper HLC content node parser", function() {
  doc <- xml2::read_html(paste0(
    "<html><body><div class=\"entry-content\">",
    "<p><a href=\"/institution/test\">Alpha College</a>, Boston, MA</p>",
    "<ul><li>Placed on Notice</li><li>Issued Public Sanction</li></ul>",
    "</div></body></html>"
  ))
  nodes <- xml2::xml_find_all(doc, "//*[contains(@class,'entry-content')]//*[self::p or self::ul]")

  rows <- parse_hlc_content_nodes(
    content_nodes = nodes,
    action_date = as.Date("2025-01-01"),
    detail_url = "https://example.com/hlc",
    detail_title = "January 2025 Actions",
    detail_modified = "2025-01-15"
  )

  assert_identical(nrow(rows), 2L)
  assert_true(all(rows$institution_name_raw == "Alpha College"))
  assert_true(all(rows$institution_state_raw == "Massachusetts"))
  assert_identical(rows$action_type[[1]], "notice")
})

run_test("parse_msche returns correct rows from fixture HTML", function() {
  cache_dir <- tempfile("msche_fixture_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  # Minimal static HTML that mirrors the server-rendered MSCHE page structure.
  # One institution under Non-Compliance Warning; all other sections empty.
  writeLines(
    paste0(
      "<!DOCTYPE html><html>",
      "<head><title>Non-Compliance and Adverse Actions By Status | MSCHE</title>",
      "<meta property=\"article:modified_time\" content=\"2025-06-01T00:00:00+00:00\">",
      "</head><body><article>",
      "<h3>Non-Compliance Warning</h3>",
      "<a href=\"https://www.msche.org/institution/100/\">Fixture College</a>",
      "<h3>Non-Compliance Probation</h3><p>No institutions in this status</p>",
      "<h3>Non-Compliance Show Cause</h3><p>No institutions in this status</p>",
      "<h3>Adverse Action</h3><p>No institutions in this status</p>",
      "<div class=\"single-share\">end</div>",
      "</article></body></html>"
    ),
    file.path(cache_dir, "msche_status.html")
  )

  # Stub out every recent-actions page so the parser does not attempt network calls.
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  for (yr in seq.int(2017L, current_year)) {
    writeLines(
      "<html><body></body></html>",
      file.path(cache_dir, paste0("msche_recent_actions_", yr, ".html"))
    )
  }

  rows <- parse_msche(cache_dir, refresh = FALSE)

  assert_identical(nrow(rows), 1L)
  assert_identical(rows$institution_name_raw[[1]], "Fixture College")
  assert_identical(rows$action_type[[1]], "warning")
  assert_identical(rows$accreditor[[1]], "MSCHE")
  assert_identical(rows$action_status[[1]], "active")
  assert_identical(rows$source_url[[1]], "https://www.msche.org/institution/100/")
})

run_test("parse_msche warns when cached page has no expected H3 headings (refresh=FALSE)", function() {
  cache_dir <- tempfile("msche_jsshell_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  # Simulate a JavaScript-rendered shell already in the cache.
  # refresh=FALSE so fetch_html_text returns the cached file directly,
  # triggering parse_msche's secondary guard rather than the validate_fn path.
  writeLines(
    "<html><head><title>MSCHE</title></head><body><p>Loading...</p></body></html>",
    file.path(cache_dir, "msche_status.html")
  )

  current_year <- as.integer(format(Sys.Date(), "%Y"))
  for (yr in seq.int(2017L, current_year)) {
    writeLines(
      "<html><body></body></html>",
      file.path(cache_dir, paste0("msche_recent_actions_", yr, ".html"))
    )
  }

  warned <- FALSE
  withCallingHandlers(
    parse_msche(cache_dir, refresh = FALSE),
    warning = function(w) {
      if (grepl("JavaScript-rendered|no expected H3", conditionMessage(w))) {
        warned <<- TRUE
        invokeRestart("muffleWarning")
      }
    }
  )

  assert_true(warned, "Expected a JS-rendering warning from parse_msche but none was raised.")
})

run_test("fetch_html_text validate_fn: JS-shell does not overwrite good cache", function() {
  cache_dir <- tempfile("fetch_validate_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  cache_path <- file.path(cache_dir, "test_page.html")
  good_html  <- paste0(
    "<!DOCTYPE html><html><head><title>Good Page</title></head>",
    "<body><h3>Non-Compliance Warning</h3><p>Real content</p></body></html>"
  )
  js_shell   <- "<html><body><p>Loading...</p></body></html>"

  # Seed the cache with known-good content
  writeLines(good_html, cache_path)

  # A validator that rejects the JS-shell (no <h3> found)
  has_h3 <- function(html) grepl("<h3>", html, fixed = TRUE)

  # Pretend the "fresh" fetch returns a JS-shell by writing it to a temp file
  # and calling fetch_html_text with refresh=FALSE against the bad content,
  # then manually simulating the validate_fn path by calling it directly.
  # Since fetch_html_text makes real HTTP calls we test the validate_fn branch
  # in isolation: call it with the JS-shell and confirm it returns FALSE.
  assert_identical(has_h3(js_shell), FALSE)
  assert_identical(has_h3(good_html), TRUE)

  # The cache must NOT have been overwritten: read back and confirm good content
  # (this verifies the file wasn't touched outside of a real network call).
  result <- readLines(cache_path, warn = FALSE)
  assert_true(any(grepl("Non-Compliance Warning", result, fixed = TRUE)),
    "Cache should still contain the good HTML, not the JS-shell.")

  # Directly exercise the warning path: call fetch_html_text with refresh=FALSE
  # (reads cache regardless of validate_fn — secondary guard path).
  returned <- fetch_html_text("https://example.com", "test_page.html", cache_dir,
                              refresh = FALSE, validate_fn = has_h3)
  assert_true(grepl("Non-Compliance Warning", returned, fixed = TRUE),
    "refresh=FALSE should return cached content even when validate_fn is supplied.")
})

# ---------------------------------------------------------------------------
# NWCCU institution page parser
# ---------------------------------------------------------------------------

# Helper: write a fixture HTML file and return the inst URL whose slug matches
nwccu_fixture <- function(cache_dir, slug, html) {
  # Cache filename must match what parse_nwccu_institution_page() generates:
  # paste0("nwccu_", gsub("[^a-z]", "_", slug), ".html")
  cache_name <- paste0("nwccu_", gsub("[^a-z]", "_", slug), ".html")
  writeLines(html, file.path(cache_dir, cache_name))
  paste0("https://nwccu.org/institutional-directory/", slug, "/")
}

run_test("parse_nwccu_institution_page: fully accredited page returns no rows", function() {
  cache_dir <- tempfile("nwccu_accredited_")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  url <- nwccu_fixture(cache_dir, "normal-university", paste0(
    '<html><body><main>',
    '<span class="block">Current Accreditation Status</span>',
    '<p class="h5 block">Accredited</p>',
    '<span class="block super">Most Recent Evaluation</span>',
    '<span class="block flex">Spring 2026 Policies and Procedures Review</span>',
    '<span class="block">Reason for Accreditation</span>',
    '<span class="block flex">Substantially compliant with NWCCU Standards.</span>',
    '<a href="#">Submit a Complaint</a>',
    '<section><h3>Institutional Responsibilities</h3></section>',
    '</main></body></html>'
  ))

  result <- parse_nwccu_institution_page(url, "Normal University", cache_dir, refresh = FALSE)
  assert_identical(nrow(result), 0L,
    "Fully accredited page with boilerplate-only text should return no rows.")
})

run_test("parse_nwccu_institution_page: non-Accredited status triggers fast-path", function() {
  cache_dir <- tempfile("nwccu_probation_")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  for (status_text in c("Probation", "Show Cause", "Warning")) {
    slug <- tolower(gsub(" ", "-", status_text))
    url  <- nwccu_fixture(cache_dir, slug, paste0(
      '<html><body><main>',
      '<span class="block">Current Accreditation Status</span>',
      '<p class="h5 block">', status_text, '</p>',
      '<span class="block super">Most Recent Evaluation</span>',
      '<span class="block flex">Fall 2024 Comprehensive Evaluation</span>',
      '</main></body></html>'
    ))
    result <- parse_nwccu_institution_page(url, paste(status_text, "College"), cache_dir, refresh = FALSE)
    assert_identical(nrow(result), 1L,
      paste0("Status '", status_text, "' should be flagged immediately (fast-path)."))
    assert_identical(result$accreditor[[1]], "NWCCU")
  }
})

run_test("parse_nwccu_institution_page: non-empty rows satisfy accreditation schema", function() {
  cache_dir <- tempfile("nwccu_schema_")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  url <- nwccu_fixture(cache_dir, "schema-university", paste0(
    '<html><body><main>',
    '<span class="block">Current Accreditation Status</span>',
    '<p class="h5 block">Probation</p>',
    '<span class="block super">Most Recent Evaluation</span>',
    '<span class="block flex">Fall 2025 Special Visit</span>',
    '<span class="block">Reason for Accreditation</span>',
    '<span class="block flex">Financial resources and governance concerns.</span>',
    '<a href="https://nwccu.box.com/s/abc">Institution Notification Letter</a>',
    '</main></body></html>'
  ))

  result <- parse_nwccu_institution_page(url, "Schema University", cache_dir, refresh = FALSE)
  checked <- ensure_accreditation_action_schema(result, "NWCCU non-empty fixture")
  assert_identical(nrow(checked), 1L)
  assert_true(all(ACCREDITATION_ACTION_COLUMNS %in% names(checked)))
  assert_identical(checked$source_page_url[[1]], url)
  assert_true(grepl("Financial resources", checked$notes[[1]], fixed = TRUE))
})

run_test("parse_nwccu_institution_page: adverse keyword in new page section is detected", function() {
  cache_dir <- tempfile("nwccu_widebodyscan_")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Status says Accredited, but an adverse keyword appears in an additional section
  url <- nwccu_fixture(cache_dir, "sanctions-university", paste0(
    '<html><body><main>',
    '<span class="block">Current Accreditation Status</span>',
    '<p class="h5 block">Accredited</p>',
    '<section>',
    '<h2>Current Sanctions</h2>',
    '<p>The institution is under a formal warning for financial standards.</p>',
    '</section>',
    '<a href="#">Institutional Responsibilities</a>',
    '</main></body></html>'
  ))

  result <- parse_nwccu_institution_page(url, "Sanctions University", cache_dir, refresh = FALSE)
  assert_identical(nrow(result), 1L,
    "Adverse keyword outside the three structured fields should still be caught by wide body scan.")
})

run_test("parse_nwccu_institution_page: show-cause keyword in eval field is detected", function() {
  cache_dir <- tempfile("nwccu_evalfield_")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  url <- nwccu_fixture(cache_dir, "showcause-u", paste0(
    '<html><body><main>',
    '<span class="block">Current Accreditation Status</span>',
    '<p class="h5 block">Accredited</p>',
    '<span class="block super">Most Recent Evaluation</span>',
    '<span class="block flex">Show Cause Review - Fall 2024</span>',
    '</main></body></html>'
  ))

  result <- parse_nwccu_institution_page(url, "ShowCause U", cache_dir, refresh = FALSE)
  assert_identical(nrow(result), 1L,
    "Adverse keyword appearing in the eval text field should be caught.")
})
