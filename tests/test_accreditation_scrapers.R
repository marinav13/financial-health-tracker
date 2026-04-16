source(file.path(root, "scripts", "shared", "accreditation_scrapers.R"))

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

run_test("parse_msche warns when page has content but no expected H3 headings", function() {
  cache_dir <- tempfile("msche_jsshell_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  # Simulate a JavaScript-rendered shell: non-empty HTML, but none of the
  # expected H3 action headings are present.
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
      if (grepl("JavaScript-rendered", conditionMessage(w), fixed = TRUE)) {
        warned <<- TRUE
        invokeRestart("muffleWarning")
      }
    }
  )

  assert_true(warned, "Expected a JS-rendering warning from parse_msche but none was raised.")
})
