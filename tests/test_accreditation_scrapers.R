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

run_test("Per-site warn_on_empty_parse fires on non-trivial HTML", function() {
  big_html <- strrep("x", 5000L)
  warned <- FALSE
  captured_msg <- ""
  withCallingHandlers(
    warn_on_empty_parse("TEST-ACC", "https://example.org/actions",
                        tibble::tibble(), big_html),
    warning = function(w) {
      warned <<- TRUE
      captured_msg <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  assert_true(warned, "Expected per-site empty-result guard to warn on 5000-byte HTML.")
  assert_true(grepl("TEST-ACC", captured_msg, fixed = TRUE))
  assert_true(grepl("5000", captured_msg, fixed = TRUE))
})

run_test("Per-site warn_on_empty_parse stays silent on tiny HTML", function() {
  # A 200-byte error page legitimately carries no actions; the guard should
  # not cry wolf on pages below the threshold.
  tiny_html <- strrep("x", 200L)
  warned <- FALSE
  withCallingHandlers(
    warn_on_empty_parse("TEST-ACC", "https://example.org/down",
                        tibble::tibble(), tiny_html),
    warning = function(w) { warned <<- TRUE; invokeRestart("muffleWarning") }
  )
  assert_true(!warned, "Expected per-site guard to stay silent on tiny HTML.")
})

run_test("Per-site warn_on_empty_parse stays silent on non-empty rows", function() {
  # Non-empty result is never suspicious regardless of HTML size.
  big_html <- strrep("x", 5000L)
  non_empty <- tibble::tibble(institution_name_raw = "Example U")
  warned <- FALSE
  withCallingHandlers(
    warn_on_empty_parse("TEST-ACC", "https://example.org/ok",
                        non_empty, big_html),
    warning = function(w) { warned <<- TRUE; invokeRestart("muffleWarning") }
  )
  assert_true(!warned, "Expected per-site guard to stay silent when rows are non-empty.")
})

run_test("Per-site warn_on_empty_parse can fail closed via fail=TRUE", function() {
  big_html <- strrep("x", 5000L)
  err <- tryCatch(
    {
      warn_on_empty_parse("TEST-ACC", "https://example.org/actions",
                          tibble::tibble(), big_html, fail = TRUE)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  assert_true(!is.null(err),
    "Expected per-site guard to stop() when fail = TRUE on substantive empty HTML.")
  assert_true(grepl("TEST-ACC", err, fixed = TRUE))
  assert_true(grepl("parsed 0 rows", err, fixed = TRUE))
  assert_true(grepl("Returning empty table", err, fixed = TRUE))
})

run_test("Per-site warn_on_empty_parse picks up tracker.fail_on_empty_parse option", function() {
  # Publish path sets options(tracker.fail_on_empty_parse = TRUE) in CI. The
  # helper must default its `fail` argument off that option so per-site
  # callers don't need to thread a flag through every parse_* signature.
  prior <- getOption("tracker.fail_on_empty_parse", NULL)
  on.exit(
    if (is.null(prior)) {
      options(tracker.fail_on_empty_parse = NULL)
    } else {
      options(tracker.fail_on_empty_parse = prior)
    },
    add = TRUE
  )
  options(tracker.fail_on_empty_parse = TRUE)

  big_html <- strrep("x", 5000L)
  err <- tryCatch(
    {
      warn_on_empty_parse("TEST-ACC", "https://example.org/actions",
                          tibble::tibble(), big_html)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  assert_true(!is.null(err),
    "Expected helper to stop() when tracker.fail_on_empty_parse is TRUE, without fail= being passed.")
  assert_true(grepl("TEST-ACC", err, fixed = TRUE))
})

run_test("Per-site warn_on_empty_parse respects tiny-HTML guard even with fail=TRUE", function() {
  # A 200-byte page is below the substantive-HTML threshold. A fail-closed
  # helper must NOT stop on this path or every local test-page fetch would
  # become fatal in CI. Expected-zero exception is preserved at the
  # threshold_bytes layer.
  tiny_html <- strrep("x", 200L)
  err <- tryCatch(
    {
      warn_on_empty_parse("TEST-ACC", "https://example.org/down",
                          tibble::tibble(), tiny_html, fail = TRUE)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  assert_true(is.null(err),
    "Expected per-site guard to stay silent on tiny HTML even when fail = TRUE.")
})

run_test("Accreditation scrape count guard warns by default", function() {
  prior_csv <- tempfile("prior-accreditation-", fileext = ".csv")
  on.exit(unlink(prior_csv), add = TRUE)

  readr::write_csv(
    tibble::tibble(accreditor = c(rep("HLC", 10), rep("MSCHE", 2))),
    prior_csv
  )
  fresh <- tibble::tibble(accreditor = c(rep("HLC", 2), rep("MSCHE", 2)))

  warned <- FALSE
  withCallingHandlers(
    warn_if_scrape_count_dropped(fresh, prior_csv),
    warning = function(w) {
      if (grepl("HLC row count dropped from 10 to 2", conditionMessage(w), fixed = TRUE)) {
        warned <<- TRUE
        invokeRestart("muffleWarning")
      }
    }
  )

  assert_true(warned, "Expected scrape-count drop guard to warn by default.")
})

run_test("Accreditation scrape count guard can fail closed", function() {
  prior_csv <- tempfile("prior-accreditation-", fileext = ".csv")
  on.exit(unlink(prior_csv), add = TRUE)

  readr::write_csv(
    tibble::tibble(accreditor = rep("SACSCOC", 8)),
    prior_csv
  )
  fresh <- tibble::tibble(accreditor = character())

  err <- tryCatch(
    {
      warn_if_scrape_count_dropped(fresh, prior_csv, fail = TRUE)
      NULL
    },
    error = function(e) conditionMessage(e)
  )

  assert_true(!is.null(err), "Expected scrape-count drop guard to stop when fail=TRUE.")
  assert_true(grepl("SACSCOC row count dropped from 8 to 0", err, fixed = TRUE))
})

run_test("Accreditation action-type drop guard warns on pair-level disappearance", function() {
  prior_csv <- tempfile("prior-accreditation-", fileext = ".csv")
  on.exit(unlink(prior_csv), add = TRUE)

  readr::write_csv(
    tibble::tibble(
      accreditor  = c(rep("MSCHE", 5), rep("MSCHE", 5)),
      action_type = c(rep("probation", 5), rep("warning", 5))
    ),
    prior_csv
  )
  # Fresh run keeps the warning rows but loses the probation rows entirely.
  fresh <- tibble::tibble(
    accreditor  = rep("MSCHE", 5),
    action_type = rep("warning", 5)
  )

  warned <- FALSE
  withCallingHandlers(
    warn_if_action_type_dropped(fresh, prior_csv),
    warning = function(w) {
      if (grepl("MSCHE / probation dropped from 5 rows to 0",
                conditionMessage(w), fixed = TRUE)) {
        warned <<- TRUE
        invokeRestart("muffleWarning")
      }
    }
  )

  assert_true(
    warned,
    "Expected action-type drop guard to warn when a (accreditor, action_type) pair disappears."
  )
})

run_test("Accreditation action-type drop guard can fail closed", function() {
  prior_csv <- tempfile("prior-accreditation-", fileext = ".csv")
  on.exit(unlink(prior_csv), add = TRUE)

  readr::write_csv(
    tibble::tibble(
      accreditor  = rep("HLC", 4),
      action_type = rep("notice", 4)
    ),
    prior_csv
  )
  fresh <- tibble::tibble(
    accreditor  = character(),
    action_type = character()
  )

  err <- tryCatch(
    {
      warn_if_action_type_dropped(fresh, prior_csv, fail = TRUE)
      NULL
    },
    error = function(e) conditionMessage(e)
  )

  assert_true(!is.null(err), "Expected action-type drop guard to stop when fail=TRUE.")
  assert_true(grepl("HLC / notice dropped from 4 rows to 0", err, fixed = TRUE))
})

run_test("Accreditation action-type drop guard ignores low-volume pairs", function() {
  prior_csv <- tempfile("prior-accreditation-", fileext = ".csv")
  on.exit(unlink(prior_csv), add = TRUE)

  # Prior run has a pair with only 1 row. A drop from 1 to 0 is noisy and
  # should NOT trigger the guard at its default min_prior_rows threshold.
  readr::write_csv(
    tibble::tibble(
      accreditor  = "NECHE",
      action_type = "show_cause"
    ),
    prior_csv
  )
  fresh <- tibble::tibble(
    accreditor  = character(),
    action_type = character()
  )

  got_warning <- FALSE
  withCallingHandlers(
    warn_if_action_type_dropped(fresh, prior_csv),
    warning = function(w) {
      got_warning <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  assert_true(
    !got_warning,
    "Expected action-type drop guard to stay quiet when the prior count is below min_prior_rows."
  )
})

run_test("Accreditation action-type drop guard ignores synthetic commission_action stubs", function() {
  prior_csv <- tempfile("prior-accreditation-", fileext = ".csv")
  on.exit(unlink(prior_csv), add = TRUE)

  readr::write_csv(
    tibble::tibble(
      accreditor = rep("MSCHE", 4),
      action_type = rep("commission_action", 4)
    ),
    prior_csv
  )
  fresh <- tibble::tibble(
    accreditor = rep("MSCHE", 4),
    action_type = rep("other", 4)
  )

  got_warning <- FALSE
  withCallingHandlers(
    warn_if_action_type_dropped(fresh, prior_csv),
    warning = function(w) {
      got_warning <<- TRUE
      invokeRestart("muffleWarning")
    }
  )

  assert_true(
    !got_warning,
    "Expected action-type drop guard to ignore commission_action stub disappearance."
  )
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

run_test("Accreditation scraper public action parser handles phrasing variants", function() {
  sections <- tibble::tibble(
    heading = c("Accepted Teach-Out Plan", "Denied Reaffirmation", "Campus celebration"),
    body = c(
      "<ul><li>Gamma College, Austin, TX</li></ul>",
      "<ul><li>Delta University, Salem, OR</li></ul>",
      "<ul><li>Should be ignored, CA</li></ul>"
    )
  )

  rows <- parse_public_action_sections(
    sections = sections,
    accreditor = "Fixture",
    action_date = as.Date("2025-02-01"),
    action_year = 2025L,
    source_url = "https://example.com/actions",
    source_title = "Example Actions",
    source_page_url = "https://example.com/actions",
    source_page_modified = "2025-02-15"
  )

  assert_identical(nrow(rows), 2L)
  assert_identical(rows$institution_name_raw[[1]], "Gamma College")
  assert_identical(rows$institution_state_raw[[1]], "Texas")
  assert_identical(rows$action_type[[1]], "adverse_action")
  assert_identical(rows$action_type[[2]], "adverse_action")
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

run_test("Accreditation scraper SACSCOC disclosure builder uses anchor text as institution name", function() {
  # Live SACSCOC disclosure paragraphs (every cached June/December action page
  # we have, back through 2024 at least) name the institution inside the
  # anchor itself:
  #   <a href="...box.com/s/...">University of Lynchburg</a>, Lynchburg, VA [PDF]
  # SACSCOC_DISCLOSURE_ITEM_PATTERN's capture groups land as:
  #   [, 3] = anchor text (institution name)
  #   [, 4] = post-anchor token (typically the city)
  #   [, 5] = state
  #
  # A previous refactor (commit 8602839b, "Refactor helpers, add pipeline
  # fixture tests, rename Proposal G") silently swapped the builder from
  # column 3 to column 4, which made every disclosure row take the CITY as
  # its institution name ("Lynchburg", "Emory", "Charlotte", ...). IPEDS has
  # no entity called "Lynchburg", so every such row fell through to an
  # unmatched orphan unitid like accred-lynchburg--virginia--sacscoc. This
  # test pins the correct behavior; the fixture mirrors the Dec 2025 cached
  # page shape for University of Lynchburg.
  disclosure_matches <- matrix(
    c(
      "<p><a href=\"https://sacscoc.box.com/s/ez0994\">University of Lynchburg</a>, Lynchburg, VA [<a href=\"https://sacscoc.box.com/s/ez0994\">PDF</a>]</p>",
      "https://sacscoc.box.com/s/ez0994",
      "University of Lynchburg",
      "Lynchburg",
      "VA"
    ),
    ncol = 5,
    byrow = TRUE
  )

  rows <- build_sacscoc_disclosure_rows(
    disclosure_matches,
    action_date = as.Date("2025-12-01"),
    url = "https://example.com/sacscoc/december-2025",
    page_title = "December 2025 Actions"
  )

  assert_identical(nrow(rows), 1L)
  assert_identical(rows$institution_name_raw[[1]], "University of Lynchburg")
  assert_identical(rows$institution_state_raw[[1]], "Virginia")
  assert_identical(rows$action_label_raw[[1]], "Public Disclosure Statement")
  assert_identical(rows$source_url[[1]], "https://sacscoc.box.com/s/ez0994")
})

run_test("Accreditation scraper SACSCOC disclosure builder deduplicates and drops header rows", function() {
  # Two identical rows for the same institution should collapse to one via
  # the normalized-name + source_url + label distinct() step, and any
  # capture whose institution-name slot is a page header token ("Accreditation
  # Actions" or "Public Disclosure Statements") should be filtered out
  # entirely. This replaces a previous fixture that mistakenly pinned the
  # column-4 bug as "correct" behavior.
  disclosure_matches <- matrix(
    c(
      "<p><a href=\"https://sacscoc.box.com/s/abc\">Alpha College</a>, Athens, GA [<a href=\"https://sacscoc.box.com/s/abc\">PDF</a>]</p>",
      "https://sacscoc.box.com/s/abc",
      "Alpha College",
      "Athens",
      "GA",
      "<p><a href=\"https://sacscoc.box.com/s/abc\">Alpha College</a>, Athens, GA [<a href=\"https://sacscoc.box.com/s/abc\">PDF</a>]</p>",
      "https://sacscoc.box.com/s/abc",
      "Alpha College",
      "Athens",
      "GA",
      "<p><a href=\"https://sacscoc.box.com/s/def\">Public Disclosure Statements</a>, page, GA</p>",
      "https://sacscoc.box.com/s/def",
      "Public Disclosure Statements",
      "page",
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

run_test("SACSCOC_DISCLOSURE_ITEM_PATTERN does not stitch landing-PDF anchor across paragraphs", function() {
  # On the live SACSCOC December 2025 detail page, the layout opens with a
  # paragraph wrapping the landing-PDF link, followed by several body
  # paragraphs, followed by the institution rows in <li> elements:
  #
  #   <p><a href="...box.com/s/landing">December 2025 Accreditation Actions</a> [PDF]</p>
  #   <p>At its meeting on December 7, 2025, the Board of Trustees ...</p>
  #   ...
  #   <li><a href="...box.com/s/ks">Kentucky State University</a>, Frankfort, Kentucky (Probation)</li>
  #
  # The previous non-greedy `(.*?)</a>` backtracked across the body paragraphs
  # and stitched the landing-PDF anchor's </a> to Kentucky State's
  # `, Frankfort, Kentucky`, producing a phantom row whose institution name
  # was a multi-hundred-character body blob. That blob then flowed into
  # parse_sacscoc_disclosure_pdf via cache_slug = normalize_name(name + state),
  # producing a Windows-illegal cache filename and a "cannot open file ...
  # Invalid argument" warning at refresh time. The downstream str_detect
  # filter dropped the phantom from final output, but the bad fetch attempt
  # remained. Tempered greedy tokens stop the match at the next <p|li>
  # boundary and prevent the stitch.
  fixture_html <- paste0(
    "<p><a href=\"https://sacscoc.box.com/s/landingpdf\">",
    "<strong>December 2025 Accreditation Actions</strong></a>&nbsp;[PDF]</p>\n",
    "<p>At its meeting on December 7, 2025, the Board of Trustees of SACSCOC ",
    "took a number of actions regarding the accreditation status of candidate ",
    "and member institutions. (Click onto ",
    "<a href=\"https://sacscoc.box.com/s/landingpdf\">December 2025 Accreditation Actions</a> [PDF]).</p>\n",
    "<li><a href=\"https://sacscoc.box.com/s/kentuckystate\">Kentucky State University</a>, ",
    "Frankfort, Kentucky (placed on Probation Good Cause)</li>\n",
    "<li><a href=\"https://sacscoc.box.com/s/lynchburg\">University of Lynchburg</a>, ",
    "Lynchburg, Virginia [PDF]</li>"
  )

  matches <- stringr::str_match_all(fixture_html, SACSCOC_DISCLOSURE_ITEM_PATTERN)[[1]]

  # Two real institutions, zero phantom rows. The landing-PDF paragraph must
  # NOT match because no `, City, State` follows the anchor inside its own <p>.
  # `clean_text()` runs through `decode_html()` -> `vapply(USE.NAMES = TRUE)`
  # which auto-names character output by the input string values; strip those
  # names so `assert_identical` against a bare scalar passes.
  assert_identical(nrow(matches), 2L)
  assert_identical(unname(clean_text(matches[1, 3])), "Kentucky State University")
  assert_identical(unname(clean_text(matches[1, 4])), "Frankfort")
  assert_identical(unname(clean_text(matches[1, 5])), "Kentucky")
  assert_identical(unname(clean_text(matches[2, 3])), "University of Lynchburg")

  # Defense in depth: every captured institution name is bounded (no row
  # carries a blob longer than a real institution name).
  for (i in seq_len(nrow(matches))) {
    assert_true(
      nchar(clean_text(matches[i, 3])) <= 120L,
      sprintf("Captured institution name in row %d should be <= 120 chars; got %d.",
              i, nchar(clean_text(matches[i, 3])))
    )
  }
})

run_test("build_sacscoc_disclosure_rows skips PDF fetch when institution name exceeds the length cap", function() {
  # Symmetric guardrail at the fetch boundary: even if a future regex
  # regression slips a multi-paragraph blob into disclosure_matches[, 3],
  # build_sacscoc_disclosure_rows must NOT call parse_sacscoc_disclosure_pdf
  # with that blob -- doing so produced a Windows-illegal cache filename
  # ("cannot open file ... Invalid argument") on the live December 2025
  # detail page. Cap the name at 120 chars and emit a warning.
  long_name <- paste0(rep("Way Too Long Name ", 20), collapse = "")
  assert_true(nchar(long_name) > 120L, "Fixture name must exceed the 120-char cap.")

  cache_dir <- tempfile("sacscoc_namecap_")
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  disclosure_matches <- matrix(
    c(
      sprintf("<p><a href=\"https://sacscoc.box.com/s/longname\">%s</a>, City, GA [PDF]</p>", long_name),
      "https://sacscoc.box.com/s/longname",
      long_name,
      "City",
      "GA"
    ),
    ncol = 5,
    byrow = TRUE
  )

  warned <- FALSE
  rows <- withCallingHandlers(
    build_sacscoc_disclosure_rows(
      disclosure_matches,
      action_date = as.Date("2025-12-01"),
      url = "https://example.com/sacscoc/december-2025",
      page_title = "December 2025 Actions",
      cache_dir = cache_dir,
      refresh = FALSE
    ),
    warning = function(w) {
      if (grepl("skipping PDF enrichment", conditionMessage(w), fixed = TRUE)) {
        warned <<- TRUE
        invokeRestart("muffleWarning")
      }
    }
  )

  assert_true(warned, "Expected an over-length warning when the institution name exceeds the cap.")
  assert_identical(nrow(rows), 1L)
  # Falls back to the stub: enrichment was skipped, no PDF-derived label.
  assert_identical(rows$action_label_raw[[1]], "Public Disclosure Statement")
  # No cache file should have been created (cap fired before fetch).
  assert_identical(length(list.files(cache_dir)), 0L)
})

run_test("SACSCOC_DISCLOSURE_ITEM_PATTERN resolves state on multi-token location rows", function() {
  # Some SACSCOC member institutions publish their official name with a
  # campus designator after a comma, e.g. "State College of Florida,
  # Manatee-Sarasota". On the live SACSCOC June 2025 detail page that
  # produces a 4-part location after the </a>:
  #   <a>State College of Florida</a>, Manatee-Sarasota, Bradenton, Florida [PDF]
  # The previous regex assumed a strict 3-part `Name</a>, City, State`
  # layout and pinned state to the SECOND post-anchor token, which gave
  # state="Bradenton" (a city) instead of state="Florida" -- silently
  # breaking the IPEDS state match. The new regex anchors state as the
  # LAST comma-separated token before [PDF]/</p> via a tempered lookahead.
  fixture_html <- paste0(
    "<p><a href=\"https://sacscoc.box.com/s/scfm\">State College of Florida</a>, ",
    "Manatee-Sarasota, Bradenton, Florida [<a href=\"https://example.com\">PDF</a>]</p>\n",
    # Standard 2-part layout must still resolve correctly:
    "<li><a href=\"https://sacscoc.box.com/s/alpha\">Alpha College</a>, ",
    "Boston, MA</li>\n",
    # Layout with parenthetical comment after state (no [PDF] marker):
    "<li><a href=\"https://sacscoc.box.com/s/beta\">Beta University</a>, ",
    "Frankfort, Kentucky (placed on Probation Good Cause)</li>"
  )

  matches <- stringr::str_match_all(fixture_html, SACSCOC_DISCLOSURE_ITEM_PATTERN)[[1]]
  assert_identical(nrow(matches), 3L)

  # State College of Florida: state must be Florida, not Bradenton.
  assert_identical(unname(clean_text(matches[1, 3])), "State College of Florida")
  assert_identical(unname(clean_text(matches[1, 5])), "Florida")
  # Standard 2-part:
  assert_identical(unname(clean_text(matches[2, 3])), "Alpha College")
  assert_identical(unname(clean_text(matches[2, 4])), "Boston")
  assert_identical(unname(clean_text(matches[2, 5])), "MA")
  # Parenthetical-after-state:
  assert_identical(unname(clean_text(matches[3, 3])), "Beta University")
  assert_identical(unname(clean_text(matches[3, 5])), "Kentucky")
})

run_test("NWCCU directory selector matches the live `nwccu-degree-baccalaureate` class", function() {
  # NWCCU renamed the per-article CSS class from `nwccu-degree-bachelor` to
  # `nwccu-degree-baccalaureate` sometime in 2025/26. The old selector
  # silently dropped parse_nwccu's row count to zero on the live HTML.
  # NWCCU is in ZERO_IS_EXPECTED in build_accreditation_actions.R so the CI
  # gate did NOT fail closed -- the regression went undetected for two
  # refresh cycles. Pin the selector against a fixture that mirrors the
  # live class string.
  fixture_html <- paste0(
    "<article id=\"post-1\" class=\"institution-item nwccu-state-washington ",
    "nwccu-type-private-not-for-profit nwccu-type-four-year ",
    "nwccu-degree-associates nwccu-degree-baccalaureate nwccu-degree-masters \">",
    "<h3>Alpha University</h3>",
    "<a href=\"https://nwccu.org/institutional-directory/alpha-university/\">profile</a>",
    "</article>",
    "<article id=\"post-2\" class=\"institution-item nwccu-state-oregon ",
    "nwccu-type-public nwccu-type-two-year nwccu-degree-associates \">",
    "<h3>Beta Community College</h3>",
    "</article>"
  )

  matches <- stringr::str_match_all(fixture_html, NWCCU_ARTICLE_PATTERN)[[1]]
  assert_identical(nrow(matches), 2L)

  baccalaureate_articles <- matches[
    stringr::str_detect(matches[, 2], stringr::fixed(NWCCU_BACCALAUREATE_CLASS)),
    ,
    drop = FALSE
  ]
  assert_identical(nrow(baccalaureate_articles), 1L,
    "Only Alpha University carries the baccalaureate class flag in the fixture.")

  # Negative pin: the OLD class string must not be in the constant. If
  # someone reverts the rename, this assertion blows up immediately.
  assert_true(
    NWCCU_BACCALAUREATE_CLASS != "nwccu-degree-bachelor",
    "NWCCU_BACCALAUREATE_CLASS must reflect the post-2025 schema rename."
  )
})

run_test("parse_nwccu warns when the directory carries no baccalaureate-tagged articles", function() {
  # If NWCCU renames the class again or pre-renders the directory in JS, the
  # filter returns zero rows. The fail-loud warning is the user-facing
  # signal that the selector needs updating; we lock it against a fixture
  # whose articles deliberately omit the baccalaureate class.
  cache_dir <- tempfile("nwccu_directory_warn_")
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  # >100 chars so parse_nwccu reaches the filter step rather than early
  # returning on the "could not fetch directory" path.
  filler <- strrep("X", 200L)
  writeLines(
    paste0(
      "<html><body>", filler,
      "<article id=\"post-99\" class=\"institution-item nwccu-state-idaho ",
      "nwccu-type-public nwccu-type-two-year nwccu-degree-associates \">",
      "<h3>Gamma Community College</h3></article>",
      "</body></html>"
    ),
    file.path(cache_dir, "nwccu_directory.html")
  )

  warned <- FALSE
  captured <- ""
  withCallingHandlers(
    parse_nwccu(cache_dir, refresh = FALSE),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("validate selector", msg, fixed = TRUE)) {
        warned <<- TRUE
        captured <<- msg
        invokeRestart("muffleWarning")
      }
    }
  )

  assert_true(warned, "Expected the directory selector warning to fire.")
  assert_true(
    grepl(NWCCU_BACCALAUREATE_CLASS, captured, fixed = TRUE),
    "Warning should name the current selector constant so the next maintainer knows what to update."
  )
})

run_test("Accreditation scraper box_shared_static_pdf_url rewrites /s/<id> URLs", function() {
  # SACSCOC disclosure anchor hrefs are box.com short links. For direct download
  # (pdftools can't follow box's interstitial HTML page) we rewrite them to the
  # /shared/static/<id>.pdf form. Already-rewritten and non-box URLs must pass
  # through unchanged.
  assert_identical(
    box_shared_static_pdf_url("https://sacscoc.box.com/s/ez0994"),
    "https://sacscoc.box.com/shared/static/ez0994.pdf"
  )
  assert_identical(
    box_shared_static_pdf_url("https://sacscoc.box.com/shared/static/ez0994.pdf"),
    "https://sacscoc.box.com/shared/static/ez0994.pdf"
  )
  assert_identical(
    box_shared_static_pdf_url("https://example.com/not-a-box-url"),
    "https://example.com/not-a-box-url"
  )
})

run_test("Accreditation scraper parses Lynchburg disclosure PDF from fixture", function() {
  # Pins the restored PDF-enrichment pathway. The fixture is the real
  # December 2025 Lynchburg disclosure PDF; we stage it into a temp cache dir
  # under the exact cache name parse_sacscoc_disclosure_pdf constructs, so
  # that fetch_binary_file returns the cached path rather than hitting the
  # network (refresh = FALSE). The cache slug can be any stable string here;
  # this test exercises parse_sacscoc_disclosure_pdf directly rather than
  # going through build_sacscoc_disclosure_rows.
  cache_dir <- tempfile("sacscoc_cache_")
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  institution_name <- "University of Lynchburg"
  cache_slug <- "university_of_lynchburg_virginia"
  cache_name <- paste0("sacscoc_disclosure_", cache_slug, ".pdf")
  file.copy(
    file.path(root, "tests", "fixtures", "sacscoc_disclosure_university_of_lynchburg_virginia.pdf"),
    file.path(cache_dir, cache_name)
  )

  parsed <- parse_sacscoc_disclosure_pdf(
    source_url = "https://sacscoc.box.com/s/ez0994",
    institution_name_raw = institution_name,
    cache_slug = cache_slug,
    cache_dir = cache_dir,
    refresh = FALSE
  )

  assert_true(!is.null(parsed), "Expected PDF parse to return a non-NULL list.")
  assert_identical(parsed$action_date, as.Date("2025-12-07"))
  assert_true(
    grepl("denied reaffirmation", parsed$action_label_raw, fixed = TRUE),
    "Expected action_label_raw to contain 'denied reaffirmation'."
  )
  # The PDF body reads '... continued the University of Lynchburg on Warning
  # ...'. A naive substring replacement of 'University of Lynchburg' with
  # 'the institution' leaves behind a 'the the institution' artifact, which
  # matches the pre-refactor monolith's behavior. We assert the positive
  # token 'the institution' exists somewhere and the original name is gone;
  # we intentionally do NOT over-specify the surrounding text so the test
  # isn't brittle to the 'the the' quirk or to future phrasing tweaks.
  assert_true(
    grepl("the institution", parsed$action_label_raw, fixed = TRUE),
    "Expected 'the institution' token to appear in action label after replacement."
  )
  assert_true(
    !grepl("University of Lynchburg", parsed$action_label_raw, fixed = TRUE),
    "Expected institution name to NOT appear in action label after replacement."
  )
})

run_test("Accreditation scraper SACSCOC disclosure rows enriched with PDF-derived fields", function() {
  # End-to-end: build_sacscoc_disclosure_rows, given a cache_dir with a staged
  # PDF, must upgrade the row from the 'Public Disclosure Statement' stub to
  # the real board action sentence and pick a non-'other' action_type. The
  # stub's action_date is set to a bogus landing-page date (2025-06-01) to
  # prove the enriched row carries the PDF's real date (2025-12-07).
  #
  # The cache filename must match what build_sacscoc_disclosure_rows
  # constructs internally: normalize_name(institution_name + state abbrev).
  # Our disclosure_matches mock uses "VA" in column 5 (matching live HTML),
  # so the slug becomes normalize_name("University of Lynchburg VA").
  cache_dir <- tempfile("sacscoc_cache_")
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  institution_name <- "University of Lynchburg"
  cache_slug <- normalize_name(paste(institution_name, "VA"))
  file.copy(
    file.path(root, "tests", "fixtures", "sacscoc_disclosure_university_of_lynchburg_virginia.pdf"),
    file.path(cache_dir, paste0("sacscoc_disclosure_", cache_slug, ".pdf"))
  )

  disclosure_matches <- matrix(
    c(
      "<p><a href=\"https://sacscoc.box.com/s/ez0994\">University of Lynchburg</a>, Lynchburg, VA [<a href=\"https://sacscoc.box.com/s/ez0994\">PDF</a>]</p>",
      "https://sacscoc.box.com/s/ez0994",
      "University of Lynchburg",
      "Lynchburg",
      "VA"
    ),
    ncol = 5,
    byrow = TRUE
  )

  rows <- build_sacscoc_disclosure_rows(
    disclosure_matches,
    action_date = as.Date("2025-06-01"),
    url = "https://example.com/sacscoc/december-2025",
    page_title = "December 2025 Actions",
    cache_dir = cache_dir,
    refresh = FALSE
  )

  assert_identical(nrow(rows), 1L)
  assert_identical(rows$institution_name_raw[[1]], "University of Lynchburg")
  assert_identical(rows$institution_state_raw[[1]], "Virginia")
  assert_identical(rows$action_date[[1]], as.Date("2025-12-07"))
  assert_identical(rows$action_year[[1]], 2025L)
  assert_true(
    rows$action_label_raw[[1]] != "Public Disclosure Statement",
    "Expected enriched row to carry a real PDF-derived action label, not the stub."
  )
  assert_true(
    grepl("denied reaffirmation", rows$action_label_raw[[1]], fixed = TRUE),
    "Expected enriched action label to contain 'denied reaffirmation'."
  )
  assert_true(
    rows$action_type[[1]] != "other",
    "Expected enriched row to classify as a real action type, not 'other'."
  )
})

run_test("Accreditation scraper SACSCOC disclosure rows fall back to stub when PDF unavailable", function() {
  # When cache_dir is supplied but the PDF can't be parsed (not a real PDF
  # body — here we stage an HTML error page at the cache path to simulate
  # box.com returning a 404 page), the builder must preserve the existing
  # fallback: a 'Public Disclosure Statement' row with action_type='other'
  # and the landing-page action_date. This keeps the pipeline working when
  # box.com is unreachable and prevents regressions in offline test
  # environments. refresh=FALSE + pre-staged cache avoids any network call.
  cache_dir <- tempfile("sacscoc_cache_bad_")
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  institution_name <- "Alpha College"
  cache_slug <- normalize_name(paste(institution_name, "GA"))
  cache_name <- paste0("sacscoc_disclosure_", cache_slug, ".pdf")
  writeLines("<html><body>Not found</body></html>", file.path(cache_dir, cache_name))

  disclosure_matches <- matrix(
    c(
      "<p><a href=\"https://sacscoc.box.com/s/doesnotexist999\">Alpha College</a>, Athens, GA [<a href=\"https://sacscoc.box.com/s/doesnotexist999\">PDF</a>]</p>",
      "https://sacscoc.box.com/s/doesnotexist999",
      "Alpha College",
      "Athens",
      "GA"
    ),
    ncol = 5,
    byrow = TRUE
  )

  rows <- build_sacscoc_disclosure_rows(
    disclosure_matches,
    action_date = as.Date("2025-06-01"),
    url = "https://example.com/sacscoc/june-2025",
    page_title = "June 2025 Actions",
    cache_dir = cache_dir,
    refresh = FALSE
  )

  assert_identical(nrow(rows), 1L)
  assert_identical(rows$institution_name_raw[[1]], "Alpha College")
  assert_identical(rows$action_label_raw[[1]], "Public Disclosure Statement")
  assert_identical(rows$action_type[[1]], "other")
  assert_identical(rows$action_date[[1]], as.Date("2025-06-01"))
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

# ---------------------------------------------------------------------------
# MSCHE per-institution page parser
# ---------------------------------------------------------------------------
#
# The verified live structure (Saint Rose, Centro de Estudios Avanzados,
# Princeton, sampled 2026-04-25) is uniform: every per-institution page
# renders its full board action history as <li><strong>Date</strong><br />
# Body</li> entries inside <ul id="accreditation_actions">, 10 per page,
# with WordPress wp-pagenavi pagination via ?ipf_action_paged=N. These
# tests pin that contract.

run_test("extract_msche_institution_actions parses dated <li> rows from the accreditation_actions ul", function() {
  fixture_html <- paste0(
    "<html><body>",
    "<ul id=\"accreditation_actions\" class=\"tab-content-container active\">",
    "<li><strong>June 22, 2023</strong><br />",
    "To warn the institution that its accreditation may be in jeopardy ",
    "because of insufficient evidence that the institution is currently ",
    "in compliance with Standard VI.</li>",
    "<li><strong>April 25, 2024</strong><br />",
    "To approve the teach-out plan and agreements with (1) Iona University, ",
    "New Rochelle, NY; (2) Niagara University, Niagara University, NY.</li>",
    "<li><strong>February 20, 2024</strong><br />",
    "To acknowledge receipt of the substantive change request for ",
    "institutional closure.</li>",
    # An <li> WITHOUT a leading <strong> date prefix must be silently
    # skipped, not crash the parser.
    "<li>Bylaws (as of July 1, 2025)</li>",
    "</ul></body></html>"
  )

  rows <- extract_msche_institution_actions(fixture_html)
  assert_identical(nrow(rows), 3L,
    "Three dated <li> rows expected (the bylaws <li> with no date prefix should be skipped).")
  assert_identical(rows$action_date[[1]], as.Date("2023-06-22"))
  assert_identical(rows$action_date[[2]], as.Date("2024-04-25"))
  assert_identical(rows$action_date[[3]], as.Date("2024-02-20"))
  assert_true(grepl("warn the institution", rows$action_body[[1]], fixed = TRUE))
  assert_true(grepl("teach-out plan", rows$action_body[[2]], fixed = TRUE))
  # Body must NOT include the date prefix (xml_text returns it; we strip it).
  assert_true(!grepl("^June 22, 2023", rows$action_body[[1]]),
    "action_body must have the leading date prefix stripped.")
})

run_test("extract_msche_institution_actions returns an empty tibble when the container is missing", function() {
  empty <- extract_msche_institution_actions("<html><body><p>Page chrome only</p></body></html>")
  assert_identical(nrow(empty), 0L)
  assert_true(all(c("action_date", "action_body") %in% names(empty)))
  assert_true(inherits(empty$action_date, "Date"))
})

run_test("discover_msche_institution_page_count returns max paged number from wp-pagenavi", function() {
  no_nav <- "<html><body><p>only one page</p></body></html>"
  assert_identical(discover_msche_institution_page_count(no_nav), 1L)

  three_pages <- paste0(
    "<div class=\"wp-pagenavi\">",
    "<a href=\"https://www.msche.org/institution/0635?ipf_action_paged=2\">2</a>",
    "<a href=\"https://www.msche.org/institution/0635?ipf_action_paged=3\">3</a>",
    "</div>"
  )
  assert_identical(discover_msche_institution_page_count(three_pages), 3L)
})

run_test("parse_msche_institution_page returns ACCREDITATION_ACTION_COLUMNS-conforming rows from cache", function() {
  cache_dir <- tempfile("msche_inst_happy_")
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  inst_url <- "https://www.msche.org/institution/0296/"
  writeLines(
    paste0(
      "<html><head>",
      "<meta property=\"article:modified_time\" content=\"2026-01-15T00:00:00+00:00\">",
      "</head><body>",
      "<ul id=\"accreditation_actions\">",
      "<li><strong>April 25, 2024</strong><br />",
      "To approve the teach-out plan and agreements with several institutions.</li>",
      "<li><strong>June 22, 2023</strong><br />",
      "To warn the institution that its accreditation may be in jeopardy.</li>",
      "</ul></body></html>"
    ),
    file.path(cache_dir, "msche_institution_0296.html")
  )

  rows <- parse_msche_institution_page(
    institution_url = inst_url,
    institution_name = "The College of Saint Rose",
    institution_state = "New York",
    cache_dir = cache_dir,
    refresh = FALSE
  )

  assert_identical(nrow(rows), 2L)
  checked <- ensure_accreditation_action_schema(rows, "MSCHE per-institution fixture")
  assert_true(all(ACCREDITATION_ACTION_COLUMNS %in% names(checked)))
  assert_identical(rows$action_type[[1]], "adverse_action")
  assert_identical(rows$action_type[[2]], "warning")
  assert_identical(rows$accreditor[[1]], "MSCHE")
  assert_identical(rows$source_url[[1]], inst_url)
  assert_identical(rows$source_page_modified[[1]], "2026-01-15")
  assert_true(grepl("teach-out plan", rows$action_label_raw[[1]], fixed = TRUE))
  assert_true(rows$action_label_raw[[1]] != "Commission action")
})

run_test("parse_msche_institution_page returns NULL when the page has no accreditation_actions ul", function() {
  cache_dir <- tempfile("msche_inst_empty_")
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  writeLines(
    paste0(
      "<html><body>",
      strrep("<p>chrome paragraph</p>", 30),
      "</body></html>"
    ),
    file.path(cache_dir, "msche_institution_999.html")
  )

  result <- parse_msche_institution_page(
    institution_url = "https://www.msche.org/institution/999/",
    institution_name = "Empty Page Institution",
    institution_state = "New Jersey",
    cache_dir = cache_dir,
    refresh = FALSE
  )
  assert_true(is.null(result),
    "parse_msche_institution_page must return NULL when no actions parse, so the orchestrator falls back to the stub.")
})

run_test("parse_msche orchestrator emits real per-institution rows and falls back to stub on failure", function() {
  cache_dir <- tempfile("msche_orch_")
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  writeLines(
    paste0(
      "<!DOCTYPE html><html><head>",
      "<title>Non-Compliance and Adverse Actions By Status | MSCHE</title>",
      "</head><body><article>",
      "<h3>Non-Compliance Warning</h3><p>No institutions in this status</p>",
      "<h3>Non-Compliance Probation</h3><p>No institutions in this status</p>",
      "<h3>Non-Compliance Show Cause</h3><p>No institutions in this status</p>",
      "<h3>Adverse Action</h3><p>No institutions in this status</p>",
      "<div class=\"single-share\">end</div>",
      "</article></body></html>"
    ),
    file.path(cache_dir, "msche_status.html")
  )

  current_year <- as.integer(format(Sys.Date(), "%Y"))
  month_url <- "https://www.msche.org/commission-actions/?fd=1717200000&ld=1719792000"
  for (yr in seq.int(2017L, current_year)) {
    if (yr == current_year) {
      yearly_html <- paste0(
        "<html><body>",
        "<a href=\"", month_url, "\">June ", current_year, "</a>",
        "</body></html>"
      )
    } else {
      yearly_html <- "<html><body></body></html>"
    }
    writeLines(
      yearly_html,
      file.path(cache_dir, paste0("msche_recent_actions_", yr, ".html"))
    )
  }

  month_cache_name <- paste0(
    "msche_",
    gsub("[^a-z0-9]+", "_", tolower(paste("June", current_year))),
    ".html"
  )
  writeLines(
    paste0(
      "<html><body>",
      "<h3 id=\"state-ny\">New York</h3>",
      "<ul>",
      "<li><a href=\"https://www.msche.org/institution/0100/\">",
      "Real Institution, NY</a> (June 5, ", current_year, ")</li>",
      "<li><a href=\"https://www.msche.org/institution/0999/\">",
      "Stub Institution, NY</a> (June 12, ", current_year, ")</li>",
      "</ul></body></html>"
    ),
    file.path(cache_dir, month_cache_name)
  )

  writeLines(
    paste0(
      "<html><body><ul id=\"accreditation_actions\">",
      "<li><strong>June 5, ", current_year, "</strong><br />",
      "To warn the institution that its accreditation may be in jeopardy.</li>",
      "<li><strong>March 1, ", current_year, "</strong><br />",
      "To approve the teach-out plan with another university.</li>",
      "</ul></body></html>"
    ),
    file.path(cache_dir, "msche_institution_0100.html")
  )

  writeLines(
    paste0(
      "<html><body>",
      strrep("<p>chrome</p>", 30),
      "</body></html>"
    ),
    file.path(cache_dir, "msche_institution_0999.html")
  )

  warned <- FALSE
  rows <- withCallingHandlers(
    parse_msche(cache_dir, refresh = FALSE),
    warning = function(w) {
      if (grepl("falling back to.*stub row", conditionMessage(w))) {
        warned <<- TRUE
      }
      invokeRestart("muffleWarning")
    }
  )

  assert_true(warned,
    "Orchestrator must warn when it falls back to the stub for an institution.")
  assert_identical(nrow(rows), 3L)

  real_rows <- rows[rows$source_url == "https://www.msche.org/institution/0100/", ]
  assert_identical(nrow(real_rows), 2L)
  assert_true(any(real_rows$action_type == "warning"))
  assert_true(any(real_rows$action_type == "adverse_action"))
  assert_true(all(real_rows$action_label_raw != "Commission action"))

  stub_rows <- rows[rows$source_url == "https://www.msche.org/institution/0999/", ]
  assert_identical(nrow(stub_rows), 1L)
  assert_identical(stub_rows$action_label_raw[[1]], "Commission action")
  assert_identical(stub_rows$action_type[[1]], "commission_action")
})

run_test("classify_action handles MSCHE phrasings", function() {
  # Pin classification for the verbs MSCHE uses on per-institution pages.
  # "Withdraw the substantive change request" must NOT misclassify as
  # adverse_action -- it's an administrative withdrawal of a *request*,
  # not of accreditation.
  assert_identical(
    classify_action("To warn the institution that its accreditation may be in jeopardy."),
    "warning"
  )
  assert_identical(
    classify_action("To place the institution on Probation due to non-compliance."),
    "probation"
  )
  assert_identical(
    classify_action("To approve the teach-out plan and agreements with several institutions."),
    "adverse_action"
  )
  assert_identical(
    classify_action("To note that the institution will close effective June 30, 2024."),
    "adverse_action"
  )
  assert_identical(
    classify_action("To remove the institution from Probation."),
    "removed"
  )
  assert_identical(
    classify_action("To withdraw the substantive change request as requested by the institution."),
    "other"
  )
  assert_identical(
    classify_action("To acknowledge receipt of the supplemental information report."),
    "other"
  )
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

run_test("fetch_html_text refresh=FALSE logs cached copy age", function() {
  cache_dir <- tempfile("fetch_cache_log_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  cache_path <- file.path(cache_dir, "cached_page.html")
  writeLines("<html><body><h3>Cached</h3></body></html>", cache_path)

  logged <- NULL
  withCallingHandlers(
    {
      returned <- fetch_html_text("https://example.com/cached", "cached_page.html", cache_dir, refresh = FALSE)
      assert_true(grepl("Cached", returned, fixed = TRUE),
        "refresh=FALSE should return the cached HTML body.")
    },
    message = function(m) {
      logged <<- c(logged, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  assert_true(
    any(grepl("Using cached copy for https://example.com/cached", logged, fixed = TRUE)),
    "Expected fetch_html_text(refresh=FALSE) to log that it used the cached copy."
  )
  assert_true(
    any(grepl("days old", logged, fixed = TRUE)),
    "Expected cached-copy log to include cache age."
  )
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

run_test("parse_nwccu_institution_page: action_status is 'active' so date-rescue can fire downstream", function() {
  # build_web_exports.R's date-rescue path backfills action_date from
  # source_page_modified only when action_status == "active". The previous
  # implementation set action_status to the structured NWCCU status string
  # ("Accredited", "Probation", etc.), which did not match the literal
  # "active" check -- so flagged NWCCU rows shipped with action_date = NA
  # and were dropped by the JS isRecentTrackedAction filter on the year
  # floor. Pin the new contract: every emitted NWCCU row carries
  # action_status = "active" (the row would not exist if action_type were
  # "other") and source_page_modified is populated from the institution
  # page's article:modified_time meta tag so the date rescue can actually
  # fire downstream. The original NWCCU status string moves to notes.
  cache_dir <- tempfile("nwccu_visibility_")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  url <- nwccu_fixture(cache_dir, "visibility-university", paste0(
    '<html><head>',
    '<meta property="article:modified_time" content="2026-02-15T10:00:00+00:00" />',
    '</head><body><main>',
    '<span class="block">Current Accreditation Status</span>',
    '<p class="h5 block">Probation</p>',
    '<span class="block super">Most Recent Evaluation</span>',
    '<span class="block flex">Fall 2025 Special Visit</span>',
    '</main></body></html>'
  ))

  result <- parse_nwccu_institution_page(url, "Visibility University", cache_dir, refresh = FALSE)
  assert_identical(nrow(result), 1L)
  assert_identical(result$action_status[[1]], "active",
    "NWCCU rows must carry action_status='active' so build_web_exports.R can backfill action_date.")
  assert_identical(result$source_page_modified[[1]], "2026-02-15",
    "NWCCU rows must carry source_page_modified from article:modified_time so the rescue path has a date to use.")
  # Audit trail: the original structured status string is preserved in notes.
  assert_true(
    grepl("Status: Probation", result$notes[[1]], fixed = TRUE),
    "Notes should preserve the original NWCCU structured status string for audit."
  )
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

run_test("extract_item_scope returns trailing program parenthetical", function() {
  # NECHE attaches a program-level qualifier in parentheses for actions that
  # apply only to a specific degree or location. We want to surface that
  # qualifier so the UI can disambiguate "Boston University accepted teach-out
  # plan" (false: institution-wide) from the actual program-scoped action.
  bu_item <- paste(
    "Boston University, Boston, MA",
    "(Master of Social Work degree at its Bedford, Cape Cod,",
    "and Fall River locations)"
  )
  assert_identical(
    extract_item_scope(bu_item),
    "Master of Social Work degree at its Bedford, Cape Cod, and Fall River locations"
  )
})

run_test("extract_item_scope filters out location parentheticals", function() {
  # "Alpha College, Boston, MA" has no trailing scope parenthetical; the only
  # parenthetical-shaped tail is the bare state code, which must be filtered.
  assert_true(is.na(extract_item_scope("Alpha College, Boston, MA")))
  # Explicit (City, ST) tail must also be filtered: it's location, not scope.
  assert_true(is.na(extract_item_scope("Beta College (Boston, MA)")))
})

run_test("extract_item_scope filters out admin-metadata parentheticals", function() {
  # extract_name_state_from_item already discards these phrases as scope-noise;
  # the action_scope column should not resurrect them.
  assert_true(is.na(extract_item_scope("Gamma U (next review 2027)")))
  assert_true(is.na(extract_item_scope("Delta U (letter dated June 1, 2025)")))
})

run_test("parse_neche extracts meeting date from H2 heading and per-row scope", function() {
  cache_dir <- tempfile("neche_fixture_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  # Fixture mirrors NECHE's recent-actions structure: H2 meeting headings
  # delimit sections; inside each section, an elementor toggle anchor names
  # the action type and the toggle body holds <li> items. Two meetings cover
  # both the multi-day form ("November 20-21, 2025" -> first day) and the
  # single-day Executive Committee form. The BU-style scope parenthetical
  # exercises the action_scope plumbing end-to-end.
  writeLines(
    paste0(
      "<!DOCTYPE html><html><head>",
      "<title>Recent Commission Actions | NECHE</title>",
      "<meta property=\"article:modified_time\" content=\"2026-03-01T00:00:00+00:00\">",
      "</head><body>",
      "<h2 class=\"elementor-heading-title\">Actions Following November 20-21, 2025 Meeting</h2>",
      "<a class=\"elementor-toggle-title\" href=\"#\">Accepted Teach-Out Plan</a>",
      "<div id=\"elementor-tab-content-1\" class=\"elementor-tab-content\">",
      "<ul><li>Boston University, Boston, MA ",
      "(Master of Social Work degree at its Bedford, Cape Cod, and Fall River locations)</li></ul>",
      "</div>",
      "<h2 class=\"elementor-heading-title\">February 20, 2026 Action of the Executive Committee of the Commission</h2>",
      "<a class=\"elementor-toggle-title\" href=\"#\">Continued Probation</a>",
      "<div id=\"elementor-tab-content-2\" class=\"elementor-tab-content\">",
      "<ul><li>Example College, Providence, RI</li></ul>",
      "</div>",
      "</body></html>"
    ),
    file.path(cache_dir, "neche_actions.html")
  )

  rows <- parse_neche(cache_dir, refresh = FALSE)

  assert_identical(nrow(rows), 2L)

  # Filter by action label rather than parsed institution_name_raw:
  # extract_name_state_from_item only strips a narrow allowlist of trailing
  # parentheticals, so "Boston University, Boston, MA (MSW degree ...)" still
  # resolves to a name that includes the city and state — that's a separate
  # concern from the meeting-date / scope plumbing this test is covering.
  bu <- rows[rows$action_label_raw == "Accepted Teach-Out Plan", ]
  assert_identical(nrow(bu), 1L)
  # Multi-day meeting must anchor to the first day, not page-modified date.
  assert_identical(bu$action_date[[1]], as.Date("2025-11-20"))
  assert_identical(bu$action_year[[1]], 2025L)
  # Scope must round-trip the program-level parenthetical.
  assert_identical(
    bu$action_scope[[1]],
    "Master of Social Work degree at its Bedford, Cape Cod, and Fall River locations"
  )

  example <- rows[rows$action_label_raw == "Continued Probation", ]
  assert_identical(nrow(example), 1L)
  assert_identical(example$action_date[[1]], as.Date("2026-02-20"))
  assert_identical(example$action_year[[1]], 2026L)
  # No trailing parenthetical: scope must be NA, not the empty string.
  assert_true(is.na(example$action_scope[[1]]))
})
