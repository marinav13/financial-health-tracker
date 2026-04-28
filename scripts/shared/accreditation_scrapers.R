################################################################################
# scripts/shared/accreditation_scrapers.R
#
# Per-accreditor HTML scraper functions that extract accreditation action data
# from six major accreditors' websites (MSCHE, HLC, SACSCOC, NECHE, WSCUC).
# Each accreditor has a different site structure, so this file contains custom
# scraping logic for each one, all converting output to a standardized table.
#
################################################################################

# ---------------------------------------------------------------------------
# SHARED INNER-LOOP PRIMITIVES FOR HTML PARSING
# ---------------------------------------------------------------------------

ACCREDITATION_ACTION_COLUMNS <- c(
  "institution_name_raw",
  "institution_state_raw",
  "accreditor",
  "action_type",
  "action_label_raw",
  "action_status",
  "action_date",
  "action_year",
  # Per-row qualifier (e.g. "Master of Social Work degree at its Bedford, Cape
  # Cod, and Fall River locations") so the UI can avoid implying that a
  # program-level NECHE/WSCUC action is institution-wide. Set by
  # parse_items_to_rows; scrapers that bypass that helper emit NA and
  # ensure_accreditation_action_schema fills the column with NA.
  "action_scope",
  "source_url",
  "source_title",
  "notes",
  "last_seen_at",
  "source_page_url",
  "source_page_modified"
)

empty_accreditation_action_rows <- function() {
  tibble::tibble(
    institution_name_raw = character(),
    institution_state_raw = character(),
    accreditor = character(),
    action_type = character(),
    action_label_raw = character(),
    action_status = character(),
    action_date = as.Date(character()),
    action_year = integer(),
    action_scope = character(),
    source_url = character(),
    source_title = character(),
    notes = character(),
    last_seen_at = character(),
    source_page_url = character(),
    source_page_modified = character()
  )
}

ensure_accreditation_action_schema <- function(df, context = "accreditation scraper output") {
  if (is.null(df) || (nrow(df) == 0L && ncol(df) == 0L)) {
    return(empty_accreditation_action_rows())
  }

  # Newly added columns whose absence is non-fatal: scrapers that bypass
  # parse_items_to_rows (MSCHE current-status + monthly, HLC, SACSCOC,
  # NWCCU) build their result tibbles inline and don't know about
  # action_scope. We pad it here with NA_character_ rather than asking
  # every scraper to spell it out, while keeping the fail-closed stop()
  # below for every other required column the rest of the pipeline
  # genuinely depends on (institution_name_raw, accreditor, source_url,
  # ...). Keeping this allowlist explicit prevents the helper from
  # silently swallowing future schema drift.
  paddable_cols <- "action_scope"
  for (nm in intersect(paddable_cols, setdiff(ACCREDITATION_ACTION_COLUMNS, names(df)))) {
    df[[nm]] <- NA_character_
  }

  missing_cols <- setdiff(ACCREDITATION_ACTION_COLUMNS, names(df))
  if (length(missing_cols) > 0L && nrow(df) > 0L) {
    stop(
      sprintf(
        "%s is missing required accreditation action columns: %s",
        context,
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  for (nm in missing_cols) df[[nm]] <- NA
  df$last_seen_at <- as.character(df$last_seen_at)
  df[, c(ACCREDITATION_ACTION_COLUMNS, setdiff(names(df), ACCREDITATION_ACTION_COLUMNS)), drop = FALSE]
}

# Per-site empty-result guard. Emits a warning when a scraper returns zero
# rows but the input HTML was substantive — the shape that indicates either a
# selector regression, a JS-rendered page the scraper can't see, or a silent
# content change. A genuine "no public actions right now" outcome and a
# silent regression look identical at the row-count level; comparing row
# count to HTML size is the cheapest signal that distinguishes them.
#
# This complements the category-level cross-run guards in
# warn_if_scrape_count_dropped and warn_if_action_type_dropped, which only
# fire after a prior CSV exists to compare against. The per-site warning
# surfaces the same class of bug on a first-run or fresh-cache refresh.
#
# Parameters:
#   accreditor       - accreditor label used in the warning message.
#   source_url       - URL whose parse returned zero rows.
#   rows             - the candidate result tibble (or any object with nrow()).
#   html             - the HTML body the parser consumed; accepts character
#                      vectors (HTML_chunks concatenated) or NULL.
#   threshold_bytes  - minimum HTML size at which 0 rows is considered
#                      suspicious. Default 2000 avoids false positives on
#                      tiny error pages or maintenance banners that
#                      legitimately carry no action data.
#   detail           - optional extra context appended to the warning.
#
# Returns invisible(NULL). By default emits a warning() and keeps running so
# local/exploratory scraper iteration is not disrupted. When `fail = TRUE`
# (or the process-level option `tracker.fail_on_empty_parse` is TRUE), the
# helper stop()s instead. The publish path in build_accreditation_actions.R
# flips that option on in CI (unless --allow-partial-accreditation is set),
# so a CI refresh fails fast at the parser site rather than continuing
# through every downstream step and only being caught by the workflow's log
# grep gate at the end. Symmetric with warn_if_scrape_count_dropped and
# warn_if_action_type_dropped, which expose the same fail= surface.
warn_on_empty_parse <- function(accreditor, source_url, rows, html,
                                threshold_bytes = 2000L, detail = NULL,
                                fail = getOption("tracker.fail_on_empty_parse", FALSE)) {
  if (!is.null(rows) && is.data.frame(rows) && nrow(rows) > 0L) return(invisible(NULL))
  html_bytes <- if (is.null(html)) 0L else sum(nchar(html))
  if (html_bytes < threshold_bytes) return(invisible(NULL))
  msg <- sprintf(
    "%s: parsed 0 rows from %s (HTML %d bytes)%s. Returning empty table \u2014 validate scraper output.",
    accreditor, source_url, html_bytes,
    if (is.null(detail)) "" else paste0(" \u2014 ", detail)
  )
  if (isTRUE(fail)) {
    stop(msg, call. = FALSE)
  }
  warning(msg, call. = FALSE)
  invisible(NULL)
}

# Converts a heading + list of institution items into standardized action rows.
# Shared by accreditors that use the same pattern: a heading describing the action
# (e.g., "Probation"), followed by <li> items for each institution.
parse_items_to_rows <- function(raw_items, accreditor, heading,
                                action_date, action_year,
                                source_url, source_title,
                                source_page_url, source_page_modified) {
  purrr::map_dfr(raw_items, function(item) {
    # Parse the institution name and state from the list item text
    parsed <- extract_name_state_from_item(item)
    tibble::tibble(
      institution_name_raw  = parsed$institution_name_raw,
      institution_state_raw = parsed$institution_state_raw,
      accreditor            = accreditor,
      action_type           = classify_action(heading),
      action_label_raw      = heading,
      action_status         = classify_status(heading),
      action_date           = action_date,
      action_year           = action_year,
      action_scope          = extract_item_scope(item),
      source_url            = source_url,
      source_title          = source_title,
      notes                 = item,
      last_seen_at          = Sys.time(),
      source_page_url       = source_page_url,
      source_page_modified  = source_page_modified
    )
  })
}

# Extracts all <li> items from an HTML block and returns their cleaned text content.
# Used to pull institution names from bulleted lists on accreditor pages.
extract_list_items_from_html <- function(html_block, item_pattern = "(?s)<li[^>]*>(.*?)</li>") {
  # Find all <li> elements and capture their content (group 2)
  item_matches <- stringr::str_match_all(html_block, item_pattern)[[1]]
  # Return empty character vector if no matches
  if (nrow(item_matches) == 0) {
    return(character())
  }
  # Clean the extracted text (decode HTML, normalize whitespace)
  raw_items <- clean_text(item_matches[, 2])
  # Return only non-empty items
  raw_items[nzchar(raw_items)]
}

# Splits HTML into sections based on a regex pattern that captures heading + body.
extract_regex_heading_sections <- function(html, section_pattern) {
  matches <- stringr::str_match_all(html, section_pattern)[[1]]
  if (nrow(matches) == 0) {
    return(tibble::tibble(heading = character(), body = character()))
  }
  tibble::tibble(
    heading = clean_text(matches[, 2]),
    body = matches[, 3]
  )
}

# Splits HTML into sections using HTML heading tags (default h2) to delimit sections.
# Matches opening tags with or without attributes (e.g. `<h2>` or `<h2 class="...">`).
extract_tag_heading_sections <- function(html, heading_tag = "h2") {
  heading_pattern <- paste0("(?s)<", heading_tag, "(?:\\s[^>]*)?>(.*?)</", heading_tag, ">")
  heading_matches <- stringr::str_match_all(html, heading_pattern)[[1]]
  heading_locs <- stringr::str_locate_all(html, heading_pattern)[[1]]
  if (nrow(heading_matches) == 0 || nrow(heading_locs) == 0) {
    return(tibble::tibble(heading = character(), body = character()))
  }

  headings <- clean_text(heading_matches[, 2])
  bodies <- purrr::map_chr(seq_along(headings), function(i) {
    start_pos <- heading_locs[i, 2] + 1L
    end_pos <- if (i < nrow(heading_locs)) heading_locs[i + 1, 1] - 1L else nchar(html)
    substr(html, start_pos, end_pos)
  })

  tibble::tibble(heading = headings, body = bodies)
}

# Iterates over HTML sections and extracts public action items (filtering by keywords).
# Only sections whose headings contain public action keywords are processed.
parse_public_action_sections <- function(sections, accreditor,
                                         action_date, action_year,
                                         source_url, source_title,
                                         source_page_url, source_page_modified) {
  if (nrow(sections) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(seq_len(nrow(sections)), function(i) {
    heading <- sections$heading[[i]]
    if (!has_public_action_keywords(heading)) {
      return(tibble::tibble())
    }

    raw_items <- extract_list_items_from_html(sections$body[[i]])
    if (length(raw_items) == 0) {
      return(tibble::tibble())
    }

    parse_items_to_rows(
      raw_items = raw_items,
      accreditor = accreditor,
      heading = heading,
      action_date = action_date,
      action_year = action_year,
      source_url = source_url,
      source_title = source_title,
      source_page_url = source_page_url,
      source_page_modified = source_page_modified
    )
  })
}

MSCHE_CURRENT_STATUS_ACTION_TYPES <- c(
  "Non-Compliance Warning" = "warning",
  "Non-Compliance Probation" = "probation",
  "Non-Compliance Show Cause" = "show_cause",
  "Adverse Action" = "adverse_action"
)
MSCHE_CURRENT_STATUS_URL <- "https://www.msche.org/non-compliance-and-adverse-actions-by-status/"
MSCHE_RECENT_ACTIONS_URL <- "https://www.msche.org/recent-commission-actions/"

# Per-institution page parsing. Verified across Saint Rose (closed),
# Centro de Estudios Avanzados (currently sanctioned), and Princeton
# (routine/healthy): every per-institution page renders its full board
# action history inside <ul id="accreditation_actions"> as a flat list of
# <li><strong>Month DD, YYYY</strong><br />Body text</li> entries, 10 per
# page, with WordPress wp-pagenavi pagination at the bottom and an
# `?ipf_action_paged=N` URL parameter for additional pages. The
# per-institution page is the source of truth for the board action date
# (the monthly index conflates multiple actions on the same date) and
# carries the full history including pre-2017 entries.
MSCHE_INSTITUTION_PATH_PATTERN <- "/institution/([0-9]+)/?"
MSCHE_INSTITUTION_PAGINATION_PATTERN <- "ipf_action_paged=([0-9]+)"
MSCHE_INSTITUTION_DATE_FORMAT <- "%B %d, %Y"

MSCHE_RECENT_ACTIONS_LINK_PATTERN <- paste0(
  "<a href=\"(https://www\\.msche\\.org/commission-actions/\\?fd=[0-9]+(?:&amp;|&)ld=[0-9]+)\">",
  "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+([0-9]{4})</a>"
)

HLC_CURRENT_NOTICE_ACTION_LABELS <- c(
  "On Notice" = "On Notice",
  "On Probation" = "On Probation",
  "Removal of Sanction (past six months)" = "Removal of Sanction",
  "Withdrawal of Accreditation" = "Withdrawal of Accreditation"
)
HLC_ACTIONS_URL <- "https://www.hlcommission.org/for-students/accreditation-actions/"
HLC_CURRENT_NOTICE_BLOCK_PATTERN <- "(?s)<h3 class=\"wp-block-heading\">Current Public Disclosure Notices</h3>(.*?)(?=<h3 class=\"kt-adv-heading|</main>)"
HLC_NOTICE_PANE_PATTERN <- "(?s)<span class=\"kt-blocks-accordion-title\">(On Notice|On Probation|Removal of Sanction \\(past six months\\)|Withdrawal of Accreditation)</span>.*?<ul class=\"kt-svg-icon-list\">(.*?)</ul>"
HLC_DETAIL_LINK_PATTERN <- "href=\"(https://www\\.hlcommission\\.org/for-students/accreditation-actions/[a-z]+-20[0-9]{2}(?:-actions)?/?)\">([A-Za-z]+)\\s+([0-9]{4})</a>"

NECHE_TOGGLE_SECTION_PATTERN <- paste0(
  "(?s)<a class=\"elementor-toggle-title\"[^>]*>(.*?)</a>",
  ".*?<div id=\"elementor-tab-content-[^\"]+\" class=\"elementor-tab-content[^\"]*\"[^>]*>(.*?)</div>"
)
NECHE_ACTIONS_URL <- "https://www.neche.org/recent-commission-actions/"

# Matches a meeting date inside a NECHE H2 heading. NECHE uses two formats:
#   "Actions Following November 20-21, 2025 Meeting"
#   "February 20, 2026 Action of the Executive Committee of the Commission"
# For multi-day meetings we anchor the action_date to the first day so that
# "November 20-21, 2025" becomes 2025-11-20.
NECHE_MEETING_DATE_PATTERN <- paste0(
  "(January|February|March|April|May|June|July|August|September|October|November|December)",
  "\\s+(\\d{1,2})(?:-\\d{1,2})?,\\s*(\\d{4})"
)

WSCUC_ARCHIVE_URLS <- c(
  "https://www.wscuc.org/post/category/commission-actions/",
  "https://www.wscuc.org/post/category/commission-actions/page/2/"
)
WSCUC_DETAIL_LINK_PATTERN <- "href=\"(https://www\\.wscuc\\.org/post/[^\"#]+commission-actions/?)\""

SACSCOC_SANCTION_KEYWORD_PATTERN <- "\\bplaced on\\b|\\bcontinued on\\b|\\bremoved from\\b|withdraws from membership|withdrawal"
SACSCOC_STANDARD_ITEM_PATTERN <- "^(.*?),\\s*([^,]+),\\s*([^,(]+)\\s*\\((.*?)\\)$"
SACSCOC_WITHDRAWAL_ITEM_PATTERN <- "^(.*?)\\s*\\(([^)]+)\\)\\s*(withdraws from membership)$"
SACSCOC_LANDING_URL <- "https://sacscoc.org/institutions/accreditation-actions-and-disclosures/"
SACSCOC_LANDING_LINK_PATTERN <- "<a href=\"(https://sacscoc.org/institutions/accreditation-actions-and-disclosures/[^\"#]+?)\">(December [0-9]{4} Accreditation Actions and Public Disclosure Statements|June [0-9]{4} Accreditation Actions and Public Disclosure Statements)</a>"
SACSCOC_DISCLOSURE_ITEM_PATTERN <- paste0(
  # The captured anchor text and the post-state filler must NOT be allowed to
  # cross a <p>/<li> boundary, or the non-greedy `.*?` backtracks across the
  # SACSCOC landing-PDF paragraph and stitches its anchor's </a> to the first
  # institution row's `, City, ST` further down the page. That was producing
  # a phantom row whose institution name was a several-hundred-character body
  # blob (caught downstream by an institution-name str_detect filter, but only
  # after parse_sacscoc_disclosure_pdf was already called with the blob and
  # constructed a Windows-illegal cache filename). Tempered greedy tokens stop
  # the match at the next paragraph/list-item boundary in either direction.
  #
  # Capture groups:
  #   2 = box.com short URL
  #   3 = anchor text (institution name)
  #   4 = city (or "campus, city" for institutions whose published name
  #       continues into a campus designator after </a>, e.g.
  #       "State College of Florida</a>, Manatee-Sarasota, Bradenton,
  #       Florida [PDF]". The city group is non-greedy and may contain
  #       commas; backtracking pushes state to the LAST comma-separated
  #       token before the [PDF] marker or the closing tag.)
  #   5 = state (allows multi-word state names like "North Carolina";
  #       postal abbreviation tried first via the alternation)
  #
  # State anchor: a zero-width tempered lookahead asserting "no further
  # `, Letter` continuation token before </p>/</li>". That's the actual
  # invariant for "state is the LAST comma-separated location token" --
  # parentheticals like "(placed on Probation Good Cause)" and inner
  # anchors like "[<a href='...'>PDF</a>]" between state and the close
  # tag are both fine because they don't start a new comma+letter
  # continuation. Earlier attempts that required `\s*\[` (the [PDF]
  # marker) failed on December rows whose state was followed by a
  # parenthetical instead, and a "no `<` until close" lookahead failed
  # on December rows whose [PDF] link is wrapped in its own anchor tag.
  "(?s)<(?:p|li)>\\s*",
  "<a href=\"(https://sacscoc\\.box\\.com/s/[^\"]+)\">",
  "((?:(?!</?(?:p|li)\\b).)*?)</a>,\\s*",
  "([^<]+?),\\s*",
  "([A-Z]{2}|[A-Za-z][A-Za-z ]*[A-Za-z])",
  "(?=(?:(?!,\\s+[a-zA-Z]|</(?:p|li)\\b).)*</(?:p|li)>)",
  "(?:(?!</?(?:p|li)\\b).)*?",
  "</(?:p|li)>"
)

# Looks up a key in a named vector, returning the value or a default if not found.
lookup_or_default <- function(key, lookup, default = key) {
  value <- unname(lookup[[key]])
  if (is.null(value) || is.na(value)) default else value
}

# ---------------------------------------------------------------------------
# SACSCOC DISCLOSURE PDF ENRICHMENT HELPERS
# ---------------------------------------------------------------------------
#
# SACSCOC publishes a single-page "Public Disclosure Statement" PDF per
# institution on box.com. The landing page and detail pages only give us the
# institution name, city, state, and an anchor URL. The actual board action
# (e.g. "denied reaffirmation, continued the institution on Warning") and the
# exact action date are only present inside the PDF body.
#
# Prior to the refactor that split this file out of the monolithic
# build_accreditation_actions.R, every SACSCOC disclosure row was enriched by
# downloading the PDF and extracting action text + date. The refactor dropped
# these helpers, which silently downgraded every disclosure row to a generic
# "Public Disclosure Statement" stub with action_type="other". These helpers
# restore that behavior; callers must fall back to the stub on fetch/parse
# failure so the pipeline still works when box.com is unreachable.

# Rewrites a SACSCOC box.com short URL (https://sacscoc.box.com/s/<id>) to the
# corresponding direct-download URL (https://sacscoc.box.com/shared/static/<id>.pdf).
# Returns the input unchanged if it doesn't match the short-URL shape or is
# already in /shared/static/ form.
box_shared_static_pdf_url <- function(url) {
  url <- as.character(url)
  if (is.na(url) || !nzchar(url)) return(url)
  if (stringr::str_detect(url, "/shared/static/")) return(url)
  box_id <- stringr::str_match(url, "box\\.com/s/([A-Za-z0-9]+)")[, 2]
  ifelse(
    is.na(box_id),
    url,
    paste0("https://sacscoc.box.com/shared/static/", box_id, ".pdf")
  )
}

# Fetches a binary file (e.g. a PDF) with disk caching and graceful fallback
# to cache on network failures. Mirrors fetch_html_text's contract but returns
# the local file path rather than file contents, since binary content is
# typically processed by libraries that take paths (e.g. pdftools::pdf_text).
# Caching is essential because box.com is occasionally slow or unreachable.
fetch_binary_file <- function(url, cache_name, cache_dir, refresh = TRUE) {
  cache_path <- file.path(cache_dir, cache_name)
  if (!refresh && file.exists(cache_path)) {
    return(cache_path)
  }
  tryCatch(
    {
      resp <- httr2::request(url) |>
        httr2::req_user_agent("FinancialHealthProject/1.0") |>
        httr2::req_perform()
      raw_body <- httr2::resp_body_raw(resp)
      writeBin(raw_body, cache_path)
      cache_path
    },
    error = function(e) {
      if (file.exists(cache_path)) {
        message("Falling back to cached binary for ", url)
        cache_path
      } else {
        stop("Failed to fetch ", url, ": ", e$message)
      }
    }
  )
}

# Extracts the first Month-Day-Year date occurrence anywhere in a block of
# text, regardless of surrounding context. Used as a fallback when the
# structured "On <date>, the SACSCOC Board of Trustees ..." sentence is not
# present (e.g. PDFs with slightly different phrasing).
extract_date_from_text_anywhere <- function(x) {
  txt <- as.character(x)
  match <- stringr::str_match(
    txt,
    "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+([0-9]{1,2})(?:-[0-9]{1,2})?,\\s+([0-9]{4})"
  )
  parsed_text <- ifelse(
    !is.na(match[, 1]),
    paste(match[, 2], match[, 3], match[, 4]),
    NA_character_
  )
  suppressWarnings(as.Date(parsed_text, format = "%B %d %Y"))
}

# Downloads and parses a SACSCOC disclosure PDF, returning a list with
# `action_label_raw`, `action_date`, and `raw_text`, or NULL if the PDF can't
# be fetched, isn't a valid PDF, or doesn't contain a recognizable action
# sentence. Callers should fall back to the stub "Public Disclosure Statement"
# behavior when NULL is returned.
#
# Institution name replacement: we strip the institution's own name out of the
# extracted action text and replace it with "the institution", so that the
# action_label_raw is a portable sentence about the action itself rather than
# a sentence that mentions "University of Lynchburg was continued on Warning".
# This keeps downstream search/classification focused on the action keywords.
parse_sacscoc_disclosure_pdf <- function(source_url, institution_name_raw, cache_slug,
                                         cache_dir, refresh = TRUE) {
  pdf_url <- box_shared_static_pdf_url(source_url)
  pdf_path <- tryCatch(
    fetch_binary_file(
      pdf_url,
      paste0("sacscoc_disclosure_", cache_slug, ".pdf"),
      cache_dir = cache_dir,
      refresh = refresh
    ),
    error = function(e) NA_character_
  )
  if (is.na(pdf_path) || !file.exists(pdf_path)) {
    return(NULL)
  }

  header_raw <- tryCatch(readBin(pdf_path, what = "raw", n = 5), error = function(e) raw())
  if (length(header_raw) < 4 || rawToChar(header_raw[1:4]) != "%PDF") {
    return(NULL)
  }

  pdf_pages <- tryCatch(
    pdftools::pdf_text(pdf_path),
    error = function(e) character()
  )
  if (length(pdf_pages) == 0) {
    return(NULL)
  }

  raw_text <- paste(pdf_pages, collapse = "\n")
  action_match <- stringr::str_match(
    raw_text,
    "(?s)On\\s+((?:January|February|March|April|May|June|July|August|September|October|November|December)\\s+[0-9]{1,2},\\s+[0-9]{4}),\\s+the SACSCOC Board of Trustees\\s+(.*?)\\."
  )

  action_date <- if (!is.na(action_match[1, 2])) {
    suppressWarnings(as.Date(action_match[1, 2], format = "%B %d, %Y"))
  } else {
    extract_date_from_text_anywhere(raw_text)
  }

  action_text <- clean_text(action_match[1, 3])
  if (is.na(action_text) || !nzchar(action_text)) {
    action_text <- clean_text(stringr::str_match(
      raw_text,
      "(?s)What is the accreditation status of .*?\\?\\s+(.*?)\\."
    )[, 2])
  }
  if (is.na(action_text) || !nzchar(action_text)) {
    return(NULL)
  }

  if (!is.na(institution_name_raw) && nzchar(institution_name_raw)) {
    action_text <- stringr::str_replace_all(
      action_text,
      stringr::regex(institution_name_raw, ignore_case = TRUE),
      "the institution"
    )
    action_text <- clean_text(action_text)
  }

  list(
    action_label_raw = action_text,
    action_date = action_date,
    raw_text = raw_text
  )
}

# Parses a single SACSCOC sanction item, extracting institution name, state, and action details.
parse_sacscoc_sanction_item <- function(item, action_date, url, page_title) {
  item_clean <- clean_text(item)
  item_no_pdf <- stringr::str_remove(item_clean, "\\s*\\[?PDF\\]?$")
  item_no_pdf <- stringr::str_squish(item_no_pdf)

  standard_match <- stringr::str_match(item_no_pdf, SACSCOC_STANDARD_ITEM_PATTERN)
  if (!is.na(standard_match[1, 1])) {
    action_label <- clean_text(standard_match[1, 5])
    return(tibble::tibble(
      institution_name_raw = clean_text(standard_match[1, 2]),
      institution_state_raw = clean_text(standard_match[1, 4]),
      accreditor = "SACSCOC",
      action_type = classify_action(action_label),
      action_label_raw = action_label,
      action_status = classify_status(action_label),
      action_date = action_date,
      action_year = as.integer(format(action_date, "%Y")),
      source_url = url,
      source_title = page_title,
      notes = item_clean,
      last_seen_at = Sys.time(),
      source_page_url = url,
      source_page_modified = NA_character_
    ))
  }

  withdrawal_match <- stringr::str_match(item_no_pdf, SACSCOC_WITHDRAWAL_ITEM_PATTERN)
  if (!is.na(withdrawal_match[1, 1])) {
    state_abbr <- stringr::str_match(withdrawal_match[1, 3], ",\\s*([A-Z]{2})$")[, 2]
    action_label <- clean_text(withdrawal_match[1, 4])
    return(tibble::tibble(
      institution_name_raw = clean_text(withdrawal_match[1, 2]),
      institution_state_raw = state_name(state_abbr),
      accreditor = "SACSCOC",
      action_type = "adverse_action",
      action_label_raw = action_label,
      action_status = "active",
      action_date = action_date,
      action_year = as.integer(format(action_date, "%Y")),
      source_url = url,
      source_title = page_title,
      notes = item_clean,
      last_seen_at = Sys.time(),
      source_page_url = url,
      source_page_modified = NA_character_
    ))
  }

  tibble::tibble()
}

# Converts regex matches for public disclosure statements into standardized rows.
#
# SACSCOC disclosure paragraphs name the institution inside the anchor itself:
#   <a href="...box.com/s/...">University of Lynchburg</a>, Lynchburg, VA [PDF]
# SACSCOC_DISCLOSURE_ITEM_PATTERN's capture groups are therefore:
#   [, 2] = box.com URL
#   [, 3] = anchor text (the institution name)
#   [, 4] = post-anchor token (typically the city)
#   [, 5] = state (abbreviation or full name)
# Reading column 4 instead of column 3 would name every row after its CITY
# ("Lynchburg", "Emory", "Charlotte", etc.), which silently breaks IPEDS
# matching — a regression introduced and then pinned by a test in an earlier
# refactor. Reverted here; the tests now assert the correct shape.
#
# PDF enrichment: when cache_dir is supplied, each row's linked box.com PDF is
# downloaded and parsed to recover the real board action sentence (e.g.
# "denied reaffirmation, continued the institution on Warning") and the exact
# action date. On fetch/parse failure we fall back to the generic "Public
# Disclosure Statement" stub so the pipeline still works when box.com is
# unreachable. When cache_dir is NULL (e.g. from unit tests that don't want to
# hit the network), enrichment is skipped entirely.
build_sacscoc_disclosure_rows <- function(disclosure_matches, action_date, url, page_title,
                                          cache_dir = NULL, refresh = TRUE) {
  if (nrow(disclosure_matches) == 0) {
    return(tibble::tibble())
  }

  # Cap on institution_name length before PDF fetch. cache_slug is built via
  # normalize_name(paste(name, state)) and used as the cache filename. A real
  # SACSCOC institution name fits comfortably under 100 chars; if a regex
  # regression produces a multi-paragraph blob, we don't want to construct a
  # 4 KB filename that Windows refuses with "Invalid argument" mid-build.
  # Skip the PDF fetch on overlong names and let the row fall back to the
  # "Public Disclosure Statement" stub. The institution-name str_detect
  # filter further down already drops the row from the final output for the
  # known phantom-blob case; this cap is the symmetric guardrail at the
  # fetch boundary.
  MAX_INSTITUTION_NAME_LEN <- 120L

  purrr::map_dfr(seq_len(nrow(disclosure_matches)), function(i) {
    institution_name <- clean_text(disclosure_matches[i, 3])
    source_url <- disclosure_matches[i, 2]

    name_too_long <- !is.na(institution_name) && nchar(institution_name) > MAX_INSTITUTION_NAME_LEN

    enriched <- if (!is.null(cache_dir) && !name_too_long) {
      tryCatch(
        parse_sacscoc_disclosure_pdf(
          source_url = source_url,
          institution_name_raw = institution_name,
          cache_slug = normalize_name(paste(
            institution_name,
            clean_text(disclosure_matches[i, 5])
          )),
          cache_dir = cache_dir,
          refresh = refresh
        ),
        error = function(e) NULL
      )
    } else {
      if (name_too_long) {
        warning(sprintf(
          "build_sacscoc_disclosure_rows: institution_name_raw is %d chars (cap %d); skipping PDF enrichment to avoid an oversized cache filename. Source: %s",
          nchar(institution_name), MAX_INSTITUTION_NAME_LEN, source_url
        ), call. = FALSE)
      }
      NULL
    }

    action_label_raw <- if (!is.null(enriched) && nzchar(enriched$action_label_raw)) {
      enriched$action_label_raw
    } else {
      "Public Disclosure Statement"
    }
    action_type <- if (!is.null(enriched)) {
      classify_action(action_label_raw)
    } else {
      "other"
    }
    row_action_date <- if (!is.null(enriched) && !is.na(enriched$action_date)) {
      enriched$action_date
    } else {
      action_date
    }

    tibble::tibble(
      institution_name_raw = institution_name,
      institution_state_raw = state_name(clean_text(disclosure_matches[i, 5])),
      accreditor = "SACSCOC",
      action_type = action_type,
      action_label_raw = action_label_raw,
      action_status = classify_status(action_label_raw),
      action_date = row_action_date,
      action_year = as.integer(format(row_action_date, "%Y")),
      source_url = source_url,
      source_title = paste(page_title, "- Public Disclosure Statement"),
      notes = clean_text(paste(
        disclosure_matches[i, 3],
        disclosure_matches[i, 4],
        disclosure_matches[i, 5]
      )),
      last_seen_at = Sys.time(),
      source_page_url = url,
      source_page_modified = NA_character_
    )
  }) |>
    dplyr::filter(
      !stringr::str_detect(
        institution_name_raw,
        "Accreditation Actions|Public Disclosure Statements"
      )
    ) |>
    dplyr::distinct(
      accreditor,
      source_url,
      institution_name_normalized = normalize_name(institution_name_raw),
      action_label_raw,
      .keep_all = TRUE
    ) |>
    dplyr::select(-institution_name_normalized)
}

HLC_DETAIL_ACTION_PREFIX_PATTERN <- "^(Approved|Accepted|Affirmed|Denied|Placed|Continued|Removed|Withdrew|Withdrawn|Issued|Extended|Required)"
HLC_INSTITUTION_STATE_SUFFIX_PATTERN <- ",\\s*[A-Z]{2}$"

# Builds action rows for HLC from institution name, state, and action text strings.
build_hlc_action_rows <- function(inst_name, inst_state, actions, action_date, detail_url, detail_title, detail_modified) {
  if (is.null(inst_name) || !nzchar(inst_name)) {
    return(NULL)
  }
  actions <- clean_text(actions)
  actions <- actions[stringr::str_detect(actions, HLC_DETAIL_ACTION_PREFIX_PATTERN)]
  if (length(actions) == 0) {
    return(NULL)
  }
  purrr::map(actions, function(action_txt) {
    tibble::tibble(
      institution_name_raw = inst_name,
      institution_state_raw = inst_state,
      accreditor = "HLC",
      action_type = classify_action(action_txt, "HLC"),
      action_label_raw = action_txt,
      action_status = classify_status(action_txt),
      action_date = action_date,
      action_year = suppressWarnings(as.integer(format(action_date, "%Y"))),
      source_url = detail_url,
      source_title = detail_title,
      notes = action_txt,
      last_seen_at = Sys.time(),
      source_page_url = detail_url,
      source_page_modified = detail_modified
    )
  })
}

# Tracks the current institution while parsing HLC paragraphs, handling different text layouts.
# If a paragraph has a link, uses the link text as institution name; otherwise parses from paragraph.
update_hlc_current_institution <- function(p_text, p_link) {
  current_institution <- NULL
  current_state <- NULL
  action_text <- NULL

  if (!inherits(p_link, "xml_missing")) {
    inst_name <- unname(clean_text(xml2::xml_text(p_link)))
    rest_text <- clean_text(stringr::str_remove(p_text, paste0("^", stringr::fixed(inst_name), "\\s*,\\s*")))
    if (stringr::str_detect(rest_text, HLC_INSTITUTION_STATE_SUFFIX_PATTERN)) {
      parsed <- extract_name_state_from_item(rest_text)
      current_institution <- inst_name
      current_state <- unname(parsed$institution_state_raw)
    } else if (stringr::str_detect(p_text, HLC_INSTITUTION_STATE_SUFFIX_PATTERN)) {
      parsed <- extract_name_state_from_item(p_text)
      current_institution <- unname(parsed$institution_name_raw)
      current_state <- unname(parsed$institution_state_raw)
    } else {
      current_institution <- inst_name
      action_text <- unname(clean_text(stringr::str_remove(p_text, paste0("^", stringr::fixed(inst_name), "\\s*,\\s*"))))
    }
  } else if (stringr::str_detect(p_text, HLC_INSTITUTION_STATE_SUFFIX_PATTERN)) {
    parsed <- extract_name_state_from_item(p_text)
    current_institution <- unname(parsed$institution_name_raw)
    current_state <- unname(parsed$institution_state_raw)
  }

  list(
    institution = current_institution,
    state = current_state,
    action_text = action_text
  )
}

# Parses HLC detail page content nodes (p and ul elements), tracking institution context.
parse_hlc_content_nodes <- function(content_nodes, action_date, detail_url, detail_title, detail_modified) {
  current_institution <- NULL
  current_state <- NULL
  rows <- list()

  for (node in content_nodes) {
    node_name <- xml2::xml_name(node)
    if (identical(node_name, "p")) {
      p_text <- clean_text(xml2::xml_text(node))
      if (!nzchar(p_text)) next

      updated <- update_hlc_current_institution(
        p_text = p_text,
        p_link = xml2::xml_find_first(node, ".//a[contains(@href, '/institution/')]")
      )

      if (!is.null(updated$institution)) {
        current_institution <- updated$institution
      }
      if (!is.null(updated$state)) {
        current_state <- updated$state
      }
      if (!is.null(updated$action_text) && nzchar(updated$action_text)) {
        rows <- c(
          rows,
          build_hlc_action_rows(
            inst_name = current_institution,
            inst_state = current_state,
            actions = updated$action_text,
            action_date = action_date,
            detail_url = detail_url,
            detail_title = detail_title,
            detail_modified = detail_modified
          )
        )
      }
    } else if (identical(node_name, "ul")) {
      li_text <- xml2::xml_find_all(node, ".//li") |> xml2::xml_text() |> clean_text()
      rows <- c(
        rows,
        build_hlc_action_rows(
          inst_name = current_institution,
          inst_state = current_state,
          actions = li_text,
          action_date = action_date,
          detail_url = detail_url,
          detail_title = detail_title,
          detail_modified = detail_modified
        )
      )
    }
  }

  dplyr::bind_rows(rows)
}

# Pure helper: extracts (action_date, action_body) rows from a single
# MSCHE per-institution page's HTML by iterating <ul id="accreditation_actions">/li.
# Returns an empty tibble (with correct column types) if the container is
# missing or every <li> fails to parse. Module-level so it can be tested
# in isolation without staging cache files.
extract_msche_institution_actions <- function(html) {
  empty <- tibble::tibble(action_date = as.Date(character()), action_body = character())
  if (is.null(html) || !nzchar(html)) return(empty)
  doc <- tryCatch(xml2::read_html(html), error = function(e) NULL)
  if (is.null(doc)) return(empty)
  ul <- xml2::xml_find_first(doc, "//ul[@id='accreditation_actions']")
  if (inherits(ul, "xml_missing")) return(empty)
  li_nodes <- xml2::xml_find_all(ul, "./li")
  if (length(li_nodes) == 0L) return(empty)

  rows <- purrr::map(li_nodes, function(li) {
    strong_node <- xml2::xml_find_first(li, "./strong")
    if (inherits(strong_node, "xml_missing")) return(NULL)
    date_text <- clean_text(xml2::xml_text(strong_node))
    parsed_date <- suppressWarnings(as.Date(date_text, format = MSCHE_INSTITUTION_DATE_FORMAT))
    if (is.na(parsed_date)) return(NULL)
    # xml_text on the full <li> returns the visible text including the date
    # prefix. Strip the leading date so action_body is the action sentence
    # alone; xml2 already collapses tag boundaries and decodes entities.
    full_text <- clean_text(xml2::xml_text(li))
    body_text <- stringr::str_remove(
      full_text,
      paste0("^", stringr::fixed(date_text), "\\s*")
    )
    body_text <- stringr::str_squish(body_text)
    if (!nzchar(body_text)) return(NULL)
    tibble::tibble(action_date = parsed_date, action_body = body_text)
  })
  rows <- purrr::compact(rows)
  if (length(rows) == 0L) return(empty)
  dplyr::bind_rows(rows)
}

# Pure helper: returns the highest paged-URL number referenced in a
# per-institution page (i.e., total page count). Defaults to 1 when no
# pagination block is present (institution has <= 10 actions).
discover_msche_institution_page_count <- function(html) {
  if (is.null(html) || !nzchar(html)) return(1L)
  matches <- stringr::str_match_all(html, MSCHE_INSTITUTION_PAGINATION_PATTERN)[[1]]
  if (nrow(matches) == 0L) return(1L)
  page_nums <- suppressWarnings(as.integer(matches[, 2]))
  page_nums <- page_nums[!is.na(page_nums)]
  if (length(page_nums) == 0L) return(1L)
  as.integer(max(c(1L, page_nums)))
}

# Fetches and parses every paginated page of a single MSCHE per-institution
# page, returning one row per board action conforming to
# ACCREDITATION_ACTION_COLUMNS (action_scope = NA, accreditor = "MSCHE").
# Returns NULL when the page-1 fetch fails or yields zero parseable
# actions; callers should fall back to the monthly-index "Commission
# action" stub on NULL.
#
# Caching: page 1 is cached as `msche_institution_<id>.html`; pagination
# pages 2+ are cached as `msche_institution_<id>_p<N>.html`. Cache id is
# the numeric MSCHE institution id parsed from the URL, with a
# normalized-name fallback when the URL doesn't match the expected shape.
# A 0.5s sleep between pagination fetches mirrors parse_nwccu's rate
# limit.
parse_msche_institution_page <- function(institution_url,
                                         institution_name,
                                         institution_state,
                                         cache_dir,
                                         refresh = TRUE) {
  if (is.null(institution_url) || is.na(institution_url) || !nzchar(institution_url)) {
    return(NULL)
  }
  cache_id_match <- stringr::str_match(institution_url, MSCHE_INSTITUTION_PATH_PATTERN)
  cache_id <- if (!is.na(cache_id_match[1, 2])) {
    cache_id_match[1, 2]
  } else {
    normalize_name(institution_name %||% "unknown")
  }

  page_one_cache <- paste0("msche_institution_", cache_id, ".html")
  page_one_html <- tryCatch(
    fetch_html_text(institution_url, page_one_cache, cache_dir, refresh = refresh),
    error = function(e) NULL
  )
  if (is.null(page_one_html) || nchar(page_one_html) < 200L) return(NULL)

  page_one_modified <- extract_page_modified_date(page_one_html)
  page_count <- discover_msche_institution_page_count(page_one_html)

  all_actions <- extract_msche_institution_actions(page_one_html)
  if (page_count > 1L) {
    base_url <- sub("/$", "", institution_url)
    for (n in seq.int(2L, page_count)) {
      Sys.sleep(0.5)
      page_url <- paste0(base_url, "?ipf_action_paged=", n)
      page_cache <- paste0("msche_institution_", cache_id, "_p", n, ".html")
      page_html <- tryCatch(
        fetch_html_text(page_url, page_cache, cache_dir, refresh = refresh),
        error = function(e) NULL
      )
      if (is.null(page_html) || nchar(page_html) < 200L) next
      all_actions <- dplyr::bind_rows(
        all_actions,
        extract_msche_institution_actions(page_html)
      )
    }
  }

  if (nrow(all_actions) == 0L) return(NULL)

  # Dedupe across pages by (date, body). Some sites occasionally repeat
  # the page-1 list when ipf_action_paged exceeds the real count; the
  # explicit dedupe protects against that without changing semantics
  # when pagination is well-behaved.
  all_actions <- dplyr::distinct(all_actions, action_date, action_body, .keep_all = TRUE)

  tibble::tibble(
    institution_name_raw = institution_name,
    institution_state_raw = institution_state,
    accreditor = "MSCHE",
    action_type = classify_action(all_actions$action_body, "MSCHE"),
    action_label_raw = all_actions$action_body,
    action_status = classify_status(all_actions$action_body),
    action_date = all_actions$action_date,
    action_year = as.integer(format(all_actions$action_date, "%Y")),
    action_scope = NA_character_,
    source_url = institution_url,
    source_title = paste("MSCHE Statement of Accreditation Status -", institution_name),
    notes = all_actions$action_body,
    # Match the POSIXct shape every other scraper emits (parse_items_to_rows,
    # parse_sacscoc_*, build_hlc_action_rows). ensure_accreditation_action_schema
    # coerces last_seen_at to character at the boundary; emitting character
    # here breaks dplyr::bind_rows when combining with current_status_rows
    # (which uses bare Sys.time()).
    last_seen_at = Sys.time(),
    source_page_url = institution_url,
    source_page_modified = page_one_modified
  )
}

# Scrapes MSCHE accreditation data from two sources: current non-compliance status
# (from a single status page with H3 sections) and recent commission actions
# (by iterating monthly pages, then fetching each unique institution's per-institution
# page for the actual board-action sentences). The monthly index alone gives only
# institution name + month; the per-institution page is the source of truth for
# action text and date.
parse_msche <- function(cache_dir, refresh) {
  url <- MSCHE_CURRENT_STATUS_URL
  html <- fetch_html_text(url, "msche_status.html", cache_dir, refresh = refresh)
  page_title <- extract_page_title(html)
  page_modified <- extract_page_modified_date(html)

  section_matches <- stringr::str_match_all(
    html,
    "(?s)<h3>(Non-Compliance Warning|Non-Compliance Probation|Non-Compliance Show Cause|Adverse Action)</h3>(.*?)(?=<h3>|<div class=\"single-share\"|</article>)"
  )[[1]]

  # Guard: if the page returned non-empty HTML but none of the expected H3
  # action headings are present, the page was likely rendered by JavaScript and
  # httr2 received only the pre-JS shell.  Emit a warning so callers know the
  # 0-row result is suspicious rather than a genuine "no actions" outcome.
  if (nrow(section_matches) == 0 && nzchar(trimws(html))) {
    has_any_heading <- any(vapply(
      names(MSCHE_CURRENT_STATUS_ACTION_TYPES),
      function(lbl) grepl(lbl, html, fixed = TRUE),
      logical(1)
    ))
    if (!has_any_heading) {
      warning(sprintf(
        paste(
          "parse_msche: fetched %s but found none of the expected H3 action",
          "headings (%s). The page may be JavaScript-rendered and unavailable",
          "to httr2. Returning 0 current-status rows \u2014 validate output."
        ),
        url,
        paste(names(MSCHE_CURRENT_STATUS_ACTION_TYPES), collapse = ", ")
      ))
    }
  }

  current_status_rows <- if (nrow(section_matches) == 0) {
    tibble::tibble()
  } else {
    purrr::map_dfr(seq_len(nrow(section_matches)), function(i) {
      heading <- clean_text(section_matches[i, 2])
      body <- section_matches[i, 3]
      if (stringr::str_detect(body, "No institutions in this status")) {
        return(tibble::tibble())
      }

      link_matches <- stringr::str_match_all(body, "<a[^>]*href=\"([^\"]+)\"[^>]*>(.*?)</a>")[[1]]
      if (nrow(link_matches) == 0) {
        return(tibble::tibble())
      }

      action_label <- lookup_or_default(heading, MSCHE_CURRENT_STATUS_ACTION_TYPES, default = "other")

      tibble::tibble(
        institution_name_raw = clean_text(link_matches[, 3]),
        institution_state_raw = NA_character_,
        accreditor = "MSCHE",
        action_type = action_label,
        action_label_raw = heading,
        action_status = "active",
        action_date = as.Date(NA),
        action_year = NA_integer_,
        source_url = link_matches[, 2],
        source_title = page_title,
        notes = heading,
        last_seen_at = Sys.time(),
        source_page_url = url,
        source_page_modified = page_modified
      )
    })
  }

  # Discovery-only: returns one row per (institution, month) tuple seen on
  # this monthly index page. No action text is set here -- the per-institution
  # parser (called below) is the source of truth for action label, type, and
  # date. The monthly index date is preserved as `monthly_action_date` for
  # use as a stub fallback when the per-institution fetch fails.
  parse_msche_month_page <- function(month_url, month_label) {
    cache_name <- paste0("msche_", gsub("[^a-z0-9]+", "_", tolower(month_label)), ".html")
    month_html <- fetch_html_text(month_url, cache_name, cache_dir, refresh = refresh)
    page_modified_month <- extract_page_modified_date(month_html)
    doc <- xml2::read_html(month_html)

    state_sections <- xml2::xml_find_all(doc, "//h3[starts-with(@id,'state-')]")
    if (length(state_sections) == 0) {
      return(tibble::tibble())
    }

    purrr::map_dfr(state_sections, function(state_node) {
      state_label <- clean_text(xml2::xml_text(state_node))
      list_node <- xml2::xml_find_first(state_node, "following-sibling::*[1][self::ul]")
      if (inherits(list_node, "xml_missing")) {
        return(tibble::tibble())
      }

      item_nodes <- xml2::xml_find_all(list_node, "./li[a[contains(@href, '/institution/')]]")
      if (length(item_nodes) == 0) {
        return(tibble::tibble())
      }

      purrr::map_dfr(item_nodes, function(item_node) {
        inst_link <- xml2::xml_find_first(item_node, ".//a[contains(@href, '/institution/')]")
        inst_name_raw <- clean_text(xml2::xml_text(inst_link))
        inst_name_clean <- stringr::str_remove(inst_name_raw, ",\\s*[A-Z]{2}$")
        date_text <- clean_text(xml2::xml_text(item_node))
        date_match <- stringr::str_match(date_text, "\\((January|February|March|April|May|June|July|August|September|October|November|December)\\s+[0-9]{1,2},\\s+[0-9]{4}\\)")
        entry_date <- suppressWarnings(as.Date(stringr::str_remove_all(date_match[, 1], "[()]"), format = "%B %d, %Y"))

        tibble::tibble(
          institution_url = xml2::xml_attr(inst_link, "href"),
          institution_name_raw = inst_name_clean,
          institution_state_raw = state_name(state_label),
          source_page_url = month_url,
          source_page_modified = page_modified_month,
          month_label = month_label,
          monthly_action_date = entry_date
        )
      })
    })
  }

  # Discovery-only: walks the per-year monthly index pages from 2017 to
  # present and returns the union of institution-discovery tuples emitted
  # by parse_msche_month_page across all months. Deduping happens in the
  # caller (build_msche_per_institution_rows) since it needs the full
  # tuple list to construct stub fallbacks per month.
  parse_msche_recent_actions <- function() {
    years <- seq.int(2017L, max(2017L, as.integer(format(Sys.Date(), "%Y"))))

    month_links <- purrr::map_dfr(years, function(one_year) {
      recent_url <- if (one_year == as.integer(format(Sys.Date(), "%Y"))) {
        MSCHE_RECENT_ACTIONS_URL
      } else {
        paste0(MSCHE_RECENT_ACTIONS_URL, "?my=", one_year)
      }

      recent_html <- fetch_html_text(
        recent_url,
        paste0("msche_recent_actions_", one_year, ".html"),
        cache_dir,
        refresh = refresh
      )

      links <- stringr::str_match_all(
        recent_html,
        MSCHE_RECENT_ACTIONS_LINK_PATTERN
      )[[1]]

      if (nrow(links) == 0) {
        return(tibble::tibble())
      }

      tibble::tibble(
        month_url = gsub("&amp;", "&", links[, 2], fixed = TRUE),
        month_label = paste(links[, 3], links[, 4])
      )
    }) |>
      dplyr::distinct()

    if (nrow(month_links) == 0) {
      return(tibble::tibble())
    }

    purrr::map_dfr(seq_len(nrow(month_links)), function(i) {
      parse_msche_month_page(month_links$month_url[[i]], month_links$month_label[[i]])
    })
  }

  # Constructs the stub fallback rows for one institution when its
  # per-institution page fetch/parse fails. One stub row per (institution,
  # month) tuple from the discoveries -- preserves the existing
  # "Commission action" shape so coverage doesn't disappear when the
  # institution page is unavailable.
  build_stub_fallback <- function(inst_discoveries) {
    tibble::tibble(
      institution_name_raw = inst_discoveries$institution_name_raw,
      institution_state_raw = inst_discoveries$institution_state_raw,
      accreditor = "MSCHE",
      action_type = "commission_action",
      action_label_raw = "Commission action",
      action_status = NA_character_,
      action_date = inst_discoveries$monthly_action_date,
      action_year = suppressWarnings(as.integer(format(inst_discoveries$monthly_action_date, "%Y"))),
      action_scope = NA_character_,
      source_url = inst_discoveries$institution_url,
      source_title = paste("MSCHE Statement of Accreditation Status -", inst_discoveries$institution_name_raw),
      notes = paste("Recent Commission Action:", inst_discoveries$month_label),
      # Match the POSIXct shape used by the other branch + every other
      # scraper. Schema helper coerces to character at the boundary.
      last_seen_at = Sys.time(),
      source_page_url = inst_discoveries$source_page_url,
      source_page_modified = inst_discoveries$source_page_modified
    )
  }

  # Orchestrator: dedupe institutions across the monthly index, fetch
  # each per-institution page once (with pagination), and emit the full
  # per-action history. Falls back to the monthly-index "Commission
  # action" stubs when a per-institution page can't be fetched or yields
  # zero parseable actions.
  build_msche_per_institution_rows <- function(discoveries) {
    if (is.null(discoveries) || nrow(discoveries) == 0L) {
      return(tibble::tibble())
    }
    unique_inst <- discoveries |>
      dplyr::filter(!is.na(institution_url) & nzchar(institution_url)) |>
      dplyr::distinct(institution_url, .keep_all = TRUE)
    if (nrow(unique_inst) == 0L) return(tibble::tibble())

    message(sprintf(
      "  MSCHE: parsing %d unique institution pages (per-action enrichment)...",
      nrow(unique_inst)
    ))

    purrr::map_dfr(seq_len(nrow(unique_inst)), function(i) {
      Sys.sleep(0.5)  # rate limit per institution; pagination adds its own
      r <- unique_inst[i, ]
      result <- tryCatch(
        parse_msche_institution_page(
          institution_url = r$institution_url,
          institution_name = r$institution_name_raw,
          institution_state = r$institution_state_raw,
          cache_dir = cache_dir,
          refresh = refresh
        ),
        error = function(e) {
          warning(sprintf(
            "MSCHE: per-institution parse errored for %s: %s",
            r$institution_url, conditionMessage(e)
          ), call. = FALSE)
          NULL
        }
      )
      if (is.null(result) || nrow(result) == 0L) {
        fallback <- discoveries |>
          dplyr::filter(institution_url == r$institution_url)
        warning(sprintf(
          "MSCHE: per-institution parse returned no actions for %s -- falling back to %d stub row(s)",
          r$institution_url, nrow(fallback)
        ), call. = FALSE)
        return(build_stub_fallback(fallback))
      }
      result
    })
  }

  discoveries <- parse_msche_recent_actions()
  per_institution_rows <- build_msche_per_institution_rows(discoveries)

  dplyr::bind_rows(current_status_rows, per_institution_rows) |>
    dplyr::distinct()
}

# Scrapes HLC accreditation data from two sources: current public disclosure notices
# (from accordion panels on the main page) and historical action detail pages
# (links to monthly action pages from 2024 onward).
parse_hlc <- function(cache_dir, refresh) {
  url <- HLC_ACTIONS_URL
  html <- fetch_html_text(url, "hlc_actions.html", cache_dir, refresh = refresh)
  page_title <- extract_page_title(html)
  page_modified <- extract_page_modified_date(html)

  current_notice_block <- stringr::str_match(
    html,
    HLC_CURRENT_NOTICE_BLOCK_PATTERN
  )[, 2]

  current_notice_rows <- if (is.na(current_notice_block)) {
    tibble::tibble()
  } else {
    pane_matches <- stringr::str_match_all(
      current_notice_block,
      HLC_NOTICE_PANE_PATTERN
    )[[1]]

    if (nrow(pane_matches) == 0) {
      tibble::tibble()
    } else {
      purrr::map_dfr(seq_len(nrow(pane_matches)), function(i) {
        heading <- clean_text(pane_matches[i, 2])
        body <- pane_matches[i, 3]
        li_matches <- stringr::str_match_all(
          body,
          "(?s)<li[^>]*>.*?<a[^>]*href=\"([^\"]+)\"[^>]*>(.*?)</a>\\s*(?:&nbsp;)?\\s*\\(([^)]+)\\).*?</li>"
        )[[1]]

        if (nrow(li_matches) == 0) {
          return(tibble::tibble())
        }

        raw_action <- lookup_or_default(heading, HLC_CURRENT_NOTICE_ACTION_LABELS)

        tibble::tibble(
          institution_name_raw = clean_text(li_matches[, 3]),
          institution_state_raw = clean_text(li_matches[, 4]),
          accreditor = "HLC",
          action_type = classify_action(raw_action),
          action_label_raw = raw_action,
          action_status = classify_status(raw_action),
          action_date = as.Date(NA),
          action_year = NA_integer_,
          source_url = li_matches[, 2],
          source_title = paste(page_title, "-", heading),
          notes = heading,
          last_seen_at = Sys.time(),
          source_page_url = url,
          source_page_modified = page_modified
        )
      })
    }
  }

  # Parses an HLC monthly action detail page, extracting institution names and their actions.
  parse_hlc_detail_page <- function(detail_url) {
    detail_html <- fetch_html_text(
      detail_url,
      paste0("hlc_", basename(gsub("/$", "", detail_url)), ".html"),
      cache_dir,
      refresh = refresh
    )
    detail_title <- extract_page_title(detail_html)
    detail_modified <- extract_page_modified_date(detail_html)
    doc <- xml2::read_html(detail_html)

    date_match <- stringr::str_match(
      clean_text(detail_title),
      "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+([0-9]{4})"
    )
    action_date <- if (!is.na(date_match[1, 1])) {
      as.Date(paste0(date_match[1, 2], " 01 ", date_match[1, 3]), format = "%B %d %Y")
    } else {
      as.Date(NA)
    }

    content_nodes <- xml2::xml_find_all(doc, "//*[contains(@class,'entry-content')]//*[self::p or self::ul]")
    if (length(content_nodes) == 0) {
      return(tibble::tibble())
    }
    parse_hlc_content_nodes(
      content_nodes = content_nodes,
      action_date = action_date,
      detail_url = detail_url,
      detail_title = detail_title,
      detail_modified = detail_modified
    )
  }

  link_matches <- stringr::str_match_all(
    html,
    HLC_DETAIL_LINK_PATTERN
  )[[1]]

  historical_rows <- if (nrow(link_matches) == 0) {
    tibble::tibble()
  } else {
    detail_urls <- unique(link_matches[as.integer(link_matches[, 4]) >= 2024, 2])
    purrr::map_dfr(detail_urls, parse_hlc_detail_page)
  }

  combined <- dplyr::bind_rows(current_notice_rows, historical_rows) |>
    dplyr::distinct()
  warn_on_empty_parse("HLC", url, combined, html)
  combined
}

# Scrapes a SACSCOC detail page, extracting both sanction actions and public disclosure statements.
parse_sacscoc_detail_page <- function(url, cache_dir, refresh) {
  html <- fetch_html_text(
    url,
    paste0("sacscoc_", basename(gsub("/$", "", url)), ".html"),
    cache_dir,
    refresh = refresh
  )
  page_title <- extract_page_title(html)
  page_modified <- extract_page_modified_date(html)

  date_match <- stringr::str_match(clean_text(page_title), "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+([0-9]{4})")
  action_date <- if (!is.na(date_match[1, 1])) {
    as.Date(paste0(date_match[1, 2], " 01 ", date_match[1, 3]), format = "%B %d %Y")
  } else {
    as.Date(NA)
  }

  li_matches <- stringr::str_match_all(html, "(?s)<li>(.*?)</li>")[[1]]
  sanction_rows <- if (nrow(li_matches) == 0) {
    tibble::tibble()
  } else {
    raw_items <- clean_text(li_matches[, 2])
    raw_items <- raw_items[stringr::str_detect(raw_items, SACSCOC_SANCTION_KEYWORD_PATTERN)]
    purrr::map_dfr(raw_items, parse_sacscoc_sanction_item, action_date = action_date, url = url, page_title = page_title)
  }

  disclosure_matches <- stringr::str_match_all(html, SACSCOC_DISCLOSURE_ITEM_PATTERN)[[1]]
  disclosure_rows <- build_sacscoc_disclosure_rows(
    disclosure_matches,
    action_date = action_date,
    url = url,
    page_title = page_title,
    cache_dir = cache_dir,
    refresh = refresh
  )

  combined <- dplyr::bind_rows(sanction_rows, disclosure_rows) |>
    dplyr::distinct()

  if (nrow(combined) > 0 && !is.na(page_modified)) {
    combined$source_page_modified <- page_modified
  }

  combined
}

# Scrapes SACSCOC by first finding links to June/December action pages from the landing page,
# then fetching each detail page to extract sanctions and disclosure statements.
parse_sacscoc <- function(cache_dir, refresh) {
  landing_url <- SACSCOC_LANDING_URL
  html <- fetch_html_text(landing_url, "sacscoc_disclosures.html", cache_dir, refresh = refresh)

  links <- stringr::str_match_all(
    html,
    SACSCOC_LANDING_LINK_PATTERN
  )[[1]]

  if (nrow(links) == 0) {
    warn_on_empty_parse(
      "SACSCOC", landing_url, tibble::tibble(), html,
      detail = "no detail-page links found on landing page"
    )
    return(tibble::tibble())
  }

  detail_urls <- unique(links[, 2])
  rows <- purrr::map_dfr(detail_urls, function(u) parse_sacscoc_detail_page(u, cache_dir, refresh))
  warn_on_empty_parse("SACSCOC", landing_url, rows, html)
  rows
}

# Scrapes NECHE accreditation actions from their recent actions page, extracting
# institutions grouped by action type from toggle/accordion sections. The page
# is first split into meeting sections by H2 headings so that each row inherits
# its meeting's action date (rather than the page-last-modified date).
parse_neche <- function(cache_dir, refresh) {
  url <- NECHE_ACTIONS_URL
  html <- fetch_html_text(url, "neche_actions.html", cache_dir, refresh = refresh)
  page_title <- extract_page_title(html)
  page_modified <- extract_page_modified_date(html)

  meeting_sections <- extract_tag_heading_sections(html, heading_tag = "h2")

  rows <- purrr::map_dfr(seq_len(nrow(meeting_sections)), function(i) {
    heading <- meeting_sections$heading[[i]]
    body <- meeting_sections$body[[i]]

    date_match <- stringr::str_match(heading, NECHE_MEETING_DATE_PATTERN)
    meeting_date <- if (is.na(date_match[1, 1])) {
      as.Date(NA)
    } else {
      as.Date(
        paste(date_match[1, 4], date_match[1, 3], date_match[1, 2]),
        format = "%Y %d %B"
      )
    }
    meeting_year <- suppressWarnings(as.integer(format(meeting_date, "%Y")))

    toggle_sections <- extract_regex_heading_sections(body, NECHE_TOGGLE_SECTION_PATTERN)
    parse_public_action_sections(
      sections = toggle_sections,
      accreditor = "NECHE",
      action_date = meeting_date,
      action_year = meeting_year,
      source_url = url,
      source_title = page_title,
      source_page_url = url,
      source_page_modified = page_modified
    )
  })

  warn_on_empty_parse(
    "NECHE", url, rows, html,
    detail = if (nrow(meeting_sections) == 0L) "no meeting H2 headings matched on page" else NULL
  )
  rows
}

parse_wscuc_detail_page <- function(url, cache_dir, refresh) {
  html <- fetch_html_text(
    url,
    paste0("wscuc_", basename(gsub("/$", "", url)), ".html"),
    cache_dir,
    refresh = refresh
  )
  page_title <- extract_page_title(html)
  page_modified <- extract_page_modified_date(html)

  date_match <- stringr::str_match(
    clean_text(page_title),
    "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+([0-9]{4})"
  )
  action_date <- if (!is.na(date_match[1, 1])) {
    as.Date(paste0(date_match[1, 2], " 01 ", date_match[1, 3]), format = "%B %d %Y")
  } else {
    as.Date(NA)
  }

  sections <- extract_tag_heading_sections(html, heading_tag = "h2")
  parse_public_action_sections(
    sections = sections,
    accreditor = "WSCUC",
    action_date = action_date,
    action_year = suppressWarnings(as.integer(format(action_date, "%Y"))),
    source_url = url,
    source_title = page_title,
    source_page_url = url,
    source_page_modified = page_modified
  )
}

parse_wscuc <- function(cache_dir, refresh) {
  # Collect both the detail URLs and the archive HTML sizes so the per-site
  # empty guard can distinguish between "archives were blank (expected 0)"
  # and "archives were substantive but no detail links matched (suspicious)".
  archive_scan <- purrr::map(seq_along(WSCUC_ARCHIVE_URLS), function(i) {
    archive_html <- fetch_html_text(
      WSCUC_ARCHIVE_URLS[[i]],
      paste0("wscuc_commission_actions_archive_", i, ".html"),
      cache_dir,
      refresh = refresh
    )

    links <- stringr::str_match_all(
      archive_html,
      WSCUC_DETAIL_LINK_PATTERN
    )[[1]]

    list(
      urls = if (nrow(links) == 0) character() else links[, 2],
      html = archive_html
    )
  })
  combined_html <- paste(vapply(archive_scan, function(x) {
    if (is.null(x$html)) "" else as.character(x$html)[[1L]]
  }, character(1)), collapse = "")

  detail_urls <- tibble::tibble(url = unlist(lapply(archive_scan, `[[`, "urls"))) |>
    dplyr::distinct() |>
    dplyr::filter(stringr::str_detect(url, "20(24|25|26)")) |>
    dplyr::pull(url)

  if (length(detail_urls) == 0) {
    warn_on_empty_parse(
      "WSCUC", WSCUC_ARCHIVE_URLS[[1]], tibble::tibble(), combined_html,
      detail = "no detail-page links matched in any commission-actions archive"
    )
    return(tibble::tibble())
  }

  rows <- purrr::map_dfr(detail_urls, function(u) parse_wscuc_detail_page(u, cache_dir, refresh))
  warn_on_empty_parse("WSCUC", WSCUC_ARCHIVE_URLS[[1]], rows, combined_html)
  rows
}

build_match_suggestions <- function(unmatched_df, candidates_df, max_candidates = 3L) {
  if (nrow(unmatched_df) == 0) {
    return(unmatched_df)
  }

  purrr::map_dfr(seq_len(nrow(unmatched_df)), function(i) {
    row <- unmatched_df[i, ]
    same_state <- candidates_df
    if (!is.na(row$institution_state_normalized) && nzchar(row$institution_state_normalized)) {
      same_state <- candidates_df |>
        dplyr::filter(state_full == row$institution_state_normalized)
    }
    pool <- if (nrow(same_state) > 0) same_state else candidates_df
    dists <- utils::adist(row$institution_name_normalized, pool$norm_name)
    ord <- order(dists[1, ], pool$institution_name)
    top_n <- head(ord, max_candidates)
    suggestions <- paste0(
      pool$institution_name[top_n],
      " (",
      pool$state_full[top_n],
      "; UNITID ",
      pool$unitid[top_n],
      "; distance ",
      as.integer(dists[1, top_n]),
      ")"
    )
    dplyr::bind_cols(
      row,
      tibble::tibble(suggested_matches = paste(suggestions, collapse = " | "))
    )
  })
}

# ---------------------------------------------------------------------------
# NWCCU (Northwest Commission on Colleges and Universities)
# ---------------------------------------------------------------------------

NWCCU_DIRECTORY_URL <- "https://nwccu.org/institutional-directory/"
NWCCU_BASE_URL      <- "https://nwccu.org"

# CSS class on each <article class="institution-item ..."> entry that flags
# institutions granting bachelor's-equivalent degrees. NWCCU renamed this
# class from "nwccu-degree-bachelor" -> "nwccu-degree-baccalaureate"
# sometime in 2025/26, which silently dropped parse_nwccu's row count to
# zero (the older selector matched nothing on the new HTML, and NWCCU was
# in ZERO_IS_EXPECTED so the CI gate didn't fail closed). Hoisted into a
# constant so the warning string and the filter stay in sync.
NWCCU_BACCALAUREATE_CLASS <- "nwccu-degree-baccalaureate"

# Regex patterns derived from the live HTML structure (2025).
# article class: "institution-item nwccu-state-XX nwccu-degree-baccalaureate ..."
NWCCU_ARTICLE_PATTERN  <- "(?s)<article[^>]+class=\"institution-item([^\"]*?)\"[^>]*>(.*?)</article>"
NWCCU_LINK_PATTERN     <- "href=\"(https://nwccu\\.org/institutional-directory/[^\"]+/)\""
NWCCU_NOTIF_PATTERN    <- "href=\"(https://nwccu\\.(?:box|app\\.box)\\.com/[^\"]+)\"[^>]*>[^<]*Institution Notification Letter"
NWCCU_NOTIF_PATTERN2   <- "Institution Notification Letter[^<]*</[^>]+>[\\s\\S]{0,200}href=\"(https://(?:nwccu\\.box\\.com|app\\.box\\.com)/[^\"]+)\""
NWCCU_STATUS_PATTERN   <- "Current Accreditation Status</span>\\s*<p[^>]*>([^<]+)</p>"
NWCCU_EVAL_PATTERN     <- "Most Recent Evaluation</span>\\s*<span[^>]*>([^<]+)</span>"
NWCCU_REASON_PATTERN   <- "Reason for Accreditation</span>\\s*<span[^>]*>([^<]+)</span>"

# Keywords that indicate an actionable adverse finding in page text or eval text
NWCCU_ADVERSE_KEYWORDS <- paste(
  c("show cause", "warning", "probation", "notice of concern",
    "notice of warning", "summary suspension", "denial of accreditation",
    "withdrawal of accreditation", "terminate", "termination",
    "loses", "loss of", "no longer", "current accreditation status is"),
  collapse = "|"
)

parse_nwccu_institution_page <- function(inst_url, inst_name, cache_dir, refresh = FALSE) {
  cache_name <- paste0("nwccu_", gsub("[^a-z]", "_", basename(sub("/$", "", inst_url))), ".html")
  html <- fetch_html_text(inst_url, cache_name, cache_dir, refresh = refresh)
  if (is.null(html) || nchar(html) < 100) {
    return(tibble::tibble())
  }

  # Extract status, evaluation, reason, and notification link
  status_match   <- stringr::str_match(html, NWCCU_STATUS_PATTERN)
  eval_match     <- stringr::str_match(html, NWCCU_EVAL_PATTERN)
  reason_match   <- stringr::str_match(html, NWCCU_REASON_PATTERN)

  # Notification letter link (preferred) or fallback to status-derived
  notif_match  <- stringr::str_match(html, NWCCU_NOTIF_PATTERN)
  if (is.na(notif_match[1, 2])) {
    notif_match <- stringr::str_match(html, NWCCU_NOTIF_PATTERN2)
  }

  status <- if (!is.na(status_match[1, 2])) status_match[1, 2] else NA
  eval    <- if (!is.na(eval_match[1, 2]))     eval_match[1, 2]     else NA
  reason  <- if (!is.na(reason_match[1, 2])) reason_match[1, 2]   else NA

  # Check for adverse keywords in the main text. The earlier implementation
  # scanned the entire raw HTML, which silently flagged every NWCCU
  # institution page as adverse: inline <script> on every page contains the
  # Web Worker call `r.terminate()` (matches "terminate"), and a featured
  # news item carries the phrase "is no longer a desired trait" (matches
  # "no longer"). With the directory selector now correctly returning 134
  # baccalaureate institutions, that wide scan would emit 134 false-positive
  # adverse rows. Restrict the scan to the parsed text content of <main>,
  # excluding <script>/<style>, so only actual page copy (status field,
  # eval text, "Current Sanctions" sections, etc.) is considered.
  scan_text <- tryCatch(
    {
      doc <- xml2::read_html(html)
      main_node <- xml2::xml_find_first(doc, "//main")
      if (inherits(main_node, "xml_missing")) {
        # Fall back to the full body if the page lacks a <main>; still strip
        # script/style so JS doesn't bleed into the scan.
        main_node <- xml2::xml_find_first(doc, "//body")
      }
      if (!inherits(main_node, "xml_missing")) {
        for (junk in xml2::xml_find_all(main_node, ".//script|.//style")) {
          xml2::xml_remove(junk)
        }
        gsub("\\s+", " ", tolower(xml2::xml_text(main_node)))
      } else {
        ""
      }
    },
    error = function(e) ""
  )
  has_adverse_keyword <- nzchar(scan_text) &&
    stringr::str_detect(scan_text, NWCCU_ADVERSE_KEYWORDS)

  # Determine action type from status and keywords
  action_type <- "other"
  if (!is.na(status)) {
    status_lower <- tolower(status)
    if (stringr::str_detect(status_lower, "show cause|probation|warning|suspension")) {
      action_type <- "warning"
    } else if (stringr::str_detect(status_lower, "denial|withdraw|terminate")) {
      action_type <- "termination"
    }
  }
  if (has_adverse_keyword && action_type == "other") {
    action_type <- "warning"
  }

  # Skip if not an adverse action
  if (action_type == "other") {
    return(tibble::tibble())
  }

  source_url <- if (!is.na(notif_match[1, 2])) notif_match[1, 2] else inst_url
  notes <- paste(na.omit(c(
    if (!is.na(status)) paste("Status:", status) else NA_character_,
    if (!is.na(eval)) paste("Evaluation:", eval) else NA_character_,
    if (!is.na(reason)) paste("Reason:", reason) else NA_character_
  )), collapse = " | ")

  # Pull the page-modified date from the institution page so downstream
  # date-rescue paths in build_web_exports.R can backfill action_date
  # from source_page_modified. NWCCU institution pages carry an Open
  # Graph article:modified_time meta tag (verified across the cached
  # corpus) that extract_page_modified_date already understands.
  page_modified <- extract_page_modified_date(html)

  tibble::tibble(
    institution_name_raw = inst_name,
    institution_state_raw = NA_character_,
    accreditor            = "NWCCU",
    action_type           = action_type,
    action_label_raw      = paste0(ifelse(!is.na(status), status, "Accreditation action"), ifelse(!is.na(eval), paste0(" – ", eval), "")),
    # build_web_exports.R's date-rescue path (action_date backfill from
    # source_page_modified) only fires when action_status == "active". The
    # structured NWCCU status string ("Accredited", "Probation", etc.) is
    # the institution's overall accreditation status -- not the status of
    # this specific row's adverse signal. We've already classified the
    # action as adverse (action_type != "other") to reach this point, so
    # mark the row as actively flagged. The original status string moves
    # to notes (already captured above) where the audit trail is preserved.
    action_status         = "active",
    # Date type must match the other scrapers (NECHE/HLC/SACSCOC/WSCUC all emit
    # `as.Date(NA)` when no per-row date is available). Emitting NA_character_
    # here breaks dplyr::bind_rows in build_accreditation_actions.R with
    # "Can't combine action_date <date> and action_date <character>". This was
    # masked while parse_nwccu returned zero rows from the broken
    # nwccu-degree-bachelor selector.
    action_date           = as.Date(NA),
    action_year           = NA_integer_,
    source_page_url       = inst_url,
    source_title          = paste0("NWCCU Institution Notification Letter – ", inst_name),
    source_url            = source_url,
    notes                 = notes,
    last_seen_at          = as.character(Sys.Date()),
    source_page_modified  = page_modified
  )
}

parse_nwccu <- function(cache_dir, refresh = FALSE) {
  dir_html <- fetch_html_text(NWCCU_DIRECTORY_URL, "nwccu_directory.html", cache_dir, refresh = refresh)
  if (is.null(dir_html) || nchar(dir_html) < 100) {
    message("NWCCU: could not fetch directory")
    return(tibble::tibble())
  }

  # Extract baccalaureate-granting institutions. The class flag is hoisted
  # into NWCCU_BACCALAUREATE_CLASS so a future schema rename (like the 2025
  # `bachelor` -> `baccalaureate` rename that caused this scraper to silently
  # drop to zero rows) only requires updating the constant.
  article_matches <- stringr::str_match_all(dir_html, NWCCU_ARTICLE_PATTERN)[[1]]
  institutions <- tibble::tibble(
    article_class = article_matches[, 2],
    article_html  = article_matches[, 3]
  )
  institutions <- dplyr::filter(
    institutions,
    stringr::str_detect(article_class, stringr::fixed(NWCCU_BACCALAUREATE_CLASS))
  )

  if (nrow(institutions) == 0) {
    # Non-empty directory HTML but no baccalaureate-tagged articles -- either
    # the CSS class convention changed again or the directory was rendered by
    # JS. Emit a warning so the refresh workflow surfaces the regression.
    warning(
      sprintf(
        "NWCCU: parsed 0 baccalaureate-granting institutions from %s (directory HTML %d bytes). Returning empty table \u2014 validate selector '%s'.",
        NWCCU_DIRECTORY_URL,
        nchar(dir_html),
        NWCCU_BACCALAUREATE_CLASS
      ),
      call. = FALSE
    )
    return(tibble::tibble())
  }

  message(sprintf("  NWCCU: %d baccalaureate-granting institutions found; fetching individual pages …", nrow(institutions)))

  results <- purrr::map_dfr(seq_len(nrow(institutions)), function(i) {
    Sys.sleep(0.5)  # rate limit to avoid IP ban
    article_html <- institutions$article_html[[i]]
    link_m <- stringr::str_match(article_html, NWCCU_LINK_PATTERN)
    inst_url <- if (!is.na(link_m[1, 2])) link_m[1, 2] else NA_character_
    if (is.na(inst_url)) return(tibble::tibble())

    # Extract institution name from article
    name_match <- stringr::str_match(article_html, "<h3[^>]*>([^<]+)</h3>")
    inst_name <- if (!is.na(name_match[1, 2])) name_match[1, 2] else NA_character_

    tryCatch(
      parse_nwccu_institution_page(inst_url, inst_name, cache_dir, refresh),
      error = function(e) {
        message("  NWCCU: could not fetch ", inst_url, " — ", e$message)
        tibble::tibble()
      }
    )
  })

  if (nrow(results) == 0) {
    message("  NWCCU: no adverse actions found")
    return(tibble::tibble())
  }

  message(sprintf("  NWCCU: %d adverse-action row(s) extracted", nrow(results)))
  results
}

# ---------------------------------------------------------------------------
# SCRAPE-COUNT REGRESSION GUARD
# ---------------------------------------------------------------------------

# Compares freshly scraped row counts per accreditor against the previously
# written output CSV.  Emits a warning for any accreditor whose count has
# dropped by more than `drop_fraction` (default 40%) relative to the prior
# run, as long as the prior run produced at least `min_prior_rows` rows for
# that accreditor.  This catches silent failures where a site's HTML changed
# and the scraper returned an empty result instead of an error.
#
# @param fresh_df      Data frame returned by the combined scrape (must have
#                      an `accreditor` column).
# @param prior_csv     Path to the previously written actions CSV.  If the
#                      file does not exist, the check is skipped silently.
# @param drop_fraction Fractional drop threshold (0–1) that triggers a
#                      warning.  Default 0.4 = warn if count fell by >= 40%.
# @param min_prior_rows Minimum prior-run row count for an accreditor before
#                       the check is applied.  Avoids false positives for
#                       accreditors that routinely have very few actions.
warn_if_scrape_count_dropped <- function(fresh_df,
                                         prior_csv,
                                         drop_fraction = 0.4,
                                         min_prior_rows = 5L,
                                         fail = FALSE) {
  if (!file.exists(prior_csv)) {
    return(invisible(NULL))
  }
  # Only `accreditor` is needed for the per-accreditor count comparison.
  # Reading the full CSV trips readr's type-inference on sparsely-populated
  # columns (action_scope is logical-from-NA across the leading 1000 rows
  # then chokes on the few real strings later) and emits a generic
  # "parsing issues, call `problems()`" warning that's confusing because
  # this helper doesn't care about anything except the accreditor column.
  # cols_only skips every other column entirely.
  prior_df <- tryCatch(
    readr::read_csv(
      prior_csv,
      show_col_types = FALSE,
      col_types = readr::cols_only(accreditor = readr::col_character())
    ),
    error = function(e) NULL
  )
  if (is.null(prior_df) || !("accreditor" %in% names(prior_df))) {
    return(invisible(NULL))
  }

  prior_counts <- table(prior_df$accreditor)
  fresh_counts <- table(fresh_df$accreditor)

  for (acc in names(prior_counts)) {
    prior_n <- as.integer(prior_counts[[acc]])
    if (prior_n < min_prior_rows) next
    fresh_n <- if (acc %in% names(fresh_counts)) as.integer(fresh_counts[[acc]]) else 0L
    if (fresh_n == 0L || (prior_n - fresh_n) / prior_n >= drop_fraction) {
      msg <- sprintf(
        paste(
          "warn_if_scrape_count_dropped: %s row count dropped from %d to %d",
          "(%.0f%% decrease). The accreditor's site structure may have changed,",
          "or the scraper served a stale cache. Validate output before publishing."
        ),
        acc, prior_n, fresh_n,
        100 * (prior_n - fresh_n) / prior_n
      )
      if (isTRUE(fail)) {
        stop(msg, call. = FALSE)
      }
      warning(msg, call. = FALSE)
    }
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Finer-grained scraper-drift detector.
#
# warn_if_scrape_count_dropped compares totals per accreditor. That catches the
# case "accreditor returned nothing", but not the case "accreditor X's parser
# for action_type Y silently returned 0 rows because that sub-page's markup
# changed, while the other sub-pages still work."
#
# This helper compares (accreditor, action_type) pair counts between the prior
# and fresh runs and emits a warning when a pair that previously had at least
# `min_prior_rows` drops to 0 in the fresh run. A full-disappearance signal is
# a much cleaner scraper-breakage indicator than a fractional drop.
#
# The pair-level check is deliberately strict (drop-to-zero) to minimise
# false positives from normal month-to-month churn.
warn_if_action_type_dropped <- function(fresh_df,
                                        prior_csv,
                                        min_prior_rows = 3L,
                                        ignore_action_types = c("commission_action"),
                                        fail = FALSE) {
  if (!file.exists(prior_csv)) {
    return(invisible(NULL))
  }
  # Only `accreditor` and `action_type` are used for the (accreditor,
  # action_type) pair comparison. Same readr-autodetect-on-sparse-columns
  # rationale as warn_if_scrape_count_dropped above.
  prior_df <- tryCatch(
    readr::read_csv(
      prior_csv,
      show_col_types = FALSE,
      col_types = readr::cols_only(
        accreditor = readr::col_character(),
        action_type = readr::col_character()
      )
    ),
    error = function(e) NULL
  )
  if (is.null(prior_df) || nrow(prior_df) == 0L) {
    return(invisible(NULL))
  }
  required <- c("accreditor", "action_type")
  if (!all(required %in% names(prior_df)) ||
      !all(required %in% names(fresh_df))) {
    return(invisible(NULL))
  }

  prior_pairs <- as.data.frame(table(
    accreditor  = prior_df$accreditor,
    action_type = prior_df$action_type
  ), stringsAsFactors = FALSE)
  names(prior_pairs)[3] <- "prior_n"

  # table() on zero-length vectors produces a 1D result in R >= 4.5, so skip
  # the table() call when fresh_df is empty and treat all prior pairs as
  # having dropped to 0.
  if (nrow(fresh_df) == 0L) {
    joined <- prior_pairs
    joined$fresh_n <- 0L
  } else {
    fresh_pairs <- as.data.frame(table(
      accreditor  = fresh_df$accreditor,
      action_type = fresh_df$action_type
    ), stringsAsFactors = FALSE)
    names(fresh_pairs)[3] <- "fresh_n"
    joined <- merge(prior_pairs, fresh_pairs,
                    by = c("accreditor", "action_type"),
                    all.x = TRUE)
    joined$fresh_n[is.na(joined$fresh_n)] <- 0L
  }

  dropped <- joined[
    joined$prior_n >= min_prior_rows &
      joined$fresh_n == 0L &
      !(joined$action_type %in% ignore_action_types),
    ,
    drop = FALSE
  ]
  if (nrow(dropped) == 0L) return(invisible(NULL))

  for (i in seq_len(nrow(dropped))) {
    row <- dropped[i, , drop = FALSE]
    msg <- sprintf(
      paste(
        "warn_if_action_type_dropped: %s / %s dropped from %d rows to 0.",
        "This (accreditor, action_type) combination disappeared from the fresh",
        "scrape. The accreditor's markup may have changed, or a sub-parser",
        "quietly failed. Validate output before publishing."
      ),
      row$accreditor, row$action_type, as.integer(row$prior_n)
    )
    if (isTRUE(fail)) {
      stop(msg, call. = FALSE)
    }
    warning(msg, call. = FALSE)
  }
  invisible(NULL)
}
