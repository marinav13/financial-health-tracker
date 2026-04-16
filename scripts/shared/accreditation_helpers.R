################################################################################
# scripts/shared/accreditation_helpers.R
#
# PURPOSE:
#   Provides pure text-processing and classification helper functions for the
#   accreditation data pipeline. These functions handle HTML decoding, name
#   normalization, state lookups, caching, metadata extraction, and action
#   classification logic needed to standardize and classify accreditation actions
#   from various accreditor websites.
#
# USAGE:
#   Source this file after utils.R inside main() in build_accreditation_actions.R.
#   All functions in this file are called by the scraper functions in
#   accreditation_scrapers.R.
#
# DEPENDENCIES:
#   Requires: dplyr, httr2, readr, stringr, xml2 (loaded by the caller)
#
################################################################################

# ---------------------------------------------------------------------------
# HTML / TEXT CLEANING AND NORMALIZATION
# ---------------------------------------------------------------------------

#' Strip HTML tags and decode entities to plain text
#'
#' Removes HTML markup and converts HTML entities to their plain text equivalents.
#' Converts <br> and <br/> tags into newlines; other tags are removed.
#'
#' @param x character vector to decode (coerced to character if needed)
#'
#' @return character vector with HTML tags removed, entities decoded, and
#'         structure preserved as newlines; returns NA_character_ for NA or empty inputs
decode_html <- function(x) {
  x <- as.character(x)
  vapply(x, function(one) {
    if (is.na(one) || !nzchar(one)) return(NA_character_)
    # Replace <br> and <br/> tags (case-insensitive) with newline characters
    # This preserves line breaks in the original HTML formatting
    one <- gsub("<br\\s*/?>", "\n", one, ignore.case = TRUE)
    # Parse the HTML in a minimal document context to decode entities properly
    doc <- xml2::read_html(paste0("<html><body>", one, "</body></html>"))
    # Extract all text content, which handles entity decoding automatically
    xml2::xml_text(xml2::xml_find_first(doc, "//body"))
  }, character(1))
}

#' Clean and normalize text: decode HTML and collapse whitespace
#'
#' Strips HTML markup, decodes entities, and normalizes whitespace by collapsing
#' all whitespace sequences (including tabs, carriage returns, newlines) to single
#' spaces. Also handles non-breaking spaces (Unicode 00A0).
#'
#' @param x character vector to clean
#'
#' @return character vector with HTML removed, whitespace normalized, and
#'         trimmed of leading/trailing space
clean_text <- function(x) {
  x |>
    decode_html() |>
    # Replace any sequence of tab, newline, carriage return with single space
    stringr::str_replace_all("[\r\n\t]+", " ") |>
    # Replace non-breaking space (Unicode 00A0) with regular space
    stringr::str_replace_all("\u00a0", " ") |>
    # Remove leading/trailing whitespace and collapse interior sequences
    stringr::str_squish()
}

#' Normalize institution name for fuzzy matching
#'
#' Converts institution names to a standardized form suitable for fuzzy matching:
#' lowercases, expands "&" to "and", abbreviates "Saint" to "St", and removes
#' all non-alphanumeric characters (except spaces).
#'
#' @param x character vector of institution names
#'
#' @return character vector with names normalized to lowercase tokens for matching.
#'         For example: "Saint Mary's University & College" becomes
#'         "st mary s university and college"
normalize_name <- function(x) {
  x |>
    as.character() |>
    stringr::str_to_lower() |>
    # Expand ampersand to full word for matching (e.g., "A&M" -> "a and m")
    stringr::str_replace_all("&", " and ") |>
    # Abbreviate "Saint" (word boundary to match) to "St"
    stringr::str_replace_all("\\bsaint\\b", "st") |>
    # Remove all special characters except alphanumeric and space
    stringr::str_replace_all("[^a-z0-9 ]", " ") |>
    # Clean up any resulting whitespace issues
    stringr::str_squish()
}

# ---------------------------------------------------------------------------
# STATE ABBREVIATION LOOKUP
# ---------------------------------------------------------------------------

#' State name lookup table: maps postal abbreviations to full names
#'
#' Includes all US states (via R's built-in state.abb and state.name),
#' plus US territories and special jurisdictions commonly found in
#' accreditation data (DC, PR, VI, GU, MP, AS).
.accred_state_lookup <- c(
  setNames(state.name, state.abb),
  DC = "District of Columbia",
  PR = "Puerto Rico",
  VI = "Virgin Islands",
  GU = "Guam",
  MP = "Northern Mariana Islands",
  AS = "American Samoa"
)

#' Convert two-letter state abbreviation to full state name
#'
#' Maps postal abbreviations (CA, TX, etc.) to their full names.
#' If no match is found, returns the input unchanged.
#'
#' @param x character vector of state abbreviations (case-insensitive)
#'
#' @return character vector with abbreviations converted to full names;
#'         unmapped values are returned as-is. For example: "CA" becomes
#'         "California"; "XX" stays "XX".
state_name <- function(x) {
  x_chr <- as.character(x)
  # Look up each value in the lookup table (case-insensitive via toupper)
  out   <- unname(.accred_state_lookup[toupper(x_chr)])
  # For any NAs in the result, keep the original input value
  out[is.na(out)] <- x_chr[is.na(out)]
  out
}

# ---------------------------------------------------------------------------
# HTTP FETCHING WITH CACHE FALLBACK
# ---------------------------------------------------------------------------

#' Fetch HTML from URL with intelligent caching
#'
#' Downloads HTML from a URL and caches the result to disk. On subsequent calls,
#' can return the cached copy without a network request. If a fetch fails but a
#' cache exists, automatically falls back to the cached version (graceful degradation).
#'
#' This pattern is especially useful for accreditation scraping because accreditor
#' websites are often slow or temporarily unavailable, and cached data from a
#' previous run is better than failing entirely.
#'
#' @param url character. The URL to fetch
#' @param cache_name character. Filename to save the cached HTML under (e.g., "msche_status.html")
#' @param cache_dir character. Directory where cached files are stored
#' @param refresh logical. If FALSE (default is TRUE), skip the network call and
#'        return the cached copy if it exists. If TRUE, always attempt to fetch
#'        from the network.
#'
#' @return character. The HTML response body as a string
#'
#' @details
#'   Behavior:
#'   - If refresh=FALSE and cache exists: returns cache immediately (no network call)
#'   - If refresh=TRUE (or refresh=FALSE and no cache): attempts network fetch
#'   - On fetch success: saves response to cache_dir/cache_name and returns body
#'   - On fetch failure with existing cache: logs a message and returns cached copy
#'   - On fetch failure without cache: raises an error with the failure reason
fetch_html_text <- function(url, cache_name, cache_dir, refresh = TRUE) {
  cache_path <- file.path(cache_dir, cache_name)
  # Quick return if not refreshing and cache exists
  if (!refresh && file.exists(cache_path)) {
    return(readr::read_file(cache_path))
  }
  tryCatch({
    # Perform HTTP GET request with a User-Agent header to identify our requests
    resp <- httr2::request(url) |>
      httr2::req_user_agent("FinancialHealthProject/1.0") |>
      httr2::req_perform()
    body <- httr2::resp_body_string(resp)
    # Save the response to cache for future use
    readr::write_file(body, cache_path)
    body
  }, error = function(e) {
    # On network error, try to use cached copy as fallback
    if (file.exists(cache_path)) {
      message("Falling back to cached copy for ", url)
      readr::read_file(cache_path)
    } else {
      # No cache available, so the fetch failure is fatal
      stop("Failed to fetch ", url, ": ", e$message)
    }
  })
}

# ---------------------------------------------------------------------------
# PAGE METADATA EXTRACTION
# ---------------------------------------------------------------------------

#' Extract page modification date from Open Graph metadata
#'
#' Searches the HTML for the Open Graph (OG) article:modified_time meta tag,
#' which many web pages use to declare when the page was last updated.
#' This is used to detect if accreditation data on a page has changed.
#'
#' @param html character. The raw HTML string to search
#'
#' @return character. ISO date string (YYYY-MM-DD format) or NA if not found.
#'         For example: "2025-04-15"
#'
#' @details
#'   Looks for patterns like:
#'   <meta property="article:modified_time" content="2025-04-15" ... />
extract_page_modified_date <- function(html) {
  # Regex: look for the Open Graph meta tag with article:modified_time,
  # and capture the date portion (YYYY-MM-DD format)
  match <- stringr::str_match(
    html,
    "article:modified_time\" content=\"([0-9]{4}-[0-9]{2}-[0-9]{2})"
  )
  # Return the captured group (column 2); column 1 is the full match
  match[, 2]
}

#' Extract and clean the page title from HTML
#'
#' Finds the <title> element in the HTML document and extracts the text content,
#' then applies clean_text() to decode entities and normalize whitespace.
#'
#' @param html character. The raw HTML string to search
#'
#' @return character. The cleaned title text, or NA if no title found
#'
#' @details
#'   Example: "<title>January 2025 &ndash; Actions</title>"
#'   becomes "January 2025 - Actions"
extract_page_title <- function(html) {
  # Regex: match the title element and capture its content (non-greedy match)
  title_match <- stringr::str_match(html, "<title>(.*?)</title>")
  # Extract the captured content and clean it (decode HTML and normalize whitespace)
  clean_text(title_match[, 2])
}

# ---------------------------------------------------------------------------
# ACCREDITATION ACTION / STATUS CLASSIFICATION
# ---------------------------------------------------------------------------

#' Classify raw accreditor action text to canonical action type
#'
#' Maps free-form text from accreditor websites to standardized action types.
#' This allows different accreditors' terminology to be normalized into
#' consistent categories for comparison and reporting.
#'
#' Accreditation action types (regulatory severity order):
#'   - "warning": College must address specific compliance issues (least severe)
#'   - "notice": Formal notice of concern or compliance issue
#'   - "monitoring": College is under monitoring; not yet a formal sanction
#'   - "probation": College's accreditation is probationary; must demonstrate compliance
#'   - "show_cause": College must demonstrate why accreditation should not be withdrawn
#'   - "adverse_action": Accreditation withdrawn, membership removed, or closure (most severe)
#'   - "removed": A previous action has been removed or lifted
#'   - "other": Does not match any above category
#'
#' @param raw_action character. The text label from an accreditor (e.g., "Non-Compliance Warning")
#' @param accreditor character. (Optional) The accreditor name, used for accreditor-specific logic
#'
#' @return character. One of the canonical action type strings listed above
#'
#' @details
#'   Classification rules are order-dependent; earlier matches take precedence.
#'   Keywords are matched case-insensitively.
classify_action <- function(raw_action, accreditor = NA_character_) {
  txt <- stringr::str_to_lower(as.character(raw_action))
  dplyr::case_when(
    # "Removed" actions: a previous action has been lifted or resolved
    stringr::str_detect(txt, "removed from warning|remove notice of concern|removal of sanction") ~ "removed",
    stringr::str_detect(txt, "removed from probation") ~ "removed",
    # "Show Cause": most serious short of actual withdrawal
    stringr::str_detect(txt, "show cause") ~ "show_cause",
    # "Adverse Action": accreditation withdrawn, membership removed, or institution closed
    stringr::str_detect(txt, "closure") ~ "adverse_action",
    stringr::str_detect(txt, "removed from membership|withdrawal|withdraws from membership|withdraw candidate|withdraw accreditation") ~ "adverse_action",
    # "Probation": accreditation status is probationary (must cure deficiencies)
    stringr::str_detect(txt, "probation") ~ "probation",
    # "Warning": institution must fix issues but accreditation not yet threatened
    stringr::str_detect(txt, "warning") ~ "warning",
    # "Notice": formal notice of concern or non-compliance
    stringr::str_detect(txt, "notice of concern") ~ "notice",
    stringr::str_detect(txt, "notice") ~ "notice",
    # "Monitoring": institution under heightened scrutiny or monitoring
    stringr::str_detect(txt, "monitor") ~ "monitoring",
    # Catch-all for other adverse terminology
    stringr::str_detect(txt, "adverse") ~ "adverse_action",
    # Fallback for unrecognized text
    TRUE ~ "other"
  )
}

#' Classify action text to status: "active" or "resolved"
#'
#' Distinguishes between current accreditation problems and actions that have
#' been removed or lifted. Used to filter for currently relevant accreditation risks.
#'
#' @param raw_action character. The action text to classify
#'
#' @return character. Either "resolved" (action has been lifted) or "active"
#'         (action is currently in effect)
classify_status <- function(raw_action) {
  txt <- stringr::str_to_lower(as.character(raw_action))
  dplyr::case_when(
    # Look for keywords indicating the action has been removed or resolved
    stringr::str_detect(txt, "removed from warning|removed from probation|removal of sanction|remove notice of concern|removed notice of concern") ~ "resolved",
    # Everything else is treated as active (currently affecting the college)
    TRUE ~ "active"
  )
}

#' Check if action text contains public-action keywords
#'
#' Returns TRUE if the text indicates a serious accreditation action that would
#' be publicly disclosed to students and prospective students. These keywords
#' identify actions important enough to flag for downstream processing.
#'
#' @param x character vector. Action text to search
#'
#' @return logical vector. TRUE if the text contains public-action keywords,
#'         FALSE otherwise
#'
#' @details
#'   Public-action keywords are those indicating serious accreditation issues:
#'   warning, probation, show cause, notice of concern, withdrawal, closure, adverse.
#'   These indicate situations where students' federal financial aid eligibility
#'   may be at risk or will be soon.
has_public_action_keywords <- function(x) {
  txt <- stringr::str_to_lower(as.character(x))
  stringr::str_detect(
    txt,
    "warning|probation|show cause|notice of concern|notice|withdrawal of accreditation|withdraws from membership|closure|adverse"
  )
}

# ---------------------------------------------------------------------------
# INSTITUTION NAME AND STATE PARSING
# ---------------------------------------------------------------------------

#' Extract institution name and state from accreditor list item text
#'
#' Parses text strings that list institutions with location information.
#' Accreditors list institutions in various formats; this function handles
#' the most common patterns:
#'   - "Institution Name, City, ST" (name, city, state abbreviation)
#'   - "Institution Name, ST" (name, state abbreviation)
#'   - "Institution Name (ST)" (name in parentheses with state)
#'
#' First removes any parenthetical metadata like "(next review)" or "(closure of)".
#'
#' @param x character. A string containing an institution name and state information
#'
#' @return list with two elements:
#'   - institution_name_raw: cleaned institution name, or original text if parsing fails
#'   - institution_state_raw: full state name (via state_name()), or NA if not found
#'
#' @details
#'   Examples:
#'   - "University of Maine, Orono, ME" -> "University of Maine" + "Maine"
#'   - "Boston College, MA" -> "Boston College" + "Massachusetts"
#'   - "Drake University (IA)" -> "Drake University" + "Iowa"
#'   - "Unknown School" (no state) -> "Unknown School" + NA
extract_name_state_from_item <- function(x) {
  item <- clean_text(x)
  # Remove trailing metadata in parentheses like (next review), (letter dated),
  # (closure of), (two proposals) - these are notes, not location info
  item <- stringr::str_remove(
    item,
    "\\s*\\((next review|letter dated|closure of|two proposals).*?\\)$"
  )
  item <- stringr::str_squish(item)

  # Try pattern 1: "Name, City, ST" (two commas, state at end)
  match <- stringr::str_match(item, "^(.*?),\\s*[^,]+,\\s*([A-Z]{2})$")
  if (!is.na(match[1, 1])) {
    return(list(institution_name_raw = clean_text(match[1, 2]),
                institution_state_raw = state_name(match[1, 3])))
  }

  # Try pattern 2: "Name, ST" (one comma, two-letter state at end)
  match <- stringr::str_match(item, "^(.*?),\\s*([A-Z]{2})$")
  if (!is.na(match[1, 1])) {
    return(list(institution_name_raw = clean_text(match[1, 2]),
                institution_state_raw = state_name(match[1, 3])))
  }

  # Try pattern 3: "Name (City, ST)" or similar - state abbr at end of parenthetical
  match <- stringr::str_match(item, "^(.*?)\\s*\\(([^)]+)\\)$")
  if (!is.na(match[1, 1])) {
    # Extract two-letter state abbreviation from the end of the parenthetical
    state_abbr <- stringr::str_match(match[1, 3], "([A-Z]{2})$")[, 2]
    return(list(institution_name_raw = clean_text(match[1, 2]),
                institution_state_raw = state_name(state_abbr)))
  }

  # No pattern matched; return the entire cleaned item as name, NA for state
  list(institution_name_raw  = item,
       institution_state_raw = NA_character_)
}

# ---------------------------------------------------------------------------
# INSTITUTION MATCHING TO TRACKER DATABASE
# ---------------------------------------------------------------------------

#' Match institutions from accreditor actions to the college tracker database
#'
#' Performs a two-pass fuzzy match of institution names scraped from accreditor
#' websites to known institutions in our tracker database (IPEDS or similar source).
#' This is necessary because accreditors often use slightly different names
#' (abbreviations, full vs. short names, etc.) than our authoritative sources.
#'
#' Matching strategy (in order of specificity):
#'   1. Normalized name + state: most specific, fewest false positives.
#'      Example: normalized("Boston College") + "Massachusetts" matches
#'               to tracked "Boston College" in Massachusetts
#'   2. Normalized name only: catches institutions with state mismatches or
#'      institutions with multiple campuses across states. Example: a "System"
#'      header might match multiple campuses in lookup_name_only
#'
#' @param actions_df data.frame. Accreditation actions with columns:
#'        - institution_name_normalized: normalized name (lowercase, no special chars)
#'        - institution_state_normalized: full state name
#'
#' @param lookup_exact data.frame. Exact match lookup table with columns:
#'        - norm_name: normalized institution name
#'        - state_match: state to match against
#'        - matched_unitid: UNITID or institution identifier
#'        - tracker_name: canonical name from tracker database
#'        - tracker_state: state from tracker database
#'
#' @param lookup_name_only data.frame. Name-only lookup table (same columns as
#'        lookup_exact except state_match). Used for fallback matching.
#'
#' @return data.frame. Input actions_df with columns added:
#'         - unitid: the matched institution ID, or NA if unmatched
#'         - tracker_name: canonical institution name from tracker
#'         - tracker_state: state from tracker
#'         - match_method: one of "normalized_name_plus_state",
#'                         "normalized_name_only", or "unmatched"
#'
#' @details
#'   The function uses dplyr::coalesce() to prioritize exact matches over
#'   name-only matches. Intermediate join columns are dropped before returning.
match_institutions_to_tracker <- function(actions_df, lookup_exact, lookup_name_only) {
  actions_df |>
    # Pass 1: Try exact match (name + state)
    dplyr::left_join(
      lookup_exact,
      by = c(
        "institution_name_normalized" = "norm_name",
        "institution_state_normalized" = "state_match"
      )
    ) |>
    # Rename exact match results to temporary columns for later coalescing
    dplyr::rename(
      exact_unitid       = matched_unitid,
      exact_tracker_name = tracker_name,
      exact_tracker_state = tracker_state
    ) |>
    # Pass 2: Try name-only match (fallback)
    dplyr::left_join(
      lookup_name_only,
      by = c("institution_name_normalized" = "norm_name")
    ) |>
    # Rename name-only results to temporary columns
    dplyr::rename(
      name_only_unitid        = matched_unitid,
      name_only_tracker_name  = tracker_name,
      name_only_tracker_state = tracker_state
    ) |>
    # Merge the results: use exact match if found, fall back to name-only
    dplyr::mutate(
      unitid       = dplyr::coalesce(exact_unitid, name_only_unitid),
      tracker_name  = dplyr::coalesce(exact_tracker_name, name_only_tracker_name),
      tracker_state = dplyr::coalesce(exact_tracker_state, name_only_tracker_state),
      # Record which matching strategy succeeded
      match_method  = dplyr::case_when(
        !is.na(exact_unitid)                            ~ "normalized_name_plus_state",
        is.na(exact_unitid) & !is.na(name_only_unitid) ~ "normalized_name_only",
        TRUE                                            ~ "unmatched"
      )
    ) |>
    # Clean up temporary columns created during the join
    dplyr::select(
      -exact_unitid, -exact_tracker_name, -exact_tracker_state,
      -name_only_unitid, -name_only_tracker_name, -name_only_tracker_state
    )
}
