################################################################################
# scripts/shared/accreditation_helpers.R
#
# Pure text-processing and classification helpers for the accreditation data
# pipeline. Handles HTML decoding, name normalization, state lookups, caching,
# metadata extraction, and action classification to standardize accreditation
# actions from various accreditor websites.
################################################################################

# ---------------------------------------------------------------------------
# HTML / TEXT CLEANING AND NORMALIZATION
# ---------------------------------------------------------------------------

# Decodes HTML entities and strips tags, converting <br> tags to newlines for structure.
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

# Decodes HTML, then collapses all whitespace (tabs, newlines, non-breaking spaces) to single spaces.
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

# Normalizes institution names for accreditor-to-IPEDS fuzzy matching.
# Implementation is centralized in scripts/shared/name_normalization.R so
# that all three pipeline name-matching forms (accreditation, cuts,
# grant_witness) live side by side and can't silently drift.
if (!exists("normalize_name_accreditation", mode = "function")) {
  .accred_shared_dir <- if (exists("root", inherits = TRUE)) {
    file.path(root, "scripts", "shared")
  } else {
    file.path(getwd(), "scripts", "shared")
  }
  source(file.path(.accred_shared_dir, "name_normalization.R"))
  rm(.accred_shared_dir)
}
normalize_name <- normalize_name_accreditation
normalize_accreditation_name <- normalize_name_accreditation

# ---------------------------------------------------------------------------
# STATE ABBREVIATION LOOKUP
# ---------------------------------------------------------------------------

# Lookup table: maps postal abbreviations to full state names, including US territories.
.accred_state_lookup <- c(
  setNames(state.name, state.abb),
  DC = "District of Columbia",
  PR = "Puerto Rico",
  VI = "Virgin Islands",
  GU = "Guam",
  MP = "Northern Mariana Islands",
  AS = "American Samoa"
)

# Converts two-letter state abbreviation to full name; returns input unchanged if not found.
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

# Fetches HTML from URL with disk caching and graceful fallback to cache on network failures.
# Caching is essential here because accreditor websites are often slow or temporarily unavailable;
# falling back to cached data from a previous run is better than failing entirely.
# validate_fn, when provided, is called with the raw response body (string).
# It should return TRUE if the content is usable and FALSE if it looks like a
# JavaScript-rendered shell or is otherwise invalid.
#
# When validate_fn returns FALSE:
#   - The cache is NOT overwritten (preserving last-known-good data).
#   - If a cached copy exists, a warning is emitted and the cached copy is
#     returned so the pipeline continues with stale-but-valid data.
#   - If no cached copy exists, stop() is called with actionable instructions.
fetch_html_text <- function(url, cache_name, cache_dir, refresh = TRUE,
                            validate_fn = NULL) {
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

    # Content validation: if the caller supplied a validator and the fresh
    # response fails it, do NOT write to cache — fall back to existing cache or
    # stop with instructions if no cache is available.
    if (!is.null(validate_fn) && !isTRUE(validate_fn(body))) {
      if (file.exists(cache_path)) {
        cache_mtime <- format(file.info(cache_path)$mtime, "%Y-%m-%d %H:%M")
        warning(paste(
          sprintf("fetch_html_text: fresh response from %s failed content validation", url),
          sprintf("(possible JavaScript-rendered shell). Cache NOT overwritten."),
          sprintf("Falling back to cached copy last updated %s.", cache_mtime),
          "Re-fetch the page with a JS-capable tool to refresh the cache.",
          sep = "\n"
        ), call. = FALSE)
        return(readr::read_file(cache_path))
      } else {
        stop(paste(
          sprintf("fetch_html_text: fresh response from %s failed content validation", url),
          "(possible JavaScript-rendered shell) and no cached fallback exists.",
          "Re-fetch the page with a JS-capable tool (e.g. chromote or browser",
          sprintf("automation) and save the rendered HTML to:\n  %s", cache_path),
          sep = "\n"
        ), call. = FALSE)
      }
    }

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

# Extracts the page modification date from Open Graph meta tag (article:modified_time).
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

# Extracts and cleans the page <title> element content.
extract_page_title <- function(html) {
  # Regex: match the title element and capture its content (non-greedy match)
  title_match <- stringr::str_match(html, "<title>(.*?)</title>")
  # Extract the captured content and clean it (decode HTML and normalize whitespace)
  clean_text(title_match[, 2])
}

# ---------------------------------------------------------------------------
# ACCREDITATION ACTION / STATUS CLASSIFICATION
# ---------------------------------------------------------------------------

# Classifies free-form action text to canonical types: warning, notice, monitoring,
# probation, show_cause, adverse_action, removed, other.
# Matching is case-insensitive and order-dependent (earlier patterns take precedence).
classify_action <- function(raw_action, accreditor = NA_character_) {
  txt <- stringr::str_to_lower(as.character(raw_action))
  dplyr::case_when(
    # "Removed" actions: a previous action has been lifted or resolved
    stringr::str_detect(txt, "removed from warning|remove notice of concern|removal of sanction") ~ "removed",
    stringr::str_detect(txt, "removed from probation") ~ "removed",
    # "Show Cause": most serious short of actual withdrawal
    stringr::str_detect(txt, "show cause") ~ "show_cause",
    # "Adverse Action": accreditation withdrawn, membership removed, or institution closed
    stringr::str_detect(txt, "closure|teach-?out|teach out|denied reaffirmation|deny reaffirmation") ~ "adverse_action",
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

# Classifies action text as "active" or "resolved" based on removal keywords.
classify_status <- function(raw_action) {
  txt <- stringr::str_to_lower(as.character(raw_action))
  dplyr::case_when(
    # Look for keywords indicating the action has been removed or resolved
    stringr::str_detect(txt, "removed from warning|removed from probation|removal of sanction|remove notice of concern|removed notice of concern") ~ "resolved",
    # Everything else is treated as active (currently affecting the college)
    TRUE ~ "active"
  )
}

# Returns TRUE if text contains serious action keywords (warning, probation, show cause, etc.)
# indicating a publicly significant accreditation issue.
has_public_action_keywords <- function(x) {
  txt <- stringr::str_to_lower(as.character(x))
  stringr::str_detect(
    txt,
    "warning|probation|show cause|notice of concern|notice|withdrawal of accreditation|withdraws from membership|closure|teach-?out|teach out|denied reaffirmation|deny reaffirmation|adverse"
  )
}

# ---------------------------------------------------------------------------
# INSTITUTION NAME AND STATE PARSING
# ---------------------------------------------------------------------------

# Parses institution name and state from accreditor list item text.
# Handles formats: "Name, City, ST", "Name, ST", or "Name (ST)".
# Strips trailing parenthetical metadata like "(next review)" before parsing.
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

# Two-pass fuzzy match of scraped institution names to tracker database.
# Pass 1: normalized name + state (most specific). Pass 2: normalized name
# only, but only when the accreditor did not supply a state or the tracker
# state agrees. This avoids unsafe matches between same-named institutions in
# different states. Returns unitid, tracker_name, tracker_state, and match_method.
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
    dplyr::mutate(
      exact_unitid = as.character(exact_unitid),
      name_only_unitid = as.character(name_only_unitid),
      name_only_state_eligible = is.na(institution_state_normalized) |
        !nzchar(institution_state_normalized) |
        institution_state_normalized == name_only_tracker_state,
      name_only_unitid = dplyr::if_else(name_only_state_eligible, name_only_unitid, NA_character_),
      name_only_tracker_name = dplyr::if_else(name_only_state_eligible, name_only_tracker_name, NA_character_),
      name_only_tracker_state = dplyr::if_else(name_only_state_eligible, name_only_tracker_state, NA_character_)
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
      -name_only_unitid, -name_only_tracker_name, -name_only_tracker_state,
      -name_only_state_eligible
    )
}
