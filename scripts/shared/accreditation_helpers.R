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

# Normalizes accreditor names/codes into the short codes used across the
# tracker exports and frontend payloads.
normalize_accreditor_code <- function(x) {
  value <- trimws(as.character(x %||% ""))
  dplyr::case_when(
    grepl("higher learning commission|\\bhlc\\b", value, ignore.case = TRUE, perl = TRUE) ~ "HLC",
    grepl("middle states|\\bmsche\\b", value, ignore.case = TRUE, perl = TRUE) ~ "MSCHE",
    grepl("new england commission|\\bneche\\b", value, ignore.case = TRUE, perl = TRUE) ~ "NECHE",
    grepl("southern association|sacscoc", value, ignore.case = TRUE, perl = TRUE) ~ "SACSCOC",
    grepl("western association|wasc senior|wscuc", value, ignore.case = TRUE, perl = TRUE) ~ "WSCUC",
    grepl("northwest commission|nwccu", value, ignore.case = TRUE, perl = TRUE) ~ "NWCCU",
    TRUE ~ value
  )
}

normalize_action_join_text <- function(x) {
  value <- as.character(x)
  value[is.na(value)] <- ""
  value <- tolower(trimws(value))
  gsub("\\s+", " ", value, perl = TRUE)
}

normalize_action_join_date <- function(x) {
  values <- trimws(as.character(x %||% ""))
  values[!nzchar(values) | values == "NA"] <- ""
  values
}

build_accreditation_action_source_key <- function(unitid,
                                                  institution_name,
                                                  accreditor,
                                                  action_type,
                                                  action_label,
                                                  action_date,
                                                  source_url = NA_character_,
                                                  source_page_url = NA_character_,
                                                  file_id = NA_character_) {
  normalized_unitid <- trimws(as.character(unitid %||% ""))
  normalized_unitid[is.na(normalized_unitid) | normalized_unitid == "NA"] <- ""
  normalized_name <- normalize_action_join_text(institution_name)
  normalized_accreditor <- normalize_accreditor_code(accreditor)
  normalized_type <- normalize_action_join_text(action_type)
  normalized_label <- normalize_action_join_text(action_label)
  normalized_date <- normalize_action_join_date(action_date)
  locator <- as.character(source_url)
  locator[is.na(locator)] <- ""
  locator <- trimws(locator)
  page_locator <- as.character(source_page_url)
  page_locator[is.na(page_locator)] <- ""
  page_locator <- trimws(page_locator)
  if (length(page_locator) == 1L && length(locator) > 1L) {
    page_locator <- rep(page_locator, length(locator))
  } else if (length(locator) == 1L && length(page_locator) > 1L) {
    locator <- rep(locator, length(page_locator))
  }
  locator[!nzchar(locator)] <- page_locator[!nzchar(locator)]
  normalized_file_id <- as.character(file_id)
  normalized_file_id[is.na(normalized_file_id)] <- ""
  normalized_file_id <- trimws(normalized_file_id)

  paste(
    normalized_unitid,
    normalized_name,
    normalized_accreditor,
    normalized_type,
    normalized_label,
    normalized_date,
    locator,
    normalized_file_id,
    sep = "||"
  )
}

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

cache_freshness_label <- function(cache_path, now = Sys.time()) {
  info <- file.info(cache_path)
  mtime <- info$mtime[[1]]
  if (is.na(mtime)) return("last updated at an unknown time")

  age_days <- as.numeric(difftime(now, mtime, units = "days"))
  if (!is.finite(age_days)) return(sprintf("last updated %s", format(mtime, "%Y-%m-%d %H:%M")))

  sprintf(
    "last updated %s (%.1f days old)",
    format(mtime, "%Y-%m-%d %H:%M"),
    age_days
  )
}

cache_age_days <- function(cache_path, now = Sys.time()) {
  info <- file.info(cache_path)
  mtime <- info$mtime[[1]]
  if (is.na(mtime)) return(NA_real_)
  age_days <- as.numeric(difftime(now, mtime, units = "days"))
  if (!is.finite(age_days)) return(NA_real_)
  age_days
}

.accreditation_fetch_telemetry <- new.env(parent = emptyenv())
.accreditation_fetch_telemetry$events <- list()

reset_accreditation_fetch_telemetry <- function() {
  .accreditation_fetch_telemetry$events <- list()
  invisible(NULL)
}

record_accreditation_fetch_event <- function(accreditor,
                                             url,
                                             resource_type,
                                             outcome,
                                             cache_path = NA_character_) {
  cache_age <- if (!is.na(cache_path) && nzchar(cache_path) && file.exists(cache_path)) {
    cache_age_days(cache_path)
  } else {
    NA_real_
  }

  .accreditation_fetch_telemetry$events[[length(.accreditation_fetch_telemetry$events) + 1L]] <- data.frame(
    accreditor = as.character(accreditor %||% NA_character_),
    url = as.character(url %||% NA_character_),
    resource_type = as.character(resource_type %||% NA_character_),
    outcome = as.character(outcome %||% NA_character_),
    cache_age_days = cache_age,
    stringsAsFactors = FALSE
  )
  invisible(NULL)
}

get_accreditation_fetch_telemetry <- function() {
  if (length(.accreditation_fetch_telemetry$events) == 0L) {
    return(data.frame(
      accreditor = character(),
      url = character(),
      resource_type = character(),
      outcome = character(),
      cache_age_days = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  dplyr::bind_rows(.accreditation_fetch_telemetry$events)
}

with_accreditation_fetch_context <- function(accreditor, expr) {
  old <- getOption("tracker.current_accreditor", default = NA_character_)
  options(tracker.current_accreditor = accreditor)
  on.exit(options(tracker.current_accreditor = old), add = TRUE)
  force(expr)
}

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
  accreditor <- getOption("tracker.current_accreditor", default = NA_character_)
  fetch_fresh_body <- function() {
    primary_err <- NULL
    body <- tryCatch(
      {
        resp <- httr2::request(url) |>
          httr2::req_user_agent("FinancialHealthProject/1.0") |>
          httr2::req_perform()
        httr2::resp_body_string(resp)
      },
      error = function(e) {
        primary_err <<- e
        NULL
      }
    )
    if (!is.null(body)) {
      return(list(body = body, method = "httr2"))
    }

    tmp_path <- tempfile("html-fetch-", fileext = ".html")
    on.exit(unlink(tmp_path), add = TRUE)
    fallback_err <- tryCatch(
      {
        download_with_retry(url, tmp_path, mode = "wb", quiet = TRUE, timeout = 60L, retries = 2L)
        NULL
      },
      error = function(e) e
    )
    if (is.null(fallback_err) && file.exists(tmp_path)) {
      return(list(body = readr::read_file(tmp_path), method = "download.file"))
    }

    python_bin <- Sys.which(c("python", "python3"))
    python_bin <- python_bin[nzchar(python_bin)][1]
    if (!is.na(python_bin) && nzchar(python_bin)) {
      py_script <- tempfile("html-fetch-", fileext = ".py")
      writeLines(
        c(
          "import pathlib",
          "import sys",
          "import urllib.request",
          "req = urllib.request.Request(sys.argv[1], headers={'User-Agent': 'FinancialHealthProject/1.0'})",
          "pathlib.Path(sys.argv[2]).write_bytes(urllib.request.urlopen(req, timeout=60).read())"
        ),
        py_script
      )
      on.exit(unlink(py_script), add = TRUE)
      py_out <- tryCatch(
        system2(
          python_bin,
          c(py_script, url, tmp_path),
          stdout = TRUE,
          stderr = TRUE
        ),
        error = function(e) structure(conditionMessage(e), status = 1L)
      )
      py_status <- attr(py_out, "status", exact = TRUE)
      if (is.null(py_status) && file.exists(tmp_path)) {
        return(list(body = readr::read_file(tmp_path), method = "python"))
      }
      python_err <- paste(py_out, collapse = "\n")
    } else {
      python_err <- "python interpreter not found"
    }

    stop(
      "Failed to fetch ", url, ": ", conditionMessage(primary_err),
      "; download.file fallback also failed: ", conditionMessage(fallback_err),
      "; python fallback also failed: ", python_err,
      call. = FALSE
    )
  }
  # Quick return if not refreshing and cache exists
  if (!refresh && file.exists(cache_path)) {
    record_accreditation_fetch_event(accreditor, url, "html", "cache_read", cache_path)
    message(sprintf(
      "Using cached copy for %s (%s)",
      url,
      cache_freshness_label(cache_path)
    ))
    return(readr::read_file(cache_path))
  }
  tryCatch({
    # Perform HTTP GET request with a User-Agent header to identify our
    # requests. Some NECHE detail pages fail under httr2 on Windows while
    # still succeeding via download.file/libcurl, so keep a second fetch
    # path before falling back to stale cache.
    fresh <- fetch_fresh_body()
    body <- fresh$body

    # Content validation: if the caller supplied a validator and the fresh
    # response fails it, do NOT write to cache — fall back to existing cache or
    # stop with instructions if no cache is available.
    if (!is.null(validate_fn) && !isTRUE(validate_fn(body))) {
      if (file.exists(cache_path)) {
        record_accreditation_fetch_event(accreditor, url, "html", "cache_fallback", cache_path)
        warning(paste(
          sprintf("fetch_html_text: fresh response from %s failed content validation", url),
          sprintf("(possible JavaScript-rendered shell). Cache NOT overwritten."),
          sprintf("Falling back to cached copy %s.", cache_freshness_label(cache_path)),
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
    status <- switch(
      fresh$method,
      "download.file" = "fresh_fetch_download_file",
      "python" = "fresh_fetch_python",
      "fresh_fetch"
    )
    record_accreditation_fetch_event(accreditor, url, "html", status, cache_path)
    body
  }, error = function(e) {
    # On network error, try to use cached copy as fallback
    if (file.exists(cache_path)) {
      record_accreditation_fetch_event(accreditor, url, "html", "cache_fallback", cache_path)
      message(sprintf(
        "Falling back to cached copy for %s (%s)",
        url,
        cache_freshness_label(cache_path)
      ))
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
#
# Adverse-action disambiguation (MSCHE branch/location vs. institutional):
# MSCHE substantive-change paperwork frequently mentions "decision to close"
# and "teach-out plan" in the context of a branch campus or additional
# location, e.g. "To acknowledge receipt of the substantive change request.
# To note the institution's decision to close the additional location at
# 123 Main St." These are routine administrative acknowledgments and must
# not be classified as institutional adverse actions. The is_branch_or_location
# guard below detects that context and demotes those rows to "other" so the
# adverse_action bucket only contains true institution-level signals.
# Real institutional closures use phrasings the guard intentionally does
# not match: "close all locations", "cease instruction at all locations",
# "the institution will close", "decision to close ... institution",
# "voluntary withdrawal", etc.
classify_action <- function(raw_action, accreditor = NA_character_) {
  txt <- stringr::str_to_lower(as.character(raw_action))

  # Sub-institutional / regulatory-paperwork context: the action concerns
  # something other than the institution itself ceasing operations.
  # Covers (a) additional locations, branch campuses, instructional
  # sites; (b) federally-required candidate-institution teach-out plans
  # (34 CFR 602.23(f)(1)(ii)) which are application paperwork, not real
  # teach-outs. Used to suppress false-positive adverse_action
  # classification on MSCHE substantive-change rows.
  is_branch_or_location <- stringr::str_detect(
    txt,
    paste(
      "additional location",
      "branch campus",
      "instructional site",
      "off[- ]campus (?:site|location)",
      "teach-?out (?:plan|agreement|agreements?) as required of candidate",
      "as required of candidate institutions",
      sep = "|"
    )
  )

  # True institution-level closure / withdrawal signals. These phrasings
  # describe the institution itself ceasing operations, surrendering
  # accreditation, or being denied reaffirmation -- not a sub-unit change.
  is_institutional_adverse <- stringr::str_detect(
    txt,
    paste(
      # institution-level closure verbs
      "the institution will close",
      "will close (?:effective|all locations|permanently)",
      "close all locations",
      "cease instruction at all locations",
      "intention to cease instruction",
      "intent to cease instruction",
      "institutional closure",
      # decision-to-close, but only when scoped to the institution itself
      "decision to close [^.]{0,80}?(?:the institution|all locations|all of its locations|operations|permanently)",
      # accreditation-status terminations
      "denied reaffirmation",
      "deny reaffirmation",
      "removed from membership",
      "withdraws from membership",
      "withdraw accreditation",
      "withdrawal of accreditation",
      "voluntar(?:ily|y) surrender",
      "voluntar(?:ily|y) withdrawal",
      sep = "|"
    )
  )

  # Generic teach-out / closure signals. These trigger adverse_action
  # only when the branch-or-location guard does not. Most non-MSCHE
  # accreditors emit short labels like "Accepted Teach-Out Plan" with
  # no sub-unit qualifier, so they fall through here as adverse.
  is_generic_teachout_or_closure <- stringr::str_detect(
    txt,
    "closure|teach-?out|teach out|withdraw candidate"
  )

  dplyr::case_when(
    # "Removed" actions: a previous action has been lifted or resolved
    stringr::str_detect(txt, "removed from warning|remove the institution from warning|remove notice of concern|removal of sanction") ~ "removed",
    stringr::str_detect(txt, "removed from probation|remove the institution from probation") ~ "removed",
    # "Show Cause": most serious short of actual withdrawal
    stringr::str_detect(txt, "show cause") ~ "show_cause",
    # "Adverse Action": accreditation withdrawn, membership removed, or institution closed.
    # Order matters: institutional signals always win; generic teach-out /
    # closure language demotes to "other" when the text is clearly about
    # a branch campus or additional location.
    is_institutional_adverse ~ "adverse_action",
    is_generic_teachout_or_closure & !is_branch_or_location ~ "adverse_action",
    # "Probation": accreditation status is probationary (must cure deficiencies)
    stringr::str_detect(txt, "probation") ~ "probation",
    # "Warning": institution must fix issues but accreditation not yet threatened
    stringr::str_detect(txt, "warning|warn the institution") ~ "warning",
    # "Notice": formal notice of concern or non-compliance
    stringr::str_detect(txt, "notice of concern") ~ "notice",
    stringr::str_detect(txt, "notice") ~ "notice",
    # "Monitoring": institution under heightened scrutiny or monitoring
    stringr::str_detect(txt, "monitor") ~ "monitoring",
    # Catch-all for other adverse terminology
    stringr::str_detect(txt, "adverse") ~ "adverse_action",
    # Fallback for unrecognized text (includes branch/location substantive
    # changes that mention "close" or "teach-out" but lack institutional
    # signals -- those are administrative, not adverse).
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
# MSCHE PROCEDURAL-DROP PATTERNS (canonical, single source of truth)
# ---------------------------------------------------------------------------
#
# MSCHE per-institution pages emit large volumes of routine procedural rows
# (progress-report acknowledgments, supplemental-information requests,
# COVID-era visit delays, candidate-institution paperwork, etc.). These
# rows have no public-interest signal and were previously filtered only
# at render time by `MSCHE_PROCEDURAL_DROP_PATTERNS` in
# `js/accreditation.js`. The list now lives here as the canonical source
# and is also applied at the scrape boundary so procedural rows never
# enter the persisted CSVs / audit / export pipelines in the first place.
#
# Pattern syntax: PCRE strings, anchored with ^\s* and matched
# case-insensitively against the action label. Mirror these patterns
# (case-insensitive, anchored, same semantics) in
# `js/accreditation.js` -- when in doubt the R copy is canonical.
# Tests in `tests/test_accreditation_helpers.R` pin behavior.
#
# Hold the line on additions: per CLAUDE.md, the right fix for a
# preamble-stripped row that LOOKS procedural but ISN'T is to add a
# better short-label rule in `derive_action_label_short()`, not to
# loosen this list.
MSCHE_PROCEDURAL_DROP_PATTERNS <- c(
  # "Staff acted on behalf of the Commission" prefix is now stripped at
  # the scrape boundary, but kept here as `(?:...)?` so the patterns
  # also match historical CSV rows that retain the preamble.
  "^\\s*(?:staff acted on behalf of the commission )?to request (?:a |an )?supplemental information report",
  "^\\s*(?:staff acted on behalf of the commission )?to request (?:a |an )?monitoring report",
  "^\\s*(?:staff acted on behalf of the commission )?to request (?:a |an )?candidate assessment",
  "^\\s*(?:staff acted on behalf of the commission )?to request an? updated teach-?out plan",
  # Length cap [^.]{0,200} avoids crossing sentence boundaries while
  # tolerating long "to require ... teach-out plan" preambles.
  "^\\s*to require [^.]{0,200}?teach-?out plan",
  "^\\s*to request [^.]{0,200}?teach-?out plan",
  "^\\s*to note the follow-up team visit",
  "^\\s*to note that the complex substantive change visit occurred",
  "^\\s*to note that an? updated teach-?out plan [^.]{0,80}? will not be required",
  # COVID-19 / pandemic distance-learning waiver -- temporary
  # procedural relief, not a sanction or status change.
  "^\\s*(?:staff acted on behalf of the commission )?to temporarily waive substantive change policy",
  # Federal regulation 34 CFR 602.23(f)(1)(ii) candidate-institution
  # teach-out plans -- application paperwork, not real teach-outs.
  "^\\s*to approve the teach-?out plan as required of candidate",
  # Rejection of a previously-required teach-out plan submission --
  # procedural feedback, not a sanction action.
  "^\\s*to reject the teach-?out plan",
  # Supplemental info report deemed inadequate -- same procedural
  # feedback shape as above.
  "^\\s*to note that the supplemental information report was not conducive",
  "^\\s*(?:staff acted (?:on behalf of the commission )?)?to acknowledge receipt of",
  "^\\s*to note the (?:show cause |follow-?up |on-site |virtual )?visit by the commission'?s representatives",
  "^\\s*to note that .* hosted a virtual site visit",
  "^\\s*to note that .* (?:will not be continuing as|is now due|are now due|was not received)",
  "^\\s*to note that the institution received the notification of adverse action",
  "^\\s*to note that the administrator of the appeal",
  "^\\s*to postpone a decision on",
  "^\\s*to reject the supplemental information report",
  "^\\s*to request submission of signed teach-?out agreements",
  "^\\s*to request an updated accreditation readiness report",
  "^\\s*to remind the institution of",
  "^\\s*to grant a delay of the monitoring report",
  "^\\s*to grant accreditation because the institution has met the requirements of the addition or change of primary accreditor"
)

# Substantive-keep override: phrasings that signal a real institution-
# level event (closure, surrender, merger, true teach-out approval) and
# must NOT be dropped even when the row's preamble matches a procedural
# pattern. Saint Rose's December 2023 row is the canonical example:
# starts "To acknowledge receipt of the institution's notification ..."
# (procedural-shaped) but the body announces "intention to cease
# instruction at all locations" -- a true adverse_action.
#
# Mirrors MSCHE_SUBSTANTIVE_KEEP_PATTERN in js/accreditation.js plus the
# institution-level signals in classify_action's is_institutional_adverse.
# The R-side filter is applied to the FULL action body (not just a
# short label), so patterns search anywhere in the text rather than
# anchored at the start.
MSCHE_SUBSTANTIVE_KEEP_PATTERNS <- c(
  # JS-mirror substantive shapes
  "merger of",
  "accepted teach-?out plan",
  "to approve the (?:updated )?teach-?out plan(?! as required of candidate)",
  "to approve the teach-?out agreements?",
  "approved teach-?out plan",
  "approved teach-?out agreements?",
  "voluntar(?:ily|y) surrender(?:ed)?(?: accreditation)?",
  "to accept [^.]{0,160}?voluntar(?:ily|y) surrender",
  # Institution-level closure / sanction signals
  "the institution will close",
  "will close (?:effective|all locations|permanently)",
  "close all locations",
  "cease instruction at all locations",
  "intention to cease instruction",
  "intent to cease instruction",
  "institutional closure",
  "decision to close [^.]{0,80}?(?:the institution|all locations|all of its locations|operations|permanently)",
  "denied reaffirmation",
  "deny reaffirmation",
  "removed from membership",
  "withdraws from membership",
  "withdraw accreditation",
  "withdrawal of accreditation",
  # Substantive non-closure events tied to legal status / corporate change
  "surviving institution",
  "anticipated date of the transaction"
)

# Returns TRUE for any label that should be dropped at the scrape
# boundary: matches one of the procedural drop patterns AND does not
# match any substantive-keep override. Case-insensitive. Vectorized.
is_msche_procedural_drop <- function(label) {
  if (length(label) == 0L) return(logical(0))
  txt <- as.character(label)
  txt[is.na(txt)] <- ""
  combined_drop <- paste(MSCHE_PROCEDURAL_DROP_PATTERNS, collapse = "|")
  combined_keep <- paste(MSCHE_SUBSTANTIVE_KEEP_PATTERNS, collapse = "|")
  has_drop <- stringr::str_detect(txt, stringr::regex(combined_drop, ignore_case = TRUE))
  has_keep <- stringr::str_detect(txt, stringr::regex(combined_keep, ignore_case = TRUE))
  has_drop & !has_keep
}

# Strips the "Staff acted on behalf of the Commission [to ]" boilerplate
# preamble from MSCHE action text. Applied at the scrape boundary so
# downstream consumers (action_label_raw, notes, source keys, audit)
# see the meaningful action sentence and not the delegated-action
# preamble. Idempotent: re-running on already-stripped text is a no-op
# because the preamble pattern is anchored at the start. NA-safe and
# vectorized.
#
# Replacement is "To " (capitalized) so the post-strip string starts
# with the same casing convention as non-staff MSCHE rows ("To
# acknowledge receipt of...", "To accept the progress report.").
strip_msche_staff_preamble <- function(label) {
  if (length(label) == 0L) return(label)
  txt <- as.character(label)
  stripped <- stringr::str_replace(
    txt,
    stringr::regex(
      "^\\s*staff acted on behalf of the commission(?:\\s+to)?\\s+",
      ignore_case = TRUE
    ),
    "To "
  )
  matched <- stringr::str_detect(
    txt,
    stringr::regex(
      "^\\s*staff acted on behalf of the commission(?:\\s+to)?\\s+",
      ignore_case = TRUE
    )
  )
  needs_case_normalization <- matched & stringr::str_detect(
    stripped,
    "^To\\s+[A-Z]{2,}\\b"
  )
  if (any(needs_case_normalization, na.rm = TRUE)) {
    parts <- stringr::str_match(
      stripped[needs_case_normalization],
      "^(To\\s+)([A-Z]{2,})(\\b.*)$"
    )
    stripped[needs_case_normalization] <- paste0(
      parts[, 2],
      tolower(parts[, 3]),
      parts[, 4]
    )
  }
  stripped
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

# Extracts a trailing parenthetical *scope* note from an accreditor list item,
# e.g. the "(Master of Social Work degree at its Bedford ... locations)" suffix
# that NECHE attaches to program-level actions. Returns NA_character_ when the
# parenthetical is location metadata (e.g. "(Boston, MA)") or one of the known
# administrative notes that `extract_name_state_from_item` already discards.
extract_item_scope <- function(x) {
  items <- clean_text(x)
  match <- stringr::str_match(items, "\\(([^)]+)\\)\\s*$")
  scope <- match[, 2]
  missing <- is.na(scope) | !nzchar(scope)
  # Location parenthetical: ends in a 2-letter state abbreviation.
  is_location <- !missing & stringr::str_detect(scope, "[A-Z]{2}\\s*$")
  # Administrative notes filtered by extract_name_state_from_item.
  is_metadata <- !missing & stringr::str_detect(
    tolower(scope),
    "^(next review|letter dated|closure of|two proposals)"
  )
  scope[missing | is_location | is_metadata] <- NA_character_
  # decode_html()/clean_text() runs through vapply(..., USE.NAMES = TRUE),
  # which auto-names the output by the input character values; those names
  # ride through str_match() into the captured column. Strip them so the
  # helper returns a plain unnamed character vector (callers like
  # parse_items_to_rows treat this column as data, not a lookup map).
  unname(scope)
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
