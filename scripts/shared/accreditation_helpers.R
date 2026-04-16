# scripts/shared/accreditation_helpers.R
#
# Pure text-processing and classification helpers for the accreditation pipeline.
# Source this after utils.R inside main() in build_accreditation_actions.R.
#
# Requires: dplyr, httr2, readr, stringr, xml2 (loaded by the caller)

# ---------------------------------------------------------------------------
# HTML / text cleaning
# ---------------------------------------------------------------------------

# Strips HTML tags from x, decoding entities and <br> into newlines.
decode_html <- function(x) {
  x <- as.character(x)
  vapply(x, function(one) {
    if (is.na(one) || !nzchar(one)) return(NA_character_)
    one <- gsub("<br\\s*/?>", "\n", one, ignore.case = TRUE)
    doc <- xml2::read_html(paste0("<html><body>", one, "</body></html>"))
    xml2::xml_text(xml2::xml_find_first(doc, "//body"))
  }, character(1))
}

# Decodes HTML and collapses all whitespace to single spaces.
clean_text <- function(x) {
  x |>
    decode_html() |>
    stringr::str_replace_all("[\r\n\t]+", " ") |>
    stringr::str_replace_all("\u00a0", " ") |>
    stringr::str_squish()
}

# Normalises an institution name to a lowercase token for fuzzy matching.
normalize_name <- function(x) {
  x |>
    as.character() |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("&", " and ") |>
    stringr::str_replace_all("\\bsaint\\b", "st") |>
    stringr::str_replace_all("[^a-z0-9 ]", " ") |>
    stringr::str_squish()
}

# ---------------------------------------------------------------------------
# State name lookup
# ---------------------------------------------------------------------------

.accred_state_lookup <- c(
  setNames(state.name, state.abb),
  DC = "District of Columbia",
  PR = "Puerto Rico",
  VI = "Virgin Islands",
  GU = "Guam",
  MP = "Northern Mariana Islands",
  AS = "American Samoa"
)

# Converts a two-letter postal abbreviation to a full state name.
# Returns the original value when no match is found.
state_name <- function(x) {
  x_chr <- as.character(x)
  out   <- unname(.accred_state_lookup[toupper(x_chr)])
  out[is.na(out)] <- x_chr[is.na(out)]
  out
}

# ---------------------------------------------------------------------------
# HTTP fetch with cache fallback
# ---------------------------------------------------------------------------

# Fetches `url` and writes the response body to `cache_dir/cache_name`.
# When `refresh = FALSE`, returns the cached copy if it exists.
# On network failure, falls back to the cached copy if available.
fetch_html_text <- function(url, cache_name, cache_dir, refresh = TRUE) {
  cache_path <- file.path(cache_dir, cache_name)
  if (!refresh && file.exists(cache_path)) {
    return(readr::read_file(cache_path))
  }
  tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_user_agent("FinancialHealthProject/1.0") |>
      httr2::req_perform()
    body <- httr2::resp_body_string(resp)
    readr::write_file(body, cache_path)
    body
  }, error = function(e) {
    if (file.exists(cache_path)) {
      message("Falling back to cached copy for ", url)
      readr::read_file(cache_path)
    } else {
      stop("Failed to fetch ", url, ": ", e$message)
    }
  })
}

# ---------------------------------------------------------------------------
# Page metadata extraction
# ---------------------------------------------------------------------------

# Extracts the ISO date from an Open Graph article:modified_time meta tag.
extract_page_modified_date <- function(html) {
  match <- stringr::str_match(
    html,
    "article:modified_time\" content=\"([0-9]{4}-[0-9]{2}-[0-9]{2})"
  )
  match[, 2]
}

# Extracts and cleans the <title> element from raw HTML.
extract_page_title <- function(html) {
  title_match <- stringr::str_match(html, "<title>(.*?)</title>")
  clean_text(title_match[, 2])
}

# ---------------------------------------------------------------------------
# Action / status classification
# ---------------------------------------------------------------------------

# Maps raw accreditor action text to a canonical action type.
classify_action <- function(raw_action, accreditor = NA_character_) {
  txt <- stringr::str_to_lower(as.character(raw_action))
  dplyr::case_when(
    stringr::str_detect(txt, "removed from warning|remove notice of concern|removal of sanction") ~ "removed",
    stringr::str_detect(txt, "removed from probation") ~ "removed",
    stringr::str_detect(txt, "show cause") ~ "show_cause",
    stringr::str_detect(txt, "closure") ~ "adverse_action",
    stringr::str_detect(txt, "removed from membership|withdrawal|withdraws from membership|withdraw candidate|withdraw accreditation") ~ "adverse_action",
    stringr::str_detect(txt, "probation") ~ "probation",
    stringr::str_detect(txt, "warning") ~ "warning",
    stringr::str_detect(txt, "notice of concern") ~ "notice",
    stringr::str_detect(txt, "notice") ~ "notice",
    stringr::str_detect(txt, "monitor") ~ "monitoring",
    stringr::str_detect(txt, "adverse") ~ "adverse_action",
    TRUE ~ "other"
  )
}

# Returns "resolved" or "active" based on the raw action text.
classify_status <- function(raw_action) {
  txt <- stringr::str_to_lower(as.character(raw_action))
  dplyr::case_when(
    stringr::str_detect(txt, "removed from warning|removed from probation|removal of sanction|remove notice of concern|removed notice of concern") ~ "resolved",
    TRUE ~ "active"
  )
}

# Returns TRUE when the action text contains public-action keywords.
has_public_action_keywords <- function(x) {
  txt <- stringr::str_to_lower(as.character(x))
  stringr::str_detect(
    txt,
    "warning|probation|show cause|notice of concern|notice|withdrawal of accreditation|withdraws from membership|closure|adverse"
  )
}

# ---------------------------------------------------------------------------
# Name/state parsing
# ---------------------------------------------------------------------------

# Parses a "Institution Name, City, ST" or "Institution Name, ST" string into
# a named list with institution_name_raw and institution_state_raw.
extract_name_state_from_item <- function(x) {
  item <- clean_text(x)
  item <- stringr::str_remove(
    item,
    "\\s*\\((next review|letter dated|closure of|two proposals).*?\\)$"
  )
  item <- stringr::str_squish(item)

  match <- stringr::str_match(item, "^(.*?),\\s*[^,]+,\\s*([A-Z]{2})$")
  if (!is.na(match[1, 1])) {
    return(list(institution_name_raw = clean_text(match[1, 2]),
                institution_state_raw = state_name(match[1, 3])))
  }

  match <- stringr::str_match(item, "^(.*?),\\s*([A-Z]{2})$")
  if (!is.na(match[1, 1])) {
    return(list(institution_name_raw = clean_text(match[1, 2]),
                institution_state_raw = state_name(match[1, 3])))
  }

  match <- stringr::str_match(item, "^(.*?)\\s*\\(([^)]+)\\)$")
  if (!is.na(match[1, 1])) {
    state_abbr <- stringr::str_match(match[1, 3], "([A-Z]{2})$")[, 2]
    return(list(institution_name_raw = clean_text(match[1, 2]),
                institution_state_raw = state_name(state_abbr)))
  }

  list(institution_name_raw  = item,
       institution_state_raw = NA_character_)
}

# ---------------------------------------------------------------------------
# Institution matching helper
# ---------------------------------------------------------------------------

# Joins a normalised actions data frame to the tracker via two strategies:
#   1. normalized name + state  (most specific, fewest false positives)
#   2. normalized name only     (catches cross-state awards)
# Adds columns: unitid, tracker_name, tracker_state, match_method.
# Intermediate join columns are dropped before returning.
#
# actions_df must already contain institution_name_normalized and
# institution_state_normalized.  Both lookup tables must contain columns
# matched_unitid, tracker_name, tracker_state, and norm_name; lookup_exact
# must also contain state_match.
match_institutions_to_tracker <- function(actions_df, lookup_exact, lookup_name_only) {
  actions_df |>
    dplyr::left_join(
      lookup_exact,
      by = c(
        "institution_name_normalized" = "norm_name",
        "institution_state_normalized" = "state_match"
      )
    ) |>
    dplyr::rename(
      exact_unitid       = matched_unitid,
      exact_tracker_name = tracker_name,
      exact_tracker_state = tracker_state
    ) |>
    dplyr::left_join(
      lookup_name_only,
      by = c("institution_name_normalized" = "norm_name")
    ) |>
    dplyr::rename(
      name_only_unitid        = matched_unitid,
      name_only_tracker_name  = tracker_name,
      name_only_tracker_state = tracker_state
    ) |>
    dplyr::mutate(
      unitid       = dplyr::coalesce(exact_unitid, name_only_unitid),
      tracker_name  = dplyr::coalesce(exact_tracker_name, name_only_tracker_name),
      tracker_state = dplyr::coalesce(exact_tracker_state, name_only_tracker_state),
      match_method  = dplyr::case_when(
        !is.na(exact_unitid)                            ~ "normalized_name_plus_state",
        is.na(exact_unitid) & !is.na(name_only_unitid) ~ "normalized_name_only",
        TRUE                                            ~ "unmatched"
      )
    ) |>
    dplyr::select(
      -exact_unitid, -exact_tracker_name, -exact_tracker_state,
      -name_only_unitid, -name_only_tracker_name, -name_only_tracker_state
    )
}
