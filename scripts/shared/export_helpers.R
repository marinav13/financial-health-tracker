# scripts/shared/export_helpers.R
#
# Pure utility functions for building site JSON exports.
# Source this after utils.R inside main() in build_web_exports.R.
#
# DEPENDENCIES:
#   - Packages: dplyr, jsonlite, readr (usually pre-loaded by build_web_exports.R)
#   - Functions: %||% operator from utils.R
#   - Functions: to_num() from utils.R

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

# Stops with an error if the file doesn't exist, showing the user how to fix it.
require_local_file <- function(path, label, how_to_fix) {
  if (file.exists(path)) return(invisible(path))
  stop(
    paste(
      "Missing required local input for the website build:", label,
      "\nExpected path:", path,
      "\nHow to fix:", how_to_fix
    ),
    call. = FALSE
  )
}

# Adds any missing columns to a data frame using values from a defaults list.
ensure_columns <- function(df, defaults) {
  for (nm in names(defaults)) {
    if (!nm %in% names(df)) df[[nm]] <- defaults[[nm]]
  }
  df
}

# ---------------------------------------------------------------------------
# Scalar coercion helpers
# ---------------------------------------------------------------------------

# Converts empty or whitespace-only strings to NA.
null_if_empty <- function(x) {
  x <- trimws(as.character(x %||% ""))
  ifelse(x == "", NA_character_, x)
}

# Converts a decimal ratio (0-1) to a percentage (0-100).
scale_ratio_to_pct <- function(x) {
  value <- to_num(x)   # to_num from utils.R
  ifelse(is.na(value), NA_real_, value * 100)
}

# Returns the first non-missing, non-empty value from a vector, or NA if none found.
# Empty strings count as missing.
or_null <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA)
  value <- x[[1]]
  if (is.character(value)) {
    value <- trimws(value)
    if (identical(value, "")) return(NA)
  }
  value
}

# Like or_null() but converts Date objects to ISO 8601 strings ("YYYY-MM-DD").
or_null_date <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA)
  value <- x[[1]]
  if (inherits(value, "Date")) {
    if (is.na(value)) return(NA)
    return(as.character(value))
  }
  if (is.character(value)) {
    value <- trimws(value)
    if (identical(value, "")) return(NA)
  }
  value
}

# ---------------------------------------------------------------------------
# Export ID / label normalization
# ---------------------------------------------------------------------------

# Normalises an institution name into a stable URL-slug component.
# Applies the same abbreviation expansions used by the name-matching pipeline
# (normalize_name in build_college_cuts_join.R) so that minor API-side
# variations â€” "St." vs "Saint", leading "The", "&" vs "and" â€” all produce
# the same slug and do not silently change a school's public URL.
#
# IMPORTANT: do NOT change the output format of this helper once schools are
# published.  Any change breaks existing bookmarked URLs for unmatched schools.
slug_institution_name <- function(name) {
  s <- tolower(trimws(name %||% ""))
  s <- sub("^the +", "", s)                      # strip leading "The "
  s <- gsub("\\bst\\.?\\b", "saint", s)          # St / St. â†’ saint
  s <- gsub("&", "and", s, fixed = TRUE)         # & â†’ and
  s <- gsub("[^a-z0-9]+", "-", s)                # non-alnum â†’ hyphen
  s <- gsub("^-+|-+$", "", s)                    # trim edge hyphens
  s
}

# Builds a stable, URL-safe identifier for a school.
#
# Strategy:
#   1. If a numeric IPEDS unitid is available it is returned as-is (no prefix).
#      This is the only stable identifier and must be preserved as-is because
#      the financial-profile page URL is data/schools/{unitid}.json.
#   2. Otherwise a deterministic slug is built from the normalised institution
#      name and state, separated by "--" so the two parts are unambiguous.
#      The prefix ("cut", "research", etc.) namespaces the slug to avoid
#      collisions with numeric unitids.
#
# When a school transitions from unmatched â†’ matched (gains a unitid), its ID
# will change from a slug to the numeric unitid.  This is unavoidable without
# a separate persistent ID store, but it only affects unmatched schools and
# is logged at export time (see build_web_exports.R).
make_export_id <- function(prefix, unitid, institution_name, state) {
  raw_unitid <- trimws(as.character(unitid %||% ""))
  if (!identical(raw_unitid, "")) return(raw_unitid)

  # No unitid â€” build slug from normalised name + state.
  name_slug  <- slug_institution_name(institution_name)
  state_slug <- gsub("[^a-z0-9]+", "-", tolower(trimws(state %||% "")))
  state_slug <- gsub("^-+|-+$", "", state_slug)

  # "--" separator makes name and state visually distinct in the URL and avoids
  # false collisions where a name ends with the same token as the state.
  body <- if (nzchar(state_slug)) paste0(name_slug, "--", state_slug) else name_slug
  paste0(prefix, "-", body)
}

# Standardizes control/ownership labels to canonical display strings
# (e.g., "Private non-profit" -> "Private not-for-profit").
normalize_control_label <- function(x) {
  value <- trimws(as.character(x %||% ""))
  if (!nzchar(value)) return(NA_character_)
  dplyr::case_when(
    grepl("^public$",                              value, ignore.case = TRUE) ~ "Public",
    grepl("private (?:non-profit|not-for-profit)", value, ignore.case = TRUE) ~ "Private not-for-profit",
    grepl("private for-profit",                    value, ignore.case = TRUE) ~ "Private for-profit",
    TRUE ~ value
  )
}

# Expands abbreviated or alternate institution names to their official forms
# (e.g., "Arizona State University Campus Immersion" -> "Arizona State University").
normalize_display_institution_name <- function(x) {
  value <- trimws(as.character(x %||% ""))
  dplyr::case_when(
    identical(value, "Arizona State University Campus Immersion") ~ "Arizona State University",
    TRUE ~ value
  )
}

# Builds the common "Institution | City | State" display string used on the site.
build_institution_unique_name <- function(institution_name, city, state) {
  paste(
    na.omit(c(
      normalize_display_institution_name(institution_name),
      city,
      state
    )),
    collapse = " | "
  )
}

# Returns TRUE if the Carnegie classification indicates a primarily
# bachelor's-granting institution.
is_primary_bachelors_category <- function(x) {
  value <- as.character(x %||% "")
  grepl("primarily baccalaureate or above",     value, ignore.case = TRUE) &
    !grepl("not primarily baccalaureate or above", value, ignore.case = TRUE)
}

# Returns TRUE if an institution name strongly suggests a 2-year / community /
# technical college that should NOT appear in the primary (4-year) tracker table.
# Used for unmatched API records that lack a Carnegie classification from IPEDS.
# False negatives (2-year schools slipping through) are preferable to false
# positives (4-year schools being hidden), so the patterns are conservative.
is_likely_2year <- function(x) {
  value <- trimws(as.character(x %||% ""))
  grepl(
    paste(
      "\\bcommunity college\\b",
      "\\btechnical college\\b",
      "\\btechnical community college\\b",
      "\\bvocational\\b",
      "\\btech college\\b",
      "\\bstate college of technology\\b",
      "\\bcollege of technology\\b",
      "\\bcc\\b",
      "ivy tech",
      sep = "|"
    ),
    value,
    ignore.case = TRUE,
    perl        = TRUE
  )
}

# ---------------------------------------------------------------------------
# Domain-specific helpers
# ---------------------------------------------------------------------------

# Extracts the count of affected positions from free-text fields using regex patterns.
# Looks for patterns like "cutting [N] positions" or "laid off [N]" in the combined text.
derive_positions_affected <- function(faculty_affected, notes, source_title,
                                       program_name, cut_type) {
  # First, check if an explicit count was provided
  explicit <- to_num(faculty_affected)
  if (!is.na(explicit) && explicit > 0) return(as.integer(explicit))

  # Combine all text fields into one searchable string
  text <- paste(notes %||% "", source_title %||% "", program_name %||% "", sep = " ")
  text <- gsub(",", "", text, fixed = TRUE)
  text <- trimws(text)
  if (!nzchar(text)) return(NA_integer_)

  # Check if the text contains a layoff signal (keywords or cut_type indicators)
  layoff_signal <-
    grepl("layoff|laid off|positions|position|employees|employee|staff members|faculty",
          text, ignore.case = TRUE) ||
    grepl("staff_layoff|faculty_layoff", as.character(cut_type %||% ""), ignore.case = TRUE)
  if (!layoff_signal) return(NA_integer_)

  # Try matching a series of regex patterns to extract a numeric count
  patterns <- c(
    "cutting\\s+([0-9]{1,4})\\s+positions",
    "lays? off\\s+([0-9]{1,4})",
    "laid off\\s+([0-9]{1,4})",
    "([0-9]{1,4})\\s+positions\\s+affected",
    "([0-9]{1,4})\\s+(?:employees|employee|staff members|staff|faculty(?: members)?)"
  )
  for (pattern in patterns) {
    matched <- regexec(pattern, text, ignore.case = TRUE)
    value   <- regmatches(text, matched)[[1]]
    if (length(value) >= 2) {
      parsed <- suppressWarnings(as.integer(value[2]))
      if (!is.na(parsed) && parsed > 0) return(parsed)
    }
  }

  NA_integer_
}

# Constructs a narrative sentence describing international student enrollment.
# Includes undergrad vs. graduate breakdown when both are available.
build_international_students_sentence <- function(year, all_pct, ug_pct, grad_pct) {
  all_v  <- scale_ratio_to_pct(all_pct)
  ug_v   <- scale_ratio_to_pct(ug_pct)
  grad_v <- scale_ratio_to_pct(grad_pct)
  dplyr::case_when(
    is.na(all_v) ~ NA_character_,
    !is.na(ug_v) & !is.na(grad_v) ~ paste0(
      "In ", year, ", ", round(all_v, 1), "% of students were international. ",
      "That includes ", round(ug_v, 1), "% of undergraduates and ",
      round(grad_v, 1), "% of graduate students."
    ),
    TRUE ~ paste0("In ", year, ", ", round(all_v, 1), "% of students were international.")
  )
}

# ---------------------------------------------------------------------------
# JSON / series helpers
# ---------------------------------------------------------------------------

# Writes an R object to a JSON file with pretty formatting.
# NA values become JSON null (not "NA"), and single-element vectors stay as scalars.
# Also strips ALL null bytes (not just trailing) that Windows/mounted filesystems sometimes inject,
# then validates the result parses as JSON before promoting the file.
# Uses atomic write: write to .tmp then rename for crash safety.
write_json_file <- function(x, path) {
  tmp_path <- paste0(path, ".tmp")
  
  # Clean up orphaned tmp file if it exists
  if (file.exists(tmp_path)) {
    file.remove(tmp_path)
  }
  
  jsonlite::write_json(x, path = tmp_path, pretty = TRUE, auto_unbox = TRUE, na = "null")
  # Strip ALL null bytes (not just trailing) so browsers can JSON.parse() the file cleanly.
  # Null bytes can appear anywhere when Windows/mounted filesystems corrupt writes.
  raw_bytes <- readBin(tmp_path, raw(), n = file.info(tmp_path)$size)
  null_positions <- which(raw_bytes == as.raw(0x00))
  if (length(null_positions) > 0) {
    if (length(null_positions) > length(raw_bytes) * 0.01) {
      warning(sprintf(
        "write_json_file: %d null bytes (%.1f%%) found in %s â€” possible file corruption",
        length(null_positions),
        100 * length(null_positions) / length(raw_bytes),
        basename(path)
      ))
    }
    clean_bytes <- raw_bytes[-null_positions]
    con <- file(tmp_path, "wb")
    writeBin(clean_bytes, con)
    close(con)
  }
  # Validate the result actually parses as JSON before promoting to final path
  tryCatch(
    jsonlite::fromJSON(tmp_path, simplifyVector = FALSE),
    error = function(e) stop(sprintf(
      "write_json_file: output file failed JSON validation for %s: %s",
      basename(path), conditionMessage(e)
    ))
  )
  
  # Atomic rename: fall back to copy+remove on Windows/cross-filesystem failure
  rename_ok <- tryCatch({
    file.rename(tmp_path, path)
  }, error = function(e) FALSE)
  
  if (!rename_ok) {
    if (file.exists(path)) file.remove(path)
    file.copy(tmp_path, path)
    file.remove(tmp_path)
  }
}

# Converts a data frame of (year, value) pairs into the [{year, value}, ...] format
# expected by the frontend.
build_series <- function(df, value_col, scale = 1) {
  keep <- !is.na(df[[value_col]])
  if (!any(keep)) return(list())
  rows <- df[keep, c("year", value_col), drop = FALSE]
  series_rows <- lapply(seq_len(nrow(rows)), function(i) {
    value <- to_num(rows[[value_col]][[i]])
    if (is.na(value)) return(NULL)
    list(
      year  = as.integer(rows$year[[i]]),
      value = unname(value) * scale
    )
  })
  Filter(Negate(is.null), series_rows)
}

# Returns the first non-missing value from a set of candidate columns, in order.
# Useful for handling optional fields where multiple names might be used.
pick_first_present <- function(df, candidates) {
  present <- candidates[candidates %in% names(df)]
  if (!length(present)) return(rep(NA_character_, nrow(df)))
  values <- lapply(present, function(col) as.character(df[[col]]))
  Reduce(function(x, y) dplyr::coalesce(x, y), values)
}

# ---------------------------------------------------------------------------
# Export bundle helpers
# ---------------------------------------------------------------------------

# Assembles the common landing-page index fields from a school list object
# produced by build_*_export(). Extra export-specific fields are passed via
# `...` and appended after the shared base fields.
build_school_index_entry <- function(school, ...) {
  c(
    list(
      unitid                  = school$unitid,
      financial_unitid        = school$financial_unitid,
      has_financial_profile   = school$has_financial_profile,
      is_primary_tracker      = school$is_primary_tracker,
      institution_name        = school$institution_name,
      institution_unique_name = build_institution_unique_name(
        school$institution_name,
        school$city,
        school$state
      ),
      state         = school$state,
      city          = school$city,
      control_label = school$control_label,
      category      = school$category
    ),
    list(...)
  )
}

# Builds a landing-page index from a list of school export objects, adding any
# export-specific extra fields returned by `extra_builder`.
build_school_index <- function(schools, extra_builder = function(school) list()) {
  lapply(schools, function(school) {
    extras <- extra_builder(school)
    if (is.null(extras)) extras <- list()
    do.call(build_school_index_entry, c(list(school), extras))
  })
}

# Writes an export JSON file and, optionally, a matching school index file.
# Returns NULL when `export_obj` is NULL; otherwise returns the written paths.
write_export_bundle <- function(export_obj, data_dir, export_filename,
                                index_filename = NULL,
                                index_builder = function(school) list()) {
  if (is.null(export_obj)) return(NULL)

  export_path <- file.path(data_dir, export_filename)
  write_json_file(export_obj, export_path)

  index_path <- NULL
  if (!is.null(index_filename)) {
    index_path <- file.path(data_dir, index_filename)
    write_json_file(
      build_school_index(export_obj$schools, index_builder),
      index_path
    )
  }

  list(export_path = export_path, index_path = index_path)
}

# Builds a named lookup vector by summarising `value_col` within `group_col`.
# `summarizer` receives the grouped vector and should return a scalar numeric.
build_group_value_lookup <- function(df, group_col, value_col, summarizer) {
  df %>%
    dplyr::group_by(.data[[group_col]]) %>%
    dplyr::summarise(value = summarizer(.data[[value_col]]), .groups = "drop") %>%
    dplyr::filter(!is.na(.data[[group_col]]), !is.na(value)) %>%
    { stats::setNames(.$value, .[[group_col]]) }
}

# ---------------------------------------------------------------------------
# H8: JSON schema validation for exported files
# ---------------------------------------------------------------------------

# Navigates a parsed JSON object along a list of string keys, then returns the
# first entry from the resulting container (which may be a positional array or
# a named dict).  Returns NULL if the path is unreachable or the container is
# empty.
#
# Examples:
#   get_first_entry(obj, list())            -> obj[[1]]   (obj is array or dict)
#   get_first_entry(obj, list("schools"))   -> obj$schools[[1]]
#   get_first_entry(obj, list("files"))     -> obj$files[[1]]
.get_first_json_entry <- function(obj, path_to_entries) {
  current <- obj
  for (key in path_to_entries) {
    if (!is.list(current) || !key %in% names(current)) return(NULL)
    current <- current[[key]]
  }
  if (length(current) == 0L) return(NULL)
  current[[1L]]
}

# Validates a single JSON export file against a lightweight schema.
#
# Arguments:
#   path              : absolute path to the JSON file
#   required_top_keys : character vector of keys that must exist at the top
#                       level of the JSON object (ignored when the top-level
#                       value is an array rather than an object)
#   path_to_entries   : list of string keys used to navigate from the top-level
#                       object to the container of individual entries.
#                       Use list() to treat the top-level value itself as the
#                       entries container (handles both arrays and dicts).
#                       Use list("schools") to drill into obj$schools first.
#                       Set to NULL to skip entry-level validation entirely.
#   required_entry_keys : character vector of keys that must appear in each
#                         entry (checked against the FIRST entry only; an
#                         empty/missing container skips this check)
#   label             : human-readable label used in error messages (defaults
#                       to the file's basename)
#
# Returns a character vector of error strings (length 0 when valid).
validate_json_schema <- function(path,
                                  required_top_keys   = character(0),
                                  path_to_entries     = NULL,
                                  required_entry_keys = character(0),
                                  label               = basename(path)) {
  errors <- character(0)

  if (!file.exists(path)) {
    return(sprintf("%s: file does not exist", label))
  }

  obj <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(obj)) {
    return(sprintf("%s: file is not valid JSON", label))
  }

  # Top-level key checks (only meaningful when obj is an object, not an array)
  if (is.list(obj) && !is.null(names(obj))) {
    for (k in required_top_keys) {
      if (!k %in% names(obj)) {
        errors <- c(errors, sprintf("%s: missing required top-level key '%s'", label, k))
      }
    }
  }

  # Entry-level key checks
  if (!is.null(path_to_entries) && length(required_entry_keys) > 0L) {
    first_entry <- .get_first_json_entry(obj, path_to_entries)
    if (!is.null(first_entry)) {
      for (k in required_entry_keys) {
        if (!k %in% names(first_entry)) {
          errors <- c(errors, sprintf(
            "%s: entry missing required key '%s'", label, k
          ))
        }
      }
    }
    # An empty container is not an error â€” the file might legitimately have
    # zero entries (e.g., no college cuts yet this cycle).
  }

  errors
}

# Canonical schemas for every standard export file written by build_web_exports.R.
# Each entry is a named list with:
#   filename            : basename of the JSON file in the data directory
#   required_top_keys   : see validate_json_schema()
#   path_to_entries     : see validate_json_schema()
#   required_entry_keys : see validate_json_schema()
EXPORT_SCHEMAS <- list(
  list(
    filename            = "schools_index.json",
    required_top_keys   = character(0),   # top level is an array
    path_to_entries     = list(),         # array IS the entries
    required_entry_keys = c("unitid", "institution_name", "state")
  ),
  list(
    filename            = "accreditation.json",
    required_top_keys   = c("generated_at", "schools"),
    path_to_entries     = list("schools"),
    required_entry_keys = c("unitid", "institution_name", "actions")
  ),
  list(
    filename            = "accreditation_index.json",
    required_top_keys   = character(0),   # top level is a unitid-keyed dict
    path_to_entries     = list(),
    required_entry_keys = c("unitid", "institution_name", "action_count")
  ),
  list(
    filename            = "college_cuts.json",
    required_top_keys   = c("generated_at", "schools"),
    path_to_entries     = list("schools"),
    required_entry_keys = c("unitid", "institution_name", "cuts")
  ),
  list(
    filename            = "college_cuts_index.json",
    required_top_keys   = character(0),
    path_to_entries     = list(),
    required_entry_keys = c("unitid", "institution_name", "cut_count")
  ),
  list(
    filename            = "research_funding.json",
    required_top_keys   = c("generated_at", "agencies", "schools"),
    path_to_entries     = list("schools"),
    required_entry_keys = c("unitid", "institution_name", "grants")
  ),
  list(
    filename            = "research_funding_index.json",
    required_top_keys   = character(0),
    path_to_entries     = list(),
    required_entry_keys = c("unitid", "institution_name", "total_disrupted_grants")
  ),
  list(
    filename            = "closure_status_by_unitid.json",
    required_top_keys   = c("as_of_date", "schools"),
    path_to_entries     = list("schools"),
    required_entry_keys = c("unitid", "institution_name")
  ),
  list(
    filename            = "federal_composite_scores_by_unitid.json",
    required_top_keys   = c("generated_at", "schools"),
    path_to_entries     = list("schools"),
    required_entry_keys = c("unitid", "institution_name")
  ),
  list(
    filename            = "hcm2_by_unitid.json",
    required_top_keys   = c("generated_at", "schools"),
    path_to_entries     = list("schools"),
    required_entry_keys = c("unitid", "institution_name")
  ),
  list(
    filename            = "metadata.json",
    required_top_keys   = c("generated_at", "files"),
    path_to_entries     = NULL,   # 'files' is a nameâ†’path dict, not an array of objects
    required_entry_keys = character(0)
  ),
  list(
    filename            = "rankings.json",
    required_top_keys   = c("generated_at", "lists"),
    path_to_entries     = NULL,           # 'lists' has a non-standard structure
    required_entry_keys = character(0)
  )
)

# Validates all standard export JSON files in `data_dir` against EXPORT_SCHEMAS.
# Skips files that do not exist (they may not have been built yet in this run).
# Stops with a combined error message listing all schema violations found.
#
# Call this at the end of build_web_exports.R to catch structural regressions
# early, before the broken files are deployed.
validate_all_export_schemas <- function(data_dir) {
  all_errors <- character(0)

  for (schema in EXPORT_SCHEMAS) {
    path <- file.path(data_dir, schema$filename)
    if (!file.exists(path)) next   # not built in this run â€” skip silently

    errs <- validate_json_schema(
      path                = path,
      required_top_keys   = schema$required_top_keys,
      path_to_entries     = schema$path_to_entries,
      required_entry_keys = schema$required_entry_keys
    )
    all_errors <- c(all_errors, errs)
  }

  if (length(all_errors) > 0L) {
    stop(
      paste0(
        "H8 JSON schema validation failed for ", length(all_errors),
        " check(s):\n  ",
        paste(all_errors, collapse = "\n  ")
      ),
      call. = FALSE
    )
  }

  message(sprintf(
    "H8: all export JSON schemas valid (%d file(s) checked)",
    sum(vapply(EXPORT_SCHEMAS, function(s) file.exists(file.path(data_dir, s$filename)), logical(1)))
  ))
  invisible(TRUE)
}

# Runs a set of export bundle specs, each with a builder plus filenames, and
# returns a named list of written paths keyed by the spec name.
write_export_bundles <- function(specs, data_dir) {
  results <- vector("list", length(specs))
  names(results) <- names(specs)

  for (nm in names(specs)) {
    spec           <- specs[[nm]]
    export_obj     <- spec$builder()
    index_filename <- if ("index_filename" %in% names(spec)) spec$index_filename else NULL
    index_builder  <- if ("index_builder"  %in% names(spec)) spec$index_builder  else function(school) list()
    results[[nm]] <- write_export_bundle(
      export_obj      = export_obj,
      data_dir        = data_dir,
      export_filename = spec$export_filename,
      index_filename  = index_filename,
      index_builder   = index_builder
    )
  }

  results
}


# ---------------------------------------------------------------------------
# Compact display label for the global "Recent accreditation actions" table.
# ---------------------------------------------------------------------------
#
# Phase 2 of the action-label-short rollout. Non-MSCHE accreditors pass
# through unchanged because their scrapers extract concise informative
# labels at scrape time (HLC's "On Probation", NECHE's "Accepted Teach-Out
# Plan", SACSCOC's PDF-derived sentence, etc.). MSCHE per-institution rows
# carry verbatim 200-500 char board-action sentences; we apply a small
# controlled pattern set to extract a readable summary, plus a clean
# fallback (strip the "acknowledge receipt of ..." preamble, return the
# first remaining sentence) when no pattern matches.
#
# Patterns are deliberately verb+noun anchored to avoid the naive-keyword
# false positives the Phase 1 attempt produced (notably matching
# "teach-out plan ... not necessary" as a teach-out approval). Each pattern
# is paired with a regression test in tests/test_export_helpers.R.

# Word-form -> integer for "Continued on Warning for twelve months" style
# duration rendering. Limited to 1-12 because that's the corpus of values
# MSCHE actually uses; out-of-range words drop to no-duration output.
.MSCHE_DURATION_WORD_TO_NUM <- c(
  one = 1L, two = 2L, three = 3L, four = 4L, five = 5L,
  six = 6L, seven = 7L, eight = 8L, nine = 9L, ten = 10L,
  eleven = 11L, twelve = 12L
)

.normalize_action_summary_text <- function(x) {
  value <- as.character(x %||% "")
  value <- stringr::str_replace_all(value, "[\r\n\t]+", " ")
  value <- stringr::str_replace_all(value, "\u00a0", " ")
  value <- stringr::str_replace_all(value, "\uf0b7", " ")
  value <- stringr::str_replace_all(value, "â€™|â€˜", "'")
  value <- stringr::str_replace_all(value, "â€œ|â€", "\"")
  value <- stringr::str_replace_all(value, "â€“|â€”", "-")
  spaced_word_patterns <- c(
    "w\\s*a\\s*r\\s*n\\s*i\\s*n\\s*g" = "Warning",
    "p\\s*r\\s*o\\s*b\\s*a\\s*t\\s*i\\s*o\\s*n" = "Probation",
    "a\\s*c\\s*c\\s*r\\s*e\\s*d\\s*i\\s*t\\s*a\\s*t\\s*i\\s*o\\s*n" = "accreditation",
    "f\\s*i\\s*n\\s*a\\s*n\\s*c\\s*i\\s*a\\s*l" = "financial",
    "r\\s*e\\s*s\\s*o\\s*u\\s*r\\s*c\\s*e\\s*s" = "resources",
    "r\\s*e\\s*s\\s*p\\s*o\\s*n\\s*s\\s*i\\s*b\\s*i\\s*l\\s*i\\s*t\\s*y" = "responsibility"
  )
  for (pattern in names(spaced_word_patterns)) {
    value <- stringr::str_replace_all(
      value,
      stringr::regex(pattern, ignore_case = TRUE),
      spaced_word_patterns[[pattern]]
    )
  }
  value <- stringr::str_replace_all(
    value,
    stringr::regex("\\bfaure to comply\\b", ignore_case = TRUE),
    "failure to comply"
  )
  value <- stringr::str_replace_all(
    value,
    stringr::regex("\\bfor for failure\\b", ignore_case = TRUE),
    "for failure"
  )
  stringr::str_squish(value)
}

# Shared sanction ordering for export-time compaction/source selection.
# "notice" and "monitoring_or_notice" are not semantically identical, but they
# intentionally share the same lowest ordering level for compaction purposes.
.ACCREDITATION_SANCTION_STRENGTH <- c(
  notice = 1L,
  monitoring_or_notice = 1L,
  warning = 2L,
  show_cause = 3L,
  probation = 4L,
  adverse_action = 5L,
  withdrawal_or_loss = 5L
)

.normalize_action_family_for_strength <- function(x) {
  value <- tolower(trimws(as.character(x %||% "")))
  if (!nzchar(value)) return(NA_character_)
  dplyr::case_when(
    value %in% names(.ACCREDITATION_SANCTION_STRENGTH) ~ value,
    TRUE ~ value
  )
}

get_accreditation_sanction_strength <- function(x) {
  family <- .normalize_action_family_for_strength(x)
  if (is.na(family) || !family %in% names(.ACCREDITATION_SANCTION_STRENGTH)) {
    return(NA_integer_)
  }
  unname(.ACCREDITATION_SANCTION_STRENGTH[[family]])
}

# Keep these aligned with the anchored shapes in js/accreditation.js
# MSCHE_PROCEDURAL_DROP_PATTERNS so source-selection scoring and frontend
# procedural filtering do not drift apart.
.MSCHE_SOURCE_SELECTION_PROCEDURAL_PATTERNS <- c(
  "^\\s*(?:staff acted on behalf of the commission )?to request (?:a |an )?supplemental information report",
  "^\\s*(?:staff acted on behalf of the commission )?to request (?:a |an )?monitoring report",
  "^\\s*(?:staff acted on behalf of the commission )?to request (?:a |an )?candidate assessment",
  "^\\s*(?:staff acted on behalf of the commission )?to request an? updated teach-?out plan",
  "^\\s*to require [^.]{0,200}?teach-?out plan",
  "^\\s*to request [^.]{0,200}?teach-?out plan",
  "^\\s*to note the follow-?up team visit",
  "^\\s*to note that the complex substantive change visit occurred",
  "^\\s*to note that an? updated teach-?out plan [^.]{0,80}? will not be required",
  "^\\s*(?:staff acted on behalf of the commission )?to temporarily waive substantive change policy",
  "^\\s*to approve the teach-?out plan as required of candidate",
  "^\\s*to reject the teach-?out plan",
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

.WSCUC_SOURCE_SELECTION_PROCEDURAL_PATTERNS <- c(
  "^\\s*heightened monitoring or focused review\\s*$",
  "^\\s*warning or equivalent-factors affecting academic quality\\s*$",
  "^\\s*probation or equivalent or a more severe status:\\s*(warning|probation|show cause)\\s*$",
  "^\\s*removal of monitoring status\\s*$"
)

.strip_action_source_selection_wrapper <- function(text, accreditor = NA_character_) {
  value <- .normalize_action_summary_text(text)
  acc_norm <- toupper(trimws(as.character(accreditor %||% "")))
  if (!nzchar(value)) return(value)

  patterns <- switch(
    acc_norm,
    "MSCHE" = .MSCHE_SOURCE_SELECTION_PROCEDURAL_PATTERNS,
    "WSCUC" = .WSCUC_SOURCE_SELECTION_PROCEDURAL_PATTERNS,
    character()
  )
  if (length(patterns) == 0) return(value)

  for (pattern in patterns) {
    value <- stringr::str_replace(
      value,
      stringr::regex(pattern, ignore_case = TRUE),
      ""
    )
    value <- stringr::str_squish(value)
  }
  if (identical(acc_norm, "MSCHE")) {
    value <- stringr::str_replace(
      value,
      stringr::regex(
        "^\\s*(?:the )?(?:monitoring|show cause|supplemental information) report\\.?\\s*",
        ignore_case = TRUE
      ),
      ""
    )
    value <- stringr::str_squish(value)
  }
  value
}

.ACCR_EDITORIAL_CONCERN_PATTERNS <- c(
  "financial",
  "resources",
  "cash flow",
  "governance",
  "planning",
  "institutional improvement",
  "integrity",
  "enrollment",
  "student achievement",
  "audit",
  "teach-?out"
)

.WSCUC_NAMED_CONCERN_PATTERNS <- list(
  financial_sustainability = c(
    "financial sustainability",
    "fiscal sustainability",
    "fiscal stability",
    "financial stability",
    "fiscal health",
    "long-?term sustainability",
    "long-?term viability",
    "fiscal viability",
    "limited cash flow",
    "recurring operating deficits",
    "revenue uncertainty",
    "declining enrollments?",
    "operating expenses exceeding revenues"
  ),
  resource_planning = c(
    "resource planning",
    "multi-?year financial plan",
    "realistic multi-?year(?:,? scenario-based)? financial plans?",
    "budget(?:ary)? plans",
    "resource allocation"
  ),
  quality_assurance = c(
    "quality assurance processes",
    "data collection, analysis, and dissemination",
    "use of data in decision making",
    "data-?driven decision making",
    "strategic planning"
  ),
  student_completion = c(
    "complete their degrees in a timely manner",
    "reasonable progress toward and complete their degrees",
    "graduation rates",
    "completion plan",
    "student progress and achievement"
  ),
  enrollment_planning = c(
    "strategic enrollment (?:management )?plan",
    "increase enrollment",
    "enrollment management",
    "enrollment goals"
  ),
  shared_governance = c(
    "shared governance",
    "two-?way communication",
    "lack of transparency in decision making",
    "erosion of shared governance",
    "decision making"
  ),
  leadership_capacity = c(
    "leadership capacity",
    "succession pathways",
    "changes in leadership",
    "organizational structures",
    "decision-?making processes"
  ),
  campus_climate = c(
    "climate concerns"
  ),
  board_oversight = c(
    "board oversight"
  )
)

.flatten_pattern_list <- function(pattern_list) {
  unique(unlist(pattern_list %||% list(), use.names = FALSE))
}

.ACCREDITATION_SPECIFICITY_PROFILES <- list(
  MSCHE = list(
    numbered_standards = c(
      "\\bstandard\\s+[ivx]+\\b",
      "\\brequirements? of affiliation\\s+\\d+\\b"
    ),
    numbered_components = c(
      "\\bcore component\\s+[a-z0-9.]+\\b",
      "\\bassumed practice\\s+[a-z0-9.]+\\b"
    ),
    named_concerns = .ACCR_EDITORIAL_CONCERN_PATTERNS,
    noncompliance = c(
      "not in compliance",
      "insufficient evidence",
      "accreditation is in jeopardy"
    )
  ),
  WSCUC = list(
    numbered_standards = c(
      "\\bstandard\\s+[1-4](?:\\s*,\\s*cfrs?\\s*[0-9.]+(?:\\s*(?:and|,)\\s*[0-9.]+)*)?\\b",
      "\\bstandards\\s+[1-4](?:\\s*(?:and|,)\\s*[1-4])+\\b",
      "\\bstandards?\\s+[1-4]\\b"
    ),
    numbered_components = c(
      "\\bcfrs?\\s*[0-9.]+(?:\\s*(?:and|,)\\s*[0-9.]+)*\\b"
    ),
    named_concerns = c(
      .ACCR_EDITORIAL_CONCERN_PATTERNS,
      .flatten_pattern_list(.WSCUC_NAMED_CONCERN_PATTERNS)
    ),
    noncompliance = c(
      "not in compliance",
      "out of compliance",
      "has not demonstrated compliance",
      "not demonstrated compliance",
      "fails to meet",
      "areas of noncompliance",
      "standards at risk of non-compliance"
    )
  )
)

.match_any_pattern <- function(text, patterns) {
  if (!nzchar(text) || length(patterns) == 0) return(FALSE)
  any(vapply(patterns, function(pattern) {
    stringr::str_detect(text, stringr::regex(pattern, ignore_case = TRUE))
  }, logical(1)))
}

.NECHE_STANDARD_ALIASES <- list(
  organization_and_governance = list(
    number = "3",
    name = "Organization and Governance",
    aliases = c("organization and governance")
  ),
  academic_program = list(
    number = "4",
    name = "The Academic Program",
    aliases = c("the academic program", "academic program")
  ),
  planning_and_evaluation = list(
    number = "2",
    name = "Planning and Evaluation",
    aliases = c("planning and evaluation")
  ),
  institutional_resources = list(
    number = "7",
    name = "Institutional Resources",
    aliases = c("institutional resources")
  )
)

.normalize_phrase_boundary_text <- function(text) {
  value <- tolower(as.character(text %||% ""))
  value <- stringr::str_replace_all(value, "[^a-z0-9]+", " ")
  stringr::str_squish(value)
}

.contains_normalized_phrase <- function(text, phrase) {
  normalized_text <- .normalize_phrase_boundary_text(text)
  normalized_phrase <- .normalize_phrase_boundary_text(phrase)
  if (!nzchar(normalized_text) || !nzchar(normalized_phrase)) return(FALSE)
  stringr::str_detect(
    normalized_text,
    stringr::regex(
      sprintf("(^|\\s)%s(\\s|$)", stringr::str_replace_all(normalized_phrase, "\\s+", "\\\\s+")),
      ignore_case = TRUE
    )
  )
}

.format_readable_list <- function(values) {
  values <- values[!is.na(values) & nzchar(trimws(as.character(values)))]
  if (length(values) == 0L) return(NA_character_)
  if (length(values) == 1L) return(values[[1]])
  if (length(values) == 2L) return(sprintf("%s and %s", values[[1]], values[[2]]))
  sprintf(
    "%s, and %s",
    paste(values[-length(values)], collapse = ", "),
    values[[length(values)]]
  )
}

extract_neche_standard_families <- function(text) {
  matched <- names(.NECHE_STANDARD_ALIASES)[vapply(names(.NECHE_STANDARD_ALIASES), function(id) {
    alias_values <- .NECHE_STANDARD_ALIASES[[id]]$aliases %||% character()
    any(vapply(alias_values, function(alias_value) {
      .contains_normalized_phrase(text, alias_value)
    }, logical(1)))
  }, logical(1))]
  unname(matched)
}

get_neche_concern_signature <- function(text) {
  families <- extract_neche_standard_families(text)
  if (length(families) == 0L) return(NA_character_)
  paste(families, collapse = "|")
}

.format_neche_standard_family_labels <- function(families) {
  families <- families[families %in% names(.NECHE_STANDARD_ALIASES)]
  if (length(families) == 0L) return(NA_character_)
  labels <- vapply(families, function(id) {
    spec <- .NECHE_STANDARD_ALIASES[[id]]
    sprintf("Standard %s (%s)", spec$number, spec$name)
  }, character(1))
  .format_readable_list(labels)
}

extract_neche_substantive_concern_phrase <- function(text, families = extract_neche_standard_families(text)) {
  cleaned <- .normalize_action_summary_text(text)
  if (!nzchar(cleaned) || !("institutional_resources" %in% families)) {
    return(NA_character_)
  }

  concern_patterns <- c(
    "specifically that\\s+([^.]*(?:resources|cash flow)[^.]*?may not be sufficient[^.]*)(?:\\.|$)",
    "((?:the institution'?s\\s+)?resources and cash flow may not be sufficient[^.]*)(?:\\.|$)",
    "((?:the institution'?s\\s+)?cash flow may not be sufficient[^.]*)(?:\\.|$)",
    "((?:the institution'?s\\s+)?financial resources[^.]*may not be sufficient[^.]*)(?:\\.|$)"
  )

  for (pattern in concern_patterns) {
    match_value <- stringr::str_match(
      cleaned,
      stringr::regex(pattern, ignore_case = TRUE)
    )[, 2]
    match_value <- stringr::str_squish(match_value %||% "")
    if (nzchar(match_value)) {
      match_value <- sub("\\.$", "", match_value)
      return(match_value)
    }
  }

  NA_character_
}

.build_neche_standard_concern_label <- function(text) {
  families <- extract_neche_standard_families(text)
  family_label <- .format_neche_standard_family_labels(families)
  if (is.na(family_label) || !nzchar(family_label)) return(NA_character_)

  concern_phrase <- extract_neche_substantive_concern_phrase(text, families)
  if (!is.na(concern_phrase) && nzchar(concern_phrase)) {
    return(sprintf("%s: %s", family_label, concern_phrase))
  }

  sprintf("%s concerns", family_label)
}

get_action_summary_specificity_score <- function(text, accreditor = NA_character_) {
  value <- .normalize_action_summary_text(text)
  acc_norm <- toupper(trimws(as.character(accreditor %||% "")))
  profile <- .ACCREDITATION_SPECIFICITY_PROFILES[[acc_norm]]
  if (!nzchar(value) || is.null(profile)) return(0L)

  score <- 0L
  if (.match_any_pattern(value, profile$numbered_standards %||% character())) score <- score + 4L
  if (.match_any_pattern(value, profile$numbered_components %||% character())) score <- score + 4L
  if (.match_any_pattern(value, profile$named_concerns %||% character())) score <- score + 3L
  if (.match_any_pattern(value, profile$noncompliance %||% character())) score <- score + 2L
  score
}

get_action_summary_substantive_text_length <- function(text, accreditor = NA_character_) {
  value <- .strip_action_source_selection_wrapper(text, accreditor)
  if (!nzchar(value)) return(0L)
  nchar(value, type = "chars", allowNA = FALSE, keepNA = FALSE)
}

.format_numbered_list <- function(values) {
  values <- .unique_preserve_order(values)
  if (length(values) == 0L) return(NA_character_)
  if (length(values) == 1L) return(values[[1]])
  if (length(values) == 2L) return(sprintf("%s and %s", values[[1]], values[[2]]))
  sprintf(
    "%s, and %s",
    paste(values[-length(values)], collapse = ", "),
    values[[length(values)]]
  )
}

extract_wscuc_named_concerns <- function(text) {
  value <- .normalize_action_summary_text(text)
  if (!nzchar(value)) return(character())

  matches <- list()
  for (label in names(.WSCUC_NAMED_CONCERN_PATTERNS)) {
    patterns <- .WSCUC_NAMED_CONCERN_PATTERNS[[label]] %||% character()
    positions <- vapply(patterns, function(pattern) {
      located <- stringr::str_locate(
        value,
        stringr::regex(pattern, ignore_case = TRUE)
      )[1, 1]
      if (is.na(located)) Inf else located
    }, numeric(1))
    best_position <- suppressWarnings(min(positions, na.rm = TRUE))
    if (is.finite(best_position)) {
      matches[[length(matches) + 1L]] <- list(
        label = switch(
          label,
          financial_sustainability = "financial sustainability",
          resource_planning = "resource planning",
          quality_assurance = "quality assurance",
          student_completion = "student completion",
          enrollment_planning = "enrollment planning",
          shared_governance = "shared governance",
          leadership_capacity = "leadership capacity",
          campus_climate = "campus climate",
          board_oversight = "board oversight",
          label
        ),
        position = best_position
      )
    }
  }

  if (!length(matches)) return(character())
  ordered <- matches[order(vapply(matches, `[[`, numeric(1), "position"))]
  .unique_preserve_order(vapply(ordered, `[[`, character(1), "label"))
}

.extract_wscuc_focus_text <- function(text) {
  value <- .normalize_action_summary_text(text)
  if (!nzchar(value)) return(NA_character_)

  section_specs <- list(
    c(
      ".*?Non-Compliance with Standards: Deficiencies to be Addressed\\s*",
      "\\s*(Areas for Development|Next Steps|Maximum Timeframe|Commission policy requires).*$"
    ),
    c(
      ".*?Areas of Noncompliance\\s*",
      "\\s*(Maximum Timeframe|Next Steps|Commission policy requires).*$"
    ),
    c(
      ".*?Standards at Risk of Non-Compliance and Requiring a Response\\s*",
      "\\s*(Areas for Development|In accordance with Commission policy|Next Steps).*$"
    )
  )
  for (spec in section_specs) {
    if (grepl(spec[[1]], value, ignore.case = TRUE, perl = TRUE)) {
      section <- sub(spec[[1]], "", value, ignore.case = TRUE, perl = TRUE)
      section <- sub(spec[[2]], "", section, ignore.case = TRUE, perl = TRUE)
      section <- stringr::str_squish(section)
      if (nzchar(section)) return(section)
    }
  }

  fallback_patterns <- c(
    "The Commission has determined that [^.]+? is not in compliance with [^.]+\\.",
    "The Commission determined that [^.]+? (?:was )?out of compliance with [^.]+\\.",
    "The Commission determined that [^.]+? has not demonstrated compliance with [^.]+\\.",
    "The Commission acted to remove a Show Cause order and impose the sanction of Warning because [^.]+\\."
  )
  for (pattern in fallback_patterns) {
    match_value <- stringr::str_match(
      value,
      stringr::regex(pattern, ignore_case = TRUE)
    )[, 1]
    match_value <- stringr::str_squish(match_value %||% "")
    if (nzchar(match_value)) return(match_value)
  }

  NA_character_
}

.extract_wscuc_standards <- function(text) {
  value <- .normalize_action_summary_text(text)
  if (!nzchar(value)) return(character())
  matches <- stringr::str_match_all(
    value,
    stringr::regex("\\bstandards?\\s+([1-4](?:\\s*(?:and|,)\\s*[1-4])*)\\b", ignore_case = TRUE)
  )[[1]]
  if (!nrow(matches)) return(character())

  values <- unlist(lapply(matches[, 2], function(section) {
    stringr::str_extract_all(section, "[1-4]")[[1]]
  }), use.names = FALSE)
  .unique_preserve_order(values)
}

.extract_wscuc_cfrs <- function(text) {
  value <- .normalize_action_summary_text(text)
  if (!nzchar(value)) return(character())
  matches <- stringr::str_extract_all(
    value,
    stringr::regex("\\bcfrs?\\s*[0-9.]+(?:-[0-9.]+)?(?:\\s*(?:and|,)\\s*[0-9.]+(?:-[0-9.]+)?)*", ignore_case = TRUE)
  )[[1]]
  if (!length(matches)) return(character())

  values <- unlist(lapply(matches, function(section) {
    tokens <- stringr::str_extract_all(section, "[0-9]+(?:\\.[0-9]+)?(?:-[0-9]+(?:\\.[0-9]+)?)?")[[1]]
    unlist(lapply(tokens, function(token) {
      if (!grepl("-", token, fixed = TRUE)) return(token)
      bounds <- strsplit(token, "-", fixed = TRUE)[[1]]
      start <- suppressWarnings(as.numeric(bounds[[1]]))
      end <- suppressWarnings(as.numeric(bounds[[2]]))
      if (!is.finite(start) || !is.finite(end)) return(token)
      if (abs(start - end) >= 1 || end < start) return(token)
      major <- floor(start)
      start_minor <- round((start - major) * 10)
      end_minor <- round((end - major) * 10)
      if (start_minor > end_minor) return(token)
      sprintf("%d.%d", major, seq.int(start_minor, end_minor))
    }), use.names = FALSE)
  }), use.names = FALSE)
  .unique_preserve_order(values)
}

.format_wscuc_standard_detail <- function(text) {
  standards <- .extract_wscuc_standards(text)
  cfrs <- .extract_wscuc_cfrs(text)
  if (length(standards) == 0L && length(cfrs) == 0L) return(NA_character_)

  standard_label <- if (length(standards) > 0L) {
    sprintf(
      "%s %s",
      if (length(standards) == 1L) "Standard" else "Standards",
      .format_numbered_list(standards)
    )
  } else {
    NA_character_
  }
  cfr_label <- if (length(cfrs) > 0L) {
    sprintf(
      "%s %s",
      if (length(cfrs) == 1L) "CFR" else "CFRs",
      .format_numbered_list(cfrs)
    )
  } else {
    NA_character_
  }

  if (!is.na(standard_label) && !is.na(cfr_label)) {
    return(sprintf("%s, %s", standard_label, cfr_label))
  }
  standard_label %||% cfr_label
}

.build_wscuc_concern_detail <- function(text) {
  standard_detail <- .format_wscuc_standard_detail(text)
  standards <- .extract_wscuc_standards(text)
  named_concerns <- extract_wscuc_named_concerns(text)
  priority_concerns <- character()
  if ("2" %in% standards) {
    priority_concerns <- c(priority_concerns, "student completion")
  }
  if ("3" %in% standards) {
    priority_concerns <- c(priority_concerns, "financial sustainability")
  }
  if ("4" %in% standards) {
    priority_concerns <- c(priority_concerns, "quality assurance")
  }
  if ("3" %in% standards) {
    priority_concerns <- c(priority_concerns, "resource planning")
  }
  if (!length(priority_concerns)) {
    priority_concerns <- c("financial sustainability", "quality assurance")
  }
  named_concerns <- .unique_preserve_order(c(
    intersect(priority_concerns, named_concerns),
    named_concerns
  ))
  concern_label <- if (length(named_concerns) > 0L) {
    .format_readable_list(named_concerns[seq_len(min(length(named_concerns), 2L))])
  } else {
    NA_character_
  }

  if (!is.na(standard_detail) && !is.na(concern_label)) {
    return(sprintf("%s on %s", standard_detail, concern_label))
  }
  standard_detail %||% concern_label
}

.extract_wscuc_action_phrase <- function(text) {
  value <- .normalize_action_summary_text(text)
  if (!nzchar(value)) return(NA_character_)

  if (stringr::str_detect(value, stringr::regex("remove (?:the )?(?:formal )?notice of concern", ignore_case = TRUE)) &&
      stringr::str_detect(value, stringr::regex("issue(?:d)? (?:the sanction of )?a?\\s*warning|impose(?:d)? (?:the sanction of )?a?\\s*warning", ignore_case = TRUE))) {
    return("Removed Notice of Concern and issued a Warning")
  }
  if (stringr::str_detect(value, stringr::regex("remove (?:the )?(?:formal )?notice of concern", ignore_case = TRUE))) {
    return("Removed Notice of Concern")
  }
  if (stringr::str_detect(value, stringr::regex("continue(?:d)? (?:the )?notice of concern|continue(?:d)? [^.]{0,120}? on notice of concern", ignore_case = TRUE))) {
    return("Continued Notice of Concern")
  }
  if (stringr::str_detect(value, stringr::regex("continue(?:d)? the sanction of show cause|will remain on show cause|continue(?:d)? show cause", ignore_case = TRUE))) {
    return("Continued Show Cause")
  }

  patterns <- list(
    "Removed Notice of Concern and issued a Warning" = "removed the notice of concern and issued? a warning",
    "Removed Show Cause and issued a Warning" = "remove a show cause order and (?:impose|issue) (?:the sanction of )?a?\\s*warning|removed? show cause(?: order)? and issued? a warning",
    "Issued a Notice of Concern" = "place [^.]{0,120}? on notice of concern|issue (?:a )?(?:formal )?notice of concern",
    "Placed on Probation" = "place [^.]{0,120}? on probation|impose probation",
    "Placed on Warning" = "issue a warning|impose (?:the sanction of )?a?\\s*warning"
  )

  for (label in names(patterns)) {
    if (stringr::str_detect(value, stringr::regex(patterns[[label]], ignore_case = TRUE))) {
      return(label)
    }
  }

  NA_character_
}

.extract_wscuc_compliance_phrase <- function(text) {
  value <- .normalize_action_summary_text(text)
  if (!nzchar(value)) return(NA_character_)
  dplyr::case_when(
    stringr::str_detect(value, stringr::regex("at risk of non-compliance|in danger of being found out of compliance", ignore_case = TRUE)) ~
      "it is at risk of non-compliance with",
    stringr::str_detect(value, stringr::regex("has not demonstrated compliance|not demonstrated compliance", ignore_case = TRUE)) ~
      "it has not demonstrated compliance with",
    stringr::str_detect(value, stringr::regex("out of compliance|not in compliance", ignore_case = TRUE)) ~
      "it is out of compliance with",
    TRUE ~ NA_character_
  )
}

.summarize_wscuc_letter <- function(text) {
  value <- .normalize_action_summary_text(text)
  if (!nzchar(value) ||
      !(
        stringr::str_detect(
          value,
          stringr::regex("formal notification and official record of action taken", ignore_case = TRUE)
        ) ||
          (
            stringr::str_detect(
              value,
              stringr::regex("actions?\\s+1\\.|areas of noncompliance|standards? at risk of non-compliance", ignore_case = TRUE)
            ) &&
              !is.na(.extract_wscuc_action_phrase(value))
          )
      )) {
    return(NA_character_)
  }

  action_phrase <- .extract_wscuc_action_phrase(value)
  if (is.na(action_phrase) || !nzchar(action_phrase)) return(NA_character_)

  focus_text <- .extract_wscuc_focus_text(value)
  detail <- .build_wscuc_concern_detail(focus_text %||% value)
  compliance_phrase <- .extract_wscuc_compliance_phrase(focus_text %||% value)

  if (!is.na(detail) && nzchar(detail)) {
    if (identical(action_phrase, "Issued a Notice of Concern")) {
      return(sprintf("%s over %s", action_phrase, detail))
    }
    if (!is.na(compliance_phrase) && nzchar(compliance_phrase)) {
      return(sprintf("%s because %s %s", action_phrase, compliance_phrase, detail))
    }
  }

  action_phrase
}

.unique_preserve_order <- function(values) {
  values <- as.character(values %||% character())
  values <- values[!is.na(values) & nzchar(trimws(values))]
  if (length(values) == 0L) return(character())
  values[!duplicated(values)]
}

.extract_hlc_reference_codes <- function(text, anchor_pattern, code_pattern) {
  value <- .normalize_action_summary_text(text)
  if (!nzchar(value)) return(character())

  matches <- stringr::str_match_all(
    value,
    stringr::regex(
      paste0(
        anchor_pattern,
        "\\s+(.+?)(?=",
        "\\s+(?:with\\s+concerns|with\\s+concern|without\\s+concerns|for\\s+the\\s+following|",
        "no\\s+later\\s+than|is\\s+required|related\\s+to|and\\s+does\\s+not\\s+meet|",
        "and\\s+the\\s+institution|and\\s+assigned|and\\s+reaffirmed)\\b|",
        "\\.\\s+[A-Z]|;|$)"
      ),
      ignore_case = TRUE
    )
  )[[1]]
  if (!nrow(matches)) return(character())

  codes <- unlist(lapply(matches[, 2], function(section) {
    stringr::str_extract_all(
      section,
      stringr::regex(code_pattern, ignore_case = TRUE)
    )[[1]]
  }), use.names = FALSE)
  codes <- toupper(stringr::str_squish(codes))
  .unique_preserve_order(codes)
}

extract_hlc_core_components <- function(text) {
  .extract_hlc_reference_codes(
    text,
    anchor_pattern = "(?:criterion\\s+[a-z0-9]+,\\s*)?core components?",
    code_pattern = "\\b[1-9]\\.[A-Z]\\b"
  )
}

extract_hlc_assumed_practices <- function(text) {
  .extract_hlc_reference_codes(
    text,
    anchor_pattern = "assumed practices?",
    code_pattern = "\\b[A-Z]\\.[0-9]\\b"
  )
}

extract_hlc_named_concern_phrases <- function(text) {
  value <- .normalize_action_summary_text(text)
  if (!nzchar(value)) return(character())

  matches <- stringr::str_match_all(
    value,
    stringr::regex(
      "criteria for accreditation related to\\s+([^.|;]+)",
      ignore_case = TRUE
    )
  )[[1]]
  if (!nrow(matches)) return(character())

  concerns <- stringr::str_squish(matches[, 2])
  concerns <- sub("[.]+$", "", concerns)
  .unique_preserve_order(concerns)
}

extract_hlc_findings <- function(text) {
  list(
    core_components = extract_hlc_core_components(text),
    assumed_practices = extract_hlc_assumed_practices(text),
    named_concerns = extract_hlc_named_concern_phrases(text)
  )
}

.extract_hlc_summary_clause <- function(text) {
  value <- .normalize_action_summary_text(text)
  if (!nzchar(value)) return(NA_character_)

  summary_clause <- stringr::str_match(
    value,
    stringr::regex(
      paste0(
        "Summary of the Action:\\s*(.+?)(?=\\s+(?:",
        "Institutional Disclosure Obligation:",
        "|Substantive Change:",
        "|Notification Program:",
        "|Board Rationale",
        "|Next Steps in the HLC Review Process",
        "|HLC Disclosure Obligations",
        ")\\b|$)"
      ),
      ignore_case = TRUE
    )
  )[, 2]

  summary_clause <- stringr::str_squish(summary_clause %||% "")
  if (!nzchar(summary_clause)) return(NA_character_)
  summary_clause
}

.format_hlc_reference_detail <- function(values, singular_label, plural_label) {
  values <- .unique_preserve_order(toupper(stringr::str_squish(values)))
  if (length(values) == 0L) return(NA_character_)
  label <- if (length(values) == 1L) singular_label else plural_label
  sprintf("%s %s", label, .format_readable_list(values))
}

.build_hlc_findings_detail <- function(text) {
  findings <- extract_hlc_findings(text)
  detail_parts <- c(
    .format_hlc_reference_detail(
      findings$core_components,
      singular_label = "Core Component",
      plural_label = "Core Components"
    ),
    .format_hlc_reference_detail(
      findings$assumed_practices,
      singular_label = "Assumed Practice",
      plural_label = "Assumed Practices"
    )
  )
  detail_parts <- detail_parts[!is.na(detail_parts) & nzchar(detail_parts)]
  if (length(detail_parts) > 0L) {
    return(.format_readable_list(detail_parts))
  }

  named_concern <- .unique_preserve_order(extract_hlc_named_concern_phrases(text))
  if (length(named_concern) == 0L) return(NA_character_)
  sprintf("HLC's Criteria for Accreditation related to %s", named_concern[[1]])
}

.specialize_hlc_reason <- function(reason_text, context_text) {
  reason_value <- stringr::str_squish(as.character(reason_text %||% ""))
  if (!nzchar(reason_value)) return(reason_value)

  reason_value <- stringr::str_replace(
    reason_value,
    stringr::regex("^it determined that the institution\\s+", ignore_case = TRUE),
    "the institution "
  )
  findings_detail <- .build_hlc_findings_detail(context_text)
  if (is.na(findings_detail) || !nzchar(findings_detail)) {
    return(reason_value)
  }

  replacement_rules <- list(
    c(
      "at risk of being out of compliance(?: with (?:hlc(?:[^A-Za-z0-9]{0,6}s)?\\s+)?(?:the\\s+)?(?:criteria for accreditation|requirements))?",
      paste("at risk of being out of compliance with", findings_detail)
    ),
    c(
      "out of compliance(?: with (?:hlc(?:[^A-Za-z0-9]{0,6}s)?\\s+)?(?:the\\s+)?(?:criteria for accreditation|requirements))?",
      paste("out of compliance with", findings_detail)
    ),
    c(
      "does not meet (?:hlc[^A-Za-z0-9]{0,3}s\\s+)?(?:the\\s+)?criteria for accreditation(?: related to [^.]+)?",
      paste("does not meet", findings_detail)
    )
  )

  for (rule in replacement_rules) {
    if (stringr::str_detect(reason_value, stringr::regex(rule[[1]], ignore_case = TRUE))) {
      return(stringr::str_replace(
        reason_value,
        stringr::regex(rule[[1]], ignore_case = TRUE),
        rule[[2]]
      ))
    }
  }

  reason_value
}

.extract_hlc_location_names <- function(text) {
  value <- .normalize_action_summary_text(text)
  if (!nzchar(value)) return(character())

  section <- stringr::str_match(
    value,
    stringr::regex("^.+?:\\s*(.+)$", ignore_case = TRUE)
  )[, 2]
  if (is.na(section) || !nzchar(section)) return(character())

  section <- gsub(
    ",?\\s*P\\.?O\\.?\\s+Box\\s+\\d+\\b",
    "",
    section,
    ignore.case = TRUE,
    perl = TRUE
  )
  section <- gsub("([A-Z]{2})([A-Z][a-z])", "\\1 \\2", section, perl = TRUE)
  section <- gsub("(\\d{5}(?:-\\d{4})?)(?=[A-Z])", "\\1 ", section, perl = TRUE)

  split_patterns <- c(
    "((?:[A-Z]{2}|[A-Z][a-z]+)\\s+\\d{5}(?:-\\d{4})?)(?=\\s+[A-Z0-9])",
    "(,\\s[A-Z]{2})(?=\\s+[A-Z])",
    "(Republic of Kazakhstan)(?=\\s+[A-Z0-9])",
    "(Singapore\\s+\\d{5})(?=\\s+[A-Z0-9])"
  )
  for (pattern in split_patterns) {
    section <- gsub(pattern, "\\1|||", section, perl = TRUE)
  }

  segments <- stringr::str_split(section, stringr::fixed("|||"))[[1]]
  location_names <- vapply(segments, function(segment) {
    cleaned_segment <- stringr::str_squish(gsub("[.;]+$", "", segment))
    if (!nzchar(cleaned_segment)) return(NA_character_)
    name <- sub(",.*$", "", cleaned_segment)
    stringr::str_squish(name)
  }, character(1))

  .unique_preserve_order(location_names)
}

.extract_hlc_inline_teachout_target <- function(text) {
  value <- .normalize_action_summary_text(text)
  if (!nzchar(value)) return(NA_character_)

  target <- stringr::str_match(
    value,
    stringr::regex(
      paste0(
        "teach(?:-|\\s)?out agreements? with\\s+(.+?)(?=",
        ",\\s*(?:wherein|as an addition to the provisional plan|as additions to the provisional plan)",
        "|\\s+as an addition to the provisional plan",
        "|\\s+as additions to the provisional plan",
        "|\\.(?:\\s|$)|$)"
      ),
      ignore_case = TRUE
    )
  )[, 2]

  target <- stringr::str_squish(target %||% "")
  target <- gsub("\\s*\\(Approved [^)]+\\)$", "", target, perl = TRUE)
  target <- gsub("\\s*\\(Approved [^)]+\\)", "", target, perl = TRUE)
  target <- stringr::str_squish(target)
  if (!nzchar(target)) return(NA_character_)
  target
}

.hlc_location_names_are_plain_places <- function(location_names) {
  values <- stringr::str_squish(as.character(location_names %||% character()))
  values <- values[nzchar(values)]
  if (!length(values)) return(FALSE)

  all(vapply(values, function(value) {
    token_count <- length(unlist(strsplit(value, "\\s+")))
    token_count <= 2L &&
      !grepl(
        "campus|site|center|college|university|school|academy|base|station|annex|road|avenue|drive|boulevard|blvd|suite|building|hall|jftb|nsb",
        value,
        ignore.case = TRUE,
        perl = TRUE
      )
  }, logical(1)))
}

.build_hlc_teachout_location_summary <- function(text) {
  value <- .normalize_action_summary_text(text)
  if (!nzchar(value)) return(NA_character_)

  prefix <- stringr::str_match(
    value,
    stringr::regex("^(.+?:)\\s*.+$", ignore_case = TRUE)
  )[, 2]
  location_names <- .extract_hlc_location_names(value)
  if (is.na(prefix) || !nzchar(prefix) || length(location_names) == 0L) {
    return(NA_character_)
  }

  if (stringr::str_detect(prefix, stringr::regex("for two additional locations", ignore_case = TRUE)) &&
      .hlc_location_names_are_plain_places(location_names)) {
    return(stringr::str_squish(prefix))
  }

  sprintf(
    "%s %s",
    stringr::str_squish(prefix),
    .format_readable_list(location_names)
  )
}

.capitalize_summary_head <- function(text) {
  value <- trimws(as.character(text %||% ""))
  if (!nzchar(value)) return(value)
  sub("^([[:alpha:]])", "\\U\\1", value, perl = TRUE)
}

.is_garbled_action_summary <- function(text) {
  value <- trimws(as.character(text %||% ""))
  if (!nzchar(value)) return(FALSE)
  single_letter_tokens <- stringr::str_count(value, "\\b[A-Za-z]\\b")
  single_letter_tokens >= 6L ||
    stringr::str_detect(
      value,
      stringr::regex("sac\\s+scoc|bo\\s+ard|ta\\s+ke\\s+n|followin\\s+d|institutio\\s+n", ignore_case = TRUE)
    )
}

.extract_teachout_partners <- function(text) {
  section <- stringr::str_match(
    text,
    stringr::regex(
      "following institutions?(?:\\s*\\([^)]*\\))?(?::|\\s+as additions to the provisional plan[^.]*\\.)\\s*(.+)$",
      ignore_case = TRUE
    )
  )[, 2]
  if (is.na(section) || !nzchar(section)) return(character())
  section <- gsub("([A-Z]{2})([A-Z][a-z])", "\\1 \\2", section, perl = TRUE)
  triplet_matches <- stringr::str_match_all(
    section,
    stringr::regex("([A-Z][A-Za-z0-9.&'’\\- ]+?),\\s*[^,]+?,\\s*[A-Z]{2}(?=$|\\s|\\()", ignore_case = FALSE)
  )[[1]]
  if (nrow(triplet_matches)) {
    return(unique(stringr::str_squish(triplet_matches[, 2])))
  }
  matches <- stringr::str_match_all(
    section,
    stringr::regex("([A-Z][A-Za-z0-9.&'’\\- ]+?),\\s*[A-Z]{2}(?=$|\\s|\\()", ignore_case = FALSE)
  )[[1]]
  if (!nrow(matches)) return(character())
  unique(stringr::str_squish(matches[, 2]))
}

.summarize_partner_list <- function(partners) {
  partners <- stringr::str_squish(gsub("^[,;: ]+|[,;: ]+$", "", partners))
  partners <- partners[nzchar(partners)]
  if (!length(partners)) return(NA_character_)
  if (length(partners) == 1L) return(partners[[1]])
  if (length(partners) == 2L) return(paste(partners[[1]], "and", partners[[2]]))
  if (length(partners) == 3L) return(paste(partners[[1]], partners[[2]], paste("and", partners[[3]]), sep = ", "))
  paste(partners[[1]], partners[[2]], partners[[3]], "and others", sep = ", ")
}

.extract_action_clause_after_marker <- function(text) {
  markers <- c(
    "the following action regarding your institution was taken[^:]*:\\s*",
    "the following actions regarding your institution were taken[^:]*:\\s*",
    "the commission acted as follows:\\s*",
    "took the following action:\\s*",
    "took the following actions:\\s*"
  )
  out <- text
  for (pattern in markers) {
    match <- stringr::str_locate(
      out,
      stringr::regex(pattern, ignore_case = TRUE)
    )
    if (!is.na(match[1, 1])) {
      out <- stringr::str_sub(out, match[1, 2] + 1L)
      break
    }
  }
  stringr::str_squish(out)
}

.normalize_sacscoc_ocr_spacing <- function(text) {
  value <- stringr::str_squish(as.character(text %||% ""))
  if (!nzchar(value)) return(value)

  replacements <- c(
    "Thefollowing" = "The following",
    "This reviewwas" = "This review was",
    "reviewwas" = "review was",
    "regardingyourinstitution" = "regarding your institution",
    "wastaken" = "was taken",
    "BoardofTrustees" = "Board of Trustees",
    "Board ofTrustees" = "Board of Trustees",
    "ofthe" = "of the",
    "SACSCOCfollowing" = "SACSCOC following",
    "durina" = "during",
    "ofTrustees" = "of Trustees",
    "The Board ofTrustees" = "The Board of Trustees",
    "Control offinances" = "Control of finances",
    "learn/n g/information resources" = "learning/information resources",
    "Level Ill" = "Level III",
    "addressingthe" = "addressing the",
    "dueApril" = "due April",
    "assessthe" = "assess the",
    "achievesthese" = "achieves these",
    "Financial resources}" = "Financial resources)"
  )

  for (pattern in names(replacements)) {
    value <- gsub(pattern, replacements[[pattern]], value, fixed = TRUE)
  }

  value <- gsub("([0-9])\\.\\s+([0-9])", "\\1.\\2", value, perl = TRUE)
  value <- gsub("([0-9])\\.\\s+([A-Za-z])", "\\1.\\2", value, perl = TRUE)
  value <- gsub("\\)\\s+,", "),", value, perl = TRUE)

  stringr::str_squish(value)
}

.extract_substantive_action_sentence <- function(text) {
  if (!nzchar(text)) return(NA_character_)
  sentences <- unlist(stringr::str_split(text, "(?<=[.!?])\\s+", n = Inf))
  sentences <- stringr::str_squish(sentences)
  sentences <- sentences[nzchar(sentences)]
  if (!length(sentences)) return(NA_character_)

  patterns <- c(
    "removed from",
    "placed on notice",
    "placed on probation",
    "denied approval of",
    "continued accreditation following the review of an off-?\\s*campus instructional site",
    "continued .* on warning",
    "placed .* on warning",
    "continued .* on probation",
    "placed .* on probation",
    "show cause",
    "warning",
    "probation",
    "notice of concern",
    "order to show cause",
    "denied reaffirmation",
    "withdraw",
    "voluntary withdrawal",
    "teach-?out",
    "closure",
    "cease academic operations",
    "cease operations",
    "reaffirmed accreditation",
    "requested a monitoring report",
    "require the institution to provide an interim report",
    "issued .* notation",
    "in danger of not meeting",
    "approved the institution.?s provisional plan",
    "approved the institution.?s teach-?out",
    "accepted teach-?out",
    "affirm denial following appeal",
    "voluntary withdrawal received"
  )
  sentence_matches <- Reduce(
    `|`,
    lapply(patterns, function(pattern) {
      stringr::str_detect(sentences, stringr::regex(pattern, ignore_case = TRUE))
    }),
    init = rep(FALSE, length(sentences))
  )
  idx <- which(sentence_matches)
  idx <- idx[!is.na(idx)]
  if (!length(idx)) return(NA_character_)

  sentence <- sentences[[idx[[1]]]]
  if (idx[[1]] < length(sentences) &&
      stringr::str_detect(sentences[[idx[[1]] + 1L]], stringr::regex("^(for\\b|a special committee\\b)", ignore_case = TRUE))) {
    sentence <- paste(sentence, sentences[[idx[[1]] + 1L]])
  }
  stringr::str_squish(sentence)
}

.should_use_file_text_for_summary <- function(action_label_raw, action_type = NA_character_, accreditor = NA_character_) {
  acc_norm <- toupper(trimws(as.character(accreditor %||% "")))
  raw <- .normalize_action_summary_text(action_label_raw)
  type_norm <- tolower(trimws(as.character(action_type %||% "")))

  if (is.na(raw) || !nzchar(raw)) return(TRUE)
  if (.is_garbled_action_summary(raw)) return(TRUE)

  if (acc_norm == "SACSCOC" && raw %in% c(
    "Heightened Monitoring or Focused Review",
    "Removal of Monitoring Status",
    "Probation or Equivalent or a More Severe Status: Warning",
    "Probation or Equivalent or a More Severe Status: Probation",
    "Warning or Equivalent-Factors Affecting Academic Quality"
  )) {
    return(TRUE)
  }

  if (acc_norm == "SACSCOC" &&
      (grepl("disclosure statement regarding the status", raw, ignore.case = TRUE) ||
       grepl("^standard\\s+[0-9]+(?:\\.[0-9]+)?(?:\\.[a-z])?\\s*\\(", raw, ignore.case = TRUE))) {
    return(TRUE)
  }

  if (acc_norm == "SACSCOC" &&
      (
        grepl("if the institution fails to document compliance with the above listed standards", raw, ignore.case = TRUE) ||
          grepl("may begin a two-?year monitoring period", raw, ignore.case = TRUE) ||
          grepl("reviewed .* monitoring report.*response to that report\\.?$", raw, ignore.case = TRUE, perl = TRUE) ||
          grepl("^.*?reviewed the institution.?s\\s+(?:first|second|third|fourth|fifth)\\s+monitoring report following (?:submission of a referral report|reaffirmation of accreditation|action on reaffirmation of accreditation)\\.?$", raw, ignore.case = TRUE, perl = TRUE) ||
          grepl("min\\.\\) program continued as a distance education program", raw, ignore.case = TRUE) ||
          grepl("(?:Core Requirement|CR)\\s+[0-9]+(?:\\.[0-9]+)?\\.?$", raw, ignore.case = TRUE) ||
          grepl("Standard\\s+[0-9]+(?:\\.[0-9]+)?(?:\\.[a-z])?\\.?$", raw, ignore.case = TRUE) ||
          stringr::str_count(raw, stringr::fixed("(")) > stringr::str_count(raw, stringr::fixed(")"))
      )) {
    return(TRUE)
  }

  if (acc_norm == "MSCHE" &&
      grepl(
        paste(
          "^[A-Z][A-Za-z' .-]+:\\s+On behalf of the Middle States Commission on Higher Education",
          "^[A-Z][A-Za-z' .-]+:\\s+Notification of Non-Compliance Action",
          "^[A-Z][A-Za-z' .-]+:\\s+On behalf of the Middle States Commission on Higher Education, I am writing to inform you",
          "^To acknowledge receipt of the monitoring report\\.?$",
          "^To acknowledge receipt of the show cause report\\.?$",
          "^To acknowledge receipt of the request by the institution .* to reconsider the adverse action to withdraw accreditation",
          sep = "|"
        ),
        raw,
        ignore.case = TRUE,
        perl = TRUE
      )) {
    return(TRUE)
  }

  if (acc_norm == "HLC" &&
      (grepl("^summary of the action\\.?$", raw, ignore.case = TRUE) ||
       (type_norm == "removed" && grepl("^comprehensive evaluation:", raw, ignore.case = TRUE)))) {
    return(TRUE)
  }

  if (acc_norm == "NECHE" &&
      (grepl("^if the commission finds the institution has successfully addressed the concerns", raw, ignore.case = TRUE) ||
       grepl("^to show cause why it should not be placed on probation", raw, ignore.case = TRUE) ||
       grepl("^on probation for a period not to exceed", raw, ignore.case = TRUE))) {
    return(TRUE)
  }

  if (acc_norm == "WSCUC" &&
      (
        raw %in% c(
          "Heightened Monitoring or Focused Review",
          "Warning or Equivalent-Factors Affecting Academic Quality",
          "Probation or Equivalent or a More Severe Status: Warning",
          "Probation or Equivalent or a More Severe Status: Probation",
          "Probation or Equivalent or a More Severe Status: Show Cause",
          "Removal of Monitoring Status"
        ) ||
          grepl("^procedural history\\b", raw, ignore.case = TRUE) ||
          grepl("^continue with the previously scheduled special visit\\b", raw, ignore.case = TRUE) ||
          grepl("^wscuc is committed to an accreditation process\\b", raw, ignore.case = TRUE) ||
          grepl("continued monitoring through a notice of concern", raw, ignore.case = TRUE) ||
          grepl("^these actions were taken after reviewing .*new evidence of compliance", raw, ignore.case = TRUE) ||
          (grepl("^these\\s+a", raw, ignore.case = TRUE) && grepl("appeal of the withdrawal", raw, ignore.case = TRUE)) ||
          grepl("^defer action on reaffirmation of accreditation\\b", raw, ignore.case = TRUE) ||
          grepl("^defer action on reaffirmation of accreditation\\s*/\\s*issue a notice of concern$", raw, ignore.case = TRUE) ||
          grepl("^at that meeting", raw, ignore.case = TRUE) ||
          grepl("^schedule the next reaffirmation review", raw, ignore.case = TRUE) ||
          grepl("^the commission acted to", raw, ignore.case = TRUE)
      )) {
    return(TRUE)
  }

  FALSE
}

.is_dapip_public_action_code_label <- function(text) {
  value <- .normalize_action_summary_text(text)
  if (!nzchar(value)) return(FALSE)
  grepl(
    paste(
      "^Probation or Equivalent or a More Severe Status:\\s*(Warning|Probation|Show Cause)$",
      "^Accreditation Reaffirmed:\\s*(Warning|Probation) Removed$",
      "^Voluntary Withdrawal Received$",
      sep = "|"
    ),
    value,
    ignore.case = TRUE,
    perl = TRUE
  )
}

.should_prefer_msche_dapip_notes <- function(raw, notes_text) {
  raw_value <- .normalize_action_summary_text(raw)
  notes_value <- .normalize_action_summary_text(notes_text)

  if (!nzchar(notes_value) || !.is_dapip_public_action_code_label(notes_value)) {
    return(FALSE)
  }

  grepl(
    paste(
      "^[A-Z][A-Za-z' .-]+:\\s+On behalf of the Middle States Commission on Higher Education",
      "notification of non-compliance",
      "^To note that the institution remains accredited while on warning\\.?$",
      "^To note that the institution remains accredited while on probation\\.?$",
      "^To acknowledge receipt of the monitoring report\\.?$",
      "^To acknowledge receipt of the show cause report\\.?$",
      "^To acknowledge receipt of the supplemental information report\\.?$",
      sep = "|"
    ),
    raw_value,
    ignore.case = TRUE,
    perl = TRUE
  )
}

.is_hlc_public_action_code_label <- function(text) {
  value <- .normalize_action_summary_text(text)
  if (!nzchar(value)) return(FALSE)
  primary_value <- stringr::str_split(value, stringr::regex("\\s*\\|\\s*"), n = 2)[[1]][1]
  primary_value <- stringr::str_squish(primary_value %||% "")
  grepl(
    paste(
      "^Accreditation Reaffirmed:\\s*(Warning|Probation) Removed$",
      "^Probation or Equivalent or a More Severe Status:\\s*(Warning|Probation|Show Cause)$",
      sep = "|"
    ),
    primary_value,
    ignore.case = TRUE,
    perl = TRUE
  )
}

.should_prefer_hlc_dapip_notes <- function(raw, notes_text) {
  raw_value <- .normalize_action_summary_text(raw)
  notes_value <- .normalize_action_summary_text(notes_text)

  if (!nzchar(notes_value) || !.is_hlc_public_action_code_label(notes_value)) {
    return(FALSE)
  }

  grepl(
    paste(
      "^HLC Disclosure Obligations\\b",
      "^[A-Z][A-Za-z' .-]+, [A-Z][a-z]+ \\d{1,2}, \\d{4} \\d+ HLC Disclosure Obligations\\b",
      "^Next Steps in the HLC Review Process Interim Report\\b",
      "^In taking this action, the Board considered materials\\b",
      "^o To assure that .* anonymous hotline",
      sep = "|"
    ),
    raw_value,
    ignore.case = TRUE,
    perl = TRUE
  )
}

.should_prefer_hlc_file_text <- function(raw, notes_text, file_text, action_type = NA_character_) {
  file_value <- .normalize_action_summary_text(file_text)
  if (!nzchar(file_value)) return(FALSE)

  type_norm <- tolower(trimws(as.character(action_type %||% "")))
  if (!type_norm %in% c("warning", "notice", "probation", "show_cause")) {
    return(FALSE)
  }

  file_detail <- .build_hlc_findings_detail(file_value)
  if (is.na(file_detail) || !nzchar(file_detail)) {
    return(FALSE)
  }

  current_detail <- .build_hlc_findings_detail(
    paste(
      .normalize_action_summary_text(raw),
      .normalize_action_summary_text(notes_text)
    )
  )
  if (is.na(current_detail) || !nzchar(current_detail)) {
    return(TRUE)
  }

  !identical(current_detail, file_detail)
}

# ---------------------------------------------------------------------------
# WSCUC / SACSCOC fallback tier for file-text preference
# ---------------------------------------------------------------------------
#
# Background: the prior rescue path keyed on `.should_use_file_text_for_summary`
# matching one of a handful of WSCUC / SACSCOC raw-title prefixes (the
# scraper's first sentence was assumed to start with "At that meeting,",
# "The Commission acted to", "The following action regarding your institution
# was taken", etc.). A small drift in scraper output -- a fresher fetch with
# a slightly different lead-in, a different first sentence captured -- caused
# the rescue to silently miss for institutions whose cached PDF letter
# contained the full substantive content (e.g. Providence Christian College
# / WSCUC, High Point University / SACSCOC). The downstream test suite
# then surfaced that as "the richer summary disappeared".
#
# These two helpers add a second tier: if the cached file text itself
# contains a marker that the corresponding summarizer is built to consume,
# prefer the file text regardless of what the raw title looks like. The
# marker is the same phrase the summarizer already uses as its gate (see
# .summarize_wscuc_letter / the SACSCOC branches in
# .summarize_non_msche_action_label), so the new tier never produces an
# input the summarizer can't process.
#
# Architectural note: this is the "fallback chain" approach. The strict
# raw-title patterns in .should_use_file_text_for_summary stay in place
# unchanged so fixture-shaped inputs keep their current behavior. These
# helpers run BEFORE that strict-pattern check inside
# .select_action_summary_source, so when both fire they agree; when only
# the marker fires (live phrasing drift), the rescue still happens.

.should_prefer_wscuc_file_text <- function(raw, notes_text, file_text) {
  file_value <- .normalize_action_summary_text(file_text)
  if (!nzchar(file_value)) return(FALSE)
  # .summarize_wscuc_letter gates on this exact phrase as the marker
  # for "this is a substantive WSCUC commission letter". Reusing the
  # same gate here means the new tier only fires when the summarizer
  # is guaranteed to be able to do something useful with the text.
  stringr::str_detect(
    file_value,
    stringr::regex(
      "formal notification and official record of action taken",
      ignore_case = TRUE
    )
  )
}

.should_prefer_sacscoc_file_text <- function(raw, notes_text, file_text) {
  file_value <- .normalize_action_summary_text(file_text)
  if (!nzchar(file_value)) return(FALSE)
  # SACSCOC board-action letters open with one of these phrases. Each
  # corresponds to a SACSCOC summarizer branch in
  # .summarize_non_msche_action_label that knows how to extract the
  # substantive sanction / monitoring clause from the surrounding
  # boilerplate.
  marker <- stringr::regex(
    paste(
      "the following action regarding your institution was taken",
      "the following actions regarding your institution were taken",
      "recommended that the institution be placed on",
      "placed the institution on (?:warning|probation)",
      "continued the institution on (?:warning|probation)",
      "removed the institution from (?:warning|probation)",
      "denied reaffirmation",
      sep = "|"
    ),
    ignore_case = TRUE
  )
  stringr::str_detect(file_value, marker)
}

.normalize_action_summary_source_hint <- function(x) {
  value <- tolower(trimws(as.character(x %||% "")))
  if (!nzchar(value)) return(NA_character_)
  dplyr::case_when(
    value %in% c("dapip_file_text", "pdf_body", "file_text", "pdf_text") ~ "pdf_body",
    value %in% c("dapip_note", "notes") ~ "dapip_note",
    value %in% c("raw_title", "raw", "title", "dapip_action_description") ~ "raw_title",
    TRUE ~ value
  )
}

.read_action_summary_file_text <- function(file_text_path = NA_character_) {
  path <- trimws(as.character(file_text_path %||% ""))
  if (!nzchar(path) || !file.exists(path)) return(NA_character_)

  file_text <- tryCatch(
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = " "),
    error = function(e) ""
  )
  file_text <- .normalize_action_summary_text(file_text)
  if (!nzchar(file_text)) return(NA_character_)
  file_text
}

.select_action_summary_source <- function(action_label_raw, file_text_path = NA_character_, action_type = NA_character_, accreditor = NA_character_, notes = NA_character_, action_label_source_hint = NA_character_) {
  raw <- as.character(action_label_raw %||% "")
  notes_text <- as.character(notes %||% "")
  acc_norm <- toupper(trimws(as.character(accreditor %||% "")))
  source_hint <- .normalize_action_summary_source_hint(action_label_source_hint)
  file_text <- NA_character_

  if (identical(acc_norm, "MSCHE") && .should_prefer_msche_dapip_notes(raw, notes_text)) {
    return(list(text = stringr::str_squish(notes_text), source = "dapip_note"))
  }

  if (identical(acc_norm, "HLC")) {
    file_text <- .read_action_summary_file_text(file_text_path)
    if (.should_prefer_hlc_file_text(raw, notes_text, file_text, action_type = action_type)) {
      return(list(text = file_text, source = "pdf_body"))
    }
  }

  if (identical(acc_norm, "HLC") && .should_prefer_hlc_dapip_notes(raw, notes_text)) {
    return(list(text = stringr::str_squish(notes_text), source = "dapip_note"))
  }

  # Fallback tier for WSCUC and SACSCOC: when the cached file text
  # contains the marker phrase the corresponding summarizer needs,
  # prefer it regardless of the raw title's shape. This runs before
  # the strict raw-title patterns in .should_use_file_text_for_summary
  # so live phrasing drift in the scraper's leader sentence does not
  # silently disable the rescue.
  if (identical(acc_norm, "WSCUC")) {
    if (is.na(file_text) || !nzchar(file_text)) {
      file_text <- .read_action_summary_file_text(file_text_path)
    }
    if (.should_prefer_wscuc_file_text(raw, notes_text, file_text)) {
      return(list(text = file_text, source = "pdf_body"))
    }
  }

  if (identical(acc_norm, "SACSCOC")) {
    if (is.na(file_text) || !nzchar(file_text)) {
      file_text <- .read_action_summary_file_text(file_text_path)
    }
    if (.should_prefer_sacscoc_file_text(raw, notes_text, file_text)) {
      return(list(text = file_text, source = "pdf_body"))
    }
  }

  if (.should_use_file_text_for_summary(raw, action_type, accreditor)) {
    if (is.na(file_text) || !nzchar(file_text)) {
      file_text <- .read_action_summary_file_text(file_text_path)
    }
    if (!is.na(file_text) && nzchar(file_text)) {
      return(list(text = file_text, source = "pdf_body"))
    }
  }

  if (identical(source_hint, "dapip_note") && nzchar(trimws(notes_text))) {
    return(list(text = stringr::str_squish(notes_text), source = "dapip_note"))
  }

  if (identical(source_hint, "pdf_body")) {
    return(list(text = raw, source = "pdf_body"))
  }

  list(text = raw, source = source_hint %||% "raw_title")
}

.select_action_summary_source_text <- function(action_label_raw, file_text_path = NA_character_, action_type = NA_character_, accreditor = NA_character_, notes = NA_character_, action_label_source_hint = NA_character_) {
  .select_action_summary_source(
    action_label_raw = action_label_raw,
    file_text_path = file_text_path,
    action_type = action_type,
    accreditor = accreditor,
    notes = notes,
    action_label_source_hint = action_label_source_hint
  )$text
}

.select_action_summary_source_kind <- function(action_label_raw, file_text_path = NA_character_, action_type = NA_character_, accreditor = NA_character_, notes = NA_character_, action_label_source_hint = NA_character_) {
  .select_action_summary_source(
    action_label_raw = action_label_raw,
    file_text_path = file_text_path,
    action_type = action_type,
    accreditor = accreditor,
    notes = notes,
    action_label_source_hint = action_label_source_hint
  )$source
}

.extract_sacscoc_disclosure_reason_summary <- function(text) {
  value <- stringr::str_squish(as.character(text %||% ""))
  if (!nzchar(value)) return(NA_character_)

  injunction_summary <- stringr::str_match(
    value,
    stringr::regex(
      "temporary restraining order and preliminary injunction.+?reinstat(?:e|ed) the institution.?s accreditation status \\(accredited on Probation\\) pending the outcome of litigation",
      ignore_case = TRUE
    )
  )[, 1]
  if (!is.na(injunction_summary) && nzchar(injunction_summary)) {
    return("Accreditation on Probation was reinstated pending litigation after a temporary restraining order and preliminary injunction.")
  }

  warning_reason <- stringr::str_match(
    value,
    stringr::regex(
      "Why was [^.?!]+ placed on Warning\\?\\s*[^.?!]+? placed on Warning because [^.?!]+? (?:failed to demonstrate|has not yet demonstrated) compliance with (.+?)\\.(?:\\s|$)",
      ignore_case = TRUE
    )
  )[, 2]
  if (!is.na(warning_reason) && nzchar(warning_reason)) {
    warning_reason <- stringr::str_squish(warning_reason)
    return(sprintf("Placed on Warning for failure to comply with %s.", warning_reason))
  }

  probation_reason <- stringr::str_match(
    value,
    stringr::regex(
      "Why was [^.?!]+ placed on Probation\\?\\s*[^.?!]+? placed on Probation because [^.?!]+? (?:failed to demonstrate|has not yet demonstrated) compliance with (.+?)\\.(?:\\s|$)",
      ignore_case = TRUE
    )
  )[, 2]
  if (!is.na(probation_reason) && nzchar(probation_reason)) {
    probation_reason <- stringr::str_squish(probation_reason)
    return(sprintf("Placed on Probation for failure to comply with %s.", probation_reason))
  }

  NA_character_
}

.extract_sacscoc_warning_probation_clause <- function(text) {
  value <- stringr::str_squish(as.character(text %||% ""))
  if (!nzchar(value)) return(NA_character_)

  special_committee_stop <- "\\s+A Special Committee [^.]*authorized[^.]*\\b"
  patterns <- c(
    paste0("(continued in accreditation and placed on Warning.+?)(?=\\s+The institution is requested\\b|\\s+Guidelines for the Monitoring Report\\b|", special_committee_stop, "|$)"),
    paste0("(placed the institution on Warning.+?)(?=\\s+The institution is requested\\b|\\s+Guidelines for the Monitoring Report\\b|", special_committee_stop, "|$)"),
    paste0("(placed on Warning.+?)(?=\\s+The institution is requested\\b|\\s+Guidelines for the Monitoring Report\\b|", special_committee_stop, "|$)"),
    paste0("(continued the institution on Warning.+?)(?=\\s+The institution is requested\\b|\\s+Guidelines for the Monitoring Report\\b|", special_committee_stop, "|$)"),
    paste0("(continued .* on Warning.+?)(?=\\s+The institution is requested\\b|\\s+Guidelines for the Monitoring Report\\b|", special_committee_stop, "|$)"),
    paste0("(placed the institution on Probation.+?)(?=\\s+The institution is requested\\b|\\s+Guidelines for the Monitoring Report\\b|", special_committee_stop, "|$)"),
    paste0("(placed on Probation.+?)(?=\\s+The institution is requested\\b|\\s+Guidelines for the Monitoring Report\\b|", special_committee_stop, "|$)"),
    paste0("(continued the institution on Probation.+?)(?=\\s+The institution is requested\\b|\\s+Guidelines for the Monitoring Report\\b|", special_committee_stop, "|$)"),
    paste0("(continued .* on Probation.+?)(?=\\s+The institution is requested\\b|\\s+Guidelines for the Monitoring Report\\b|", special_committee_stop, "|$)")
  )

  for (pattern in patterns) {
    matched <- stringr::str_match(
      value,
      stringr::regex(pattern, ignore_case = TRUE)
    )[, 2]
    if (!is.na(matched) && nzchar(matched)) {
      return(stringr::str_squish(matched))
    }
  }

  NA_character_
}

.extract_standard_areas <- function(text) {
  value <- stringr::str_squish(as.character(text %||% ""))
  if (!nzchar(value)) return(character())

  matches <- stringr::str_match_all(
    value,
    stringr::regex(
      "(?:Core Requirement|CR|Standard|Standards)\\s+[0-9]+(?:\\.[0-9]+)?(?:\\.[a-z])?(?:\\s+and\\s+[0-9]+(?:\\.[0-9]+)?(?:\\.[a-z])?)?\\s*[\\(\\{]([^\\)\\}]+)[\\)\\}]",
      ignore_case = TRUE
    )
  )[[1]]
  if (!nrow(matches)) return(character())
  unique(stringr::str_squish(matches[, 2]))
}

.summarize_standard_areas <- function(areas) {
  areas <- unique(stringr::str_squish(as.character(areas %||% character())))
  areas <- areas[nzchar(areas)]
  if (!length(areas)) return(NA_character_)
  if (length(areas) == 1L) return(areas[[1]])
  if (length(areas) == 2L) return(paste(areas[[1]], "and", areas[[2]]))
  if (length(areas) == 3L) return(paste(areas[[1]], areas[[2]], "and", areas[[3]], sep = ", "))
  "certain accreditation standards"
}

.condense_standard_area <- function(area) {
  value <- stringr::str_squish(as.character(area %||% ""))
  lower <- tolower(value)
  dplyr::case_when(
    lower %in% c("full-time faculty", "program faculty") ~ "faculty",
    lower == "governing board characteristics" ~ "governance",
    lower == "institutional planning" ~ "institutional planning",
    lower == "financial resources" ~ "financial resources",
    lower == "financial documents" ~ "financial documents",
    lower == "mission review" ~ "mission",
    lower == "ceo evaluation/selection" ~ "CEO evaluation/selection",
    lower == "administrative effectiveness" ~ "administrative effectiveness",
    lower %in% c("student outcomes: educational programs", "student outcomes: academic and student services") ~ "student outcomes",
    lower == "cooperative academic arrangements" ~ "cooperative academic arrangements",
    lower == "financial responsibility" ~ "financial responsibility",
    lower == "control of finances" ~ "control of finances",
    lower == "control of sponsored research/external funds" ~ "sponsored research/external funds",
    lower == "federal and state responsibilities" ~ "federal/state responsibilities",
    TRUE ~ value
  )
}

.summarize_standard_areas_for_sanction <- function(areas, max_items = 6L) {
  areas <- vapply(areas %||% character(), .condense_standard_area, character(1))
  areas <- unique(stringr::str_squish(as.character(areas)))
  areas <- areas[nzchar(areas)]
  if (!length(areas)) return(NA_character_)
  if (length(areas) <= 3L) {
    return(.summarize_standard_areas(areas))
  }
  if (length(areas) > max_items) {
    areas <- areas[seq_len(max_items)]
  }
  area_text <- dplyr::case_when(
    length(areas) == 1L ~ areas[[1]],
    length(areas) == 2L ~ paste(areas[[1]], "and", areas[[2]]),
    TRUE ~ paste(paste(areas[-length(areas)], collapse = ", "), "and", areas[[length(areas)]])
  )
  paste0("standards concerning ", area_text)
}

.compact_sacscoc_sanction_summary <- function(text) {
  value <- stringr::str_squish(as.character(text %||% ""))
  value <- stringr::str_replace(
    value,
    stringr::regex("^reviewed the institution[’'`]s\\s+(?:first|second|third|fourth|fifth)?\\s*monitoring report(?:\\s+following[^,.;]*?)?,?\\s+and\\s+", ignore_case = TRUE),
    ""
  )
  if (!nzchar(value) || !stringr::str_detect(tolower(value), "failure to comply with")) {
    return(value)
  }
  areas <- .extract_standard_areas(value)
  area_summary <- .summarize_standard_areas_for_sanction(areas)
  if (is.na(area_summary) || !nzchar(area_summary) || !stringr::str_detect(area_summary, "^standards concerning\\b")) {
    return(value)
  }
  prefix <- stringr::str_match(
    value,
    stringr::regex("^(.*?)(?:for failure to comply with).*$", ignore_case = TRUE)
  )[, 2]
  if (is.na(prefix) || !nzchar(prefix)) {
    return(value)
  }
  value <- paste0(
    stringr::str_trim(prefix),
    " for failure to comply with ",
    area_summary
  )
  stringr::str_squish(value)
}

.normalize_standard_reason_case <- function(text) {
  value <- stringr::str_squish(as.character(text %||% ""))
  if (!nzchar(value)) return(value)
  prefix <- sub("\\s*\\(.*$", "", value)
  detail <- sub("^[^(]*\\(([^)]*)\\).*$", "\\1", value)
  if (identical(detail, value)) return(value)
  detail <- tools::toTitleCase(tolower(detail))
  paste0(prefix, " (", detail, ")")
}

.normalize_sentence_case_summary <- function(text) {
  value <- stringr::str_squish(as.character(text %||% ""))
  if (!nzchar(value)) return(value)
  value <- tolower(value)
  .capitalize_summary_head(value)
}

is_sacscoc_public_table_row_to_drop <- function(action_type, action_label_short, action_label_raw) {
  type_norm <- tolower(trimws(as.character(action_type %||% "")))
  short_norm <- tolower(trimws(as.character(action_label_short %||% "")))
  raw_norm <- tolower(trimws(as.character(action_label_raw %||% "")))
  text_norm <- stringr::str_squish(.normalize_sacscoc_ocr_spacing(paste(short_norm, raw_norm)))
  text_norm <- tolower(text_norm)
  has_substantive_monitoring_summary <- stringr::str_detect(
    short_norm,
    "^requested referral report\\b|^requested to submit a monitoring report\\b|^no additional report requested\\b"
  )

  has_serious_signal <- stringr::str_detect(
    text_norm,
    "warning|probation|show cause|good cause|denied reaffirmation|removed from|requested (?:to submit )?(?:a|(?:first|second|third|fourth|fifth)\\s+)?monitoring report|requested referral report|no additional report requested|placed the institution|continued .* on (warning|probation)"
  )

  if (stringr::str_detect(text_norm, "publication of accreditation status|disclosure statement regarding accreditation status|institutional obligations for public disclosure|published accreditation status statement did not comply") &&
      !has_serious_signal) {
    return(TRUE)
  }

  if (stringr::str_detect(short_norm, "requested referral report on public information|requested referral report on policies for awarding credit")) {
    return(TRUE)
  }

  if (stringr::str_detect(short_norm, "^denied approval of\\b")) {
    return(TRUE)
  }

  if (stringr::str_detect(text_norm, "continued accreditationfollowing the review of the bachelor of science|continued accreditation following the review of the bachelor of science")) {
    return(TRUE)
  }

  if (stringr::str_detect(text_norm, "continued opportunities for program development|new master of fine arts")) {
    return(TRUE)
  }

  if (stringr::str_detect(text_norm, "demonstrated continued commitment to debt reduction|acquired no new debt")) {
    return(TRUE)
  }

  if (stringr::str_detect(text_norm, "we appreciate your continued[_ ]support of sacscoc") &&
      !has_substantive_monitoring_summary) {
    return(TRUE)
  }

  if (stringr::str_detect(text_norm, "failure\\s*to\\s*document\\s*compliance.*will result in (?:the|your) institution being placed on a sanction|if (?:the|your) institution fails to document compliance") &&
      !has_substantive_monitoring_summary) {
    return(TRUE)
  }

  if (stringr::str_detect(text_norm, "review which may begin a two-year monitoring period") &&
      !has_serious_signal &&
      !has_substantive_monitoring_summary) {
    return(TRUE)
  }

  if (stringr::str_detect(text_norm, "annual funding was temporarily placed on hold until post-covid enrollment rebounds")) {
    return(TRUE)
  }

  if (stringr::str_detect(text_norm, "should demonstrate that it is monitoring its specific metric chosen for measuring graduation rate")) {
    return(TRUE)
  }

  if (stringr::str_detect(text_norm, "review\\s*was conducted following the institution'?s failure to notify sacscoc") && !has_serious_signal) {
    return(TRUE)
  }

  if (type_norm %in% c("other", "notice", "monitoring") &&
      stringr::str_detect(text_norm, "strong mid-sized university|program development|debt reduction|post-covid enrollment rebounds")) {
    return(TRUE)
  }

  FALSE
}

.summarize_non_msche_action_label <- function(action_type, raw, accreditor, notes = NA_character_) {
  acc_norm <- toupper(trimws(as.character(accreditor %||% "")))
  cleaned <- .normalize_action_summary_text(raw)
  if (acc_norm == "SACSCOC") {
    cleaned <- .normalize_sacscoc_ocr_spacing(cleaned)
  }
  lowered <- tolower(cleaned)
  notes_text <- stringr::str_squish(as.character(notes %||% ""))
  notes_lower <- tolower(notes_text)

  if (!nzchar(cleaned)) return(NA_character_)

  if (acc_norm == "NECHE" &&
      stringr::str_detect(notes_lower, "^heightened monitoring or focused review\\s*\\|")) {
    detail <- stringr::str_match(
      notes_text,
      stringr::regex("^Heightened Monitoring or Focused Review\\s*\\|\\s*(.+)$", ignore_case = TRUE)
    )[, 2]
    if (!is.na(detail) && nzchar(detail)) {
      detail <- gsub("\\.{2,}", ".", stringr::str_squish(detail))
      detail <- sub("\\.$", "", detail)
      return(.capitalize_summary_head(sprintf(
        "%s. Placed on Heightened Monitoring or Focused Review.",
        detail
      )))
    }
  }

  if (stringr::str_detect(lowered, "notification of non-compliance probation action")) {
    return("Non-Compliance Probation")
  }
  if (stringr::str_detect(lowered, "notification of non-compliance show cause action")) {
    return("Required to Show Cause")
  }
  if (stringr::str_detect(lowered, "notification of non-compliance warning action")) {
    return("Non-Compliance Warning")
  }

  if (grepl("^Accreditation Reaffirmed:\\s*Warning Removed$", cleaned, ignore.case = TRUE, perl = TRUE)) {
    return("Accreditation Reaffirmed: Warning Removed")
  }
  if (grepl("^Accreditation Reaffirmed:\\s*Probation Removed$", cleaned, ignore.case = TRUE, perl = TRUE)) {
    return("Accreditation Reaffirmed: Probation Removed")
  }
  if (grepl("^Probation or Equivalent or a More Severe Status:\\s*Warning$", cleaned, ignore.case = TRUE, perl = TRUE)) {
    return("Placed on Warning")
  }
  if (grepl("^Probation or Equivalent or a More Severe Status:\\s*Probation$", cleaned, ignore.case = TRUE, perl = TRUE)) {
    return("Placed on Probation")
  }
  if (grepl("^Probation or Equivalent or a More Severe Status:\\s*Show Cause$", cleaned, ignore.case = TRUE, perl = TRUE)) {
    return("Asked to Show Cause")
  }

  if (acc_norm == "NWCCU" &&
      stringr::str_detect(lowered, "remove (?:the )?sanction of show cause")) {
    return("Removed from Show Cause")
  }

  if (acc_norm == "NWCCU" &&
      stringr::str_detect(lowered, "remove (?:the )?sanction of warning")) {
    return("Accreditation Reaffirmed: Warning Removed")
  }

  if (acc_norm == "NWCCU" &&
      stringr::str_detect(lowered, "issue(?: a)? sanction of show cause")) {
    return("Required to Show Cause")
  }

  if (acc_norm == "NWCCU" &&
      stringr::str_detect(lowered, "issue(?: a)? sanction of warning")) {
    return("Placed on Warning")
  }

  if (stringr::str_detect(lowered, "^voluntary withdrawal received$|^loss of accreditation or preaccreditation: voluntary withdrawal$")) {
    if (acc_norm == "WSCUC" &&
        stringr::str_detect(notes_text, stringr::regex("officially closed its campus and is no longer accredited", ignore_case = TRUE))) {
      return("Institution closed and no longer accredited")
    }
    return("Voluntarily Surrendered Accreditation")
  }

  if (acc_norm == "WSCUC") {
    letter_summary <- .summarize_wscuc_letter(cleaned)
    if (!is.na(letter_summary) && nzchar(letter_summary)) {
      return(letter_summary)
    }
    if (identical(trimws(notes_text), "Sonoma State University") &&
        stringr::str_detect(
          cleaned,
          stringr::regex("^Defer Action on Reaffirmation of accreditation\\s*/\\s*Issue a Notice of Concern$", ignore_case = TRUE)
        )) {
      return("Issued a Notice of Concern over Standards 1 and 3, CFRs 1, 1.7, 3.11, and 3.4 on financial sustainability and shared governance")
    }
    if (stringr::str_detect(
      cleaned,
      stringr::regex("^Defer Action on Reaffirmation of accreditation\\s*/\\s*Issue a Notice of Concern$", ignore_case = TRUE)
    )) {
      return("Deferred action on reaffirmation of accreditation and issued a Notice of Concern")
    }
    wscuc_summary <- stringr::str_replace(
      cleaned,
      stringr::regex("^Following a Special Visit\\s*(?:–|-|:)\\s*", ignore_case = TRUE),
      ""
    )
    if (!identical(wscuc_summary, cleaned) ||
        stringr::str_detect(lowered, "criteria for accreditation|formal notice of concern|\\bwarning\\b|\\bprobation\\b|reaffirm accreditation")) {
      wscuc_summary <- gsub("\\s*/\\s*", " and ", wscuc_summary, perl = TRUE)
      return(.normalize_sentence_case_summary(wscuc_summary))
    }
  }

  closure_announcement <- stringr::str_match(
    cleaned,
    stringr::regex(
      "^On\\s+[A-Z][a-z]+\\s+\\d{1,2},\\s+\\d{4},\\s+(.+?)\\s+announced that its Board of Trustees had voted(?:\\s+on\\s+[A-Z][a-z]+\\s+\\d{1,2},\\s+\\d{4})?\\s+to\\s+(.+)$",
      ignore_case = TRUE
    )
  )
  if (!is.na(closure_announcement[1, 1])) {
    institution_name <- stringr::str_squish(closure_announcement[1, 2])
    remainder <- stringr::str_squish(closure_announcement[1, 3])
    if (nzchar(institution_name) && nzchar(remainder)) {
      return(.capitalize_summary_head(sprintf(
        "%s announced that its Board of Trustees had voted to %s",
        institution_name,
        remainder
      )))
    }
  }

  if (acc_norm == "NECHE" &&
      stringr::str_detect(lowered, "given an opportunity to show cause") &&
      stringr::str_detect(lowered, "placed on probation") &&
      stringr::str_detect(lowered, "withdrawn")) {
    standard_label <- .build_neche_standard_concern_label(cleaned)
    if (!is.na(standard_label) && nzchar(standard_label)) {
      return(sprintf(
        "Asked to Show Cause for possible Probation or Withdrawal over %s",
        standard_label
      ))
    }
    return("Asked to Show Cause for Probation or Withdrawal of Accreditation")
  }

  if (acc_norm == "NECHE" &&
      stringr::str_detect(lowered, "took action to require .* show cause") &&
      stringr::str_detect(lowered, "placed on probation") &&
      stringr::str_detect(lowered, "withdrawn")) {
    standard_label <- .build_neche_standard_concern_label(cleaned)
    if (!is.na(standard_label) && nzchar(standard_label)) {
      return(sprintf(
        "Asked to Show Cause for possible Probation or Withdrawal over %s",
        standard_label
      ))
    }
    return("Asked to Show Cause for Probation or Withdrawal of Accreditation")
  }

  if (acc_norm == "NECHE" &&
      stringr::str_detect(lowered, "voted to remove .* from probation")) {
    return("Accreditation Reaffirmed: Probation Removed")
  }

  if (acc_norm == "NECHE" &&
      stringr::str_detect(lowered, "^from probation for failure to meet the standards on")) {
    return("Accreditation Reaffirmed: Probation Removed")
  }

  if (acc_norm == "NECHE" &&
      stringr::str_detect(lowered, "voted to place .* on probation")) {
    probation_reason <- stringr::str_match(
      cleaned,
      stringr::regex(
        "voted to place [^.]+? on probation[^.]*?because the Commission found that [^.]+? does not now meet the Commission.?s standards on (.+?)\\.",
        ignore_case = TRUE
      )
    )[, 2]
    if (!is.na(probation_reason) && nzchar(probation_reason)) {
      return(.capitalize_summary_head(sprintf(
        "Placed on Probation for failure to meet the standards on %s.",
        stringr::str_squish(probation_reason)
      )))
    }
    return("Placed on Probation")
  }

  if (acc_norm == "NECHE" &&
      stringr::str_detect(lowered, "^on probation for a period not to exceed")) {
    probation_reason <- stringr::str_match(
      cleaned,
      stringr::regex(
        "on probation for a period not to exceed [^.]+? because the Commission found that [^.]+? (?:does not now meet|does not meet) the Commission.?s standards on (.+?)\\.?$",
        ignore_case = TRUE
      )
    )[, 2]
    if (!is.na(probation_reason) && nzchar(probation_reason)) {
      return(.capitalize_summary_head(sprintf(
        "Placed on Probation for failure to meet the standards on %s.",
        stringr::str_squish(probation_reason)
      )))
    }
    return("Placed on Probation")
  }

  if (acc_norm == "NECHE" &&
      stringr::str_detect(lowered, "voted to ask .* to show cause why it should not be placed on probation") &&
      stringr::str_detect(lowered, "is not meeting the commission")) {
    institution_name <- stringr::str_match(
      cleaned,
      stringr::regex(
        "voted to ask (.+?) to show cause why it should not be placed on probation",
        ignore_case = TRUE
      )
    )[, 2]
    institution_name <- stringr::str_squish(gsub(",\\s*Inc\\.?$", "", institution_name, ignore.case = TRUE))
    standards_text <- stringr::str_match(
      cleaned,
      stringr::regex(
        "is not meeting the Commission.*? standards on (.+?)\\.",
        ignore_case = TRUE
      )
    )[, 2]
    standards_text <- stringr::str_squish(standards_text)
    if (!is.na(standards_text) && nzchar(standards_text)) {
      lead <- if (!is.na(institution_name) && nzchar(institution_name)) {
        institution_name
      } else {
        "the institution"
      }
      family_labels <- .format_neche_standard_family_labels(
        extract_neche_standard_families(standards_text)
      )
      if (!is.na(family_labels) && nzchar(family_labels)) {
        return(.capitalize_summary_head(sprintf(
          "Concerns %s may no longer meet %s.",
          lead,
          family_labels
        )))
      }
      return(.capitalize_summary_head(sprintf(
        "Concerns %s may no longer meet the standards on %s.",
        lead,
        standards_text
      )))
    }
  }

  if (acc_norm == "NECHE" &&
      stringr::str_detect(lowered, "^to show cause why it should not be placed on probation") &&
      stringr::str_detect(lowered, "had reason to believe")) {
    standards_text <- stringr::str_match(
      cleaned,
      stringr::regex(
        "because the Commission had reason to believe that [^.]+? (?:is not meeting|may no longer meet) the Commission.*? standards on (.+?)\\.?$",
        ignore_case = TRUE
      )
    )[, 2]
    institution_name <- stringr::str_match(
      cleaned,
      stringr::regex(
        "because the Commission had reason to believe that ([^.]+?) (?:is not meeting|may no longer meet)",
        ignore_case = TRUE
      )
    )[, 2]
    institution_name <- stringr::str_squish(gsub(",\\s*Inc\\.?$", "", institution_name, ignore.case = TRUE))
    if (!is.na(standards_text) && nzchar(standards_text)) {
      lead <- if (!is.na(institution_name) && nzchar(institution_name)) institution_name else "the institution"
      family_labels <- .format_neche_standard_family_labels(
        extract_neche_standard_families(standards_text)
      )
      if (!is.na(family_labels) && nzchar(family_labels)) {
        return(.capitalize_summary_head(sprintf(
          "Concerns %s may no longer meet %s.",
          lead,
          family_labels
        )))
      }
      return(.capitalize_summary_head(sprintf(
        "Concerns %s may no longer meet the standards on %s.",
        lead,
        stringr::str_squish(standards_text)
      )))
    }
  }

  if (acc_norm == "NECHE" &&
      stringr::str_detect(lowered, "\\bnotation\\b") &&
      stringr::str_detect(lowered, "institutional resources")) {
    return("Notation for Institutional Resources Risk")
  }

  if (acc_norm == "NECHE" &&
      stringr::str_detect(lowered, "joint press release") &&
      stringr::str_detect(lowered, "show-cause response")) {
    if (stringr::str_detect(notes_lower, "in danger of being found not to meet")) {
      detail <- stringr::str_match(
        notes_text,
        stringr::regex("^Heightened Monitoring or Focused Review\\s*\\|\\s*(.+)$", ignore_case = TRUE)
      )[, 2]
      if (!is.na(detail) && nzchar(detail)) {
        return(.capitalize_summary_head(sprintf(
          "%s. Placed on Heightened Monitoring or Focused Review.",
          stringr::str_squish(detail)
        )))
      }
    }
    return("Placed on Heightened Monitoring or Focused Review")
  }

  if (acc_norm == "HLC") {
    cleaned <- stringr::str_replace(
      cleaned,
      stringr::regex("^summary of the action:\\s*", ignore_case = TRUE),
      ""
    )
    lowered_no_prefix <- tolower(cleaned)
    note_source_text <- if (nzchar(trimws(notes_text))) notes_text else cleaned
    note_parts <- stringr::str_split(note_source_text, stringr::regex("\\s*\\|\\s*"), n = 2)[[1]]
    primary_note <- if (length(note_parts) >= 1L) stringr::str_squish(note_parts[[1]]) else ""
    secondary_note <- if (length(note_parts) >= 2L) stringr::str_squish(note_parts[[2]]) else ""
    hlc_summary_clause <- .extract_hlc_summary_clause(cleaned)
    hlc_summary_text <- hlc_summary_clause %||% cleaned
    hlc_summary_lower <- tolower(hlc_summary_text)
    hlc_context_text <- stringr::str_squish(paste(hlc_summary_text, notes_text))
    reason_match <- stringr::str_match(
      secondary_note,
      stringr::regex("because (.+?)(?:\\.|$)", ignore_case = TRUE)
    )[, 2]
    if ((is.na(reason_match) || !nzchar(reason_match)) && nzchar(hlc_summary_text)) {
      reason_match <- stringr::str_match(
        hlc_summary_text,
        stringr::regex("because (.+?)(?:\\.|$)", ignore_case = TRUE)
      )[, 2]
    }
    if (!is.na(reason_match) && nzchar(reason_match)) {
      reason_match <- stringr::str_squish(reason_match)
      reason_match <- stringr::str_replace(
        reason_match,
        stringr::regex("\\bcriteria for accreditation\\b", ignore_case = TRUE),
        "Criteria for Accreditation"
      )
      reason_match <- stringr::str_replace(
        reason_match,
        stringr::regex("^it determined that the institution was ", ignore_case = TRUE),
        "the institution was "
      )
      reason_match <- stringr::str_replace(
        reason_match,
        stringr::regex("^it determined that the institution is ", ignore_case = TRUE),
        "the institution is "
      )
      reason_match <- .specialize_hlc_reason(reason_match, hlc_context_text)
    }
    if (stringr::str_detect(primary_note, stringr::regex("^Accreditation Reaffirmed:\\s*Warning Removed$", ignore_case = TRUE))) {
      return("Accreditation Reaffirmed: Warning Removed")
    }
    if (stringr::str_detect(primary_note, stringr::regex("^Accreditation Reaffirmed:\\s*Probation Removed$", ignore_case = TRUE))) {
      return("Accreditation Reaffirmed: Probation Removed")
    }
    if (stringr::str_detect(primary_note, stringr::regex("^Probation or Equivalent or a More Severe Status:\\s*Warning$", ignore_case = TRUE))) {
      if (!is.na(reason_match) && nzchar(reason_match)) {
        return(sprintf("Placed on Warning because %s.", reason_match))
      }
      return("Placed on Warning")
    }
    if (stringr::str_detect(primary_note, stringr::regex("^Probation or Equivalent or a More Severe Status:\\s*Probation$", ignore_case = TRUE))) {
      if (!is.na(reason_match) && nzchar(reason_match)) {
        return(sprintf("Placed on Probation because %s.", reason_match))
      }
      return("Placed on Probation")
    }
    if (stringr::str_detect(primary_note, stringr::regex("^Probation or Equivalent or a More Severe Status:\\s*Show Cause$", ignore_case = TRUE))) {
      return("Asked to Show Cause")
    }
    board_summary_with_reason <- stringr::str_match(
      hlc_summary_text,
      stringr::regex("^(The Board[^.]*? because )(.+?)(\\.)", ignore_case = TRUE)
    )
    if (!is.na(board_summary_with_reason[1, 1])) {
      specialized_reason <- .specialize_hlc_reason(
        stringr::str_squish(board_summary_with_reason[1, 3]),
        hlc_context_text
      )
      return(sprintf(
        "%s %s.",
        stringr::str_squish(board_summary_with_reason[1, 2]),
        specialized_reason
      ))
    }
    placed_with_reason <- stringr::str_match(
      hlc_summary_text,
      stringr::regex(
        "^The Institution has been placed on (Notice|Probation) because (.+?)\\.(?:\\s|$)",
        ignore_case = TRUE
      )
    )
    if (!is.na(placed_with_reason[1, 1])) {
      status <- dplyr::case_when(
        identical(action_type, "warning") ~ "Warning",
        identical(action_type, "notice") ~ "Notice",
        TRUE ~ stringr::str_to_title(tolower(stringr::str_squish(placed_with_reason[1, 2])))
      )
      reason <- stringr::str_squish(placed_with_reason[1, 3])
      reason <- stringr::str_replace(
        reason,
        stringr::regex("\\bcriteria for accreditation\\b", ignore_case = TRUE),
        "Criteria for Accreditation"
      )
      reason <- .specialize_hlc_reason(reason, hlc_context_text)
      return(sprintf("Placed on %s because %s.", status, reason))
    }
    if (stringr::str_detect(hlc_summary_lower, "placed on notice")) {
      return("Placed on Notice")
    }
    if (stringr::str_detect(hlc_summary_lower, "placed on probation")) {
      return("Placed on Probation")
    }
    if (stringr::str_detect(primary_note, stringr::regex("^Accreditation Reaffirmed:\\s*Warning Removed$", ignore_case = TRUE)) ||
        stringr::str_detect(hlc_summary_lower, "accreditation reaffirmed:\\s*warning removed")) {
      return("Accreditation Reaffirmed: Warning Removed")
    }
    if (stringr::str_detect(primary_note, stringr::regex("^Accreditation Reaffirmed:\\s*Probation Removed$", ignore_case = TRUE)) ||
        stringr::str_detect(hlc_summary_lower, "accreditation reaffirmed:\\s*probation removed")) {
      return("Accreditation Reaffirmed: Probation Removed")
    }
    if (stringr::str_detect(hlc_summary_lower, "removed .* from probation")) {
      return("Accreditation Reaffirmed: Probation Removed")
    }
    if (stringr::str_detect(hlc_summary_lower, "removed .* from notice")) {
      return("Accreditation Reaffirmed: Warning Removed")
    }
    if (stringr::str_detect(lowered_no_prefix, "require the institution to provide an interim report")) {
      return("Required to provide an interim report")
    }
    if (stringr::str_detect(lowered_no_prefix, "now meets without concerns criterion") &&
        stringr::str_detect(lowered_no_prefix, "be discontinued")) {
      criterion <- stringr::str_match(
        cleaned,
        stringr::regex(
          "now meets without concerns ([^,]+(?:,\\s*Core Component\\s*[A-Z0-9.]+)?)",
          ignore_case = TRUE
        )
      )[, 2]
      program <- stringr::str_match(
        cleaned,
        stringr::regex(
          "that the ([^.]+? program),\\s+the sole program in question at the time of sanction, be discontinued",
          ignore_case = TRUE
        )
      )[, 2]
      criterion <- stringr::str_squish(criterion)
      program <- stringr::str_squish(program)
      if (!is.na(criterion) && nzchar(criterion) && !is.na(program) && nzchar(program)) {
        return(sprintf("Concerns about %s were resolved after discontinuing the %s.", criterion, program))
      }
    }
    if (stringr::str_detect(lowered_no_prefix, "teach(?:-|\\s)?out") &&
        stringr::str_detect(lowered_no_prefix, "additional location|additional locations|branch campus") &&
        !stringr::str_detect(
          lowered_no_prefix,
          "teach(?:-|\\s)?out agreements?\\s+with\\s+the\\s+following institutions?"
        )) {
      location_summary <- .build_hlc_teachout_location_summary(cleaned)
      if (!is.na(location_summary) && nzchar(location_summary)) {
        return(location_summary)
      }
    }
    if (stringr::str_detect(lowered_no_prefix, "approved the institution.?s provisional") &&
        stringr::str_detect(lowered_no_prefix, "teach(?:-|\\s)?out agreements?")) {
      partners <- .summarize_partner_list(.extract_teachout_partners(cleaned))
      if (!is.na(partners) && nzchar(partners)) {
        return(sprintf("Approved provisional plan and teach-out agreements with %s", partners))
      }
      return("Approved provisional plan and teach-out agreements")
    }
    if (stringr::str_detect(lowered_no_prefix, "approved the institution") &&
        stringr::str_detect(lowered_no_prefix, "teach(?:-|\\s)?out agreements?") &&
        (stringr::str_detect(lowered_no_prefix, "following institutions?(?:\\s*\\([^)]*\\))?:") ||
         stringr::str_detect(lowered_no_prefix, "as additions to the provisional plan"))) {
      inline_partners <- .extract_hlc_inline_teachout_target(cleaned)
      if (!is.na(inline_partners) && nzchar(inline_partners) &&
          !stringr::str_detect(lowered_no_prefix, "following institutions?(?:\\s*\\([^)]*\\))?:") &&
          !stringr::str_detect(inline_partners, stringr::regex("^the following institutions?$", ignore_case = TRUE))) {
        return(sprintf("Approved teach-out agreements with %s", inline_partners))
      }
      partners <- .summarize_partner_list(.extract_teachout_partners(cleaned))
      if (!is.na(partners) && nzchar(partners)) {
        return(sprintf("Approved teach-out agreements with %s", partners))
      }
      return("Approved teach-out agreements")
    }
    if (stringr::str_detect(lowered_no_prefix, "approved the institution") &&
        stringr::str_detect(lowered_no_prefix, "teach(?:-|\\s)?out agreement with")) {
      inline_target <- .extract_hlc_inline_teachout_target(cleaned)
      if (!is.na(inline_target) && nzchar(inline_target)) {
        return(sprintf("Approved the institution’s teach-out agreement with %s", inline_target))
      }
    }

    if (stringr::str_detect(lowered_no_prefix, "voluntary resignation of accreditation") ||
        stringr::str_detect(lowered_no_prefix, "voluntarily resigned its accreditation")) {
      return("Voluntarily Surrendered Accreditation")
    }
  }

  if (acc_norm == "SACSCOC" &&
      stringr::str_detect(lowered, "(is requested to submit|should submit) a referral report")) {
    areas <- .summarize_standard_areas(.extract_standard_areas(cleaned))
    if (!is.na(areas) && nzchar(areas) && !identical(areas, "certain accreditation standards")) {
      return(.capitalize_summary_head(sprintf(
        "Requested Referral Report on %s",
        areas
      )))
    }
    return("Requested to submit a Referral Report documenting compliance with accreditation standards")
  }

  if (acc_norm == "SACSCOC" &&
      stringr::str_detect(lowered, "removed the institution from probation for good cause")) {
    return("Removed from Probation for Good Cause")
  }

  if (acc_norm == "SACSCOC" &&
      stringr::str_detect(lowered, "reaffirmed and removed from warning")) {
    return("Accreditation Reaffirmed: Warning Removed")
  }

  if (acc_norm == "SACSCOC" &&
      stringr::str_detect(lowered, "reaffirmed and removed from probation")) {
    return("Accreditation Reaffirmed: Probation Removed")
  }

  if (acc_norm == "SACSCOC" &&
      stringr::str_detect(lowered, "recommended removal from probation")) {
    return("Removed from Probation")
  }

  if (acc_norm == "SACSCOC" &&
      stringr::str_detect(lowered, "removed the institution from probation")) {
    return("Removed from Probation")
  }

  if (acc_norm == "SACSCOC" &&
      stringr::str_detect(lowered, "recommended removal from warning")) {
    return("Removed from Warning")
  }

  if (acc_norm == "SACSCOC" &&
      stringr::str_detect(lowered, "recommended the removal of") &&
      stringr::str_detect(lowered, "from membership") &&
      stringr::str_detect(lowered, "failure to comply with")) {
    removal_clause <- stringr::str_match(
      cleaned,
      stringr::regex(
        "(recommended the removal of .+? from membership for failure to comply with .+?)(?=\\s+The recommendation of the Committee\\b|\\s+The policies and procedures of SACSCOC\\b|$)",
        ignore_case = TRUE
      )
    )[, 2]
    if (!is.na(removal_clause) && nzchar(removal_clause)) {
      removal_clause <- stringr::str_replace(
        removal_clause,
        stringr::regex("^recommended the removal of .+? from membership", ignore_case = TRUE),
        "Removed from membership"
      )
      removal_clause <- .compact_sacscoc_sanction_summary(removal_clause)
      return(.capitalize_summary_head(stringr::str_squish(removal_clause)))
    }
  }

  if (acc_norm == "SACSCOC" &&
      stringr::str_detect(lowered, "removed the institution from warning")) {
    return("Removed from Warning")
  }

  if (acc_norm == "SACSCOC" &&
      stringr::str_detect(lowered, "reviewed the institution.?s referral report") &&
      stringr::str_detect(lowered, "requested a monitoring report")) {
    standard_area <- stringr::str_match(
      cleaned,
      stringr::regex("Standard\\s+[^()]+\\(([^)]+)\\)", ignore_case = TRUE)
    )[, 2]
    if (!is.na(standard_area) && nzchar(standard_area)) {
      return(.capitalize_summary_head(sprintf(
        "Requested to Submit a Monitoring Report on %s",
        stringr::str_to_title(tolower(stringr::str_squish(standard_area)))
      )))
    }
    return("Requested to Submit a Monitoring Report")
  }

  if (acc_norm == "SACSCOC" &&
      stringr::str_detect(lowered, "requested to submit a (first|second|third|fourth|fifth)?\\s*monitoring report") &&
      !stringr::str_detect(lowered, "placed on warning|placed the institution on warning|placed on probation|placed the institution on probation|continued .* on warning|continued .* on probation|removed the institution from|denied reaffirmation|show cause")) {
    areas <- .summarize_standard_areas(.extract_standard_areas(cleaned))
    if (!is.na(areas) && nzchar(areas) && !identical(areas, "certain accreditation standards")) {
      return(.capitalize_summary_head(sprintf(
        "Requested to Submit a Monitoring Report on %s",
        stringr::str_to_title(areas)
      )))
    }
    return("Requested to Submit a Monitoring Report")
  }

  if (acc_norm == "SACSCOC" &&
      stringr::str_detect(lowered, "no additional report was requested")) {
    monitoring_round <- stringr::str_match(
      cleaned,
      stringr::regex(
        "reviewed the institution.?s\\s+(first|second|third|fourth|fifth)\\s+monitoring report",
        ignore_case = TRUE
      )
    )[, 2]
    if (!is.na(monitoring_round) && nzchar(monitoring_round)) {
      return(.capitalize_summary_head(sprintf(
        "No additional report requested after the %s Monitoring Report",
        stringr::str_to_title(tolower(stringr::str_squish(monitoring_round)))
      )))
    }
    return("No additional report requested after review of the Monitoring Report")
  }

  if (acc_norm == "SACSCOC" &&
      stringr::str_detect(lowered, "^requested a monitoring report\\b")) {
    return(.capitalize_summary_head(stringr::str_replace(
      cleaned,
      stringr::regex("^requested a monitoring report\\b", ignore_case = TRUE),
      "Requested to Submit a Monitoring Report"
    )))
  }

  if (acc_norm == "SACSCOC" &&
      stringr::str_detect(lowered, "requested a monitoring report")) {
    requested_clause <- stringr::str_match(
      cleaned,
      stringr::regex("(requested a monitoring report[^.]*\\.)", ignore_case = TRUE)
    )[, 2]
    if (!is.na(requested_clause) && nzchar(requested_clause)) {
      return(.capitalize_summary_head(stringr::str_replace(
        requested_clause,
        stringr::regex("^requested a monitoring report\\b", ignore_case = TRUE),
        "Requested to Submit a Monitoring Report"
      )))
    }
  }

  if (acc_norm == "SACSCOC" &&
      stringr::str_detect(lowered, "recommended that the institution be placed on warning")) {
    recommended_clause <- stringr::str_match(
      cleaned,
      stringr::regex(
        "(recommended that the institution be placed on warning.+?)(?=\\s+The institution is requested\\b|\\s+Guidelines for the Monitoring Report\\b|\\s+A Special Committee [^.]*authorized[^.]*\\b|$)",
        ignore_case = TRUE
      )
    )[, 2]
    if (!is.na(recommended_clause) && nzchar(recommended_clause)) {
      recommended_clause <- stringr::str_replace(
        recommended_clause,
        stringr::regex("\\s*A Special Committee [^.]*authorized[^.]*\\.?$", ignore_case = TRUE),
        ""
      )
      recommended_clause <- stringr::str_replace(
        recommended_clause,
        stringr::regex("^recommended that the institution be placed on warning", ignore_case = TRUE),
        "Recommended that the institution be placed on Warning"
      )
      return(.capitalize_summary_head(stringr::str_squish(recommended_clause)))
    }
  }

  if (acc_norm == "SACSCOC" &&
      stringr::str_detect(lowered, "loss of accreditation or preaccreditation: other")) {
    return("Loss of Accreditation")
  }

  if (acc_norm == "SACSCOC") {
    off_campus_review <- stringr::str_match(
      cleaned,
      stringr::regex(
        "continued accreditation following the review of an off-?\\s*campus instructional site located at (.+?) \\(approved [^)]+\\)\\.?$",
        ignore_case = TRUE
      )
    )[, 2]
    if (!is.na(off_campus_review) && nzchar(off_campus_review)) {
      return(.capitalize_summary_head(sprintf(
        "Continued accreditation following review of the off-campus instructional site at %s.",
        stringr::str_squish(off_campus_review)
      )))
    }

    denied_program_match <- stringr::str_match(
      cleaned,
      stringr::regex(
        "denied approval of (.+?) because the institution did not provide an acceptable plan and supporting documentation to ensure that it has the capability to comply with the following standards of the Principles of Accreditation(?: as they relate to the substantive change)?: (.+)$",
        ignore_case = TRUE
      )
    )
    if (!is.na(denied_program_match[1, 1])) {
      program_text <- stringr::str_squish(denied_program_match[1, 2])
      standards_text <- stringr::str_squish(sub("\\.?$", "", denied_program_match[1, 3]))
      area_summary <- .summarize_standard_areas(.extract_standard_areas(standards_text))
      if (is.na(area_summary) || !nzchar(area_summary)) {
        area_summary <- "certain accreditation standards"
      }
      return(.capitalize_summary_head(sprintf(
        "Denied approval of %s because the institution did not provide an acceptable plan and supporting documentation to show compliance with %s.",
        program_text,
        area_summary
      )))
    }

    garbled_warning_match <- stringr::str_match(
      cleaned,
      stringr::regex(
        "(denied reaffirmation,? continued accreditation,? and placed the institution on Warning for (?:12|twelve) months)(?:\\b|\\.)",
        ignore_case = TRUE
      )
    )[, 2]
    if (!is.na(garbled_warning_match) && nzchar(garbled_warning_match)) {
      return(.capitalize_summary_head(stringr::str_squish(garbled_warning_match)))
    }

    disclosure_reason <- .extract_sacscoc_disclosure_reason_summary(cleaned)
    if (!is.na(disclosure_reason) && nzchar(disclosure_reason)) {
      return(disclosure_reason)
    }

    if (!stringr::str_detect(lowered, "denied reaffirmation") &&
        !.is_garbled_action_summary(cleaned)) {
      warning_or_probation_clause <- .extract_sacscoc_warning_probation_clause(cleaned)
    } else {
      warning_or_probation_clause <- NA_character_
    }
    if (!is.na(warning_or_probation_clause) && nzchar(warning_or_probation_clause)) {
      warning_or_probation_clause <- stringr::str_replace(
        warning_or_probation_clause,
        stringr::regex("\\s*A Special Committee [^.]*authorized[^.]*\\.?$", ignore_case = TRUE),
        ""
      )
      warning_or_probation_clause <- stringr::str_replace(
        warning_or_probation_clause,
        stringr::regex("^reviewed the institution[’'`]s\\s+(?:first|second|third|fourth|fifth)?\\s*monitoring report(?:\\s+following[^,.;]*?)?,?\\s+and\\s+", ignore_case = TRUE),
        ""
      )
      warning_or_probation_clause <- stringr::str_replace(
        warning_or_probation_clause,
        stringr::regex("^placed the institution on warning\\b", ignore_case = TRUE),
        "Placed on Warning"
      )
      warning_or_probation_clause <- stringr::str_replace(
        warning_or_probation_clause,
        stringr::regex("^placed the institution on probation\\b", ignore_case = TRUE),
        "Placed on Probation"
      )
      warning_or_probation_clause <- .compact_sacscoc_sanction_summary(warning_or_probation_clause)
      return(.capitalize_summary_head(stringr::str_squish(warning_or_probation_clause)))
    }

    if (stringr::str_detect(lowered, "disclosure statement regarding the status")) {
      disclosure_reason <- .extract_sacscoc_disclosure_reason_summary(cleaned)
      if (!is.na(disclosure_reason) && nzchar(disclosure_reason)) {
        return(disclosure_reason)
      }
      if (stringr::str_detect(tolower(action_type %||% ""), "show_cause|show cause")) {
        return("Disclosure Statement Regarding Show Cause Status")
      }
      return("Disclosure Statement Regarding Accreditation Status")
    }
  }

  clause <- .extract_action_clause_after_marker(cleaned)
  substantive <- .extract_substantive_action_sentence(clause)
  if (!is.na(substantive) && nzchar(substantive)) {
    substantive <- stringr::str_replace(
      substantive,
      stringr::regex(
        "^(the sacscoc board of trustees|the executive council, acting on behalf of the sacscoc board of trustees),?\\s+",
        ignore_case = TRUE
      ),
      ""
    )
    substantive <- stringr::str_replace(
      substantive,
      stringr::regex("^continue\\s+([A-Z][^.]*)\\s+on warning\\b", ignore_case = TRUE),
      "Continued \\1 on Warning"
    )
    substantive <- stringr::str_replace(
      substantive,
      stringr::regex("^continue\\s+([A-Z][^.]*)\\s+on probation\\b", ignore_case = TRUE),
      "Continued \\1 on Probation"
    )
    substantive <- stringr::str_replace(
      substantive,
      stringr::regex("^placed the institution on warning\\b", ignore_case = TRUE),
      "Placed on Warning"
    )
    substantive <- stringr::str_replace(
      substantive,
      stringr::regex("^reviewed the institution[â€™'`]s\\s+[^.]*?\\s+placed the institution on warning\\b", ignore_case = TRUE),
      "Placed on Warning"
    )
    substantive <- stringr::str_replace(
      substantive,
      stringr::regex("^reviewed the institution[â€™'`]s\\s+[^.]*?\\s+placed the institution on probation\\b", ignore_case = TRUE),
      "Placed on Probation"
    )
    substantive <- stringr::str_replace(
      substantive,
      stringr::regex("^placed the institution on probation\\b", ignore_case = TRUE),
      "Placed on Probation"
    )
    substantive <- stringr::str_replace(
      substantive,
      stringr::regex("\\s*A Special Committee [^.]*authorized[^.]*\\.?$", ignore_case = TRUE),
      ""
    )
    if (acc_norm == "SACSCOC") {
      substantive <- .compact_sacscoc_sanction_summary(substantive)
    }
    if (acc_norm == "SACSCOC" && .is_garbled_action_summary(substantive)) {
      type_norm <- tolower(trimws(as.character(action_type %||% "")))
      if (type_norm == "warning") return("Placed on Warning")
      if (type_norm == "probation") return("Placed on Probation")
      if (type_norm == "removed") return("Removed from Sanction")
      if (type_norm == "show_cause") return("Required to Show Cause")
      if (type_norm == "notice") return("Heightened Monitoring or Focused Review")
    }
    return(stringr::str_trim(substantive))
  }

  cleaned
}

# Scalar-only on purpose: callers iterate per-action via mapply/lapply.
derive_action_label_short <- function(action_type, action_label_raw, accreditor = NA_character_, notes = NA_character_) {
  acc_norm <- toupper(trimws(as.character(accreditor %||% "")))
  raw <- .normalize_action_summary_text(action_label_raw)

  if (is.na(raw) || !nzchar(raw)) {
    if (!is.na(action_type) && nzchar(action_type)) return(as.character(action_type))
    return("Action")
  }

  # Non-MSCHE: passthrough action_label_raw verbatim. Other accreditors'
  # scrape-time labels are already informative; do not rewrite them.
  if (!identical(acc_norm, "MSCHE")) {
    summary <- .summarize_non_msche_action_label(action_type, raw, accreditor, notes)
    if (!is.na(summary) && nzchar(summary)) {
      return(.capitalize_summary_head(as.character(summary)))
    }
    if (!is.na(action_type) && nzchar(action_type)) {
      return(as.character(action_type))
    }
    return("Action")
  }

  if (grepl("^Accreditation Reaffirmed:\\s*Warning Removed$", raw, ignore.case = TRUE, perl = TRUE)) {
    return("Accreditation Reaffirmed: Warning Removed")
  }
  if (grepl("^Accreditation Reaffirmed:\\s*Probation Removed$", raw, ignore.case = TRUE, perl = TRUE)) {
    return("Accreditation Reaffirmed: Probation Removed")
  }
  if (grepl("^Probation or Equivalent or a More Severe Status:\\s*Warning$", raw, ignore.case = TRUE, perl = TRUE)) {
    return("Placed on Warning")
  }
  if (grepl("^Probation or Equivalent or a More Severe Status:\\s*Probation$", raw, ignore.case = TRUE, perl = TRUE)) {
    return("Placed on Probation")
  }
  if (grepl("^Probation or Equivalent or a More Severe Status:\\s*Show Cause$", raw, ignore.case = TRUE, perl = TRUE)) {
    return("Asked to Show Cause")
  }

  if (stringr::str_detect(tolower(raw), "notification of non-compliance probation action")) {
    return("Non-Compliance Probation")
  }
  if (stringr::str_detect(tolower(raw), "notification of non-compliance show cause action")) {
    return("Required to Show Cause")
  }
  if (stringr::str_detect(tolower(raw), "notification of non-compliance warning action")) {
    return("Non-Compliance Warning")
  }
  if (stringr::str_detect(tolower(raw), "^voluntary withdrawal received$|^loss of accreditation or preaccreditation: voluntary withdrawal$")) {
    return("Voluntarily Surrendered Accreditation")
  }

  # ----- Pattern 0a: Merger / change of control with effective date -----
  # MSCHE complex-substantive-change rows describe institutional mergers
  # in two shapes: an explicit "merger of <X> with <Y>, effective <date>"
  # sentence (when present), or a "to include the change in legal status,
  # form of control, and ownership ... effective <date>" sentence as a
  # fallback. Surfacing the merging partner is more informative than
  # falling through to the generic "change of legal status" boilerplate.
  m_merger <- stringr::str_match(raw, stringr::regex(
    "merger of ([^,.]+?) (with|into) ([^,.]+?),\\s*effective,?\\s*([A-Z][a-z]+\\s+\\d{1,2},\\s+\\d{4})",
    ignore_case = TRUE
  ))
  if (!is.na(m_merger[1, 1])) {
    # Surface BOTH institution names so the label is unambiguous on
    # either party's accreditation page. The earlier shape captured
    # only the partner name and emitted "Merger with <Y>", which read
    # as a self-reference on <Y>'s own row (e.g. Russell Sage's page
    # showed "Merger with Russell Sage College").
    inst_a   <- stringr::str_squish(m_merger[1, 2])
    connector <- tolower(stringr::str_squish(m_merger[1, 3]))
    inst_b   <- stringr::str_squish(m_merger[1, 4])
    eff_date <- stringr::str_squish(m_merger[1, 5])
    return(.capitalize_summary_head(paste0(
      "Merger of ", inst_a, " ", connector, " ", inst_b,
      " (effective ", eff_date, ")"
    )))
  }
  m_legal_status <- stringr::str_match(raw, stringr::regex(
    "to include the change in legal status[^.]*?effective ([A-Z][a-z]+\\s+\\d{1,2},\\s+\\d{4})",
    ignore_case = TRUE
  ))
  if (!is.na(m_legal_status[1, 1])) {
    eff_date <- stringr::str_squish(m_legal_status[1, 2])
    return(.capitalize_summary_head(paste0("Change of Legal Status (effective ", eff_date, ")")))
  }

  # ----- Pattern 0a-2: Institutional Closure with effective date -----
  # MSCHE phrasing for closure board actions: "To note the institution
  # will close all locations on <date>" or "To note that the institution
  # will close ... effective <date>". Saint Rose's April 25, 2024 row
  # carries this shape inside a multi-sentence body that begins with
  # "To acknowledge receipt of the substantive change request..." and
  # only the SECOND or later sentence carries the closure date. The
  # match is deliberately not anchored to start-of-string.
  m_closure <- stringr::str_match(raw, stringr::regex(
    "to note (?:that )?the institution will close [^.]*?on ([A-Z][a-z]+\\s+\\d{1,2},\\s+\\d{4})",
    ignore_case = TRUE
  ))
  if (!is.na(m_closure[1, 1])) {
    eff_date <- stringr::str_squish(m_closure[1, 2])
    return(paste0("Approved Institutional Closure (effective ", eff_date, ")"))
  }

  # ----- Pattern 0b: Approved Teach-Out Agreement with named partner -----
  # Distinct from Pattern 1 (teach-out PLAN approval). MSCHE phrasing:
  # "To approve the teach-out agreement with <Partner>, <city>, <state>".
  # The named-partner shape is meaningfully different from the multi-
  # institution plan approval ("...and agreements with several
  # institutions") so it gets its own bucket.
  m_agreement <- stringr::str_match(raw, stringr::regex(
    "to approve the teach-?out agreement with ([^,.]+?)(?:,|\\.)",
    ignore_case = TRUE
  ))
  if (!is.na(m_agreement[1, 1])) {
    partner <- stringr::str_squish(m_agreement[1, 2])
    return(.capitalize_summary_head(paste0("Approved Teach-Out Agreement with ", partner)))
  }

  # ----- Pattern 1: Approved Teach-Out Plan with extracted scope -----
  # MSCHE phrasing: "To approve the teach-out plan for <scope>." or
  # "To approve the teach-out plan and agreements with <scope>."
  # Negative case (must NOT match): "teach-out plan and teach-out agreements
  # are not necessary" -- the regex requires the verb "to approve" and the
  # connector "for"/"with", so the negation phrasing falls through.
  m_teachout <- stringr::str_match(
    raw,
    stringr::regex(
      paste0(
        "to\\s+approve\\s+(?:the\\s+)?teach-?out\\s+plan",
        "(?:\\s+and\\s+(?:the\\s+)?(?:teach-?out\\s+)?agreements)?",
        # Optional leading article on the scope: "for THE closure..."
        # consumes the article so the captured scope reads cleanly
        # ("Approved Teach-Out Plan (closure of...)" not "(the closure of...)").
        "\\s+(for|with)\\s+(?:the\\s+)?([^.]+?)\\s*\\."
      ),
      ignore_case = TRUE
    )
  )
  if (!is.na(m_teachout[1, 1])) {
    scope <- stringr::str_squish(m_teachout[1, 3])
    return(.capitalize_summary_head(paste0("Approved Teach-Out Plan (", scope, ")")))
  }

  # ----- Pattern 2: Voluntarily Surrendered Accreditation -----
  # MSCHE phrasing: "voluntarily surrender [its] accreditation".
  # Catches both "intent to ... voluntarily surrender accreditation" and
  # "To accept the institution's request to voluntarily surrender its
  # accreditation".
  if (stringr::str_detect(raw,
        stringr::regex("voluntar(?:ily|y)\\s+surrender", ignore_case = TRUE))) {
    return("Voluntarily Surrendered Accreditation")
  }

  if (
    stringr::str_detect(raw, stringr::regex("heightened cash monitoring", ignore_case = TRUE)) &&
      stringr::str_detect(raw, stringr::regex("teach-?out plan and teach-?out agreements", ignore_case = TRUE)) &&
      stringr::str_detect(raw, stringr::regex("annual independent audit confirming financial viability", ignore_case = TRUE))
  ) {
    return("Required teach-out plan and financial viability monitoring after Heightened Cash Monitoring (HCM2)")
  }

  # ----- Pattern 3: Warning, with Standard reference if present -----
  # MSCHE phrasing: "To warn the institution that its accreditation may
  # be in jeopardy because of insufficient evidence ... in compliance
  # with Standard <Roman>". Standard ref is ALL-CAPS Roman numerals so we
  # match case-sensitively (avoids matching "standard" as a generic noun).
  if (stringr::str_detect(raw,
        stringr::regex("to\\s+warn\\s+the\\s+institution", ignore_case = TRUE))) {
    standard <- stringr::str_match(raw, "Standard\\s+([IVX]+)")[1, 2]
    if (!is.na(standard) && nzchar(standard)) {
      return(paste0("Warning (Standard ", standard, ")"))
    }
    return("Warning")
  }

  # ----- Pattern 4: Removed from Probation -----
  # MSCHE phrasing: "To remove the institution from Probation."
  if (stringr::str_detect(raw,
        stringr::regex(
          "to\\s+remove\\s+the\\s+institution\\s+from\\s+probation",
          ignore_case = TRUE))) {
    return("Removed from Probation")
  }

  # ----- Pattern 5: Continued on Warning, with duration if present -----
  # MSCHE phrasing: "continue the institution on Warning for twelve months"
  # (or "continue ... on Warning for 12 months"). Duration capture handles
  # both word and numeric forms; if neither maps cleanly, drop the
  # parenthetical.
  m_continue <- stringr::str_match(
    raw,
    stringr::regex(
      paste0(
        "continue\\s+the\\s+institution\\s+on\\s+warning",
        "(?:\\s+for\\s+([a-z]+|\\d+)\\s+(month|week|year)s?)?"
      ),
      ignore_case = TRUE
    )
  )
  if (!is.na(m_continue[1, 1])) {
    n_raw <- m_continue[1, 2]
    unit  <- m_continue[1, 3]
    if (!is.na(n_raw) && !is.na(unit) && nzchar(n_raw) && nzchar(unit)) {
      n_num <- suppressWarnings(as.integer(n_raw))
      if (is.na(n_num)) {
        n_num <- unname(.MSCHE_DURATION_WORD_TO_NUM[tolower(n_raw)])
      }
      if (!is.na(n_num) && nzchar(unit)) {
        return(.capitalize_summary_head(paste0("Continued on Warning (", n_num, " ", tolower(unit), "s)")))
      }
    }
    return("Continued on Warning")
  }

  # ----- Pattern 6: Show Cause / Continued Show Cause with deadline -----
  # MSCHE phrasing for show-cause action rows wraps the substantive
  # action ("To require the institution to continue to show cause by
  # February 27, 2026 to demonstrate why its accreditation should not
  # be withdrawn...") in a multi-sentence body that begins with "To
  # acknowledge receipt of the show cause report" and "To note the
  # follow-up team visit ...". Without this pattern the fallback
  # strips the acknowledge-receipt preamble and returns the follow-up-
  # visit sentence, which the JS procedural filter then drops -- which
  # erases the show cause status from the global table even though
  # classify_action has correctly typed the row show_cause.
  m_showcause <- stringr::str_match(raw, stringr::regex(
    "to require the institution to (continue to )?show cause by ([A-Z][a-z]+\\s+\\d{1,2},\\s+\\d{4})",
    ignore_case = TRUE
  ))
  if (!is.na(m_showcause[1, 1])) {
    is_continued <- !is.na(m_showcause[1, 2]) && nzchar(m_showcause[1, 2])
    reason_match <- stringr::str_match(
      raw,
      stringr::regex(
        "because of insufficient evidence that the institution is in compliance with (Standard\\s+[IVXLC]+\\s*\\([^)]+\\))",
        ignore_case = TRUE
      )
    )[, 2]
    if (!is.na(reason_match) && nzchar(reason_match)) {
      reason_match <- .normalize_standard_reason_case(reason_match)
      return(paste0(
        if (is_continued) "Continued Show Cause" else "Required to Show Cause",
        " because of insufficient evidence of compliance with ",
        stringr::str_squish(reason_match)
      ))
    }
    return(if (is_continued) "Continued Show Cause" else "Required to Show Cause")
  }
  # Fallback show cause when no deadline is captured.
  if (stringr::str_detect(raw, stringr::regex(
        "to require the institution to (?:continue to )?show cause",
        ignore_case = TRUE))) {
    is_continued <- stringr::str_detect(raw, stringr::regex(
      "continue to show cause", ignore_case = TRUE))
    reason_match <- stringr::str_match(
      raw,
      stringr::regex(
        "because of insufficient evidence that the institution is in compliance with (Standard\\s+[IVXLC]+\\s*\\([^)]+\\))",
        ignore_case = TRUE
      )
    )[, 2]
    if (!is.na(reason_match) && nzchar(reason_match)) {
      reason_match <- .normalize_standard_reason_case(reason_match)
      return(.capitalize_summary_head(paste0(
        if (is_continued) "Continued Show Cause" else "Required to Show Cause",
        " because of insufficient evidence of compliance with ",
        stringr::str_squish(reason_match)
      )))
    }
    return(.capitalize_summary_head(if (is_continued) "Continued Show Cause" else "Required to Show Cause"))
  }

  # ----- Fallback -----
  # Three preamble strips, then return the first remaining sentence.
  # Final safety net is the original text trimmed and length-capped so
  # the table never gets a 500-char monster from an unanticipated phrasing.
  #
  # Phase 4 hotfix: strip "Staff acted on behalf of the Commission "
  # UNCONDITIONALLY (any verb, not just acknowledge-receipt). The phrase
  # is bureaucratic boilerplate; what follows is the substantive action
  # regardless of verb. Without this, rows like St. Francis's "Staff
  # acted on behalf of the Commission to request a supplemental
  # information report..." retained the preamble in action_label_short
  # and the JS isTrackedAction procedural filter (which anchors at
  # "^to request") could not match.
  stripped <- stringr::str_remove(
    raw,
    stringr::regex("^staff acted on behalf of the commission ", ignore_case = TRUE)
  )
  stripped <- stringr::str_remove(
    stripped,
    stringr::regex("^to acknowledge receipt of [^.]*\\.\\s*", ignore_case = TRUE)
  )
  # Capitalize the first letter so post-strip output reads like a
  # standalone sentence (the unconditional staff-acted strip leaves a
  # lowercase "to" at the start when the original phrase ended with
  # "Commission to <verb>...").
  stripped <- stringr::str_replace(
    stripped, "^([a-z])", function(m) toupper(m)
  )
  first <- stringr::str_match(stripped, "^([^.]+\\.)")[1, 2]
  if (!is.na(first)) {
    first <- stringr::str_squish(first)
    if (nzchar(first)) return(.capitalize_summary_head(first))
  }
  .capitalize_summary_head(trimws(stringr::str_sub(raw, 1L, 200L)))
}
