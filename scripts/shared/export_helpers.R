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
# variations — "St." vs "Saint", leading "The", "&" vs "and" — all produce
# the same slug and do not silently change a school's public URL.
#
# IMPORTANT: do NOT change the output format of this helper once schools are
# published.  Any change breaks existing bookmarked URLs for unmatched schools.
slug_institution_name <- function(name) {
  s <- tolower(trimws(name %||% ""))
  s <- sub("^the +", "", s)                      # strip leading "The "
  s <- gsub("\\bst\\.?\\b", "saint", s)          # St / St. → saint
  s <- gsub("&", "and", s, fixed = TRUE)         # & → and
  s <- gsub("[^a-z0-9]+", "-", s)                # non-alnum → hyphen
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
# When a school transitions from unmatched → matched (gains a unitid), its ID
# will change from a slug to the numeric unitid.  This is unavoidable without
# a separate persistent ID store, but it only affects unmatched schools and
# is logged at export time (see build_web_exports.R).
make_export_id <- function(prefix, unitid, institution_name, state) {
  raw_unitid <- trimws(as.character(unitid %||% ""))
  if (!identical(raw_unitid, "")) return(raw_unitid)

  # No unitid — build slug from normalised name + state.
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
        "write_json_file: %d null bytes (%.1f%%) found in %s — possible file corruption",
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
  lapply(seq_len(nrow(rows)), function(i) {
    list(
      year  = as.integer(rows$year[[i]]),
      value = unname(rows[[value_col]][[i]]) * scale
    )
  })
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

# Runs a set of export bundle specs, each with a builder plus filenames, and
# returns a named list of written paths keyed by the spec name.
write_export_bundles <- function(specs, data_dir) {
  results <- vector("list", length(specs))
  names(results) <- names(specs)

  for (nm in names(specs))