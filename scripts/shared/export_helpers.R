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

# Builds a stable, URL-safe identifier for a school. Uses IPEDS unitid if
# available, otherwise builds a slug from institution name and state.
make_export_id <- function(prefix, unitid, institution_name, state) {
  raw_unitid <- trimws(as.character(unitid %||% ""))
  if (!identical(raw_unitid, "")) return(raw_unitid)

  # Build slug from name and state
  base       <- paste(institution_name %||% "", state %||% "", sep = " | ")
  normalized <- tolower(base)
  # Replace all non-alphanumeric characters with hyphens
  normalized <- gsub("[^a-z0-9]+", "-", normalized)
  # Remove leading/trailing hyphens
  normalized <- gsub("^-+|-+$",    "",  normalized)
  paste0(prefix, "-", normalized)
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
write_json_file <- function(x, path) {
  jsonlite::write_json(x, path = path, pretty = TRUE, auto_unbox = TRUE, na = "null")
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
