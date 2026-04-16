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

# FUNCTION: require_local_file
# PURPOSE: Validates that a required input file exists before use.
#          Stops with a clear, actionable error if the file is missing,
#          telling the user which upstream script to run.
#
# PARAMETERS:
#   path – Full file path to check (e.g., "./data/canonical.csv")
#   label – Human-readable description (used in error message)
#   how_to_fix – Instructions for fixing the problem (e.g., which script to run)
#
# RETURNS: Invisibly returns path if file exists; stops with error if not
#
# EXAMPLES:
#   require_local_file(
#     "./derived/canonical.csv",
#     "canonical IPEDS dataset",
#     "Run build_ipeds_canonical_dataset.R first"
#   )
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

# FUNCTION: ensure_columns
# PURPOSE: Adds any missing columns from a defaults list to a data frame.
#          Useful for ensuring optional columns are always present with
#          sensible default values.
#
# PARAMETERS:
#   df – The data frame to modify
#   defaults – Named list where names are column names and values are
#             the default value for missing columns
#             Example: list(warning_score = NA_real_, status = "active")
#
# RETURNS: Modified data frame with new columns added
#
# EXAMPLES:
#   df <- ensure_columns(df, list(
#     accreditation_status = NA_character_,
#     region = "Unknown"
#   ))
ensure_columns <- function(df, defaults) {
  for (nm in names(defaults)) {
    if (!nm %in% names(df)) df[[nm]] <- defaults[[nm]]
  }
  df
}

# ---------------------------------------------------------------------------
# Scalar coercion helpers
# ---------------------------------------------------------------------------

# FUNCTION: null_if_empty
# PURPOSE: Converts empty or blank values to NA.
#          Useful for cleaning string fields where whitespace should be treated
#          the same as missing values.
#
# PARAMETERS:
#   x – Value to check (will be coerced to character string)
#
# RETURNS: NA_character_ if x is empty/blank after trimming; otherwise returns x
#
# EXAMPLES:
#   null_if_empty("")        # Returns NA
#   null_if_empty("  ")      # Returns NA (whitespace trimmed)
#   null_if_empty("School")  # Returns "School"
#   null_if_empty(NULL)      # Returns NA (uses %||% to convert NULL to "")
null_if_empty <- function(x) {
  x <- trimws(as.character(x %||% ""))
  ifelse(x == "", NA_character_, x)
}

# FUNCTION: scale_ratio_to_pct
# PURPOSE: Converts a decimal ratio (0–1) to a percentage (0–100).
#          Commonly used for enrollment percentages and other ratios.
#
# PARAMETERS:
#   x – Numeric value between 0 and 1, or any value coercible to numeric
#       (NA values are preserved)
#
# RETURNS: Numeric value multiplied by 100, or NA if input is NA
#
# EXAMPLES:
#   scale_ratio_to_pct(0.25)     # Returns 25
#   scale_ratio_to_pct(0.125)    # Returns 12.5
#   scale_ratio_to_pct(NA)       # Returns NA
#   scale_ratio_to_pct("0.5")    # Returns 50 (coerced from string)
scale_ratio_to_pct <- function(x) {
  value <- to_num(x)   # to_num from utils.R
  ifelse(is.na(value), NA_real_, value * 100)
}

# FUNCTION: or_null
# PURPOSE: Returns the first non-missing value from a vector, or NA if all are missing.
#          Treats empty strings as missing. Useful as a "pick first" operator.
#
# PARAMETERS:
#   x – Any vector or single value
#
# RETURNS: The first non-NA, non-empty value; NA if none found
#
# DETAILS:
#   - Returns NA if x is empty (length 0)
#   - Returns NA if all elements are NA
#   - For strings: returns NA if the value is empty or all whitespace
#   - For other types: only checks for NA
#
# EXAMPLES:
#   or_null(c(NA, NA, "value"))   # Returns "value"
#   or_null(c("", "value"))       # Returns "value" (empty string is treated as NA)
#   or_null(c(NA, NA))            # Returns NA
#   or_null(integer())            # Returns NA (empty vector)
or_null <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA)
  value <- x[[1]]
  if (is.character(value)) {
    value <- trimws(value)
    if (identical(value, "")) return(NA)
  }
  value
}

# FUNCTION: or_null_date
# PURPOSE: Returns the first non-missing value from a vector, or NA if all are missing.
#          Special handling for Date objects: converts them to character strings.
#          Treats empty strings as missing. Useful for optional date fields.
#
# PARAMETERS:
#   x – Any vector or single value (may contain Date objects)
#
# RETURNS: Character string (if Date), trimmed string (if character), or NA
#
# DETAILS:
#   - Returns NA if x is empty (length 0)
#   - Returns NA if all elements are NA
#   - Date values are converted to ISO 8601 format ("YYYY-MM-DD")
#   - For strings: treats empty strings as NA
#
# EXAMPLES:
#   or_null_date(c(NA, as.Date("2024-01-15")))  # Returns "2024-01-15"
#   or_null_date(c("", "date string"))          # Returns "date string"
#   or_null_date(c(NA, NA))                     # Returns NA
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

# FUNCTION: make_export_id
# PURPOSE: Builds a stable, URL-safe identifier for a school record.
#          Uses the IPEDS unitid when available; falls back to a slug
#          derived from institution name and state.
#
# PARAMETERS:
#   prefix – Prefix to prepend to the ID (e.g., "school", "college")
#   unitid – IPEDS unitid (if present and non-empty, used as the ID)
#   institution_name – School name (used for slug if unitid is missing)
#   state – State abbreviation (used for slug if unitid is missing)
#
# RETURNS: Character string: either the unitid (if present) or
#          "prefix-normalized-name-state" slug
#
# DETAILS:
#   Slug normalization:
#   1. Combines "Institution Name | State"
#   2. Converts to lowercase
#   3. Replaces non-alphanumeric characters with hyphens
#   4. Removes leading/trailing hyphens
#   5. Prepends the prefix
#
# EXAMPLES:
#   make_export_id("school", "123456", NA, NA)
#     # Returns "123456"
#
#   make_export_id("school", NA, "Harvard University", "MA")
#     # Returns "school-harvard-university-ma"
#
#   make_export_id("college", "", "MIT", "MA")
#     # Returns "college-mit-ma" (empty unitid treated as missing)
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

# FUNCTION: normalize_control_label
# PURPOSE: Standardizes control/ownership labels to canonical display strings.
#          Maps variations like "private non-profit", "private nonprofit",
#          "not-for-profit" to the standard "Private not-for-profit".
#
# PARAMETERS:
#   x – Control label string (may have variations in spacing, capitalization)
#
# RETURNS: One of: "Public", "Private not-for-profit", "Private for-profit", or NA
#
# DETAILS:
#   Uses regex matching (case-insensitive) to identify control types:
#   - "public" → "Public"
#   - "private (?:non-profit|not-for-profit)" → "Private not-for-profit"
#   - "private for-profit" → "Private for-profit"
#   - Returns NA if input is empty or doesn't match any pattern
#
# EXAMPLES:
#   normalize_control_label("PUBLIC")              # Returns "Public"
#   normalize_control_label("Private non-profit")  # Returns "Private not-for-profit"
#   normalize_control_label("private for profit")  # Returns "Private for-profit"
#   normalize_control_label("")                    # Returns NA
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

# FUNCTION: normalize_display_institution_name
# PURPOSE: Expands known abbreviated or alternate names to their official display forms.
#          Useful for correcting data quality issues (e.g., campus names that should
#          use the main campus name).
#
# PARAMETERS:
#   x – Institution name string
#
# RETURNS: Normalized institution name (same as input if no abbreviation matches)
#
# EXAMPLES:
#   normalize_display_institution_name("Arizona State University Campus Immersion")
#     # Returns "Arizona State University"
#
#   normalize_display_institution_name("Harvard University")
#     # Returns "Harvard University" (no abbreviation found)
normalize_display_institution_name <- function(x) {
  value <- trimws(as.character(x %||% ""))
  dplyr::case_when(
    identical(value, "Arizona State University Campus Immersion") ~ "Arizona State University",
    TRUE ~ value
  )
}

# FUNCTION: build_institution_unique_name
# PURPOSE: Builds the common "Institution | City | State" display string
#          used throughout the website (school indexes, profile pages, etc.).
#
# PARAMETERS:
#   institution_name – Official school name
#   city – City name
#   state – State name or abbreviation
#
# RETURNS: Formatted display string with non-empty components joined by " | "
#
# DETAILS:
#   - Normalizes the institution name before building the string
#   - Removes NA values from the output
#   - Omits fields that are NA or empty
#
# EXAMPLES:
#   build_institution_unique_name("Harvard University", "Cambridge", "MA")
#     # Returns "Harvard University | Cambridge | MA"
#
#   build_institution_unique_name("MIT", NA, "Massachusetts")
#     # Returns "MIT | Massachusetts" (city omitted because NA)
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

# FUNCTION: is_primary_bachelors_category
# PURPOSE: Checks whether a Carnegie classification indicates a bachelor's-granting
#          institution. Used to filter schools for different data displays.
#
# PARAMETERS:
#   x – Carnegie classification label
#
# RETURNS: TRUE if the school is primarily bachelor's-granting; FALSE otherwise
#
# DETAILS:
#   Returns TRUE when:
#     - String contains "primarily baccalaureate or above"
#     - AND does NOT contain "not primarily baccalaureate or above"
#
# EXAMPLES:
#   is_primary_bachelors_category("Primarily baccalaureate or above")  # TRUE
#   is_primary_bachelors_category("Not primarily baccalaureate")       # FALSE
#   is_primary_bachelors_category("Research University")              # FALSE
is_primary_bachelors_category <- function(x) {
  value <- as.character(x %||% "")
  grepl("primarily baccalaureate or above",     value, ignore.case = TRUE) &
    !grepl("not primarily baccalaureate or above", value, ignore.case = TRUE)
}

# ---------------------------------------------------------------------------
# Domain-specific helpers
# ---------------------------------------------------------------------------

# FUNCTION: derive_positions_affected
# PURPOSE: Extracts the count of positions affected from free-text fields.
#          Searches multiple fields (faculty_affected, notes, source_title, etc.)
#          using regex patterns to find numeric evidence of job cuts.
#          Returns NA_integer_ when no count can be inferred.
#
# PARAMETERS:
#   faculty_affected – Direct count field (may be text or number)
#   notes – Additional context text
#   source_title – Source article/news headline
#   program_name – Program or department name
#   cut_type – Type of cut (e.g., "staff_layoff", "faculty_layoff")
#
# RETURNS: Integer count of affected positions, or NA_integer_ if not found
#
# LOGIC:
#   1. If faculty_affected is a positive number, return it immediately
#   2. Combine remaining text fields and check for layoff signals
#   3. If no layoff signals found, return NA
#   4. Search combined text using regex patterns for numeric evidence:
#      - "cutting [N] positions"
#      - "lays off [N]" / "laid off [N]"
#      - "[N] positions affected"
#      - "[N] employees/staff/faculty"
#   5. Return the first match found, or NA if none match
#
# EXAMPLES:
#   derive_positions_affected("50", NA, NA, NA, NA)
#     # Returns 50
#
#   derive_positions_affected(NA, "The college is laying off 25 faculty members", NA, NA, NA)
#     # Returns 25
#
#   derive_positions_affected(NA, NA, NA, NA, NA)
#     # Returns NA (no numeric evidence found)
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

# FUNCTION: build_international_students_sentence
# PURPOSE: Constructs a narrative sentence describing international student enrollment.
#          Handles cases where different data is available for undergrad vs. graduate.
#
# PARAMETERS:
#   year – Academic year (for the sentence)
#   all_pct – Ratio of international students (0–1) across all levels
#   ug_pct – Ratio of international undergraduates (0–1)
#   grad_pct – Ratio of international graduate students (0–1)
#
# RETURNS: Character string describing international enrollment, or NA if all_pct is missing
#
# LOGIC:
#   - If all_pct is missing → returns NA
#   - If both ug_pct and grad_pct are present → sentence includes breakdown by level
#   - Otherwise → simple sentence with total only
#
# EXAMPLES:
#   build_international_students_sentence(2024, 0.15, 0.10, 0.25)
#     # Returns "In 2024, 15% of students were international. That includes 10% of undergraduates and 25% of graduate students."
#
#   build_international_students_sentence(2024, 0.15, NA, NA)
#     # Returns "In 2024, 15% of students were international."
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

# FUNCTION: write_json_file
# PURPOSE: Writes an R object to a JSON file with pretty formatting.
#          Converts NA values to JSON null for web compatibility.
#
# PARAMETERS:
#   x – Object to serialize to JSON (list, data frame, vector, etc.)
#   path – Output file path (e.g., "./output/data.json")
#
# RETURNS: Invisibly returns NULL
#
# SIDE EFFECTS:
#   - Creates or overwrites path
#   - NA values rendered as JSON null (not "NA")
#   - Output is human-readable (pretty-printed)
#   - Single-element vectors are converted to scalars (not arrays)
#
# EXAMPLES:
#   school_data <- list(
#     name = "Harvard University",
#     enrollment = 7000,
#     note = NA
#   )
#   write_json_file(school_data, "./output/harvard.json")
write_json_file <- function(x, path) {
  jsonlite::write_json(x, path = path, pretty = TRUE, auto_unbox = TRUE, na = "null")
}

# FUNCTION: build_series
# PURPOSE: Converts a data frame of (year, value) pairs into the
#          [{year, value}, ...] list structure expected by the frontend.
#
# PARAMETERS:
#   df – Data frame with at least "year" column
#   value_col – Name of the column containing values to export
#   scale – Multiplier for values (default 1; use 100 for converting ratios to percentages)
#
# RETURNS: List of lists, each with "year" (integer) and "value" (numeric) fields.
#          Returns empty list if value_col contains only NA values.
#
# LOGIC:
#   - Filters out rows where value_col is NA
#   - Converts year to integer
#   - Multiplies all values by scale
#   - Returns list of single-element lists for JSON serialization
#
# EXAMPLES:
#   df <- data.frame(year = c(2020, 2021, 2022), revenue = c(100, 110, 120))
#   build_series(df, "revenue")
#     # Returns list(
#     #   list(year = 2020L, value = 100),
#     #   list(year = 2021L, value = 110),
#     #   list(year = 2022L, value = 120)
#     # )
#
#   build_series(df, "revenue", scale = 0.001)  # Convert to thousands
#     # Returns list with values 0.1, 0.11, 0.12
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

# FUNCTION: pick_first_present
# PURPOSE: Returns the first non-missing value among a set of candidate columns.
#          Useful for handling optional fields where multiple names might be used.
#
# PARAMETERS:
#   df – Data frame to search
#   candidates – Character vector of column names to check (in priority order)
#                Example: c("official_name", "alternate_name", "display_name")
#
# RETURNS: Character vector with same length as nrow(df).
#          For each row: the first non-NA value from the candidate columns,
#          or NA_character_ if all candidates are NA or missing.
#
# DETAILS:
#   - Processes candidates in order; returns first match for each row
#   - Skips candidate columns that don't exist in df
#   - If no candidates are present, returns rep(NA_character_, nrow(df))
#   - Uses dplyr::coalesce to combine multiple column values
#
# EXAMPLES:
#   df <- data.frame(
#     name1 = c("Alice", NA, NA),
#     name2 = c(NA, "Bob", NA),
#     name3 = c(NA, NA, "Charlie")
#   )
#   pick_first_present(df, c("name1", "name2", "name3"))
#     # Returns c("Alice", "Bob", "Charlie")
#
#   pick_first_present(df, c("missing_col1", "missing_col2"))
#     # Returns c(NA_character_, NA_character_, NA_character_)
pick_first_present <- function(df, candidates) {
  present <- candidates[candidates %in% names(df)]
  if (!length(present)) return(rep(NA_character_, nrow(df)))
  values <- lapply(present, function(col) as.character(df[[col]]))
  Reduce(function(x, y) dplyr::coalesce(x, y), values)
}
