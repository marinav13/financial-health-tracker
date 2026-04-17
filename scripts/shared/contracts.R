# scripts/shared/contracts.R
#
# Data contracts for the IPEDS pipeline. Each major script boundary has a
# dedicated validator that stops with a clear, actionable error when the
# incoming or outgoing data frame fails basic shape/type expectations.

# ---------------------------------------------------------------------------
# Core validators
# ---------------------------------------------------------------------------

# Checks that a data frame contains all required columns; stops with an error listing any missing ones.
assert_columns <- function(df, required, label = "data frame") {
  missing <- setdiff(required, names(df))
  if (length(missing) == 0L) return(invisible(df))
  stop(
    sprintf(
      "Contract violation in %s: %d required column(s) missing:\n  %s",
      label,
      length(missing),
      paste(missing, collapse = "\n  ")
    ),
    call. = FALSE
  )
}

# Checks that key column combinations are unique; reports the first few duplicates if any exist.
assert_no_duplicate_keys <- function(df, key_cols, label = "data frame") {
  if (!all(key_cols %in% names(df))) return(invisible(df))

  # Create composite key strings: "unitid·year" for each row
  counts <- table(do.call(paste, c(df[key_cols], sep = "\u00b7")))
  dupes  <- names(counts[counts > 1L])

  if (length(dupes) == 0L) return(invisible(df))

  # Show first 5 duplicate keys, then indicate if there are more
  preview <- paste(head(dupes, 5L), collapse = "\n  ")
  stop(
    sprintf(
      "Contract violation in %s: duplicate (%s) key(s) found (%d total):\n  %s%s",
      label,
      paste(key_cols, collapse = ", "),
      length(dupes),
      preview,
      if (length(dupes) > 5L) sprintf("\n  ... and %d more", length(dupes) - 5L) else ""
    ),
    call. = FALSE
  )
}

# Checks that specified columns match expected R types (e.g., is.character, is.numeric); skips columns not present in the data frame.
assert_column_types <- function(df, type_specs, label = "data frame") {
  bad <- character()
  for (col in names(type_specs)) {
    if (!col %in% names(df)) next
    test_fn <- type_specs[[col]]
    if (!test_fn(df[[col]])) {
      # Build error message showing the actual class(es) found
      bad <- c(bad, sprintf("%s (got %s)", col, paste(class(df[[col]]), collapse = "/")))
    }
  }
  if (length(bad) == 0L) return(invisible(df))
  stop(
    sprintf(
      "Contract violation in %s: %d column(s) have unexpected types:\n  %s",
      label,
      length(bad),
      paste(bad, collapse = "\n  ")
    ),
    call. = FALSE
  )
}

# ---------------------------------------------------------------------------
# Column-spec constants
# ---------------------------------------------------------------------------

# --- canonical dataset output (build_ipeds_canonical_dataset.R) -----------

# Columns that must be present in the canonical IPEDS dataset (prevents silent data corruption).
CANONICAL_REQUIRED_COLS <- c(
  # Identity columns: basic school info
  "unitid", "institution_name", "institution_unique_name",
  "year", "control_label", "state", "city",
  # Enrollment
  "enrollment_headcount_total",
  # Financial metrics
  "revenue_total", "expenses_total", "loss_amount",
  "net_tuition_total", "tuition_dependence_pct",
  # Risk flags: identify financially troubled schools
  "ended_2024_at_loss", "losses_last_3_of_5"
)

# Columns that uniquely identify each row in the canonical dataset.
CANONICAL_KEY_COLS <- c("unitid", "year")

# Type requirements for critical canonical columns (unitid = text, year = numeric).
CANONICAL_TYPE_SPECS <- list(
  unitid = is.character,
  year   = is.numeric
)

# --- enriched dataset inputs (shared by workbook and export scripts) -------

# Minimal columns required by any script that reads the enriched dataset.
ENRICHED_REQUIRED_COLS <- c(
  "unitid", "institution_name", "year", "control_label", "state"
)

# Columns that uniquely identify rows in enriched datasets.
ENRICHED_KEY_COLS <- c("unitid", "year")

# Type requirements for enriched dataset (only unitid strictly validated).
ENRICHED_TYPE_SPECS <- list(
  unitid = is.character
)

# --- article workbook input (build_article_workbook.R) --------------------

# All columns required to build the article workbook (enriched base plus financial metrics).
WORKBOOK_REQUIRED_COLS <- c(
  ENRICHED_REQUIRED_COLS,
  "enrollment_headcount_total",
  "revenue_total", "expenses_total",
  "tuition_dependence_pct", "ended_2024_at_loss"
)

# Unique identifier for workbook rows (school-year pairs).
WORKBOOK_KEY_COLS <- c("unitid", "year")

# --- web export input (build_web_exports.R) --------------------------------

# All columns required to build JSON exports for the website.
EXPORT_REQUIRED_COLS <- c(
  ENRICHED_REQUIRED_COLS,
  "institution_unique_name",
  "enrollment_headcount_total",
  "revenue_total", "expenses_total",
  "tuition_dependence_pct"
)

# Unique identifier for export rows (school-year pairs).
EXPORT_KEY_COLS <- c("unitid", "year")

# ---------------------------------------------------------------------------
# Convenience validators
# ---------------------------------------------------------------------------

# Validates canonical IPEDS output before writing to CSV: checks required columns, no duplicates, and correct types.
validate_canonical_output <- function(df) {
  assert_columns(df, CANONICAL_REQUIRED_COLS, "canonical IPEDS output")
  assert_no_duplicate_keys(df, CANONICAL_KEY_COLS, "canonical IPEDS output")
  assert_column_types(df, CANONICAL_TYPE_SPECS, "canonical IPEDS output")
  invisible(df)
}

# Generic validator for any script that reads the enriched dataset: checks required columns and types.
validate_enriched_input <- function(df, label = "enriched input") {
  assert_columns(df, ENRICHED_REQUIRED_COLS, label)
  assert_column_types(df, ENRICHED_TYPE_SPECS, label)
  invisible(df)
}

# Validates the workbook dataset after numeric conversion, checking all required columns are present.
validate_workbook_input <- function(df) {
  assert_columns(df, WORKBOOK_REQUIRED_COLS, "article workbook input")
  invisible(df)
}

# Validates the export dataset after numeric conversion: checks required columns and no duplicate keys.
validate_export_input <- function(df) {
  assert_columns(df, EXPORT_REQUIRED_COLS, "web export input")
  assert_no_duplicate_keys(df, EXPORT_KEY_COLS, "web export input")
  invisible(df)
}
