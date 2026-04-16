# scripts/shared/contracts.R
#
# Data contracts for the IPEDS pipeline. Each major script boundary has a
# dedicated validator that stops with a clear, actionable error when the
# incoming or outgoing data frame fails basic shape/type expectations.
#
# STRUCTURE:
#   This file defines:
#   - Core validators: reusable functions for checking columns, keys, and types
#   - Column-spec constants: required columns and type specs for each script boundary
#   - Convenience validators: high-level validators called from actual scripts
#
# Core validators (reusable):
#   assert_columns            – required columns present
#   assert_no_duplicate_keys  – no duplicate (unitid, year) rows
#   assert_column_types       – named type assertions on specific columns
#
# Column-spec constants (one per script boundary):
#   CANONICAL_REQUIRED_COLS / CANONICAL_KEY_COLS / CANONICAL_TYPE_SPECS
#   ENRICHED_REQUIRED_COLS  / ENRICHED_KEY_COLS  / ENRICHED_TYPE_SPECS
#   WORKBOOK_REQUIRED_COLS  / WORKBOOK_KEY_COLS
#   EXPORT_REQUIRED_COLS    / EXPORT_KEY_COLS
#
# Convenience validators (call these from scripts):
#   validate_canonical_output(df)       – after build_ipeds_canonical_dataset.R
#   validate_enriched_input(df, label)  – generic enriched-dataset ingestion
#   validate_workbook_input(df)         – after numeric conversion in build_article_workbook.R
#   validate_export_input(df)           – after numeric conversion in build_web_exports.R

# ---------------------------------------------------------------------------
# Core validators
# ---------------------------------------------------------------------------

# FUNCTION: assert_columns
# PURPOSE: Validates that a data frame contains all required columns.
#          Stops with a readable error listing every missing column.
#
# PARAMETERS:
#   df – The data frame to check
#   required – Character vector of required column names
#   label – Human-readable description of the data frame (used in error messages)
#
# RETURNS: Invisibly returns df if check passes; stops with error if not
#
# EXAMPLES:
#   assert_columns(my_df, c("unitid", "year"), "input dataset")
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

# FUNCTION: assert_no_duplicate_keys
# PURPOSE: Validates that combinations of key columns are unique.
#          Stops with a clear error when duplicate (unitid, year) pairs are found.
#          Reports the first few offending key values to help diagnose the source.
#
# PARAMETERS:
#   df – The data frame to check
#   key_cols – Character vector of column names that form the key
#             (e.g., c("unitid", "year"))
#   label – Human-readable description of the data frame (used in error messages)
#
# RETURNS: Invisibly returns df if check passes; stops with error if not
#
# DETAILS:
#   Uses paste() with a special separator (·) to create composite key strings,
#   then counts occurrences of each unique key combination.
#
# EXAMPLES:
#   assert_no_duplicate_keys(my_df, c("unitid", "year"), "input dataset")
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

# FUNCTION: assert_column_types
# PURPOSE: Validates that specified columns have the expected R types.
#          Stops with a clear error when type expectations are violated.
#
# PARAMETERS:
#   df – The data frame to check
#   type_specs – Named list of type-test functions
#               Example: list(unitid = is.character, year = is.numeric)
#               Keys are column names; values are predicate functions
#   label – Human-readable description of the data frame (used in error messages)
#
# RETURNS: Invisibly returns df if check passes; stops with error if not
#
# DETAILS:
#   Each value in type_specs should be a function that returns TRUE if the
#   column satisfies the type requirement (e.g., is.character, is.numeric).
#   Missing columns in type_specs are skipped (not an error).
#
# EXAMPLES:
#   specs <- list(unitid = is.character, year = is.numeric)
#   assert_column_types(my_df, specs, "canonical output")
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

# CANONICAL_REQUIRED_COLS
# PURPOSE: Lists columns that must be present in the canonical IPEDS dataset.
#          These are a curated subset of ~80 total columns produced by
#          build_ipeds_canonical_dataset.R
#
# RATIONALE: If any of these are absent, downstream website and workbook
#            exports will silently produce wrong results. We hard-stop to
#            prevent silent data corruption.
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

# CANONICAL_KEY_COLS
# PURPOSE: Columns that uniquely identify each row in the canonical dataset.
#          Used to detect duplicate school-year combinations.
CANONICAL_KEY_COLS <- c("unitid", "year")

# CANONICAL_TYPE_SPECS
# PURPOSE: Type requirements for critical canonical columns.
#          Ensures unitid is text (not numeric) and year is numeric.
CANONICAL_TYPE_SPECS <- list(
  unitid = is.character,
  year   = is.numeric
)

# --- enriched dataset inputs (shared by workbook and export scripts) -------

# ENRICHED_REQUIRED_COLS
# PURPOSE: Minimal columns required by any script ingesting the enriched dataset.
#          These are available from both the workbook and export processes.
ENRICHED_REQUIRED_COLS <- c(
  "unitid", "institution_name", "year", "control_label", "state"
)

# ENRICHED_KEY_COLS
# PURPOSE: Columns that uniquely identify rows in enriched datasets.
ENRICHED_KEY_COLS <- c("unitid", "year")

# ENRICHED_TYPE_SPECS
# PURPOSE: Type requirements for enriched dataset columns.
#          Only unitid is strictly validated (must be character).
ENRICHED_TYPE_SPECS <- list(
  unitid = is.character
)

# --- article workbook input (build_article_workbook.R) --------------------

# WORKBOOK_REQUIRED_COLS
# PURPOSE: All columns required to build the article workbook.
#          Includes enriched base columns plus financial metrics.
WORKBOOK_REQUIRED_COLS <- c(
  ENRICHED_REQUIRED_COLS,
  "enrollment_headcount_total",
  "revenue_total", "expenses_total",
  "tuition_dependence_pct", "ended_2024_at_loss"
)

# WORKBOOK_KEY_COLS
# PURPOSE: Unique identifier for workbook rows (school-year pairs).
WORKBOOK_KEY_COLS <- c("unitid", "year")

# --- web export input (build_web_exports.R) --------------------------------

# EXPORT_REQUIRED_COLS
# PURPOSE: All columns required to build JSON exports for the website.
#          Includes enriched base columns plus financial and identity data.
EXPORT_REQUIRED_COLS <- c(
  ENRICHED_REQUIRED_COLS,
  "institution_unique_name",
  "enrollment_headcount_total",
  "revenue_total", "expenses_total",
  "tuition_dependence_pct"
)

# EXPORT_KEY_COLS
# PURPOSE: Unique identifier for export rows (school-year pairs).
EXPORT_KEY_COLS <- c("unitid", "year")

# ---------------------------------------------------------------------------
# Convenience validators
# ---------------------------------------------------------------------------

# FUNCTION: validate_canonical_output
# PURPOSE: High-level validator called at the end of build_ipeds_canonical_dataset.R
#          Checks that the canonical IPEDS output is complete and well-formed
#          before writing to CSV.
#
# PARAMETERS:
#   df – The canonical IPEDS dataset to validate
#
# RETURNS: Invisibly returns df if all checks pass; stops with error if not
#
# CHECKS:
#   - All required canonical columns present
#   - No duplicate (unitid, year) pairs
#   - unitid is character, year is numeric
#
# EXAMPLES:
#   canonical_df <- build_canonical_dataset()
#   validate_canonical_output(canonical_df)
#   write_csv_atomic(canonical_df, path)
validate_canonical_output <- function(df) {
  assert_columns(df, CANONICAL_REQUIRED_COLS, "canonical IPEDS output")
  assert_no_duplicate_keys(df, CANONICAL_KEY_COLS, "canonical IPEDS output")
  assert_column_types(df, CANONICAL_TYPE_SPECS, "canonical IPEDS output")
  invisible(df)
}

# FUNCTION: validate_enriched_input
# PURPOSE: Generic validator for any script that reads the enriched dataset.
#          Ensures the enriched dataset has minimum required columns and types.
#
# PARAMETERS:
#   df – The enriched dataset to validate
#   label – Script name or description (e.g., "build_article_workbook")
#           Used in error messages to help trace the violation source
#
# RETURNS: Invisibly returns df if all checks pass; stops with error if not
#
# CHECKS:
#   - All enriched required columns present
#   - unitid is character
#
# EXAMPLES:
#   enriched_df <- read_csv("enriched_dataset.csv")
#   validate_enriched_input(enriched_df, "workbook builder")
validate_enriched_input <- function(df, label = "enriched input") {
  assert_columns(df, ENRICHED_REQUIRED_COLS, label)
  assert_column_types(df, ENRICHED_TYPE_SPECS, label)
  invisible(df)
}

# FUNCTION: validate_workbook_input
# PURPOSE: Validates the workbook dataset after numeric conversion.
#          Called in build_article_workbook.R just before computing warning scores.
#
# PARAMETERS:
#   df – The workbook dataset to validate (after numeric column conversions)
#
# RETURNS: Invisibly returns df if all checks pass; stops with error if not
#
# CHECKS:
#   - All required workbook columns present
#
# EXAMPLES:
#   workbook_df <- convert_to_numeric(enriched_df)
#   validate_workbook_input(workbook_df)
#   scores <- compute_warning_scores(workbook_df)
validate_workbook_input <- function(df) {
  assert_columns(df, WORKBOOK_REQUIRED_COLS, "article workbook input")
  invisible(df)
}

# FUNCTION: validate_export_input
# PURPOSE: Validates the export dataset after numeric conversion.
#          Called in build_web_exports.R just before building JSON school profiles.
#
# PARAMETERS:
#   df – The export dataset to validate (after numeric column conversions)
#
# RETURNS: Invisibly returns df if all checks pass; stops with error if not
#
# CHECKS:
#   - All required export columns present
#   - No duplicate (unitid, year) pairs
#
# EXAMPLES:
#   export_df <- convert_to_numeric(enriched_df)
#   validate_export_input(export_df)
#   for (school in unique(export_df$unitid)) { build_school_json(school) }
validate_export_input <- function(df) {
  assert_columns(df, EXPORT_REQUIRED_COLS, "web export input")
  assert_no_duplicate_keys(df, EXPORT_KEY_COLS, "web export input")
  invisible(df)
}
