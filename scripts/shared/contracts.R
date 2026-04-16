# scripts/shared/contracts.R
#
# Data contracts for the IPEDS pipeline.  Each major script boundary has a
# dedicated validator that stops with a clear, actionable error when the
# incoming or outgoing data frame fails basic shape/type expectations.
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

# Stops with a readable error listing every missing column when `df` does not
# contain all of `required`.  `label` names the data frame in the error text.
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

# Stops when any combination of `key_cols` appears more than once in `df`.
# Reports the first few offending key values to help diagnose the source.
assert_no_duplicate_keys <- function(df, key_cols, label = "data frame") {
  if (!all(key_cols %in% names(df))) return(invisible(df))
  counts <- table(do.call(paste, c(df[key_cols], sep = "\u00b7")))
  dupes  <- names(counts[counts > 1L])
  if (length(dupes) == 0L) return(invisible(df))
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

# Stops when a column does not satisfy its declared type test.
# `type_specs` is a named list: list(unitid = is.character, year = is.numeric).
assert_column_types <- function(df, type_specs, label = "data frame") {
  bad <- character()
  for (col in names(type_specs)) {
    if (!col %in% names(df)) next
    test_fn <- type_specs[[col]]
    if (!test_fn(df[[col]])) {
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

# --- canonical dataset output (build_ipeds_canonical_dataset.R) ------------

# Curated must-have columns; a subset of the full ~80-column canonical list.
# If any of these are absent the downstream website and workbook will silently
# produce wrong results, so we hard-stop before writing the CSV.
CANONICAL_REQUIRED_COLS <- c(
  # identity
  "unitid", "institution_name", "institution_unique_name",
  "year", "control_label", "state", "city",
  # enrolment
  "enrollment_headcount_total",
  # financials
  "revenue_total", "expenses_total", "loss_amount",
  "net_tuition_total", "tuition_dependence_pct",
  # trend flags
  "ended_2024_at_loss", "losses_last_3_of_5"
)

CANONICAL_KEY_COLS <- c("unitid", "year")

CANONICAL_TYPE_SPECS <- list(
  unitid = is.character,
  year   = is.numeric
)

# --- enriched dataset inputs (shared by workbook and export scripts) -------

ENRICHED_REQUIRED_COLS <- c(
  "unitid", "institution_name", "year", "control_label", "state"
)

ENRICHED_KEY_COLS <- c("unitid", "year")

ENRICHED_TYPE_SPECS <- list(
  unitid = is.character
)

# --- article workbook input (build_article_workbook.R) ---------------------

WORKBOOK_REQUIRED_COLS <- c(
  ENRICHED_REQUIRED_COLS,
  "enrollment_headcount_total",
  "revenue_total", "expenses_total",
  "tuition_dependence_pct", "ended_2024_at_loss"
)

WORKBOOK_KEY_COLS <- c("unitid", "year")

# --- web export input (build_web_exports.R) --------------------------------

EXPORT_REQUIRED_COLS <- c(
  ENRICHED_REQUIRED_COLS,
  "institution_unique_name",
  "enrollment_headcount_total",
  "revenue_total", "expenses_total",
  "tuition_dependence_pct"
)

EXPORT_KEY_COLS <- c("unitid", "year")

# ---------------------------------------------------------------------------
# Convenience validators
# ---------------------------------------------------------------------------

# Validates the canonical IPEDS dataset just before it is written to CSV.
# Called at the end of build_ipeds_canonical_dataset.R main().
validate_canonical_output <- function(df) {
  assert_columns(df, CANONICAL_REQUIRED_COLS, "canonical IPEDS output")
  assert_no_duplicate_keys(df, CANONICAL_KEY_COLS, "canonical IPEDS output")
  assert_column_types(df, CANONICAL_TYPE_SPECS, "canonical IPEDS output")
  invisible(df)
}

# Generic validator for any script that ingests the enriched dataset.
# `label` should name the calling script so errors are easy to trace.
validate_enriched_input <- function(df, label = "enriched input") {
  assert_columns(df, ENRICHED_REQUIRED_COLS, label)
  assert_column_types(df, ENRICHED_TYPE_SPECS, label)
  invisible(df)
}

# Validates the workbook input after numeric conversion.
# Called in build_article_workbook.R just before warning-score computation.
validate_workbook_input <- function(df) {
  assert_columns(df, WORKBOOK_REQUIRED_COLS, "article workbook input")
  invisible(df)
}

# Validates the web-export input after numeric conversion.
# Called in build_web_exports.R just before the school-level JSON loop.
validate_export_input <- function(df) {
  assert_columns(df, EXPORT_REQUIRED_COLS, "web export input")
  assert_no_duplicate_keys(df, EXPORT_KEY_COLS, "web export input")
  invisible(df)
}
