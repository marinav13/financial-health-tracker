# ============================================================================
# scripts/shared/ipeds_collector_helpers.R
# ============================================================================
#
# PURPOSE:
#   Low-level helpers for reading IPEDS data files and building the wide,
#   raw-but-decoded institution-year dataset. Pure transformations that
#   are unit-testable without network or cache access.
#
# WHERE IT FITS:
#   - Source this AFTER utils.R and ipeds_helpers.R
#   - Used inside collect_ipeds_data.R's main() function
#   - Handles data extraction from IPEDS zip files and dictionary lookups
#
# KEY CONCEPTS:
#   - EAP (Employee Activity Profile): IPEDS staffing survey (tables 100, 210)
#   - FTE calculation: FT + (PT / 3) — part-time staff counted as 1/3 equivalent
#   - Table indexing: pre-built split lists (by unitid) for fast lookups
#   - Dictionary lookups: code ? label mappings from IPEDS downloadable dictionaries
#
# IPEDS VARIABLES USED:
#   - unitid: unique college ID
#   - EAP tables: Employee Activity Profile (staffing data)
#   - EAPFT: full-time staff count
#   - EAPPT: part-time staff count
#   - EAPTOT: total staff headcount
#   - FTE: Full-Time Equivalent (12-month unduplicated enrollment or staffing count)
#
# DEPENDENCIES:
#   - dplyr, purrr (loaded by caller)
#   - get_number, first_non_null, safe_divide from utils.R
#
# ============================================================================

# ---------------------------------------------------------------------------
# EAP (Employee Activity Profile) Index Helpers
# ---------------------------------------------------------------------------

#' Retrieve Total Staff FTE from EAP 100 Index
#'
#' Looks up full-time-equivalent total-staff count from the pre-built EAP 100
#' index (Employee Activity Profile, table 100). FTE is calculated as:
#'   FTE = full-time staff + (part-time staff / 3)
#'
#' @param unitid Character or numeric institution ID (IPEDS unitid)
#' @param eap_100_index List of data frames, keyed by unitid, containing EAP 100 rows
#'
#' @return Numeric FTE value (full-time + part-time/3), or NA_real_ if:
#'   - unitid not found in index
#'   - row is empty
#'   - both EAPFT and EAPPT are NA
#'
#' @details
#'   Formula: FTE = FT + PT/3 (part-time staff are weighted at 1/3 of full-time)
#'   Uses first row only if multiple rows exist for a unitid.
#'
get_eap_total_fte <- function(unitid, eap_100_index) {
  # Look up the institution's EAP 100 data
  hit <- eap_100_index[[unitid]]
  # Return NA if not found or empty
  if (is.null(hit) || nrow(hit) == 0) return(NA_real_)
  # Take first row only
  hit <- hit[1L, , drop = FALSE]
  # Extract full-time and part-time staff counts
  ft <- get_number(hit, "EAPFT")
  pt <- get_number(hit, "EAPPT")
  # Return NA if both are missing
  if (is.na(ft) && is.na(pt)) return(NA_real_)
  # Calculate FTE: treat missing values as 0
  dplyr::coalesce(ft, 0) + dplyr::coalesce(pt, 0) / 3
}

#' Retrieve Instructional Staff FTE from EAP 210 Index
#'
#' Looks up full-time-equivalent instructional-staff count from EAP 210
#' (Employee Activity Profile, table 210 — instructional staff only).
#' FTE calculated as: FTE = full-time instructional staff + (part-time instructional staff / 3)
#'
#' @param unitid Character or numeric institution ID
#' @param eap_210_index List of data frames, keyed by unitid, containing EAP 210 rows
#'
#' @return Numeric FTE value, or NA_real_ if not found/empty/both FT and PT are NA
#'
#' @details
#'   Instructional staff = faculty and teaching-focused personnel only.
#'   Does not include administrators, support staff, or other non-teaching roles.
#'   Formula: FTE = FT + PT/3
#'
get_eap_instructional_fte <- function(unitid, eap_210_index) {
  # Look up the institution's EAP 210 data
  hit <- eap_210_index[[unitid]]
  # Return NA if not found or empty
  if (is.null(hit) || nrow(hit) == 0) return(NA_real_)
  # Take first row only
  hit <- hit[1L, , drop = FALSE]
  # Extract full-time and part-time instructional staff counts
  ft <- get_number(hit, "EAPFT")
  pt <- get_number(hit, "EAPPT")
  # Return NA if both are missing
  if (is.na(ft) && is.na(pt)) return(NA_real_)
  # Calculate FTE: treat missing values as 0
  dplyr::coalesce(ft, 0) + dplyr::coalesce(pt, 0) / 3
}

#' Retrieve Total Staff Headcount from EAP 100 Index
#'
#' Looks up total staff headcount (raw employee count, not FTE) from EAP 100.
#' Headcount is the raw number of people employed, regardless of full-time
#' or part-time status.
#'
#' @param unitid Character or numeric institution ID
#' @param eap_100_index List of data frames, keyed by unitid
#'
#' @return Numeric headcount from EAPTOT field, or NA_real_ if not found/empty
#'
#' @details
#'   EAPTOT = total staff headcount across all employment categories.
#'   Does not convert to FTE; raw employee count.
#'
get_eap_total_headcount <- function(unitid, eap_100_index) {
  # Look up the institution's EAP 100 data
  hit <- eap_100_index[[unitid]]
  # Return NA if not found or empty
  if (is.null(hit) || nrow(hit) == 0) return(NA_real_)
  # Take first row only
  hit <- hit[1L, , drop = FALSE]
  # Extract total headcount
  get_number(hit, "EAPTOT")
}

#' Retrieve Instructional Staff Headcount from EAP 210 Index
#'
#' Looks up instructional staff headcount (raw employee count) from EAP 210.
#' Headcount is not converted to FTE.
#'
#' @param unitid Character or numeric institution ID
#' @param eap_210_index List of data frames, keyed by unitid
#'
#' @return Numeric headcount from EAPTOT field (EAP 210), or NA_real_ if not found/empty
#'
#' @details
#'   EAPTOT in EAP 210 = instructional staff headcount only.
#'   Raw count, not FTE-adjusted.
#'
get_eap_instructional_headcount <- function(unitid, eap_210_index) {
  # Look up the institution's EAP 210 data
  hit <- eap_210_index[[unitid]]
  # Return NA if not found or empty
  if (is.null(hit) || nrow(hit) == 0) return(NA_real_)
  # Take first row only
  hit <- hit[1L, , drop = FALSE]
  # Extract instructional staff headcount
  get_number(hit, "EAPTOT")
}

# ---------------------------------------------------------------------------
# Institution-level Field Lookups (used inside the per-unitid loop)
# ---------------------------------------------------------------------------

#' Lookup a Single Row from a Pre-Built Table Index
#'
#' Returns a data frame row from a pre-indexed IPEDS table, or NULL if the
#' institution or table doesn't exist. Used for fast O(1) lookups during
#' row building.
#'
#' @param table_index List of split data frames, keyed first by table alias,
#'   then by unitid (produced by split(tbl, tbl$UNITID))
#' @param alias Character table alias (e.g., "hd", "f1_gasb", "ic")
#' @param unitid Character or numeric institution ID
#'
#' @return Single-row data frame, or NULL if:
#'   - alias has no table in index
#'   - unitid not found in that table
#'   - empty result set
#'
#' @details
#'   Used to quickly retrieve institution-specific rows from IPEDS tables.
#'   Example: lookup_row(all_tables, "hd", "123456") gets institution
#'   123456's HD (institutional characteristics) row.
#'
lookup_row <- function(table_index, alias, unitid) {
  # Look up the table by alias
  idx <- table_index[[alias]]
  # Return NULL if table doesn't exist
  if (is.null(idx)) return(NULL)
  # Look up the institution within that table
  row <- idx[[unitid]]
  # Return NULL if institution doesn't exist or is empty
  if (is.null(row) || nrow(row) == 0) return(NULL)
  # Return first row (in case of duplicates)
  row[1L, , drop = FALSE]
}

#' Lookup a String Field from a One-Row Data Frame
#'
#' Retrieves a string value from a single-row data frame by column name,
#' returning NA_character_ for missing/blank values. Safe wrapper that
#' handles common edge cases (absent columns, NULL values, blanks).
#'
#' @param row Single-row data frame, or NULL
#' @param field_name Character column name to extract
#'
#' @return Character value from the field, or NA_character_ if:
#'   - row is NULL
#'   - field_name is NA or ""
#'   - field_name not in row's columns
#'   - value is NULL or blank ("")
#'
#' @details
#'   Coerces result to character. Empty strings are treated as NA.
#'
lookup_string <- function(row, field_name) {
  # Handle NULL row or invalid field_name
  if (is.null(row) || is.na(field_name) || identical(field_name, "")) return(NA_character_)
  # Check if field exists in row
  if (!(field_name %in% names(row))) return(NA_character_)
  # Extract value
  value <- row[[field_name]][[1]]
  # Return NA if NULL or empty string
  if (is.null(value) || identical(as.character(value), "")) NA_character_ else as.character(value)
}

#' Lookup a Numeric Field from a One-Row Data Frame
#'
#' Retrieves a numeric value from a single-row data frame by column name.
#' Strips commas (common in IPEDS text files) before converting to numeric.
#' Returns NA_real_ for blank or non-numeric values.
#'
#' @param row Single-row data frame, or NULL
#' @param field_name Character column name to extract
#'
#' @return Numeric value, or NA_real_ if:
#'   - field is blank/NA (via lookup_string)
#'   - value cannot be parsed as numeric
#'
#' @details
#'   Common IPEDS values have commas: "1,234,567" becomes 1234567.
#'   Suppresses coercion warnings (e.g., from failed conversions).
#'
lookup_number <- function(row, field_name) {
  # First get the string value (handles NULL row, missing field, etc.)
  value <- lookup_string(row, field_name)
  # If string lookup returned NA, return NA numeric
  if (is.na(value)) return(NA_real_)
  # Remove commas, trim whitespace, and convert to numeric
  suppressWarnings(as.numeric(gsub(",", "", trimws(value), fixed = TRUE)))
}

# ---------------------------------------------------------------------------
# GASB/FASB Core Revenue Computation Helpers
# ---------------------------------------------------------------------------

#' Rebuild GASB Core Revenue from Total Revenue
#'
#' Computes core revenue (educational/main operations revenue) from total
#' revenue reported in GASB finance forms by subtracting auxiliary enterprises,
#' hospital, and independent-operations revenue.
#'
#' Core revenue excludes:
#'   - Auxiliary enterprises (bookstore, housing, dining, etc.)
#'   - Hospital services (for institutions that operate hospitals)
#'   - Independent operations (unrelated business income)
#'
#' @param f1_row Single-row data frame from GASB Finance form (F1), or NULL
#'
#' @return Numeric core revenue, or NA_real_ if:
#'   - f1_row is NULL
#'   - total revenue (F1D01) is NA
#'
#' @details
#'   Formula: Core = Total - Auxiliary - Hospital - Independent
#'   Only used when F1D01 (total revenue) is present; otherwise returns NA.
#'   Treats missing auxiliary/hospital/independent as $0.
#'
rebuild_core_revenue_gasb <- function(f1_row) {
  # Return NA if no row provided
  if (is.null(f1_row)) return(NA_real_)
  # Get total operating and non-operating revenues
  total <- lookup_number(f1_row, "F1D01")
  # Return NA if total is missing
  if (is.na(total)) return(NA_real_)
  # Subtract auxiliary (F1B05), hospital (F1B06), and independent (F1B07) revenues
  # coalesce: treat missing as 0, keep actual values
  total -
    dplyr::coalesce(lookup_number(f1_row, "F1B05"), 0) -
    dplyr::coalesce(lookup_number(f1_row, "F1B06"), 0) -
    dplyr::coalesce(lookup_number(f1_row, "F1B07"), 0)
}

#' Rebuild FASB Core Revenue from Total Revenue
#'
#' Computes core revenue from total revenue reported in FASB finance forms
#' by subtracting auxiliary enterprises, hospital, and independent-operations
#' revenue. FASB is used by private not-for-profit institutions.
#'
#' Core revenue excludes same categories as GASB:
#'   - Auxiliary enterprises
#'   - Hospital services
#'   - Independent operations
#'
#' @param f2_row Single-row data frame from FASB Finance form (F2), or NULL
#' @param total_revenues_field Character name of total revenues column
#'   (varies by year; e.g., "F2D01" or other field)
#'
#' @return Numeric core revenue, or NA_real_ if:
#'   - f2_row is NULL
#'   - total_revenues_field is NA
#'
#' @details
#'   Formula: Core = Total - Auxiliary (F2D12) - Hospital (F2D13) - Independent (F2D14)
#'   Treats missing auxiliary/hospital/independent as $0.
#'
rebuild_core_revenue_fasb <- function(f2_row, total_revenues_field) {
  # Return NA if no row provided
  if (is.null(f2_row)) return(NA_real_)
  # Get total revenues using the specified field name
  total <- lookup_number(f2_row, total_revenues_field)
  # Return NA if total is missing
  if (is.na(total)) return(NA_real_)
  # Subtract auxiliary (F2D12), hospital (F2D13), and independent (F2D14) revenues
  total -
    dplyr::coalesce(lookup_number(f2_row, "F2D12"), 0) -
    dplyr::coalesce(lookup_number(f2_row, "F2D13"), 0) -
    dplyr::coalesce(lookup_number(f2_row, "F2D14"), 0)
}
