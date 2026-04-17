# ============================================================================
# scripts/shared/ipeds_collector_helpers.R
# ============================================================================
#
# Low-level helpers for reading IPEDS data files and building the raw-but-decoded
# institution-year dataset. Pure transformations that are unit-testable without
# network or cache access. Source AFTER utils.R and ipeds_helpers.R.
#
# Key concepts: FTE = FT + (PT / 3) for staff; table indexes provide O(1) lookups
# by unitid; dictionary lookups map codes to labels.
#
# ============================================================================

# ---------------------------------------------------------------------------
# EAP (Employee Activity Profile) Index Helpers
# ---------------------------------------------------------------------------

# Looks up total staff FTE (full-time + part-time/3) from the pre-built EAP 100 index.
get_eap_total_fte <- function(unitid, eap_100_index) {
  hit <- eap_100_index[[unitid]]
  if (is.null(hit) || nrow(hit) == 0) return(NA_real_)
  hit <- hit[1L, , drop = FALSE]
  ft <- get_number(hit, "EAPFT")
  pt <- get_number(hit, "EAPPT")
  if (is.na(ft) && is.na(pt)) return(NA_real_)
  dplyr::coalesce(ft, 0) + dplyr::coalesce(pt, 0) / 3
}

# Looks up instructional staff FTE (faculty and teaching staff only) from EAP 210 index.
get_eap_instructional_fte <- function(unitid, eap_210_index) {
  hit <- eap_210_index[[unitid]]
  if (is.null(hit) || nrow(hit) == 0) return(NA_real_)
  hit <- hit[1L, , drop = FALSE]
  ft <- get_number(hit, "EAPFT")
  pt <- get_number(hit, "EAPPT")
  if (is.na(ft) && is.na(pt)) return(NA_real_)
  dplyr::coalesce(ft, 0) + dplyr::coalesce(pt, 0) / 3
}

# Looks up total staff headcount (raw employee count, not FTE) from EAP 100.
get_eap_total_headcount <- function(unitid, eap_100_index) {
  hit <- eap_100_index[[unitid]]
  if (is.null(hit) || nrow(hit) == 0) return(NA_real_)
  hit <- hit[1L, , drop = FALSE]
  get_number(hit, "EAPTOT")
}

# Looks up instructional staff headcount (raw count) from EAP 210.
get_eap_instructional_headcount <- function(unitid, eap_210_index) {
  hit <- eap_210_index[[unitid]]
  if (is.null(hit) || nrow(hit) == 0) return(NA_real_)
  hit <- hit[1L, , drop = FALSE]
  get_number(hit, "EAPTOT")
}

# ---------------------------------------------------------------------------
# Institution-level Field Lookups
# ---------------------------------------------------------------------------

# Returns a single row from a pre-indexed IPEDS table by table alias and unitid.
lookup_row <- function(table_index, alias, unitid) {
  idx <- table_index[[alias]]
  if (is.null(idx)) return(NULL)
  row <- idx[[unitid]]
  if (is.null(row) || nrow(row) == 0) return(NULL)
  row[1L, , drop = FALSE]
}

# Retrieves a string value from a single-row data frame, returning NA_character_ for missing/blank values.
lookup_string <- function(row, field_name) {
  if (is.null(row) || is.na(field_name) || identical(field_name, "")) return(NA_character_)
  if (!(field_name %in% names(row))) return(NA_character_)
  value <- row[[field_name]][[1]]
  if (is.null(value) || identical(as.character(value), "")) NA_character_ else as.character(value)
}

# Retrieves a numeric value from a single-row data frame, stripping commas before converting.
lookup_number <- function(row, field_name) {
  value <- lookup_string(row, field_name)
  if (is.na(value)) return(NA_real_)
  suppressWarnings(as.numeric(gsub(",", "", trimws(value), fixed = TRUE)))
}

# ---------------------------------------------------------------------------
# GASB/FASB Core Revenue Computation Helpers
# ---------------------------------------------------------------------------

# Computes core revenue (educational/main operations) from total GASB revenue
# by subtracting auxiliary, hospital, and independent-operations revenue.
rebuild_core_revenue_gasb <- function(f1_row) {
  if (is.null(f1_row)) return(NA_real_)
  total <- lookup_number(f1_row, "F1D01")
  if (is.na(total)) return(NA_real_)
  total -
    dplyr::coalesce(lookup_number(f1_row, "F1B05"), 0) -
    dplyr::coalesce(lookup_number(f1_row, "F1B06"), 0) -
    dplyr::coalesce(lookup_number(f1_row, "F1B07"), 0)
}

# Computes core revenue from total FASB revenue (used by private not-for-profit
# institutions) by subtracting auxiliary, hospital, and independent-operations revenue.
rebuild_core_revenue_fasb <- function(f2_row, total_revenues_field) {
  if (is.null(f2_row)) return(NA_real_)
  total <- lookup_number(f2_row, total_revenues_field)
  if (is.na(total)) return(NA_real_)
  total -
    dplyr::coalesce(lookup_number(f2_row, "F2D12"), 0) -
    dplyr::coalesce(lookup_number(f2_row, "F2D13"), 0) -
    dplyr::coalesce(lookup_number(f2_row, "F2D14"), 0)
}
