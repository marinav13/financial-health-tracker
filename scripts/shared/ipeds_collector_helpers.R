# scripts/shared/ipeds_collector_helpers.R
#
# Helpers for scripts/collect_ipeds_data.R that read IPEDS zips and build
# the wide raw decoded institution-year dataset.  These functions are pure
# transforms of their inputs and are unit-testable without network or cache.
#
# Source this after utils.R and ipeds_helpers.R inside collect_ipeds_data.R's
# main() function.
#
# Requires: dplyr, purrr (loaded by the caller)
# Requires: get_number, first_non_null, safe_divide from utils.R

# ---------------------------------------------------------------------------
# EAP (Employee Activity Profile) index helpers
# ---------------------------------------------------------------------------

# Looks up full-time-equivalent total-staff count from the pre-built EAP 100 index.
# Returns: FTE = ft_staff + pt_staff / 3, or NA_real_ if no data for this unitid.
get_eap_total_fte <- function(unitid, eap_100_index) {
  hit <- eap_100_index[[unitid]]
  if (is.null(hit) || nrow(hit) == 0) return(NA_real_)
  hit <- hit[1L, , drop = FALSE]
  ft <- get_number(hit, "EAPFT")
  pt <- get_number(hit, "EAPPT")
  if (is.na(ft) && is.na(pt)) return(NA_real_)
  dplyr::coalesce(ft, 0) + dplyr::coalesce(pt, 0) / 3
}

# Looks up full-time-equivalent instructional-staff count from the pre-built EAP 210 index.
# Returns: FTE = ft_staff + pt_staff / 3, or NA_real_ if no data for this unitid.
get_eap_instructional_fte <- function(unitid, eap_210_index) {
  hit <- eap_210_index[[unitid]]
  if (is.null(hit) || nrow(hit) == 0) return(NA_real_)
  hit <- hit[1L, , drop = FALSE]
  ft <- get_number(hit, "EAPFT")
  pt <- get_number(hit, "EAPPT")
  if (is.na(ft) && is.na(pt)) return(NA_real_)
  dplyr::coalesce(ft, 0) + dplyr::coalesce(pt, 0) / 3
}

# Looks up total staff headcount from the pre-built EAP 100 index.
# Returns: total EAP headcount from EAPTOT, or NA_real_ if no data for this unitid.
get_eap_total_headcount <- function(unitid, eap_100_index) {
  hit <- eap_100_index[[unitid]]
  if (is.null(hit) || nrow(hit) == 0) return(NA_real_)
  hit <- hit[1L, , drop = FALSE]
  get_number(hit, "EAPTOT")
}

# Looks up instructional staff headcount from the pre-built EAP 210 index.
# Returns: instructional EAP headcount from EAPTOT, or NA_real_ if no data for this unitid.
get_eap_instructional_headcount <- function(unitid, eap_210_index) {
  hit <- eap_210_index[[unitid]]
  if (is.null(hit) || nrow(hit) == 0) return(NA_real_)
  hit <- hit[1L, , drop = FALSE]
  get_number(hit, "EAPTOT")
}

# ---------------------------------------------------------------------------
# Institution-level field lookups (used inside the per-unitid loop)
# ---------------------------------------------------------------------------

# Returns a single-row data frame from a pre-built table index, or NULL if
# the alias has no table or the unitid is absent.
# table_index: list of UNITID-keyed split lists (produced by split(tbl, tbl$UNITID))
lookup_row <- function(table_index, alias, unitid) {
  idx <- table_index[[alias]]
  if (is.null(idx)) return(NULL)
  row <- idx[[unitid]]
  if (is.null(row) || nrow(row) == 0) return(NULL)
  row[1L, , drop = FALSE]
}

# Looks up a string field from a one-row data frame by resolved IPEDS column name.
# Falls back to NA_character_ when the field is absent or blank.
lookup_string <- function(row, field_name) {
  if (is.null(row) || is.na(field_name) || identical(field_name, "")) return(NA_character_)
  if (!(field_name %in% names(row))) return(NA_character_)
  value <- row[[field_name]][[1]]
  if (is.null(value) || identical(as.character(value), "")) NA_character_ else as.character(value)
}

# Looks up a numeric field from a one-row data frame by resolved IPEDS column name.
# Strips commas before parsing; returns NA_real_ for blank or non-numeric values.
lookup_number <- function(row, field_name) {
  value <- lookup_string(row, field_name)
  if (is.na(value)) return(NA_real_)
  suppressWarnings(as.numeric(gsub(",", "", trimws(value), fixed = TRUE)))
}

# ---------------------------------------------------------------------------
# GASB/FASB core revenue computation helpers
# ---------------------------------------------------------------------------

# Rebuilds GASB core revenue by subtracting auxiliary/hospital/independent-
# operations from the IPEDS-reported total.  Only used when F1D01 is present.
rebuild_core_revenue_gasb <- function(f1_row) {
  if (is.null(f1_row)) return(NA_real_)
  total <- lookup_number(f1_row, "F1D01")
  if (is.na(total)) return(NA_real_)
  total -
    dplyr::coalesce(lookup_number(f1_row, "F1B05"), 0) -
    dplyr::coalesce(lookup_number(f1_row, "F1B06"), 0) -
    dplyr::coalesce(lookup_number(f1_row, "F1B07"), 0)
}

# Rebuilds FASB core revenue by subtracting auxiliary/hospital/independent-
# operations from the IPEDS-reported total investment-return revenue.
rebuild_core_revenue_fasb <- function(f2_row, total_revenues_field) {
  if (is.null(f2_row)) return(NA_real_)
  total <- lookup_number(f2_row, total_revenues_field)
  if (is.na(total)) return(NA_real_)
  total -
    dplyr::coalesce(lookup_number(f2_row, "F2D12"), 0) -
    dplyr::coalesce(lookup_number(f2_row, "F2D13"), 0) -
    dplyr::coalesce(lookup_number(f2_row, "F2D14"), 0)
}
