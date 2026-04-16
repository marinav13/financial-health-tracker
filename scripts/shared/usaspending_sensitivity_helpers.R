# usaspending_sensitivity_helpers.R
# Pure helper functions for the USAspending sensitivity analysis.
#
# Only the helpers actually used by the production script live here, so the file
# stays small and the test surface is bounded.
#
# Source this before sourcing the main script:
#   source(file.path(getwd(), "scripts", "shared", "usaspending_sensitivity_helpers.R"))

# -------------------------------------------------------------------
# Numeric helpers
# -------------------------------------------------------------------

#' Coerce a value to numeric, returning NA_real_ on failure.
#' @param x Any value.
#' @return Numeric or NA_real_.
safe_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  suppressWarnings(as.numeric(x))
}

# -------------------------------------------------------------------
# Date helpers
# -------------------------------------------------------------------

#' Parse a date, treating blank and whitespace-only strings as NA.
#' @param x Character vector of dates.
#' @return Date vector.
parse_date_safe <- function(x) {
  x <- trimws(as.character(x))
  x[!nzchar(x)] <- NA_character_
  as.Date(x)
}

#' Convert a USAspending fiscal year/month reporting period to the last
#' calendar day of that period.
#'
#' USAspending reports by fiscal year (Oct = month 1) and reporting month.
#' The "last calendar date" is the day before the first day of the following
#' calendar month.
#'
#' @param fy Fiscal year (integer, e.g. 2025).
#' @param fm Reporting month within the fiscal year (1–12, Oct=1 … Sep=12).
#' @return Date of the last calendar day in the reporting period.
period_end_date_from_fiscal <- function(fy, fm) {
  fy <- as.integer(fy)
  fm <- as.integer(fm)
  cal_month <- ((fm + 8L) %% 12L) + 1L
  cal_year  <- ifelse(cal_month >= 10L, fy - 1L, fy)
  next_month_year <- ifelse(cal_month == 12L, cal_year + 1L, cal_year)
  next_month     <- ifelse(cal_month == 12L, 1L, cal_month + 1L)
  as.Date(sprintf("%04d-%02d-01", next_month_year, next_month)) - 1L
}

# -------------------------------------------------------------------
# Proposal / summary helpers
# -------------------------------------------------------------------

#' Build the risky-continuation filter summary table from flagged rows.
#'
#' This is the only production summary in the simplified script. The full
#' comparison table and distribution statistics are exploratory artefacts and
#' have been removed.
#'
#' @param flagged tibble with a 'proposal_flag' column and standard signal
#'        columns (post_termination_positive_cont_rev_*, institution_key, etc.).
#' @return tibble summarising the flagged grants.
summarise_risky_filter <- function(flagged) {
  flagged |>
    dplyr::filter(proposal_flag) |>
    dplyr::summarise(
      proposal = "risky_continuation_filter",
      grants_flagged = dplyr::n(),
      institutions_affected = dplyr::n_distinct(institution_key),
      excluded_award_remaining = sum(award_remaining, na.rm = TRUE),
      flagged_with_jul_dec_2025_activity = sum(
        post_termination_positive_cont_rev_jul_dec_2025 > 0,
        na.rm = TRUE
      ),
      flagged_with_2026_activity = sum(
        post_termination_positive_cont_rev_2026 > 0,
        na.rm = TRUE
      )
    )
}
