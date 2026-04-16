# ============================================================================
# scripts/shared/ipeds_enrichment_helpers.R
# ============================================================================
#
# PURPOSE:
#   Enriches a single institution's multi-year IPEDS data with year-over-year
#   (YoY) and multi-year (5-year, 10-year) trend metrics. Each function adds
#   one thematic group of metrics (enrollment, revenue, tuition, etc.).
#
# WHERE IT FITS:
#   - Source this AFTER utils.R
#   - Use enrich_group() via group_modify() in the data pipeline
#   - Called once per institution per data load
#
# KEY CONCEPTS:
#   - All functions work on rowwise() data (one calculation per row)
#   - Lookups: named vectors mapping years to values for lag operations
#   - safe_pct_change(): computes ((new - old) / old) * 100, handles NAs
#   - NAs propagate through calculations for data integrity
#
# IPEDS VARIABLES USED:
#   - unitid: unique college ID (from IPEDS HD file)
#   - FTE (Full-Time Equivalent): 12-month unduplicated count, enrollment metric
#   - Title IV: federal financial aid eligibility marker
#   - EF: enrollment survey (Customizable Data Selection)
#   - F: finance survey (FASB/GASB/PFP forms)
#
# ============================================================================

# ---------------------------------------------------------------------------
# Enrollment and Staffing Enrichment
# ---------------------------------------------------------------------------

#' Enrich Enrollment and Staffing Metrics
#'
#' Calculates 5-year, 1-year, and trend-based enrollment/staffing metrics
#' for a single institution. Compares current year values to lagged years
#' to measure growth or decline across student counts and workforce size.
#'
#' @param df A data frame with one row per institution-year (already sorted by year)
#' @param years Integer vector of years available in df (used for lookups)
#' @param lookups List of named numeric vectors (year -> value mappings) for:
#'   - enroll, enroll_fte: headcount and FTE enrollment
#'   - staff_fte, staff_instr_fte: total and instructional FTE staffing
#'   - staff_head, staff_instr_head: total and instructional headcount
#'
#' @return Data frame with new columns:
#'   - enrollment_pct_change_5yr: % change in headcount enrollment vs 5 yrs ago
#'   - enrollment_decreased_5yr: "Yes"/"No" indicator
#'   - enroll_fte_pct_change_5yr: % change in FTE enrollment vs 5 yrs ago
#'   - enrollment_decline_last_3_of_5: "Yes" if declined in 3+ of last 5 years
#'   - enroll_fte_decline_last_3_of_5: "Yes" if FTE declined in 3+ of last 5 years
#'   - staff_total_pct_change_5yr, staff_instructional_fte_pct_change_5yr: staffing changes
#'   - enrollment_change_1yr, staff_change_1yr: YoY changes
#'
enrich_enrollment_staffing <- function(df, years, lookups) {
  df %>%
    rowwise() %>%
    mutate(
      # 5-year headcount enrollment change: (current enrollment - enrollment 5 years ago) / enrollment 5 years ago * 100
      enrollment_pct_change_5yr                    = safe_pct_change(enrollment_headcount_total, unname(lookups$enroll[as.character(year - 5)])),
      # Simple Yes/No flag: did headcount decline over 5 years?
      enrollment_decreased_5yr                     = case_when(is.na(enrollment_pct_change_5yr) ~ NA_character_, enrollment_pct_change_5yr < 0 ~ "Yes", TRUE ~ "No"),
      # 5-year FTE enrollment change (FTE = Full-Time Equivalent, the primary enrollment metric)
      enroll_fte_pct_change_5yr                    = safe_pct_change(fte_12_months, unname(lookups$enroll_fte[as.character(year - 5)])),
      # Risk flag: did FTE enrollment decline in 3 or more of the past 5 years?
      enrollment_decline_last_3_of_5               = ifelse(count_decline_years(years, unname(lookups$enroll), year - 5, year - 1, 0) >= 3, "Yes", "No"),
      # Risk flag: did FTE-based enrollment decline in 3 or more of the past 5 years?
      enroll_fte_decline_last_3_of_5               = ifelse(count_decline_years(years, unname(lookups$enroll_fte), year - 5, year - 1, 0) >= 3, "Yes", "No"),
      # 5-year total staff FTE change (includes all full-time and part-time staff)
      staff_total_pct_change_5yr                   = safe_pct_change(staff_fte_total, unname(lookups$staff_fte[as.character(year - 5)])),
      # 5-year instructional staff FTE change (faculty and teaching staff only)
      staff_instructional_fte_pct_change_5yr       = safe_pct_change(staff_fte_instructional, unname(lookups$staff_instr_fte[as.character(year - 5)])),
      # 5-year total staff headcount change (raw count of employees)
      staff_total_headcount_pct_change_5yr         = safe_pct_change(staff_headcount_total, unname(lookups$staff_head[as.character(year - 5)])),
      # 5-year instructional staff headcount change
      staff_instructional_headcount_pct_change_5yr = safe_pct_change(staff_headcount_instructional, unname(lookups$staff_instr_head[as.character(year - 5)])),
      # 1-year YoY headcount enrollment change
      enrollment_change_1yr                        = safe_pct_change(enrollment_headcount_total, unname(lookups$enroll[as.character(year - 1)])),
      # 1-year YoY staff headcount change
      staff_change_1yr                             = safe_pct_change(staff_headcount_total, unname(lookups$staff_head[as.character(year - 1)]))
    ) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# Revenue Metrics Enrichment
# ---------------------------------------------------------------------------

#' Enrich Revenue Metrics
#'
#' Calculates 5-year, 1-year, and risk-based revenue metrics including
#' nominal (actual dollar) and adjusted (inflation-corrected) versions.
#' Flags institutions with significant revenue declines.
#'
#' @param df Data frame with one row per institution-year
#' @param years Integer vector of available years
#' @param lookups List with revenue, revenue_adj, op_margin vectors
#'
#' @return Data frame with new columns:
#'   - revenue_pct_change_5yr_nominal: % change in nominal revenue
#'   - revenue_pct_change_5yr: % change in inflation-adjusted revenue
#'   - revenue_decreased_5yr: "Yes"/"No" indicator
#'   - revenue_10pct_drop_last_3_of_5: "Yes" if >10% decline in 3+ of past 5 years
#'   - losses_last_3_of_5: "Yes" if negative operating margin in 3+ of past 5 years
#'
enrich_revenue_metrics <- function(df, years, lookups) {
  df %>%
    rowwise() %>%
    mutate(
      # 5-year nominal revenue change (not adjusted for inflation)
      revenue_pct_change_5yr_nominal    = safe_pct_change(revenue_total, unname(lookups$revenue[as.character(year - 5)])),
      # 5-year inflation-adjusted revenue change (comparable across time)
      revenue_pct_change_5yr            = safe_pct_change(revenue_total_adjusted, unname(lookups$revenue_adj[as.character(year - 5)])),
      # Alias for clarity in downstream use
      revenue_pct_change_5yr_adjusted   = revenue_pct_change_5yr,
      # Simple Yes/No: did inflation-adjusted revenue decline?
      revenue_decreased_5yr             = case_when(is.na(revenue_pct_change_5yr) ~ NA_character_, revenue_pct_change_5yr < 0 ~ "Yes", TRUE ~ "No"),
      # 1-year nominal revenue change
      revenue_change_1yr_nominal        = safe_pct_change(revenue_total, unname(lookups$revenue[as.character(year - 1)])),
      # 1-year inflation-adjusted revenue change
      revenue_change_1yr                = safe_pct_change(revenue_total_adjusted, unname(lookups$revenue_adj[as.character(year - 1)])),
      # Risk flag: >10% revenue decline in 3 or more of the past 5 years?
      # threshold_pct = -10 means: count years where pct_change <= -10%
      revenue_10pct_drop_last_3_of_5    = ifelse(count_decline_years(years, unname(lookups$revenue_adj), year - 5, year - 1, -10) >= 3, "Yes", "No"),
      # Risk flag: negative operating margin (expenses > revenue) in 3+ of past 5 years?
      losses_last_3_of_5                = ifelse(count_negative_years(years, unname(lookups$op_margin), (year - 4):year, 0) >= 3, "Yes", "No")
    ) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# Tuition and Net Revenue Metrics Enrichment
# ---------------------------------------------------------------------------

#' Enrich Tuition and Enrollment Yield Metrics
#'
#' Calculates trends in net tuition revenue (tuition after financial aid
#' discounts), per-student tuition metrics, admissions yield, and discount rates.
#' Both nominal and inflation-adjusted versions.
#'
#' @param df Data frame with one row per institution-year
#' @param years Integer vector of available years
#' @param lookups List with net_tuition, net_tuition_adj, net_tuition_fte,
#'   net_tuition_fte_adj, yield, discount vectors
#'
#' @return Data frame with new columns:
#'   - net_tuition_pct_change_5yr: % change in total net tuition revenue
#'   - net_tuition_per_fte_change_5yr: % change in per-student net tuition
#'   - yield_pct_change_5yr: % change in admissions yield
#'   - discount_pct_change_5yr: % change in discount rate (institutional aid)
#'
enrich_tuition_metrics <- function(df, years, lookups) {
  df %>%
    rowwise() %>%
    mutate(
      # 5-year nominal net tuition change (total revenue after aid discounts)
      net_tuition_pct_change_5yr_nominal       = safe_pct_change(net_tuition_total, unname(lookups$net_tuition[as.character(year - 5)])),
      # 5-year inflation-adjusted net tuition change
      net_tuition_pct_change_5yr               = safe_pct_change(net_tuition_total_adjusted, unname(lookups$net_tuition_adj[as.character(year - 5)])),
      # Alias for downstream consistency
      net_tuition_pct_change_5yr_adjusted      = net_tuition_pct_change_5yr,
      # 5-year nominal net tuition per FTE student change
      net_tuition_per_fte_change_5yr_nominal   = safe_pct_change(net_tuition_per_fte, unname(lookups$net_tuition_fte[as.character(year - 5)])),
      # 5-year inflation-adjusted per-student net tuition change
      net_tuition_per_fte_change_5yr           = safe_pct_change(net_tuition_per_fte_adjusted, unname(lookups$net_tuition_fte_adj[as.character(year - 5)])),
      # Alias for downstream use
      net_tuition_per_fte_change_5yr_adjusted  = net_tuition_per_fte_change_5yr,
      # 5-year admissions yield change (% of admitted students who enroll)
      yield_pct_change_5yr                     = safe_pct_change(admissions_yield, unname(lookups$yield[as.character(year - 5)])),
      # 5-year discount rate change (% of gross tuition paid by institution as aid)
      discount_pct_change_5yr                  = safe_pct_change(discount_rate, unname(lookups$discount[as.character(year - 5)]))
    ) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# Government and Philanthropic Funding Enrichment
# ---------------------------------------------------------------------------

#' Enrich Government Funding Metrics
#'
#' Calculates trends in government support: federal grants/contracts, Pell-adjusted
#' federal support, and state appropriations. Tracks both total funding and
#' funding as percentage of core revenue.
#'
#' @param df Data frame with one row per institution-year
#' @param years Integer vector of available years
#' @param lookups List with govt, govt_adj, federal_adj, federal_adj_adj,
#'   state, state_adj vectors
#'
#' @return Data frame with new columns tracking 5-year changes in:
#'   - government_funding (federal + state combined)
#'   - federal_grants_contracts_pell_adjusted (excludes Pell where required)
#'   - state_funding (state appropriations only)
#'
enrich_funding_metrics <- function(df, years, lookups) {
  df %>%
    rowwise() %>%
    mutate(
      # 5-year nominal government funding change (federal + state)
      government_funding_pct_change_5yr_nominal                        = safe_pct_change(government_funding_total, unname(lookups$govt[as.character(year - 5)])),
      # 5-year inflation-adjusted government funding change
      government_funding_pct_change_5yr                                = safe_pct_change(government_funding_total_adjusted, unname(lookups$govt_adj[as.character(year - 5)])),
      # Alias for consistency
      government_funding_pct_change_5yr_adjusted                       = government_funding_pct_change_5yr,
      # 5-year nominal Pell-adjusted federal grants/contracts change
      # (excludes Pell grants under certain accounting methods for comparability)
      federal_grants_contracts_pell_adjusted_pct_change_5yr_nominal    = safe_pct_change(federal_grants_contracts_pell_adjusted, unname(lookups$federal_adj[as.character(year - 5)])),
      # 5-year inflation-adjusted Pell-adjusted federal funding change
      federal_grants_contracts_pell_adjusted_pct_change_5yr            = safe_pct_change(federal_grants_contracts_pell_adjusted_adjusted, unname(lookups$federal_adj_adj[as.character(year - 5)])),
      # Alias for consistency
      federal_grants_contracts_pell_adjusted_pct_change_5yr_adjusted   = federal_grants_contracts_pell_adjusted_pct_change_5yr,
      # 5-year nominal state appropriations change
      state_funding_pct_change_5yr_nominal                             = safe_pct_change(state_funding, unname(lookups$state[as.character(year - 5)])),
      # 5-year inflation-adjusted state funding change
      state_funding_pct_change_5yr                                     = safe_pct_change(state_funding_adjusted, unname(lookups$state_adj[as.character(year - 5)])),
      # Alias for consistency
      state_funding_pct_change_5yr_adjusted                            = state_funding_pct_change_5yr
    ) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# Endowment Metrics Enrichment
# ---------------------------------------------------------------------------

#' Enrich Endowment Value Metrics
#'
#' Calculates 5-year trends in endowment market value, both nominal and
#' inflation-adjusted. Endowments are long-term institutional financial reserves.
#'
#' @param df Data frame with one row per institution-year
#' @param years Integer vector of available years
#' @param lookups List with endowment, endowment_adj vectors
#'
#' @return Data frame with new columns:
#'   - endowment_pct_change_5yr_nominal: % change in endowment value (nominal)
#'   - endowment_pct_change_5yr: % change in inflation-adjusted endowment value
#'
enrich_endowment_metrics <- function(df, years, lookups) {
  df %>%
    rowwise() %>%
    mutate(
      # 5-year nominal endowment value change
      endowment_pct_change_5yr_nominal   = safe_pct_change(endowment_value, unname(lookups$endowment[as.character(year - 5)])),
      # 5-year inflation-adjusted endowment value change
      endowment_pct_change_5yr           = safe_pct_change(endowment_value_adjusted, unname(lookups$endowment_adj[as.character(year - 5)])),
      # Alias for consistency
      endowment_pct_change_5yr_adjusted  = endowment_pct_change_5yr
    ) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# International Student Enrollment Metrics
# ---------------------------------------------------------------------------

#' Enrich International Student Enrollment Metrics
#'
#' Calculates trends in international (non-resident alien) student enrollment
#' at 5-year and 10-year horizons. International students are a revenue source
#' and strategic enrollment segment.
#'
#' @param df Data frame with one row per institution-year
#' @param years Integer vector of available years
#' @param lookups List with intl vector (international enrollment by year)
#'
#' @return Data frame with new columns:
#'   - international_enrollment_change_10yr: absolute count change over 10 years
#'   - international_enrollment_pct_change_10yr: % change in international enrollment (10yr)
#'   - international_enrollment_increase_10yr: "Yes"/"No" flag
#'   - international_student_count_change_5yr: absolute count change (5yr)
#'   - international_enrollment_pct_change_5yr: % change (5yr)
#'   - international_enrollment_increase_5yr: "Yes"/"No" flag
#'
enrich_international_metrics <- function(df, years, lookups) {
  df %>%
    rowwise() %>%
    mutate(
      # 10-year absolute change in international student count
      international_enrollment_change_10yr     = enrollment_nonresident_total - unname(lookups$intl[as.character(year - 10)]),
      # 10-year % change in international enrollment
      international_enrollment_pct_change_10yr = safe_pct_change(enrollment_nonresident_total, unname(lookups$intl[as.character(year - 10)])),
      # 10-year growth flag for international students
      international_enrollment_increase_10yr   = case_when(
        is.na(unname(lookups$intl[as.character(year - 10)])) | is.na(enrollment_nonresident_total) ~ NA_character_,
        enrollment_nonresident_total > unname(lookups$intl[as.character(year - 10)]) ~ "Yes",
        TRUE ~ "No"
      ),
      # 5-year absolute change in international student count
      international_student_count_change_5yr   = enrollment_nonresident_total - unname(lookups$intl[as.character(year - 5)]),
      # 5-year % change in international enrollment
      international_enrollment_pct_change_5yr  = safe_pct_change(enrollment_nonresident_total, unname(lookups$intl[as.character(year - 5)])),
      # 5-year growth flag for international students
      international_enrollment_increase_5yr    = case_when(
        is.na(unname(lookups$intl[as.character(year - 5)])) | is.na(enrollment_nonresident_total) ~ NA_character_,
        enrollment_nonresident_total > unname(lookups$intl[as.character(year - 5)]) ~ "Yes",
        TRUE ~ "No"
      )
    ) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# Transfer-Out Rate Metrics
# ---------------------------------------------------------------------------

#' Enrich Transfer-Out Rate Metrics
#'
#' Calculates trends in bachelor's degree transfer-out rates (students who start
#' at this institution and complete degrees elsewhere). Higher rates may indicate
#' institutional instability or enrollment pressures.
#'
#' @param df Data frame with one row per institution-year
#' @param years Integer vector of available years
#' @param lookups List with transfer_out vector (bachelor transfer-out rates)
#'
#' @return Data frame with new columns:
#'   - transfer_out_rate_bachelor_change_5yr: percentage point change (5yr)
#'   - transfer_out_rate_bachelor_increase_5yr: "Yes"/"No" flag
#'   - transfer_out_rate_bachelor_change_10yr: percentage point change (10yr)
#'   - transfer_out_rate_bachelor_increase_10yr: "Yes"/"No" flag
#'
enrich_transfer_out_metrics <- function(df, years, lookups) {
  df %>%
    rowwise() %>%
    mutate(
      # 5-year percentage point change in transfer-out rate
      # (Note: simple difference, not %, since rate is already a percentage)
      transfer_out_rate_bachelor_change_5yr    = transfer_out_rate_bachelor - unname(lookups$transfer_out[as.character(year - 5)]),
      # 5-year growth flag: did transfer-out rate increase?
      transfer_out_rate_bachelor_increase_5yr  = case_when(
        is.na(unname(lookups$transfer_out[as.character(year - 5)])) | is.na(transfer_out_rate_bachelor) ~ NA_character_,
        transfer_out_rate_bachelor > unname(lookups$transfer_out[as.character(year - 5)]) ~ "Yes",
        TRUE ~ "No"
      ),
      # 10-year percentage point change in transfer-out rate
      transfer_out_rate_bachelor_change_10yr   = transfer_out_rate_bachelor - unname(lookups$transfer_out[as.character(year - 10)]),
      # 10-year growth flag: did transfer-out rate increase?
      transfer_out_rate_bachelor_increase_10yr = case_when(
        is.na(unname(lookups$transfer_out[as.character(year - 10)])) | is.na(transfer_out_rate_bachelor) ~ NA_character_,
        transfer_out_rate_bachelor > unname(lookups$transfer_out[as.character(year - 10)]) ~ "Yes",
        TRUE ~ "No"
      )
    ) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# Loan and Loss Metrics
# ---------------------------------------------------------------------------

#' Enrich Loan and Operating Loss Metrics
#'
#' Enriches federal loan participation data and operating loss history.
#' Uses helper function latest_non_null() to find most recent loan data,
#' and loss_frequency() to count years with negative margins.
#'
#' @param df Data frame with one row per institution-year
#' @param years Integer vector of available years
#' @param lookups List with loss, loss_adj vectors
#'
#' @return Data frame with new columns:
#'   - share_grad_students: % of enrollment that is graduate students
#'   - loan_year_latest: most recent year with federal loan data
#'   - loan_pct_undergrad_federal_latest: most recent % of undergrads taking loans
#'   - loan_count_undergrad_federal_latest: most recent count of undergrads with loans
#'   - loan_avg_undergrad_federal_latest: most recent average undergraduate loan amount
#'   - ended_2024_at_loss: "Yes"/"No" for 2024 operating loss
#'   - loss_amount_2024: absolute dollar loss (2024)
#'   - loss_amount_2024_adjusted: inflation-adjusted loss (2024)
#'   - loss_years_last_10: count of years with negative margin (past 10 years)
#'   - loss_years_last_5: count of years with negative margin (past 5 years)
#'
enrich_loss_loan_metrics <- function(df, years, lookups) {
  loss2024          <- unname(lookups$loss["2024"])
  loan_pct_latest   <- latest_non_null(df, "loan_pct_undergrad_federal")
  loan_count_latest <- latest_non_null(df, "loan_count_undergrad_federal")
  loan_avg_latest   <- latest_non_null(df, "loan_avg_undergrad_federal")

  df %>%
    rowwise() %>%
    mutate(
      # Graduate students as share of total enrollment
      share_grad_students                 = safe_divide(enrollment_headcount_graduate, enrollment_headcount_total),
      # Most recent year with any federal loan data
      loan_year_latest                    = dplyr::coalesce(loan_pct_latest$year, loan_count_latest$year, loan_avg_latest$year),
      # Most recent % of undergraduates taking federal loans
      loan_pct_undergrad_federal_latest   = loan_pct_latest$value,
      # Most recent count of undergraduates with federal loans
      loan_count_undergrad_federal_latest = loan_count_latest$value,
      # Most recent average federal loan amount per borrowing undergraduate
      loan_avg_undergrad_federal_latest   = loan_avg_latest$value,
      # Aliases for downstream compatibility
      federal_loan_pct_most_recent        = loan_pct_latest$value,
      federal_loan_count_most_recent      = loan_count_latest$value,
      federal_loan_avg_most_recent        = loan_avg_latest$value,
      # Did the institution end 2024 with a negative operating margin (loss)?
      ended_2024_at_loss                  = case_when(is.na(loss2024) ~ NA_character_, loss2024 < 0 ~ "Yes", TRUE ~ "No"),
      # Actual dollar loss in 2024 (negative number)
      loss_amount_2024                    = loss2024,
      # Inflation-adjusted loss in 2024 (to constant base year dollars)
      loss_amount_2024_adjusted           = unname(lookups$loss_adj["2024"]),
      # Count of years with operating loss in past 10 years
      loss_years_last_10                  = loss_frequency(years, unname(lookups$loss), year, 10),
      # Count of years with operating loss in past 5 years
      loss_years_last_5                   = loss_frequency(years, unname(lookups$loss), year, 5)
    ) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# Narrative Sentence Generation
# ---------------------------------------------------------------------------

#' Generate Narrative Sentences for Reports
#'
#' Builds human-readable sentences summarizing key financial and enrollment
#' metrics for institutional profiles and reports. These sentences appear in
#' institution-specific dashboards and PDF reports.
#'
#' @param df Data frame with one row per institution-year
#'
#' @return Data frame with new columns containing complete sentences:
#'   - international_students_sentence: % and breakdown of international enrollment
#'   - enrollment_change_sentence: 5-year enrollment trend statement
#'   - revenue_change_sentence: 5-year revenue trend statement
#'   - staffing_change_sentence: 5-year staff change statement
#'   - federal_grants_contracts_dependence_sentence: federal revenue reliance
#'   - state_funding_sentence: state appropriation dependency
#'
enrich_narrative_sentences <- function(df) {
  df %>%
    mutate(
      # Sentence describing international student enrollment and composition
      international_students_sentence = case_when(
        is.na(pct_international_all) ~ NA_character_,
        !is.na(pct_international_undergraduate) & !is.na(pct_international_graduate) ~ paste0("In ", year, ", ", round(pct_international_all * 100), "% of students were international. That includes ", round(pct_international_undergraduate * 100), "% of undergraduates and ", round(pct_international_graduate * 100), "% of graduate students."),
        TRUE ~ paste0("In ", year, ", ", round(pct_international_all * 100), "% of students were international.")
      ),
      # Sentence describing 5-year headcount enrollment trend
      enrollment_change_sentence = ifelse(is.na(enrollment_pct_change_5yr), NA_character_, paste0("12-month unduplicated headcount changed by ", round(enrollment_pct_change_5yr, 1), "% over the past five years.")),
      # Sentence describing 5-year total revenue trend
      revenue_change_sentence = ifelse(is.na(revenue_pct_change_5yr), NA_character_, paste0("Total revenue changed by ", round(revenue_pct_change_5yr, 1), "% over the past five years.")),
      # Sentence describing 5-year staff headcount trend
      staffing_change_sentence = ifelse(is.na(staff_total_headcount_pct_change_5yr), NA_character_, paste0("Total staff headcount changed by ", round(staff_total_headcount_pct_change_5yr, 1), "% over the past five years.")),
      # Sentence describing Pell-adjusted federal grants/contracts dependence
      federal_grants_contracts_dependence_sentence = ifelse(is.na(federal_grants_contracts_pell_adjusted_pct_core_revenue), NA_character_, paste0(round(federal_grants_contracts_pell_adjusted_pct_core_revenue * 100, 1), "% of core revenue came from Pell-adjusted federal grants and contracts in ", year, ".")),
      # Sentence describing state appropriation dependence
      state_funding_sentence = ifelse(is.na(state_funding_pct_core_revenue), NA_character_, paste0(round(state_funding_pct_core_revenue * 100, 1), "% of core revenue came from state appropriations."))
    )
}

# ---------------------------------------------------------------------------
# Per-Institution Enrichment Coordinator
# ---------------------------------------------------------------------------

#' Enrich All Metrics for One Institution's Time Series
#'
#' Master enrichment function that adds year-over-year and multi-year trend
#' metrics to a single institution's rows (grouped by unitid). Calls all
#' thematic enrichment sub-functions in sequence, building lookups once
#' and passing them through to avoid repeated data structure conversions.
#'
#' FLOW:
#'   1. Sort by year to ensure chronological order
#'   2. Build named lookup lists (year -> value) for all key metrics
#'   3. Call each enrichment function with lookups attached
#'   4. Generate narrative sentences
#'
#' @param df Data frame subset for ONE institution (one unitid), can be multiple years
#'
#' @return Data frame with all original columns plus enriched trend columns
#'   (enrollment changes, revenue metrics, tuition trends, funding analysis,
#'   endowment changes, international student trends, transfer-out rates,
#'   loan participation, operating losses, and narrative sentences)
#'
#' @details
#'   Called via group_modify() in the main pipeline:
#'     canonical_data %>%
#'       group_by(unitid) %>%
#'       group_modify(~enrich_group(.x))
#'
enrich_group <- function(df) {
  # Sort by year to ensure chronological order for lagging operations
  df <- df %>% arrange(year)
  years <- df$year

  # Build named lookup vectors for fast year-based value retrieval
  # Each list element maps year (as character) to a financial/enrollment metric
  # This avoids repeated setNames() calls inside row-level operations
  lookups <- list(
    # Enrollment lookups
    enroll           = stats::setNames(df$enrollment_headcount_total,                      years),
    enroll_fte       = stats::setNames(df$fte_12_months,                                   years),
    # Staffing lookups
    staff_fte        = stats::setNames(df$staff_fte_total,                                 years),
    staff_instr_fte  = stats::setNames(df$staff_fte_instructional,                         years),
    staff_head       = stats::setNames(df$staff_headcount_total,                           years),
    staff_instr_head = stats::setNames(df$staff_headcount_instructional,                   years),
    # Revenue lookups (nominal and adjusted for inflation)
    revenue          = stats::setNames(df$revenue_total,                                   years),
    revenue_adj      = stats::setNames(df$revenue_total_adjusted,                          years),
    # Operating performance lookup
    op_margin        = stats::setNames(df$operating_margin,                                years),
    # Net tuition lookups
    net_tuition      = stats::setNames(df$net_tuition_total,                               years),
    net_tuition_adj  = stats::setNames(df$net_tuition_total_adjusted,                      years),
    net_tuition_fte  = stats::setNames(df$net_tuition_per_fte,                             years),
    net_tuition_fte_adj = stats::setNames(df$net_tuition_per_fte_adjusted,                 years),
    # Admissions lookups
    yield            = stats::setNames(df$admissions_yield,                                years),
    discount         = stats::setNames(df$discount_rate,                                   years),
    # Government funding lookups
    govt             = stats::setNames(df$government_funding_total,                        years),
    govt_adj         = stats::setNames(df$government_funding_total_adjusted,               years),
    # Endowment lookups
    endowment        = stats::setNames(df$endowment_value,                                 years),
    endowment_adj    = stats::setNames(df$endowment_value_adjusted,                        years),
    # International student lookups
    intl             = stats::setNames(df$enrollment_nonresident_total,                    years),
    # Operating loss lookups
    loss             = stats::setNames(df$loss_amount,                                     years),
    loss_adj         = stats::setNames(df$loss_amount_adjusted,                            years),
    # Federal grants (Pell-adjusted) lookups
    federal_adj      = stats::setNames(df$federal_grants_contracts_pell_adjusted,          years),
    federal_adj_adj  = stats::setNames(df$federal_grants_contracts_pell_adjusted_adjusted, years),
    # State funding lookups
    state            = stats::setNames(df$state_funding,                                   years),
    state_adj        = stats::setNames(df$state_funding_adjusted,                          years),
    # Transfer-out rate lookups
    transfer_out     = stats::setNames(df$transfer_out_rate_bachelor,                      years)
  )

  # Chain all enrichment functions together, passing lookups through
  df |>
    enrich_enrollment_staffing(years, lookups) |>
    enrich_revenue_metrics(years, lookups) |>
    enrich_tuition_metrics(years, lookups) |>
    enrich_funding_metrics(years, lookups) |>
    enrich_endowment_metrics(years, lookups) |>
    enrich_international_metrics(years, lookups) |>
    enrich_transfer_out_metrics(years, lookups) |>
    enrich_loss_loan_metrics(years, lookups) |>
    enrich_narrative_sentences()
}
