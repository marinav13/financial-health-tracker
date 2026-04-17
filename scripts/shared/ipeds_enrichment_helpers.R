# ============================================================================
# scripts/shared/ipeds_enrichment_helpers.R
# ============================================================================
#
# Adds year-over-year (YoY) and multi-year (5-year, 10-year) trend metrics to
# a single institution's multi-year IPEDS data. Each function adds one thematic
# group of metrics (enrollment, revenue, tuition, etc.). Source AFTER utils.R.
# Use enrich_group() via group_modify() in the data pipeline.
#
# Key helpers: safe_pct_change() computes ((new - old) / old) * 100, handling
# NAs. All functions work on rowwise() data (one calculation per row).
#
# ============================================================================

# ---------------------------------------------------------------------------
# Enrollment and Staffing Enrichment
# ---------------------------------------------------------------------------

# Adds 5-year and 1-year enrollment/staffing metrics comparing current values
# to lagged years to measure growth or decline.
enrich_enrollment_staffing <- function(df, years, lookups) {
  df %>%
    rowwise() %>%
    mutate(
      enrollment_pct_change_5yr                    = safe_pct_change(enrollment_headcount_total, unname(lookups$enroll[as.character(year - 5)])),
      enrollment_decreased_5yr                     = case_when(is.na(enrollment_pct_change_5yr) ~ NA_character_, enrollment_pct_change_5yr < 0 ~ "Yes", TRUE ~ "No"),
      enroll_fte_pct_change_5yr                    = safe_pct_change(fte_12_months, unname(lookups$enroll_fte[as.character(year - 5)])),
      enrollment_decline_last_3_of_5               = ifelse(count_decline_years(years, unname(lookups$enroll), year - 5, year - 1, 0) >= 3, "Yes", "No"),
      enroll_fte_decline_last_3_of_5               = ifelse(count_decline_years(years, unname(lookups$enroll_fte), year - 5, year - 1, 0) >= 3, "Yes", "No"),
      staff_total_pct_change_5yr                   = safe_pct_change(staff_fte_total, unname(lookups$staff_fte[as.character(year - 5)])),
      staff_instructional_fte_pct_change_5yr       = safe_pct_change(staff_fte_instructional, unname(lookups$staff_instr_fte[as.character(year - 5)])),
      staff_total_headcount_pct_change_5yr         = safe_pct_change(staff_headcount_total, unname(lookups$staff_head[as.character(year - 5)])),
      staff_instructional_headcount_pct_change_5yr = safe_pct_change(staff_headcount_instructional, unname(lookups$staff_instr_head[as.character(year - 5)])),
      enrollment_change_1yr                        = safe_pct_change(enrollment_headcount_total, unname(lookups$enroll[as.character(year - 1)])),
      staff_change_1yr                             = safe_pct_change(staff_headcount_total, unname(lookups$staff_head[as.character(year - 1)]))
    ) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# Revenue Metrics Enrichment
# ---------------------------------------------------------------------------

# Adds 5-year and 1-year revenue metrics including nominal and inflation-adjusted
# versions, plus flags for significant revenue declines.
enrich_revenue_metrics <- function(df, years, lookups) {
  df %>%
    rowwise() %>%
    mutate(
      revenue_pct_change_5yr_nominal    = safe_pct_change(revenue_total, unname(lookups$revenue[as.character(year - 5)])),
      revenue_pct_change_5yr            = safe_pct_change(revenue_total_adjusted, unname(lookups$revenue_adj[as.character(year - 5)])),
      revenue_pct_change_5yr_adjusted   = revenue_pct_change_5yr,
      revenue_decreased_5yr             = case_when(is.na(revenue_pct_change_5yr) ~ NA_character_, revenue_pct_change_5yr < 0 ~ "Yes", TRUE ~ "No"),
      revenue_change_1yr_nominal        = safe_pct_change(revenue_total, unname(lookups$revenue[as.character(year - 1)])),
      revenue_change_1yr                = safe_pct_change(revenue_total_adjusted, unname(lookups$revenue_adj[as.character(year - 1)])),
      revenue_10pct_drop_last_3_of_5    = ifelse(count_decline_years(years, unname(lookups$revenue_adj), year - 5, year - 1, -10) >= 3, "Yes", "No"),
      losses_last_3_of_5                = ifelse(count_negative_years(years, unname(lookups$op_margin), (year - 4):year, 0) >= 3, "Yes", "No")
    ) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# Tuition and Net Revenue Metrics Enrichment
# ---------------------------------------------------------------------------

# Adds 5-year trends in net tuition revenue, per-student tuition, admissions
# yield, and discount rates. Includes both nominal and inflation-adjusted versions.
enrich_tuition_metrics <- function(df, years, lookups) {
  df %>%
    rowwise() %>%
    mutate(
      net_tuition_pct_change_5yr_nominal       = safe_pct_change(net_tuition_total, unname(lookups$net_tuition[as.character(year - 5)])),
      net_tuition_pct_change_5yr               = safe_pct_change(net_tuition_total_adjusted, unname(lookups$net_tuition_adj[as.character(year - 5)])),
      net_tuition_pct_change_5yr_adjusted      = net_tuition_pct_change_5yr,
      net_tuition_per_fte_change_5yr_nominal   = safe_pct_change(net_tuition_per_fte, unname(lookups$net_tuition_fte[as.character(year - 5)])),
      net_tuition_per_fte_change_5yr           = safe_pct_change(net_tuition_per_fte_adjusted, unname(lookups$net_tuition_fte_adj[as.character(year - 5)])),
      net_tuition_per_fte_change_5yr_adjusted  = net_tuition_per_fte_change_5yr,
      yield_pct_change_5yr                     = safe_pct_change(admissions_yield, unname(lookups$yield[as.character(year - 5)])),
      discount_pct_change_5yr                  = safe_pct_change(discount_rate, unname(lookups$discount[as.character(year - 5)]))
    ) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# Government and Philanthropic Funding Enrichment
# ---------------------------------------------------------------------------

# Adds 5-year trends in federal grants/contracts, Pell-adjusted federal support,
# and state appropriations. Tracks both total funding and funding as % of core revenue.
enrich_funding_metrics <- function(df, years, lookups) {
  df %>%
    rowwise() %>%
    mutate(
      government_funding_pct_change_5yr_nominal                        = safe_pct_change(government_funding_total, unname(lookups$govt[as.character(year - 5)])),
      government_funding_pct_change_5yr                                = safe_pct_change(government_funding_total_adjusted, unname(lookups$govt_adj[as.character(year - 5)])),
      government_funding_pct_change_5yr_adjusted                       = government_funding_pct_change_5yr,
      federal_grants_contracts_pell_adjusted_pct_change_5yr_nominal    = safe_pct_change(federal_grants_contracts_pell_adjusted, unname(lookups$federal_adj[as.character(year - 5)])),
      federal_grants_contracts_pell_adjusted_pct_change_5yr            = safe_pct_change(federal_grants_contracts_pell_adjusted_adjusted, unname(lookups$federal_adj_adj[as.character(year - 5)])),
      federal_grants_contracts_pell_adjusted_pct_change_5yr_adjusted   = federal_grants_contracts_pell_adjusted_pct_change_5yr,
      state_funding_pct_change_5yr_nominal                             = safe_pct_change(state_funding, unname(lookups$state[as.character(year - 5)])),
      state_funding_pct_change_5yr                                     = safe_pct_change(state_funding_adjusted, unname(lookups$state_adj[as.character(year - 5)])),
      state_funding_pct_change_5yr_adjusted                            = state_funding_pct_change_5yr
    ) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# Endowment Metrics Enrichment
# ---------------------------------------------------------------------------

# Adds 5-year trends in endowment market value, both nominal and inflation-adjusted.
enrich_endowment_metrics <- function(df, years, lookups) {
  df %>%
    rowwise() %>%
    mutate(
      endowment_pct_change_5yr_nominal   = safe_pct_change(endowment_value, unname(lookups$endowment[as.character(year - 5)])),
      endowment_pct_change_5yr           = safe_pct_change(endowment_value_adjusted, unname(lookups$endowment_adj[as.character(year - 5)])),
      endowment_pct_change_5yr_adjusted  = endowment_pct_change_5yr
    ) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# International Student Enrollment Metrics
# ---------------------------------------------------------------------------

# Adds 5-year and 10-year trends in international student enrollment.
enrich_international_metrics <- function(df, years, lookups) {
  df %>%
    rowwise() %>%
    mutate(
      international_enrollment_change_10yr     = enrollment_nonresident_total - unname(lookups$intl[as.character(year - 10)]),
      international_enrollment_pct_change_10yr = safe_pct_change(enrollment_nonresident_total, unname(lookups$intl[as.character(year - 10)])),
      international_enrollment_increase_10yr   = case_when(
        is.na(unname(lookups$intl[as.character(year - 10)])) | is.na(enrollment_nonresident_total) ~ NA_character_,
        enrollment_nonresident_total > unname(lookups$intl[as.character(year - 10)]) ~ "Yes",
        TRUE ~ "No"
      ),
      international_student_count_change_5yr   = enrollment_nonresident_total - unname(lookups$intl[as.character(year - 5)]),
      international_enrollment_pct_change_5yr  = safe_pct_change(enrollment_nonresident_total, unname(lookups$intl[as.character(year - 5)])),
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

# Adds 5-year and 10-year trends in bachelor's degree transfer-out rates
# (students who start at this institution and complete elsewhere).
enrich_transfer_out_metrics <- function(df, years, lookups) {
  df %>%
    rowwise() %>%
    mutate(
      transfer_out_rate_bachelor_change_5yr    = transfer_out_rate_bachelor - unname(lookups$transfer_out[as.character(year - 5)]),
      transfer_out_rate_bachelor_increase_5yr  = case_when(
        is.na(unname(lookups$transfer_out[as.character(year - 5)])) | is.na(transfer_out_rate_bachelor) ~ NA_character_,
        transfer_out_rate_bachelor > unname(lookups$transfer_out[as.character(year - 5)]) ~ "Yes",
        TRUE ~ "No"
      ),
      transfer_out_rate_bachelor_change_10yr   = transfer_out_rate_bachelor - unname(lookups$transfer_out[as.character(year - 10)]),
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

# Adds federal loan participation data and operating loss history, including
# most recent loan metrics and counts of years with negative margins.
enrich_loss_loan_metrics <- function(df, years, lookups) {
  loss2024          <- unname(lookups$loss["2024"])
  loan_pct_latest   <- latest_non_null(df, "loan_pct_undergrad_federal")
  loan_count_latest <- latest_non_null(df, "loan_count_undergrad_federal")
  loan_avg_latest   <- latest_non_null(df, "loan_avg_undergrad_federal")

  df %>%
    rowwise() %>%
    mutate(
      share_grad_students                 = safe_divide(enrollment_headcount_graduate, enrollment_headcount_total),
      loan_year_latest                    = dplyr::coalesce(loan_pct_latest$year, loan_count_latest$year, loan_avg_latest$year),
      loan_pct_undergrad_federal_latest   = loan_pct_latest$value,
      loan_count_undergrad_federal_latest = loan_count_latest$value,
      loan_avg_undergrad_federal_latest   = loan_avg_latest$value,
      federal_loan_pct_most_recent        = loan_pct_latest$value,
      federal_loan_count_most_recent      = loan_count_latest$value,
      federal_loan_avg_most_recent        = loan_avg_latest$value,
      ended_2024_at_loss                  = case_when(is.na(loss2024) ~ NA_character_, loss2024 < 0 ~ "Yes", TRUE ~ "No"),
      loss_amount_2024                    = loss2024,
      loss_amount_2024_adjusted           = unname(lookups$loss_adj["2024"]),
      loss_years_last_10                  = loss_frequency(years, unname(lookups$loss), year, 10),
      loss_years_last_5                   = loss_frequency(years, unname(lookups$loss), year, 5)
    ) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# Narrative Sentence Generation
# ---------------------------------------------------------------------------

# Adds human-readable sentences summarizing key financial and enrollment
# metrics for dashboards and PDF reports.
enrich_narrative_sentences <- function(df) {
  df %>%
    mutate(
      international_students_sentence = case_when(
        is.na(pct_international_all) ~ NA_character_,
        !is.na(pct_international_undergraduate) & !is.na(pct_international_graduate) ~ paste0("In ", year, ", ", round(pct_international_all * 100), "% of students were international. That includes ", round(pct_international_undergraduate * 100), "% of undergraduates and ", round(pct_international_graduate * 100), "% of graduate students."),
        TRUE ~ paste0("In ", year, ", ", round(pct_international_all * 100), "% of students were international.")
      ),
      enrollment_change_sentence = ifelse(is.na(enrollment_pct_change_5yr), NA_character_, paste0("12-month unduplicated headcount changed by ", round(enrollment_pct_change_5yr, 1), "% over the past five years.")),
      revenue_change_sentence = ifelse(is.na(revenue_pct_change_5yr), NA_character_, paste0("Total revenue changed by ", round(revenue_pct_change_5yr, 1), "% over the past five years.")),
      staffing_change_sentence = ifelse(is.na(staff_total_headcount_pct_change_5yr), NA_character_, paste0("Total staff headcount changed by ", round(staff_total_headcount_pct_change_5yr, 1), "% over the past five years.")),
      federal_grants_contracts_dependence_sentence = ifelse(is.na(federal_grants_contracts_pell_adjusted_pct_core_revenue), NA_character_, paste0(round(federal_grants_contracts_pell_adjusted_pct_core_revenue * 100, 1), "% of core revenue came from Pell-adjusted federal grants and contracts in ", year, ".")),
      state_funding_sentence = ifelse(is.na(state_funding_pct_core_revenue), NA_character_, paste0(round(state_funding_pct_core_revenue * 100, 1), "% of core revenue came from state appropriations."))
    )
}

# ---------------------------------------------------------------------------
# Per-Institution Enrichment Coordinator
# ---------------------------------------------------------------------------

# Master enrichment function that adds all trend metrics to a single
# institution's rows. Builds lookups once, then chains all enrichment
# functions together. Called via group_modify() in the main pipeline.
enrich_group <- function(df) {
  df <- df %>% arrange(year)
  years <- df$year

  lookups <- list(
    enroll           = stats::setNames(df$enrollment_headcount_total,                      years),
    enroll_fte       = stats::setNames(df$fte_12_months,                                   years),
    staff_fte        = stats::setNames(df$staff_fte_total,                                 years),
    staff_instr_fte  = stats::setNames(df$staff_fte_instructional,                         years),
    staff_head       = stats::setNames(df$staff_headcount_total,                           years),
    staff_instr_head = stats::setNames(df$staff_headcount_instructional,                   years),
    revenue          = stats::setNames(df$revenue_total,                                   years),
    revenue_adj      = stats::setNames(df$revenue_total_adjusted,                          years),
    op_margin        = stats::setNames(df$operating_margin,                                years),
    net_tuition      = stats::setNames(df$net_tuition_total,                               years),
    net_tuition_adj  = stats::setNames(df$net_tuition_total_adjusted,                      years),
    net_tuition_fte  = stats::setNames(df$net_tuition_per_fte,                             years),
    net_tuition_fte_adj = stats::setNames(df$net_tuition_per_fte_adjusted,                 years),
    yield            = stats::setNames(df$admissions_yield,                                years),
    discount         = stats::setNames(df$discount_rate,                                   years),
    govt             = stats::setNames(df$government_funding_total,                        years),
    govt_adj         = stats::setNames(df$government_funding_total_adjusted,               years),
    endowment        = stats::setNames(df$endowment_value,                                 years),
    endowment_adj    = stats::setNames(df$endowment_value_adjusted,                        years),
    intl             = stats::setNames(df$enrollment_nonresident_total,                    years),
    loss             = stats::setNames(df$loss_amount,                                     years),
    loss_adj         = stats::setNames(df$loss_amount_adjusted,                            years),
    federal_adj      = stats::setNames(df$federal_grants_contracts_pell_adjusted,          years),
    federal_adj_adj  = stats::setNames(df$federal_grants_contracts_pell_adjusted_adjusted, years),
    state            = stats::setNames(df$state_funding,                                   years),
    state_adj        = stats::setNames(df$state_funding_adjusted,                          years),
    transfer_out     = stats::setNames(df$transfer_out_rate_bachelor,                      years)
  )

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
