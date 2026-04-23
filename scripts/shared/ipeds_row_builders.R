# scripts/shared/ipeds_row_builders.R
#
# Canonical row-builder and shared IPEDS support helpers extracted from ipeds_helpers.R.
# Source this after utils.R and before scripts that call build_canonical_ipeds_row().

# ---------------------------------------------------------------------------
# Numeric / financial helpers
# ---------------------------------------------------------------------------

# Converts negative spend values to positive (IPEDS sometimes reports expenses
# as negative numbers in certain FASB table layouts).
as_positive_spend <- function(x) {
  ifelse(is.na(x), NA_real_, abs(x))
}

# Replaces NA with 0 (useful when summing across optional fields).
zero_if_null <- function(x) {
  ifelse(is.na(x), 0, x)
}

# Returns the sum of `values`, or NA when every element is NA.
sum_if_any <- function(values) {
  if (all(is.na(values))) return(NA_real_)
  sum(values, na.rm = TRUE)
}

# Selects a single-row numeric value from the GASB, FASB, or PFP column
# depending on the institution's reporting model.  `gasb_col`, `fasb_col`, and
# `pfp_col` are column-name strings; the matching branch returns to_num(value).
pick_finance_col <- function(row, gasb_col, fasb_col, pfp_col,
                              uses_gasb_finance, uses_fasb_finance, control_label) {
  if (uses_gasb_finance) {
    to_num(row[[gasb_col]][[1]])
  } else if (uses_fasb_finance) {
    to_num(row[[fasb_col]][[1]])
  } else if (identical(control_label, "Private for-profit")) {
    to_num(row[[pfp_col]][[1]])
  } else {
    NA_real_
  }
}

# Coalesces the first non-NA numeric value found across `field_names` in `row`,
# returning NA when every named column is absent or NA.  Handles optional
# columns that may not exist in every year's raw extract.
coalesce_row_fields <- function(row, field_names) {
  vals <- lapply(field_names, function(nm) {
    if (nm %in% names(row)) to_num(row[[nm]][[1]]) else NA_real_
  })
  do.call(dplyr::coalesce, vals)
}

# Returns the first scalar value from a one-row data frame, or `default` when
# the column is absent/empty. This keeps the row-builder helpers readable.
row_value <- function(row, col, default = NA) {
  if (!(col %in% names(row)) || length(row[[col]]) == 0) {
    return(default)
  }
  row[[col]][[1]]
}

# ---------------------------------------------------------------------------
# Canonical row-builder helpers
# ---------------------------------------------------------------------------

make_ipeds_row_context <- function(row, decode_lookups) {
  control_label <- get_control_label(row_value(row, "control"))
  reporting_model_code <- trimws(as.character(row_value(row, "reporting_model") %||% ""))
  uses_fasb_finance <- identical(control_label, "Private not-for-profit") || identical(reporting_model_code, "2")
  uses_gasb_finance <- identical(control_label, "Public") && !uses_fasb_finance

  list(
    unitid = as.character(row_value(row, "unitid")),
    year = as.integer(row_value(row, "year")),
    control_label = control_label,
    sector_decoded = decode_value(row_value(row, "sector"), decode_lookups$hd_sector_lookup),
    level_decoded = decode_value(row_value(row, "level"), decode_lookups$hd_level_lookup),
    status_decoded = decode_value(row_value(row, "status"), decode_lookups$hd_act_lookup),
    is_active_decoded = decode_value(row_value(row, "is_active"), decode_lookups$hd_active_lookup),
    state_full = get_state_name(row_value(row, "state")),
    fte12 = to_num(row_value(row, "fte_12_months")),
    uses_fasb_finance = uses_fasb_finance,
    uses_gasb_finance = uses_gasb_finance
  )
}

keep_ipeds_canonical_row <- function(context) {
  if (!(context$control_label %in% c("Public", "Private not-for-profit", "Private for-profit"))) {
    return(FALSE)
  }
  if (!identical(context$level_decoded, "Four or more years")) {
    return(FALSE)
  }
  if (!is.na(context$sector_decoded) && grepl("^Administrative Unit", context$sector_decoded)) {
    return(FALSE)
  }
  if (!is.na(context$is_active_decoded) && !(context$is_active_decoded %in% c("Yes", "Imputed as active"))) {
    return(FALSE)
  }
  TRUE
}

build_identity_fields <- function(row, context, decode_lookups) {
  list(
    unitid = context$unitid,
    institution_name = dplyr::coalesce(row_value(row, "institution_name_latest"), row_value(row, "institution_name")),
    institution_unique_name = dplyr::coalesce(
      row_value(row, "institution_unique_name_latest"),
      paste(na.omit(c(row_value(row, "institution_name"), row_value(row, "city"), context$state_full)), collapse = " | ")
    ),
    year = context$year,
    control_label = context$control_label,
    state = dplyr::coalesce(row_value(row, "state_latest"), context$state_full),
    city = dplyr::coalesce(row_value(row, "city_latest"), row_value(row, "city")),
    urbanization = decode_value(row_value(row, "urbanization"), decode_lookups$hd_locale_lookup),
    sector = context$sector_decoded,
    level = context$level_decoded,
    category = decode_value(row_value(row, "category"), decode_lookups$hd_category_lookup),
    institution_status = context$status_decoded,
    is_active = context$is_active_decoded,
    hbcu = decode_value(row_value(row, "hbcu"), decode_lookups$hd_hbcu_lookup),
    tribal_college = decode_value(row_value(row, "tribal_college"), decode_lookups$hd_tribal_lookup),
    grad_offering = decode_value(row_value(row, "grad_offering"), decode_lookups$hd_grad_offering_lookup),
    reporting_model = decode_value(row_value(row, "reporting_model"), decode_lookups$flags_form_lookup),
    access_earnings = decode_value(row_value(row, "access_earnings"), decode_lookups$hd_access_lookup),
    size = decode_value(row_value(row, "size"), decode_lookups$hd_size_lookup),
    grad_program_mix = decode_value(row_value(row, "grad_program_mix"), decode_lookups$hd_grad_mix_lookup),
    undergrad_program_mix = decode_value(row_value(row, "undergrad_program_mix"), decode_lookups$hd_ug_mix_lookup),
    religious_affiliation = decode_value(row_value(row, "religious_affiliation"), decode_lookups$ic_religious_affiliation_lookup),
    all_programs_distance_education = decode_yes_no_field(row_value(row, "all_programs_distance_education"))
  )
}

calculate_ipeds_finance_components <- function(row, context) {
  revenue <- pick_finance_col(
    row,
    "total_operating_nonoperating_revenues_gasb",
    "total_revenues_investment_return_fasb",
    "total_revenues_investment_return_pfp",
    context$uses_gasb_finance, context$uses_fasb_finance, context$control_label
  )
  expenses <- pick_finance_col(
    row,
    "total_expenses_deductions_current_total_gasb",
    "total_expenses_fasb",
    "total_expenses_total_amount_pfp",
    context$uses_gasb_finance, context$uses_fasb_finance, context$control_label
  )
  net_tuition_total <- if (context$uses_gasb_finance) {
    to_num(row_value(row, "tuition_fees_after_discounts_allowances_gasb"))
  } else if (context$uses_fasb_finance) {
    (to_num(row_value(row, "tuition_and_fees_fasb")) +
      zero_if_null(to_num(row_value(row, "allowances_applied_to_tuition_fasb")))) -
      (zero_if_null(to_num(row_value(row, "institutional_grants_funded_fasb"))) +
         zero_if_null(to_num(row_value(row, "institutional_grants_unfunded_fasb"))))
  } else if (identical(context$control_label, "Private for-profit")) {
    (to_num(row_value(row, "tuition_fees_pfp")) +
      zero_if_null(to_num(row_value(row, "discounts_allowances_applied_tuition_fees_pfp")))) -
      zero_if_null(to_num(row_value(row, "institutional_grants_pfp")))
  } else {
    NA_real_
  }
  federal_funding <- pick_finance_col(
    row,
    "federal_operating_grants_contracts_gasb",
    "federal_grants_contracts_fasb",
    "federal_grants_contracts_pfp",
    context$uses_gasb_finance, context$uses_fasb_finance, context$control_label
  )
  state_funding <- pick_finance_col(
    row,
    "state_appropriations_gasb",
    "state_approps_fasb",
    "state_appropriations_pfp",
    context$uses_gasb_finance, context$uses_fasb_finance, context$control_label
  )
  assets <- to_num(row_value(row, "assets"))
  liabilities <- to_num(row_value(row, "liabilities"))
  unrestricted_assets <- if (context$uses_gasb_finance) {
    to_num(row_value(row, "unrestricted_public"))
  } else if (context$uses_fasb_finance) {
    to_num(row_value(row, "total_unrestricted_net_assets_fasb"))
  } else {
    NA_real_
  }
  gross_tuition <- if (context$uses_gasb_finance) {
    zero_if_null(to_num(row_value(row, "discounts_allowances_applied_tuition_fees_gasb"))) +
      zero_if_null(to_num(row_value(row, "tuition_fees_after_discounts_allowances_gasb")))
  } else if (context$uses_fasb_finance) {
    zero_if_null(to_num(row_value(row, "tuition_and_fees_fasb"))) +
      zero_if_null(to_num(row_value(row, "allowances_applied_to_tuition_fasb")))
  } else {
    NA_real_
  }
  discount_rate <- if (context$uses_gasb_finance) {
    safe_divide(to_num(row_value(row, "discounts_allowances_applied_tuition_fees_gasb")), gross_tuition)
  } else if (context$uses_fasb_finance) {
    safe_divide(to_num(row_value(row, "institutional_grants_unfunded_fasb")), gross_tuition)
  } else {
    NA_real_
  }
  endowment_value <- if (context$uses_gasb_finance) {
    dplyr::coalesce(
      to_num(row_value(row, "value_endowment_assets_end_gasb")),
      if ("F1H02" %in% names(row)) to_num(row_value(row, "F1H02")) else NA_real_,
      to_num(row_value(row, "F1H01"))
    )
  } else if (context$uses_fasb_finance) {
    dplyr::coalesce(
      to_num(row_value(row, "value_endowment_end_fasb")),
      if ("F2H02" %in% names(row)) to_num(row_value(row, "F2H02")) else NA_real_,
      to_num(row_value(row, "F2H01"))
    )
  } else {
    NA_real_
  }
  government_funding <- if (is.na(federal_funding) && is.na(state_funding)) {
    NA_real_
  } else {
    zero_if_null(federal_funding) + zero_if_null(state_funding)
  }
  research_expense <- coalesce_row_fields(row, c(
    "research_expenses_total_gasb", "research_expenses_total_fasb", "research_expenses_total_pfp",
    "research_expense", "F1C021", "F2E021", "F3E02A1"
  ))
  core_expenses <- coalesce_row_fields(row, c(
    "core_expenses_gasb", "core_expenses_fasb", "core_expenses_pfp",
    "core_expenses", "F1COREXP", "F2COREXP", "F3COREXP"
  ))
  core_revenue <- dplyr::coalesce(
    to_num(row_value(row, "core_revenue")),
    if (context$uses_gasb_finance) {
      total_rev <- to_num(row_value(row, "total_operating_nonoperating_revenues_gasb"))
      aux_rev <- to_num(row_value(row, "auxiliary_enterprises_revenue_gasb"))
      hosp_rev <- to_num(row_value(row, "hospital_services_revenue_gasb"))
      indep_rev <- to_num(row_value(row, "independent_operations_revenue_gasb"))
      if (is.na(total_rev)) NA_real_ else total_rev - zero_if_null(aux_rev) - zero_if_null(hosp_rev) - zero_if_null(indep_rev)
    } else if (context$uses_fasb_finance) {
      total_rev <- to_num(row_value(row, "total_revenues_investment_return_fasb"))
      aux_rev <- to_num(row_value(row, "auxiliary_enterprises_revenue_fasb"))
      hosp_rev <- to_num(row_value(row, "hospital_revenue_fasb"))
      indep_rev <- to_num(row_value(row, "independent_operations_revenue_fasb"))
      if (is.na(total_rev)) NA_real_ else total_rev - zero_if_null(aux_rev) - zero_if_null(hosp_rev) - zero_if_null(indep_rev)
    } else {
      NA_real_
    }
  )
  gov_grants_fasb <- dplyr::coalesce(
    to_num(row_value(row, "gov_grants_fasb")),
    if (context$uses_fasb_finance) {
      safe_divide(
        sum_if_any(c(
          to_num(row_value(row, "federal_grants_contracts_fasb")),
          to_num(row_value(row, "state_grants_contracts_fasb")),
          to_num(row_value(row, "local_grants_contracts_fasb"))
        )),
        core_revenue
      )
    } else {
      NA_real_
    }
  )
  state_approps_percent_core_gasb <- dplyr::coalesce(
    to_num(row_value(row, "state_approps_percent_core_gasb")),
    if (context$uses_gasb_finance) safe_divide(to_num(row_value(row, "state_appropriations_gasb")), core_revenue) else NA_real_
  )
  state_revenue_fte_fasb <- dplyr::coalesce(
    to_num(row_value(row, "state_revenue_fte_fasb")),
    if (context$uses_fasb_finance) safe_divide(to_num(row_value(row, "state_approps_fasb")), context$fte12) else NA_real_
  )
  gov_revenue_fte_fasb <- dplyr::coalesce(
    to_num(row_value(row, "gov_revenue_fte_fasb")),
    if (context$uses_fasb_finance) {
      safe_divide(
        sum_if_any(c(
          to_num(row_value(row, "federal_grants_contracts_fasb")),
          to_num(row_value(row, "state_grants_contracts_fasb")),
          to_num(row_value(row, "local_grants_contracts_fasb"))
        )),
        context$fte12
      )
    } else {
      NA_real_
    }
  )
  loss_amount <- if (is.na(revenue) || is.na(expenses)) NA_real_ else revenue - expenses
  pell_accounting_method <- trimws(as.character(row_value(row, "pell_accounting_method") %||% ""))
  pell_grants <- to_num(row_value(row, "pell_grants"))
  federal_adj <- if (context$uses_gasb_finance) {
    federal_funding
  } else if (context$uses_fasb_finance) {
    if (is.na(federal_funding)) {
      NA_real_
    } else if (identical(pell_accounting_method, "2")) {
      pmax(federal_funding - zero_if_null(pell_grants), 0)
    } else {
      federal_funding
    }
  } else if (identical(context$control_label, "Private for-profit")) {
    federal_funding
  } else {
    NA_real_
  }
  endow_spend_raw <- if (context$uses_gasb_finance) {
    as_positive_spend(to_num(row_value(row, "endowment_spending_distribution_current_use_gasb")))
  } else if (context$uses_fasb_finance) {
    as_positive_spend(to_num(row_value(row, "spending_distribution_for_current_use_fasb")))
  } else {
    NA_real_
  }
  endowment_assets_per_fte_gasb <- to_num(row_value(row, "endowment_assets_per_fte_gasb"))
  endowment_assets_per_fte_fasb <- to_num(row_value(row, "endowment_assets_per_fte_fasb"))

  list(
    revenue = revenue,
    expenses = expenses,
    net_tuition_total = net_tuition_total,
    federal_funding = federal_funding,
    state_funding = state_funding,
    assets = assets,
    liabilities = liabilities,
    unrestricted_assets = unrestricted_assets,
    discount_rate = discount_rate,
    endowment_value = endowment_value,
    government_funding = government_funding,
    research_expense = research_expense,
    core_expenses = core_expenses,
    core_revenue = core_revenue,
    gov_grants_fasb = gov_grants_fasb,
    state_approps_percent_core_gasb = state_approps_percent_core_gasb,
    state_revenue_fte_fasb = state_revenue_fte_fasb,
    gov_revenue_fte_fasb = gov_revenue_fte_fasb,
    loss_amount = loss_amount,
    federal_adj = federal_adj,
    endow_spend_raw = endow_spend_raw,
    revenue_adjusted = inflate_to_base_year(revenue, context$year),
    expenses_adjusted = inflate_to_base_year(expenses, context$year),
    loss_amount_adjusted = inflate_to_base_year(loss_amount, context$year),
    net_tuition_total_adjusted = inflate_to_base_year(net_tuition_total, context$year),
    federal_funding_adjusted = inflate_to_base_year(federal_funding, context$year),
    federal_adj_adjusted = inflate_to_base_year(federal_adj, context$year),
    state_funding_adjusted = inflate_to_base_year(state_funding, context$year),
    government_funding_adjusted = inflate_to_base_year(government_funding, context$year),
    core_revenue_adjusted = inflate_to_base_year(core_revenue, context$year),
    endowment_value_adjusted = inflate_to_base_year(endowment_value, context$year),
    assets_adjusted = inflate_to_base_year(assets, context$year),
    liabilities_adjusted = inflate_to_base_year(liabilities, context$year),
    endowment_assets_per_fte_gasb = endowment_assets_per_fte_gasb,
    endowment_assets_per_fte_fasb = endowment_assets_per_fte_fasb,
    endowment_assets_per_fte = ifelse(
      identical(context$control_label, "Public"),
      endowment_assets_per_fte_gasb,
      endowment_assets_per_fte_fasb
    ),
    gov_grants_contracts_pct_core_revenue_gasb = to_num(row_value(row, "gov_grants_contracts_pct_core_revenue_gasb")),
    gov_grants_contracts_pct_core_revenue_fasb = to_num(row_value(row, "gov_grants_contracts_pct_core_revenue_fasb")),
    state_appropriations_pct_core_revenue_gasb = to_num(row_value(row, "state_appropriations_pct_core_revenue_gasb"))
  )
}

build_enrollment_fields <- function(row, context) {
  enrollment_headcount_total <- to_num(row_value(row, "enrollment_headcount_total"))
  enrollment_headcount_undergrad <- to_num(row_value(row, "enrollment_headcount_undergrad"))
  enrollment_headcount_graduate <- to_num(row_value(row, "enrollment_headcount_graduate"))
  enrollment_nonresident_total <- to_num(row_value(row, "enrollment_nonresident_total"))
  enrollment_nonresident_undergrad <- to_num(row_value(row, "enrollment_nonresident_undergrad"))
  enrollment_nonresident_graduate <- to_num(row_value(row, "enrollment_nonresident_graduate"))
  staff_fte_instructional <- to_num(row_value(row, "fte_instructional"))

  list(
    fte_12_months = context$fte12,
    fte_undergrad = to_num(row_value(row, "fte_undergrad")),
    fte_graduate = to_num(row_value(row, "fte_graduate")),
    enrollment_headcount_total = enrollment_headcount_total,
    enrollment_headcount_undergrad = enrollment_headcount_undergrad,
    enrollment_headcount_graduate = enrollment_headcount_graduate,
    enrollment_nonresident_total = enrollment_nonresident_total,
    enrollment_nonresident_undergrad = enrollment_nonresident_undergrad,
    enrollment_nonresident_graduate = enrollment_nonresident_graduate,
    staff_fte_total = to_num(row_value(row, "fte_total_staff")),
    staff_fte_instructional = staff_fte_instructional,
    students_per_instructional_staff_fte = safe_divide(context$fte12, staff_fte_instructional),
    transfer_out_rate_bachelor = to_num(row_value(row, "transfer_out_rate_bachelor")),
    staff_headcount_total = to_num(row_value(row, "staff_headcount_total")),
    staff_headcount_instructional = to_num(row_value(row, "staff_headcount_instructional")),
    loan_pct_undergrad_federal = to_num(row_value(row, "loan_pct_undergrad_federal")),
    loan_avg_undergrad_federal = to_num(row_value(row, "loan_avg_undergrad_federal")),
    loan_count_undergrad_federal = to_num(row_value(row, "loan_count_undergrad_federal")),
    pct_international_all = safe_divide(enrollment_nonresident_total, enrollment_headcount_total),
    pct_international_undergraduate = safe_divide(enrollment_nonresident_undergrad, enrollment_headcount_undergrad),
    pct_international_graduate = safe_divide(enrollment_nonresident_graduate, enrollment_headcount_graduate)
  )
}

build_finance_fields <- function(row, context, finance) {
  list(
    research_expense = finance$research_expense,
    research_expense_per_fte = safe_divide(finance$research_expense, context$fte12),
    research_expense_pct_core_expenses = safe_divide(finance$research_expense, finance$core_expenses),
    revenue_total = finance$revenue,
    revenue_total_adjusted = finance$revenue_adjusted,
    expenses_total = finance$expenses,
    expenses_total_adjusted = finance$expenses_adjusted,
    net_tuition_total = finance$net_tuition_total,
    net_tuition_total_adjusted = finance$net_tuition_total_adjusted,
    net_tuition_per_fte = safe_divide(finance$net_tuition_total, context$fte12),
    net_tuition_per_fte_adjusted = safe_divide(finance$net_tuition_total_adjusted, context$fte12),
    admissions_yield = to_num(row_value(row, "admissions_yield")),
    federal_funding = finance$federal_funding,
    federal_funding_adjusted = finance$federal_funding_adjusted,
    federal_grants_contracts_pell_adjusted = finance$federal_adj,
    federal_grants_contracts_pell_adjusted_adjusted = finance$federal_adj_adjusted,
    federal_grants_contracts_pell_adjusted_pct_core_revenue = safe_divide(finance$federal_adj, finance$core_revenue),
    state_funding = finance$state_funding,
    state_funding_adjusted = finance$state_funding_adjusted,
    core_revenue = finance$core_revenue,
    core_revenue_adjusted = finance$core_revenue_adjusted,
    state_funding_pct_core_revenue = safe_divide(finance$state_funding, finance$core_revenue),
    state_approps_percent_core_gasb = finance$state_approps_percent_core_gasb,
    gov_grants_fasb = finance$gov_grants_fasb,
    state_revenue_fte_fasb = finance$state_revenue_fte_fasb,
    gov_revenue_fte_fasb = finance$gov_revenue_fte_fasb,
    gov_grants_contracts_pct_core_revenue_gasb = finance$gov_grants_contracts_pct_core_revenue_gasb,
    gov_grants_contracts_pct_core_revenue_fasb = finance$gov_grants_contracts_pct_core_revenue_fasb,
    state_appropriations_pct_core_revenue_gasb = finance$state_appropriations_pct_core_revenue_gasb,
    government_funding_total = finance$government_funding,
    government_funding_total_adjusted = finance$government_funding_adjusted,
    government_funding_pct_total_revenue = safe_divide(finance$government_funding, finance$revenue),
    endowment_value = finance$endowment_value,
    endowment_value_adjusted = finance$endowment_value_adjusted,
    endowment_spending_current_use = finance$endow_spend_raw,
    endowment_spending_current_use_adjusted = inflate_to_base_year(finance$endow_spend_raw, context$year),
    endowment_spending_current_use_pct_core_revenue = safe_divide(finance$endow_spend_raw, finance$core_revenue),
    endowment_assets_per_fte_gasb = finance$endowment_assets_per_fte_gasb,
    endowment_assets_per_fte_fasb = finance$endowment_assets_per_fte_fasb,
    endowment_assets_per_fte = finance$endowment_assets_per_fte,
    endowment_assets_per_fte_adjusted = safe_divide(finance$endowment_value_adjusted, context$fte12),
    assets = finance$assets,
    assets_adjusted = finance$assets_adjusted,
    liabilities = finance$liabilities,
    liabilities_adjusted = finance$liabilities_adjusted
  )
}

build_risk_fields <- function(context, finance) {
  list(
    loss_amount = finance$loss_amount,
    loss_amount_adjusted = finance$loss_amount_adjusted,
    ended_year_at_loss = dplyr::case_when(
      is.na(finance$loss_amount) ~ NA_character_,
      finance$loss_amount < 0 ~ "Yes",
      TRUE ~ "No"
    ),
    operating_margin = ifelse(
      is.na(finance$revenue) | finance$revenue == 0 | is.na(finance$expenses),
      NA_real_,
      ((finance$revenue - finance$expenses) / finance$revenue) * 100
    ),
    tuition_dependence_ratio = safe_divide(finance$net_tuition_total, finance$revenue),
    tuition_dependence_pct = safe_divide(finance$net_tuition_total, finance$revenue) * 100,
    leverage = safe_divide(finance$liabilities, finance$assets),
    liquidity = safe_divide(finance$unrestricted_assets, finance$assets),
    discount_rate = finance$discount_rate
  )
}

build_canonical_ipeds_row <- function(row, decode_lookups) {
  context <- make_ipeds_row_context(row, decode_lookups)
  if (!keep_ipeds_canonical_row(context)) {
    return(NULL)
  }

  finance <- calculate_ipeds_finance_components(row, context)
  identity_fields <- build_identity_fields(row, context, decode_lookups)
  enrollment_fields <- build_enrollment_fields(row, context)
  finance_fields <- build_finance_fields(row, context, finance)
  risk_fields <- build_risk_fields(context, finance)

  tibble::as_tibble(c(identity_fields, enrollment_fields, finance_fields, risk_fields))
}

# ---------------------------------------------------------------------------
# CPI / inflation adjustment
# ---------------------------------------------------------------------------

# Annual average CPI-U (all urban consumers, BLS series CUUR0000SA0).
# Update this table when a new data year is added to the pipeline.
cpi_u_annual_avg <- c(
  `2014` = 236.736,
  `2015` = 237.017,
  `2016` = 240.007,
  `2017` = 245.120,
  `2018` = 251.107,
  `2019` = 255.657,
  `2020` = 258.811,
  `2021` = 270.970,
  `2022` = 292.655,
  `2023` = 304.702,
  `2024` = 313.689
)

# Adjusts `value` from `year` dollars to `base_year` dollars using CPI-U.
# Returns NA when either year is outside the cpi_u_annual_avg table.
inflate_to_base_year <- function(value, year, base_year = 2024) {
  year_key <- as.character(year)
  base_key <- as.character(base_year)
  ifelse(
    is.na(value) | is.na(year) |
      !(year_key %in% names(cpi_u_annual_avg)) |
      !(base_key %in% names(cpi_u_annual_avg)),
    NA_real_,
    value * (unname(cpi_u_annual_avg[[base_key]]) / unname(cpi_u_annual_avg[[year_key]]))
  )
}

# ---------------------------------------------------------------------------
# IPEDS code decoding
# ---------------------------------------------------------------------------

# Decodes the numeric IPEDS CONTROL code to a human-readable label.
get_control_label <- function(control_code) {
  code <- trimws(as.character(control_code %||% ""))
  dplyr::case_when(
    code == "1" ~ "Public",
    code == "2" ~ "Private not-for-profit",
    code == "3" ~ "Private for-profit",
    TRUE ~ as.character(control_code)
  )
}

# Decodes standard IPEDS Yes/No fields (1/2/-1/-2 codes).
decode_yes_no_field <- function(x) {
  code <- trimws(as.character(x %||% ""))
  dplyr::case_when(
    code == "1"  ~ "Yes",
    code == "2"  ~ "No",
    code == "-1" ~ "Not reported",
    code == "-2" ~ "Not applicable",
    code == ""   ~ NA_character_,
    TRUE ~ as.character(x)
  )
}

# Decodes a raw code using a named lookup vector; falls back to the code
# itself when no match is found.
decode_value <- function(code, lookup) {
  key <- trimws(as.character(code %||% ""))
  if (!length(lookup) || is.na(key) || key == "") {
    return(ifelse(key == "", NA_character_, as.character(code)))
  }
  if (key %in% names(lookup)) lookup[[key]] else as.character(code)
}

# ---------------------------------------------------------------------------
# State lookup
# ---------------------------------------------------------------------------

# Maps two-letter postal abbreviations to full state names for all US states
# and territories used in IPEDS.
state_lookup <- c(
  AL = "Alabama", AK = "Alaska", AZ = "Arizona", AR = "Arkansas",
  CA = "California", CO = "Colorado", CT = "Connecticut", DE = "Delaware",
  DC = "District of Columbia", FL = "Florida", GA = "Georgia", HI = "Hawaii",
  ID = "Idaho", IL = "Illinois", IN = "Indiana", IA = "Iowa", KS = "Kansas",
  KY = "Kentucky", LA = "Louisiana", ME = "Maine", MD = "Maryland",
  MA = "Massachusetts", MI = "Michigan", MN = "Minnesota", MS = "Mississippi",
  MO = "Missouri", MT = "Montana", NE = "Nebraska", NV = "Nevada",
  NH = "New Hampshire", NJ = "New Jersey", NM = "New Mexico", NY = "New York",
  NC = "North Carolina", ND = "North Dakota", OH = "Ohio", OK = "Oklahoma",
  OR = "Oregon", PA = "Pennsylvania", RI = "Rhode Island",
  SC = "South Carolina", SD = "South Dakota", TN = "Tennessee", TX = "Texas",
  UT = "Utah", VT = "Vermont", VA = "Virginia", WA = "Washington",
  WV = "West Virginia", WI = "Wisconsin", WY = "Wyoming",
  PR = "Puerto Rico", GU = "Guam", VI = "U.S. Virgin Islands",
  AS = "American Samoa", MP = "Northern Mariana Islands",
  FM = "Federated States of Micronesia", MH = "Marshall Islands", PW = "Palau"
)

# IPEDS state/territory codes excluded from the domestic finance tracker cohort.
excluded_state_codes <- c("PR", "GU", "VI", "AS", "MP", "FM", "MH", "PW")

# Converts postal abbreviations to full state names using state_lookup.
# Leaves unrecognised values unchanged.
get_state_name <- function(state_abbr) {
  vals    <- as.character(state_abbr)
  keys    <- toupper(trimws(ifelse(is.na(vals), "", vals)))
  out     <- vals
  matched <- keys %in% names(state_lookup)
  out[matched] <- unname(state_lookup[keys[matched]])
  out
}

# ---------------------------------------------------------------------------
# File download / ZIP management helpers
# ---------------------------------------------------------------------------

# Downloads `url` to `out_file` if the file does not already exist.
# Uses the shared `download_with_retry` helper (utils.R) so a hanging
# mirror can't stall the refresh workflow indefinitely.
download_if_missing <- function(url, out_file) {
  if (file.exists(out_file)) return(invisible(out_file))
  url <- gsub("&amp;", "&", as.character(url), fixed = TRUE)
  download_with_retry(url, destfile = out_file, mode = "wb", quiet = TRUE)
  out_file
}

# Returns TRUE only when `path` is a non-empty, valid ZIP archive.
is_valid_zip_archive <- function(path) {
  if (!file.exists(path)) return(FALSE)
  tryCatch(
    nrow(utils::unzip(path, list = TRUE)) > 0,
    error = function(e) FALSE
  )
}

# Fetches the IPEDS data-dictionary ZIP for `table_name` if it is missing or
# corrupt, placing the result at `out_file`.
ensure_dictionary_archive <- function(table_name, out_file, year = 2024) {
  if (is_valid_zip_archive(out_file)) return(invisible(out_file))
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  url <- sprintf(
    "https://nces.ed.gov/ipeds/dictionary-generator?year=%s&tableName=%s",
    year, table_name
  )
  if (file.exists(out_file)) file.remove(out_file)
  download_if_missing(url, out_file)
  if (!is_valid_zip_archive(out_file)) {
    stop(sprintf(
      "Dictionary download for %s did not produce a valid archive.", table_name
    ))
  }
  out_file
}

# Expands `zip_path` into `destination_path` only when the destination is
# missing or empty.
expand_zip_if_missing <- function(zip_path, destination_path) {
  if (dir.exists(destination_path)) {
    existing <- list.files(destination_path, recursive = TRUE,
                            full.names = TRUE, all.files = FALSE)
    if (length(existing) > 0) return(invisible(destination_path))
    unlink(destination_path, recursive = TRUE, force = TRUE)
  }
  dir.create(destination_path, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zip_path, exdir = destination_path)
  destination_path
}

# Returns the first file under `path` matching `pattern`, or NA if none found.
find_first_file <- function(path, pattern) {
  hits <- list.files(path, pattern = pattern, recursive = TRUE,
                      full.names = TRUE, ignore.case = TRUE)
  if (length(hits) == 0) NA_character_ else hits[[1]]
}

# ---------------------------------------------------------------------------
# IPEDS data-dictionary frequency lookup
# ---------------------------------------------------------------------------

# Reads the frequency (code ? label) table for `var_name` from a downloaded
# IPEDS dictionary archive.  `extract_root` is the directory under which
# per-table extractions are cached (typically paths$cache_aux_extract_dir).
get_frequency_lookup <- function(dictionary_archive, table_name, var_name,
                                  extract_root) {
  expanded <- file.path(extract_root, paste0("dict_", table_name))
  expand_zip_if_missing(dictionary_archive, expanded)
  xlsx_path <- find_first_file(expanded, "\\.xlsx$")
  if (is.na(xlsx_path) && dir.exists(expanded)) {
    unlink(expanded, recursive = TRUE, force = TRUE)
    expand_zip_if_missing(dictionary_archive, expanded)
    xlsx_path <- find_first_file(expanded, "\\.xlsx$")
  }
  if (is.na(xlsx_path)) return(character())
  sheets <- tryCatch(readxl::excel_sheets(xlsx_path), error = function(e) character())
  if (length(sheets) == 0 && dir.exists(expanded)) {
    unlink(expanded, recursive = TRUE, force = TRUE)
    expand_zip_if_missing(dictionary_archive, expanded)
    xlsx_path <- find_first_file(expanded, "\\.xlsx$")
    if (is.na(xlsx_path)) return(character())
    sheets <- tryCatch(readxl::excel_sheets(xlsx_path), error = function(e) character())
  }
  freq_sheet <- sheets[tolower(sheets) == "frequencies"][1]
  if (length(freq_sheet) == 0 || is.na(freq_sheet)) return(character())

  rows <- suppressMessages(
    readxl::read_excel(xlsx_path, sheet = freq_sheet,
                        col_names = FALSE, .name_repair = "minimal")
  )
  if (ncol(rows) == 0) return(character())

  bad_names <- is.na(names(rows)) | names(rows) == ""
  names(rows)[bad_names] <- paste0("X", which(bad_names))
  names(rows)[seq_len(min(5L, ncol(rows)))] <-
    c("A", "B", "C", "D", "E")[seq_len(min(5L, ncol(rows)))]

  if (!all(c("A", "D", "E") %in% names(rows))) return(character())

  rows <- rows %>%
    mutate(A = trimws(as.character(A)),
           D = trimws(as.character(D)),
           E = trimws(as.character(E))) %>%
    filter(A == var_name, !is.na(D), D != "", !is.na(E), E != "")

  stats::setNames(rows$E, rows$D)
}

# ---------------------------------------------------------------------------
# Time-series analysis helpers
# ---------------------------------------------------------------------------

# Returns the most-recent non-NA (year, value) pair for `field` in `df`.
latest_non_null <- function(df, field) {
  vals <- df %>%
    filter(!is.na(.data[[field]])) %>%
    arrange(desc(year)) %>%
    select(year, value = all_of(field)) %>%
    slice(1)
  if (nrow(vals) == 0) return(list(year = NA_real_, value = NA_real_))
  list(year = vals$year[[1]], value = vals$value[[1]])
}

# Counts years in [start_year, end_year) where the YoY change is = threshold_pct.
count_decline_years <- function(years, values, start_year, end_year,
                                 threshold_pct = 0) {
  lookup <- stats::setNames(values, years)
  count  <- 0L
  for (yr in start_year:end_year) {
    pct <- safe_pct_change(
      unname(lookup[as.character(yr + 1)]),
      unname(lookup[as.character(yr)])
    )
    if (!is.na(pct) && pct <= threshold_pct) count <- count + 1L
  }
  count
}

# Counts how many years in `target_years` have a value strictly below threshold.
count_negative_years <- function(years, values, target_years, threshold = 0) {
  lookup <- stats::setNames(values, years)
  sum(purrr::map_lgl(target_years, ~ {
    val <- unname(lookup[as.character(.x)])
    !is.na(val) && val < threshold
  }))
}

# Counts years with negative values in the window ending at `end_year`.
loss_frequency <- function(years, values, end_year, window_years) {
  lookup     <- stats::setNames(values, years)
  start_year <- end_year - window_years + 1L
  sum(vapply(start_year:end_year, function(yr) {
    val <- unname(lookup[as.character(yr)])
    !is.na(val) && val < 0
  }, logical(1)))
}

