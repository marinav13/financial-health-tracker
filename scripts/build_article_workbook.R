main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args          <- parse_cli_args(cli_args)
  ipeds         <- load_ipeds_paths()
  ipeds_layout  <- ipeds$ipeds_layout
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)

  input_csv <- get_arg_value("--input", ipeds_layout(root = ".")$dataset_csv)
  # The workbook is written as SpreadsheetML with an .xls extension so Excel can
  # open it directly without a manual rename step.
  output_workbook <- get_arg_value("--output", "./workbooks/ipeds_financial_health_article_workbook.xls")

source(file.path(getwd(), "scripts", "shared", "workbook_helpers.R"))
# Pure helpers (yes_flag, safe_pct, escape_xml, make_row, sort_df, q75_safe,
# q25_safe, count_by_group_from, weighted_intl_pct, summarize_event_subset,
# build_event_xtab, worksheet_signature, xml_cell, worksheet_xml,
# read_csv_if_exists, read_required_closure_csv) are in
# scripts/shared/workbook_helpers.R

flagship_unitids <- c(
  102553,100751,106397,104179,110635,126614,129020,130943,134130,139959,141574,153658,142285,
  145637,151351,155317,157085,159391,166629,163286,161253,170976,174066,178396,176017,180489,
  199120,200280,181464,183044,186380,187985,182290,196088,204796,207500,209551,214777,217484,
  218663,219471,221759,228778,230764,234076,231174,236948,240444,238032,240727
)

read_df <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
latest <- read_df[as.character(read_df$year) == "2024", , drop = FALSE]
prev_year <- read_df[as.character(read_df$year) == "2023", , drop = FALSE]

num_cols <- c(
  "enrollment_headcount_total","enrollment_headcount_undergrad","enrollment_headcount_graduate",
  "enrollment_nonresident_total","enrollment_nonresident_undergrad","enrollment_nonresident_graduate",
  "enrollment_pct_change_5yr","enrollment_change_1yr","share_grad_students",
  "pct_international_all","pct_international_undergraduate","pct_international_graduate",
  "international_student_count_change_5yr","international_enrollment_pct_change_5yr",
  "international_enrollment_change_10yr","international_enrollment_pct_change_10yr",
  "transfer_out_rate_bachelor","transfer_out_rate_bachelor_change_5yr","transfer_out_rate_bachelor_change_10yr",
  "staff_fte_total","staff_fte_instructional","staff_total_pct_change_5yr",
  "staff_instructional_fte_pct_change_5yr","staff_headcount_total","staff_headcount_instructional",
  "staff_total_headcount_pct_change_5yr","staff_instructional_headcount_pct_change_5yr","staff_change_1yr",
  "revenue_total","revenue_pct_change_5yr","revenue_change_1yr","expenses_total","loss_amount",
  "loss_years_last_5","loss_years_last_10","net_tuition_per_fte","net_tuition_per_fte_change_5yr",
  "tuition_dependence_pct","admissions_yield","yield_pct_change_5yr","discount_rate","discount_pct_change_5yr",
  "federal_grants_contracts_pell_adjusted","federal_grants_contracts_pell_adjusted_pct_core_revenue",
  "federal_grants_contracts_pell_adjusted_pct_change_5yr","state_funding","state_funding_pct_core_revenue",
  "state_funding_pct_change_5yr","endowment_value","endowment_pct_change_5yr","liquidity",
  "liquidity_percentile_private_nfp","leverage","leverage_percentile_private_nfp","loan_year_latest",
  "federal_loan_pct_most_recent","federal_loan_count_most_recent","federal_loan_avg_most_recent"
)

for (nm in intersect(num_cols, names(latest))) {
  latest[[nm]] <- to_num(latest[[nm]])
}
for (nm in intersect(num_cols, names(prev_year))) {
  prev_year[[nm]] <- to_num(prev_year[[nm]])
}
for (nm in intersect(num_cols, names(read_df))) {
  read_df[[nm]] <- to_num(read_df[[nm]])
}

latest$warning_score_core <- compute_warning_score_core(latest)

tuition_dependence_q75 <- q75_safe(latest$tuition_dependence_pct)
international_share_q75 <- q75_safe(latest$pct_international_all)
federal_dependence_q75 <- q75_safe(latest$federal_grants_contracts_pell_adjusted_pct_core_revenue)
private_tuition_dependence_q75 <- q75_safe(latest$tuition_dependence_pct[latest$control_label != "Public"])
private_leverage_q75 <- q75_safe(latest$leverage[latest$control_label != "Public"])
private_liquidity_q25 <- q25_safe(latest$liquidity[latest$control_label != "Public"])

latest$high_tuition_dependence <- !is.na(latest$tuition_dependence_pct) & !is.na(tuition_dependence_q75) &
  latest$tuition_dependence_pct >= tuition_dependence_q75
latest$high_international_share <- !is.na(latest$pct_international_all) & !is.na(international_share_q75) &
  latest$pct_international_all >= international_share_q75
latest$high_federal_dependence <- !is.na(latest$federal_grants_contracts_pell_adjusted_pct_core_revenue) &
  !is.na(federal_dependence_q75) &
  latest$federal_grants_contracts_pell_adjusted_pct_core_revenue >= federal_dependence_q75

latest$multi_signal_score <- row_score(
  yes_flag(latest$enrollment_decline_last_3_of_5),
  yes_flag(latest$revenue_10pct_drop_last_3_of_5),
  yes_flag(latest$losses_last_3_of_5),
  latest$high_tuition_dependence,
  !is.na(latest$net_tuition_per_fte_change_5yr) & latest$net_tuition_per_fte_change_5yr < 0,
  latest$high_international_share,
  latest$high_federal_dependence
)

latest$staffing_cut_risk_score <- row_score(
  yes_flag(latest$enrollment_decline_last_3_of_5),
  yes_flag(latest$revenue_10pct_drop_last_3_of_5),
  yes_flag(latest$losses_last_3_of_5),
  yes_flag(latest$ended_year_at_loss),
  !is.na(latest$net_tuition_per_fte_change_5yr) & latest$net_tuition_per_fte_change_5yr < 0,
  !is.na(latest$tuition_dependence_pct) & latest$tuition_dependence_pct >= 50,
  !is.na(latest$endowment_pct_change_5yr) & latest$endowment_pct_change_5yr < 0,
  !is.na(latest$staff_instructional_headcount_pct_change_5yr) & latest$staff_instructional_headcount_pct_change_5yr < 0
)

is_private <- latest$control_label != "Public"
latest$private_closure_risk_score <- row_score(
  is_private & yes_flag(latest$enrollment_decline_last_3_of_5),
  is_private & yes_flag(latest$losses_last_3_of_5),
  is_private & yes_flag(latest$ended_year_at_loss),
  is_private & !is.na(latest$net_tuition_per_fte_change_5yr) & latest$net_tuition_per_fte_change_5yr < 0,
  is_private & !is.na(latest$tuition_dependence_pct) & !is.na(private_tuition_dependence_q75) & latest$tuition_dependence_pct >= private_tuition_dependence_q75,
  is_private & !is.na(latest$liquidity) & !is.na(private_liquidity_q25) & latest$liquidity <= private_liquidity_q25,
  is_private & !is.na(latest$leverage) & !is.na(private_leverage_q75) & latest$leverage >= private_leverage_q75,
  is_private & !is.na(latest$endowment_pct_change_5yr) & latest$endowment_pct_change_5yr < 0,
  is_private & !is.na(latest$staff_instructional_headcount_pct_change_5yr) & latest$staff_instructional_headcount_pct_change_5yr < 0
)

is_public_nonflagship <- latest$control_label == "Public" & !(as.integer(latest$unitid) %in% flagship_unitids)
latest$public_campus_risk_score <- row_score(
  is_public_nonflagship & yes_flag(latest$enrollment_decline_last_3_of_5),
  is_public_nonflagship & !is.na(latest$enrollment_pct_change_5yr) & latest$enrollment_pct_change_5yr <= -20,
  is_public_nonflagship & !is.na(latest$staff_total_headcount_pct_change_5yr) & latest$staff_total_headcount_pct_change_5yr < 0,
  is_public_nonflagship & !is.na(latest$staff_instructional_headcount_pct_change_5yr) & latest$staff_instructional_headcount_pct_change_5yr < 0,
  is_public_nonflagship & yes_flag(latest$transfer_out_rate_bachelor_increase_5yr),
  is_public_nonflagship & yes_flag(latest$transfer_out_rate_bachelor_increase_10yr),
  is_public_nonflagship & !is.na(latest$state_funding_pct_change_5yr) & latest$state_funding_pct_change_5yr < 0,
  is_public_nonflagship & (is.na(latest$revenue_total) | is.na(latest$expenses_total))
)

latest$closure_risk_track <- dplyr::case_when(
  latest$control_label == "Public" ~ "Public campus restructuring risk",
  TRUE ~ "Private college closure risk"
)

bacc_category_label <- "Degree-granting, primarily baccalaureate or above"

groups <- list(
  all = latest,
  public = latest[latest$control_label == "Public", , drop = FALSE],
  private_nfp = latest[latest$control_label == "Private not-for-profit", , drop = FALSE],
  private_fp = latest[latest$control_label == "Private for-profit", , drop = FALSE],
  bacc_public = latest[latest$control_label == "Public" & latest$category == bacc_category_label, , drop = FALSE],
  bacc_private_nfp = latest[latest$control_label == "Private not-for-profit" & latest$category == bacc_category_label, , drop = FALSE],
  bacc_private_fp = latest[latest$control_label == "Private for-profit" & latest$category == bacc_category_label, , drop = FALSE]
)

groups_2023 <- list(
  all = prev_year,
  public = prev_year[prev_year$control_label == "Public", , drop = FALSE],
  private_nfp = prev_year[prev_year$control_label == "Private not-for-profit", , drop = FALSE],
  private_fp = prev_year[prev_year$control_label == "Private for-profit", , drop = FALSE],
  bacc_public = prev_year[prev_year$control_label == "Public" & prev_year$category == bacc_category_label, , drop = FALSE],
  bacc_private_nfp = prev_year[prev_year$control_label == "Private not-for-profit" & prev_year$category == bacc_category_label, , drop = FALSE],
  bacc_private_fp = prev_year[prev_year$control_label == "Private for-profit" & prev_year$category == bacc_category_label, , drop = FALSE]
)

summary_metric_specs <- list(
  list(metric = "Enrollment decline in 3 of last 5 years", pred = function(df) yes_flag(df$enrollment_decline_last_3_of_5)),
  list(metric = "Revenue decline in 3 of last 5 years", pred = function(df) yes_flag(df$revenue_10pct_drop_last_3_of_5)),
  list(metric = "In the red in 3 of last 5 years", pred = function(df) yes_flag(df$losses_last_3_of_5)),
  list(metric = "Enrollment and revenue decline in 3 of last 5 years", pred = function(df) yes_flag(df$enrollment_decline_last_3_of_5) & yes_flag(df$revenue_10pct_drop_last_3_of_5)),
  list(metric = "Enrollment, revenue decline and in the red in 3 of last 5 years", pred = function(df) yes_flag(df$enrollment_decline_last_3_of_5) & yes_flag(df$revenue_10pct_drop_last_3_of_5) & yes_flag(df$losses_last_3_of_5)),
  list(metric = "Enrollment decline and in the red in 3 of last 5 years", pred = function(df) yes_flag(df$enrollment_decline_last_3_of_5) & yes_flag(df$losses_last_3_of_5)),
  list(metric = "Revenue decline and in the red in 3 of last 5 years", pred = function(df) yes_flag(df$revenue_10pct_drop_last_3_of_5) & yes_flag(df$losses_last_3_of_5)),
  list(metric = "Enrollment decline in 2024", pred = function(df) !is.na(df$enrollment_change_1yr) & df$enrollment_change_1yr < 0),
  list(metric = "Revenue decline in 2024", pred = function(df) !is.na(df$revenue_change_1yr) & df$revenue_change_1yr < 0),
  list(metric = "In the red in 2024", pred = function(df) yes_flag(df$ended_year_at_loss)),
  list(metric = "International enrollment increased over past decade", pred = function(df) yes_flag(df$international_enrollment_increase_10yr)),
  list(metric = "International enrollment increased over past 5 years", pred = function(df) yes_flag(df$international_enrollment_increase_5yr)),
  list(metric = "Staffing cut over past 5 years", pred = function(df) !is.na(df$staff_total_headcount_pct_change_5yr) & df$staff_total_headcount_pct_change_5yr < 0, notes = "Using staff headcount"),
  list(metric = "Instructional staffing cut over past 5 years", pred = function(df) !is.na(df$staff_instructional_headcount_pct_change_5yr) & df$staff_instructional_headcount_pct_change_5yr < 0, notes = "Using staff headcount"),
  list(metric = "Ended 2024 fiscal year at a loss", pred = function(df) yes_flag(df$ended_year_at_loss)),
  list(metric = "Net tuition per FTE decreased over past 5 years", pred = function(df) !is.na(df$net_tuition_per_fte_change_5yr) & df$net_tuition_per_fte_change_5yr < 0, notes = "Using net tuition per FTE"),
  list(metric = "State appropriations decreased over past 5 years", pred = function(df) !is.na(df$state_funding_pct_change_5yr) & df$state_funding_pct_change_5yr < 0),
  list(metric = "State appropriations increased over past 5 years", pred = function(df) !is.na(df$state_funding_pct_change_5yr) & df$state_funding_pct_change_5yr > 0),
  list(metric = "Endowment increased over past 5 years", pred = function(df) !is.na(df$endowment_pct_change_5yr) & df$endowment_pct_change_5yr > 0),
  list(metric = "Endowment decreased over past 5 years", pred = function(df) !is.na(df$endowment_pct_change_5yr) & df$endowment_pct_change_5yr < 0),
  list(metric = "Enrollment decreased over past 5 years", pred = function(df) !is.na(df$enrollment_pct_change_5yr) & df$enrollment_pct_change_5yr < 0),
  list(metric = "Revenue decreased over past 5 years", pred = function(df) !is.na(df$revenue_pct_change_5yr) & df$revenue_pct_change_5yr < 0),
  list(metric = "Transfer-out rate increased over past 5 years", pred = function(df) !is.na(df$transfer_out_rate_bachelor_change_5yr) & df$transfer_out_rate_bachelor_change_5yr > 0)
)

summary_rows <- do.call(rbind, lapply(summary_metric_specs, function(spec) {
  counts <- count_by_group_from(groups, spec$pred)
  pcts <- pct_by_group_from(groups, spec$pred)
  make_count_pct_rows(spec$metric, counts, pcts, spec$notes %||% "")
}))

private_nfp_only <- groups$private_nfp
disc_count <- sum(!is.na(private_nfp_only$discount_pct_change_5yr) & private_nfp_only$discount_pct_change_5yr > 0, na.rm = TRUE)
disc_pct <- safe_pct(disc_count, nrow(private_nfp_only))
summary_rows <- append_rows(
  summary_rows,
  make_row("Discount rate increased over past 5 years", "count", disc_count, "", disc_count, "", "", sum(!is.na(groups$bacc_private_nfp$discount_pct_change_5yr) & groups$bacc_private_nfp$discount_pct_change_5yr > 0, na.rm = TRUE), "", "Private nonprofit only"),
  make_row("Discount rate increased over past 5 years", "percent", disc_pct, "", disc_pct, "", "", safe_pct(sum(!is.na(groups$bacc_private_nfp$discount_pct_change_5yr) & groups$bacc_private_nfp$discount_pct_change_5yr > 0, na.rm = TRUE), nrow(groups$bacc_private_nfp)), "", "Private nonprofit only")
)

intl_all <- sapply(groups, weighted_intl_pct, num_col = "enrollment_nonresident_total", den_col = "enrollment_headcount_total")
intl_grad <- sapply(groups, weighted_intl_pct, num_col = "enrollment_nonresident_graduate", den_col = "enrollment_headcount_graduate")
intl_ug <- sapply(groups, weighted_intl_pct, num_col = "enrollment_nonresident_undergrad", den_col = "enrollment_headcount_undergrad")

summary_rows <- append_rows(
  summary_rows,
  make_group_row("Students who are international", "percent", intl_all),
  make_group_row("Graduate students who are international", "percent", intl_grad),
  make_group_row("Undergraduate students who are international", "percent", intl_ug)
)

repeated_losses_share <- pct_by_group_from(groups, function(df) yes_flag(df$losses_last_3_of_5))
revenue_change_median <- numeric_stat_by_group(groups, "revenue_pct_change_5yr")
net_tuition_per_fte_change_median <- numeric_stat_by_group(groups, "net_tuition_per_fte_change_5yr")
transfer_out_rate_median <- numeric_stat_by_group(groups, "transfer_out_rate_bachelor")
transfer_out_change_median <- numeric_stat_by_group(groups, "transfer_out_rate_bachelor_change_5yr")
staffing_cut_share <- pct_by_group_from(groups, function(df) !is.na(df$staff_total_headcount_pct_change_5yr) & df$staff_total_headcount_pct_change_5yr < 0)
state_funding_change_median <- numeric_stat_by_group(groups, "state_funding_pct_change_5yr")

summary_rows <- append_rows(
  summary_rows,
  make_group_row("Repeated losses national share by sector", "percent", repeated_losses_share, "Share with losses in 3 of last 5 years"),
  make_group_row("5-year revenue change by sector", "median percent", revenue_change_median, "Median institution-level change"),
  make_group_row("5-year net tuition per FTE change by sector", "median percent", net_tuition_per_fte_change_median, "Median institution-level change"),
  make_group_row("Transfer-out rate by sector", "median rate", transfer_out_rate_median, "Bachelor cohort"),
  make_group_row("Transfer-out rate change over past 5 years by sector", "median point change", transfer_out_change_median, "Bachelor cohort"),
  make_group_row("Staffing cuts national share by sector", "percent", staffing_cut_share, "Share with 5-year staff headcount decline"),
  make_group_row("State-funding change by sector", "median percent", state_funding_change_median, "Mostly meaningful for publics")
)

loss_2023_counts <- count_by_group_from(groups_2023, function(df) yes_flag(df$ended_year_at_loss))
loss_2023_pcts <- pct_by_group_from(groups_2023, function(df) yes_flag(df$ended_year_at_loss))
loss_2024_counts <- count_by_group_from(groups, function(df) yes_flag(df$ended_year_at_loss))
loss_2024_pcts <- pct_by_group_from(groups, function(df) yes_flag(df$ended_year_at_loss))
summary_rows <- append_rows(
  summary_rows,
  make_group_row("Ended fiscal year at a loss", "2023 count", loss_2023_counts, "Compared with 2024"),
  make_group_row("Ended fiscal year at a loss", "2023 percent", loss_2023_pcts, "Compared with 2024"),
  make_group_row("Ended fiscal year at a loss", "2024 count", loss_2024_counts, "Compared with 2023"),
  make_group_row("Ended fiscal year at a loss", "2024 percent", loss_2024_pcts, "Compared with 2023"),
  make_group_row("Change in institutions ending year at a loss", "count change 2024 minus 2023", loss_2024_counts - loss_2023_counts, "2024 minus 2023")
)

top_fed <- top_metric_by_group_from(groups, "federal_grants_contracts_pell_adjusted_pct_core_revenue")
top_state <- top_metric_by_group_from(groups, "state_funding_pct_core_revenue")

summary_rows <- append_rows(
  summary_rows,
  make_group_row(
    "Highest federal funding share of core revenue",
    "institution",
    sapply(top_fed, function(row) if (is.null(row)) NA else row$institution_name[[1]]),
    "Pell-adjusted federal grants/contracts"
  ),
  make_group_row(
    "Highest federal funding share of core revenue",
    "percent",
    sapply(top_fed, function(row) if (is.null(row)) NA else row$federal_grants_contracts_pell_adjusted_pct_core_revenue[[1]] * 100),
    "Pell-adjusted federal grants/contracts"
  ),
  make_group_row(
    "Highest state funding share of core revenue",
    "institution",
    sapply(top_state, function(row) if (is.null(row)) NA else row$institution_name[[1]]),
    "State appropriations"
  ),
  make_group_row(
    "Highest state funding share of core revenue",
    "percent",
    sapply(top_state, function(row) if (is.null(row)) NA else row$state_funding_pct_core_revenue[[1]] * 100),
    "State appropriations"
  )
)

sheet_index_rows <- do.call(rbind, list(
  make_row("Worksheet index", "Summary", "", "", "", "", "", "", "", "Top-level counts and sector shares. Includes all filtered institutions plus baccalaureate-only breakouts."),
  make_row("Worksheet index", "ReportAnswers", "", "", "", "", "", "", "", "Topline reporting answers, counts, and notes explaining how each figure was calculated."),
  make_row("Worksheet index", "All_2024", "", "", "", "", "", "", "", "All 2024 predominantly baccalaureate institutions included in the workbook universe."),
  make_row("Worksheet index", "EnrollDecl3of5", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges with enrollment declines in 3 of the last 5 years."),
  make_row("Worksheet index", "RevDecl3of5", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges with revenue declines in 3 of the last 5 years."),
  make_row("Worksheet index", "Red3of5", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges that ended 3 of the last 5 years at a loss."),
  make_row("Worksheet index", "EnrollRev3of5", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges with both enrollment and revenue declines in 3 of the last 5 years."),
  make_row("Worksheet index", "EnrollRed3of5", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges with enrollment declines in 3 of the last 5 years and losses in 3 of the last 5 years."),
  make_row("Worksheet index", "All3Signals", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges with enrollment declines, revenue declines, and losses in 3 of the last 5 years."),
  make_row("Worksheet index", "IntlUp5yr", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges ranked by biggest increase in international students over the past 5 years."),
  make_row("Worksheet index", "IntlUp10yr", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges ranked by biggest increase in international students over the past 10 years."),
  make_row("Worksheet index", "Flagships", "", "", "", "", "", "", "", "Predominantly baccalaureate public flagships ranked by core stress signals such as enrollment, revenue, and losses."),
  make_row("Worksheet index", "FlagshipFed", "", "", "", "", "", "", "", "Predominantly baccalaureate public flagships ranked by dependence on Pell-adjusted federal grants and contracts."),
  make_row("Worksheet index", "ResearchLeaders", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges ranked by research spending per FTE, with research spending levels and research spending as a share of core expenses."),
  make_row("Worksheet index", "Loss2024", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges that ended 2024 at a loss."),
  make_row("Worksheet index", "StateDown5yr", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges with declining state appropriations over the past 5 years."),
  make_row("Worksheet index", "EndowDown5yr", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges with declining endowment value over the past 5 years."),
  make_row("Worksheet index", "DiscRateUp5yr", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges with rising discount rates over the past 5 years."),
  make_row("Worksheet index", "EnrollDown5yr", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges ranked by biggest enrollment decline over the past 5 years."),
  make_row("Worksheet index", "RevDown5yr", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges ranked by biggest revenue decline over the past 5 years."),
  make_row("Worksheet index", "StaffDown5yr", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges ranked by biggest total staff headcount decline over the past 5 years."),
  make_row("Worksheet index", "InstrStaffDown5yr", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges ranked by biggest instructional staff headcount decline over the past 5 years."),
  make_row("Worksheet index", "StaffNetTuitionDown", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges with both total staff headcount cuts and falling net tuition revenue per FTE over the past 5 years."),
  make_row("Worksheet index", "StaffCutRisk", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges ranked by the risk factors that most often show up before major staffing cuts: enrollment decline, revenue decline, repeated losses, ending 2024 at a loss, falling net tuition per FTE, high tuition dependence, endowment decline, and instructional staffing cuts."),
  make_row("Worksheet index", "TransferOutUp5yr", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges with rising transfer-out rates over the past 5 years."),
  make_row("Worksheet index", "TransferOutUp10yr", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges with rising transfer-out rates over the past 10 years."),
  make_row("Worksheet index", "FedDepend", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges ranked by dependence on Pell-adjusted federal grants and contracts."),
  make_row("Worksheet index", "StateDepend", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges ranked by dependence on state appropriations."),
  make_row("Worksheet index", "StateBySt", "", "", "", "", "", "", "", "State-level public breakdowns using predominantly baccalaureate institutions."),
  make_row("Worksheet index", "YearsAtLoss", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges ranked by how many of the past 10 years they ended at a loss."),
  make_row("Worksheet index", "TuitionDepend", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges ranked by net tuition dependence."),
  make_row("Worksheet index", "NetTuitionDown", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges with falling net tuition revenue per FTE over the past 5 years."),
  make_row("Worksheet index", "IntlShare", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges ranked by international-student share."),
  make_row("Worksheet index", "LowCushion", "", "", "", "", "", "", "", "Predominantly baccalaureate private nonprofits ranked by lowest liquidity."),
  make_row("Worksheet index", "HighDebt", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges ranked by leverage."),
  make_row("Worksheet index", "FedAndIntl", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges with both high federal dependence and high international share."),
  make_row("Worksheet index", "MultiSignal", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges with the highest combined warning-signal score."),
  make_row("Worksheet index", "PrivateCloseRisk", "", "", "", "", "", "", "", "Predominantly baccalaureate private colleges ranked by a transparent closure-risk score built from enrollment decline, repeated losses, ending 2024 at a loss, net tuition decline, high tuition dependence, weak liquidity, high leverage, endowment decline, and instructional staffing cuts."),
  make_row("Worksheet index", "PublicCampusRisk", "", "", "", "", "", "", "", "Predominantly baccalaureate non-flagship public campuses ranked by a restructuring-risk score built from enrollment decline, large 5-year enrollment losses, staffing cuts, rising transfer-out rates, falling state funding, and missing campus-level revenue or expense data."),
  make_row("Worksheet index", "PublicFinBad50", "", "", "", "", "", "", "", "Top 50 public institutions with the most finance-page indicators currently styled as bad on the site."),
  make_row("Worksheet index", "PrivateFinBad50", "", "", "", "", "", "", "", "Top 50 private institutions with the most finance-page indicators currently styled as bad on the site."),
  make_row("Worksheet index", "LossTuition", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges with repeated losses and high tuition dependence."),
  make_row("Worksheet index", "PrivNFPStress", "", "", "", "", "", "", "", "Predominantly baccalaureate private nonprofits with rising discount rates and falling net tuition per FTE."),
  make_row("Worksheet index", "MultiFront", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges with high international share, high federal dependence, negative 5-year revenue change, and losses in at least 3 of the last 10 years."),
  make_row("Worksheet index", "DistressCore", "", "", "", "", "", "", "", "Broad distress list using the workbook warning-score definition: at least 4 of 6 core warning signs."),
  make_row("Worksheet index", "DistressIntl10", "", "", "", "", "", "", "", "Distressed colleges that also increased international enrollment over the past 10 years."),
  make_row("Worksheet index", "StaffCutsYoY", "", "", "", "", "", "", "", "Year-by-year counts of institutions with staffing cuts versus the prior year, plus the total number of total and instructional staff positions cut or added."),
  make_row("Worksheet index", "PublicFedTop", "", "", "", "", "", "", "", "Public universities ranked by federal grants and contracts as a share of core revenue."),
  make_row("Worksheet index", "GradDependTop", "", "", "", "", "", "", "", "Universities ranked by graduate-student share and Grad PLUS borrowing intensity."),
  make_row("Worksheet index", "PublicGradTop", "", "", "", "", "", "", "", "Public universities ranked by graduate-student share and Grad PLUS borrowing intensity."),
  make_row("Worksheet index", "StudPerInstr50", "", "", "", "", "", "", "", "Top 50 universities with the highest students-per-instructional-staff ratio, using the FTE-based IPEDS measure."),
  make_row("Worksheet index", "FlagshipCuts", "", "", "", "", "", "", "", "Public flagships matched to still-disrupted federal research cuts from Grant Witness."),
  make_row("Worksheet index", "DistressCompare", "", "", "", "", "", "", "", "Year comparison for the distress paragraph framing, including 2024 toplines and 2019/2014 context."),
  make_row("Worksheet index", "IntlOffset10yr", "", "", "", "", "", "", "", "Institutions where domestic enrollment would have fallen further without 10-year international enrollment growth."),
  make_row("Worksheet index", "BiggestDropsNoIntl", "", "", "", "", "", "", "", "All ranked institutions where 2014-2024 enrollment would have fallen further without international enrollment growth, sorted by the biggest implied domestic drops."),
  make_row("Worksheet index", "AccredFinanceXtab", "", "", "", "", "", "", "", "Finance, enrollment, and staffing comparison for institutions with versus without accreditation actions."),
  make_row("Worksheet index", "AccredMatches", "", "", "", "", "", "", "", "Matched 4-year primarily bachelor's institutions with accreditation actions and finance metrics."),
  make_row("Worksheet index", "CutsFinanceXtab", "", "", "", "", "", "", "", "Finance, enrollment, and staffing comparison for institutions with versus without college cuts."),
  make_row("Worksheet index", "CutsMatches", "", "", "", "", "", "", "", "Matched 4-year primarily bachelor's institutions with college cuts and finance metrics."),
  make_row("Worksheet index", "HCM2Summary", "", "", "", "", "", "", "", "Quarterly counts for schools on heightened cash monitoring level 2 in the 2024-2025 Federal Student Aid snapshots."),
  make_row("Worksheet index", "HCM2All", "", "", "", "", "", "", "", "All normalized HCM2 rows from the December 2024 through December 2025 quarterly source files."),
  make_row("Worksheet index", "HCM2Dec24Drop", "", "", "", "", "", "", "", "Schools on HCM2 in December 2024 that had dropped off the list by December 2025."),
  make_row("Worksheet index", "HCM2Mar25Drop", "", "", "", "", "", "", "", "Schools on HCM2 in March 2025 that had dropped off the list by December 2025."),
  make_row("Worksheet index", "HCM2Jun25Drop", "", "", "", "", "", "", "", "Schools on HCM2 in June 2025 that had dropped off the list by December 2025."),
  make_row("Worksheet index", "HCM2Dec24Stay", "", "", "", "", "", "", "", "Schools on HCM2 in December 2024 that remained on every quarterly list through December 2025."),
  make_row("Worksheet index", "HCM2Mar25Stay", "", "", "", "", "", "", "", "Schools on HCM2 in March 2025 that remained on every quarterly list through December 2025."),
  make_row("Worksheet index", "RunningClosures", "", "", "", "", "", "", "", "Running list of federal closure events and IPEDS-supported closure or merger/consolidation events from 2008 to the present."),
  make_row("Worksheet index", "MainCampusClosures", "", "", "", "", "", "", "", "Main-campus or full-system closures derived from federal closure files."),
  make_row("Worksheet index", "BranchCampusClosures", "", "", "", "", "", "", "", "Branch-campus closures derived from PEPS rows and monthly federal reports."),
  make_row("Worksheet index", "MergersConsol", "", "", "", "", "", "", "", "IPEDS exits that look more like mergers or consolidations than clean closures."),
  make_row("Worksheet index", "PrivFedMainClose", "", "", "", "", "", "", "", "Selective list of private-sector main/full-system closures from federal closure sources only."),
  make_row("Worksheet index", "IntlVulnerable", "", "", "", "", "", "", "", "High-international-share institutions from the 10-year offset list with additional financial warning signs."),
  make_row("Worksheet index", "IntlVulnLarge", "", "", "", "", "", "", "", "Same as IntlVulnerable, limited to institutions with at least 5,000 students.")
))

summary_rows <- append_rows(sheet_index_rows, summary_rows)

all_sheet_columns <- c(
  "unitid","institution_name","state","city","control_label","sector","category","urbanization","religious_affiliation","all_programs_distance_education","closure_risk_track","private_closure_risk_score","public_campus_risk_score","staffing_cut_risk_score",
  "warning_score_core","enrollment_decline_last_3_of_5","revenue_10pct_drop_last_3_of_5","losses_last_3_of_5","ended_year_at_loss","loss_years_last_5","loss_years_last_10",
  "enrollment_headcount_total","enrollment_pct_change_5yr","enrollment_decreased_5yr","enrollment_change_1yr",
  "share_grad_students","pct_international_all","pct_international_undergraduate","pct_international_graduate","international_student_count_change_5yr","international_enrollment_pct_change_5yr","international_enrollment_increase_5yr","international_enrollment_change_10yr","international_enrollment_pct_change_10yr","international_enrollment_increase_10yr",
  "transfer_out_rate_bachelor","transfer_out_rate_bachelor_change_5yr","transfer_out_rate_bachelor_increase_5yr","transfer_out_rate_bachelor_change_10yr","transfer_out_rate_bachelor_increase_10yr",
  "staff_headcount_total","staff_headcount_instructional","staff_total_headcount_pct_change_5yr","staff_instructional_headcount_pct_change_5yr","staff_change_1yr",
  "students_per_instructional_staff_fte","sector_median_students_per_instructional_staff_fte",
  "revenue_total","revenue_pct_change_5yr","revenue_decreased_5yr","revenue_change_1yr","expenses_total","loss_amount",
  "net_tuition_per_fte","net_tuition_per_fte_change_5yr","tuition_dependence_pct",
  "discount_rate","discount_pct_change_5yr",
  "federal_grants_contracts_pell_adjusted","federal_grants_contracts_pell_adjusted_pct_core_revenue","federal_grants_contracts_pell_adjusted_pct_change_5yr",
  "state_funding","state_funding_pct_core_revenue","state_funding_pct_change_5yr",
  "research_expense","research_expense_per_fte","research_expense_pct_core_expenses","sector_research_spending_n","research_spending_per_fte_percentile","research_spending_peer_bucket",
  "endowment_value","endowment_pct_change_5yr",
  "liquidity","liquidity_percentile_private_nfp","leverage","leverage_percentile_private_nfp",
  "loan_year_latest","federal_loan_pct_most_recent","federal_loan_count_most_recent","federal_loan_avg_most_recent",
  "grad_plus_recipients","grad_plus_disbursements_amt","grad_plus_disbursements_per_recipient",
  "high_tuition_dependence","high_international_share","high_federal_dependence","multi_signal_score"
)

missing_all_sheet_columns <- setdiff(all_sheet_columns, names(latest))
for (nm in missing_all_sheet_columns) {
  latest[[nm]] <- NA
}
all_sheet <- latest[, all_sheet_columns, drop = FALSE]
all_sheet_bacc <- all_sheet[all_sheet$category == bacc_category_label, , drop = FALSE]


enr35 <- sort_df(all_sheet_bacc[all_sheet_bacc$enrollment_decline_last_3_of_5 == "Yes", , drop = FALSE], c("enrollment_pct_change_5yr","revenue_pct_change_5yr","loss_amount"))
rev35 <- sort_df(all_sheet_bacc[all_sheet_bacc$revenue_10pct_drop_last_3_of_5 == "Yes", , drop = FALSE], c("revenue_pct_change_5yr","loss_amount","enrollment_pct_change_5yr"))
red35 <- sort_df(all_sheet_bacc[all_sheet_bacc$losses_last_3_of_5 == "Yes", , drop = FALSE], c("loss_amount"))
enrrev <- sort_df(all_sheet_bacc[all_sheet_bacc$enrollment_decline_last_3_of_5 == "Yes" & all_sheet_bacc$revenue_10pct_drop_last_3_of_5 == "Yes", , drop = FALSE], c("enrollment_pct_change_5yr","revenue_pct_change_5yr","loss_amount"))
enrred <- sort_df(all_sheet_bacc[all_sheet_bacc$enrollment_decline_last_3_of_5 == "Yes" & all_sheet_bacc$losses_last_3_of_5 == "Yes", , drop = FALSE], c("enrollment_pct_change_5yr","revenue_pct_change_5yr","loss_amount"))
all3 <- sort_df(all_sheet_bacc[all_sheet_bacc$enrollment_decline_last_3_of_5 == "Yes" & all_sheet_bacc$revenue_10pct_drop_last_3_of_5 == "Yes" & all_sheet_bacc$losses_last_3_of_5 == "Yes", , drop = FALSE], c("enrollment_pct_change_5yr","revenue_pct_change_5yr","loss_amount"))
intl5 <- all_sheet_bacc[all_sheet_bacc$international_enrollment_increase_5yr == "Yes", , drop = FALSE]
if (nrow(intl5) > 0) intl5 <- intl5[order(-xtfrm(intl5$international_student_count_change_5yr), -xtfrm(intl5$international_enrollment_pct_change_5yr), na.last = TRUE), , drop = FALSE]
intl10 <- all_sheet_bacc[all_sheet_bacc$international_enrollment_increase_10yr == "Yes", , drop = FALSE]
if (nrow(intl10) > 0) intl10 <- intl10[order(-xtfrm(intl10$international_enrollment_change_10yr), -xtfrm(intl10$international_student_count_change_5yr), na.last = TRUE), , drop = FALSE]
flagships <- sort_df(all_sheet_bacc[all_sheet_bacc$control_label == "Public" & as.integer(all_sheet_bacc$unitid) %in% flagship_unitids, , drop = FALSE], c("enrollment_pct_change_5yr","revenue_pct_change_5yr","loss_amount"))
flagship_fed <- all_sheet_bacc[all_sheet_bacc$control_label == "Public" & as.integer(all_sheet_bacc$unitid) %in% flagship_unitids & !is.na(all_sheet_bacc$federal_grants_contracts_pell_adjusted_pct_core_revenue), , drop = FALSE]
if (nrow(flagship_fed) > 0) flagship_fed <- flagship_fed[order(-xtfrm(flagship_fed$federal_grants_contracts_pell_adjusted_pct_core_revenue), -xtfrm(flagship_fed$federal_grants_contracts_pell_adjusted), na.last = TRUE), , drop = FALSE]
research_leaders <- all_sheet_bacc[
  !is.na(all_sheet_bacc$research_expense_per_fte) & all_sheet_bacc$research_expense_per_fte > 0,
  , drop = FALSE]
if (nrow(research_leaders) > 0) {
  research_leaders <- research_leaders[
    order(
      -xtfrm(research_leaders$research_expense_per_fte),
      -xtfrm(research_leaders$research_expense_pct_core_expenses),
      -xtfrm(research_leaders$research_expense),
      na.last = TRUE
    ),
    , drop = FALSE
  ]
}
loss2024 <- sort_df(all_sheet_bacc[all_sheet_bacc$ended_year_at_loss == "Yes", , drop = FALSE], c("loss_amount"))
state_down5yr <- sort_df(all_sheet_bacc[!is.na(all_sheet_bacc$state_funding_pct_change_5yr) & all_sheet_bacc$state_funding_pct_change_5yr < 0, , drop = FALSE], c("state_funding_pct_change_5yr"))
endow_down5yr <- sort_df(all_sheet_bacc[!is.na(all_sheet_bacc$endowment_pct_change_5yr) & all_sheet_bacc$endowment_pct_change_5yr < 0, , drop = FALSE], c("endowment_pct_change_5yr"))
disc_up5yr <- all_sheet_bacc[!is.na(all_sheet_bacc$discount_pct_change_5yr) & all_sheet_bacc$discount_pct_change_5yr > 0, , drop = FALSE]
if (nrow(disc_up5yr) > 0) disc_up5yr <- disc_up5yr[order(-xtfrm(disc_up5yr$discount_pct_change_5yr), na.last = TRUE), , drop = FALSE]
enroll_down5yr <- sort_df(all_sheet_bacc[!is.na(all_sheet_bacc$enrollment_pct_change_5yr) & all_sheet_bacc$enrollment_pct_change_5yr < 0, , drop = FALSE], c("enrollment_pct_change_5yr"))
rev_down5yr <- sort_df(all_sheet_bacc[!is.na(all_sheet_bacc$revenue_pct_change_5yr) & all_sheet_bacc$revenue_pct_change_5yr < 0, , drop = FALSE], c("revenue_pct_change_5yr"))
staff_down5yr <- sort_df(all_sheet_bacc[!is.na(all_sheet_bacc$staff_total_headcount_pct_change_5yr) & all_sheet_bacc$staff_total_headcount_pct_change_5yr < 0, , drop = FALSE], c("staff_total_headcount_pct_change_5yr","staff_change_1yr"))
instr_staff_down5yr <- sort_df(all_sheet_bacc[!is.na(all_sheet_bacc$staff_instructional_headcount_pct_change_5yr) & all_sheet_bacc$staff_instructional_headcount_pct_change_5yr < 0, , drop = FALSE], c("staff_instructional_headcount_pct_change_5yr","staff_change_1yr"))
staff_net_tuition_down <- all_sheet_bacc[
  !is.na(all_sheet_bacc$staff_total_headcount_pct_change_5yr) &
    !is.na(all_sheet_bacc$net_tuition_per_fte_change_5yr) &
    all_sheet_bacc$staff_total_headcount_pct_change_5yr < 0 &
    all_sheet_bacc$net_tuition_per_fte_change_5yr < 0,
  , drop = FALSE]
if (nrow(staff_net_tuition_down) > 0) {
  staff_net_tuition_down <- staff_net_tuition_down[
    order(
      xtfrm(staff_net_tuition_down$staff_total_headcount_pct_change_5yr),
      xtfrm(staff_net_tuition_down$net_tuition_per_fte_change_5yr),
      xtfrm(staff_net_tuition_down$revenue_pct_change_5yr),
      na.last = TRUE
    ),
    , drop = FALSE
  ]
}
staff_cut_risk <- all_sheet_bacc[!is.na(all_sheet_bacc$staffing_cut_risk_score), , drop = FALSE]
if (nrow(staff_cut_risk) > 0) staff_cut_risk <- staff_cut_risk[order(-xtfrm(staff_cut_risk$staffing_cut_risk_score), xtfrm(staff_cut_risk$staff_total_headcount_pct_change_5yr), xtfrm(staff_cut_risk$net_tuition_per_fte_change_5yr), xtfrm(staff_cut_risk$revenue_pct_change_5yr), na.last = TRUE), , drop = FALSE]
transfer_out_up5yr <- all_sheet_bacc[!is.na(all_sheet_bacc$transfer_out_rate_bachelor_change_5yr) & all_sheet_bacc$transfer_out_rate_bachelor_change_5yr > 0, , drop = FALSE]
if (nrow(transfer_out_up5yr) > 0) transfer_out_up5yr <- transfer_out_up5yr[order(-xtfrm(transfer_out_up5yr$transfer_out_rate_bachelor_change_5yr), -xtfrm(transfer_out_up5yr$transfer_out_rate_bachelor), na.last = TRUE), , drop = FALSE]
transfer_out_up10yr <- all_sheet_bacc[!is.na(all_sheet_bacc$transfer_out_rate_bachelor_change_10yr) & all_sheet_bacc$transfer_out_rate_bachelor_change_10yr > 0, , drop = FALSE]
if (nrow(transfer_out_up10yr) > 0) transfer_out_up10yr <- transfer_out_up10yr[order(-xtfrm(transfer_out_up10yr$transfer_out_rate_bachelor_change_10yr), -xtfrm(transfer_out_up10yr$transfer_out_rate_bachelor), na.last = TRUE), , drop = FALSE]
fed_depend <- all_sheet_bacc[!is.na(all_sheet_bacc$federal_grants_contracts_pell_adjusted_pct_core_revenue), , drop = FALSE]
if (nrow(fed_depend) > 0) fed_depend <- fed_depend[order(-xtfrm(fed_depend$federal_grants_contracts_pell_adjusted_pct_core_revenue), -xtfrm(fed_depend$federal_grants_contracts_pell_adjusted), na.last = TRUE), , drop = FALSE]
state_depend <- all_sheet_bacc[!is.na(all_sheet_bacc$state_funding_pct_core_revenue), , drop = FALSE]
if (nrow(state_depend) > 0) state_depend <- state_depend[order(-xtfrm(state_depend$state_funding_pct_core_revenue), -xtfrm(state_depend$state_funding), na.last = TRUE), , drop = FALSE]
years_at_loss <- all_sheet_bacc[!is.na(all_sheet_bacc$loss_years_last_10), , drop = FALSE]
if (nrow(years_at_loss) > 0) years_at_loss <- years_at_loss[order(-xtfrm(years_at_loss$loss_years_last_10), -xtfrm(years_at_loss$loss_years_last_5), xtfrm(years_at_loss$loss_amount), na.last = TRUE), , drop = FALSE]
tuition_depend <- all_sheet_bacc[!is.na(all_sheet_bacc$tuition_dependence_pct), , drop = FALSE]
if (nrow(tuition_depend) > 0) tuition_depend <- tuition_depend[order(-xtfrm(tuition_depend$tuition_dependence_pct), xtfrm(tuition_depend$enrollment_pct_change_5yr), xtfrm(tuition_depend$revenue_pct_change_5yr), na.last = TRUE), , drop = FALSE]
net_tuition_down <- all_sheet_bacc[!is.na(all_sheet_bacc$net_tuition_per_fte_change_5yr) & all_sheet_bacc$net_tuition_per_fte_change_5yr < 0, , drop = FALSE]
if (nrow(net_tuition_down) > 0) net_tuition_down <- net_tuition_down[order(xtfrm(net_tuition_down$net_tuition_per_fte_change_5yr), xtfrm(net_tuition_down$revenue_pct_change_5yr), na.last = TRUE), , drop = FALSE]
intl_share <- all_sheet_bacc[!is.na(all_sheet_bacc$pct_international_all), , drop = FALSE]
if (nrow(intl_share) > 0) intl_share <- intl_share[order(-xtfrm(intl_share$pct_international_all), -xtfrm(intl_share$pct_international_graduate), na.last = TRUE), , drop = FALSE]
low_cushion <- all_sheet_bacc[all_sheet_bacc$control_label == "Private not-for-profit" & !is.na(all_sheet_bacc$liquidity), , drop = FALSE]
if (nrow(low_cushion) > 0) low_cushion <- low_cushion[order(xtfrm(low_cushion$liquidity), -xtfrm(low_cushion$leverage), na.last = TRUE), , drop = FALSE]
high_debt <- all_sheet_bacc[!is.na(all_sheet_bacc$leverage), , drop = FALSE]
if (nrow(high_debt) > 0) high_debt <- high_debt[order(-xtfrm(high_debt$leverage), xtfrm(high_debt$liquidity), na.last = TRUE), , drop = FALSE]
fed_and_intl <- all_sheet_bacc[all_sheet_bacc$high_federal_dependence & all_sheet_bacc$high_international_share, , drop = FALSE]
if (nrow(fed_and_intl) > 0) fed_and_intl <- fed_and_intl[order(-xtfrm(fed_and_intl$federal_grants_contracts_pell_adjusted_pct_core_revenue), -xtfrm(fed_and_intl$pct_international_all), xtfrm(fed_and_intl$revenue_pct_change_5yr), na.last = TRUE), , drop = FALSE]
multi_signal <- all_sheet_bacc[!is.na(all_sheet_bacc$multi_signal_score), , drop = FALSE]
if (nrow(multi_signal) > 0) multi_signal <- multi_signal[order(-xtfrm(multi_signal$multi_signal_score), -xtfrm(multi_signal$warning_score_core), xtfrm(multi_signal$revenue_pct_change_5yr), xtfrm(multi_signal$enrollment_pct_change_5yr), na.last = TRUE), , drop = FALSE]
private_close_risk <- all_sheet_bacc[all_sheet_bacc$control_label != "Public" & !is.na(all_sheet_bacc$private_closure_risk_score), , drop = FALSE]
if (nrow(private_close_risk) > 0) private_close_risk <- private_close_risk[order(-xtfrm(private_close_risk$private_closure_risk_score), xtfrm(private_close_risk$net_tuition_per_fte_change_5yr), xtfrm(private_close_risk$revenue_pct_change_5yr), xtfrm(private_close_risk$enrollment_pct_change_5yr), na.last = TRUE), , drop = FALSE]
public_campus_risk <- all_sheet_bacc[all_sheet_bacc$control_label == "Public" & !(as.integer(all_sheet_bacc$unitid) %in% flagship_unitids) & !is.na(all_sheet_bacc$public_campus_risk_score), , drop = FALSE]
if (nrow(public_campus_risk) > 0) public_campus_risk <- public_campus_risk[order(-xtfrm(public_campus_risk$public_campus_risk_score), xtfrm(public_campus_risk$enrollment_pct_change_5yr), xtfrm(public_campus_risk$state_funding_pct_change_5yr), na.last = TRUE), , drop = FALSE]
students_per_instr_top50 <- all_sheet_bacc[!is.na(all_sheet_bacc$students_per_instructional_staff_fte), , drop = FALSE]
if (nrow(students_per_instr_top50) > 0) {
  students_per_instr_top50 <- students_per_instr_top50[
    order(
      -xtfrm(students_per_instr_top50$students_per_instructional_staff_fte),
      xtfrm(students_per_instr_top50$institution_name),
      na.last = TRUE
    ),
    , drop = FALSE
  ]
  students_per_instr_top50 <- utils::head(students_per_instr_top50, 50)
}

# Mirror the finance-page card logic so workbook rankings match what readers see on school pages.
finance_bad <- all_sheet_bacc
finance_bad$bad_revenue_change <- !is.na(finance_bad$revenue_pct_change_5yr) & finance_bad$revenue_pct_change_5yr <= -5
finance_bad$bad_latest_loss <- yes_flag(finance_bad$ended_year_at_loss)
finance_bad$bad_repeat_losses <- yes_flag(finance_bad$losses_last_3_of_5)
finance_bad$bad_net_tuition_change <- !is.na(finance_bad$net_tuition_per_fte_change_5yr) & finance_bad$net_tuition_per_fte_change_5yr <= -5
finance_bad$bad_enrollment_change <- !is.na(finance_bad$enrollment_pct_change_5yr) & finance_bad$enrollment_pct_change_5yr <= -5
finance_bad$bad_enrollment_flag <- yes_flag(finance_bad$enrollment_decline_last_3_of_5)
finance_bad$bad_staff_change <- !is.na(finance_bad$staff_total_headcount_pct_change_5yr) & finance_bad$staff_total_headcount_pct_change_5yr <= -5
finance_bad$bad_endowment_change <- !is.na(finance_bad$endowment_pct_change_5yr) & finance_bad$endowment_pct_change_5yr <= -5
finance_bad$bad_federal_change <- !is.na(finance_bad$federal_grants_contracts_pell_adjusted_pct_change_5yr) & finance_bad$federal_grants_contracts_pell_adjusted_pct_change_5yr <= -5
finance_bad$bad_state_change <- !is.na(finance_bad$state_funding_pct_change_5yr) & finance_bad$state_funding_pct_change_5yr <= -5
finance_bad$finance_page_bad_count <- row_score(
  finance_bad$bad_revenue_change,
  finance_bad$bad_latest_loss,
  finance_bad$bad_repeat_losses,
  finance_bad$bad_net_tuition_change,
  finance_bad$bad_enrollment_change,
  finance_bad$bad_enrollment_flag,
  finance_bad$bad_staff_change,
  finance_bad$bad_endowment_change,
  finance_bad$bad_federal_change,
  finance_bad$bad_state_change
)

# Compare institutions with accreditation actions or program cuts against the
# same 2024 primarily bachelor's tracker universe used throughout the workbook.
accreditation_summary <- read_csv_if_exists("./data_pipelines/accreditation/accreditation_tracker_institution_summary.csv")
college_cuts_summary <- read_csv_if_exists("./data_pipelines/college_cuts/college_cuts_financial_tracker_institution_summary.csv")
hcm_summary <- read_csv_if_exists("./data_pipelines/federal_hcm/hcm_level2_summary.csv")
hcm_all <- read_csv_if_exists("./data_pipelines/federal_hcm/hcm_level2_snapshots_2024_2025.csv")
hcm_dec24_drop <- read_csv_if_exists("./data_pipelines/federal_hcm/hcm2_dec2024_dropped_since.csv")
hcm_mar25_drop <- read_csv_if_exists("./data_pipelines/federal_hcm/hcm2_mar2025_dropped_since.csv")
hcm_jun25_drop <- read_csv_if_exists("./data_pipelines/federal_hcm/hcm2_jun2025_dropped_since.csv")
hcm_dec24_stay <- read_csv_if_exists("./data_pipelines/federal_hcm/hcm2_dec2024_remained_since.csv")
hcm_mar25_stay <- read_csv_if_exists("./data_pipelines/federal_hcm/hcm2_mar2025_remained_since.csv")
# Closure tabs are now imported from the published closure Google Sheet so this
# workbook stays decoupled from the heavier federal scraping workflow.
running_closures <- read_required_closure_csv("./data_pipelines/federal_closure/derived/running_closures.csv")
main_campus_closures <- read_required_closure_csv("./data_pipelines/federal_closure/derived/main_campus_closures.csv")
branch_campus_closures <- read_required_closure_csv("./data_pipelines/federal_closure/derived/branch_campus_closures.csv")
mergers_consol <- read_required_closure_csv("./data_pipelines/federal_closure/derived/mergers_consolidations.csv")
private_federal_main_closures <- read_required_closure_csv("./data_pipelines/federal_closure/derived/private_sector_federal_main_closures.csv")
accreditation_summary$unitid <- to_num(accreditation_summary$unitid)
college_cuts_summary$matched_unitid <- to_num(college_cuts_summary$matched_unitid)

# Keep the audit tabs tied to the same finance-page universe so the toplines are
# apples-to-apples with the main tracker rather than the broader source files.
accreditation_summary_bacc <- merge(
  finance_bad,
  accreditation_summary[
    !is.na(accreditation_summary$unitid) & accreditation_summary$action_count > 0,
    c(
      "unitid","tracker_name","tracker_state","accreditors","action_types","action_labels",
      "active_actions","has_active_warning","has_active_warning_or_notice","has_active_adverse_action",
      "action_count","latest_action_date","latest_action_year"
    ),
    drop = FALSE
  ],
  by = "unitid",
  all.x = FALSE,
  all.y = FALSE
)
if (nrow(accreditation_summary_bacc) > 0) {
  accreditation_summary_bacc <- accreditation_summary_bacc[
    order(
      -xtfrm(accreditation_summary_bacc$action_count),
      -xtfrm(accreditation_summary_bacc$finance_page_bad_count),
      -xtfrm(accreditation_summary_bacc$warning_score_core),
      xtfrm(accreditation_summary_bacc$revenue_pct_change_5yr),
      na.last = TRUE
    ),
    , drop = FALSE
  ]
}

college_cuts_summary_bacc <- merge(
  finance_bad,
  college_cuts_summary[
    !is.na(college_cuts_summary$matched_unitid) & college_cuts_summary$cut_records > 0,
    c(
      "matched_unitid","institution_name_collegecuts","institution_state_full","cut_records","cut_types",
      "latest_cut_announcement_date","first_cut_announcement_date","total_students_affected_known",
      "total_faculty_affected_known","staff_layoff_records","program_suspension_records",
      "department_closure_records","campus_closure_records","institution_closure_records",
      "teach_out_records","financial_warning_count"
    ),
    drop = FALSE
  ],
  by.x = "unitid",
  by.y = "matched_unitid",
  all.x = FALSE,
  all.y = FALSE
)
if (nrow(college_cuts_summary_bacc) > 0) {
  college_cuts_summary_bacc <- college_cuts_summary_bacc[
    order(
      -xtfrm(college_cuts_summary_bacc$cut_records),
      -xtfrm(college_cuts_summary_bacc$finance_page_bad_count),
      -xtfrm(college_cuts_summary_bacc$warning_score_core),
      xtfrm(college_cuts_summary_bacc$revenue_pct_change_5yr),
      na.last = TRUE
    ),
    , drop = FALSE
  ]
}

# summarize_event_subset() and build_event_xtab() are in workbook_helpers.R

accreditation_any_unitids <- accreditation_summary_bacc$unitid
accreditation_warning_unitids <- accreditation_summary_bacc$unitid[trimws(tolower(as.character(accreditation_summary_bacc$has_active_warning_or_notice))) == "true"]
accreditation_adverse_unitids <- accreditation_summary_bacc$unitid[trimws(tolower(as.character(accreditation_summary_bacc$has_active_adverse_action))) == "true"]
college_cuts_any_unitids <- college_cuts_summary_bacc$unitid

accredit_finance_xtab <- rbind(
  build_event_xtab(finance_bad, accreditation_any_unitids, "Any accreditation action since 2019"),
  build_event_xtab(finance_bad, accreditation_warning_unitids, "Active warning/notice"),
  build_event_xtab(finance_bad, accreditation_adverse_unitids, "Active adverse action")
)

cuts_finance_xtab <- build_event_xtab(finance_bad, college_cuts_any_unitids, "Any college cut")

public_fin_bad50 <- finance_bad[finance_bad$control_label == "Public", , drop = FALSE]
if (nrow(public_fin_bad50) > 0) {
  public_fin_bad50 <- public_fin_bad50[
    order(
      -xtfrm(public_fin_bad50$finance_page_bad_count),
      -xtfrm(public_fin_bad50$warning_score_core),
      xtfrm(public_fin_bad50$revenue_pct_change_5yr),
      xtfrm(public_fin_bad50$enrollment_pct_change_5yr),
      na.last = TRUE
    ),
    , drop = FALSE
  ]
  public_fin_bad50 <- utils::head(public_fin_bad50, 50)
}

private_fin_bad50 <- finance_bad[finance_bad$control_label != "Public", , drop = FALSE]
if (nrow(private_fin_bad50) > 0) {
  private_fin_bad50 <- private_fin_bad50[
    order(
      -xtfrm(private_fin_bad50$finance_page_bad_count),
      -xtfrm(private_fin_bad50$warning_score_core),
      xtfrm(private_fin_bad50$revenue_pct_change_5yr),
      xtfrm(private_fin_bad50$enrollment_pct_change_5yr),
      na.last = TRUE
    ),
    , drop = FALSE
  ]
  private_fin_bad50 <- utils::head(private_fin_bad50, 50)
}

loss_tuition <- all_sheet_bacc[!is.na(all_sheet_bacc$loss_years_last_10) & !is.na(all_sheet_bacc$tuition_dependence_pct), , drop = FALSE]
if (nrow(loss_tuition) > 0) loss_tuition <- loss_tuition[order(-xtfrm(loss_tuition$loss_years_last_10), -xtfrm(loss_tuition$tuition_dependence_pct), xtfrm(loss_tuition$revenue_pct_change_5yr), na.last = TRUE), , drop = FALSE]
priv_nfp_stress <- all_sheet_bacc[all_sheet_bacc$control_label == "Private not-for-profit" & !is.na(all_sheet_bacc$discount_pct_change_5yr) & !is.na(all_sheet_bacc$net_tuition_per_fte_change_5yr) & all_sheet_bacc$discount_pct_change_5yr > 0 & all_sheet_bacc$net_tuition_per_fte_change_5yr < 0, , drop = FALSE]
if (nrow(priv_nfp_stress) > 0) priv_nfp_stress <- priv_nfp_stress[order(-xtfrm(priv_nfp_stress$discount_pct_change_5yr), xtfrm(priv_nfp_stress$net_tuition_per_fte_change_5yr), na.last = TRUE), , drop = FALSE]
multi_front <- all_sheet_bacc[
  !is.na(all_sheet_bacc$pct_international_all) &
    !is.na(all_sheet_bacc$federal_grants_contracts_pell_adjusted_pct_core_revenue) &
    !is.na(all_sheet_bacc$revenue_pct_change_5yr) &
    !is.na(all_sheet_bacc$loss_years_last_10) &
    all_sheet_bacc$high_international_share &
    all_sheet_bacc$high_federal_dependence &
    all_sheet_bacc$revenue_pct_change_5yr < 0 &
    all_sheet_bacc$loss_years_last_10 >= 3,
  , drop = FALSE]
if (nrow(multi_front) > 0) multi_front <- multi_front[order(-xtfrm(multi_front$federal_grants_contracts_pell_adjusted_pct_core_revenue), -xtfrm(multi_front$pct_international_all), -xtfrm(multi_front$loss_years_last_10), xtfrm(multi_front$revenue_pct_change_5yr), na.last = TRUE), , drop = FALSE]

distress_core <- all_sheet_bacc[
  !is.na(all_sheet_bacc$warning_score_core) &
    all_sheet_bacc$warning_score_core >= 4,
  , drop = FALSE]
if (nrow(distress_core) > 0) distress_core <- distress_core[order(-xtfrm(distress_core$warning_score_core), xtfrm(distress_core$revenue_pct_change_5yr), xtfrm(distress_core$enrollment_pct_change_5yr), na.last = TRUE), , drop = FALSE]

distress_intl10 <- all_sheet_bacc[
  !is.na(all_sheet_bacc$warning_score_core) &
    all_sheet_bacc$warning_score_core >= 4 &
    all_sheet_bacc$international_enrollment_increase_10yr == "Yes",
  , drop = FALSE]
if (nrow(distress_intl10) > 0) distress_intl10 <- distress_intl10[order(-xtfrm(distress_intl10$warning_score_core), -xtfrm(distress_intl10$international_enrollment_change_10yr), na.last = TRUE), , drop = FALSE]

# Year-over-year staffing cuts: keep the existing institution counts, but also
# sum the actual headcount reductions so the workbook shows how many jobs were
# cut rather than only how many campuses reported a decline. Include 2014 as
# the visible staffing baseline, then show later years as cuts relative to the
# prior year's staffing base.
staff_cut_baseline_2014 <- {
  base <- read_df[
    as.integer(read_df$year) == 2014L,
    c("unitid", "staff_headcount_total", "staff_headcount_instructional"),
    drop = FALSE
  ]
  base_total <- base[!is.na(base$staff_headcount_total), , drop = FALSE]
  base_instr <- base[!is.na(base$staff_headcount_instructional), , drop = FALSE]
  data.frame(
    year = 2014L,
    total_staff_headcount_year = if (nrow(base_total) == 0) NA_real_ else sum(base_total$staff_headcount_total, na.rm = TRUE),
    total_instructional_staff_headcount_year = if (nrow(base_instr) == 0) NA_real_ else sum(base_instr$staff_headcount_instructional, na.rm = TRUE),
    prior_year_total_staff_headcount = NA_real_,
    prior_year_total_instructional_staff_headcount = NA_real_,
    institutions_with_staff_data = nrow(base_total),
    institutions_cutting_staff = NA_real_,
    institutions_increasing_staff = NA_real_,
    institutions_flat_staff = NA_real_,
    total_staff_positions_cut = NA_real_,
    total_staff_positions_added = NA_real_,
    net_staff_change = NA_real_,
    total_staff_positions_cut_pct_of_prior_year = NA_real_,
    institutions_with_instructional_staff_data = nrow(base_instr),
    institutions_cutting_instructional_staff = NA_real_,
    institutions_increasing_instructional_staff = NA_real_,
    institutions_flat_instructional_staff = NA_real_,
    total_instructional_positions_cut = NA_real_,
    total_instructional_positions_added = NA_real_,
    net_instructional_change = NA_real_,
    total_instructional_positions_cut_pct_of_prior_year = NA_real_,
    stringsAsFactors = FALSE
  )
}

staff_cut_yoy_comparisons <- do.call(rbind, lapply(2015:2024, function(y) {
  prev <- read_df[
    as.integer(read_df$year) == (y - 1L),
    c("unitid", "staff_headcount_total", "staff_headcount_instructional"),
    drop = FALSE
  ]
  curr <- read_df[
    as.integer(read_df$year) == y,
    c("unitid", "staff_headcount_total", "staff_headcount_instructional"),
    drop = FALSE
  ]
  names(prev)[2:3] <- c("staff_prev", "instructional_prev")
  names(curr)[2:3] <- c("staff_curr", "instructional_curr")
  joined <- merge(prev, curr, by = "unitid", all = FALSE)

  joined_total <- joined[!is.na(joined$staff_prev) & !is.na(joined$staff_curr), , drop = FALSE]
  joined_instr <- joined[!is.na(joined$instructional_prev) & !is.na(joined$instructional_curr), , drop = FALSE]

  prior_total_staff_headcount <- if (nrow(joined_total) == 0) NA_real_ else sum(joined_total$staff_prev, na.rm = TRUE)
  current_total_staff_headcount <- if (nrow(joined_total) == 0) NA_real_ else sum(joined_total$staff_curr, na.rm = TRUE)
  prior_instructional_staff_headcount <- if (nrow(joined_instr) == 0) NA_real_ else sum(joined_instr$instructional_prev, na.rm = TRUE)
  current_instructional_staff_headcount <- if (nrow(joined_instr) == 0) NA_real_ else sum(joined_instr$instructional_curr, na.rm = TRUE)
  total_cut_amount <- if (nrow(joined_total) == 0) 0 else sum(pmax(joined_total$staff_prev - joined_total$staff_curr, 0), na.rm = TRUE)
  total_added_amount <- if (nrow(joined_total) == 0) 0 else sum(pmax(joined_total$staff_curr - joined_total$staff_prev, 0), na.rm = TRUE)
  instructional_cut_amount <- if (nrow(joined_instr) == 0) 0 else sum(pmax(joined_instr$instructional_prev - joined_instr$instructional_curr, 0), na.rm = TRUE)
  instructional_added_amount <- if (nrow(joined_instr) == 0) 0 else sum(pmax(joined_instr$instructional_curr - joined_instr$instructional_prev, 0), na.rm = TRUE)

  data.frame(
    year = y,
    total_staff_headcount_year = current_total_staff_headcount,
    total_instructional_staff_headcount_year = current_instructional_staff_headcount,
    prior_year_total_staff_headcount = prior_total_staff_headcount,
    prior_year_total_instructional_staff_headcount = prior_instructional_staff_headcount,
    institutions_with_staff_data = nrow(joined_total),
    institutions_cutting_staff = sum(joined_total$staff_curr < joined_total$staff_prev, na.rm = TRUE),
    institutions_increasing_staff = sum(joined_total$staff_curr > joined_total$staff_prev, na.rm = TRUE),
    institutions_flat_staff = sum(joined_total$staff_curr == joined_total$staff_prev, na.rm = TRUE),
    total_staff_positions_cut = total_cut_amount,
    total_staff_positions_added = total_added_amount,
    net_staff_change = total_added_amount - total_cut_amount,
    total_staff_positions_cut_pct_of_prior_year = safe_pct(total_cut_amount, prior_total_staff_headcount),
    institutions_with_instructional_staff_data = nrow(joined_instr),
    institutions_cutting_instructional_staff = sum(joined_instr$instructional_curr < joined_instr$instructional_prev, na.rm = TRUE),
    institutions_increasing_instructional_staff = sum(joined_instr$instructional_curr > joined_instr$instructional_prev, na.rm = TRUE),
    institutions_flat_instructional_staff = sum(joined_instr$instructional_curr == joined_instr$instructional_prev, na.rm = TRUE),
    total_instructional_positions_cut = instructional_cut_amount,
    total_instructional_positions_added = instructional_added_amount,
    net_instructional_change = instructional_added_amount - instructional_cut_amount,
    total_instructional_positions_cut_pct_of_prior_year = safe_pct(instructional_cut_amount, prior_instructional_staff_headcount),
    stringsAsFactors = FALSE
  )
}))

staff_cut_yoy <- rbind(staff_cut_baseline_2014, staff_cut_yoy_comparisons)

public_fed_top <- all_sheet_bacc[all_sheet_bacc$control_label == "Public" & !is.na(all_sheet_bacc$federal_grants_contracts_pell_adjusted_pct_core_revenue), , drop = FALSE]
if (nrow(public_fed_top) > 0) {
  public_fed_top <- public_fed_top[order(-xtfrm(public_fed_top$federal_grants_contracts_pell_adjusted_pct_core_revenue), -xtfrm(public_fed_top$research_expense_per_fte), na.last = TRUE), , drop = FALSE]
  public_fed_top$federal_grants_contracts_pct_core_revenue_pct <- public_fed_top$federal_grants_contracts_pell_adjusted_pct_core_revenue * 100
}

grad_depend_top <- all_sheet_bacc[!is.na(all_sheet_bacc$share_grad_students), , drop = FALSE]
if (nrow(grad_depend_top) > 0) {
  grad_depend_top <- grad_depend_top[order(-xtfrm(grad_depend_top$share_grad_students), -xtfrm(grad_depend_top$grad_plus_disbursements_per_recipient), na.last = TRUE), , drop = FALSE]
  grad_depend_top$share_grad_students_pct <- grad_depend_top$share_grad_students * 100
}

public_grad_top <- all_sheet_bacc[all_sheet_bacc$control_label == "Public" & !is.na(all_sheet_bacc$share_grad_students), , drop = FALSE]
if (nrow(public_grad_top) > 0) {
  public_grad_top <- public_grad_top[order(-xtfrm(public_grad_top$share_grad_students), -xtfrm(public_grad_top$grad_plus_disbursements_per_recipient), na.last = TRUE), , drop = FALSE]
  public_grad_top$share_grad_students_pct <- public_grad_top$share_grad_students * 100
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
research_json_path <- "./data/research_funding.json"
flagship_cuts <- data.frame()
if (file.exists(research_json_path)) {
  research_json <- jsonlite::fromJSON(research_json_path, simplifyVector = FALSE)
  research_school_rows <- lapply(research_json$schools, function(x) {
    data.frame(
      unitid = suppressWarnings(as.numeric(x$unitid %||% NA)),
      institution_name = x$institution_name %||% NA_character_,
      state = x$state %||% NA_character_,
      control_label = x$control_label %||% NA_character_,
      total_disrupted_grants = suppressWarnings(as.numeric(x$total_disrupted_grants %||% NA)),
      total_disrupted_award_remaining = suppressWarnings(as.numeric(x$total_disrupted_award_remaining %||% NA)),
      latest_termination_date = x$latest_termination_date %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })
  research_school_df <- dplyr::bind_rows(research_school_rows)
  flagship_cuts <- research_school_df[
    research_school_df$control_label == "Public" &
      as.integer(research_school_df$unitid) %in% flagship_unitids &
      !is.na(research_school_df$total_disrupted_award_remaining) &
      research_school_df$total_disrupted_award_remaining > 0,
    , drop = FALSE]
if (nrow(flagship_cuts) > 0) {
    flagship_cuts <- flagship_cuts[order(-xtfrm(flagship_cuts$total_disrupted_award_remaining), -xtfrm(flagship_cuts$total_disrupted_grants), na.last = TRUE), , drop = FALSE]
  }
}

calc_distress_compare <- function(year_value) {
  year_df <- read_df[
    as.integer(read_df$year) == as.integer(year_value) &
      read_df$category == bacc_category_label,
    , drop = FALSE]
  year_df$warning_score_core <- compute_warning_score_core(year_df)
  distress_df <- year_df[
    !is.na(year_df$warning_score_core) &
      year_df$warning_score_core >= 4,
    , drop = FALSE]
  longrun_df <- year_df[
    year_df$enrollment_decline_last_3_of_5 == "Yes" &
      year_df$losses_last_3_of_5 == "Yes",
    , drop = FALSE]
  data.frame(
    year = year_value,
    institutions_total = nrow(year_df),
    distress_count = nrow(distress_df),
    distress_pct = safe_pct(nrow(distress_df), nrow(year_df)) * 100,
    enrollment_drop_10pct_count = sum(!is.na(year_df$enrollment_pct_change_5yr) & year_df$enrollment_pct_change_5yr <= -10, na.rm = TRUE),
    revenue_drop_10pct_count = sum(!is.na(year_df$revenue_pct_change_5yr) & year_df$revenue_pct_change_5yr <= -10, na.rm = TRUE),
    distress_students = sum(year_df$enrollment_headcount_total[!is.na(year_df$warning_score_core) & year_df$warning_score_core >= 4], na.rm = TRUE),
    longrun_count = nrow(longrun_df),
    longrun_students = sum(longrun_df$enrollment_headcount_total, na.rm = TRUE),
    five_year_enrollment_metrics_available = sum(!is.na(year_df$enrollment_pct_change_5yr)),
    five_year_revenue_metrics_available = sum(!is.na(year_df$revenue_pct_change_5yr)),
    comparison_note = if (sum(!is.na(year_df$enrollment_pct_change_5yr)) == 0 || sum(!is.na(year_df$revenue_pct_change_5yr)) == 0) {
      "Five-year enrollment and revenue trend fields are not available for this year, so the distress comparison is not directly comparable."
    } else {
      "Comparable using the same primarily baccalaureate 4-year universe and warning-score method."
    },
    stringsAsFactors = FALSE
  )
}

distress_compare <- do.call(rbind, lapply(c(2024L, 2019L, 2014L), calc_distress_compare))

intl_base_2014 <- read_df[as.integer(read_df$year) == 2014L, c("unitid","enrollment_headcount_total"), drop = FALSE]
names(intl_base_2014)[2] <- "enrollment_headcount_total_2014"
intl_offset_10yr <- merge(all_sheet_bacc, intl_base_2014, by = "unitid", all.x = TRUE)
intl_offset_10yr$total_change_10yr_proxy <- intl_offset_10yr$enrollment_headcount_total - intl_offset_10yr$enrollment_headcount_total_2014
intl_offset_10yr$domestic_change_10yr_proxy <- intl_offset_10yr$total_change_10yr_proxy - intl_offset_10yr$international_enrollment_change_10yr
intl_offset_10yr$pct_international_all_pct <- intl_offset_10yr$pct_international_all * 100
intl_offset_10yr <- intl_offset_10yr[
  !is.na(intl_offset_10yr$total_change_10yr_proxy) &
    !is.na(intl_offset_10yr$international_enrollment_change_10yr) &
    intl_offset_10yr$international_enrollment_change_10yr > 0 &
    intl_offset_10yr$domestic_change_10yr_proxy < 0,
  , drop = FALSE]
if (nrow(intl_offset_10yr) > 0) {
  intl_offset_10yr <- intl_offset_10yr[order(xtfrm(intl_offset_10yr$domestic_change_10yr_proxy), -xtfrm(intl_offset_10yr$pct_international_all_pct), na.last = TRUE), , drop = FALSE]
}
intl_offset_10yr_ranked <- intl_offset_10yr
if (nrow(intl_offset_10yr_ranked) > 0) {
  intl_offset_10yr_ranked$rank_biggest_drop_if_not_for_internationals <- seq_len(nrow(intl_offset_10yr_ranked))
}

intl_offset_q75 <- if (nrow(intl_offset_10yr) == 0) NA_real_ else q75_safe(intl_offset_10yr$pct_international_all_pct)
intl_vulnerable <- intl_offset_10yr[
  !is.na(intl_offset_q75) &
    !is.na(intl_offset_10yr$pct_international_all_pct) &
    intl_offset_10yr$pct_international_all_pct >= intl_offset_q75 &
    (
      intl_offset_10yr$enrollment_decline_last_3_of_5 == "Yes" |
      intl_offset_10yr$losses_last_3_of_5 == "Yes" |
      intl_offset_10yr$ended_year_at_loss == "Yes" |
      (!is.na(intl_offset_10yr$revenue_pct_change_5yr) & intl_offset_10yr$revenue_pct_change_5yr < 0)
    ),
  , drop = FALSE]
if (nrow(intl_vulnerable) > 0) {
  intl_vulnerable <- intl_vulnerable[order(-xtfrm(intl_vulnerable$pct_international_all_pct), xtfrm(intl_vulnerable$domestic_change_10yr_proxy), na.last = TRUE), , drop = FALSE]
}

intl_vulnerable_large <- intl_vulnerable[!is.na(intl_vulnerable$enrollment_headcount_total) & intl_vulnerable$enrollment_headcount_total >= 5000, , drop = FALSE]

report_answers <- data.frame(
  question = c(
    "2024 distressed institutions in the primarily baccalaureate universe",
    "2024 distressed institutions as a share of the primarily baccalaureate universe",
    "2024 institutions with at least a 10% five-year enrollment drop",
    "2024 institutions with at least a 10% five-year revenue drop",
    "2024 students enrolled at distressed institutions",
    "2024 long-running challenge institutions with enrollment dips and losses in 3 of the last 5 years",
    "2024 students enrolled at those long-running challenge institutions",
    "2019 distressed institutions in the same universe",
    "2019 institutions with at least a 10% five-year enrollment drop",
    "2019 institutions with at least a 10% five-year revenue drop",
    "2019 long-running challenge institutions with enrollment dips and losses in 3 of the last 5 years",
    "2014 comparison note",
    "Colleges in distress with rising international enrollment over 10 years",
    "Public flagships with still-disrupted federal research cuts",
    "Public flagships with at least $1M still disrupted in federal research cuts",
    "Institutions cutting staffing from 2023 to 2024"
  ),
  value = c(
    distress_compare$distress_count[distress_compare$year == 2024],
    distress_compare$distress_pct[distress_compare$year == 2024],
    distress_compare$enrollment_drop_10pct_count[distress_compare$year == 2024],
    distress_compare$revenue_drop_10pct_count[distress_compare$year == 2024],
    distress_compare$distress_students[distress_compare$year == 2024],
    distress_compare$longrun_count[distress_compare$year == 2024],
    distress_compare$longrun_students[distress_compare$year == 2024],
    distress_compare$distress_count[distress_compare$year == 2019],
    distress_compare$enrollment_drop_10pct_count[distress_compare$year == 2019],
    distress_compare$revenue_drop_10pct_count[distress_compare$year == 2019],
    distress_compare$longrun_count[distress_compare$year == 2019],
    distress_compare$comparison_note[distress_compare$year == 2014],
    nrow(distress_intl10),
    nrow(flagship_cuts),
    if (nrow(flagship_cuts) == 0) 0 else sum(flagship_cuts$total_disrupted_award_remaining >= 1e6, na.rm = TRUE),
    staff_cut_yoy$institutions_cutting_staff[staff_cut_yoy$year == 2024]
  ),
  note = c(
    "warning_score_core >= 4 in the 2024 primarily baccalaureate workbook universe.",
    "Share of the 2024 primarily baccalaureate workbook universe with warning_score_core >= 4.",
    "Count of 2024 primarily baccalaureate institutions with enrollment_pct_change_5yr <= -10.",
    "Count of 2024 primarily baccalaureate institutions with revenue_pct_change_5yr <= -10.",
    "Sum of 2024 enrollment_headcount_total for institutions in the distress group.",
    "Count of 2024 institutions where enrollment_decline_last_3_of_5 == Yes and losses_last_3_of_5 == Yes.",
    "Sum of 2024 enrollment_headcount_total for institutions with both enrollment declines and repeated losses.",
    "warning_score_core >= 4 in the 2019 primarily baccalaureate workbook universe.",
    "Count of 2019 primarily baccalaureate institutions with enrollment_pct_change_5yr <= -10.",
    "Count of 2019 primarily baccalaureate institutions with revenue_pct_change_5yr <= -10.",
    "Count of 2019 institutions where enrollment_decline_last_3_of_5 == Yes and losses_last_3_of_5 == Yes.",
    "2014 lacks populated five-year trend fields in the canonical dataset, so it is not directly comparable to 2019 and 2024 on this framing.",
    "Same distress definition, limited to institutions with international_enrollment_increase_10yr == Yes.",
    "Matched Grant Witness research-funding schools to the predefined flagship unitid list and kept positive still-disrupted totals only.",
    "Subset of public flagships with positive still-disrupted totals of at least $1 million.",
    "Counts institutions whose 2024 staff_headcount_total is below 2023."
  ),
  stringsAsFactors = FALSE
)

state_public <- all_sheet_bacc[all_sheet_bacc$control_label == "Public" & !is.na(all_sheet_bacc$state) & all_sheet_bacc$state != "", , drop = FALSE]
state_breakdown <- do.call(rbind, lapply(split(state_public, state_public$state), function(df) {
  df <- df[!is.na(df$state_funding_pct_change_5yr), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)
  down_n <- sum(df$state_funding_pct_change_5yr < 0, na.rm = TRUE)
  up_n <- sum(df$state_funding_pct_change_5yr > 0, na.rm = TRUE)
  data.frame(
    state = df$state[1],
    public_institutions_with_state_change = nrow(df),
    state_funding_down_5yr_count = down_n,
    state_funding_down_5yr_percent = safe_pct(down_n, nrow(df)),
    state_funding_up_5yr_count = up_n,
    state_funding_up_5yr_percent = safe_pct(up_n, nrow(df)),
    mean_state_funding_pct_change_5yr = mean(df$state_funding_pct_change_5yr, na.rm = TRUE),
    median_state_funding_pct_change_5yr = median(df$state_funding_pct_change_5yr, na.rm = TRUE),
    mean_state_funding_pct_core_revenue = mean(df$state_funding_pct_core_revenue, na.rm = TRUE) * 100,
    median_state_funding_pct_core_revenue = median(df$state_funding_pct_core_revenue, na.rm = TRUE) * 100,
    biggest_state_funding_drop_pct_5yr = min(df$state_funding_pct_change_5yr, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))
if (!is.null(state_breakdown) && nrow(state_breakdown) > 0) {
  state_breakdown <- state_breakdown[order(state_breakdown$state_funding_down_5yr_percent, state_breakdown$mean_state_funding_pct_change_5yr, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
}

make_benchmark_tab <- function(group_list, label_prefix = "") {
  rows <- lapply(names(group_list), function(gname) {
    df <- group_list[[gname]]
    data.frame(
      group = paste0(label_prefix, gname),
      institutions = nrow(df),
      repeated_losses_share = if (nrow(df) == 0) NA_real_ else safe_pct(sum(yes_flag(df$losses_last_3_of_5), na.rm = TRUE), nrow(df)),
      revenue_pct_change_5yr_median = {
        x <- to_num(df$revenue_pct_change_5yr); x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else median(x)
      },
      net_tuition_per_fte_change_5yr_median = {
        x <- to_num(df$net_tuition_per_fte_change_5yr); x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else median(x)
      },
      transfer_out_rate_bachelor_median = {
        x <- to_num(df$transfer_out_rate_bachelor); x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else median(x)
      },
      transfer_out_rate_bachelor_change_5yr_median = {
        x <- to_num(df$transfer_out_rate_bachelor_change_5yr); x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else median(x)
      },
      staffing_cut_share = if (nrow(df) == 0) NA_real_ else safe_pct(sum(!is.na(df$staff_total_headcount_pct_change_5yr) & df$staff_total_headcount_pct_change_5yr < 0, na.rm = TRUE), nrow(df)),
      state_funding_pct_change_5yr_median = {
        x <- to_num(df$state_funding_pct_change_5yr); x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else median(x)
      },
      mean_state_funding_pct_change_5yr = {
        x <- to_num(df$state_funding_pct_change_5yr); x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else mean(x)
      },
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

sector_benchmarks <- make_benchmark_tab(list(
  all = groups$all,
  public = groups$public,
  private_nfp = groups$private_nfp,
  private_fp = groups$private_fp
))

bacc_benchmarks <- make_benchmark_tab(list(
  bacc_public = groups$bacc_public,
  bacc_private_nfp = groups$bacc_private_nfp,
  bacc_private_fp = groups$bacc_private_fp
))

worksheets <- list(
  Summary = summary_rows,
  ReportAnswers = report_answers,
  BaccBenchmarks = bacc_benchmarks,
  All_2024 = all_sheet_bacc,
  EnrollDecl3of5 = enr35,
  RevDecl3of5 = rev35,
  Red3of5 = red35,
  EnrollRev3of5 = enrrev,
  EnrollRed3of5 = enrred,
  All3Signals = all3,
  IntlUp5yr = intl5,
  IntlUp10yr = intl10,
  Flagships = flagships,
  FlagshipFed = flagship_fed,
  ResearchLeaders = research_leaders,
  Loss2024 = loss2024,
  StateDown5yr = state_down5yr,
  EndowDown5yr = endow_down5yr,
  DiscRateUp5yr = disc_up5yr,
  EnrollDown5yr = enroll_down5yr,
  RevDown5yr = rev_down5yr,
  StaffDown5yr = staff_down5yr,
  InstrStaffDown5yr = instr_staff_down5yr,
  StaffNetTuitionDown = staff_net_tuition_down,
  StaffCutRisk = staff_cut_risk,
  TransferOutUp5yr = transfer_out_up5yr,
  TransferOutUp10yr = transfer_out_up10yr,
  FedDepend = fed_depend,
  StateDepend = state_depend,
  StateBySt = state_breakdown,
  YearsAtLoss = years_at_loss,
  TuitionDepend = tuition_depend,
  NetTuitionDown = net_tuition_down,
  IntlShare = intl_share,
  LowCushion = low_cushion,
  HighDebt = high_debt,
  FedAndIntl = fed_and_intl,
  MultiSignal = multi_signal,
  PrivateCloseRisk = private_close_risk,
  PublicCampusRisk = public_campus_risk,
  PublicFinBad50 = public_fin_bad50,
  PrivateFinBad50 = private_fin_bad50,
  LossTuition = loss_tuition,
  PrivNFPStress = priv_nfp_stress,
  MultiFront = multi_front,
  DistressCore = distress_core,
  DistressIntl10 = distress_intl10,
  StaffCutsYoY = staff_cut_yoy,
  PublicFedTop = public_fed_top,
  GradDependTop = grad_depend_top,
  PublicGradTop = public_grad_top,
  StudPerInstr50 = students_per_instr_top50,
  FlagshipCuts = flagship_cuts,
  DistressCompare = distress_compare,
  IntlOffset10yr = intl_offset_10yr,
  BiggestDropsNoIntl = intl_offset_10yr_ranked,
  AccredFinanceXtab = accredit_finance_xtab,
  AccredMatches = accreditation_summary_bacc,
  CutsFinanceXtab = cuts_finance_xtab,
  CutsMatches = college_cuts_summary_bacc,
  HCM2Summary = hcm_summary,
  HCM2All = hcm_all,
  HCM2Dec24Drop = hcm_dec24_drop,
  HCM2Mar25Drop = hcm_mar25_drop,
  HCM2Jun25Drop = hcm_jun25_drop,
  HCM2Dec24Stay = hcm_dec24_stay,
  HCM2Mar25Stay = hcm_mar25_stay,
  # Closure tabs sit near the bottom of the workbook so the running list and
  # its derived splits are easy to find together.
  RunningClosures = running_closures,
  MainCampusClosures = main_campus_closures,
  BranchCampusClosures = branch_campus_closures,
  MergersConsol = mergers_consol,
  PrivFedMainClose = private_federal_main_closures,
  IntlVulnerable = intl_vulnerable,
  IntlVulnLarge = intl_vulnerable_large
)

# worksheet_signature(), xml_cell(), worksheet_xml() are in workbook_helpers.R

worksheet_names <- names(worksheets)
non_summary_names <- setdiff(worksheet_names, "Summary")
seen_signatures <- character(0)
duplicate_worksheet_names <- character(0)

for (name in rev(non_summary_names)) {
  sig <- worksheet_signature(worksheets[[name]])
  if (sig %in% seen_signatures) {
    duplicate_worksheet_names <- c(duplicate_worksheet_names, name)
  } else {
    seen_signatures <- c(seen_signatures, sig)
  }
}

if (length(duplicate_worksheet_names) > 0) {
  worksheets <- worksheets[setdiff(names(worksheets), duplicate_worksheet_names)]
  worksheets$Summary <- worksheets$Summary[
    !(worksheets$Summary$metric == "Worksheet index" &
        worksheets$Summary$statistic %in% duplicate_worksheet_names),
    , drop = FALSE
  ]
}

wb <- c(
  '<?xml version="1.0"?>',
  '<?mso-application progid="Excel.Sheet"?>',
  '<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:x="urn:schemas-microsoft-com:office:excel" xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet" xmlns:html="http://www.w3.org/TR/REC-html40">',
  '  <Styles>',
  '    <Style ss:ID="Default" ss:Name="Normal"><Alignment ss:Vertical="Bottom"/></Style>',
  '    <Style ss:ID="Header"><Font ss:Bold="1"/></Style>',
  '  </Styles>'
)

for (nm in names(worksheets)) {
  wb <- c(wb, worksheet_xml(nm, worksheets[[nm]]))
}
wb <- c(wb, '</Workbook>')

dir.create(dirname(output_workbook), recursive = TRUE, showWarnings = FALSE)
writeLines(wb, output_workbook, useBytes = TRUE)
cat(sprintf("Saved article workbook to %s\n", normalizePath(output_workbook, winslash = "/", mustWork = FALSE)))
invisible(list(workbook = normalizePath(output_workbook, winslash = "/", mustWork = FALSE)))
}

if (sys.nframe() == 0) {
  result <- main()
  quit(save = "no", status = 0)
}
