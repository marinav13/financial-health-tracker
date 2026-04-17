################################################################################
# build_article_workbook.R
#
# Generates a 49-sheet Excel workbook for journalist research on college financial
# health. Combines IPEDS financial data with risk scores, closure indicators, and
# accreditation/college cuts tracking to support investigative reporting.
#
# Usage: Rscript scripts/build_article_workbook.R [--input CSV] [--output XLS]
################################################################################

main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args          <- parse_cli_args(cli_args)
  ipeds         <- load_ipeds_paths()
  ipeds_layout  <- ipeds$ipeds_layout
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)

  input_csv <- get_arg_value("--input", ipeds_layout(root = ".")$dataset_csv)
  # SpreadsheetML with .xls extension opens directly in Excel without rename
  output_workbook <- get_arg_value("--output", "./workbooks/ipeds_financial_health_article_workbook.xls")

  source(file.path(getwd(), "scripts", "shared", "workbook_helpers.R"))
  source(file.path(getwd(), "scripts", "shared", "contracts.R"))

  # Pipeline: Load data -> Compute risk scores -> Build summary stats ->
  #           Define sheet specs -> Assemble worksheets -> Write SpreadsheetML

# ============================================================================
# SECTION: Configuration Constants
# Update these values whenever new IPEDS financial data is released.
# ============================================================================

# Public flagship universities, used to identify non-flagship campuses in
# public_campus_risk_score and to filter flagship-specific workbook sheets.
flagship_unitids <- c(
  102553,100751,106397,104179,110635,126614,129020,130943,134130,139959,141574,153658,142285,
  145637,151351,155317,157085,159391,166629,163286,161253,170976,174066,178396,176017,180489,
  199120,200280,181464,183044,186380,187985,182290,196088,204796,207500,209551,214777,217484,
  218663,219471,221759,228778,230764,234076,231174,236948,240444,238032,240727
)

# Latest data year and prior comparison year for year-over-year framing.
latest_year   <- 2024L
prev_year_num <- 2023L

# ============================================================================
# SECTION: Risk Score Computation Functions
# These functions compute risk scores by counting financial warning signals.
# Each score is built by summing boolean conditions (TRUE=1, FALSE=0):
#   - Higher scores = more financial stress indicators present
#   - Scores reflect common patterns before institutional distress events
# ============================================================================

# Sums 7 financial warning signals: enrollment decline, revenue loss, repeated losses,
# tuition dependence, falling net tuition, international reliance, and federal aid dependence.
compute_multi_signal_score <- function(df) {
  # Depends on df$high_tuition_dependence, df$high_international_share, and
  # df$high_federal_dependence being pre-computed on df before calling.
  row_score(
    yes_flag(df$enrollment_decline_last_3_of_5),
    yes_flag(df$revenue_10pct_drop_last_3_of_5),
    yes_flag(df$losses_last_3_of_5),
    df$high_tuition_dependence,
    !is.na(df$net_tuition_per_fte_change_5yr) & df$net_tuition_per_fte_change_5yr < 0,
    df$high_international_share,
    df$high_federal_dependence
  )
}

# Sums 8 signals that precede major staffing cuts: enrollment/revenue decline, losses,
# low tuition per FTE, high tuition dependence, endowment decline, and staff cuts.
compute_staffing_cut_risk_score <- function(df) {
  row_score(
    yes_flag(df$enrollment_decline_last_3_of_5),
    yes_flag(df$revenue_10pct_drop_last_3_of_5),
    yes_flag(df$losses_last_3_of_5),
    yes_flag(df$ended_year_at_loss),
    !is.na(df$net_tuition_per_fte_change_5yr) & df$net_tuition_per_fte_change_5yr < 0,
    !is.na(df$tuition_dependence_pct) & df$tuition_dependence_pct >= 50,
    !is.na(df$endowment_pct_change_5yr) & df$endowment_pct_change_5yr < 0,
    !is.na(df$staff_instructional_headcount_pct_change_5yr) & df$staff_instructional_headcount_pct_change_5yr < 0
  )
}

# Sums 9 distress signals for private colleges: enrollment decline, losses, net tuition loss,
# high tuition dependence, weak liquidity, high leverage, endowment decline, and staffing cuts.
compute_private_closure_risk_score <- function(df, tuition_q75, leverage_q75, liquidity_q25) {
  is_private <- df$control_label != "Public"
  row_score(
    is_private & yes_flag(df$enrollment_decline_last_3_of_5),
    is_private & yes_flag(df$losses_last_3_of_5),
    is_private & yes_flag(df$ended_year_at_loss),
    is_private & !is.na(df$net_tuition_per_fte_change_5yr) & df$net_tuition_per_fte_change_5yr < 0,
    is_private & !is.na(df$tuition_dependence_pct) & !is.na(tuition_q75) & df$tuition_dependence_pct >= tuition_q75,
    is_private & !is.na(df$liquidity) & !is.na(liquidity_q25) & df$liquidity <= liquidity_q25,
    is_private & !is.na(df$leverage) & !is.na(leverage_q75) & df$leverage >= leverage_q75,
    is_private & !is.na(df$endowment_pct_change_5yr) & df$endowment_pct_change_5yr < 0,
    is_private & !is.na(df$staff_instructional_headcount_pct_change_5yr) & df$staff_instructional_headcount_pct_change_5yr < 0
  )
}

# Sums 8 distress signals for non-flagship public campuses: enrollment/staff cuts,
# rising transfer-out rates, state funding decline, and missing financial data.
compute_public_campus_risk_score <- function(df, flagship_unitids) {
  is_nonflagship <- df$control_label == "Public" & !(as.integer(df$unitid) %in% flagship_unitids)
  row_score(
    is_nonflagship & yes_flag(df$enrollment_decline_last_3_of_5),
    is_nonflagship & !is.na(df$enrollment_pct_change_5yr) & df$enrollment_pct_change_5yr <= -20,
    is_nonflagship & !is.na(df$staff_total_headcount_pct_change_5yr) & df$staff_total_headcount_pct_change_5yr < 0,
    is_nonflagship & !is.na(df$staff_instructional_headcount_pct_change_5yr) & df$staff_instructional_headcount_pct_change_5yr < 0,
    is_nonflagship & yes_flag(df$transfer_out_rate_bachelor_increase_5yr),
    is_nonflagship & yes_flag(df$transfer_out_rate_bachelor_increase_10yr),
    is_nonflagship & !is.na(df$state_funding_pct_change_5yr) & df$state_funding_pct_change_5yr < 0,
    is_nonflagship & (is.na(df$revenue_total) | is.na(df$expenses_total))
  )
}

# ============================================================================
# SECTION: Load and Prepare Data
# Read the IPEDS canonical dataset and convert numeric columns
# ============================================================================

# Load the full IPEDS financial dataset across all years
read_df <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
# Filter for latest and prior year separately for comparisons
latest    <- read_df[as.character(read_df$year) == as.character(latest_year),   , drop = FALSE]
prev_year <- read_df[as.character(read_df$year) == as.character(prev_year_num), , drop = FALSE]

# List of columns that should be stored as numbers (not text)
# These include percentages, headcounts, dollar amounts, and calculated ratios
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

# Convert specified columns to numeric across all three datasets
for (nm in intersect(num_cols, names(latest))) {
  latest[[nm]] <- to_num(latest[[nm]])
}
for (nm in intersect(num_cols, names(prev_year))) {
  prev_year[[nm]] <- to_num(prev_year[[nm]])
}
for (nm in intersect(num_cols, names(read_df))) {
  read_df[[nm]] <- to_num(read_df[[nm]])
}

# Validate that required columns exist and data looks reasonable
validate_workbook_input(read_df)

# ============================================================================
# SECTION: Compute Risk Scores
# Calculate warning scores and risk metrics for all institutions
# ============================================================================

# Core warning score: sum of 6 main financial distress signals
# (defined in shared contracts.R)
latest$warning_score_core <- compute_warning_score_core(latest)

# Calculate sector percentiles to identify institutions in top 25% of risk metrics
# These are used to flag "high dependence" conditions in risk score functions
tuition_dependence_q75 <- q75_safe(latest$tuition_dependence_pct)
international_share_q75 <- q75_safe(latest$pct_international_all)
federal_dependence_q75 <- q75_safe(latest$federal_grants_contracts_pell_adjusted_pct_core_revenue)
private_tuition_dependence_q75 <- q75_safe(latest$tuition_dependence_pct[latest$control_label != "Public"])
private_leverage_q75 <- q75_safe(latest$leverage[latest$control_label != "Public"])
private_liquidity_q25 <- q25_safe(latest$liquidity[latest$control_label != "Public"])

# Flag institutions with tuition revenue above 75th percentile (high dependence on student fees)
latest$high_tuition_dependence <- !is.na(latest$tuition_dependence_pct) & !is.na(tuition_dependence_q75) &
  latest$tuition_dependence_pct >= tuition_dependence_q75
# Flag institutions with international enrollment above 75th percentile (high reliance on nonresident fees)
latest$high_international_share <- !is.na(latest$pct_international_all) & !is.na(international_share_q75) &
  latest$pct_international_all >= international_share_q75
# Flag institutions with federal aid above 75th percentile (high reliance on Title IV programs)
latest$high_federal_dependence <- !is.na(latest$federal_grants_contracts_pell_adjusted_pct_core_revenue) &
  !is.na(federal_dependence_q75) &
  latest$federal_grants_contracts_pell_adjusted_pct_core_revenue >= federal_dependence_q75

# Compute all risk scores using the functions defined above
latest$multi_signal_score <- compute_multi_signal_score(latest)
latest$staffing_cut_risk_score <- compute_staffing_cut_risk_score(latest)
latest$private_closure_risk_score <- compute_private_closure_risk_score(
  latest,
  tuition_q75  = private_tuition_dependence_q75,
  leverage_q75 = private_leverage_q75,
  liquidity_q25 = private_liquidity_q25
)
latest$public_campus_risk_score <- compute_public_campus_risk_score(latest, flagship_unitids)

# Assign risk category label based on institution control
latest$closure_risk_track <- dplyr::case_when(
  latest$control_label == "Public" ~ "Public campus restructuring risk",
  TRUE ~ "Private college closure risk"
)

# ============================================================================
# SECTION: Build Institutional Groups for Analysis
# Organize institutions by control/sector for comparative reporting
# ============================================================================

# Standard IPEDS category label for 4-year bachelor's-focused institutions
bacc_category_label <- "Degree-granting, primarily baccalaureate or above"

# Create 7 subgroups of the latest year data for comparison analysis
groups <- list(
  all = latest,
  public = latest[latest$control_label == "Public", , drop = FALSE],
  private_nfp = latest[latest$control_label == "Private not-for-profit", , drop = FALSE],
  private_fp = latest[latest$control_label == "Private for-profit", , drop = FALSE],
  bacc_public = latest[latest$control_label == "Public" & latest$category == bacc_category_label, , drop = FALSE],
  bacc_private_nfp = latest[latest$control_label == "Private not-for-profit" & latest$category == bacc_category_label, , drop = FALSE],
  bacc_private_fp = latest[latest$control_label == "Private for-profit" & latest$category == bacc_category_label, , drop = FALSE]
)

# Same grouping for prior year to enable year-over-year comparisons
groups_2023 <- list(
  all = prev_year,
  public = prev_year[prev_year$control_label == "Public", , drop = FALSE],
  private_nfp = prev_year[prev_year$control_label == "Private not-for-profit", , drop = FALSE],
  private_fp = prev_year[prev_year$control_label == "Private for-profit", , drop = FALSE],
  bacc_public = prev_year[prev_year$control_label == "Public" & prev_year$category == bacc_category_label, , drop = FALSE],
  bacc_private_nfp = prev_year[prev_year$control_label == "Private not-for-profit" & prev_year$category == bacc_category_label, , drop = FALSE],
  bacc_private_fp = prev_year[prev_year$control_label == "Private for-profit" & prev_year$category == bacc_category_label, , drop = FALSE]
)

# ============================================================================
# SECTION: Build Summary Statistics
# Create topline metrics and counts for the workbook Summary sheet
# ============================================================================

# Define key financial health metrics and their filters
# Each spec includes: metric name, predicate function to identify institutions
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

# Build count and percentage rows for each metric across all institutional groups
# Result: table showing metric prevalence by sector and category
summary_rows <- do.call(rbind, lapply(summary_metric_specs, function(spec) {
  counts <- count_by_group_from(groups, spec$pred)
  pcts <- pct_by_group_from(groups, spec$pred)
  make_count_pct_rows(spec$metric, counts, pcts, spec$notes %||% "")
}))

# Add custom rows for discount rate metric (nonprofit data only)
private_nfp_only <- groups$private_nfp
disc_count <- sum(!is.na(private_nfp_only$discount_pct_change_5yr) & private_nfp_only$discount_pct_change_5yr > 0, na.rm = TRUE)
disc_pct <- safe_pct(disc_count, nrow(private_nfp_only))
summary_rows <- append_rows(
  summary_rows,
  make_row("Discount rate increased over past 5 years", "count", disc_count, "", disc_count, "", "", sum(!is.na(groups$bacc_private_nfp$discount_pct_change_5yr) & groups$bacc_private_nfp$discount_pct_change_5yr > 0, na.rm = TRUE), "", "Private nonprofit only"),
  make_row("Discount rate increased over past 5 years", "percent", disc_pct, "", disc_pct, "", "", safe_pct(sum(!is.na(groups$bacc_private_nfp$discount_pct_change_5yr) & groups$bacc_private_nfp$discount_pct_change_5yr > 0, na.rm = TRUE), nrow(groups$bacc_private_nfp)), "", "Private nonprofit only")
)

# Calculate international student shares across sectors (weighted by enrollment)
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

# ============================================================================
# SECTION: Define Worksheet Specifications
# Each spec defines a filtered set of institutions and their sort order
# ============================================================================

sheet_index_specs <- list(
  list(name = "Summary", description = "Top-level counts and sector shares. Includes all filtered institutions plus baccalaureate-only breakouts."),
  list(name = "ReportAnswers", description = "Topline reporting answers, counts, and notes explaining how each figure was calculated."),
  list(name = "All_2024", description = "All 2024 predominantly baccalaureate institutions included in the workbook universe."),
  list(name = "EnrollDecl3of5", description = "Predominantly baccalaureate colleges with enrollment declines in 3 of the last 5 years."),
  list(name = "RevDecl3of5", description = "Predominantly baccalaureate colleges with revenue declines in 3 of the last 5 years."),
  list(name = "Red3of5", description = "Predominantly baccalaureate colleges that ended 3 of the last 5 years at a loss."),
  list(name = "EnrollRev3of5", description = "Predominantly baccalaureate colleges with both enrollment and revenue declines in 3 of the last 5 years."),
  list(name = "EnrollRed3of5", description = "Predominantly baccalaureate colleges with enrollment declines in 3 of the last 5 years and losses in 3 of the last 5 years."),
  list(name = "All3Signals", description = "Predominantly baccalaureate colleges with enrollment declines, revenue declines, and losses in 3 of the last 5 years."),
  list(name = "IntlUp5yr", description = "Predominantly baccalaureate colleges ranked by biggest increase in international students over the past 5 years."),
  list(name = "IntlUp10yr", description = "Predominantly baccalaureate colleges ranked by biggest increase in international students over the past 10 years."),
  list(name = "Flagships", description = "Predominantly baccalaureate public flagships ranked by core stress signals such as enrollment, revenue, and losses."),
  list(name = "FlagshipFed", description = "Predominantly baccalaureate public flagships ranked by dependence on Pell-adjusted federal grants and contracts."),
  list(name = "ResearchLeaders", description = "Predominantly baccalaureate colleges ranked by research spending per FTE, with research spending levels and research spending as a share of core expenses."),
  list(name = "Loss2024", description = "Predominantly baccalaureate colleges that ended 2024 at a loss."),
  list(name = "StateDown5yr", description = "Predominantly baccalaureate colleges with declining state appropriations over the past 5 years."),
  list(name = "EndowDown5yr", description = "Predominantly baccalaureate colleges with declining endowment value over the past 5 years."),
  list(name = "DiscRateUp5yr", description = "Predominantly baccalaureate colleges with rising discount rates over the past 5 years."),
  list(name = "EnrollDown5yr", description = "Predominantly baccalaureate colleges ranked by biggest enrollment decline over the past 5 years."),
  list(name = "RevDown5yr", description = "Predominantly baccalaureate colleges ranked by biggest revenue decline over the past 5 years."),
  list(name = "StaffDown5yr", description = "Predominantly baccalaureate colleges ranked by biggest total staff headcount decline over the past 5 years."),
  list(name = "InstrStaffDown5yr", description = "Predominantly baccalaureate colleges ranked by biggest instructional staff headcount decline over the past 5 years."),
  list(name = "StaffNetTuitionDown", description = "Predominantly baccalaureate colleges with both total staff headcount cuts and falling net tuition revenue per FTE over the past 5 years."),
  list(name = "StaffCutRisk", description = "Predominantly baccalaureate colleges ranked by the risk factors that most often show up before major staffing cuts: enrollment decline, revenue decline, repeated losses, ending 2024 at a loss, falling net tuition per FTE, high tuition dependence, endowment decline, and instructional staffing cuts."),
  list(name = "TransferOutUp5yr", description = "Predominantly baccalaureate colleges with rising transfer-out rates over the past 5 years."),
  list(name = "TransferOutUp10yr", description = "Predominantly baccalaureate colleges with rising transfer-out rates over the past 10 years."),
  list(name = "FedDepend", description = "Predominantly baccalaureate colleges ranked by dependence on Pell-adjusted federal grants and contracts."),
  list(name = "StateDepend", description = "Predominantly baccalaureate colleges ranked by dependence on state appropriations."),
  list(name = "StateBySt", description = "State-level public breakdowns using predominantly baccalaureate institutions."),
  list(name = "YearsAtLoss", description = "Predominantly baccalaureate colleges ranked by how many of the past 10 years they ended at a loss."),
  list(name = "TuitionDepend", description = "Predominantly baccalaureate colleges ranked by net tuition dependence."),
  list(name = "NetTuitionDown", description = "Predominantly baccalaureate colleges with falling net tuition revenue per FTE over the past 5 years."),
  list(name = "IntlShare", description = "Predominantly baccalaureate colleges ranked by international-student share."),
  list(name = "LowCushion", description = "Predominantly baccalaureate private nonprofits ranked by lowest liquidity."),
  list(name = "HighDebt", description = "Predominantly baccalaureate colleges ranked by leverage."),
  list(name = "FedAndIntl", description = "Predominantly baccalaureate colleges with both high federal dependence and high international share."),
  list(name = "MultiSignal", description = "Predominantly baccalaureate colleges with the highest combined warning-signal score."),
  list(name = "PrivateCloseRisk", description = "Predominantly baccalaureate private colleges ranked by a transparent closure-risk score built from enrollment decline, repeated losses, ending 2024 at a loss, net tuition decline, high tuition dependence, weak liquidity, high leverage, endowment decline, and instructional staffing cuts."),
  list(name = "PublicCampusRisk", description = "Predominantly baccalaureate non-flagship public campuses ranked by a restructuring-risk score built from enrollment decline, large 5-year enrollment losses, staffing cuts, rising transfer-out rates, falling state funding, and missing campus-level revenue or expense data."),
  list(name = "PublicFinBad50", description = "Top 50 public institutions with the most finance-page indicators currently styled as bad on the site."),
  list(name = "PrivateFinBad50", description = "Top 50 private institutions with the most finance-page indicators currently styled as bad on the site."),
  list(name = "LossTuition", description = "Predominantly baccalaureate colleges with repeated losses and high tuition dependence."),
  list(name = "PrivNFPStress", description = "Predominantly baccalaureate private nonprofits with rising discount rates and falling net tuition per FTE."),
  list(name = "MultiFront", description = "Predominantly baccalaureate colleges with high international share, high federal dependence, negative 5-year revenue change, and losses in at least 3 of the last 10 years."),
  list(name = "DistressCore", description = "Broad distress list using the workbook warning-score definition: at least 4 of 6 core warning signs."),
  list(name = "DistressIntl10", description = "Distressed colleges that also increased international enrollment over the past 10 years."),
  list(name = "StaffCutsYoY", description = "Year-by-year counts of institutions with staffing cuts versus the prior year, plus the total number of total and instructional staff positions cut or added."),
  list(name = "PublicFedTop", description = "Public universities ranked by federal grants and contracts as a share of core revenue."),
  list(name = "GradDependTop", description = "Universities ranked by graduate-student share and Grad PLUS borrowing intensity."),
  list(name = "PublicGradTop", description = "Public universities ranked by graduate-student share and Grad PLUS borrowing intensity."),
  list(name = "StudPerInstr50", description = "Top 50 universities with the highest students-per-instructional-staff ratio, using the FTE-based IPEDS measure."),
  list(name = "FlagshipCuts", description = "Public flagships matched to still-disrupted federal research cuts from Grant Witness."),
  list(name = "DistressCompare", description = "Year comparison for the distress paragraph framing, including 2024 toplines and 2019/2014 context."),
  list(name = "IntlOffset10yr", description = "Institutions where domestic enrollment would have fallen further without 10-year international enrollment growth."),
  list(name = "BiggestDropsNoIntl", description = "All ranked institutions where 2014-2024 enrollment would have fallen further without international enrollment growth, sorted by the biggest implied domestic drops."),
  list(name = "AccredFinanceXtab", description = "Finance, enrollment, and staffing comparison for institutions with versus without accreditation actions."),
  list(name = "AccredMatches", description = "Matched 4-year primarily bachelor's institutions with accreditation actions and finance metrics."),
  list(name = "CutsFinanceXtab", description = "Finance, enrollment, and staffing comparison for institutions with versus without college cuts."),
  list(name = "CutsMatches", description = "Matched 4-year primarily bachelor's institutions with college cuts and finance metrics."),
  list(name = "HCM2Summary", description = "Quarterly counts for schools on heightened cash monitoring level 2 in the 2024-2025 Federal Student Aid snapshots."),
  list(name = "HCM2All", description = "All normalized HCM2 rows from the December 2024 through December 2025 quarterly source files."),
  list(name = "HCM2Dec24Drop", description = "Schools on HCM2 in December 2024 that had dropped off the list by December 2025."),
  list(name = "HCM2Mar25Drop", description = "Schools on HCM2 in March 2025 that had dropped off the list by December 2025."),
  list(name = "HCM2Jun25Drop", description = "Schools on HCM2 in June 2025 that had dropped off the list by December 2025."),
  list(name = "HCM2Dec24Stay", description = "Schools on HCM2 in December 2024 that remained on every quarterly list through December 2025."),
  list(name = "HCM2Mar25Stay", description = "Schools on HCM2 in March 2025 that remained on every quarterly list through December 2025."),
  list(name = "RunningClosures", description = "Running list of federal closure events and IPEDS-supported closure or merger/consolidation events from 2008 to the present."),
  list(name = "MainCampusClosures", description = "Main-campus or full-system closures derived from federal closure files."),
  list(name = "BranchCampusClosures", description = "Branch-campus closures derived from PEPS rows and monthly federal reports."),
  list(name = "MergersConsol", description = "IPEDS exits that look more like mergers or consolidations than clean closures."),
  list(name = "PrivFedMainClose", description = "Selective list of private-sector main/full-system closures from federal closure sources only."),
  list(name = "IntlVulnerable", description = "High-international-share institutions from the 10-year offset list with additional financial warning signs."),
  list(name = "IntlVulnLarge", description = "Same as IntlVulnerable, limited to institutions with at least 5,000 students.")
)
sheet_index_rows <- build_worksheet_index_rows(sheet_index_specs)

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

base_sheet_specs <- list(
  EnrollDecl3of5 = list(
    filter_fn = function(df) df$enrollment_decline_last_3_of_5 == "Yes",
    order_fn = function(df) sort_df(df, c("enrollment_pct_change_5yr", "revenue_pct_change_5yr", "loss_amount"))
  ),
  RevDecl3of5 = list(
    filter_fn = function(df) df$revenue_10pct_drop_last_3_of_5 == "Yes",
    order_fn = function(df) sort_df(df, c("revenue_pct_change_5yr", "loss_amount", "enrollment_pct_change_5yr"))
  ),
  Red3of5 = list(
    filter_fn = function(df) df$losses_last_3_of_5 == "Yes",
    order_fn = function(df) sort_df(df, c("loss_amount"))
  ),
  EnrollRev3of5 = list(
    filter_fn = function(df) df$enrollment_decline_last_3_of_5 == "Yes" & df$revenue_10pct_drop_last_3_of_5 == "Yes",
    order_fn = function(df) sort_df(df, c("enrollment_pct_change_5yr", "revenue_pct_change_5yr", "loss_amount"))
  ),
  EnrollRed3of5 = list(
    filter_fn = function(df) df$enrollment_decline_last_3_of_5 == "Yes" & df$losses_last_3_of_5 == "Yes",
    order_fn = function(df) sort_df(df, c("enrollment_pct_change_5yr", "revenue_pct_change_5yr", "loss_amount"))
  ),
  All3Signals = list(
    filter_fn = function(df) df$enrollment_decline_last_3_of_5 == "Yes" & df$revenue_10pct_drop_last_3_of_5 == "Yes" & df$losses_last_3_of_5 == "Yes",
    order_fn = function(df) sort_df(df, c("enrollment_pct_change_5yr", "revenue_pct_change_5yr", "loss_amount"))
  ),
  IntlUp5yr = list(
    filter_fn = function(df) df$international_enrollment_increase_5yr == "Yes",
    order_fn = function(df) df[order(-xtfrm(df$international_student_count_change_5yr), -xtfrm(df$international_enrollment_pct_change_5yr), na.last = TRUE), , drop = FALSE]
  ),
  IntlUp10yr = list(
    filter_fn = function(df) df$international_enrollment_increase_10yr == "Yes",
    order_fn = function(df) df[order(-xtfrm(df$international_enrollment_change_10yr), -xtfrm(df$international_student_count_change_5yr), na.last = TRUE), , drop = FALSE]
  ),
  Flagships = list(
    filter_fn = function(df) df$control_label == "Public" & as.integer(df$unitid) %in% flagship_unitids,
    order_fn = function(df) sort_df(df, c("enrollment_pct_change_5yr", "revenue_pct_change_5yr", "loss_amount"))
  ),
  FlagshipFed = list(
    filter_fn = function(df) df$control_label == "Public" & as.integer(df$unitid) %in% flagship_unitids & !is.na(df$federal_grants_contracts_pell_adjusted_pct_core_revenue),
    order_fn = function(df) df[order(-xtfrm(df$federal_grants_contracts_pell_adjusted_pct_core_revenue), -xtfrm(df$federal_grants_contracts_pell_adjusted), na.last = TRUE), , drop = FALSE]
  ),
  ResearchLeaders = list(
    filter_fn = function(df) !is.na(df$research_expense_per_fte) & df$research_expense_per_fte > 0,
    order_fn = function(df) df[order(-xtfrm(df$research_expense_per_fte), -xtfrm(df$research_expense_pct_core_expenses), -xtfrm(df$research_expense), na.last = TRUE), , drop = FALSE]
  ),
  Loss2024 = list(
    filter_fn = function(df) df$ended_year_at_loss == "Yes",
    order_fn = function(df) sort_df(df, c("loss_amount"))
  ),
  StateDown5yr = list(
    filter_fn = function(df) !is.na(df$state_funding_pct_change_5yr) & df$state_funding_pct_change_5yr < 0,
    order_fn = function(df) sort_df(df, c("state_funding_pct_change_5yr"))
  ),
  EndowDown5yr = list(
    filter_fn = function(df) !is.na(df$endowment_pct_change_5yr) & df$endowment_pct_change_5yr < 0,
    order_fn = function(df) sort_df(df, c("endowment_pct_change_5yr"))
  ),
  DiscRateUp5yr = list(
    filter_fn = function(df) !is.na(df$discount_pct_change_5yr) & df$discount_pct_change_5yr > 0,
    order_fn = function(df) df[order(-xtfrm(df$discount_pct_change_5yr), na.last = TRUE), , drop = FALSE]
  ),
  EnrollDown5yr = list(
    filter_fn = function(df) !is.na(df$enrollment_pct_change_5yr) & df$enrollment_pct_change_5yr < 0,
    order_fn = function(df) sort_df(df, c("enrollment_pct_change_5yr"))
  ),
  RevDown5yr = list(
    filter_fn = function(df) !is.na(df$revenue_pct_change_5yr) & df$revenue_pct_change_5yr < 0,
    order_fn = function(df) sort_df(df, c("revenue_pct_change_5yr"))
  ),
  StaffDown5yr = list(
    filter_fn = function(df) !is.na(df$staff_total_headcount_pct_change_5yr) & df$staff_total_headcount_pct_change_5yr < 0,
    order_fn = function(df) sort_df(df, c("staff_total_headcount_pct_change_5yr", "staff_change_1yr"))
  ),
  InstrStaffDown5yr = list(
    filter_fn = function(df) !is.na(df$staff_instructional_headcount_pct_change_5yr) & df$staff_instructional_headcount_pct_change_5yr < 0,
    order_fn = function(df) sort_df(df, c("staff_instructional_headcount_pct_change_5yr", "staff_change_1yr"))
  ),
  StaffNetTuitionDown = list(
    filter_fn = function(df) !is.na(df$staff_total_headcount_pct_change_5yr) & !is.na(df$net_tuition_per_fte_change_5yr) & df$staff_total_headcount_pct_change_5yr < 0 & df$net_tuition_per_fte_change_5yr < 0,
    order_fn = function(df) df[order(xtfrm(df$staff_total_headcount_pct_change_5yr), xtfrm(df$net_tuition_per_fte_change_5yr), xtfrm(df$revenue_pct_change_5yr), na.last = TRUE), , drop = FALSE]
  ),
  StaffCutRisk = list(
    filter_fn = function(df) !is.na(df$staffing_cut_risk_score),
    order_fn = function(df) df[order(-xtfrm(df$staffing_cut_risk_score), xtfrm(df$staff_total_headcount_pct_change_5yr), xtfrm(df$net_tuition_per_fte_change_5yr), xtfrm(df$revenue_pct_change_5yr), na.last = TRUE), , drop = FALSE]
  ),
  TransferOutUp5yr = list(
    filter_fn = function(df) !is.na(df$transfer_out_rate_bachelor_change_5yr) & df$transfer_out_rate_bachelor_change_5yr > 0,
    order_fn = function(df) df[order(-xtfrm(df$transfer_out_rate_bachelor_change_5yr), -xtfrm(df$transfer_out_rate_bachelor), na.last = TRUE), , drop = FALSE]
  ),
  TransferOutUp10yr = list(
    filter_fn = function(df) !is.na(df$transfer_out_rate_bachelor_change_10yr) & df$transfer_out_rate_bachelor_change_10yr > 0,
    order_fn = function(df) df[order(-xtfrm(df$transfer_out_rate_bachelor_change_10yr), -xtfrm(df$transfer_out_rate_bachelor), na.last = TRUE), , drop = FALSE]
  ),
  FedDepend = list(
    filter_fn = function(df) !is.na(df$federal_grants_contracts_pell_adjusted_pct_core_revenue),
    order_fn = function(df) df[order(-xtfrm(df$federal_grants_contracts_pell_adjusted_pct_core_revenue), -xtfrm(df$federal_grants_contracts_pell_adjusted), na.last = TRUE), , drop = FALSE]
  ),
  StateDepend = list(
    filter_fn = function(df) !is.na(df$state_funding_pct_core_revenue),
    order_fn = function(df) df[order(-xtfrm(df$state_funding_pct_core_revenue), -xtfrm(df$state_funding), na.last = TRUE), , drop = FALSE]
  ),
  YearsAtLoss = list(
    filter_fn = function(df) !is.na(df$loss_years_last_10),
    order_fn = function(df) df[order(-xtfrm(df$loss_years_last_10), -xtfrm(df$loss_years_last_5), xtfrm(df$loss_amount), na.last = TRUE), , drop = FALSE]
  ),
  TuitionDepend = list(
    filter_fn = function(df) !is.na(df$tuition_dependence_pct),
    order_fn = function(df) df[order(-xtfrm(df$tuition_dependence_pct), xtfrm(df$enrollment_pct_change_5yr), xtfrm(df$revenue_pct_change_5yr), na.last = TRUE), , drop = FALSE]
  ),
  NetTuitionDown = list(
    filter_fn = function(df) !is.na(df$net_tuition_per_fte_change_5yr) & df$net_tuition_per_fte_change_5yr < 0,
    order_fn = function(df) df[order(xtfrm(df$net_tuition_per_fte_change_5yr), xtfrm(df$revenue_pct_change_5yr), na.last = TRUE), , drop = FALSE]
  ),
  IntlShare = list(
    filter_fn = function(df) !is.na(df$pct_international_all),
    order_fn = function(df) df[order(-xtfrm(df$pct_international_all), -xtfrm(df$pct_international_graduate), na.last = TRUE), , drop = FALSE]
  ),
  LowCushion = list(
    filter_fn = function(df) df$control_label == "Private not-for-profit" & !is.na(df$liquidity),
    order_fn = function(df) df[order(xtfrm(df$liquidity), -xtfrm(df$leverage), na.last = TRUE), , drop = FALSE]
  ),
  HighDebt = list(
    filter_fn = function(df) !is.na(df$leverage),
    order_fn = function(df) df[order(-xtfrm(df$leverage), xtfrm(df$liquidity), na.last = TRUE), , drop = FALSE]
  ),
  FedAndIntl = list(
    filter_fn = function(df) df$high_federal_dependence & df$high_international_share,
    order_fn = function(df) df[order(-xtfrm(df$federal_grants_contracts_pell_adjusted_pct_core_revenue), -xtfrm(df$pct_international_all), xtfrm(df$revenue_pct_change_5yr), na.last = TRUE), , drop = FALSE]
  ),
  MultiSignal = list(
    filter_fn = function(df) !is.na(df$multi_signal_score),
    order_fn = function(df) df[order(-xtfrm(df$multi_signal_score), -xtfrm(df$warning_score_core), xtfrm(df$revenue_pct_change_5yr), xtfrm(df$enrollment_pct_change_5yr), na.last = TRUE), , drop = FALSE]
  ),
  PrivateCloseRisk = list(
    filter_fn = function(df) df$control_label != "Public" & !is.na(df$private_closure_risk_score),
    order_fn = function(df) df[order(-xtfrm(df$private_closure_risk_score), xtfrm(df$net_tuition_per_fte_change_5yr), xtfrm(df$revenue_pct_change_5yr), xtfrm(df$enrollment_pct_change_5yr), na.last = TRUE), , drop = FALSE]
  ),
  PublicCampusRisk = list(
    filter_fn = function(df) df$control_label == "Public" & !(as.integer(df$unitid) %in% flagship_unitids) & !is.na(df$public_campus_risk_score),
    order_fn = function(df) df[order(-xtfrm(df$public_campus_risk_score), xtfrm(df$enrollment_pct_change_5yr), xtfrm(df$state_funding_pct_change_5yr), na.last = TRUE), , drop = FALSE]
  ),
  StudPerInstr50 = list(
    filter_fn = function(df) !is.na(df$students_per_instructional_staff_fte),
    order_fn = function(df) df[order(-xtfrm(df$students_per_instructional_staff_fte), xtfrm(df$institution_name), na.last = TRUE), , drop = FALSE],
    head_n = 50
  )
)

base_sheets <- build_workbook_sheets(all_sheet_bacc, base_sheet_specs)

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

finance_sheet_specs <- list(
  PublicFinBad50 = list(
    filter_fn = function(df) df$control_label == "Public",
    order_fn = function(df) df[order(-xtfrm(df$finance_page_bad_count), -xtfrm(df$warning_score_core), xtfrm(df$revenue_pct_change_5yr), xtfrm(df$enrollment_pct_change_5yr), na.last = TRUE), , drop = FALSE],
    head_n = 50
  ),
  PrivateFinBad50 = list(
    filter_fn = function(df) df$control_label != "Public",
    order_fn = function(df) df[order(-xtfrm(df$finance_page_bad_count), -xtfrm(df$warning_score_core), xtfrm(df$revenue_pct_change_5yr), xtfrm(df$enrollment_pct_change_5yr), na.last = TRUE), , drop = FALSE],
    head_n = 50
  )
)

theme_sheet_specs <- list(
  LossTuition = list(
    filter_fn = function(df) !is.na(df$loss_years_last_10) & !is.na(df$tuition_dependence_pct),
    order_fn = function(df) df[order(-xtfrm(df$loss_years_last_10), -xtfrm(df$tuition_dependence_pct), xtfrm(df$revenue_pct_change_5yr), na.last = TRUE), , drop = FALSE]
  ),
  PrivNFPStress = list(
    filter_fn = function(df) df$control_label == "Private not-for-profit" & !is.na(df$discount_pct_change_5yr) & !is.na(df$net_tuition_per_fte_change_5yr) & df$discount_pct_change_5yr > 0 & df$net_tuition_per_fte_change_5yr < 0,
    order_fn = function(df) df[order(-xtfrm(df$discount_pct_change_5yr), xtfrm(df$net_tuition_per_fte_change_5yr), na.last = TRUE), , drop = FALSE]
  ),
  MultiFront = list(
    filter_fn = function(df) !is.na(df$pct_international_all) & !is.na(df$federal_grants_contracts_pell_adjusted_pct_core_revenue) & !is.na(df$revenue_pct_change_5yr) & !is.na(df$loss_years_last_10) & df$high_international_share & df$high_federal_dependence & df$revenue_pct_change_5yr < 0 & df$loss_years_last_10 >= 3,
    order_fn = function(df) df[order(-xtfrm(df$federal_grants_contracts_pell_adjusted_pct_core_revenue), -xtfrm(df$pct_international_all), -xtfrm(df$loss_years_last_10), xtfrm(df$revenue_pct_change_5yr), na.last = TRUE), , drop = FALSE]
  ),
  DistressCore = list(
    filter_fn = function(df) !is.na(df$warning_score_core) & df$warning_score_core >= 4,
    order_fn = function(df) df[order(-xtfrm(df$warning_score_core), xtfrm(df$revenue_pct_change_5yr), xtfrm(df$enrollment_pct_change_5yr), na.last = TRUE), , drop = FALSE]
  ),
  DistressIntl10 = list(
    filter_fn = function(df) !is.na(df$warning_score_core) & df$warning_score_core >= 4 & df$international_enrollment_increase_10yr == "Yes",
    order_fn = function(df) df[order(-xtfrm(df$warning_score_core), -xtfrm(df$international_enrollment_change_10yr), na.last = TRUE), , drop = FALSE]
  )
)

finance_sheets <- build_workbook_sheets(finance_bad, finance_sheet_specs)
theme_sheets <- build_workbook_sheets(all_sheet_bacc, theme_sheet_specs)
distress_intl10 <- theme_sheets$DistressIntl10

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

# Year-over-year staffing cuts: keep the existing institution counts, but also
# sum the actual headcount reductions so the workbook shows how many jobs were
# cut rather than only how many campuses reported a decline. Include 2014 as
# the visible staffing baseline, then show later years as cuts relative to the
# prior year's staffing base.
staff_cut_yoy <- build_staff_cut_yoy(read_df, start_year = latest_year - 10L, end_year = latest_year)

graduate_sheet_specs <- list(
  PublicFedTop = list(
    filter_fn = function(df) df$control_label == "Public" & !is.na(df$federal_grants_contracts_pell_adjusted_pct_core_revenue),
    order_fn = function(df) {
      out <- df[order(-xtfrm(df$federal_grants_contracts_pell_adjusted_pct_core_revenue), -xtfrm(df$research_expense_per_fte), na.last = TRUE), , drop = FALSE]
      out$federal_grants_contracts_pct_core_revenue_pct <- out$federal_grants_contracts_pell_adjusted_pct_core_revenue * 100
      out
    }
  ),
  GradDependTop = list(
    filter_fn = function(df) !is.na(df$share_grad_students),
    order_fn = function(df) {
      out <- df[order(-xtfrm(df$share_grad_students), -xtfrm(df$grad_plus_disbursements_per_recipient), na.last = TRUE), , drop = FALSE]
      out$share_grad_students_pct <- out$share_grad_students * 100
      out
    }
  ),
  PublicGradTop = list(
    filter_fn = function(df) df$control_label == "Public" & !is.na(df$share_grad_students),
    order_fn = function(df) {
      out <- df[order(-xtfrm(df$share_grad_students), -xtfrm(df$grad_plus_disbursements_per_recipient), na.last = TRUE), , drop = FALSE]
      out$share_grad_students_pct <- out$share_grad_students * 100
      out
    }
  )
)

graduate_sheets <- build_workbook_sheets(all_sheet_bacc, graduate_sheet_specs)

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

distress_compare <- build_distress_compare(read_df, bacc_category_label, years = c(latest_year, latest_year - 5L, latest_year - 10L))

intl_base_2014 <- read_df[as.integer(read_df$year) == latest_year - 10L, c("unitid","enrollment_headcount_total"), drop = FALSE]
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

report_answers <- build_report_answers(
  distress_compare = distress_compare,
  distress_intl10 = distress_intl10,
  flagship_cuts = flagship_cuts,
  staff_cut_yoy = staff_cut_yoy
)

state_breakdown <- build_state_breakdown(all_sheet_bacc)

sector_benchmarks <- build_benchmark_tab(list(
  all = groups$all,
  public = groups$public,
  private_nfp = groups$private_nfp,
  private_fp = groups$private_fp
))

bacc_benchmarks <- build_benchmark_tab(list(
  bacc_public = groups$bacc_public,
  bacc_private_nfp = groups$bacc_private_nfp,
  bacc_private_fp = groups$bacc_private_fp
))

worksheets <- build_article_workbook_registry(
  summary_rows = summary_rows,
  report_answers = report_answers,
  bacc_benchmarks = bacc_benchmarks,
  all_sheet_bacc = all_sheet_bacc,
  base_sheets = base_sheets,
  state_breakdown = state_breakdown,
  finance_sheets = finance_sheets,
  theme_sheets = theme_sheets,
  staff_cut_yoy = staff_cut_yoy,
  graduate_sheets = graduate_sheets,
  flagship_cuts = flagship_cuts,
  distress_compare = distress_compare,
  intl_offset_10yr = intl_offset_10yr,
  intl_offset_10yr_ranked = intl_offset_10yr_ranked,
  accredit_finance_xtab = accredit_finance_xtab,
  accreditation_summary_bacc = accreditation_summary_bacc,
  cuts_finance_xtab = cuts_finance_xtab,
  college_cuts_summary_bacc = college_cuts_summary_bacc,
  hcm_summary = hcm_summary,
  hcm_all = hcm_all,
  hcm_dec24_drop = hcm_dec24_drop,
  hcm_mar25_drop = hcm_mar25_drop,
  hcm_jun25_drop = hcm_jun25_drop,
  hcm_dec24_stay = hcm_dec24_stay,
  hcm_mar25_stay = hcm_mar25_stay,
  running_closures = running_closures,
  main_campus_closures = main_campus_closures,
  branch_campus_closures = branch_campus_closures,
  mergers_consol = mergers_consol,
  private_federal_main_closures = private_federal_main_closures,
  intl_vulnerable = intl_vulnerable,
  intl_vulnerable_large = intl_vulnerable_large
)

# worksheet_signature(), xml_cell(), worksheet_xml() are in workbook_helpers.R
worksheets <- prune_duplicate_worksheets(worksheets)

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
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      