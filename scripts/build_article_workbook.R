main <- function(cli_args = NULL) {
args <- if (is.null(cli_args)) commandArgs(trailingOnly = TRUE) else cli_args

get_arg_value <- function(flag, default) {
  idx <- match(flag, args)
  if (!is.na(idx) && idx < length(args)) {
    return(args[[idx + 1L]])
  }
  default
}

input_csv <- get_arg_value("--input", "./ipeds/ipeds_financial_health_dataset_2014_2024.csv")
output_xml <- get_arg_value("--output", "./workbooks/ipeds_financial_health_article_workbook_r.xml")

flagship_unitids <- c(
  102553,100751,106397,104179,110635,126614,129020,130943,134130,139959,141574,153658,142285,
  145637,151351,155317,157085,159391,166629,163286,161253,170976,174066,178396,176017,180489,
  199120,200280,181464,183044,186380,187985,182290,196088,204796,207500,209551,214777,217484,
  218663,219471,221759,228778,230764,234076,231174,236948,240444,238032,240727
)

to_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NULL")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", "", x, fixed = TRUE)))
}

yes_flag <- function(x) {
  trimws(tolower(as.character(x))) == "yes"
}

safe_pct <- function(num, den) {
  ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, (num / den) * 100)
}

escape_xml <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

make_row <- function(metric, statistic, all = NA, public = NA, private_nfp = NA, private_fp = NA,
                     bacc_public = NA, bacc_private_nfp = NA, bacc_private_fp = NA, notes = "") {
  data.frame(
    metric = metric,
    statistic = statistic,
    all = all,
    public = public,
    private_nfp = private_nfp,
    private_fp = private_fp,
    bacc_public = bacc_public,
    bacc_private_nfp = bacc_private_nfp,
    bacc_private_fp = bacc_private_fp,
    notes = notes,
    stringsAsFactors = FALSE
  )
}

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

latest$warning_score_core <- rowSums(cbind(
  yes_flag(latest$enrollment_decline_last_3_of_5),
  yes_flag(latest$revenue_10pct_drop_last_3_of_5),
  yes_flag(latest$losses_last_3_of_5),
  yes_flag(latest$ended_year_at_loss),
  !is.na(latest$staff_total_headcount_pct_change_5yr) & latest$staff_total_headcount_pct_change_5yr < 0,
  !is.na(latest$net_tuition_per_fte_change_5yr) & latest$net_tuition_per_fte_change_5yr < 0
), na.rm = TRUE)

q75_safe <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE))
}

q25_safe <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE))
}

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

latest$multi_signal_score <- rowSums(cbind(
  yes_flag(latest$enrollment_decline_last_3_of_5),
  yes_flag(latest$revenue_10pct_drop_last_3_of_5),
  yes_flag(latest$losses_last_3_of_5),
  latest$high_tuition_dependence,
  !is.na(latest$net_tuition_per_fte_change_5yr) & latest$net_tuition_per_fte_change_5yr < 0,
  latest$high_international_share,
  latest$high_federal_dependence
), na.rm = TRUE)

latest$staffing_cut_risk_score <- rowSums(cbind(
  yes_flag(latest$enrollment_decline_last_3_of_5),
  yes_flag(latest$revenue_10pct_drop_last_3_of_5),
  yes_flag(latest$losses_last_3_of_5),
  yes_flag(latest$ended_year_at_loss),
  !is.na(latest$net_tuition_per_fte_change_5yr) & latest$net_tuition_per_fte_change_5yr < 0,
  !is.na(latest$tuition_dependence_pct) & latest$tuition_dependence_pct >= 50,
  !is.na(latest$endowment_pct_change_5yr) & latest$endowment_pct_change_5yr < 0,
  !is.na(latest$staff_instructional_headcount_pct_change_5yr) & latest$staff_instructional_headcount_pct_change_5yr < 0
), na.rm = TRUE)

latest$private_closure_risk_score <- rowSums(cbind(
  latest$control_label != "Public" & yes_flag(latest$enrollment_decline_last_3_of_5),
  latest$control_label != "Public" & yes_flag(latest$losses_last_3_of_5),
  latest$control_label != "Public" & yes_flag(latest$ended_year_at_loss),
  latest$control_label != "Public" & !is.na(latest$net_tuition_per_fte_change_5yr) & latest$net_tuition_per_fte_change_5yr < 0,
  latest$control_label != "Public" & !is.na(latest$tuition_dependence_pct) & !is.na(private_tuition_dependence_q75) & latest$tuition_dependence_pct >= private_tuition_dependence_q75,
  latest$control_label != "Public" & !is.na(latest$liquidity) & !is.na(private_liquidity_q25) & latest$liquidity <= private_liquidity_q25,
  latest$control_label != "Public" & !is.na(latest$leverage) & !is.na(private_leverage_q75) & latest$leverage >= private_leverage_q75,
  latest$control_label != "Public" & !is.na(latest$endowment_pct_change_5yr) & latest$endowment_pct_change_5yr < 0,
  latest$control_label != "Public" & !is.na(latest$staff_instructional_headcount_pct_change_5yr) & latest$staff_instructional_headcount_pct_change_5yr < 0
), na.rm = TRUE)

latest$public_campus_risk_score <- rowSums(cbind(
  latest$control_label == "Public" & !(as.integer(latest$unitid) %in% flagship_unitids) & yes_flag(latest$enrollment_decline_last_3_of_5),
  latest$control_label == "Public" & !(as.integer(latest$unitid) %in% flagship_unitids) & !is.na(latest$enrollment_pct_change_5yr) & latest$enrollment_pct_change_5yr <= -20,
  latest$control_label == "Public" & !(as.integer(latest$unitid) %in% flagship_unitids) & !is.na(latest$staff_total_headcount_pct_change_5yr) & latest$staff_total_headcount_pct_change_5yr < 0,
  latest$control_label == "Public" & !(as.integer(latest$unitid) %in% flagship_unitids) & !is.na(latest$staff_instructional_headcount_pct_change_5yr) & latest$staff_instructional_headcount_pct_change_5yr < 0,
  latest$control_label == "Public" & !(as.integer(latest$unitid) %in% flagship_unitids) & yes_flag(latest$transfer_out_rate_bachelor_increase_5yr),
  latest$control_label == "Public" & !(as.integer(latest$unitid) %in% flagship_unitids) & yes_flag(latest$transfer_out_rate_bachelor_increase_10yr),
  latest$control_label == "Public" & !(as.integer(latest$unitid) %in% flagship_unitids) & !is.na(latest$state_funding_pct_change_5yr) & latest$state_funding_pct_change_5yr < 0,
  latest$control_label == "Public" & !(as.integer(latest$unitid) %in% flagship_unitids) & (is.na(latest$revenue_total) | is.na(latest$expenses_total))
), na.rm = TRUE)

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

count_by_group <- function(pred) {
  sapply(groups, function(df) sum(pred(df), na.rm = TRUE))
}

count_by_group_from <- function(group_list, pred) {
  sapply(group_list, function(df) sum(pred(df), na.rm = TRUE))
}

pct_by_group <- function(pred) {
  sapply(groups, function(df) {
    if (nrow(df) == 0) return(NA_real_)
    safe_pct(sum(pred(df), na.rm = TRUE), nrow(df))
  })
}

median_by_group <- function(field) {
  sapply(groups, function(df) {
    x <- to_num(df[[field]])
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    median(x, na.rm = TRUE)
  })
}

mean_by_group <- function(field) {
  sapply(groups, function(df) {
    x <- to_num(df[[field]])
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    mean(x, na.rm = TRUE)
  })
}

weighted_intl_pct <- function(df, num_col, den_col) {
  keep <- !is.na(df[[num_col]]) & !is.na(df[[den_col]]) & df[[den_col]] > 0
  if (!any(keep)) return(NA_real_)
  safe_pct(sum(df[[num_col]][keep], na.rm = TRUE), sum(df[[den_col]][keep], na.rm = TRUE))
}

top_metric_by_group <- function(metric) {
  lapply(groups, function(df) {
    keep <- !is.na(df[[metric]])
    if (!any(keep)) return(NULL)
    df <- df[keep, , drop = FALSE]
    df[order(df[[metric]], decreasing = TRUE), , drop = FALSE][1, , drop = FALSE]
  })
}

summary_rows <- do.call(rbind, list())

add_count_pct_rows <- function(metric, pred, notes = "") {
  counts <- count_by_group(pred)
  pcts <- pct_by_group(pred)
  assign("summary_rows", rbind(
    get("summary_rows", inherits = TRUE),
    make_row(metric, "count", counts[["all"]], counts[["public"]], counts[["private_nfp"]], counts[["private_fp"]], counts[["bacc_public"]], counts[["bacc_private_nfp"]], counts[["bacc_private_fp"]], notes),
    make_row(metric, "percent", pcts[["all"]], pcts[["public"]], pcts[["private_nfp"]], pcts[["private_fp"]], pcts[["bacc_public"]], pcts[["bacc_private_nfp"]], pcts[["bacc_private_fp"]], notes)
  ), envir = .GlobalEnv)
}

add_count_pct_rows("Enrollment decline in 3 of last 5 years", function(df) yes_flag(df$enrollment_decline_last_3_of_5))
add_count_pct_rows("Revenue decline in 3 of last 5 years", function(df) yes_flag(df$revenue_10pct_drop_last_3_of_5))
add_count_pct_rows("In the red in 3 of last 5 years", function(df) yes_flag(df$losses_last_3_of_5))
add_count_pct_rows("Enrollment and revenue decline in 3 of last 5 years", function(df) yes_flag(df$enrollment_decline_last_3_of_5) & yes_flag(df$revenue_10pct_drop_last_3_of_5))
add_count_pct_rows("Enrollment, revenue decline and in the red in 3 of last 5 years", function(df) yes_flag(df$enrollment_decline_last_3_of_5) & yes_flag(df$revenue_10pct_drop_last_3_of_5) & yes_flag(df$losses_last_3_of_5))
add_count_pct_rows("Enrollment decline and in the red in 3 of last 5 years", function(df) yes_flag(df$enrollment_decline_last_3_of_5) & yes_flag(df$losses_last_3_of_5))
add_count_pct_rows("Revenue decline and in the red in 3 of last 5 years", function(df) yes_flag(df$revenue_10pct_drop_last_3_of_5) & yes_flag(df$losses_last_3_of_5))
add_count_pct_rows("Enrollment decline in 2024", function(df) !is.na(df$enrollment_change_1yr) & df$enrollment_change_1yr < 0)
add_count_pct_rows("Revenue decline in 2024", function(df) !is.na(df$revenue_change_1yr) & df$revenue_change_1yr < 0)
add_count_pct_rows("In the red in 2024", function(df) yes_flag(df$ended_year_at_loss))
add_count_pct_rows("International enrollment increased over past decade", function(df) yes_flag(df$international_enrollment_increase_10yr))
add_count_pct_rows("International enrollment increased over past 5 years", function(df) yes_flag(df$international_enrollment_increase_5yr))
add_count_pct_rows("Staffing cut over past 5 years", function(df) !is.na(df$staff_total_headcount_pct_change_5yr) & df$staff_total_headcount_pct_change_5yr < 0, "Using staff headcount")
add_count_pct_rows("Instructional staffing cut over past 5 years", function(df) !is.na(df$staff_instructional_headcount_pct_change_5yr) & df$staff_instructional_headcount_pct_change_5yr < 0, "Using staff headcount")
add_count_pct_rows("Ended 2024 fiscal year at a loss", function(df) yes_flag(df$ended_year_at_loss))
add_count_pct_rows("Net tuition per FTE decreased over past 5 years", function(df) !is.na(df$net_tuition_per_fte_change_5yr) & df$net_tuition_per_fte_change_5yr < 0, "Using net tuition per FTE")
add_count_pct_rows("State appropriations decreased over past 5 years", function(df) !is.na(df$state_funding_pct_change_5yr) & df$state_funding_pct_change_5yr < 0)
add_count_pct_rows("State appropriations increased over past 5 years", function(df) !is.na(df$state_funding_pct_change_5yr) & df$state_funding_pct_change_5yr > 0)
add_count_pct_rows("Endowment increased over past 5 years", function(df) !is.na(df$endowment_pct_change_5yr) & df$endowment_pct_change_5yr > 0)
add_count_pct_rows("Endowment decreased over past 5 years", function(df) !is.na(df$endowment_pct_change_5yr) & df$endowment_pct_change_5yr < 0)
add_count_pct_rows("Enrollment decreased over past 5 years", function(df) !is.na(df$enrollment_pct_change_5yr) & df$enrollment_pct_change_5yr < 0)
add_count_pct_rows("Revenue decreased over past 5 years", function(df) !is.na(df$revenue_pct_change_5yr) & df$revenue_pct_change_5yr < 0)
add_count_pct_rows("Transfer-out rate increased over past 5 years", function(df) !is.na(df$transfer_out_rate_bachelor_change_5yr) & df$transfer_out_rate_bachelor_change_5yr > 0)

private_nfp_only <- groups$private_nfp
disc_count <- sum(!is.na(private_nfp_only$discount_pct_change_5yr) & private_nfp_only$discount_pct_change_5yr > 0, na.rm = TRUE)
disc_pct <- safe_pct(disc_count, nrow(private_nfp_only))
summary_rows <- rbind(
  summary_rows,
  make_row("Discount rate increased over past 5 years", "count", disc_count, "", disc_count, "", "", sum(!is.na(groups$bacc_private_nfp$discount_pct_change_5yr) & groups$bacc_private_nfp$discount_pct_change_5yr > 0, na.rm = TRUE), "", "Private nonprofit only"),
  make_row("Discount rate increased over past 5 years", "percent", disc_pct, "", disc_pct, "", "", safe_pct(sum(!is.na(groups$bacc_private_nfp$discount_pct_change_5yr) & groups$bacc_private_nfp$discount_pct_change_5yr > 0, na.rm = TRUE), nrow(groups$bacc_private_nfp)), "", "Private nonprofit only")
)

intl_all <- sapply(groups, weighted_intl_pct, num_col = "enrollment_nonresident_total", den_col = "enrollment_headcount_total")
intl_grad <- sapply(groups, weighted_intl_pct, num_col = "enrollment_nonresident_graduate", den_col = "enrollment_headcount_graduate")
intl_ug <- sapply(groups, weighted_intl_pct, num_col = "enrollment_nonresident_undergrad", den_col = "enrollment_headcount_undergrad")

summary_rows <- rbind(
  summary_rows,
  make_row("Students who are international", "percent", intl_all[["all"]], intl_all[["public"]], intl_all[["private_nfp"]], intl_all[["private_fp"]], weighted_intl_pct(groups$bacc_public, "enrollment_nonresident_total", "enrollment_headcount_total"), weighted_intl_pct(groups$bacc_private_nfp, "enrollment_nonresident_total", "enrollment_headcount_total"), weighted_intl_pct(groups$bacc_private_fp, "enrollment_nonresident_total", "enrollment_headcount_total")),
  make_row("Graduate students who are international", "percent", intl_grad[["all"]], intl_grad[["public"]], intl_grad[["private_nfp"]], intl_grad[["private_fp"]], weighted_intl_pct(groups$bacc_public, "enrollment_nonresident_graduate", "enrollment_headcount_graduate"), weighted_intl_pct(groups$bacc_private_nfp, "enrollment_nonresident_graduate", "enrollment_headcount_graduate"), weighted_intl_pct(groups$bacc_private_fp, "enrollment_nonresident_graduate", "enrollment_headcount_graduate")),
  make_row("Undergraduate students who are international", "percent", intl_ug[["all"]], intl_ug[["public"]], intl_ug[["private_nfp"]], intl_ug[["private_fp"]], weighted_intl_pct(groups$bacc_public, "enrollment_nonresident_undergrad", "enrollment_headcount_undergrad"), weighted_intl_pct(groups$bacc_private_nfp, "enrollment_nonresident_undergrad", "enrollment_headcount_undergrad"), weighted_intl_pct(groups$bacc_private_fp, "enrollment_nonresident_undergrad", "enrollment_headcount_undergrad"))
)

repeated_losses_share <- pct_by_group(function(df) yes_flag(df$losses_last_3_of_5))
revenue_change_median <- median_by_group("revenue_pct_change_5yr")
net_tuition_per_fte_change_median <- median_by_group("net_tuition_per_fte_change_5yr")
transfer_out_rate_median <- median_by_group("transfer_out_rate_bachelor")
transfer_out_change_median <- median_by_group("transfer_out_rate_bachelor_change_5yr")
staffing_cut_share <- pct_by_group(function(df) !is.na(df$staff_total_headcount_pct_change_5yr) & df$staff_total_headcount_pct_change_5yr < 0)
state_funding_change_median <- median_by_group("state_funding_pct_change_5yr")

summary_rows <- rbind(
  summary_rows,
  make_row("Repeated losses national share by sector", "percent", repeated_losses_share[["all"]], repeated_losses_share[["public"]], repeated_losses_share[["private_nfp"]], repeated_losses_share[["private_fp"]], repeated_losses_share[["bacc_public"]], repeated_losses_share[["bacc_private_nfp"]], repeated_losses_share[["bacc_private_fp"]], "Share with losses in 3 of last 5 years"),
  make_row("5-year revenue change by sector", "median percent", revenue_change_median[["all"]], revenue_change_median[["public"]], revenue_change_median[["private_nfp"]], revenue_change_median[["private_fp"]], revenue_change_median[["bacc_public"]], revenue_change_median[["bacc_private_nfp"]], revenue_change_median[["bacc_private_fp"]], "Median institution-level change"),
  make_row("5-year net tuition per FTE change by sector", "median percent", net_tuition_per_fte_change_median[["all"]], net_tuition_per_fte_change_median[["public"]], net_tuition_per_fte_change_median[["private_nfp"]], net_tuition_per_fte_change_median[["private_fp"]], net_tuition_per_fte_change_median[["bacc_public"]], net_tuition_per_fte_change_median[["bacc_private_nfp"]], net_tuition_per_fte_change_median[["bacc_private_fp"]], "Median institution-level change"),
  make_row("Transfer-out rate by sector", "median rate", transfer_out_rate_median[["all"]], transfer_out_rate_median[["public"]], transfer_out_rate_median[["private_nfp"]], transfer_out_rate_median[["private_fp"]], transfer_out_rate_median[["bacc_public"]], transfer_out_rate_median[["bacc_private_nfp"]], transfer_out_rate_median[["bacc_private_fp"]], "Bachelor cohort"),
  make_row("Transfer-out rate change over past 5 years by sector", "median point change", transfer_out_change_median[["all"]], transfer_out_change_median[["public"]], transfer_out_change_median[["private_nfp"]], transfer_out_change_median[["private_fp"]], transfer_out_change_median[["bacc_public"]], transfer_out_change_median[["bacc_private_nfp"]], transfer_out_change_median[["bacc_private_fp"]], "Bachelor cohort"),
  make_row("Staffing cuts national share by sector", "percent", staffing_cut_share[["all"]], staffing_cut_share[["public"]], staffing_cut_share[["private_nfp"]], staffing_cut_share[["private_fp"]], staffing_cut_share[["bacc_public"]], staffing_cut_share[["bacc_private_nfp"]], staffing_cut_share[["bacc_private_fp"]], "Share with 5-year staff headcount decline"),
  make_row("State-funding change by sector", "median percent", state_funding_change_median[["all"]], state_funding_change_median[["public"]], state_funding_change_median[["private_nfp"]], state_funding_change_median[["private_fp"]], state_funding_change_median[["bacc_public"]], state_funding_change_median[["bacc_private_nfp"]], state_funding_change_median[["bacc_private_fp"]], "Mostly meaningful for publics")
)

loss_2023_counts <- count_by_group_from(groups_2023, function(df) yes_flag(df$ended_year_at_loss))
loss_2023_pcts <- sapply(groups_2023, function(df) if (nrow(df) == 0) NA_real_ else safe_pct(sum(yes_flag(df$ended_year_at_loss), na.rm = TRUE), nrow(df)))
loss_2024_counts <- count_by_group(function(df) yes_flag(df$ended_year_at_loss))
loss_2024_pcts <- sapply(groups, function(df) if (nrow(df) == 0) NA_real_ else safe_pct(sum(yes_flag(df$ended_year_at_loss), na.rm = TRUE), nrow(df)))
summary_rows <- rbind(
  summary_rows,
  make_row("Ended fiscal year at a loss", "2023 count", loss_2023_counts[["all"]], loss_2023_counts[["public"]], loss_2023_counts[["private_nfp"]], loss_2023_counts[["private_fp"]], loss_2023_counts[["bacc_public"]], loss_2023_counts[["bacc_private_nfp"]], loss_2023_counts[["bacc_private_fp"]], "Compared with 2024"),
  make_row("Ended fiscal year at a loss", "2023 percent", loss_2023_pcts[["all"]], loss_2023_pcts[["public"]], loss_2023_pcts[["private_nfp"]], loss_2023_pcts[["private_fp"]], loss_2023_pcts[["bacc_public"]], loss_2023_pcts[["bacc_private_nfp"]], loss_2023_pcts[["bacc_private_fp"]], "Compared with 2024"),
  make_row("Ended fiscal year at a loss", "2024 count", loss_2024_counts[["all"]], loss_2024_counts[["public"]], loss_2024_counts[["private_nfp"]], loss_2024_counts[["private_fp"]], loss_2024_counts[["bacc_public"]], loss_2024_counts[["bacc_private_nfp"]], loss_2024_counts[["bacc_private_fp"]], "Compared with 2023"),
  make_row("Ended fiscal year at a loss", "2024 percent", loss_2024_pcts[["all"]], loss_2024_pcts[["public"]], loss_2024_pcts[["private_nfp"]], loss_2024_pcts[["private_fp"]], loss_2024_pcts[["bacc_public"]], loss_2024_pcts[["bacc_private_nfp"]], loss_2024_pcts[["bacc_private_fp"]], "Compared with 2023"),
  make_row("Change in institutions ending year at a loss", "count change 2024 minus 2023",
           loss_2024_counts[["all"]] - loss_2023_counts[["all"]],
           loss_2024_counts[["public"]] - loss_2023_counts[["public"]],
           loss_2024_counts[["private_nfp"]] - loss_2023_counts[["private_nfp"]],
           loss_2024_counts[["private_fp"]] - loss_2023_counts[["private_fp"]],
           loss_2024_counts[["bacc_public"]] - loss_2023_counts[["bacc_public"]],
           loss_2024_counts[["bacc_private_nfp"]] - loss_2023_counts[["bacc_private_nfp"]],
           loss_2024_counts[["bacc_private_fp"]] - loss_2023_counts[["bacc_private_fp"]],
           "2024 minus 2023")
)

top_fed <- top_metric_by_group("federal_grants_contracts_pell_adjusted_pct_core_revenue")
top_state <- top_metric_by_group("state_funding_pct_core_revenue")

summary_rows <- rbind(
  summary_rows,
  make_row(
    "Highest federal funding share of core revenue", "institution",
    if (!is.null(top_fed$all)) top_fed$all$institution_name else NA,
    if (!is.null(top_fed$public)) top_fed$public$institution_name else NA,
    if (!is.null(top_fed$private_nfp)) top_fed$private_nfp$institution_name else NA,
    if (!is.null(top_fed$private_fp)) top_fed$private_fp$institution_name else NA,
    if (!is.null(top_fed$bacc_public)) top_fed$bacc_public$institution_name else NA,
    if (!is.null(top_fed$bacc_private_nfp)) top_fed$bacc_private_nfp$institution_name else NA,
    if (!is.null(top_fed$bacc_private_fp)) top_fed$bacc_private_fp$institution_name else NA,
    "Pell-adjusted federal grants/contracts"
  ),
  make_row(
    "Highest federal funding share of core revenue", "percent",
    if (!is.null(top_fed$all)) top_fed$all$federal_grants_contracts_pell_adjusted_pct_core_revenue * 100 else NA,
    if (!is.null(top_fed$public)) top_fed$public$federal_grants_contracts_pell_adjusted_pct_core_revenue * 100 else NA,
    if (!is.null(top_fed$private_nfp)) top_fed$private_nfp$federal_grants_contracts_pell_adjusted_pct_core_revenue * 100 else NA,
    if (!is.null(top_fed$private_fp)) top_fed$private_fp$federal_grants_contracts_pell_adjusted_pct_core_revenue * 100 else NA,
    if (!is.null(top_fed$bacc_public)) top_fed$bacc_public$federal_grants_contracts_pell_adjusted_pct_core_revenue * 100 else NA,
    if (!is.null(top_fed$bacc_private_nfp)) top_fed$bacc_private_nfp$federal_grants_contracts_pell_adjusted_pct_core_revenue * 100 else NA,
    if (!is.null(top_fed$bacc_private_fp)) top_fed$bacc_private_fp$federal_grants_contracts_pell_adjusted_pct_core_revenue * 100 else NA,
    "Pell-adjusted federal grants/contracts"
  ),
  make_row(
    "Highest state funding share of core revenue", "institution",
    if (!is.null(top_state$all)) top_state$all$institution_name else NA,
    if (!is.null(top_state$public)) top_state$public$institution_name else NA,
    if (!is.null(top_state$private_nfp)) top_state$private_nfp$institution_name else NA,
    if (!is.null(top_state$private_fp)) top_state$private_fp$institution_name else NA,
    if (!is.null(top_state$bacc_public)) top_state$bacc_public$institution_name else NA,
    if (!is.null(top_state$bacc_private_nfp)) top_state$bacc_private_nfp$institution_name else NA,
    if (!is.null(top_state$bacc_private_fp)) top_state$bacc_private_fp$institution_name else NA,
    "State appropriations"
  ),
  make_row(
    "Highest state funding share of core revenue", "percent",
    if (!is.null(top_state$all)) top_state$all$state_funding_pct_core_revenue * 100 else NA,
    if (!is.null(top_state$public)) top_state$public$state_funding_pct_core_revenue * 100 else NA,
    if (!is.null(top_state$private_nfp)) top_state$private_nfp$state_funding_pct_core_revenue * 100 else NA,
    if (!is.null(top_state$private_fp)) top_state$private_fp$state_funding_pct_core_revenue * 100 else NA,
    if (!is.null(top_state$bacc_public)) top_state$bacc_public$state_funding_pct_core_revenue * 100 else NA,
    if (!is.null(top_state$bacc_private_nfp)) top_state$bacc_private_nfp$state_funding_pct_core_revenue * 100 else NA,
    if (!is.null(top_state$bacc_private_fp)) top_state$bacc_private_fp$state_funding_pct_core_revenue * 100 else NA,
    "State appropriations"
  )
)

sheet_index_rows <- do.call(rbind, list(
  make_row("Worksheet index", "Summary", "", "", "", "", "", "", "", "Top-level counts and sector shares. Includes all filtered institutions plus baccalaureate-only breakouts."),
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
  make_row("Worksheet index", "LossTuition", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges with repeated losses and high tuition dependence."),
  make_row("Worksheet index", "PrivNFPStress", "", "", "", "", "", "", "", "Predominantly baccalaureate private nonprofits with rising discount rates and falling net tuition per FTE."),
  make_row("Worksheet index", "MultiFront", "", "", "", "", "", "", "", "Predominantly baccalaureate colleges with high international share, high federal dependence, negative 5-year revenue change, and losses in at least 3 of the last 10 years.")
))

summary_rows <- rbind(sheet_index_rows, summary_rows)

all_sheet_columns <- c(
  "unitid","institution_name","state","city","control_label","sector","category","urbanization","religious_affiliation","all_programs_distance_education","closure_risk_track","private_closure_risk_score","public_campus_risk_score","staffing_cut_risk_score",
  "warning_score_core","enrollment_decline_last_3_of_5","revenue_10pct_drop_last_3_of_5","losses_last_3_of_5","ended_year_at_loss","loss_years_last_5","loss_years_last_10",
  "enrollment_headcount_total","enrollment_pct_change_5yr","enrollment_decreased_5yr","enrollment_change_1yr",
  "share_grad_students","pct_international_all","pct_international_undergraduate","pct_international_graduate","international_student_count_change_5yr","international_enrollment_pct_change_5yr","international_enrollment_increase_5yr","international_enrollment_change_10yr","international_enrollment_pct_change_10yr","international_enrollment_increase_10yr",
  "transfer_out_rate_bachelor","transfer_out_rate_bachelor_change_5yr","transfer_out_rate_bachelor_increase_5yr","transfer_out_rate_bachelor_change_10yr","transfer_out_rate_bachelor_increase_10yr",
  "staff_headcount_total","staff_headcount_instructional","staff_total_headcount_pct_change_5yr","staff_instructional_headcount_pct_change_5yr","staff_change_1yr",
  "revenue_total","revenue_pct_change_5yr","revenue_decreased_5yr","revenue_change_1yr","expenses_total","loss_amount",
  "net_tuition_per_fte","net_tuition_per_fte_change_5yr","tuition_dependence_pct",
  "discount_rate","discount_pct_change_5yr",
  "federal_grants_contracts_pell_adjusted","federal_grants_contracts_pell_adjusted_pct_core_revenue","federal_grants_contracts_pell_adjusted_pct_change_5yr",
  "state_funding","state_funding_pct_core_revenue","state_funding_pct_change_5yr",
  "research_expense","research_expense_per_fte","research_expense_pct_core_expenses","sector_research_spending_n","research_spending_per_fte_percentile","research_spending_peer_bucket",
  "endowment_value","endowment_pct_change_5yr",
  "liquidity","liquidity_percentile_private_nfp","leverage","leverage_percentile_private_nfp",
  "loan_year_latest","federal_loan_pct_most_recent","federal_loan_count_most_recent","federal_loan_avg_most_recent",
  "high_tuition_dependence","high_international_share","high_federal_dependence","multi_signal_score"
)

missing_all_sheet_columns <- setdiff(all_sheet_columns, names(latest))
for (nm in missing_all_sheet_columns) {
  latest[[nm]] <- NA
}
all_sheet <- latest[, all_sheet_columns, drop = FALSE]
all_sheet_bacc <- all_sheet[all_sheet$category == bacc_category_label, , drop = FALSE]

sort_df <- function(df, cols, decreasing = FALSE) {
  if (nrow(df) == 0) return(df)
  ord_args <- c(lapply(cols, function(col) {
    x <- df[[col]]
    if (decreasing) -xtfrm(x) else xtfrm(x)
  }), list(na.last = TRUE))
  df[do.call(order, ord_args), , drop = FALSE]
}

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
  LossTuition = loss_tuition,
  PrivNFPStress = priv_nfp_stress,
  MultiFront = multi_front
)

xml_cell <- function(value, style_id = NULL) {
  style_attr <- if (is.null(style_id)) "" else paste0(' ss:StyleID="', style_id, '"')
  if (length(value) == 0 || is.na(value) || identical(as.character(value), "")) {
    return(paste0('<Cell', style_attr, '><Data ss:Type="String"></Data></Cell>'))
  }
  num <- suppressWarnings(as.numeric(value))
  if (!is.na(num) && !grepl("[A-Za-z]", as.character(value))) {
    return(paste0('<Cell', style_attr, '><Data ss:Type="Number">', as.character(num), '</Data></Cell>'))
  }
  paste0('<Cell', style_attr, '><Data ss:Type="String">', escape_xml(value), '</Data></Cell>')
}

worksheet_xml <- function(name, df) {
  if (is.null(df) || nrow(df) == 0) {
    return(paste0(
      '<Worksheet ss:Name="', escape_xml(name), '"><Table>',
      '<Row><Cell ss:StyleID="Header"><Data ss:Type="String">No rows</Data></Cell></Row>',
      '</Table></Worksheet>'
    ))
  }
  headers <- names(df)
  out <- c(paste0('<Worksheet ss:Name="', escape_xml(name), '">'), '  <Table>')
  out <- c(out, paste0('    <Row>', paste(vapply(headers, xml_cell, character(1), style_id = "Header"), collapse = ""), '</Row>'))
  for (i in seq_len(nrow(df))) {
    vals <- unname(as.list(df[i, , drop = FALSE]))
    out <- c(out, paste0('    <Row>', paste(vapply(vals, xml_cell, character(1)), collapse = ""), '</Row>'))
  }
  out <- c(out, '  </Table>', '</Worksheet>')
  paste(out, collapse = "\n")
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

dir.create(dirname(output_xml), recursive = TRUE, showWarnings = FALSE)
writeLines(wb, output_xml, useBytes = TRUE)
cat(sprintf("Saved article workbook to %s\n", normalizePath(output_xml, winslash = "/", mustWork = FALSE)))
invisible(list(workbook = normalizePath(output_xml, winslash = "/", mustWork = FALSE)))
}

if (sys.nframe() == 0) {
  result <- main()
  quit(save = "no", status = 0)
}
