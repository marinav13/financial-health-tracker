# =========================================================================
# scripts/shared/workbook_table_helpers.R
# =========================================================================
#
# PURPOSE:
#   Provides core helpers for building and manipulating data frames that
#   become Excel workbook rows and tables. Handles data frame assembly,
#   risk scoring, percentage calculations, XML escaping, file I/O, and
#   cross-tab comparisons used throughout the workbook build pipeline.
#
# USAGE:
#   Source this after utils.R and before scripts that build workbook tabs.
#   All functions are available to downstream scripts.

# ---------------------------------------------------------------------------
# Boolean and flag helpers
# ---------------------------------------------------------------------------

# Converts input to TRUE if it equals "yes" (case-insensitive). Handles
# diverse yes/no formats from CSV imports.
yes_flag <- function(x) {
  trimws(tolower(as.character(x))) == "yes"
}

# ---------------------------------------------------------------------------
# Numeric calculation helpers
# ---------------------------------------------------------------------------

# Calculates percentage (num/den * 100), returning NA if either is NA or den is 0.
safe_pct <- function(num, den) {
  ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, (num / den) * 100)
}

# Escapes four XML-reserved characters (&, <, >, ") for safe embedding in SpreadsheetML.
# Must replace & first to avoid double-escaping.
escape_xml <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("&",  "&amp;",  x, fixed = TRUE)
  x <- gsub("<",  "&lt;",   x, fixed = TRUE)
  x <- gsub(">",  "&gt;",   x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

# ---------------------------------------------------------------------------
# Risk scoring helper
# ---------------------------------------------------------------------------

# Counts TRUE values across logical vectors (NA treated as 0). Shorthand for
# rowSums(cbind(...), na.rm = TRUE).
row_score <- function(...) rowSums(cbind(...), na.rm = TRUE)

# Computes a 0-6 distress score from six warning signals: enrollment decline,
# revenue drop, operating losses, ended year at loss, staffing cuts, and
# tuition decline (all 5-year metrics). Institutions scoring 4+ are flagged
# as distressed.
compute_warning_score_core <- function(df) {
  row_score(
    yes_flag(df$enrollment_decline_last_3_of_5),
    yes_flag(df$revenue_10pct_drop_last_3_of_5),
    yes_flag(df$losses_last_3_of_5),
    yes_flag(df$ended_year_at_loss),
    !is.na(df$staff_total_headcount_pct_change_5yr) & df$staff_total_headcount_pct_change_5yr < 0,
    !is.na(df$net_tuition_per_fte_change_5yr) & df$net_tuition_per_fte_change_5yr < 0
  )
}

# Returns the 75th percentile of x, or NA if all values are NA.
q75_safe <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE))
}

# Returns the 25th percentile of x, or NA if all values are NA.
q25_safe <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE))
}

# ---------------------------------------------------------------------------
# File I/O helpers
# ---------------------------------------------------------------------------

# Reads a CSV if it exists, otherwise returns an empty data frame.
read_csv_if_exists <- function(path) {
  if (!file.exists(path)) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
}

# Reads a required closure CSV, stopping with instructions if the file is missing.
read_required_closure_csv <- function(path) {
  if (!file.exists(path)) {
    stop(
      paste(
        "Missing required closure input:", path,
        "\nRun `python scripts/import_closure_sheet.py --sheet YOUR_CLOSURE_GOOGLE_SHEET_URL_OR_ID`",
        "\nand then rerun the workbook build."
      )
    )
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
}

# ---------------------------------------------------------------------------
# Summary-row factory
# ---------------------------------------------------------------------------

# Creates a single-row data frame for the Summary sheet, with values across
# eight institution groups (all, public, private NFP, private FP, and
# baccalaureate-only variants).
make_row <- function(metric, statistic,
                     all = NA, public = NA, private_nfp = NA, private_fp = NA,
                     bacc_public = NA, bacc_private_nfp = NA, bacc_private_fp = NA,
                     notes = "") {
  data.frame(
    metric         = metric,
    statistic      = statistic,
    all            = all,
    public         = public,
    private_nfp    = private_nfp,
    private_fp     = private_fp,
    bacc_public    = bacc_public,
    bacc_private_nfp = bacc_private_nfp,
    bacc_private_fp  = bacc_private_fp,
    notes          = notes,
    stringsAsFactors = FALSE
  )
}

# Combines multiple data frames, skipping NULL and zero-row inputs. Useful
# for building workbook tabs in stages.
append_rows <- function(...) {
  rows <- list(...)
  rows <- rows[!vapply(rows, is.null, logical(1))]
  rows <- rows[!vapply(rows, function(x) is.data.frame(x) && nrow(x) == 0, logical(1))]
  if (length(rows) == 0) return(data.frame(stringsAsFactors = FALSE))
  do.call(rbind, rows)
}

# Creates a Summary-sheet row from a named vector (e.g., "all", "public").
make_group_row <- function(metric, statistic, values, notes = "") {
  value_or_na <- function(name) {
    if (is.null(values) || !(name %in% names(values))) return(NA)
    unname(values[[name]])
  }

  make_row(
    metric,
    statistic,
    all = value_or_na("all"),
    public = value_or_na("public"),
    private_nfp = value_or_na("private_nfp"),
    private_fp = value_or_na("private_fp"),
    bacc_public = value_or_na("bacc_public"),
    bacc_private_nfp = value_or_na("bacc_private_nfp"),
    bacc_private_fp = value_or_na("bacc_private_fp"),
    notes = notes
  )
}

# Convenience wrapper that creates both a count row and a percent row for a metric.
make_count_pct_rows <- function(metric, counts, pcts, notes = "") {
  append_rows(
    make_group_row(metric, "count", counts, notes),
    make_group_row(metric, "percent", pcts, notes)
  )
}

# ---------------------------------------------------------------------------
# Data-frame utilities
# ---------------------------------------------------------------------------

# Sorts a data frame, placing NAs at the end regardless of sort direction.
sort_df <- function(df, cols, decreasing = FALSE) {
  if (nrow(df) == 0) return(df)
  ord_args <- c(lapply(cols, function(col) {
    x <- df[[col]]
    if (decreasing) -xtfrm(x) else xtfrm(x)
  }), list(na.last = TRUE))
  df[do.call(order, ord_args), , drop = FALSE]
}

# Filters, orders, and optionally limits rows of a data frame based on specs.
# Encapsulates common workbook tab processing patterns.
build_workbook_sheet <- function(df, filter_fn = NULL, order_fn = NULL, head_n = NULL) {
  out <- df
  if (!is.null(filter_fn)) {
    keep <- filter_fn(out)
    keep[is.na(keep)] <- FALSE
    out <- out[keep, , drop = FALSE]
  }
  if (!is.null(order_fn) && nrow(out) > 0) {
    out <- order_fn(out)
  }
  if (!is.null(head_n) && nrow(out) > head_n) {
    out <- utils::head(out, head_n)
  }
  out
}

# Applies build_workbook_sheet to each spec in a list, allowing different
# filters, ordering, and row limits per worksheet.
build_workbook_sheets <- function(df, specs) {
  stats::setNames(lapply(specs, function(spec) {
    if (!is.null(spec$data)) {
      return(spec$data)
    }
    build_workbook_sheet(
      df,
      filter_fn = spec$filter_fn %||% NULL,
      order_fn = spec$order_fn %||% NULL,
      head_n = spec$head_n %||% NULL
    )
  }), names(specs))
}

# Builds a worksheet index from a list of sheet specs (provides name and description).

# Creates a summary table of key financial health metrics aggregated by group.
build_benchmark_tab <- function(group_list, label_prefix = "") {
  rows <- lapply(names(group_list), function(gname) {
    df <- group_list[[gname]]
    data.frame(
      group = paste0(label_prefix, gname),
      institutions = nrow(df),
      repeated_losses_share = if (nrow(df) == 0) NA_real_ else safe_pct(sum(yes_flag(df$losses_last_3_of_5), na.rm = TRUE), nrow(df)),
      revenue_pct_change_5yr_median = {
        x <- to_num(df$revenue_pct_change_5yr); x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else stats::median(x)
      },
      net_tuition_per_fte_change_5yr_median = {
        x <- to_num(df$net_tuition_per_fte_change_5yr); x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else stats::median(x)
      },
      transfer_out_rate_bachelor_median = {
        x <- to_num(df$transfer_out_rate_bachelor); x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else stats::median(x)
      },
      transfer_out_rate_bachelor_change_5yr_median = {
        x <- to_num(df$transfer_out_rate_bachelor_change_5yr); x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else stats::median(x)
      },
      staffing_cut_share = if (nrow(df) == 0) NA_real_ else safe_pct(sum(!is.na(df$staff_total_headcount_pct_change_5yr) & df$staff_total_headcount_pct_change_5yr < 0, na.rm = TRUE), nrow(df)),
      state_funding_pct_change_5yr_median = {
        x <- to_num(df$state_funding_pct_change_5yr); x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else stats::median(x)
      },
      mean_state_funding_pct_change_5yr = {
        x <- to_num(df$state_funding_pct_change_5yr); x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else mean(x)
      },
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Builds the ReportAnswers tab: 16 key topline statistics with interpretation notes.
build_report_answers <- function(distress_compare, distress_intl10, flagship_cuts, staff_cut_yoy,
                                 latest_year = 2024L, comparison_year = latest_year - 5L,
                                 baseline_year = latest_year - 10L, prior_year = latest_year - 1L) {
  value_for_year <- function(field, year_value) {
    value <- distress_compare[[field]][distress_compare$year == year_value]
    if (length(value) == 0) NA else value[[1]]
  }
  staff_cutting_value <- function(year_value) {
    value <- staff_cut_yoy$institutions_cutting_staff[staff_cut_yoy$year == year_value]
    if (length(value) == 0) NA else value[[1]]
  }

  data.frame(
    question = c(
      sprintf("%s distressed institutions in the primarily baccalaureate universe", latest_year),
      sprintf("%s distressed institutions as a share of the primarily baccalaureate universe", latest_year),
      sprintf("%s institutions with at least a 10%% five-year enrollment drop", latest_year),
      sprintf("%s institutions with at least a 10%% five-year revenue drop", latest_year),
      sprintf("%s students enrolled at distressed institutions", latest_year),
      sprintf("%s long-running challenge institutions with enrollment dips and losses in 3 of the last 5 years", latest_year),
      sprintf("%s students enrolled at those long-running challenge institutions", latest_year),
      sprintf("%s distressed institutions in the same universe", comparison_year),
      sprintf("%s institutions with at least a 10%% five-year enrollment drop", comparison_year),
      sprintf("%s institutions with at least a 10%% five-year revenue drop", comparison_year),
      sprintf("%s long-running challenge institutions with enrollment dips and losses in 3 of the last 5 years", comparison_year),
      sprintf("%s comparison note", baseline_year),
      "Colleges in distress with rising international enrollment over 10 years",
      "Public flagships with still-disrupted federal research cuts",
      "Public flagships with at least $1M still disrupted in federal research cuts",
      sprintf("Institutions cutting staffing from %s to %s", prior_year, latest_year)
    ),
    value = c(
      value_for_year("distress_count", latest_year),
      value_for_year("distress_pct", latest_year),
      value_for_year("enrollment_drop_10pct_count", latest_year),
      value_for_year("revenue_drop_10pct_count", latest_year),
      value_for_year("distress_students", latest_year),
      value_for_year("longrun_count", latest_year),
      value_for_year("longrun_students", latest_year),
      value_for_year("distress_count", comparison_year),
      value_for_year("enrollment_drop_10pct_count", comparison_year),
      value_for_year("revenue_drop_10pct_count", comparison_year),
      value_for_year("longrun_count", comparison_year),
      value_for_year("comparison_note", baseline_year),
      nrow(distress_intl10),
      nrow(flagship_cuts),
      if (nrow(flagship_cuts) == 0) 0 else sum(flagship_cuts$total_disrupted_award_remaining >= 1e6, na.rm = TRUE),
      staff_cutting_value(latest_year)
    ),
    note = c(
      sprintf("warning_score_core >= 4 in the %s primarily baccalaureate workbook universe.", latest_year),
      sprintf("Share of the %s primarily baccalaureate workbook universe with warning_score_core >= 4.", latest_year),
      sprintf("Count of %s primarily baccalaureate institutions with enrollment_pct_change_5yr <= -10.", latest_year),
      sprintf("Count of %s primarily baccalaureate institutions with revenue_pct_change_5yr <= -10.", latest_year),
      sprintf("Sum of %s enrollment_headcount_total for institutions in the distress group.", latest_year),
      sprintf("Count of %s institutions where enrollment_decline_last_3_of_5 == Yes and losses_last_3_of_5 == Yes.", latest_year),
      sprintf("Sum of %s enrollment_headcount_total for institutions with both enrollment declines and repeated losses.", latest_year),
      sprintf("warning_score_core >= 4 in the %s primarily baccalaureate workbook universe.", comparison_year),
      sprintf("Count of %s primarily baccalaureate institutions with enrollment_pct_change_5yr <= -10.", comparison_year),
      sprintf("Count of %s primarily baccalaureate institutions with revenue_pct_change_5yr <= -10.", comparison_year),
      sprintf("Count of %s institutions where enrollment_decline_last_3_of_5 == Yes and losses_last_3_of_5 == Yes.", comparison_year),
      sprintf("%s lacks populated five-year trend fields in the canonical dataset, so it is not directly comparable to %s and %s on this framing.", baseline_year, comparison_year, latest_year),
      "Same distress definition, limited to institutions with international_enrollment_increase_10yr == Yes.",
      "Matched Grant Witness research-funding schools to the predefined flagship unitid list and kept positive still-disrupted totals only.",
      "Subset of public flagships with positive still-disrupted totals of at least $1 million.",
      sprintf("Counts institutions whose %s staff_headcount_total is below %s.", latest_year, prior_year)
    ),
    stringsAsFactors = FALSE
  )
}

# Builds the StateBySt tab: state-level summary of public institution funding trends.
build_state_breakdown <- function(df) {
  state_public <- df[
    df$control_label == "Public" &
      !is.na(df$state) &
      df$state != "",
    ,
    drop = FALSE
  ]
  out <- do.call(rbind, lapply(split(state_public, state_public$state), function(state_df) {
    state_df <- state_df[!is.na(state_df$state_funding_pct_change_5yr), , drop = FALSE]
    if (nrow(state_df) == 0) return(NULL)
    down_n <- sum(state_df$state_funding_pct_change_5yr < 0, na.rm = TRUE)
    up_n <- sum(state_df$state_funding_pct_change_5yr > 0, na.rm = TRUE)
    data.frame(
      state = state_df$state[1],
      public_institutions_with_state_change = nrow(state_df),
      state_funding_down_5yr_count = down_n,
      state_funding_down_5yr_percent = safe_pct(down_n, nrow(state_df)),
      state_funding_up_5yr_count = up_n,
      state_funding_up_5yr_percent = safe_pct(up_n, nrow(state_df)),
      mean_state_funding_pct_change_5yr = mean(state_df$state_funding_pct_change_5yr, na.rm = TRUE),
      median_state_funding_pct_change_5yr = median(state_df$state_funding_pct_change_5yr, na.rm = TRUE),
      mean_state_funding_pct_core_revenue = mean(state_df$state_funding_pct_core_revenue, na.rm = TRUE) * 100,
      median_state_funding_pct_core_revenue = median(state_df$state_funding_pct_core_revenue, na.rm = TRUE) * 100,
      biggest_state_funding_drop_pct_5yr = min(state_df$state_funding_pct_change_5yr, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))

  if (is.null(out) || nrow(out) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  out[order(out$state_funding_down_5yr_percent, out$mean_state_funding_pct_change_5yr, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
}

# Computes year-by-year staffing changes (total and instructional staff),
# including comparisons against prior year.
build_staff_cut_yoy <- function(read_df, start_year = 2014L, end_year = 2024L) {
  staff_cols <- c("unitid", "staff_headcount_total", "staff_headcount_instructional")
  base <- read_df[
    as.integer(read_df$year) == as.integer(start_year),
    staff_cols,
    drop = FALSE
  ]
  base_total <- base[!is.na(base$staff_headcount_total), , drop = FALSE]
  base_instr <- base[!is.na(base$staff_headcount_instructional), , drop = FALSE]

  baseline <- data.frame(
    year = as.integer(start_year),
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

  comparisons <- do.call(rbind, lapply(seq.int(start_year + 1L, end_year), function(y) {
    prev <- read_df[
      as.integer(read_df$year) == (as.integer(y) - 1L),
      staff_cols,
      drop = FALSE
    ]
    curr <- read_df[
      as.integer(read_df$year) == as.integer(y),
      staff_cols,
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
      year = as.integer(y),
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

  rbind(baseline, comparisons)
}

# Computes distress metrics (counts, percentages, long-running challenge)
# across multiple years for topline reporting.
build_distress_compare <- function(read_df, bacc_category_label, years = c(2024L, 2019L, 2014L)) {
  do.call(rbind, lapply(years, function(year_value) {
    year_df <- read_df[
      as.integer(read_df$year) == as.integer(year_value) &
        read_df$category == bacc_category_label,
      ,
      drop = FALSE
    ]
    year_df$warning_score_core <- compute_warning_score_core(year_df)
    distress_df <- year_df[
      !is.na(year_df$warning_score_core) &
        year_df$warning_score_core >= 4,
      ,
      drop = FALSE
    ]
    longrun_df <- year_df[
      year_df$enrollment_decline_last_3_of_5 == "Yes" &
        year_df$losses_last_3_of_5 == "Yes",
      ,
      drop = FALSE
    ]

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
  }))
}

# Applies a predicate function to each group data frame and returns match counts.
count_by_group_from <- function(group_list, pred) {
  sapply(group_list, function(df) sum(pred(df), na.rm = TRUE))
}

# Returns the percentage of matches for each group data frame.
pct_by_group_from <- function(group_list, pred) {
  sapply(group_list, function(df) {
    if (nrow(df) == 0) return(NA_real_)
    safe_pct(sum(pred(df), na.rm = TRUE), nrow(df))
  })
}

# Applies a numeric summary function (e.g., median) to one field across all groups.
numeric_stat_by_group <- function(group_list, field, stat_fn = stats::median) {
  sapply(group_list, function(df) {
    x <- to_num(df[[field]])
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    stat_fn(x, na.rm = TRUE)
  })
}

# Returns the top (or bottom) row by a metric for each group, or NULL if all NA.
top_metric_by_group_from <- function(group_list, metric, decreasing = TRUE) {
  lapply(group_list, function(df) {
    keep <- !is.na(df[[metric]])
    if (!any(keep)) return(NULL)
    df <- df[keep, , drop = FALSE]
    df[order(df[[metric]], decreasing = decreasing), , drop = FALSE][1, , drop = FALSE]
  })
}

# Calculates weighted percentage: sum(num_col) / sum(den_col) * 100.
weighted_intl_pct <- function(df, num_col, den_col) {
  keep <- !is.na(df[[num_col]]) & !is.na(df[[den_col]]) & df[[den_col]] > 0
  if (!any(keep)) return(NA_real_)
  safe_pct(sum(df[[num_col]][keep], na.rm = TRUE), sum(df[[den_col]][keep], na.rm = TRUE))
}

# ---------------------------------------------------------------------------
# Cross-tab helpers for event-vs-non-event comparisons
# ---------------------------------------------------------------------------

# Summarizes a subset into a single row of financial health metrics.
summarize_event_subset <- function(df, cohort_label, event_type, control_scope = "All") {
  median_or_na <- function(x) {
    x <- to_num(x)
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    median(x, na.rm = TRUE)
  }

  data.frame(
    event_type      = event_type,
    control_scope   = control_scope,
    cohort          = cohort_label,
    institutions    = nrow(df),
    median_finance_page_bad_count              = median_or_na(df$finance_page_bad_count),
    median_warning_score_core                  = median_or_na(df$warning_score_core),
    median_enrollment_pct_change_5yr           = median_or_na(df$enrollment_pct_change_5yr),
    enrollment_decline_last_3_of_5_pct         = if (nrow(df) == 0) NA_real_ else safe_pct(sum(yes_flag(df$enrollment_decline_last_3_of_5), na.rm = TRUE), nrow(df)),
    median_revenue_pct_change_5yr              = median_or_na(df$revenue_pct_change_5yr),
    revenue_decreased_5yr_pct                  = if (nrow(df) == 0) NA_real_ else safe_pct(sum(yes_flag(df$revenue_decreased_5yr), na.rm = TRUE), nrow(df)),
    revenue_10pct_drop_last_3_of_5_pct         = if (nrow(df) == 0) NA_real_ else safe_pct(sum(yes_flag(df$revenue_10pct_drop_last_3_of_5), na.rm = TRUE), nrow(df)),
    ended_2024_at_loss_pct                     = if (nrow(df) == 0) NA_real_ else safe_pct(sum(yes_flag(df$ended_year_at_loss), na.rm = TRUE), nrow(df)),
    losses_last_3_of_5_pct                     = if (nrow(df) == 0) NA_real_ else safe_pct(sum(yes_flag(df$losses_last_3_of_5), na.rm = TRUE), nrow(df)),
    median_staff_total_headcount_pct_change_5yr       = median_or_na(df$staff_total_headcount_pct_change_5yr),
    staff_total_decline_5yr_pct                       = if (nrow(df) == 0) NA_real_ else safe_pct(sum(!is.na(df$staff_total_headcount_pct_change_5yr) & df$staff_total_headcount_pct_change_5yr < 0, na.rm = TRUE), nrow(df)),
    median_staff_instructional_headcount_pct_change_5yr = median_or_na(df$staff_instructional_headcount_pct_change_5yr),
    staff_instructional_decline_5yr_pct               = if (nrow(df) == 0) NA_real_ else safe_pct(sum(!is.na(df$staff_instructional_headcount_pct_change_5yr) & df$staff_instructional_headcount_pct_change_5yr < 0, na.rm = TRUE), nrow(df)),
    median_net_tuition_per_fte_change_5yr      = median_or_na(df$net_tuition_per_fte_change_5yr),
    median_tuition_dependence_pct              = median_or_na(df$tuition_dependence_pct),
    median_pct_international_all_pct           = median_or_na(df$pct_international_all) * 100,
    stringsAsFactors = FALSE
  )
}

# Builds "with event" vs "without event" cross-tab rows across four control slices.
build_event_xtab <- function(base_df, event_unitids, event_type) {
  event_unitids <- unique(as.character(event_unitids[!is.na(event_unitids)]))
  base_df$.__event_flag__ <- as.character(base_df$unitid) %in% event_unitids

  scopes <- list(
    All                    = base_df,
    Public                 = base_df[base_df$control_label == "Public",               , drop = FALSE],
    `Private not-for-profit` = base_df[base_df$control_label == "Private not-for-profit", , drop = FALSE],
    `Private for-profit`   = base_df[base_df$control_label == "Private for-profit",   , drop = FALSE]
  )

  rows <- lapply(names(scopes), function(scope_name) {
    scope_df <- scopes[[scope_name]]
    rbind(
      summarize_event_subset(scope_df[ scope_df$.__event_flag__, , drop = FALSE], "With event",    event_type, scope_name),
      summarize_event_subset(scope_df[!scope_df$.__event_flag__, , drop = FALSE], "Without event", event_type, scope_name)
    )
  })

  out <- do.call(rbind, rows)
  out$event_institution_count <- length(event_unitids)
  out
}

# ---------------------------------------------------------------------------
# SpreadsheetML / XML output helpers
# ---------------------------------------------------------------------------

# Returns a stable fingerprint for detecting duplicate worksheets before writing XML.
