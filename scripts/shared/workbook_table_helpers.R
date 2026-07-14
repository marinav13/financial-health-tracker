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

# Publics now prefer the composite state-and-local-support fields, but the
# workbook can still fall back to legacy appropriations-only columns when
# older inputs are loaded.
state_support_change_col <- function(df) {
  if ("state_local_support_pct_change_5yr" %in% names(df)) "state_local_support_pct_change_5yr" else "state_funding_pct_change_5yr"
}

state_support_share_col <- function(df) {
  if ("state_local_support_pct_core_revenue" %in% names(df)) "state_local_support_pct_core_revenue" else "state_funding_pct_core_revenue"
}

state_support_amount_col <- function(df) {
  if ("state_local_support" %in% names(df)) "state_local_support" else "state_funding"
}

state_support_change_values <- function(df) {
  to_num(df[[state_support_change_col(df)]])
}

state_support_share_values <- function(df) {
  to_num(df[[state_support_share_col(df)]])
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
    state_support_change <- state_support_change_values(df)
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
      state_support_pct_change_5yr_median = {
        x <- state_support_change[!is.na(state_support_change)]; if (length(x) == 0) NA_real_ else stats::median(x)
      },
      mean_state_support_pct_change_5yr = {
        x <- state_support_change[!is.na(state_support_change)]; if (length(x) == 0) NA_real_ else mean(x)
      },
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Legacy ReportAnswers builder retained temporarily for reference while the
# workbook uses the newer grouped answer-tab builders defined below.
build_report_answers_legacy <- function(distress_compare, distress_intl10, flagship_cuts, staff_cut_yoy,
                                 all_sheet_bacc = data.frame(stringsAsFactors = FALSE),
                                 accredit_finance_xtab = data.frame(stringsAsFactors = FALSE),
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

  format_value <- function(x, digits = NULL, suffix = "") {
    if (length(x) == 0 || is.null(x) || all(is.na(x))) return(NA_character_)
    val <- x[[1]]
    if (is.numeric(val)) {
      if (!is.null(digits)) {
        val <- formatC(val, format = "f", digits = digits, big.mark = ",")
      } else {
        val <- format(round(val), big.mark = ",", scientific = FALSE, trim = TRUE)
      }
    }
    paste0(as.character(val), suffix)
  }
  format_count_student_value <- function(count, denominator, students = NULL) {
    pieces <- c(
      sprintf("%s of %s institutions", format_value(count), format_value(denominator))
    )
    if (!is.null(students) && !is.na(students)) {
      pieces <- c(pieces, sprintf("%s students", format_value(students)))
    }
    paste(pieces, collapse = "; ")
  }
  format_pct_comparison <- function(a, b, digits = 1) {
    sprintf("%s vs %s", format_value(a, digits = digits, suffix = "%"), format_value(b, digits = digits, suffix = "%"))
  }
  make_answer_row <- function(question, value, calculation, note) {
    data.frame(
      question = question,
      value = if (length(value) == 0 || is.null(value) || all(is.na(value))) NA_character_ else as.character(value[[1]]),
      calculation = calculation,
      note = note,
      stringsAsFactors = FALSE
    )
  }
  subset_stats <- function(df, predicate) {
    if (nrow(df) == 0) {
      return(list(
        count = 0L,
        denominator = 0L,
        students = NA_real_,
        pct = NA_real_
      ))
    }
    matches <- predicate(df)
    matches[is.na(matches)] <- FALSE
    subset_df <- df[matches, , drop = FALSE]
    list(
      count = nrow(subset_df),
      denominator = nrow(df),
      students = if ("enrollment_headcount_total" %in% names(subset_df)) {
        sum(to_num(subset_df$enrollment_headcount_total), na.rm = TRUE)
      } else {
        NA_real_
      },
      pct = safe_pct(nrow(subset_df), nrow(df))
    )
  }
  xtab_value <- function(event_type, cohort, field) {
    if (nrow(accredit_finance_xtab) == 0) return(NA)
    value <- accredit_finance_xtab[[field]][
      accredit_finance_xtab$event_type == event_type &
        accredit_finance_xtab$control_scope == "All" &
        accredit_finance_xtab$cohort == cohort
    ]
    if (length(value) == 0) NA else value[[1]]
  }

  all_2024_stats <- subset_stats(all_sheet_bacc, function(df) rep(TRUE, nrow(df)))
  private_nfp_df <- all_sheet_bacc[all_sheet_bacc$control_label == "Private not-for-profit", , drop = FALSE]
  public_df <- all_sheet_bacc[all_sheet_bacc$control_label == "Public", , drop = FALSE]
  private_fp_df <- all_sheet_bacc[all_sheet_bacc$control_label == "Private for-profit", , drop = FALSE]
  scope_dfs <- list(
    Public = public_df,
    `Private not-for-profit` = private_nfp_df,
    `Private for-profit` = private_fp_df
  )

  net_tuition_down_stats <- subset_stats(all_sheet_bacc, function(df) !is.na(df$net_tuition_per_fte_change_5yr) & to_num(df$net_tuition_per_fte_change_5yr) < 0)
  private_nfp_net_tuition_down_stats <- subset_stats(private_nfp_df, function(df) !is.na(df$net_tuition_per_fte_change_5yr) & to_num(df$net_tuition_per_fte_change_5yr) < 0)
  private_nfp_stress_stats <- subset_stats(private_nfp_df, function(df) !is.na(df$discount_pct_change_5yr) & to_num(df$discount_pct_change_5yr) > 0 & !is.na(df$net_tuition_per_fte_change_5yr) & to_num(df$net_tuition_per_fte_change_5yr) < 0)
  public_distress_stats <- subset_stats(public_df, function(df) !is.na(df$warning_score_core) & to_num(df$warning_score_core) >= 4)
  private_nfp_distress_stats <- subset_stats(private_nfp_df, function(df) !is.na(df$warning_score_core) & to_num(df$warning_score_core) >= 4)
  all3signals_stats <- subset_stats(all_sheet_bacc, function(df) yes_flag(df$enrollment_decline_last_3_of_5) & yes_flag(df$revenue_10pct_drop_last_3_of_5) & yes_flag(df$losses_last_3_of_5))
  enroll_decline_stats <- subset_stats(all_sheet_bacc, function(df) yes_flag(df$enrollment_decline_last_3_of_5))
  enroll_10pct_5yr_stats <- subset_stats(all_sheet_bacc, function(df) !is.na(df$enrollment_pct_change_5yr) & to_num(df$enrollment_pct_change_5yr) <= -10)
  staff_cut_5yr_stats <- subset_stats(all_sheet_bacc, function(df) !is.na(df$staff_total_headcount_pct_change_5yr) & to_num(df$staff_total_headcount_pct_change_5yr) < 0)
  rev_10pct_3of5_stats <- subset_stats(all_sheet_bacc, function(df) yes_flag(df$revenue_10pct_drop_last_3_of_5))
  losses_3of5_stats <- subset_stats(all_sheet_bacc, function(df) yes_flag(df$losses_last_3_of_5))
  private_nfp_enroll_loss_stats <- subset_stats(private_nfp_df, function(df) yes_flag(df$enrollment_decline_last_3_of_5) & yes_flag(df$losses_last_3_of_5))
  enroll10_rev10_stats <- subset_stats(all_sheet_bacc, function(df) !is.na(df$enrollment_pct_change_5yr) & to_num(df$enrollment_pct_change_5yr) <= -10 & yes_flag(df$revenue_10pct_drop_last_3_of_5))
  enroll10_loss_stats <- subset_stats(all_sheet_bacc, function(df) !is.na(df$enrollment_pct_change_5yr) & to_num(df$enrollment_pct_change_5yr) <= -10 & yes_flag(df$losses_last_3_of_5))
  rev10_loss_stats <- subset_stats(all_sheet_bacc, function(df) yes_flag(df$revenue_10pct_drop_last_3_of_5) & yes_flag(df$losses_last_3_of_5))
  enroll10_rev10_loss_stats <- subset_stats(all_sheet_bacc, function(df) !is.na(df$enrollment_pct_change_5yr) & to_num(df$enrollment_pct_change_5yr) <= -10 & yes_flag(df$revenue_10pct_drop_last_3_of_5) & yes_flag(df$losses_last_3_of_5))
  private_nfp_enroll10_loss_stats <- subset_stats(private_nfp_df, function(df) !is.na(df$enrollment_pct_change_5yr) & to_num(df$enrollment_pct_change_5yr) <= -10 & yes_flag(df$losses_last_3_of_5))
  sector_metric_specs <- list(
    list(
      question_suffix = "with enrollment declines in at least 3 of the last 5 year-to-year comparisons",
      calculation_suffix = "enrollment_decline_last_3_of_5 == Yes",
      note = "Repeated year-over-year decline flag, not the five-year 10% threshold version.",
      predicate = function(df) yes_flag(df$enrollment_decline_last_3_of_5)
    ),
    list(
      question_suffix = "with at least a 10% five-year enrollment drop",
      calculation_suffix = "enrollment_pct_change_5yr <= -10",
      note = "Five-year enrollment threshold version.",
      predicate = function(df) !is.na(df$enrollment_pct_change_5yr) & to_num(df$enrollment_pct_change_5yr) <= -10
    ),
    list(
      question_suffix = "with a revenue drop of at least 10% in 3 of the last 5 years",
      calculation_suffix = "revenue_10pct_drop_last_3_of_5 == Yes",
      note = "Repeated 10% year-over-year revenue-drop flag.",
      predicate = function(df) yes_flag(df$revenue_10pct_drop_last_3_of_5)
    ),
    list(
      question_suffix = "with at least a 10% five-year revenue drop",
      calculation_suffix = "revenue_pct_change_5yr <= -10",
      note = "Five-year revenue threshold version.",
      predicate = function(df) !is.na(df$revenue_pct_change_5yr) & to_num(df$revenue_pct_change_5yr) <= -10
    ),
    list(
      question_suffix = "that cut total staff over the past 5 years",
      calculation_suffix = "staff_total_headcount_pct_change_5yr < 0",
      note = sprintf("Means %s staff_headcount_total is below %s.", latest_year, latest_year - 5L),
      predicate = function(df) !is.na(df$staff_total_headcount_pct_change_5yr) & to_num(df$staff_total_headcount_pct_change_5yr) < 0
    ),
    list(
      question_suffix = "with losses in 3 of the last 5 years",
      calculation_suffix = "losses_last_3_of_5 == Yes",
      note = "Repeated-loss flag based on at least three negative operating-margin years in the five-year window.",
      predicate = function(df) yes_flag(df$losses_last_3_of_5)
    ),
    list(
      question_suffix = "with falling net tuition per FTE over the past 5 years",
      calculation_suffix = "net_tuition_per_fte_change_5yr < 0",
      note = "Inflation-adjusted net tuition revenue per FTE declined over five years.",
      predicate = function(df) !is.na(df$net_tuition_per_fte_change_5yr) & to_num(df$net_tuition_per_fte_change_5yr) < 0
    ),
    list(
      question_suffix = "with rising discount rates and falling net tuition per FTE",
      calculation_suffix = "discount_pct_change_5yr > 0 AND net_tuition_per_fte_change_5yr < 0",
      note = "Discounting more heavily while net tuition collected per student is falling.",
      predicate = function(df) !is.na(df$discount_pct_change_5yr) & to_num(df$discount_pct_change_5yr) > 0 & !is.na(df$net_tuition_per_fte_change_5yr) & to_num(df$net_tuition_per_fte_change_5yr) < 0
    ),
    list(
      question_suffix = "with both enrollment declines in at least 3 of the last 5 year-to-year comparisons and a revenue drop of at least 10% in 3 of the last 5 years",
      calculation_suffix = "enrollment_decline_last_3_of_5 == Yes AND revenue_10pct_drop_last_3_of_5 == Yes",
      note = "Repeated enrollment-decline flag combined with repeated 10% revenue-drop flag.",
      predicate = function(df) yes_flag(df$enrollment_decline_last_3_of_5) & yes_flag(df$revenue_10pct_drop_last_3_of_5)
    ),
    list(
      question_suffix = "with both a five-year enrollment drop of at least 10% and a revenue drop of at least 10% in 3 of the last 5 years",
      calculation_suffix = "enrollment_pct_change_5yr <= -10 AND revenue_10pct_drop_last_3_of_5 == Yes",
      note = "Five-year enrollment threshold combined with repeated 10% revenue-drop flag.",
      predicate = function(df) !is.na(df$enrollment_pct_change_5yr) & to_num(df$enrollment_pct_change_5yr) <= -10 & yes_flag(df$revenue_10pct_drop_last_3_of_5)
    ),
    list(
      question_suffix = "with both enrollment declines in at least 3 of the last 5 year-to-year comparisons and losses in 3 of the last 5 years",
      calculation_suffix = "enrollment_decline_last_3_of_5 == Yes AND losses_last_3_of_5 == Yes",
      note = "Repeated enrollment-decline flag combined with repeated-loss flag.",
      predicate = function(df) yes_flag(df$enrollment_decline_last_3_of_5) & yes_flag(df$losses_last_3_of_5)
    ),
    list(
      question_suffix = "with both a five-year enrollment drop of at least 10% and losses in 3 of the last 5 years",
      calculation_suffix = "enrollment_pct_change_5yr <= -10 AND losses_last_3_of_5 == Yes",
      note = "Five-year enrollment threshold combined with repeated-loss flag.",
      predicate = function(df) !is.na(df$enrollment_pct_change_5yr) & to_num(df$enrollment_pct_change_5yr) <= -10 & yes_flag(df$losses_last_3_of_5)
    ),
    list(
      question_suffix = "with a revenue drop of at least 10% in 3 of the last 5 years and losses in 3 of the last 5 years",
      calculation_suffix = "revenue_10pct_drop_last_3_of_5 == Yes AND losses_last_3_of_5 == Yes",
      note = "Repeated 10% revenue-drop flag combined with repeated-loss flag.",
      predicate = function(df) yes_flag(df$revenue_10pct_drop_last_3_of_5) & yes_flag(df$losses_last_3_of_5)
    ),
    list(
      question_suffix = "with enrollment declines in at least 3 of the last 5 year-to-year comparisons, a revenue drop of at least 10% in 3 of the last 5 years, and losses in 3 of the last 5 years",
      calculation_suffix = "enrollment_decline_last_3_of_5 == Yes AND revenue_10pct_drop_last_3_of_5 == Yes AND losses_last_3_of_5 == Yes",
      note = "Most severe repeated-signal combination using the year-over-year enrollment flag.",
      predicate = function(df) yes_flag(df$enrollment_decline_last_3_of_5) & yes_flag(df$revenue_10pct_drop_last_3_of_5) & yes_flag(df$losses_last_3_of_5)
    ),
    list(
      question_suffix = "with a five-year enrollment drop of at least 10%, a revenue drop of at least 10% in 3 of the last 5 years, and losses in 3 of the last 5 years",
      calculation_suffix = "enrollment_pct_change_5yr <= -10 AND revenue_10pct_drop_last_3_of_5 == Yes AND losses_last_3_of_5 == Yes",
      note = "Most severe 10%-threshold combination.",
      predicate = function(df) !is.na(df$enrollment_pct_change_5yr) & to_num(df$enrollment_pct_change_5yr) <= -10 & yes_flag(df$revenue_10pct_drop_last_3_of_5) & yes_flag(df$losses_last_3_of_5)
    )
  )
  build_sector_breakdown_rows <- function(scope_name, scope_df, metric_specs) {
    rows <- lapply(metric_specs, function(spec) {
      stats <- subset_stats(scope_df, spec$predicate)
      make_answer_row(
        sprintf("%s %s institutions %s", latest_year, tolower(scope_name), spec$question_suffix),
        format_count_student_value(stats$count, stats$denominator, stats$students),
        sprintf("Within %s %s rows in All_2024, count %s; students = sum(enrollment_headcount_total).", latest_year, tolower(scope_name), spec$calculation_suffix),
        sprintf("%s Sector breakout.", spec$note)
      )
    })
    do.call(rbind, rows)
  }

  distress_count_latest <- value_for_year("distress_count", latest_year)
  distress_pct_latest <- value_for_year("distress_pct", latest_year)
  distress_students_latest <- value_for_year("distress_students", latest_year)
  distress_universe_latest <- value_for_year("institutions_total", latest_year)
  distress_count_comparison <- value_for_year("distress_count", comparison_year)
  distress_pct_comparison <- value_for_year("distress_pct", comparison_year)
  comparison_universe <- value_for_year("institutions_total", comparison_year)

  rows <- list(
    make_answer_row(
      sprintf("%s distressed institutions in the primarily baccalaureate universe", latest_year),
      format_value(distress_count_latest),
      sprintf("Count %s rows in DistressCore where warning_score_core >= 4.", latest_year),
      sprintf("warning_score_core >= 4 in the %s primarily baccalaureate workbook universe.", latest_year)
    ),
    make_answer_row(
      sprintf("%s distressed institutions as a share of the primarily baccalaureate universe", latest_year),
      format_value(distress_pct_latest, digits = 1, suffix = "%"),
      sprintf("distress_count / institutions_total for %s in DistressCompare.", latest_year),
      sprintf("Share of the %s primarily baccalaureate workbook universe with warning_score_core >= 4.", latest_year)
    ),
    make_answer_row(
      sprintf("%s institutions with at least a 10%% five-year enrollment drop", latest_year),
      format_value(value_for_year("enrollment_drop_10pct_count", latest_year)),
      sprintf("Count %s primarily baccalaureate institutions with enrollment_pct_change_5yr <= -10.", latest_year),
      sprintf("Five-year enrollment change uses the %s versus %s comparison.", latest_year, latest_year - 5L)
    ),
    make_answer_row(
      sprintf("%s institutions with at least a 10%% five-year revenue drop", latest_year),
      format_value(value_for_year("revenue_drop_10pct_count", latest_year)),
      sprintf("Count %s primarily baccalaureate institutions with revenue_pct_change_5yr <= -10.", latest_year),
      sprintf("Five-year revenue change uses inflation-adjusted revenue totals for %s versus %s.", latest_year, latest_year - 5L)
    ),
    make_answer_row(
      sprintf("%s students enrolled at distressed institutions", latest_year),
      format_value(distress_students_latest),
      sprintf("Sum %s enrollment_headcount_total for rows where warning_score_core >= 4.", latest_year),
      sprintf("Student total covers the %s primarily baccalaureate distress group.", latest_year)
    ),
    make_answer_row(
      sprintf("%s long-running challenge institutions with enrollment dips and losses in 3 of the last 5 years", latest_year),
      format_value(value_for_year("longrun_count", latest_year)),
      sprintf("Count %s rows where enrollment_decline_last_3_of_5 == Yes and losses_last_3_of_5 == Yes.", latest_year),
      sprintf("This is the all-sector version of the repeated enrollment-losses framing for %s.", latest_year)
    ),
    make_answer_row(
      sprintf("%s students enrolled at those long-running challenge institutions", latest_year),
      format_value(value_for_year("longrun_students", latest_year)),
      sprintf("Sum %s enrollment_headcount_total for rows where enrollment_decline_last_3_of_5 == Yes and losses_last_3_of_5 == Yes.", latest_year),
      sprintf("Student total for institutions with both repeated enrollment declines and repeated losses in %s.", latest_year)
    ),
    make_answer_row(
      sprintf("%s distressed institutions in the same universe", comparison_year),
      format_value(distress_count_comparison),
      sprintf("Count %s rows in DistressCompare where warning_score_core >= 4.", comparison_year),
      sprintf("Uses the same primarily baccalaureate distress definition for %s.", comparison_year)
    ),
    make_answer_row(
      sprintf("%s institutions with at least a 10%% five-year enrollment drop", comparison_year),
      format_value(value_for_year("enrollment_drop_10pct_count", comparison_year)),
      sprintf("Count %s primarily baccalaureate institutions with enrollment_pct_change_5yr <= -10.", comparison_year),
      sprintf("Five-year enrollment change uses the %s versus %s comparison.", comparison_year, comparison_year - 5L)
    ),
    make_answer_row(
      sprintf("%s institutions with at least a 10%% five-year revenue drop", comparison_year),
      format_value(value_for_year("revenue_drop_10pct_count", comparison_year)),
      sprintf("Count %s primarily baccalaureate institutions with revenue_pct_change_5yr <= -10.", comparison_year),
      sprintf("Five-year revenue change uses inflation-adjusted revenue totals for %s versus %s.", comparison_year, comparison_year - 5L)
    ),
    make_answer_row(
      sprintf("%s long-running challenge institutions with enrollment dips and losses in 3 of the last 5 years", comparison_year),
      format_value(value_for_year("longrun_count", comparison_year)),
      sprintf("Count %s rows where enrollment_decline_last_3_of_5 == Yes and losses_last_3_of_5 == Yes.", comparison_year),
      sprintf("Comparable long-running challenge count for %s.", comparison_year)
    ),
    make_answer_row(
      sprintf("%s comparison note", baseline_year),
      format_value(value_for_year("comparison_note", baseline_year)),
      sprintf("Read the comparison_note field for %s in DistressCompare.", baseline_year),
      sprintf("%s lacks populated five-year trend fields in the canonical dataset, so it is not directly comparable to %s and %s on this framing.", baseline_year, comparison_year, latest_year)
    ),
    make_answer_row(
      "Colleges in distress with rising international enrollment over 10 years",
      format_value(nrow(distress_intl10)),
      sprintf("Count DistressIntl10 rows where warning_score_core >= 4 and international_enrollment_increase_10yr == Yes in %s.", latest_year),
      "Same distress definition, limited to institutions with international_enrollment_increase_10yr == Yes."
    ),
    make_answer_row(
      "Public flagships with still-disrupted federal research cuts",
      format_value(nrow(flagship_cuts)),
      "Count FlagshipCuts rows with positive still-disrupted Grant Witness totals.",
      "Matched Grant Witness research-funding schools to the predefined flagship unitid list and kept positive still-disrupted totals only."
    ),
    make_answer_row(
      "Public flagships with at least $1M still disrupted in federal research cuts",
      format_value(if (nrow(flagship_cuts) == 0) 0 else sum(flagship_cuts$total_disrupted_award_remaining >= 1e6, na.rm = TRUE)),
      "Count FlagshipCuts rows where total_disrupted_award_remaining >= 1,000,000.",
      "Subset of public flagships with positive still-disrupted totals of at least $1 million."
    ),
    make_answer_row(
      sprintf("Institutions cutting staffing from %s to %s", prior_year, latest_year),
      format_value(staff_cutting_value(latest_year)),
      sprintf("Read StaffCutsYoY for %s and count institutions whose %s staff_headcount_total is below %s.", latest_year, latest_year, prior_year),
      sprintf("Year-over-year staffing comparison, not the five-year staff-cut metric.")
    ),
    make_answer_row(
      sprintf("%s institutions with falling net tuition per FTE over the past 5 years", latest_year),
      format_count_student_value(net_tuition_down_stats$count, net_tuition_down_stats$denominator, net_tuition_down_stats$students),
      sprintf("Count %s rows in All_2024 where net_tuition_per_fte_change_5yr < 0; students = sum(enrollment_headcount_total) for matching rows.", latest_year),
      "Systemwide squeeze measure using inflation-adjusted net tuition revenue per FTE."
    ),
    make_answer_row(
      sprintf("%s private nonprofits with falling net tuition per FTE over the past 5 years", latest_year),
      format_count_student_value(private_nfp_net_tuition_down_stats$count, private_nfp_net_tuition_down_stats$denominator, private_nfp_net_tuition_down_stats$students),
      sprintf("Within %s private not-for-profit rows in All_2024, count net_tuition_per_fte_change_5yr < 0; students = sum(enrollment_headcount_total).", latest_year),
      "Private-nonprofit breakout of the broader net tuition per FTE decline measure."
    ),
    make_answer_row(
      sprintf("%s private nonprofits with rising discount rates and falling net tuition per FTE", latest_year),
      format_count_student_value(private_nfp_stress_stats$count, private_nfp_stress_stats$denominator, private_nfp_stress_stats$students),
      sprintf("Within %s private not-for-profit rows in All_2024, count discount_pct_change_5yr > 0 AND net_tuition_per_fte_change_5yr < 0; students = sum(enrollment_headcount_total).", latest_year),
      "Private-college pricing squeeze: more discounting but less net tuition per student."
    ),
    make_answer_row(
      "How the workbook defines rising discount rates and falling net tuition per FTE",
      "discount_pct_change_5yr > 0 AND net_tuition_per_fte_change_5yr < 0",
      "Row qualifies when the five-year discount-rate change is positive and the five-year inflation-adjusted net tuition revenue per FTE change is negative.",
      "In plain English: the institution is discounting more heavily while net tuition collected per student is falling."
    ),
    make_answer_row(
      sprintf("Core distress trend from %s to %s", comparison_year, latest_year),
      sprintf(
        "%s of %s (%s) in %s vs %s of %s (%s) in %s",
        format_value(distress_count_latest),
        format_value(if (is.na(distress_universe_latest)) all_2024_stats$denominator else distress_universe_latest),
        format_value(distress_pct_latest, digits = 1, suffix = "%"),
        latest_year,
        format_value(distress_count_comparison),
        format_value(comparison_universe),
        format_value(distress_pct_comparison, digits = 1, suffix = "%"),
        comparison_year
      ),
      sprintf("Compare DistressCompare rows for %s and %s using warning_score_core >= 4.", latest_year, comparison_year),
      "Uses the workbook's core six-signal distress definition."
    ),
    make_answer_row(
      sprintf("%s distress share for private nonprofits versus publics", latest_year),
      format_pct_comparison(private_nfp_distress_stats$pct, public_distress_stats$pct),
      sprintf("Within %s All_2024 rows, compute the share with warning_score_core >= 4 separately for Private not-for-profit and Public control groups.", latest_year),
      "Sector contrast within the same primarily baccalaureate workbook universe."
    ),
    make_answer_row(
      "Active accreditation warning/notice overlap with workbook distress",
      sprintf(
        "%s with warning/notice vs %s without; %s with warning/notice also cut staff",
        format_value(xtab_value("Active warning/notice", "With event", "distress_share_pct"), digits = 1, suffix = "%"),
        format_value(xtab_value("Active warning/notice", "Without event", "distress_share_pct"), digits = 1, suffix = "%"),
        format_value(xtab_value("Active warning/notice", "With event", "staff_total_decline_5yr_pct"), digits = 1, suffix = "%")
      ),
      "Read AccredFinanceXtab for event_type == 'Active warning/notice': use the With event and Without event rows to compare distress share and staff_total_decline_5yr_pct.",
      "External-validation cut: schools with active warnings/notices are much more likely to be distressed."
    ),
    make_answer_row(
      sprintf("%s institutions with all three severe signals at once", latest_year),
      format_count_student_value(all3signals_stats$count, all3signals_stats$denominator, all3signals_stats$students),
      sprintf("Count %s rows in All3Signals where enrollment_decline_last_3_of_5 == Yes AND revenue_10pct_drop_last_3_of_5 == Yes AND losses_last_3_of_5 == Yes; students = sum(enrollment_headcount_total).", latest_year),
      "Concentrated-trouble subset with repeated enrollment declines, repeated deep revenue drops, and repeated losses."
    ),
    make_answer_row(
      sprintf("%s institutions with enrollment declines in at least 3 of the last 5 year-to-year comparisons", latest_year),
      format_count_student_value(enroll_decline_stats$count, enroll_decline_stats$denominator, enroll_decline_stats$students),
      sprintf("Count %s rows in EnrollDecl3of5 where enrollment_decline_last_3_of_5 == Yes; students = sum(enrollment_headcount_total).", latest_year),
      "This is the repeated year-over-year decline flag, not the one-time five-year drop threshold."
    ),
    make_answer_row(
      sprintf("%s institutions with a five-year enrollment drop of at least 10%%", latest_year),
      format_count_student_value(enroll_10pct_5yr_stats$count, enroll_10pct_5yr_stats$denominator, enroll_10pct_5yr_stats$students),
      sprintf("Count %s All_2024 rows where enrollment_pct_change_5yr <= -10; students = sum(enrollment_headcount_total).", latest_year),
      "This is the one-time five-year enrollment-drop threshold version."
    ),
    make_answer_row(
      sprintf("%s institutions with a revenue drop of at least 10%% in 3 of the last 5 years", latest_year),
      format_count_student_value(rev_10pct_3of5_stats$count, rev_10pct_3of5_stats$denominator, rev_10pct_3of5_stats$students),
      sprintf("Count %s RevDecl3of5 rows where revenue_10pct_drop_last_3_of_5 == Yes; students = sum(enrollment_headcount_total).", latest_year),
      "Revenue measure uses the existing repeated 10% year-over-year decline flag."
    ),
    make_answer_row(
      sprintf("%s institutions with both a five-year enrollment drop of at least 10%% and a revenue drop of at least 10%% in 3 of the last 5 years", latest_year),
      format_count_student_value(enroll10_rev10_stats$count, enroll10_rev10_stats$denominator, enroll10_rev10_stats$students),
      sprintf("Count %s All_2024 rows where enrollment_pct_change_5yr <= -10 AND revenue_10pct_drop_last_3_of_5 == Yes; students = sum(enrollment_headcount_total).", latest_year),
      "Combines the five-year enrollment-drop threshold with the repeated revenue-drop flag."
    ),
    make_answer_row(
      sprintf("%s institutions with a five-year enrollment drop of at least 10%% and losses in 3 of the last 5 years", latest_year),
      format_count_student_value(enroll10_loss_stats$count, enroll10_loss_stats$denominator, enroll10_loss_stats$students),
      sprintf("Count %s All_2024 rows where enrollment_pct_change_5yr <= -10 AND losses_last_3_of_5 == Yes; students = sum(enrollment_headcount_total).", latest_year),
      "Enrollment side uses the five-year 10% threshold; loss side uses the repeated-loss flag."
    ),
    make_answer_row(
      sprintf("%s institutions with a revenue drop of at least 10%% in 3 of the last 5 years and losses in 3 of the last 5 years", latest_year),
      format_count_student_value(rev10_loss_stats$count, rev10_loss_stats$denominator, rev10_loss_stats$students),
      sprintf("Count %s All_2024 rows where revenue_10pct_drop_last_3_of_5 == Yes AND losses_last_3_of_5 == Yes; students = sum(enrollment_headcount_total).", latest_year),
      "Combines the repeated 10% revenue-drop flag with the repeated-loss flag."
    ),
    make_answer_row(
      sprintf("%s institutions with a five-year enrollment drop of at least 10%%, a revenue drop of at least 10%% in 3 of the last 5 years, and losses in 3 of the last 5 years", latest_year),
      format_count_student_value(enroll10_rev10_loss_stats$count, enroll10_rev10_loss_stats$denominator, enroll10_rev10_loss_stats$students),
      sprintf("Count %s All_2024 rows where enrollment_pct_change_5yr <= -10 AND revenue_10pct_drop_last_3_of_5 == Yes AND losses_last_3_of_5 == Yes; students = sum(enrollment_headcount_total).", latest_year),
      "Most severe 10%-threshold combination in the answer sheet."
    ),
    make_answer_row(
      sprintf("%s institutions that cut total staff over the past 5 years", latest_year),
      format_count_student_value(staff_cut_5yr_stats$count, staff_cut_5yr_stats$denominator, staff_cut_5yr_stats$students),
      sprintf("Count %s rows in StaffDown5yr where staff_total_headcount_pct_change_5yr < 0; students = sum(enrollment_headcount_total).", latest_year),
      sprintf("Means %s staff_headcount_total is below %s, using total staff headcount rather than FTE.", latest_year, latest_year - 5L)
    ),
    make_answer_row(
      sprintf("%s institutions with losses in 3 of the last 5 years", latest_year),
      format_count_student_value(losses_3of5_stats$count, losses_3of5_stats$denominator, losses_3of5_stats$students),
      sprintf("Count %s rows in Red3of5 where losses_last_3_of_5 == Yes; students = sum(enrollment_headcount_total).", latest_year),
      "Repeated-loss flag based on at least three negative operating-margin years in the five-year window."
    ),
    make_answer_row(
      sprintf("%s private nonprofits with both enrollment declines and losses in 3 of the last 5 years", latest_year),
      format_count_student_value(private_nfp_enroll_loss_stats$count, private_nfp_enroll_loss_stats$denominator, private_nfp_enroll_loss_stats$students),
      sprintf("Within %s private not-for-profit rows in All_2024, count enrollment_decline_last_3_of_5 == Yes AND losses_last_3_of_5 == Yes; students = sum(enrollment_headcount_total).", latest_year),
      "Private-nonprofit subset of the repeated enrollment-losses group."
    ),
    make_answer_row(
      sprintf("%s private nonprofits with both repeated losses and a five-year enrollment drop of at least 10%%", latest_year),
      format_count_student_value(private_nfp_enroll10_loss_stats$count, private_nfp_enroll10_loss_stats$denominator, private_nfp_enroll10_loss_stats$students),
      sprintf("Within %s private not-for-profit rows in All_2024, count enrollment_pct_change_5yr <= -10 AND losses_last_3_of_5 == Yes; students = sum(enrollment_headcount_total).", latest_year),
      "Private-nonprofit 10%-threshold version of the enrollment-plus-losses framing."
    )
  )

  sector_rows <- lapply(names(scope_dfs), function(scope_name) {
    build_sector_breakdown_rows(scope_name, scope_dfs[[scope_name]], sector_metric_specs)
  })

  do.call(rbind, c(rows, sector_rows))
}

# Builds a single row for the grouped ReportAnswers tab.
make_report_answer_row <- function(group, year, scope, question,
                                   count_yes = NA_real_,
                                   count_scope_total = NA_real_,
                                   pct_of_scope_institutions = NA_real_,
                                   pct_of_all_institutions = NA_real_,
                                   pct_of_yes_institutions = NA_real_,
                                   students_at_yes_institutions = NA_real_,
                                   scope_total_students = NA_real_,
                                   pct_of_scope_students = NA_real_,
                                   pct_of_all_students = NA_real_,
                                   pct_of_yes_students = NA_real_,
                                   calculation = NA_character_,
                                   note = NA_character_) {
  data.frame(
    group = group,
    year = year,
    scope = scope,
    question = question,
    count_yes = count_yes,
    count_scope_total = count_scope_total,
    pct_of_scope_institutions = pct_of_scope_institutions,
    pct_of_all_institutions = pct_of_all_institutions,
    pct_of_yes_institutions = pct_of_yes_institutions,
    students_at_yes_institutions = students_at_yes_institutions,
    scope_total_students = scope_total_students,
    pct_of_scope_students = pct_of_scope_students,
    pct_of_all_students = pct_of_all_students,
    pct_of_yes_students = pct_of_yes_students,
    calculation = calculation,
    note = note,
    stringsAsFactors = FALSE
  )
}

# Counts how many year-over-year comparisons in a window meet a decline threshold.
count_window_declines <- function(years, values, start_year, end_year, threshold_pct = 0) {
  lookup <- stats::setNames(values, years)
  decline_count <- 0L
  for (yr in seq.int(start_year, end_year)) {
    current_value <- unname(lookup[as.character(yr + 1L)])
    prior_value <- unname(lookup[as.character(yr)])
    if (is.na(current_value) || is.na(prior_value) || prior_value == 0) {
      next
    }
    pct_change <- ((current_value - prior_value) / prior_value) * 100
    if (!is.na(pct_change) && pct_change <= threshold_pct) {
      decline_count <- decline_count + 1L
    }
  }
  decline_count
}

# Adds standardized repeated-decline and threshold flags used by ReportAnswers.
prepare_report_answer_year_df <- function(read_df, target_year, bacc_category_label) {
  year_df <- read_df[
    as.integer(read_df$year) == as.integer(target_year) &
      read_df$category == bacc_category_label,
    ,
    drop = FALSE
  ]
  if (nrow(year_df) == 0) {
    return(year_df)
  }

  revenue_history_field <- if ("revenue_total_adjusted" %in% names(read_df)) {
    "revenue_total_adjusted"
  } else {
    "revenue_total"
  }

  history_mask <- read_df$category == bacc_category_label &
    as.integer(read_df$year) >= (as.integer(target_year) - 5L) &
    as.integer(read_df$year) <= as.integer(target_year)
  history_df <- read_df[history_mask, c("unitid", "year", "enrollment_headcount_total", revenue_history_field), drop = FALSE]
  unit_histories <- split(history_df, history_df$unitid)

  derive_unit_flag <- function(unitid_value, value_field, threshold_pct) {
    history_df <- unit_histories[[as.character(unitid_value)]]
    if (is.null(history_df) || nrow(history_df) == 0) {
      return(FALSE)
    }
    history_df <- history_df[order(as.integer(history_df$year)), , drop = FALSE]
    count_window_declines(
      years = as.integer(history_df$year),
      values = to_num(history_df[[value_field]]),
      start_year = as.integer(target_year) - 5L,
      end_year = as.integer(target_year) - 1L,
      threshold_pct = threshold_pct
    ) >= 3L
  }

  year_df$repeated_enrollment_decline <- yes_flag(year_df$enrollment_decline_last_3_of_5)
  year_df$repeated_revenue_decline <- vapply(year_df$unitid, derive_unit_flag, logical(1), value_field = revenue_history_field, threshold_pct = 0)
  year_df$repeated_losses <- yes_flag(year_df$losses_last_3_of_5)
  year_df$repeated_enrollment_decline_10pct <- vapply(year_df$unitid, derive_unit_flag, logical(1), value_field = "enrollment_headcount_total", threshold_pct = -10)
  year_df$repeated_revenue_decline_10pct <- if ("revenue_10pct_drop_last_3_of_5" %in% names(year_df)) {
    yes_flag(year_df$revenue_10pct_drop_last_3_of_5)
  } else {
    vapply(year_df$unitid, derive_unit_flag, logical(1), value_field = revenue_history_field, threshold_pct = -10)
  }
  year_df$five_year_enrollment_decline_10pct <- !is.na(year_df$enrollment_pct_change_5yr) & to_num(year_df$enrollment_pct_change_5yr) <= -10
  year_df$five_year_revenue_decline_10pct <- !is.na(year_df$revenue_pct_change_5yr) & to_num(year_df$revenue_pct_change_5yr) <= -10
  year_df$staff_cut_5yr <- !is.na(year_df$staff_total_headcount_pct_change_5yr) & to_num(year_df$staff_total_headcount_pct_change_5yr) < 0
  year_df$staff_cut_10pct_5yr <- !is.na(year_df$staff_total_headcount_pct_change_5yr) & to_num(year_df$staff_total_headcount_pct_change_5yr) <= -10
  year_df$net_tuition_down_5yr <- !is.na(year_df$net_tuition_per_fte_change_5yr) & to_num(year_df$net_tuition_per_fte_change_5yr) < 0
  year_df$net_tuition_down_10pct_5yr <- !is.na(year_df$net_tuition_per_fte_change_5yr) & to_num(year_df$net_tuition_per_fte_change_5yr) <= -10
  year_df$discount_rate_up_5yr <- !is.na(year_df$discount_pct_change_5yr) & to_num(year_df$discount_pct_change_5yr) > 0
  year_df$discount_rate_up_10pct_5yr <- !is.na(year_df$discount_pct_change_5yr) & to_num(year_df$discount_pct_change_5yr) >= 10

  year_df
}

# Returns the grouped metric specification list for the ReportAnswers tab.
build_report_metric_specs <- function(year_value) {
  start_year <- as.integer(year_value) - 5L
  repeated_group <- sprintf("%s Repeated-decline flags", year_value)
  repeated_10_group <- sprintf("%s Repeated >=10%% decline flags", year_value)
  threshold_group <- sprintf("%s Five-year threshold flags", year_value)
  staffing_group <- sprintf("%s Staffing / Tuition / Discount", year_value)

  c(
    list(
      list(group = repeated_group, question_suffix = "with enrollment declines in at least 3 of the last 5 year-to-year comparisons", calculation = "repeated_enrollment_decline == TRUE", note = "Repeated year-over-year enrollment-decline flag.", predicate = function(df) df$repeated_enrollment_decline),
      list(group = repeated_group, question_suffix = "with revenue declines in at least 3 of the last 5 year-to-year comparisons", calculation = "repeated_revenue_decline == TRUE", note = "Repeated year-over-year revenue-decline flag.", predicate = function(df) df$repeated_revenue_decline),
      list(group = repeated_group, question_suffix = "with losses in 3 of the last 5 years", calculation = "repeated_losses == TRUE", note = "Repeated-loss flag.", predicate = function(df) df$repeated_losses),
      list(group = repeated_group, question_suffix = "with both enrollment declines in at least 3 of the last 5 year-to-year comparisons and losses in 3 of the last 5 years", calculation = "repeated_enrollment_decline == TRUE AND repeated_losses == TRUE", note = "Repeated enrollment-decline flag plus repeated-loss flag.", predicate = function(df) df$repeated_enrollment_decline & df$repeated_losses),
      list(group = repeated_group, question_suffix = "with both enrollment declines in at least 3 of the last 5 year-to-year comparisons and revenue declines in at least 3 of the last 5 year-to-year comparisons", calculation = "repeated_enrollment_decline == TRUE AND repeated_revenue_decline == TRUE", note = "Repeated enrollment-decline flag plus repeated revenue-decline flag.", predicate = function(df) df$repeated_enrollment_decline & df$repeated_revenue_decline),
      list(group = repeated_group, question_suffix = "with both revenue declines in at least 3 of the last 5 year-to-year comparisons and losses in 3 of the last 5 years", calculation = "repeated_revenue_decline == TRUE AND repeated_losses == TRUE", note = "Repeated revenue-decline flag plus repeated-loss flag.", predicate = function(df) df$repeated_revenue_decline & df$repeated_losses),
      list(group = repeated_group, question_suffix = "with enrollment declines in at least 3 of the last 5 year-to-year comparisons, revenue declines in at least 3 of the last 5 year-to-year comparisons, and losses in 3 of the last 5 years", calculation = "repeated_enrollment_decline == TRUE AND repeated_revenue_decline == TRUE AND repeated_losses == TRUE", note = "Three repeated-decline flags together.", predicate = function(df) df$repeated_enrollment_decline & df$repeated_revenue_decline & df$repeated_losses),
      list(group = repeated_group, question_suffix = "with enrollment declines in at least 3 of the last 5 year-to-year comparisons, a revenue decline of at least 10% in at least 3 of the last 5 year-to-year comparisons, and losses in 3 of the last 5 years", calculation = "repeated_enrollment_decline == TRUE AND repeated_revenue_decline_10pct == TRUE AND repeated_losses == TRUE", note = "Repeated enrollment-decline flag, repeated >=10% revenue-decline flag, and repeated losses.", predicate = function(df) df$repeated_enrollment_decline & df$repeated_revenue_decline_10pct & df$repeated_losses)
    ),
    list(
      list(group = repeated_10_group, question_suffix = "with enrollment declines of at least 10% in at least 3 of the last 5 year-to-year comparisons", calculation = "repeated_enrollment_decline_10pct == TRUE", note = "Repeated >=10% year-over-year enrollment-decline flag.", predicate = function(df) df$repeated_enrollment_decline_10pct),
      list(group = repeated_10_group, question_suffix = "with revenue declines of at least 10% in at least 3 of the last 5 year-to-year comparisons", calculation = "repeated_revenue_decline_10pct == TRUE", note = "Repeated >=10% year-over-year revenue-decline flag.", predicate = function(df) df$repeated_revenue_decline_10pct),
      list(group = repeated_10_group, question_suffix = "with both enrollment declines of at least 10% in at least 3 of the last 5 year-to-year comparisons and revenue declines of at least 10% in at least 3 of the last 5 year-to-year comparisons", calculation = "repeated_enrollment_decline_10pct == TRUE AND repeated_revenue_decline_10pct == TRUE", note = "Repeated >=10% enrollment- and revenue-decline flags together.", predicate = function(df) df$repeated_enrollment_decline_10pct & df$repeated_revenue_decline_10pct),
      list(group = repeated_10_group, question_suffix = "with both enrollment declines of at least 10% in at least 3 of the last 5 year-to-year comparisons and losses in 3 of the last 5 years", calculation = "repeated_enrollment_decline_10pct == TRUE AND repeated_losses == TRUE", note = "Repeated >=10% enrollment-decline flag plus repeated losses.", predicate = function(df) df$repeated_enrollment_decline_10pct & df$repeated_losses),
      list(group = repeated_10_group, question_suffix = "with both revenue declines of at least 10% in at least 3 of the last 5 year-to-year comparisons and losses in 3 of the last 5 years", calculation = "repeated_revenue_decline_10pct == TRUE AND repeated_losses == TRUE", note = "Repeated >=10% revenue-decline flag plus repeated losses.", predicate = function(df) df$repeated_revenue_decline_10pct & df$repeated_losses),
      list(group = repeated_10_group, question_suffix = "with enrollment declines of at least 10% in at least 3 of the last 5 year-to-year comparisons, revenue declines of at least 10% in at least 3 of the last 5 year-to-year comparisons, and losses in 3 of the last 5 years", calculation = "repeated_enrollment_decline_10pct == TRUE AND repeated_revenue_decline_10pct == TRUE AND repeated_losses == TRUE", note = "Repeated >=10% enrollment and revenue declines plus repeated losses.", predicate = function(df) df$repeated_enrollment_decline_10pct & df$repeated_revenue_decline_10pct & df$repeated_losses)
    ),
    list(
      list(group = threshold_group, question_suffix = sprintf("with an enrollment decline of at least 10%% from %s to %s", start_year, year_value), calculation = "five_year_enrollment_decline_10pct == TRUE", note = "Five-year enrollment-threshold flag.", predicate = function(df) df$five_year_enrollment_decline_10pct),
      list(group = threshold_group, question_suffix = sprintf("with a revenue decline of at least 10%% from %s to %s", start_year, year_value), calculation = "five_year_revenue_decline_10pct == TRUE", note = "Five-year revenue-threshold flag.", predicate = function(df) df$five_year_revenue_decline_10pct),
      list(group = threshold_group, question_suffix = sprintf("with both enrollment and revenue declines of at least 10%% from %s to %s", start_year, year_value), calculation = "five_year_enrollment_decline_10pct == TRUE AND five_year_revenue_decline_10pct == TRUE", note = "Five-year enrollment and revenue thresholds together.", predicate = function(df) df$five_year_enrollment_decline_10pct & df$five_year_revenue_decline_10pct),
      list(group = threshold_group, question_suffix = sprintf("with both an enrollment decline of at least 10%% from %s to %s and losses in 3 of the last 5 years", start_year, year_value), calculation = "five_year_enrollment_decline_10pct == TRUE AND repeated_losses == TRUE", note = "Five-year enrollment-threshold flag plus repeated losses.", predicate = function(df) df$five_year_enrollment_decline_10pct & df$repeated_losses),
      list(group = threshold_group, question_suffix = sprintf("with both a revenue decline of at least 10%% from %s to %s and losses in 3 of the last 5 years", start_year, year_value), calculation = "five_year_revenue_decline_10pct == TRUE AND repeated_losses == TRUE", note = "Five-year revenue-threshold flag plus repeated losses.", predicate = function(df) df$five_year_revenue_decline_10pct & df$repeated_losses),
      list(group = threshold_group, question_suffix = sprintf("with a five-year enrollment decline of at least 10%%, a five-year revenue decline of at least 10%%, and losses in 3 of the last 5 years using the %s to %s window", start_year, year_value), calculation = "five_year_enrollment_decline_10pct == TRUE AND five_year_revenue_decline_10pct == TRUE AND repeated_losses == TRUE", note = "Five-year enrollment and revenue thresholds plus repeated losses.", predicate = function(df) df$five_year_enrollment_decline_10pct & df$five_year_revenue_decline_10pct & df$repeated_losses)
    ),
    list(
      list(group = staffing_group, question_suffix = "that cut total staff over the past 5 years", calculation = "staff_cut_5yr == TRUE", note = "Total staff headcount fell over the five-year window.", predicate = function(df) df$staff_cut_5yr),
      list(group = staffing_group, question_suffix = "that cut total staff by at least 10% over the past 5 years", calculation = "staff_cut_10pct_5yr == TRUE", note = "Total staff headcount fell by at least 10% over the five-year window.", predicate = function(df) df$staff_cut_10pct_5yr),
      list(group = staffing_group, question_suffix = "with falling net tuition per FTE over the past 5 years", calculation = "net_tuition_down_5yr == TRUE", note = "Net tuition revenue per FTE fell over five years.", predicate = function(df) df$net_tuition_down_5yr),
      list(group = staffing_group, question_suffix = "where net tuition per FTE has fallen by at least 10% over the past 5 years", calculation = "net_tuition_down_10pct_5yr == TRUE", note = "Net tuition revenue per FTE fell by at least 10% over five years.", predicate = function(df) df$net_tuition_down_10pct_5yr),
      list(group = staffing_group, question_suffix = "with rising discount rates over the past 5 years", calculation = "discount_rate_up_5yr == TRUE", note = "Discount rate increased over five years.", predicate = function(df) df$discount_rate_up_5yr),
      list(group = staffing_group, question_suffix = "where discount rates have risen by at least 10% over the past 5 years", calculation = "discount_rate_up_10pct_5yr == TRUE", note = "Discount rate increased by at least 10% over five years.", predicate = function(df) df$discount_rate_up_10pct_5yr),
      list(group = staffing_group, question_suffix = "with rising discount rates and falling net tuition per FTE", calculation = "discount_rate_up_5yr == TRUE AND net_tuition_down_5yr == TRUE", note = "Discount rates rose while net tuition per FTE fell.", predicate = function(df) df$discount_rate_up_5yr & df$net_tuition_down_5yr),
      list(group = staffing_group, question_suffix = "where discount rates have risen by at least 10% over the past 5 years and net tuition per FTE has fallen by at least 10% over the past 5 years", calculation = "discount_rate_up_10pct_5yr == TRUE AND net_tuition_down_10pct_5yr == TRUE", note = "Discount rates rose by at least 10% while net tuition per FTE fell by at least 10%.", predicate = function(df) df$discount_rate_up_10pct_5yr & df$net_tuition_down_10pct_5yr)
    )
  )
}

# Builds grouped rows for one year of ReportAnswers metrics.
build_report_rows_for_year <- function(read_df, target_year, bacc_category_label) {
  year_df <- prepare_report_answer_year_df(read_df, target_year, bacc_category_label)
  if (nrow(year_df) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  scope_specs <- list(
    list(label = "All institutions", question_prefix = "institutions", filter = function(df) rep(TRUE, nrow(df))),
    list(label = "Public institutions", question_prefix = "Public institutions", filter = function(df) df$control_label == "Public"),
    list(label = "Private not-for-profit institutions", question_prefix = "Private not-for-profit institutions", filter = function(df) df$control_label == "Private not-for-profit"),
    list(label = "Private for-profit institutions", question_prefix = "Private for-profit institutions", filter = function(df) df$control_label == "Private for-profit")
  )

  total_all_institutions <- nrow(year_df)
  total_all_students <- sum(to_num(year_df$enrollment_headcount_total), na.rm = TRUE)
  metrics <- build_report_metric_specs(target_year)

  rows <- lapply(metrics, function(metric_spec) {
    all_matches <- metric_spec$predicate(year_df)
    all_matches[is.na(all_matches)] <- FALSE
    all_yes_count <- sum(all_matches)
    all_yes_students <- sum(to_num(year_df$enrollment_headcount_total[all_matches]), na.rm = TRUE)

    do.call(rbind, lapply(scope_specs, function(scope_spec) {
      scope_df <- year_df[scope_spec$filter(year_df), , drop = FALSE]
      scope_matches <- metric_spec$predicate(scope_df)
      scope_matches[is.na(scope_matches)] <- FALSE
      count_yes <- sum(scope_matches)
      count_scope_total <- nrow(scope_df)
      students_yes <- sum(to_num(scope_df$enrollment_headcount_total[scope_matches]), na.rm = TRUE)
      scope_total_students <- sum(to_num(scope_df$enrollment_headcount_total), na.rm = TRUE)
      question <- if (scope_spec$label == "All institutions") {
        sprintf("%s institutions %s", target_year, metric_spec$question_suffix)
      } else {
        sprintf("%s %s %s", target_year, scope_spec$question_prefix, metric_spec$question_suffix)
      }

      make_report_answer_row(
        group = metric_spec$group,
        year = as.integer(target_year),
        scope = scope_spec$label,
        question = question,
        count_yes = count_yes,
        count_scope_total = count_scope_total,
        pct_of_scope_institutions = safe_pct(count_yes, count_scope_total),
        pct_of_all_institutions = safe_pct(count_yes, total_all_institutions),
        pct_of_yes_institutions = safe_pct(count_yes, all_yes_count),
        students_at_yes_institutions = students_yes,
        scope_total_students = scope_total_students,
        pct_of_scope_students = safe_pct(students_yes, scope_total_students),
        pct_of_all_students = safe_pct(students_yes, total_all_students),
        pct_of_yes_students = safe_pct(students_yes, all_yes_students),
        calculation = metric_spec$calculation,
        note = metric_spec$note
      )
    }))
  })

  do.call(rbind, rows)
}

# Builds the grouped ReportAnswers tab with denominator and student-breakout columns.
build_report_answers <- function(read_df, distress_compare,
                                 bacc_category_label = "Degree-granting, primarily baccalaureate or above",
                                 latest_year = 2024L, comparison_year = latest_year - 5L,
                                 baseline_year = latest_year - 10L) {
  comparison_note <- NA_character_
  if (nrow(distress_compare) > 0 && "comparison_note" %in% names(distress_compare)) {
    comparison_note_value <- distress_compare$comparison_note[distress_compare$year == baseline_year]
    if (length(comparison_note_value) > 0) {
      comparison_note <- comparison_note_value[[1]]
    }
  }

  setup_rows <- append_rows(
    make_report_answer_row(
      group = "Setup",
      year = as.integer(baseline_year),
      scope = "All institutions",
      question = sprintf("%s comparison note", baseline_year),
      calculation = "Pulled from DistressCompare$comparison_note for the baseline year.",
      note = comparison_note
    ),
    make_report_answer_row(
      group = "Setup",
      year = as.integer(latest_year),
      scope = "All institutions",
      question = "How the workbook defines rising discount rates and falling net tuition per FTE",
      calculation = "discount_pct_change_5yr > 0 AND net_tuition_per_fte_change_5yr < 0",
      note = "Discount rates rose over the five-year window while net tuition revenue per FTE fell over the same five-year window."
    )
  )

  append_rows(
    setup_rows,
    build_report_rows_for_year(read_df, latest_year, bacc_category_label),
    build_report_rows_for_year(read_df, comparison_year, bacc_category_label)
  )
}

# Builds the DistressAnswers tab with workbook-distress definitions and toplines.
build_distress_answers <- function(read_df, distress_compare, distress_intl10, accredit_finance_xtab,
                                   bacc_category_label = "Degree-granting, primarily baccalaureate or above",
                                   latest_year = 2024L, comparison_year = latest_year - 5L,
                                   baseline_year = latest_year - 10L) {
  value_for_year <- function(field, year_value) {
    value <- distress_compare[[field]][distress_compare$year == year_value]
    if (length(value) == 0) NA else value[[1]]
  }
  format_value <- function(x, digits = NULL, suffix = "") {
    if (length(x) == 0 || is.null(x) || all(is.na(x))) return(NA_character_)
    val <- x[[1]]
    if (is.numeric(val)) {
      if (!is.null(digits)) {
        val <- formatC(val, format = "f", digits = digits, big.mark = ",")
      } else {
        val <- format(round(val), big.mark = ",", scientific = FALSE, trim = TRUE)
      }
    }
    paste0(as.character(val), suffix)
  }
  make_answer_row <- function(question, value, calculation, note) {
    data.frame(
      question = question,
      value = if (length(value) == 0 || is.null(value) || all(is.na(value))) NA_character_ else as.character(value[[1]]),
      calculation = calculation,
      note = note,
      stringsAsFactors = FALSE
    )
  }
  distress_share_for_control <- function(year_value, control_value) {
    year_df <- read_df[
      as.integer(read_df$year) == as.integer(year_value) &
        read_df$category == bacc_category_label &
        read_df$control_label == control_value,
      ,
      drop = FALSE
    ]
    if (nrow(year_df) == 0) {
      return(NA_real_)
    }
    score <- compute_warning_score_core(year_df)
    safe_pct(sum(score >= 4, na.rm = TRUE), nrow(year_df))
  }
  xtab_value <- function(event_type, cohort, field) {
    if (nrow(accredit_finance_xtab) == 0 || !(field %in% names(accredit_finance_xtab))) return(NA)
    value <- accredit_finance_xtab[[field]][
      accredit_finance_xtab$event_type == event_type &
        accredit_finance_xtab$control_scope == "All" &
        accredit_finance_xtab$cohort == cohort
    ]
    if (length(value) == 0) NA else value[[1]]
  }

  append_rows(
    make_answer_row(
      "How the workbook defines distressed institutions",
      "warning_score_core >= 4",
      "warning_score_core counts six core warning signals and flags institutions at 4 or more.",
      "Signals are repeated enrollment decline, repeated 10% revenue decline, repeated losses, ending the latest year in the red, staff cuts over five years, and falling net tuition per FTE over five years."
    ),
    make_answer_row(
      sprintf("Core distress trend from %s to %s", comparison_year, latest_year),
      sprintf("%s (%s) vs %s (%s)", format_value(value_for_year("distress_count", latest_year)), format_value(value_for_year("distress_pct", latest_year), digits = 1, suffix = "%"), format_value(value_for_year("distress_count", comparison_year)), format_value(value_for_year("distress_pct", comparison_year), digits = 1, suffix = "%")),
      sprintf("Compare DistressCompare distress_count and distress_pct for %s and %s.", latest_year, comparison_year),
      "Uses the same primarily baccalaureate workbook universe and warning-score definition in both years."
    ),
    make_answer_row(
      sprintf("%s distress share for private nonprofits versus publics", latest_year),
      sprintf("%s vs %s", format_value(distress_share_for_control(latest_year, "Private not-for-profit"), digits = 1, suffix = "%"), format_value(distress_share_for_control(latest_year, "Public"), digits = 1, suffix = "%")),
      sprintf("Within %s primarily baccalaureate rows, compare the share with warning_score_core >= 4 for Private not-for-profit versus Public institutions.", latest_year),
      "Shows sector contrast within the same year."
    ),
    make_answer_row(
      sprintf("%s distressed institutions in the primarily baccalaureate universe", latest_year),
      format_value(value_for_year("distress_count", latest_year)),
      sprintf("Count %s primarily baccalaureate institutions where warning_score_core >= 4.", latest_year),
      "Topline distressed-institution count."
    ),
    make_answer_row(
      sprintf("%s distressed institutions as a share of the primarily baccalaureate universe", latest_year),
      format_value(value_for_year("distress_pct", latest_year), digits = 1, suffix = "%"),
      sprintf("distress_count / institutions_total for %s in DistressCompare.", latest_year),
      "Topline distressed-institution share."
    ),
    make_answer_row(
      sprintf("%s students enrolled at distressed institutions", latest_year),
      format_value(value_for_year("distress_students", latest_year)),
      sprintf("Sum enrollment_headcount_total for %s primarily baccalaureate institutions where warning_score_core >= 4.", latest_year),
      "Student exposure to the distressed-institution universe."
    ),
    make_answer_row(
      sprintf("%s colleges in distress with rising international enrollment over 10 years", latest_year),
      format_value(nrow(distress_intl10)),
      sprintf("Count rows in DistressIntl10 for %s.", latest_year),
      "Subset of distressed institutions that also posted 10-year international-enrollment growth."
    ),
    make_answer_row(
      "Active accreditation warning/notice overlap with workbook distress",
      sprintf("%s with warning/notice vs %s without; %s with warning/notice also cut staff", format_value(xtab_value("Active warning/notice", "With event", "distress_share_pct"), digits = 1, suffix = "%"), format_value(xtab_value("Active warning/notice", "Without event", "distress_share_pct"), digits = 1, suffix = "%"), format_value(xtab_value("Active warning/notice", "With event", "staff_total_decline_5yr_pct"), digits = 1, suffix = "%")),
      "Uses AccredFinanceXtab percentages for the Active warning/notice event cohort.",
      "External-warning overlap with the workbook distress definition."
    ),
    make_answer_row(
      sprintf("Core distress trend from %s to %s", baseline_year, comparison_year),
      sprintf("%s (%s) vs %s (%s)", format_value(value_for_year("distress_count", comparison_year)), format_value(value_for_year("distress_pct", comparison_year), digits = 1, suffix = "%"), format_value(value_for_year("distress_count", baseline_year)), format_value(value_for_year("distress_pct", baseline_year), digits = 1, suffix = "%")),
      sprintf("Compare DistressCompare distress_count and distress_pct for %s and %s.", comparison_year, baseline_year),
      "Use with the baseline-year comparison note if the five-year trend fields are not directly comparable."
    ),
    make_answer_row(
      sprintf("%s distress share for private nonprofits versus publics", comparison_year),
      sprintf("%s vs %s", format_value(distress_share_for_control(comparison_year, "Private not-for-profit"), digits = 1, suffix = "%"), format_value(distress_share_for_control(comparison_year, "Public"), digits = 1, suffix = "%")),
      sprintf("Within %s primarily baccalaureate rows, compare the share with warning_score_core >= 4 for Private not-for-profit versus Public institutions.", comparison_year),
      "Same sector contrast for the earlier comparison year."
    ),
    make_answer_row(
      sprintf("%s distressed institutions in the primarily baccalaureate universe", comparison_year),
      format_value(value_for_year("distress_count", comparison_year)),
      sprintf("Count %s primarily baccalaureate institutions where warning_score_core >= 4.", comparison_year),
      "Earlier-year distressed-institution count."
    ),
    make_answer_row(
      sprintf("%s distressed institutions as a share of the primarily baccalaureate universe", comparison_year),
      format_value(value_for_year("distress_pct", comparison_year), digits = 1, suffix = "%"),
      sprintf("distress_count / institutions_total for %s in DistressCompare.", comparison_year),
      "Earlier-year distressed-institution share."
    ),
    make_answer_row(
      "How the workbook defines long-running challenge institutions",
      "enrollment_decline_last_3_of_5 == Yes AND losses_last_3_of_5 == Yes",
      "Counts institutions with both repeated enrollment declines and repeated losses in the rolling five-year window.",
      "This is a narrower definition than the broader distressed-institution warning-score screen."
    ),
    make_answer_row(
      sprintf("%s long-running challenge institutions with enrollment dips and losses in 3 of the last 5 years", latest_year),
      format_value(value_for_year("longrun_count", latest_year)),
      sprintf("Count %s primarily baccalaureate institutions where enrollment_decline_last_3_of_5 == Yes AND losses_last_3_of_5 == Yes.", latest_year),
      "Latest-year long-running challenge count."
    ),
    make_answer_row(
      sprintf("%s long-running challenge institutions with enrollment dips and losses in 3 of the last 5 years", comparison_year),
      format_value(value_for_year("longrun_count", comparison_year)),
      sprintf("Count %s primarily baccalaureate institutions where enrollment_decline_last_3_of_5 == Yes AND losses_last_3_of_5 == Yes.", comparison_year),
      "Earlier-year long-running challenge count."
    )
  )
}

# Builds the ResearchCutsAnswers tab with toplines for the flagship cuts match.
build_research_cuts_answers <- function(flagship_cuts) {
  make_answer_row <- function(question, value, calculation, note) {
    data.frame(
      question = question,
      value = as.character(value),
      calculation = calculation,
      note = note,
      stringsAsFactors = FALSE
    )
  }

  append_rows(
    make_answer_row(
      "Public flagships with still-disrupted federal research cuts",
      nrow(flagship_cuts),
      "Count rows in FlagshipCuts.",
      "Flagships matched to Grant Witness cuts that still show unreleased or disrupted awards."
    ),
    make_answer_row(
      "Public flagships with at least $1M still disrupted in federal research cuts",
      sum(!is.na(flagship_cuts$total_disrupted_award_remaining) & flagship_cuts$total_disrupted_award_remaining >= 1000000, na.rm = TRUE),
      "Count FlagshipCuts rows where total_disrupted_award_remaining >= 1000000.",
      "Large remaining-disruption subset."
    )
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
    state_support_change <- state_support_change_values(state_df)
    state_support_share <- state_support_share_values(state_df)
    state_df <- state_df[!is.na(state_support_change), , drop = FALSE]
    if (nrow(state_df) == 0) return(NULL)
    state_support_change <- state_support_change_values(state_df)
    state_support_share <- state_support_share_values(state_df)
    down_n <- sum(state_support_change < 0, na.rm = TRUE)
    up_n <- sum(state_support_change > 0, na.rm = TRUE)
    data.frame(
      state = state_df$state[1],
      public_institutions_with_state_change = nrow(state_df),
      state_support_down_5yr_count = down_n,
      state_support_down_5yr_percent = safe_pct(down_n, nrow(state_df)),
      state_support_up_5yr_count = up_n,
      state_support_up_5yr_percent = safe_pct(up_n, nrow(state_df)),
      mean_state_support_pct_change_5yr = mean(state_support_change, na.rm = TRUE),
      median_state_support_pct_change_5yr = median(state_support_change, na.rm = TRUE),
      mean_state_support_pct_core_revenue = mean(state_support_share, na.rm = TRUE) * 100,
      median_state_support_pct_core_revenue = median(state_support_share, na.rm = TRUE) * 100,
      biggest_state_support_drop_pct_5yr = min(state_support_change, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))

  if (is.null(out) || nrow(out) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  out[order(out$state_support_down_5yr_percent, out$mean_state_support_pct_change_5yr, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
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

# Builds a minimal historical IPEDS dataset from the per-year cache files so
# article trend tables can use year-specific target cohorts rather than the
# current 2024 survivor-only universe.
build_article_trend_dataset <- function(year_cache_dir, years = 2014:2024) {
  cache_paths <- file.path(year_cache_dir, sprintf("ipeds_financial_health_year_%s.csv", years))
  cache_paths <- cache_paths[file.exists(cache_paths)]
  if (!length(cache_paths)) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  raw_rows <- do.call(rbind, lapply(cache_paths, read_csv_if_exists))
  if (is.null(raw_rows) || !nrow(raw_rows)) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  state_codes <- toupper(trimws(as.character(raw_rows$state)))
  control_codes <- trimws(as.character(raw_rows$control))
  level_codes <- trimws(as.character(raw_rows$level))
  sector_codes <- trimws(as.character(raw_rows$sector))
  status_codes <- trimws(as.character(raw_rows$status))
  active_codes <- trimws(as.character(raw_rows$is_active))

  keep <- !(state_codes %in% excluded_state_codes) &
    control_codes %in% c("1", "2", "3") &
    level_codes == "1" &
    sector_codes %in% c("1", "2", "3") &
    status_codes %in% c("A", "N", "R") &
    active_codes == "1"
  keep[is.na(keep)] <- FALSE
  raw_rows <- raw_rows[keep, , drop = FALSE]
  if (!nrow(raw_rows)) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  prepared_rows <- lapply(seq_len(nrow(raw_rows)), function(i) {
    row <- raw_rows[i, , drop = FALSE]
    control_label <- get_control_label(row_value(row, "control"))
    reporting_model_code <- trimws(as.character(row_value(row, "reporting_model") %||% ""))
    uses_fasb_finance <- identical(control_label, "Private not-for-profit") || identical(reporting_model_code, "2")
    uses_gasb_finance <- identical(control_label, "Public") && !uses_fasb_finance
    context <- list(
      control_label = control_label,
      uses_fasb_finance = uses_fasb_finance,
      uses_gasb_finance = uses_gasb_finance,
      year = as.integer(row_value(row, "year")),
      fte12 = to_num(row_value(row, "fte_12_months"))
    )

    finance <- calculate_ipeds_finance_components(row, context)
    enrollment_fields <- build_enrollment_fields(row, context)
    finance_fields <- build_finance_fields(row, context, finance)
    risk_fields <- build_risk_fields(context, finance)

    data.frame(
      unitid = as.character(row_value(row, "unitid")),
      institution_name = as.character(row_value(row, "institution_name")),
      year = context$year,
      control_label = control_label,
      category_raw = as.character(row_value(row, "category")),
      all_programs_distance_education_raw = as.character(row_value(row, "all_programs_distance_education")),
      enrollment_headcount_total = enrollment_fields$enrollment_headcount_total,
      enrollment_nonresident_total = enrollment_fields$enrollment_nonresident_total,
      staff_headcount_total = enrollment_fields$staff_headcount_total,
      staff_headcount_instructional = enrollment_fields$staff_headcount_instructional,
      revenue_total = finance_fields$revenue_total,
      revenue_total_adjusted = finance_fields$revenue_total_adjusted,
      net_tuition_total = finance_fields$net_tuition_total,
      net_tuition_total_adjusted = finance_fields$net_tuition_total_adjusted,
      net_tuition_per_fte = finance_fields$net_tuition_per_fte,
      net_tuition_per_fte_adjusted = finance_fields$net_tuition_per_fte_adjusted,
      operating_margin = risk_fields$operating_margin,
      ended_year_at_loss = risk_fields$ended_year_at_loss,
      stringsAsFactors = FALSE
    )
  })

  trend_df <- do.call(rbind, prepared_rows)
  if (is.null(trend_df) || !nrow(trend_df)) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  enrich_article_trend_group <- function(df) {
    if (!nrow(df)) {
      return(df)
    }

    df <- df[order(as.integer(df$year)), , drop = FALSE]
    years_vec <- as.integer(df$year)
    lookup_year_value <- function(values, target_year) {
      match_idx <- which(years_vec == as.integer(target_year))
      if (!length(match_idx)) {
        return(NA_real_)
      }
      to_num(values[[match_idx[[1]]]])
    }

    df$enrollment_pct_change_5yr <- vapply(seq_len(nrow(df)), function(i) {
      safe_pct_change(
        to_num(df$enrollment_headcount_total[[i]]),
        lookup_year_value(df$enrollment_headcount_total, years_vec[[i]] - 5L)
      )
    }, numeric(1))
    df$enrollment_decline_last_3_of_5 <- ifelse(
      vapply(years_vec, function(y) {
        count_decline_years(years_vec, df$enrollment_headcount_total, y - 5L, y - 1L, threshold_pct = 0) >= 3L
      }, logical(1)),
      "Yes",
      "No"
    )
    df$staff_total_headcount_pct_change_5yr <- vapply(seq_len(nrow(df)), function(i) {
      safe_pct_change(
        to_num(df$staff_headcount_total[[i]]),
        lookup_year_value(df$staff_headcount_total, years_vec[[i]] - 5L)
      )
    }, numeric(1))
    df$staff_instructional_headcount_pct_change_5yr <- vapply(seq_len(nrow(df)), function(i) {
      safe_pct_change(
        to_num(df$staff_headcount_instructional[[i]]),
        lookup_year_value(df$staff_headcount_instructional, years_vec[[i]] - 5L)
      )
    }, numeric(1))
    df$revenue_10pct_drop_last_3_of_5 <- ifelse(
      vapply(years_vec, function(y) {
        count_decline_years(years_vec, df$revenue_total_adjusted, y - 5L, y - 1L, threshold_pct = -10) >= 3L
      }, logical(1)),
      "Yes",
      "No"
    )
    df$losses_last_3_of_5 <- ifelse(
      vapply(years_vec, function(y) {
        count_negative_years(years_vec, df$operating_margin, (y - 4L):y, threshold = 0) >= 3L
      }, logical(1)),
      "Yes",
      "No"
    )
    df$net_tuition_per_fte_change_5yr <- vapply(seq_len(nrow(df)), function(i) {
      safe_pct_change(
        to_num(df$net_tuition_per_fte_adjusted[[i]]),
        lookup_year_value(df$net_tuition_per_fte_adjusted, years_vec[[i]] - 5L)
      )
    }, numeric(1))
    df$international_enrollment_pct_change_10yr <- vapply(seq_len(nrow(df)), function(i) {
      safe_pct_change(
        to_num(df$enrollment_nonresident_total[[i]]),
        lookup_year_value(df$enrollment_nonresident_total, years_vec[[i]] - 10L)
      )
    }, numeric(1))
    df$warning_score_core <- compute_warning_score_core(df)

    df
  }

  do.call(rbind, lapply(split(trend_df, trend_df$unitid), enrich_article_trend_group))
}

# Returns target-year rows using a year-specific primarily baccalaureate cohort
# so earlier-year comparisons can include schools that later closed or changed
# category.
article_target_year_cohort_df <- function(article_trend_df, target_year) {
  if (is.null(article_trend_df) || !nrow(article_trend_df)) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  year_mask <- as.integer(article_trend_df$year) == as.integer(target_year)
  category_codes <- trimws(as.character(article_trend_df$category_raw))
  distance_codes <- trimws(as.character(article_trend_df$all_programs_distance_education_raw))
  eligible_unitids <- unique(article_trend_df$unitid[
    year_mask &
      category_codes == "2" &
      (is.na(distance_codes) | distance_codes != "1")
  ])

  article_trend_df[
    year_mask & article_trend_df$unitid %in% eligible_unitids,
    ,
    drop = FALSE
  ]
}

# Returns a closure-sensitive start-year staff cohort with total,
# instructional, and non-instructional staff cut fields on a shared
# denominator. When `treat_missing_end_as_zero` is TRUE, schools that later
# disappear are counted as having zero staff in the end year.
article_staff_cohort_df <- function(article_trend_df, start_year, end_year,
                                    treat_missing_end_as_zero = FALSE) {
  start_df <- article_target_year_cohort_df(article_trend_df, start_year)[
    ,
    c("unitid", "control_label", "staff_headcount_total", "staff_headcount_instructional"),
    drop = FALSE
  ]
  if (!nrow(start_df)) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  names(start_df)[3:4] <- c("staff_total_start", "staff_instructional_start")

  end_df <- article_trend_df[
    as.integer(article_trend_df$year) == as.integer(end_year),
    c("unitid", "staff_headcount_total", "staff_headcount_instructional"),
    drop = FALSE
  ]
  if (nrow(end_df)) {
    names(end_df)[2:3] <- c("staff_total_end", "staff_instructional_end")
    end_df$has_end_row <- TRUE
  } else {
    end_df <- data.frame(
      unitid = character(),
      staff_total_end = numeric(),
      staff_instructional_end = numeric(),
      has_end_row = logical(),
      stringsAsFactors = FALSE
    )
  }

  joined <- merge(start_df, end_df, by = "unitid", all.x = TRUE)
  if (treat_missing_end_as_zero) {
    joined$staff_total_end[is.na(joined$has_end_row)] <- 0
    joined$staff_instructional_end[is.na(joined$has_end_row)] <- 0
  }
  joined <- joined[
    !is.na(joined$staff_total_start) &
      !is.na(joined$staff_instructional_start) &
      !is.na(joined$staff_total_end) &
      !is.na(joined$staff_instructional_end),
    ,
    drop = FALSE
  ]
  if (!nrow(joined)) {
    return(joined)
  }

  joined$staff_noninstructional_start <- joined$staff_total_start - joined$staff_instructional_start
  joined$staff_noninstructional_end <- joined$staff_total_end - joined$staff_instructional_end

  joined$total_staff_cut <- joined$staff_total_end < joined$staff_total_start
  joined$instructional_staff_cut <- joined$staff_instructional_end < joined$staff_instructional_start
  joined$noninstructional_staff_cut <- joined$staff_noninstructional_end < joined$staff_noninstructional_start

  joined$total_staff_positions_cut <- pmax(joined$staff_total_start - joined$staff_total_end, 0)
  joined$instructional_staff_positions_cut <- pmax(joined$staff_instructional_start - joined$staff_instructional_end, 0)
  joined$noninstructional_staff_positions_cut <- pmax(joined$staff_noninstructional_start - joined$staff_noninstructional_end, 0)

  joined$total_staff_change <- joined$staff_total_end - joined$staff_total_start
  joined$instructional_staff_change <- joined$staff_instructional_end - joined$staff_instructional_start
  joined$noninstructional_staff_change <- joined$staff_noninstructional_end - joined$staff_noninstructional_start

  joined
}

# Builds article-focused answer rows that mix current-workbook 2024 counts with
# year-specific cohort comparisons for earlier trend windows.
build_article_point_answers <- function(read_df, article_trend_df, distress_compare,
                                        bacc_category_label = "Degree-granting, primarily baccalaureate or above",
                                        latest_year = 2024L, comparison_year = latest_year - 5L,
                                        baseline_year = latest_year - 10L) {
  make_stats <- function(df, predicate) {
    if (is.null(df) || !nrow(df)) {
      return(list(count = 0L, total = 0L, students = NA_real_))
    }
    matches <- predicate(df)
    matches[is.na(matches)] <- FALSE
    list(
      count = sum(matches),
      total = nrow(df),
      students = sum(to_num(df$enrollment_headcount_total[matches]), na.rm = TRUE)
    )
  }

  scope_df <- function(df, scope_label) {
    if (scope_label == "Public institutions") {
      return(df[df$control_label == "Public", , drop = FALSE])
    }
    if (scope_label == "Private not-for-profit institutions") {
      return(df[df$control_label == "Private not-for-profit", , drop = FALSE])
    }
    if (scope_label == "Private for-profit institutions") {
      return(df[df$control_label == "Private for-profit", , drop = FALSE])
    }
    df
  }

  add_stat_row <- function(group, year_value, scope_label, question, df, predicate, calculation, note) {
    stats <- make_stats(scope_df(df, scope_label), predicate)
    make_report_answer_row(
      group = group,
      year = as.integer(year_value),
      scope = scope_label,
      question = question,
      count_yes = stats$count,
      count_scope_total = stats$total,
      pct_of_scope_institutions = safe_pct(stats$count, stats$total),
      students_at_yes_institutions = stats$students,
      calculation = calculation,
      note = note
    )
  }

  latest_df <- read_df[
    as.integer(read_df$year) == as.integer(latest_year) &
      read_df$category == bacc_category_label,
    ,
    drop = FALSE
  ]
  comparison_df <- read_df[
    as.integer(read_df$year) == as.integer(comparison_year) &
      read_df$category == bacc_category_label,
    ,
    drop = FALSE
  ]
  latest_article_df <- article_target_year_cohort_df(article_trend_df, latest_year)
  comparison_article_df <- article_target_year_cohort_df(article_trend_df, comparison_year)
  add_staff_scope_rows <- function(group, start_year, end_year, joined_df,
                                   metric_label, cut_field, positions_cut_field,
                                   method_label, calculation, note_prefix) {
    if (is.null(joined_df) || !nrow(joined_df)) {
      return(data.frame(stringsAsFactors = FALSE))
    }

    scope_labels <- c(
      "All institutions",
      "Public institutions",
      "Private not-for-profit institutions",
      "Private for-profit institutions"
    )
    scope_phrases <- c(
      "cohort institutions",
      "public institutions",
      "private not-for-profit institutions",
      "private for-profit institutions"
    )

    do.call(rbind, lapply(seq_along(scope_labels), function(i) {
      scope_label <- scope_labels[[i]]
      scoped_df <- scope_df(joined_df, scope_label)
      make_report_answer_row(
        group = group,
        year = as.integer(end_year),
        scope = scope_label,
        question = sprintf(
          "%s to %s %s with %s cuts (%s)",
          start_year,
          end_year,
          scope_phrases[[i]],
          metric_label,
          method_label
        ),
        count_yes = sum(scoped_df[[cut_field]], na.rm = TRUE),
        count_scope_total = nrow(scoped_df),
        pct_of_scope_institutions = safe_pct(sum(scoped_df[[cut_field]], na.rm = TRUE), nrow(scoped_df)),
        calculation = calculation,
        note = if (identical(metric_label, "total staff")) {
          total_cut_df <- scoped_df[scoped_df$total_staff_cut, , drop = FALSE]
          sprintf(
            "%s Total staff positions cut in this scope = %s. Within total-staff-cut schools, instructional change = %s and non-instructional change = %s.",
            note_prefix,
            format(round(sum(scoped_df[[positions_cut_field]], na.rm = TRUE)), big.mark = ","),
            format(round(-sum(total_cut_df$instructional_staff_change, na.rm = TRUE)), big.mark = ","),
            format(round(-sum(total_cut_df$noninstructional_staff_change, na.rm = TRUE)), big.mark = ",")
          )
        } else {
          sprintf(
            "%s Total %s positions cut in this scope = %s.",
            note_prefix,
            metric_label,
            format(round(sum(scoped_df[[positions_cut_field]], na.rm = TRUE)), big.mark = ",")
          )
        }
      )
    }))
  }

  current_distress_row <- distress_compare[distress_compare$year == latest_year, , drop = FALSE]

  setup_rows <- append_rows(
    make_report_answer_row(
      group = "Setup",
      year = as.integer(comparison_year),
      scope = "All institutions",
      question = "Year-specific target cohort definition for article trend rows",
      calculation = "Use the target year's domestic, active, four-year cohort; then keep the full historical rows for those eligible unitids when calculating five-year and 10-year trend metrics.",
      note = "This restores schools that later closed or dropped out of the 2024 cohort to the earlier-year comparison window."
    ),
    make_report_answer_row(
      group = "Setup",
      year = as.integer(latest_year),
      scope = "All institutions",
      question = "Closure-sensitive staffing cohort definition",
      calculation = "For closure-sensitive staffing rows, use the start-year cohort and treat schools missing in the end year as zero total and instructional staff at the end of the window.",
      note = "These rows let staffing comparisons keep later-closed schools in both the denominator and the cut totals."
    )
  )

  current_rows <- append_rows(
    make_report_answer_row(
      group = "2024 Current workbook universe",
      year = as.integer(latest_year),
      scope = "All institutions",
      question = sprintf("%s distressed institutions using the workbook warning-score definition", latest_year),
      count_yes = if (nrow(current_distress_row)) current_distress_row$distress_count[[1]] else NA_real_,
      count_scope_total = if (nrow(current_distress_row)) current_distress_row$institutions_total[[1]] else NA_real_,
      pct_of_scope_institutions = if (nrow(current_distress_row)) current_distress_row$distress_pct[[1]] else NA_real_,
      students_at_yes_institutions = if (nrow(current_distress_row)) current_distress_row$distress_students[[1]] else NA_real_,
      calculation = "warning_score_core >= 4",
      note = "Lead framing for the broad financial-stress paragraph."
    ),
    add_stat_row(
      "2024 Current workbook universe",
      latest_year,
      "All institutions",
      sprintf("%s institutions with enrollment declines in 3 of the last 5 years", latest_year),
      latest_df,
      function(df) yes_flag(df$enrollment_decline_last_3_of_5),
      "enrollment_decline_last_3_of_5 == Yes",
      "Current workbook topline used in the draft."
    ),
    add_stat_row(
      "2024 Current workbook universe",
      latest_year,
      "All institutions",
      sprintf("%s institutions with falling net tuition per FTE over the past 5 years", latest_year),
      latest_df,
      function(df) !is.na(df$net_tuition_per_fte_change_5yr) & to_num(df$net_tuition_per_fte_change_5yr) < 0,
      "net_tuition_per_fte_change_5yr < 0",
      "Use this for the tuition-revenue line."
    ),
    add_stat_row(
      "2024 Current workbook universe",
      latest_year,
      "All institutions",
      sprintf("%s institutions with declining instructional staff headcount over the past 5 years", latest_year),
      latest_df,
      function(df) !is.na(df$staff_instructional_headcount_pct_change_5yr) & to_num(df$staff_instructional_headcount_pct_change_5yr) < 0,
      "staff_instructional_headcount_pct_change_5yr < 0",
      "Use this for the instructional-staff line."
    ),
    add_stat_row(
      "2024 Current workbook universe",
      latest_year,
      "All institutions",
      sprintf("%s institutions with losses in 3 of the last 5 years", latest_year),
      latest_df,
      function(df) yes_flag(df$losses_last_3_of_5),
      "losses_last_3_of_5 == Yes",
      "Use this for the repeated-loss line."
    ),
    add_stat_row(
      "2024 Current workbook universe",
      latest_year,
      "Private not-for-profit institutions",
      sprintf("%s private not-for-profit institutions with both repeated losses and repeated enrollment declines", latest_year),
      latest_df,
      function(df) yes_flag(df$losses_last_3_of_5) & yes_flag(df$enrollment_decline_last_3_of_5),
      "losses_last_3_of_5 == Yes AND enrollment_decline_last_3_of_5 == Yes",
      "Use this for the nearly-one-in-five private-nonprofit line."
    ),
    add_stat_row(
      "2024 Current workbook universe",
      latest_year,
      "All institutions",
      sprintf("%s public and private not-for-profit institutions with at least a 10%% increase in international enrollment over the past 10 years", latest_year),
      latest_df[latest_df$control_label %in% c("Public", "Private not-for-profit"), , drop = FALSE],
      function(df) !is.na(df$international_enrollment_pct_change_10yr) & to_num(df$international_enrollment_pct_change_10yr) >= 10,
      "international_enrollment_pct_change_10yr >= 10",
      "Stricter international-growth cutoff for the foreign-student subsidy framing."
    )
  )

  trend_rows <- append_rows(
    add_stat_row(
      "Year-specific trend cohort",
      comparison_year,
      "All institutions",
      sprintf("%s target-year cohort institutions with enrollment declines in 3 of the last 5 years", comparison_year),
      comparison_article_df,
      function(df) yes_flag(df$enrollment_decline_last_3_of_5),
      "enrollment_decline_last_3_of_5 == Yes",
      "Use this instead of the survivor-only 2019 count when later closures should stay in the earlier-year comparison."
    ),
    add_stat_row(
      "Year-specific trend cohort",
      latest_year,
      "All institutions",
      sprintf("%s target-year cohort institutions with enrollment declines in 3 of the last 5 years", latest_year),
      latest_article_df,
      function(df) yes_flag(df$enrollment_decline_last_3_of_5),
      "enrollment_decline_last_3_of_5 == Yes",
      "Same metric as above using the 2024 target-year cohort."
    ),
    add_stat_row(
      "Year-specific trend cohort",
      comparison_year,
      "All institutions",
      sprintf("%s target-year cohort institutions with at least a 10%% five-year enrollment drop", comparison_year),
      comparison_article_df,
      function(df) !is.na(df$enrollment_pct_change_5yr) & to_num(df$enrollment_pct_change_5yr) <= -10,
      sprintf("enrollment_pct_change_5yr <= -10 using the %s to %s window", baseline_year, comparison_year),
      "This is the cleanest metric for showing the restored for-profit distress signal in 2019."
    ),
    add_stat_row(
      "Year-specific trend cohort",
      latest_year,
      "All institutions",
      sprintf("%s target-year cohort institutions with at least a 10%% five-year enrollment drop", latest_year),
      latest_article_df,
      function(df) !is.na(df$enrollment_pct_change_5yr) & to_num(df$enrollment_pct_change_5yr) <= -10,
      sprintf("enrollment_pct_change_5yr <= -10 using the %s to %s window", comparison_year, latest_year),
      "Current five-year comparison window."
    ),
    add_stat_row(
      "Year-specific trend cohort",
      comparison_year,
      "Public institutions",
      sprintf("%s public institutions with at least a 10%% five-year enrollment drop", comparison_year),
      comparison_article_df,
      function(df) !is.na(df$enrollment_pct_change_5yr) & to_num(df$enrollment_pct_change_5yr) <= -10,
      sprintf("enrollment_pct_change_5yr <= -10 using the %s to %s window", baseline_year, comparison_year),
      "Use this for the one-in-four public comparison."
    ),
    add_stat_row(
      "Year-specific trend cohort",
      latest_year,
      "Public institutions",
      sprintf("%s public institutions with at least a 10%% five-year enrollment drop", latest_year),
      latest_article_df,
      function(df) !is.na(df$enrollment_pct_change_5yr) & to_num(df$enrollment_pct_change_5yr) <= -10,
      sprintf("enrollment_pct_change_5yr <= -10 using the %s to %s window", comparison_year, latest_year),
      "Use this for the one-in-three public comparison."
    )
  )

  survivor_bias_rows <- append_rows(
    add_stat_row(
      "Survivor-bias comparison",
      comparison_year,
      "All institutions",
      sprintf("%s survivor-only workbook institutions with enrollment declines in 3 of the last 5 years", comparison_year),
      comparison_df,
      function(df) yes_flag(df$enrollment_decline_last_3_of_5),
      "enrollment_decline_last_3_of_5 == Yes using the current workbook survivor scope",
      "Shows the older 927-of-1803 framing for comparison against the year-specific target cohort."
    ),
    add_stat_row(
      "Survivor-bias comparison",
      comparison_year,
      "Private for-profit institutions",
      sprintf("%s survivor-only workbook private for-profit institutions with at least a 10%% five-year enrollment drop", comparison_year),
      comparison_df,
      function(df) !is.na(df$enrollment_pct_change_5yr) & to_num(df$enrollment_pct_change_5yr) <= -10,
      sprintf("enrollment_pct_change_5yr <= -10 using the %s to %s window and the current workbook survivor scope", baseline_year, comparison_year),
      "This is the compressed for-profit signal in the current workbook."
    ),
    add_stat_row(
      "Survivor-bias comparison",
      comparison_year,
      "Private for-profit institutions",
      sprintf("%s target-year cohort private for-profit institutions with at least a 10%% five-year enrollment drop", comparison_year),
      comparison_article_df,
      function(df) !is.na(df$enrollment_pct_change_5yr) & to_num(df$enrollment_pct_change_5yr) <= -10,
      sprintf("enrollment_pct_change_5yr <= -10 using the %s to %s window and the %s target-year cohort", baseline_year, comparison_year, comparison_year),
      "This restores later-closed schools to the earlier-year denominator."
    )
  )

  closure_staff_2014_2019 <- article_staff_cohort_df(article_trend_df, baseline_year, comparison_year, treat_missing_end_as_zero = TRUE)
  closure_staff_2019_2024 <- article_staff_cohort_df(article_trend_df, comparison_year, latest_year, treat_missing_end_as_zero = TRUE)

  staff_rows <- append_rows(
    add_staff_scope_rows(
      group = "Closure-sensitive staff cohort",
      start_year = baseline_year,
      end_year = comparison_year,
      joined_df = closure_staff_2014_2019,
      metric_label = "total staff",
      cut_field = "total_staff_cut",
      positions_cut_field = "total_staff_positions_cut",
      method_label = "closures counted as zero",
      calculation = "Compare the start-year cohort to the end year and treat schools that disappeared from the end year as zero total and instructional staff.",
      note_prefix = "Closure-sensitive method."
    ),
    add_staff_scope_rows(
      group = "Closure-sensitive staff cohort",
      start_year = baseline_year,
      end_year = comparison_year,
      joined_df = closure_staff_2014_2019,
      metric_label = "instructional staff",
      cut_field = "instructional_staff_cut",
      positions_cut_field = "instructional_staff_positions_cut",
      method_label = "closures counted as zero",
      calculation = "Compare the start-year cohort to the end year and treat schools that disappeared from the end year as zero total and instructional staff.",
      note_prefix = "Closure-sensitive method."
    ),
    add_staff_scope_rows(
      group = "Closure-sensitive staff cohort",
      start_year = baseline_year,
      end_year = comparison_year,
      joined_df = closure_staff_2014_2019,
      metric_label = "non-instructional staff",
      cut_field = "noninstructional_staff_cut",
      positions_cut_field = "noninstructional_staff_positions_cut",
      method_label = "closures counted as zero",
      calculation = "Compare the start-year cohort to the end year and treat schools that disappeared from the end year as zero total and instructional staff.",
      note_prefix = "Closure-sensitive method."
    ),
    add_staff_scope_rows(
      group = "Closure-sensitive staff cohort",
      start_year = comparison_year,
      end_year = latest_year,
      joined_df = closure_staff_2019_2024,
      metric_label = "total staff",
      cut_field = "total_staff_cut",
      positions_cut_field = "total_staff_positions_cut",
      method_label = "closures counted as zero",
      calculation = "Compare the start-year cohort to the end year and treat schools that disappeared from the end year as zero total and instructional staff.",
      note_prefix = "Closure-sensitive method."
    ),
    add_staff_scope_rows(
      group = "Closure-sensitive staff cohort",
      start_year = comparison_year,
      end_year = latest_year,
      joined_df = closure_staff_2019_2024,
      metric_label = "instructional staff",
      cut_field = "instructional_staff_cut",
      positions_cut_field = "instructional_staff_positions_cut",
      method_label = "closures counted as zero",
      calculation = "Compare the start-year cohort to the end year and treat schools that disappeared from the end year as zero total and instructional staff.",
      note_prefix = "Closure-sensitive method."
    ),
    add_staff_scope_rows(
      group = "Closure-sensitive staff cohort",
      start_year = comparison_year,
      end_year = latest_year,
      joined_df = closure_staff_2019_2024,
      metric_label = "non-instructional staff",
      cut_field = "noninstructional_staff_cut",
      positions_cut_field = "noninstructional_staff_positions_cut",
      method_label = "closures counted as zero",
      calculation = "Compare the start-year cohort to the end year and treat schools that disappeared from the end year as zero total and instructional staff.",
      note_prefix = "Closure-sensitive method."
    )
  )

  append_rows(setup_rows, current_rows, trend_rows, survivor_bias_rows, staff_rows)
}

# Builds sector/window tables for article graphics, including a closure-
# sensitive instructional-staff cohort cut view.
build_article_graphics_table <- function(article_trend_df,
                                         latest_year = 2024L,
                                         comparison_year = latest_year - 5L,
                                         baseline_year = latest_year - 10L) {
  make_graphic_row <- function(graphic, method, window_start, window_end, scope,
                               count_yes, count_scope_total, students_at_yes_institutions = NA_real_,
                               total_positions_cut = NA_real_, calculation, note) {
    data.frame(
      graphic = graphic,
      method = method,
      window_start = as.integer(window_start),
      window_end = as.integer(window_end),
      window_label = sprintf("%s to %s", window_start, window_end),
      scope = scope,
      count_yes = count_yes,
      count_scope_total = count_scope_total,
      pct_of_scope_institutions = safe_pct(count_yes, count_scope_total),
      students_at_yes_institutions = students_at_yes_institutions,
      total_positions_cut = total_positions_cut,
      calculation = calculation,
      note = note,
      stringsAsFactors = FALSE
    )
  }

  scope_specs <- list(
    list(label = "All institutions", filter = function(df) rep(TRUE, nrow(df))),
    list(label = "Public institutions", filter = function(df) df$control_label == "Public"),
    list(label = "Private not-for-profit institutions", filter = function(df) df$control_label == "Private not-for-profit"),
    list(label = "Private for-profit institutions", filter = function(df) df$control_label == "Private for-profit")
  )

  build_target_year_rows <- function(target_year, graphic, metric_label, predicate, note) {
    cohort_df <- article_target_year_cohort_df(article_trend_df, target_year)
    if (!nrow(cohort_df)) {
      return(data.frame(stringsAsFactors = FALSE))
    }
    do.call(rbind, lapply(scope_specs, function(scope_spec) {
      scope_df <- cohort_df[scope_spec$filter(cohort_df), , drop = FALSE]
      matches <- predicate(scope_df)
      matches[is.na(matches)] <- FALSE
      make_graphic_row(
        graphic = graphic,
        method = "Target-year cohort",
        window_start = as.integer(target_year) - 5L,
        window_end = as.integer(target_year),
        scope = scope_spec$label,
        count_yes = sum(matches),
        count_scope_total = nrow(scope_df),
        students_at_yes_institutions = sum(to_num(scope_df$enrollment_headcount_total[matches]), na.rm = TRUE),
        calculation = metric_label,
        note = note
      )
    }))
  }

  build_staff_cohort_rows <- function(start_year, end_year, graphic, cut_field,
                                      positions_cut_field, treat_missing_end_as_zero = FALSE) {
    joined <- article_staff_cohort_df(
      article_trend_df = article_trend_df,
      start_year = start_year,
      end_year = end_year,
      treat_missing_end_as_zero = treat_missing_end_as_zero
    )
    if (!nrow(joined)) {
      return(joined)
    }
    do.call(rbind, lapply(scope_specs, function(scope_spec) {
      scope_df <- joined[scope_spec$filter(joined), , drop = FALSE]
      make_graphic_row(
        graphic = graphic,
        method = if (treat_missing_end_as_zero) "Start-year cohort with closures counted as zero" else "Start-year cohort, matched endpoints only",
        window_start = as.integer(start_year),
        window_end = as.integer(end_year),
        scope = scope_spec$label,
        count_yes = sum(scope_df[[cut_field]], na.rm = TRUE),
        count_scope_total = nrow(scope_df),
        total_positions_cut = sum(scope_df[[positions_cut_field]], na.rm = TRUE),
        calculation = if (treat_missing_end_as_zero) "Compare the start-year cohort to the end year and treat schools missing in the end year as zero total and instructional staff." else "Compare only schools with total and instructional staff reported at both endpoints.",
        note = "Closure-inclusive staff-cut graphic rows use a shared denominator for total, instructional, and non-instructional staff."
      )
    }))
  }

  append_rows(
    build_target_year_rows(
      comparison_year,
      "Enrollment declines in 3 of the last 5 years",
      "enrollment_decline_last_3_of_5 == Yes",
      function(df) yes_flag(df$enrollment_decline_last_3_of_5),
      "Year-specific target-year cohort. Use this to compare 2019 versus 2024 without survivorship bias."
    ),
    build_target_year_rows(
      latest_year,
      "Enrollment declines in 3 of the last 5 years",
      "enrollment_decline_last_3_of_5 == Yes",
      function(df) yes_flag(df$enrollment_decline_last_3_of_5),
      "Year-specific target-year cohort. Use this to compare 2019 versus 2024 without survivorship bias."
    ),
    build_target_year_rows(
      comparison_year,
      "Enrollment decline of at least 10% over five years",
      sprintf("enrollment_pct_change_5yr <= -10 using the %s to %s window", baseline_year, comparison_year),
      function(df) !is.na(df$enrollment_pct_change_5yr) & to_num(df$enrollment_pct_change_5yr) <= -10,
      "Year-specific target-year cohort. This is the strongest restored for-profit distress signal in 2019."
    ),
    build_target_year_rows(
      latest_year,
      "Enrollment decline of at least 10% over five years",
      sprintf("enrollment_pct_change_5yr <= -10 using the %s to %s window", comparison_year, latest_year),
      function(df) !is.na(df$enrollment_pct_change_5yr) & to_num(df$enrollment_pct_change_5yr) <= -10,
      "Year-specific target-year cohort. This is the comparable 2024 window."
    ),
    build_target_year_rows(
      comparison_year,
      "Instructional staff headcount down over five years",
      sprintf("staff_instructional_headcount_pct_change_5yr < 0 using the %s to %s window", baseline_year, comparison_year),
      function(df) !is.na(df$staff_instructional_headcount_pct_change_5yr) & to_num(df$staff_instructional_headcount_pct_change_5yr) < 0,
      "Year-specific target-year cohort. This is the clean five-year IPEDS staffing metric."
    ),
    build_target_year_rows(
      latest_year,
      "Instructional staff headcount down over five years",
      sprintf("staff_instructional_headcount_pct_change_5yr < 0 using the %s to %s window", comparison_year, latest_year),
      function(df) !is.na(df$staff_instructional_headcount_pct_change_5yr) & to_num(df$staff_instructional_headcount_pct_change_5yr) < 0,
      "Year-specific target-year cohort. This is the clean five-year IPEDS staffing metric."
    ),
    build_staff_cohort_rows(
      baseline_year,
      comparison_year,
      graphic = "Total staff cuts by sector",
      cut_field = "total_staff_cut",
      positions_cut_field = "total_staff_positions_cut",
      treat_missing_end_as_zero = TRUE
    ),
    build_staff_cohort_rows(
      comparison_year,
      latest_year,
      graphic = "Total staff cuts by sector",
      cut_field = "total_staff_cut",
      positions_cut_field = "total_staff_positions_cut",
      treat_missing_end_as_zero = TRUE
    ),
    build_staff_cohort_rows(
      baseline_year,
      comparison_year,
      graphic = "Instructional staff cuts by sector",
      cut_field = "instructional_staff_cut",
      positions_cut_field = "instructional_staff_positions_cut",
      treat_missing_end_as_zero = TRUE
    ),
    build_staff_cohort_rows(
      comparison_year,
      latest_year,
      graphic = "Instructional staff cuts by sector",
      cut_field = "instructional_staff_cut",
      positions_cut_field = "instructional_staff_positions_cut",
      treat_missing_end_as_zero = TRUE
    ),
    build_staff_cohort_rows(
      baseline_year,
      comparison_year,
      graphic = "Non-instructional staff cuts by sector",
      cut_field = "noninstructional_staff_cut",
      positions_cut_field = "noninstructional_staff_positions_cut",
      treat_missing_end_as_zero = TRUE
    ),
    build_staff_cohort_rows(
      comparison_year,
      latest_year,
      graphic = "Non-instructional staff cuts by sector",
      cut_field = "noninstructional_staff_cut",
      positions_cut_field = "noninstructional_staff_positions_cut",
      treat_missing_end_as_zero = TRUE
    )
  )
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
    distress_share_pct                         = if (nrow(df) == 0) NA_real_ else safe_pct(sum(!is.na(to_num(df$warning_score_core)) & to_num(df$warning_score_core) >= 4, na.rm = TRUE), nrow(df)),
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
