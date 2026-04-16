# scripts/shared/workbook_helpers.R
#
# Pure utility helpers for the article workbook build.
# Source this inside main() in build_article_workbook.R after utils.R.
#
# Requires: no additional packages beyond base R

# ---------------------------------------------------------------------------
# Small value helpers
# ---------------------------------------------------------------------------

# Returns TRUE when the field is the literal string "Yes" (case-insensitive).
yes_flag <- function(x) {
  trimws(tolower(as.character(x))) == "yes"
}

# Percentage of num over den; NA when either is NA or den is 0.
safe_pct <- function(num, den) {
  ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, (num / den) * 100)
}

# Escapes the four XML special characters so values are safe inside XML tags.
escape_xml <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("&",  "&amp;",  x, fixed = TRUE)
  x <- gsub("<",  "&lt;",   x, fixed = TRUE)
  x <- gsub(">",  "&gt;",   x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

# Convenience wrapper for additive boolean risk scores: sums any number of
# logical predicates row-wise, treating NA as 0.  Replaces the verbose
# rowSums(cbind(...), na.rm = TRUE) idiom throughout build_article_workbook.R.
row_score <- function(...) rowSums(cbind(...), na.rm = TRUE)

# Core financial-distress score used in both the "latest year" sheet and the
# multi-year trend loop.  Any data frame with the required columns can be
# scored with this function.
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

# Returns the 75th-percentile of x, or NA when x is all-NA.
q75_safe <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE))
}

# Returns the 25th-percentile of x, or NA when x is all-NA.
q25_safe <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE))
}

# ---------------------------------------------------------------------------
# File I/O helpers
# ---------------------------------------------------------------------------

# Reads a CSV, returning an empty data frame when the file is missing.
read_csv_if_exists <- function(path) {
  if (!file.exists(path)) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
}

# Reads a CSV that must exist; stops with a helpful message when it does not.
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

# Creates a single-row data frame for the article Summary sheet.
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

# Appends any number of data frames with shared columns, skipping NULL or
# zero-row inputs. Useful for building long workbook tabs in stages.
append_rows <- function(...) {
  rows <- list(...)
  rows <- rows[!vapply(rows, is.null, logical(1))]
  rows <- rows[!vapply(rows, function(x) is.data.frame(x) && nrow(x) == 0, logical(1))]
  if (length(rows) == 0) return(data.frame(stringsAsFactors = FALSE))
  do.call(rbind, rows)
}

# Expands a named vector/list of group values into a Summary-sheet row using
# the standard workbook group order.
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

# Convenience wrapper for the common pattern of pairing count and percent rows.
make_count_pct_rows <- function(metric, counts, pcts, notes = "") {
  append_rows(
    make_group_row(metric, "count", counts, notes),
    make_group_row(metric, "percent", pcts, notes)
  )
}

# ---------------------------------------------------------------------------
# Data-frame utilities
# ---------------------------------------------------------------------------

# Sorts `df` by one or more column names; handles NAs at the end.
sort_df <- function(df, cols, decreasing = FALSE) {
  if (nrow(df) == 0) return(df)
  ord_args <- c(lapply(cols, function(col) {
    x <- df[[col]]
    if (decreasing) -xtfrm(x) else xtfrm(x)
  }), list(na.last = TRUE))
  df[do.call(order, ord_args), , drop = FALSE]
}

# Applies a predicate across an explicit list of group data frames.
count_by_group_from <- function(group_list, pred) {
  sapply(group_list, function(df) sum(pred(df), na.rm = TRUE))
}

# Percentage share by group for any row predicate.
pct_by_group_from <- function(group_list, pred) {
  sapply(group_list, function(df) {
    if (nrow(df) == 0) return(NA_real_)
    safe_pct(sum(pred(df), na.rm = TRUE), nrow(df))
  })
}

# Applies a numeric summary function (median, mean, etc.) to one field across
# each workbook group, returning a named vector.
numeric_stat_by_group <- function(group_list, field, stat_fn = stats::median) {
  sapply(group_list, function(df) {
    x <- to_num(df[[field]])
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    stat_fn(x, na.rm = TRUE)
  })
}

# Returns the top row by `metric` for each group, or NULL when that group has
# no non-missing values for the metric.
top_metric_by_group_from <- function(group_list, metric, decreasing = TRUE) {
  lapply(group_list, function(df) {
    keep <- !is.na(df[[metric]])
    if (!any(keep)) return(NULL)
    df <- df[keep, , drop = FALSE]
    df[order(df[[metric]], decreasing = decreasing), , drop = FALSE][1, , drop = FALSE]
  })
}

# Weighted percentage of num_col / den_col across df rows.
weighted_intl_pct <- function(df, num_col, den_col) {
  keep <- !is.na(df[[num_col]]) & !is.na(df[[den_col]]) & df[[den_col]] > 0
  if (!any(keep)) return(NA_real_)
  safe_pct(sum(df[[num_col]][keep], na.rm = TRUE), sum(df[[den_col]][keep], na.rm = TRUE))
}

# ---------------------------------------------------------------------------
# Cross-tab helpers for event-vs-non-event comparisons
# ---------------------------------------------------------------------------

# Summarises `df` into a single row of financial health metrics.
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

# Builds "with event" vs "without event" cross-tab rows across four sector
# slices (All / Public / Private NFP / Private FP).
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

# Returns a stable "fingerprint" for a worksheet data frame so duplicates can
# be detected before writing the XML blob.
worksheet_signature <- function(df) {
  if (is.null(df) || nrow(df) == 0) return("EMPTY")
  paste(capture.output(dput(df)), collapse = "\n")
}

# Renders a single table cell as SpreadsheetML XML.
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

# Renders a full worksheet (name + data frame) as a SpreadsheetML XML block.
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
