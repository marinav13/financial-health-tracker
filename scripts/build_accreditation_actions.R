# =========================================================================
# build_accreditation_actions.R
# =========================================================================
#
# PURPOSE: Scrape accreditation actions (sanctions, warnings, probation) from
#          regional accreditors and match them to the financial tracker dataset.
#
# INPUTS:
#   - Financial tracker CSV (latest year data with institution info)
#   - Scraped data from 6 regional accreditors:
#     * MSCHE (Middle States), HLC (Higher Learning Commission),
#     * SACSCOC (Southern Association), NECHE (New England),
#     * WSCUC (Western Association)
#
# OUTPUTS:
#   - accreditation_actions_joined.csv     (raw actions with financial metrics)
#   - accreditation_institution_summary.csv (one row per institution with action summary)
#   - accreditation_current_status.csv     (filtered to active warnings/adverse actions only)
#   - accreditation_unmatched_for_review.csv (accreditor data that didn't match institutions)
#   - accreditation_source_coverage.csv    (summary of what accreditor data was scraped)
#   - accreditation_workbook.xlsx          (all above + notes sheet)
#
# WORKFLOW:
#   1. Load financial tracker data and normalize institution names
#   2. Build fuzzy-match lookup tables (exact name+state, name-only fallback)
#   3. Fetch and scrape accreditation action pages (with local cache fallback)
#   4. Clean and normalize accreditor data
#   5. Match institutions to tracker using lookup tables
#   6. Join matched actions with financial health metrics
#   7. Build three summary views: all actions, institution summary, current active status
#   8. Identify unmatched institutions for manual review
#   9. Write outputs as CSVs and Excel workbook
#
# DOMAIN CONCEPTS:
#   - accreditation_warning: true if action_type is warning, probation, or show_cause
#   - accreditation_warning_or_notice: broader set including notice (HLC's public sanction)
#   - Action status: "active" or past/closed
#   - Unmatched records: often non-4-year schools or name mismatches across sources
#
# NOTE: This is intentionally partial coverage (6 of 7 regional accreditors).
#       ACCJC (community colleges) not yet implemented.

# Excel's cell limit is 32767 characters, but openxlsx serializes cell text
# into XML where characters expand (for example apostrophes become &apos;),
# and its internal handling still appears to need a small safety margin even
# after accounting for escaping. Keep the workbook-only copy comfortably below
# the hard limit so writeData() stays warning-free.
EXCEL_CELL_CHAR_LIMIT <- 30000L
EXCEL_CELL_TRUNCATION_SUFFIX <- " ...[truncated for Excel cell limit]"

excel_xml_escape_text <- function(text) {
  out <- gsub("&", "&amp;", text, fixed = TRUE)
  out <- gsub("<", "&lt;", out, fixed = TRUE)
  out <- gsub(">", "&gt;", out, fixed = TRUE)
  out <- gsub("\"", "&quot;", out, fixed = TRUE)
  gsub("'", "&apos;", out, fixed = TRUE)
}

excel_xml_text_length <- function(text) {
  nchar(excel_xml_escape_text(text), type = "chars")
}

truncate_excel_cell_text <- function(value,
                                     limit = EXCEL_CELL_CHAR_LIMIT,
                                     suffix = EXCEL_CELL_TRUNCATION_SUFFIX) {
  if (length(value) == 0 || is.na(value)) return(value)
  text <- as.character(value)
  if (excel_xml_text_length(text) <= limit) return(text)

  suffix_chars <- excel_xml_text_length(suffix)
  keep_limit <- max(0L, limit - suffix_chars)
  lo <- 0L
  hi <- nchar(text, type = "chars")

  while (lo < hi) {
    mid <- as.integer(ceiling((lo + hi) / 2))
    candidate <- substr(text, 1L, mid)
    if (excel_xml_text_length(candidate) <= keep_limit) {
      lo <- mid
    } else {
      hi <- mid - 1L
    }
  }

  paste0(substr(text, 1L, lo), suffix)
}

sanitize_excel_workbook_frame <- function(df,
                                          sheet_name,
                                          limit = EXCEL_CELL_CHAR_LIMIT,
                                          suffix = EXCEL_CELL_TRUNCATION_SUFFIX) {
  out <- df
  truncations <- list()

  if (!is.data.frame(out) || nrow(out) == 0L) {
    return(list(data = out, truncations = data.frame()))
  }

  char_cols <- names(out)[vapply(out, function(col) is.character(col) || is.factor(col), logical(1))]
  if (length(char_cols) == 0L) {
    return(list(data = out, truncations = data.frame()))
  }

  for (col_name in char_cols) {
    values <- as.character(out[[col_name]])
    char_counts <- ifelse(is.na(values), NA_integer_, nchar(values, type = "chars"))
    over_limit <- !is.na(char_counts) & char_counts > limit
    if (!any(over_limit)) next

    values[over_limit] <- vapply(
      values[over_limit],
      truncate_excel_cell_text,
      character(1),
      limit = limit,
      suffix = suffix,
      USE.NAMES = FALSE
    )
    out[[col_name]] <- values
    truncations[[length(truncations) + 1L]] <- data.frame(
      sheet = sheet_name,
      column = col_name,
      count = sum(over_limit),
      max_original_chars = max(char_counts[over_limit]),
      stringsAsFactors = FALSE
    )
  }

  list(
    data = out,
    truncations = if (length(truncations)) dplyr::bind_rows(truncations) else data.frame()
  )
}

main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args          <- parse_cli_args(cli_args)
  ipeds         <- load_ipeds_paths()
  ipeds_layout  <- ipeds$ipeds_layout
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)

  ensure_packages(c("dplyr", "httr2", "openxlsx", "pdftools", "purrr", "readr", "stringr", "tidyr", "xml2"))

  # -----------------------------------------------------------------------
  # PARSE COMMAND-LINE ARGUMENTS
  financial_input <- get_arg_value(
    "--financial-input",
    ipeds_layout(root = ".")$dataset_csv
  )
  output_prefix <- get_arg_value(
    "--output-prefix",
    file.path(getwd(), "data_pipelines", "accreditation", "accreditation_tracker")
  )
  refresh <- tolower(get_arg_value("--refresh", "true")) %in% c("true", "1", "yes", "y")
  allow_partial_accreditation <- arg_has(args, "--allow-partial-accreditation")
  in_ci <- identical(Sys.getenv("CI"), "true") || identical(Sys.getenv("GITHUB_ACTIONS"), "true")

  # Per-site warn_on_empty_parse() reads this option as its default `fail`.
  # In CI (without the --allow-partial-accreditation override) we want a
  # suspicious empty parse to stop() at the call site, so the refresh fails
  # fast instead of running every downstream scraper and only being caught
  # by the workflow's log-grep drift gate. Symmetric with the fail= usage
  # for warn_if_scrape_count_dropped / warn_if_action_type_dropped below.
  options(tracker.fail_on_empty_parse = in_ci && !allow_partial_accreditation)

  if (!file.exists(financial_input)) {
    stop("Financial input file not found: ", financial_input)
  }

  cache_dir <- file.path(getwd(), "data_pipelines", "accreditation", "cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(output_prefix), recursive = TRUE, showWarnings = FALSE)

  source(file.path(getwd(), "scripts", "shared", "accreditation_helpers.R"))
  source(file.path(getwd(), "scripts", "shared", "accreditation_scrapers.R"))

  # -----------------------------------------------------------------------
  # LOAD FINANCIAL TRACKER DATA
  message("Reading financial tracker data ...")
  financial_all <- readr::read_csv(financial_input, show_col_types = FALSE, progress = FALSE)
  latest_year <- suppressWarnings(max(financial_all$year, na.rm = TRUE))
  financial_latest <- financial_all |>
    dplyr::filter(year == latest_year) |>
    dplyr::mutate(
      unitid = as.character(unitid),
      institution_name_tracker = institution_name,
      state_full = state_name(state),
      norm_name = normalize_accreditation_name(institution_name)
    )

  # Build lookup tables for matching accreditor institution names to tracker names
  lookup_exact <- financial_latest |>
    dplyr::transmute(
      matched_unitid = unitid,
      tracker_name = institution_name_tracker,
      tracker_state = state_full,
      norm_name,
      state_match = state_full
    ) |>
    dplyr::add_count(norm_name, state_match, name = "candidate_count") |>
    dplyr::filter(candidate_count == 1L) |>
    dplyr::select(-candidate_count)

  # Fallback lookup: match on normalized name only
  lookup_name_only <- financial_latest |>
    dplyr::transmute(
      matched_unitid = unitid,
      tracker_name = institution_name_tracker,
      tracker_state = state_full,
      norm_name
    ) |>
    dplyr::add_count(norm_name, name = "candidate_count") |>
    dplyr::filter(candidate_count == 1L) |>
    dplyr::select(-candidate_count)

  # -----------------------------------------------------------------------
  # FETCH AND PROCESS ACCREDITATION ACTIONS FROM ALL ACCREDITORS
  message("Fetching accreditation actions ...")
  reset_accreditation_fetch_telemetry()

  # Priority 4: per-scraper validation — each parse_* is run individually so
  # we can detect and warn about any scraper that returns zero rows (likely a
  # site-structure change) before merging everything together.
  scraper_results <- list(
    MSCHE  = with_accreditation_fetch_context("MSCHE", parse_msche(cache_dir, refresh)),
    HLC    = with_accreditation_fetch_context("HLC", parse_hlc(cache_dir, refresh)),
    SACSCOC= with_accreditation_fetch_context("SACSCOC", parse_sacscoc(cache_dir, refresh)),
    NECHE  = with_accreditation_fetch_context("NECHE", parse_neche(cache_dir, refresh)),
    WSCUC  = with_accreditation_fetch_context("WSCUC", parse_wscuc(cache_dir, refresh)),
    NWCCU  = with_accreditation_fetch_context("NWCCU", parse_nwccu(cache_dir, refresh))
  )
  scraper_results <- purrr::imap(
    scraper_results,
    ~ ensure_accreditation_action_schema(.x, sprintf("%s scraper output", .y))
  )
  
  # Accreditors that may legitimately return 0 rows (no qualifying institutions under action).
  # Used to differentiate between "nothing qualifies" vs "scraper broken" when zero rows returned.
  ZERO_IS_EXPECTED <- c("NWCCU")
  
  purrr::iwalk(scraper_results, function(df, name) {
    if (nrow(df) == 0) {
      if (name %in% ZERO_IS_EXPECTED) {
        # Soft notice for accreditors where 0 is expected (no qualifying institutions)
        message(sprintf("  %s: no qualifying institutions under action (expected when no adverse findings)", name))
      } else {
        msg <- sprintf(
          "SCRAPER RETURNED 0 ROWS: %s — the accreditor's site structure may have changed, or the scraper is broken. Verify output before publishing.",
          name
        )
        if (in_ci && !allow_partial_accreditation) {
          stop(msg, call. = FALSE)
        }
        warning(msg, call. = FALSE)
      }
    } else {
      req_cols <- c("institution_name_raw", "accreditor", "action_type")
      missing_cols <- setdiff(req_cols, names(df))
      if (length(missing_cols) > 0) {
        warning(sprintf(
          "SCRAPER SCHEMA MISMATCH: %s is missing required columns: %s",
          name, paste(missing_cols, collapse = ", ")
        ), call. = FALSE)
      }
    }
  })
  raw_actions <- dplyr::bind_rows(scraper_results) |>
    dplyr::mutate(
      institution_name_raw = clean_text(institution_name_raw),
      institution_state_raw = clean_text(institution_state_raw)
    ) |>
    dplyr::mutate(
      parsed_from_name = purrr::map(institution_name_raw, extract_name_state_from_item),
      embedded_name_raw = purrr::map_chr(parsed_from_name, ~ .x$institution_name_raw),
      embedded_state_raw = purrr::map_chr(parsed_from_name, ~ .x$institution_state_raw),
      has_state = !is.na(institution_state_raw) & nzchar(institution_state_raw),
      has_embedded_state = !is.na(embedded_state_raw) & nzchar(embedded_state_raw),
      institution_name_raw = dplyr::if_else(!has_state & has_embedded_state, embedded_name_raw, institution_name_raw),
      institution_state_raw = dplyr::if_else(!has_state & has_embedded_state, embedded_state_raw, institution_state_raw)
    ) |>
    dplyr::select(-parsed_from_name, -embedded_name_raw, -embedded_state_raw, -has_state, -has_embedded_state) |>
    dplyr::mutate(
      institution_name_normalized = normalize_accreditation_name(institution_name_raw),
      institution_state_normalized = state_name(institution_state_raw),
      action_type = tolower(trimws(action_type)),
      accreditation_warning = action_type %in% c("warning", "probation", "show_cause"),
      accreditation_warning_or_notice = action_type %in% c("notice", "warning", "probation", "show_cause")
    ) |>
    match_institutions_to_tracker(lookup_exact, lookup_name_only) |>
    dplyr::select(
      unitid,
      institution_name_raw,
      institution_name_normalized,
      institution_state_raw,
      institution_state_normalized,
      tracker_name,
      tracker_state,
      match_method,
      accreditor,
      action_type,
      action_label_raw,
      action_status,
      action_date,
      action_year,
      action_scope,
      accreditation_warning,
      accreditation_warning_or_notice,
      source_url,
      source_title,
      notes,
      source_page_url,
      source_page_modified,
      last_seen_at
    ) |>
    dplyr::distinct()

  # -----------------------------------------------------------------------
  # SELECT FINANCIAL METRICS TO JOIN
  latest_fields <- c(
    "unitid",
    "institution_name",
    "year",
    "control_label",
    "sector",
    "level",
    "urbanization",
    "category",
    "state",
    "city",
    "enrollment_pct_change_5yr",
    "revenue_pct_change_5yr",
    "revenue_decreased_5yr",
    "enrollment_decreased_5yr",
    "revenue_10pct_drop_last_3_of_5",
    "enrollment_decline_last_3_of_5",
    "ended_2024_at_loss",
    "losses_last_3_of_5",
    "loss_years_last_10",
    "net_tuition_per_fte_change_5yr",
    "tuition_dependence_pct",
    "state_funding_pct_core_revenue",
    "federal_grants_contracts_pell_adjusted_pct_core_revenue",
    "pct_international_all",
    "staff_total_headcount_pct_change_5yr",
    "discount_pct_change_5yr",
    "endowment_pct_change_5yr",
    "liquidity",
    "leverage"
  )

  financial_join <- financial_latest |>
    dplyr::select(dplyr::any_of(latest_fields))

  # -----------------------------------------------------------------------
  # JOIN ACTIONS WITH FINANCIAL METRICS
  actions_joined <- raw_actions |>
    dplyr::left_join(financial_join, by = "unitid")

  # Helper: collapse multiple values into a semicolon-separated string
  collapse_unique <- function(x) {
    vals <- unique(stats::na.omit(as.character(x)))
    if (length(vals) == 0) NA_character_ else paste(sort(vals), collapse = "; ")
  }

  # -----------------------------------------------------------------------
  # BUILD THREE SUMMARY VIEWS OF ACCREDITATION ACTIONS
  # View 1: Institution summary (all actions collapsed per institution)
  institution_summary <- actions_joined |>
    dplyr::filter(!is.na(unitid)) |>
    dplyr::group_by(unitid, tracker_name, tracker_state) |>
    dplyr::summarise(
      accreditors = collapse_unique(accreditor),
      action_types = collapse_unique(action_type),
      action_labels = collapse_unique(action_label_raw),
      active_actions = collapse_unique(action_type[action_status == "active"]),
      has_active_warning = any(accreditation_warning & action_status == "active", na.rm = TRUE),
      has_active_warning_or_notice = any(accreditation_warning_or_notice & action_status == "active", na.rm = TRUE),
      has_active_adverse_action = any(action_type == "adverse_action" & action_status == "active", na.rm = TRUE),
      action_count = dplyr::n(),
      latest_action_date = if (all(is.na(action_date))) as.Date(NA) else max(action_date, na.rm = TRUE),
      latest_action_year = if (all(is.na(action_year))) NA_integer_ else max(action_year, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(financial_join, by = c("unitid"))

  # View 2: Current status (filtered to only active warnings/adverse actions)
  current_status <- institution_summary |>
    dplyr::filter(has_active_warning_or_notice | has_active_adverse_action) |>
    dplyr::arrange(
      dplyr::desc(has_active_warning_or_notice),
      dplyr::desc(has_active_adverse_action),
      tracker_name
    )

  # View 3: Unmatched institutions (accreditor data with no tracker match)
  unmatched_for_review <- actions_joined |>
    dplyr::filter(is.na(unitid)) |>
    dplyr::distinct(
      institution_name_raw,
      institution_name_normalized,
      institution_state_raw,
      institution_state_normalized,
      accreditor,
      action_type,
      action_label_raw,
      source_url,
      .keep_all = TRUE
    ) |>
    build_match_suggestions(financial_latest)

  # Coverage summary: preserve the existing action counts, then append
  # per-accreditor fetch/cache telemetry so freshness can be reviewed.
  action_counts <- actions_joined |>
    dplyr::count(accreditor, action_type, action_status, sort = TRUE) |>
    dplyr::mutate(
      row_type = "action_count",
      resource_type = NA_character_,
      fetch_total_count = NA_integer_,
      fetch_fresh_count = NA_integer_,
      fetch_cache_read_count = NA_integer_,
      fetch_cache_fallback_count = NA_integer_,
      fetch_cache_age_min_days = NA_real_,
      fetch_cache_age_max_days = NA_real_,
      scrape_row_count = NA_integer_
    ) |>
    dplyr::select(
      row_type,
      accreditor,
      action_type,
      action_status,
      n,
      resource_type,
      fetch_total_count,
      fetch_fresh_count,
      fetch_cache_read_count,
      fetch_cache_fallback_count,
      fetch_cache_age_min_days,
      fetch_cache_age_max_days,
      scrape_row_count
    )

  fetch_telemetry <- get_accreditation_fetch_telemetry()
  scraper_row_counts <- data.frame(
    accreditor = names(scraper_results),
    scrape_row_count = vapply(scraper_results, nrow, integer(1)),
    stringsAsFactors = FALSE
  )

  fetch_summary <- if (nrow(fetch_telemetry) == 0L) {
    scraper_row_counts |>
      dplyr::mutate(
        row_type = "fetch_summary",
        action_type = NA_character_,
        action_status = NA_character_,
        n = NA_integer_,
        resource_type = NA_character_,
        fetch_total_count = 0L,
        fetch_fresh_count = 0L,
        fetch_cache_read_count = 0L,
        fetch_cache_fallback_count = 0L,
        fetch_cache_age_min_days = NA_real_,
        fetch_cache_age_max_days = NA_real_
      ) |>
      dplyr::select(
        row_type,
        accreditor,
        action_type,
        action_status,
        n,
        resource_type,
        fetch_total_count,
        fetch_fresh_count,
        fetch_cache_read_count,
        fetch_cache_fallback_count,
        fetch_cache_age_min_days,
        fetch_cache_age_max_days,
        scrape_row_count
      )
  } else {
    fetch_telemetry |>
      dplyr::mutate(
        cache_age_days = suppressWarnings(as.numeric(cache_age_days))
      ) |>
      dplyr::group_by(accreditor, resource_type) |>
      dplyr::summarise(
        fetch_total_count = dplyr::n(),
        fetch_fresh_count = sum(outcome == "fresh_fetch", na.rm = TRUE),
        fetch_cache_read_count = sum(outcome == "cache_read", na.rm = TRUE),
        fetch_cache_fallback_count = sum(outcome == "cache_fallback", na.rm = TRUE),
        fetch_cache_age_min_days = {
          vals <- cache_age_days[!is.na(cache_age_days) & outcome %in% c("cache_read", "cache_fallback")]
          if (length(vals) == 0L) NA_real_ else min(vals)
        },
        fetch_cache_age_max_days = {
          vals <- cache_age_days[!is.na(cache_age_days) & outcome %in% c("cache_read", "cache_fallback")]
          if (length(vals) == 0L) NA_real_ else max(vals)
        },
        .groups = "drop"
      ) |>
      dplyr::left_join(scraper_row_counts, by = "accreditor") |>
      dplyr::mutate(
        row_type = "fetch_summary",
        action_type = NA_character_,
        action_status = NA_character_,
        n = NA_integer_
      ) |>
      dplyr::select(
        row_type,
        accreditor,
        action_type,
        action_status,
        n,
        resource_type,
        fetch_total_count,
        fetch_fresh_count,
        fetch_cache_read_count,
        fetch_cache_fallback_count,
        fetch_cache_age_min_days,
        fetch_cache_age_max_days,
        scrape_row_count
      )
  }

  source_coverage <- dplyr::bind_rows(action_counts, fetch_summary)

  # -----------------------------------------------------------------------
  # PREPARE OUTPUT FILE PATHS
  outputs <- list(
    actions = paste0(output_prefix, "_actions_joined.csv"),
    summary = paste0(output_prefix, "_institution_summary.csv"),
    current = paste0(output_prefix, "_current_status.csv"),
    unmatched = paste0(output_prefix, "_unmatched_for_review.csv"),
    coverage = paste0(output_prefix, "_source_coverage.csv"),
    workbook = paste0(output_prefix, "_workbook.xlsx")
  )

  if (nrow(actions_joined) == 0 || nrow(institution_summary) == 0) {
    stop("Accreditation refresh produced empty outputs; existing published files were left unchanged.")
  }

  # Priority 4 + 6: Compare fresh scrape row counts against prior run to
  # catch silent scraper regressions (already defined in accreditation_scrapers.R).
  warn_if_scrape_count_dropped(
    raw_actions,
    outputs$actions,
    fail = in_ci && !allow_partial_accreditation
  )
  # Finer-grained check: if a specific (accreditor, action_type) pair drops
  # from non-trivial-prior to zero, flag it. Catches sub-page scraper breakage
  # that the per-accreditor aggregate check can miss.
  warn_if_action_type_dropped(
    raw_actions,
    outputs$actions,
    fail = in_ci && !allow_partial_accreditation
  )

  # Priority 6: Log per-accreditor action counts for monitoring.
  accreditor_counts <- sort(table(raw_actions$accreditor), decreasing = TRUE)
  message("Actions scraped per accreditor:")
  for (nm in names(accreditor_counts)) {
    message(sprintf("  %-10s %d rows", nm, accreditor_counts[[nm]]))
  }
  if (exists("fetch_summary") && nrow(fetch_summary) > 0L) {
    message("Fetch/cache summary by accreditor:")
    for (i in seq_len(nrow(fetch_summary))) {
      row <- fetch_summary[i, , drop = FALSE]
      message(sprintf(
        "  %-10s %-6s fresh=%d cache=%d fallback=%d rows=%d%s",
        row$accreditor[[1]],
        ifelse(is.na(row$resource_type[[1]]), "all", row$resource_type[[1]]),
        as.integer(row$fetch_fresh_count[[1]] %||% 0L),
        as.integer(row$fetch_cache_read_count[[1]] %||% 0L),
        as.integer(row$fetch_cache_fallback_count[[1]] %||% 0L),
        as.integer(row$scrape_row_count[[1]] %||% 0L),
        if (!is.na(row$fetch_cache_age_max_days[[1]])) {
          sprintf(" cache-age %.1f-%.1f days", row$fetch_cache_age_min_days[[1]], row$fetch_cache_age_max_days[[1]])
        } else {
          ""
        }
      ))
    }
  }

  # -----------------------------------------------------------------------
  # WRITE CSV OUTPUTS
  write_csv_atomic(actions_joined, outputs$actions)
  write_csv_atomic(institution_summary, outputs$summary)
  write_csv_atomic(current_status, outputs$current)
  write_csv_atomic(unmatched_for_review, outputs$unmatched)
  write_csv_atomic(source_coverage, outputs$coverage)

  # -----------------------------------------------------------------------
  # WRITE EXCEL WORKBOOK
  workbook_frames <- list(
    actions = actions_joined,
    summary = institution_summary,
    current = current_status,
    unmatched = unmatched_for_review,
    coverage = source_coverage,
    notes = data.frame(
      notes = c(
        "This first version is intentionally partial rather than pretending to be comprehensive.",
        "Covered accreditors in the current scraper: MSCHE current sanctions page, HLC current public disclosure notices, SACSCOC latest public action/disclosure pages with explicit sanction language, NECHE recent commission actions, WSCUC commission action posts, and NWCCU institutional directory pages (4-year institutions, adverse-keyword filter).",
        "Still not covered in this build: ACCJC (community colleges). NWCCU is now covered via institutional directory page scraping.",
        "HLC uses Notice as a public sanction. Because that is not literally named Warning, the file includes both accreditation_warning and accreditation_warning_or_notice.",
        "WSCUC commission action posts include many routine reaffirmation and report items. This scraper intentionally keeps only action headings with warning, probation, notice, show cause, withdrawal, closure, or adverse language.",
        "Unmatched institutions are often schools outside the four-year tracker scope or schools whose public accreditor name does not exactly match IPEDS naming. Review the unmatched sheet before publication."
      ),
      stringsAsFactors = FALSE
    )
  )
  workbook_payloads <- lapply(names(workbook_frames), function(sheet_name) {
    sanitize_excel_workbook_frame(workbook_frames[[sheet_name]], sheet_name)
  })
  names(workbook_payloads) <- names(workbook_frames)
  workbook_truncations <- dplyr::bind_rows(lapply(workbook_payloads, `[[`, "truncations"))
  if (nrow(workbook_truncations) > 0L) {
    total_cells <- sum(workbook_truncations$count, na.rm = TRUE)
    message(sprintf(
      "Workbook text truncated for Excel cell limit: %d cell(s) across %d sheet/column combination(s).",
      total_cells,
      nrow(workbook_truncations)
    ))
    apply(workbook_truncations, 1L, function(row) {
      message(sprintf(
        "  %s.%s: %s cell(s) truncated (max original length %s chars)",
        row[["sheet"]],
        row[["column"]],
        row[["count"]],
        row[["max_original_chars"]]
      ))
      NULL
    })
  }

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "actions")
  openxlsx::writeData(wb, "actions", workbook_payloads$actions$data)
  openxlsx::addWorksheet(wb, "summary")
  openxlsx::writeData(wb, "summary", workbook_payloads$summary$data)
  openxlsx::addWorksheet(wb, "current")
  openxlsx::writeData(wb, "current", workbook_payloads$current$data)
  openxlsx::addWorksheet(wb, "unmatched")
  openxlsx::writeData(wb, "unmatched", workbook_payloads$unmatched$data)
  openxlsx::addWorksheet(wb, "coverage")
  openxlsx::writeData(wb, "coverage", workbook_payloads$coverage$data)
  openxlsx::addWorksheet(wb, "notes")
  openxlsx::writeData(wb, "notes", workbook_payloads$notes$data)
  workbook_tmp <- paste0(outputs$workbook, ".tmp")
  if (file.exists(workbook_tmp)) file.remove(workbook_tmp)
  openxlsx::saveWorkbook(wb, workbook_tmp, overwrite = TRUE)
  if (file.exists(outputs$workbook)) file.remove(outputs$workbook)
  file.rename(workbook_tmp, outputs$workbook)

  # -----------------------------------------------------------------------
  # LOG COMPLETION
  message("Saved:")
  message(" - ", outputs$actions)
  message(" - ", outputs$summary)
  message(" - ", outputs$current)
  message(" - ", outputs$unmatched)
  message(" - ", outputs$coverage)
  message(" - ", outputs$workbook)
}

if (sys.nframe() == 0) {
  main()
}
