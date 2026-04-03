main <- function(cli_args = NULL) {
  args <- if (is.null(cli_args)) commandArgs(trailingOnly = TRUE) else cli_args

  get_arg_value <- function(flag, default = NULL) {
    idx <- match(flag, args)
    if (!is.na(idx) && idx < length(args)) args[[idx + 1]] else default
  }

  ensure_packages <- function(pkgs) {
    missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing) > 0) {
      install.packages(missing, repos = "https://cloud.r-project.org")
    }
    invisible(lapply(pkgs, library, character.only = TRUE))
  }

  ensure_packages(c("dplyr", "readr", "tidyr", "stringr", "purrr", "httr", "jsonlite"))

  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

  write_csv_atomic <- function(df, path) {
    tmp <- paste0(path, ".tmp")
    on.exit(if (file.exists(tmp)) file.remove(tmp), add = TRUE)
    readr::write_csv(df, tmp, na = "")
    file.rename(tmp, path)
  }

  grant_input <- get_arg_value(
    "--grant-input",
    file.path(getwd(), "grant_witness", "grant_witness_grant_level_joined.csv")
  )
  output_dir <- get_arg_value(
    "--output-dir",
    file.path(getwd(), "grant_witness", "analysis")
  )
  cache_dir <- get_arg_value(
    "--cache-dir",
    file.path(getwd(), "grant_witness", "cache", "usaspending_sensitivity")
  )
  limit_awards <- suppressWarnings(as.integer(get_arg_value("--limit-awards", NA_character_)))
  pause_seconds <- suppressWarnings(as.numeric(get_arg_value("--pause-seconds", "0.05")))

  if (!file.exists(grant_input)) {
    stop("Grant input file not found: ", grant_input)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(cache_dir, "details"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(cache_dir, "transactions"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(cache_dir, "funding"), recursive = TRUE, showWarnings = FALSE)

  safe_num <- function(x) suppressWarnings(as.numeric(x))

  parse_date_safe <- function(x) {
    x <- as.character(x)
    x[!nzchar(x)] <- NA_character_
    as.Date(x)
  }

  period_end_date_from_fiscal <- function(fy, fm) {
    fy <- as.integer(fy)
    fm <- as.integer(fm)
    cal_month <- ((fm + 8) %% 12) + 1
    cal_year <- ifelse(cal_month >= 10, fy - 1L, fy)
    next_month_year <- ifelse(cal_month == 12L, cal_year + 1L, cal_year)
    next_month <- ifelse(cal_month == 12L, 1L, cal_month + 1L)
    as.Date(sprintf("%04d-%02d-01", next_month_year, next_month)) - 1
  }

  quantile_lookup <- function(values, probs) {
    if (length(values) == 0) {
      stats::setNames(rep(NA_real_, length(probs)), paste0("p", probs * 100))
    } else {
      stats::quantile(values, probs = probs, na.rm = TRUE, names = FALSE, type = 7) |>
        stats::setNames(paste0("p", probs * 100))
    }
  }

  build_distribution_row <- function(metric_name, values) {
    cuts <- quantile_lookup(values, c(0.5, 0.75, 0.9))
    tibble::tibble(
      metric = metric_name,
      positive_count = length(values),
      p50 = unname(cuts[["p50"]]),
      p75 = unname(cuts[["p75"]]),
      p90 = unname(cuts[["p90"]])
    )
  }

  ratio_threshold <- function(x, threshold) {
    !is.na(x) & is.finite(x) & x >= threshold
  }

  amount_or_share_threshold <- function(amount, remaining, amount_threshold, share_threshold) {
    amount_hit <- !is.na(amount) & amount >= amount_threshold
    share_hit <- !is.na(amount) & !is.na(remaining) & remaining > 0 & amount >= (share_threshold * remaining)
    amount_hit | share_hit
  }

  fetch_json_cached <- function(path, verb = c("GET", "POST"), url, body = NULL, pause_seconds = 0, retries = 5) {
    verb <- match.arg(verb)
    if (!file.exists(path)) {
      resp <- NULL
      last_error <- NULL

      for (attempt in seq_len(retries)) {
        resp <- tryCatch(
          {
            if (verb == "GET") {
              httr::GET(url, httr::user_agent("financial-health-tracker/1.0"))
            } else {
              httr::POST(
                url,
                body = body,
                encode = "json",
                httr::content_type_json(),
                httr::user_agent("financial-health-tracker/1.0")
              )
            }
          },
          error = function(e) {
            last_error <<- conditionMessage(e)
            NULL
          }
        )

        if (!is.null(resp) && !httr::http_error(resp)) {
          break
        }

        if (!is.null(resp)) {
          last_error <- sprintf("USAspending request failed [%s] %s", httr::status_code(resp), url)
        }

        if (attempt < retries) {
          Sys.sleep(max(0.25, pause_seconds) * attempt)
        }
      }

      if (is.null(resp) || httr::http_error(resp)) {
        stop(last_error %||% sprintf("USAspending request failed for %s", url))
      }

      text <- httr::content(resp, as = "text", encoding = "UTF-8")
      writeLines(text, path, useBytes = TRUE)
      if (pause_seconds > 0) Sys.sleep(pause_seconds)
    }

    jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  }

  fetch_award_detail <- function(award_id_string, pause_seconds = 0) {
    cache_path <- file.path(cache_dir, "details", paste0(gsub("[^A-Za-z0-9_\\-]", "_", award_id_string), ".json"))
    url <- sprintf("https://api.usaspending.gov/api/v2/awards/%s/", utils::URLencode(award_id_string, reserved = TRUE))
    fetch_json_cached(cache_path, verb = "GET", url = url, pause_seconds = pause_seconds)
  }

  fetch_paged_endpoint <- function(subdir, award_id_string, internal_award_id, endpoint, sort_field, pause_seconds = 0) {
    page <- 1L
    parts <- list()

    repeat {
      cache_path <- file.path(
        cache_dir,
        subdir,
        paste0(gsub("[^A-Za-z0-9_\\-]", "_", award_id_string), "_page_", page, ".json")
      )

      payload <- list(
        award_id = internal_award_id,
        page = page,
        limit = 100,
        sort = sort_field,
        order = "desc"
      )

      parsed <- fetch_json_cached(
        cache_path,
        verb = "POST",
        url = endpoint,
        body = payload,
        pause_seconds = pause_seconds
      )

      result_rows <- parsed$results
      if (is.null(result_rows) || length(result_rows) == 0) {
        if (page == 1L) {
          return(tibble::tibble())
        }
        break
      }

      if (!is.data.frame(result_rows)) {
        result_rows <- tibble::as_tibble(result_rows)
      } else {
        result_rows <- tibble::as_tibble(result_rows)
      }

      parts[[length(parts) + 1L]] <- result_rows

      page_metadata <- parsed$page_metadata
      next_page <- page_metadata[["next"]] %||% NA
      has_next <- isTRUE(page_metadata[["hasNext"]]) ||
        (!is.null(next_page) && !is.na(next_page))
      if (!has_next) break
      page <- page + 1L
    }

    dplyr::bind_rows(parts)
  }

  grants <- readr::read_csv(grant_input, show_col_types = FALSE) |>
    dplyr::mutate(
      currently_disrupted = as.logical(currently_disrupted),
      likely_higher_ed = as.logical(likely_higher_ed),
      termination_date = parse_date_safe(termination_date),
      award_outlaid = safe_num(award_outlaid),
      award_remaining = safe_num(award_remaining),
      award_value = safe_num(award_value),
      award_id_string = stringr::str_match(source_url, "award/([^/?#]+)")[, 2]
    ) |>
    dplyr::filter(
      currently_disrupted,
      likely_higher_ed,
      !is.na(award_id_string),
      !is.na(termination_date)
    ) |>
    dplyr::distinct(award_id_string, .keep_all = TRUE) |>
    dplyr::arrange(dplyr::desc(award_remaining), tracker_institution_name, project_title)

  if (!is.na(limit_awards) && limit_awards > 0) {
    grants <- dplyr::slice_head(grants, n = limit_awards)
  }

  if (nrow(grants) == 0) {
    stop("No eligible currently disrupted higher-ed grants with USAspending award ids were found.")
  }

  analyze_one_award <- function(row) {
    award_id_string <- row$award_id_string[[1]]
    error_stub <- tibble::tibble(
      award_id_string = award_id_string,
      usaspending_internal_award_id = NA_integer_,
      usaspending_live_outlay = NA_real_,
      usaspending_total_obligation = NA_real_,
      usaspending_period_end_date = as.Date(NA),
      usaspending_last_modified_date = as.Date(NA),
      period_extends_past_termination = NA,
      period_into_recent = NA,
      period_recent_or_future = NA,
      live_outlay_minus_gw_outlay = NA_real_,
      live_outlay_ratio_to_gw_outlay = NA_real_,
      post_termination_outlays_total = NA_real_,
      post_termination_outlays_jul_dec_2025 = NA_real_,
      post_termination_outlays_2026 = NA_real_,
      post_termination_positive_cont_rev_total = NA_real_,
      post_termination_positive_cont_rev_jul_dec_2025 = NA_real_,
      post_termination_positive_cont_rev_2026 = NA_real_,
      transaction_rows = NA_integer_,
      funding_rows = NA_integer_,
      api_error = NA_character_
    )

    detail <- tryCatch(
      fetch_award_detail(award_id_string, pause_seconds = pause_seconds),
      error = function(e) {
        error_stub$api_error <- paste("detail:", conditionMessage(e))
        error_stub
      }
    )

    if (is.data.frame(detail)) {
      return(detail)
    }

    internal_award_id <- detail$id %||% NA_integer_

    if (is.na(internal_award_id)) {
      error_stub$api_error <- "detail: missing internal award id"
      return(error_stub)
    }

    transactions <- tryCatch(
      fetch_paged_endpoint(
        subdir = "transactions",
        award_id_string = award_id_string,
        internal_award_id = internal_award_id,
        endpoint = "https://api.usaspending.gov/api/v2/transactions/",
        sort_field = "action_date",
        pause_seconds = pause_seconds
      ),
      error = function(e) {
        error_stub$api_error <- paste("transactions:", conditionMessage(e))
        tibble::tibble()
      }
    )

    funding <- tryCatch(
      fetch_paged_endpoint(
        subdir = "funding",
        award_id_string = award_id_string,
        internal_award_id = internal_award_id,
        endpoint = "https://api.usaspending.gov/api/v2/awards/funding/",
        sort_field = "reporting_fiscal_date",
        pause_seconds = pause_seconds
      ),
      error = function(e) {
        if (is.na(error_stub$api_error)) {
          error_stub$api_error <- paste("funding:", conditionMessage(e))
        } else {
          error_stub$api_error <- paste(error_stub$api_error, paste("funding:", conditionMessage(e)), sep = "; ")
        }
        tibble::tibble()
      }
    )

    termination_date <- row$termination_date[[1]]
    gw_outlay <- row$award_outlaid[[1]]
    gw_remaining <- row$award_remaining[[1]]

    if (nrow(transactions) == 0) {
      cont_rev_summary <- tibble::tibble(
        post_termination_positive_cont_rev_total = 0,
        post_termination_positive_cont_rev_jul_dec_2025 = 0,
        post_termination_positive_cont_rev_2026 = 0
      )
    } else {
      transactions <- transactions |>
        dplyr::mutate(
          action_date = parse_date_safe(action_date),
          federal_action_obligation = safe_num(federal_action_obligation),
          action_type_description = toupper(as.character(action_type_description))
        )

      positive_cont_rev <- transactions |>
        dplyr::filter(
          !is.na(action_date),
          action_date > termination_date,
          !is.na(federal_action_obligation),
          federal_action_obligation > 0,
          action_type_description %in% c("CONTINUATION", "REVISION")
        )

      cont_rev_summary <- positive_cont_rev |>
        dplyr::summarise(
          post_termination_positive_cont_rev_total = sum(federal_action_obligation, na.rm = TRUE),
          post_termination_positive_cont_rev_jul_dec_2025 = sum(
            dplyr::if_else(
              action_date >= as.Date("2025-07-01") & action_date <= as.Date("2025-12-31"),
              federal_action_obligation,
              0
            ),
            na.rm = TRUE
          ),
          post_termination_positive_cont_rev_2026 = sum(
            dplyr::if_else(format(action_date, "%Y") == "2026", federal_action_obligation, 0),
            na.rm = TRUE
          )
        )
    }

    if (nrow(funding) == 0) {
      outlay_summary <- tibble::tibble(
        post_termination_outlays_total = 0,
        post_termination_outlays_jul_dec_2025 = 0,
        post_termination_outlays_2026 = 0
      )
    } else {
      funding <- funding |>
        dplyr::mutate(
          reporting_fiscal_year = as.integer(reporting_fiscal_year),
          reporting_fiscal_month = as.integer(reporting_fiscal_month),
          gross_outlay_amount = safe_num(gross_outlay_amount)
        ) |>
        dplyr::filter(
          !is.na(reporting_fiscal_year),
          !is.na(reporting_fiscal_month),
          !is.na(gross_outlay_amount)
        ) |>
        dplyr::mutate(
          period_end_date = period_end_date_from_fiscal(reporting_fiscal_year, reporting_fiscal_month),
          funding_group = paste(
            federal_account,
            disaster_emergency_fund_code,
            object_class,
            program_activity_code,
            sep = "|"
          )
        ) |>
        dplyr::group_by(funding_group, reporting_fiscal_year, reporting_fiscal_month, period_end_date) |>
        dplyr::summarise(gross_outlay_amount = sum(gross_outlay_amount, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(funding_group, reporting_fiscal_year, reporting_fiscal_month) |>
        dplyr::group_by(funding_group, reporting_fiscal_year) |>
        dplyr::mutate(
          prior_outlay = dplyr::lag(gross_outlay_amount, default = 0),
          period_increment = pmax(gross_outlay_amount - prior_outlay, 0)
        ) |>
        dplyr::ungroup()

      outlay_summary <- funding |>
        dplyr::filter(period_end_date > termination_date, !is.na(period_increment), period_increment > 0) |>
        dplyr::summarise(
          post_termination_outlays_total = sum(period_increment, na.rm = TRUE),
          post_termination_outlays_jul_dec_2025 = sum(
            dplyr::if_else(
              period_end_date >= as.Date("2025-07-01") & period_end_date <= as.Date("2025-12-31"),
              period_increment,
              0
            ),
            na.rm = TRUE
          ),
          post_termination_outlays_2026 = sum(
            dplyr::if_else(format(period_end_date, "%Y") == "2026", period_increment, 0),
            na.rm = TRUE
          )
        )
    }

    live_outlay <- safe_num(detail$total_account_outlay %||% detail$total_outlay %||% NA_real_)
    live_obligation <- safe_num(detail$total_obligation %||% detail$total_account_obligation %||% NA_real_)
    pop_end_date <- parse_date_safe(detail$period_of_performance$end_date %||% NA_character_)
    last_modified_date <- parse_date_safe(detail$period_of_performance$last_modified_date %||% NA_character_)

    tibble::tibble(
      award_id_string = award_id_string,
      usaspending_internal_award_id = internal_award_id,
      usaspending_live_outlay = live_outlay,
      usaspending_total_obligation = live_obligation,
      usaspending_period_end_date = pop_end_date,
      usaspending_last_modified_date = last_modified_date,
      period_extends_past_termination = !is.na(pop_end_date) & pop_end_date > termination_date,
      period_into_recent = !is.na(pop_end_date) & pop_end_date >= as.Date("2025-07-01"),
      period_recent_or_future = (!is.na(pop_end_date) & pop_end_date > termination_date) |
        (!is.na(pop_end_date) & pop_end_date >= as.Date("2025-07-01")),
      live_outlay_minus_gw_outlay = live_outlay - gw_outlay,
      live_outlay_ratio_to_gw_outlay = dplyr::case_when(
        is.na(live_outlay) | is.na(gw_outlay) ~ NA_real_,
        gw_outlay > 0 ~ live_outlay / gw_outlay,
        gw_outlay == 0 & live_outlay > 0 ~ Inf,
        gw_outlay == 0 & live_outlay == 0 ~ 1,
        TRUE ~ NA_real_
      ),
      post_termination_outlays_total = outlay_summary$post_termination_outlays_total[[1]],
      post_termination_outlays_jul_dec_2025 = outlay_summary$post_termination_outlays_jul_dec_2025[[1]],
      post_termination_outlays_2026 = outlay_summary$post_termination_outlays_2026[[1]],
      post_termination_positive_cont_rev_total = cont_rev_summary$post_termination_positive_cont_rev_total[[1]],
      post_termination_positive_cont_rev_jul_dec_2025 = cont_rev_summary$post_termination_positive_cont_rev_jul_dec_2025[[1]],
      post_termination_positive_cont_rev_2026 = cont_rev_summary$post_termination_positive_cont_rev_2026[[1]],
      transaction_rows = nrow(transactions),
      funding_rows = nrow(funding),
      api_error = error_stub$api_error[[1]]
    )
  }

  comparison <- purrr::map_dfr(seq_len(nrow(grants)), function(i) {
    if (i %% 50 == 0) {
      cat(sprintf("Processed %s / %s awards\n", i, nrow(grants)))
    }
    analyze_one_award(grants[i, , drop = FALSE])
  })

  comparison <- grants |>
    dplyr::select(
      agency,
      grant_id,
      award_id_string,
      organization_name_display,
      organization_state,
      tracker_institution_name,
      tracker_state,
      tracker_city,
      tracker_control_label,
      matched_unitid,
      award_outlaid,
      award_remaining,
      award_value,
      termination_date,
      original_end_date,
      source_url,
      detail_url
    ) |>
    dplyr::left_join(comparison, by = "award_id_string") |>
    dplyr::mutate(
      institution_key = dplyr::coalesce(
        ifelse(!is.na(matched_unitid), paste0("unitid:", matched_unitid), NA_character_),
        paste0(stringr::str_to_lower(organization_name_display), "|", stringr::str_to_lower(organization_state))
      )
    )

  positive_distributions <- dplyr::bind_rows(
    build_distribution_row(
      "post_termination_outlays_total",
      comparison$post_termination_outlays_total[comparison$post_termination_outlays_total > 0]
    ),
    build_distribution_row(
      "post_termination_positive_cont_rev_total",
      comparison$post_termination_positive_cont_rev_total[comparison$post_termination_positive_cont_rev_total > 0]
    ),
    build_distribution_row(
      "live_outlay_minus_gw_outlay",
      comparison$live_outlay_minus_gw_outlay[comparison$live_outlay_minus_gw_outlay > 0]
    ),
    build_distribution_row(
      "live_outlay_ratio_to_gw_outlay",
      comparison$live_outlay_ratio_to_gw_outlay[
        is.finite(comparison$live_outlay_ratio_to_gw_outlay) &
          comparison$live_outlay_ratio_to_gw_outlay > 1
      ]
    )
  )

  get_cutpoint <- function(metric_name, column_name) {
    value <- positive_distributions |>
      dplyr::filter(metric == metric_name) |>
      dplyr::pull(column_name)
    if (length(value) == 0) NA_real_ else value[[1]]
  }

  empirical_cuts <- list(
    low = list(
      amount = get_cutpoint("post_termination_outlays_total", "p50"),
      cont_rev = get_cutpoint("post_termination_positive_cont_rev_total", "p50"),
      live_delta = get_cutpoint("live_outlay_minus_gw_outlay", "p50"),
      live_ratio = get_cutpoint("live_outlay_ratio_to_gw_outlay", "p50")
    ),
    medium = list(
      amount = get_cutpoint("post_termination_outlays_total", "p75"),
      cont_rev = get_cutpoint("post_termination_positive_cont_rev_total", "p75"),
      live_delta = get_cutpoint("live_outlay_minus_gw_outlay", "p75"),
      live_ratio = get_cutpoint("live_outlay_ratio_to_gw_outlay", "p75")
    ),
    high = list(
      amount = get_cutpoint("post_termination_outlays_total", "p90"),
      cont_rev = get_cutpoint("post_termination_positive_cont_rev_total", "p90"),
      live_delta = get_cutpoint("live_outlay_minus_gw_outlay", "p90"),
      live_ratio = get_cutpoint("live_outlay_ratio_to_gw_outlay", "p90")
    )
  )

  apply_rule <- function(df, name, outlay_amount, outlay_share, cont_rev_amount, cont_rev_share, live_delta, live_ratio) {
    flagged <- df |>
      dplyr::mutate(
        proposal = name,
        activity_signal = amount_or_share_threshold(post_termination_outlays_total, award_remaining, outlay_amount, outlay_share) |
          amount_or_share_threshold(post_termination_positive_cont_rev_total, award_remaining, cont_rev_amount, cont_rev_share),
        materially_higher = (!is.na(live_outlay_minus_gw_outlay) & live_outlay_minus_gw_outlay >= live_delta) |
          ratio_threshold(live_outlay_ratio_to_gw_outlay, live_ratio),
        proposal_flag = dplyr::coalesce(period_recent_or_future, FALSE) & activity_signal & materially_higher
      )

    summary <- flagged |>
      dplyr::filter(proposal_flag) |>
      dplyr::summarise(
        proposal = name,
        grants_flagged = dplyr::n(),
        institutions_affected = dplyr::n_distinct(institution_key),
        excluded_award_remaining = sum(award_remaining, na.rm = TRUE),
        flagged_with_jul_dec_2025_activity = sum(
          (post_termination_outlays_jul_dec_2025 > 0) | (post_termination_positive_cont_rev_jul_dec_2025 > 0),
          na.rm = TRUE
        ),
        flagged_with_2026_activity = sum(
          (post_termination_outlays_2026 > 0) | (post_termination_positive_cont_rev_2026 > 0),
          na.rm = TRUE
        )
      )

    list(flagged = flagged, summary = summary)
  }

  proposal_A <- apply_rule(comparison, "A", 1000, 0.05, 1000, 0.05, 1000, 1.05)
  proposal_B <- apply_rule(comparison, "B", 100000, 0.10, 100000, 0.10, 100000, 1.15)

  apply_empirical_rule <- function(df, label, cuts) {
    flagged <- df |>
      dplyr::mutate(
        proposal = paste0("C_", label),
        activity_signal = (!is.na(post_termination_outlays_total) & post_termination_outlays_total >= cuts$amount) |
          (!is.na(post_termination_positive_cont_rev_total) & post_termination_positive_cont_rev_total >= cuts$cont_rev),
        materially_higher = (!is.na(live_outlay_minus_gw_outlay) & live_outlay_minus_gw_outlay >= cuts$live_delta) |
          ratio_threshold(live_outlay_ratio_to_gw_outlay, cuts$live_ratio),
        proposal_flag = dplyr::coalesce(period_recent_or_future, FALSE) & activity_signal & materially_higher
      )

    summary <- flagged |>
      dplyr::filter(proposal_flag) |>
      dplyr::summarise(
        proposal = paste0("C_", label),
        grants_flagged = dplyr::n(),
        institutions_affected = dplyr::n_distinct(institution_key),
        excluded_award_remaining = sum(award_remaining, na.rm = TRUE),
        flagged_with_jul_dec_2025_activity = sum(
          (post_termination_outlays_jul_dec_2025 > 0) | (post_termination_positive_cont_rev_jul_dec_2025 > 0),
          na.rm = TRUE
        ),
        flagged_with_2026_activity = sum(
          (post_termination_outlays_2026 > 0) | (post_termination_positive_cont_rev_2026 > 0),
          na.rm = TRUE
        )
      )

    list(flagged = flagged, summary = summary)
  }

  proposal_C_low <- apply_empirical_rule(comparison, "low", empirical_cuts$low)
  proposal_C_medium <- apply_empirical_rule(comparison, "medium", empirical_cuts$medium)
  proposal_C_high <- apply_empirical_rule(comparison, "high", empirical_cuts$high)

  proposal_D_flagged <- comparison |>
    dplyr::mutate(
      proposal = "D",
      activity_signal = (post_termination_outlays_total > 0) | (post_termination_positive_cont_rev_total > 0),
      materially_higher = ratio_threshold(live_outlay_ratio_to_gw_outlay, 1.01),
      proposal_flag = dplyr::coalesce(period_recent_or_future, FALSE) & activity_signal & materially_higher
    )

  proposal_D_summary <- proposal_D_flagged |>
    dplyr::filter(proposal_flag) |>
    dplyr::summarise(
      proposal = "D",
      grants_flagged = dplyr::n(),
      institutions_affected = dplyr::n_distinct(institution_key),
      excluded_award_remaining = sum(award_remaining, na.rm = TRUE),
      flagged_with_jul_dec_2025_activity = sum(
        (post_termination_outlays_jul_dec_2025 > 0) | (post_termination_positive_cont_rev_jul_dec_2025 > 0),
        na.rm = TRUE
      ),
      flagged_with_2026_activity = sum(
        (post_termination_outlays_2026 > 0) | (post_termination_positive_cont_rev_2026 > 0),
        na.rm = TRUE
      )
    )

  proposal_summary <- dplyr::bind_rows(
    proposal_A$summary,
    proposal_B$summary,
    proposal_C_low$summary,
    proposal_C_medium$summary,
    proposal_C_high$summary,
    proposal_D_summary
  ) |>
    dplyr::mutate(
      analyzed_grants = nrow(comparison),
      analyzed_institutions = dplyr::n_distinct(comparison$institution_key)
    )

  empirical_thresholds <- tibble::tibble(
    evidence_level = c("low", "medium", "high"),
    outlay_threshold = c(empirical_cuts$low$amount, empirical_cuts$medium$amount, empirical_cuts$high$amount),
    cont_rev_threshold = c(empirical_cuts$low$cont_rev, empirical_cuts$medium$cont_rev, empirical_cuts$high$cont_rev),
    live_delta_threshold = c(empirical_cuts$low$live_delta, empirical_cuts$medium$live_delta, empirical_cuts$high$live_delta),
    live_ratio_threshold = c(empirical_cuts$low$live_ratio, empirical_cuts$medium$live_ratio, empirical_cuts$high$live_ratio)
  )

  write_csv_atomic(comparison, file.path(output_dir, "grant_witness_usaspending_comparison.csv"))
  write_csv_atomic(positive_distributions, file.path(output_dir, "grant_witness_usaspending_positive_distributions.csv"))
  write_csv_atomic(empirical_thresholds, file.path(output_dir, "grant_witness_usaspending_empirical_thresholds.csv"))
  write_csv_atomic(proposal_summary, file.path(output_dir, "grant_witness_usaspending_proposal_summary.csv"))
  write_csv_atomic(
    proposal_A$flagged |> dplyr::filter(proposal_flag),
    file.path(output_dir, "grant_witness_usaspending_flagged_proposal_A.csv")
  )
  write_csv_atomic(
    proposal_B$flagged |> dplyr::filter(proposal_flag),
    file.path(output_dir, "grant_witness_usaspending_flagged_proposal_B.csv")
  )
  write_csv_atomic(
    proposal_C_low$flagged |> dplyr::filter(proposal_flag),
    file.path(output_dir, "grant_witness_usaspending_flagged_proposal_C_low.csv")
  )
  write_csv_atomic(
    proposal_C_medium$flagged |> dplyr::filter(proposal_flag),
    file.path(output_dir, "grant_witness_usaspending_flagged_proposal_C_medium.csv")
  )
  write_csv_atomic(
    proposal_C_high$flagged |> dplyr::filter(proposal_flag),
    file.path(output_dir, "grant_witness_usaspending_flagged_proposal_C_high.csv")
  )
  write_csv_atomic(
    proposal_D_flagged |> dplyr::filter(proposal_flag),
    file.path(output_dir, "grant_witness_usaspending_flagged_proposal_D.csv")
  )

  cat(sprintf("Saved comparison table to %s\n", file.path(output_dir, "grant_witness_usaspending_comparison.csv")))
  cat(sprintf("Saved positive distributions to %s\n", file.path(output_dir, "grant_witness_usaspending_positive_distributions.csv")))
  cat(sprintf("Saved empirical thresholds to %s\n", file.path(output_dir, "grant_witness_usaspending_empirical_thresholds.csv")))
  cat(sprintf("Saved proposal summary to %s\n", file.path(output_dir, "grant_witness_usaspending_proposal_summary.csv")))
}

if (sys.nframe() == 0) {
  main()
}
