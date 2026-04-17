# build_grant_witness_usaspending_sensitivity.R
#
# PURPOSE
# --------
# Cross-reference Grant Witness disrupted grants against the USAspending
# transactions API to identify awards that show positive continuation or
# revision activity after the termination date reported by Grant Witness.
#
# AWARDS WITH SUCH ACTIVITY are treated as "too risky" to keep in the
# disrupted grant totals — money may still be flowing even though Grant
# Witness marked them disrupted.  The filtered award list is consumed by
# build_grant_witness_join.R to exclude those grants from the
# research-funding-cuts page.
#
# PRODUCTION OUTPUT
# -----------------
# data_pipelines/grant_witness/analysis/
#   grant_witness_usaspending_risky_continuation_filter.csv
#
# All other CSVs produced by earlier versions of this script (comparison
# table, distribution statistics, filter summaries) were exploratory artefacts
# and are no longer produced.
#
# CLI ARGUMENTS
# -------------
#   --grant-input     Path to grant-level joined CSV (default: data_pipelines/
#                    grant_witness/grant_witness_grant_level_joined.csv)
#   --output-dir     Output directory (default: data_pipelines/grant_witness/analysis)
#   --cache-dir      API response cache directory (default: data_pipelines/grant_witness/
#                    cache/usaspending_sensitivity)
#   --limit-awards   Limit to the first N awards (for local testing only)
#   --pause-seconds  Seconds between API requests (default: 0.05)
#
# USASPENDING API
# ---------------
# Transactions endpoint: https://api.usaspending.gov/api/v2/transactions/
# Each response is cached locally so reruns reuse the same snapshot and
# do not hit the network for unchanged awards.

main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  source(file.path(getwd(), "scripts", "shared", "usaspending_sensitivity_helpers.R"))
  args          <- parse_cli_args(cli_args)
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)

  ensure_packages(c("dplyr", "readr", "tidyr", "purrr", "httr", "jsonlite"))

  grant_input  <- get_arg_value(
    "--grant-input",
    file.path(getwd(), "data_pipelines", "grant_witness", "grant_witness_grant_level_joined.csv")
  )
  output_dir   <- get_arg_value(
    "--output-dir",
    file.path(getwd(), "data_pipelines", "grant_witness", "analysis")
  )
  cache_dir    <- get_arg_value(
    "--cache-dir",
    file.path(getwd(), "data_pipelines", "grant_witness", "cache", "usaspending_sensitivity")
  )
  pause_seconds <- suppressWarnings(as.numeric(get_arg_value("--pause-seconds", "0.05")))

  if (!file.exists(grant_input)) {
    stop("Grant input file not found: ", grant_input)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(cache_dir, "details"),      recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(cache_dir, "transactions"), recursive = TRUE, showWarnings = FALSE)

  # -------------------------------------------------------------------
  # Network helpers
  # -------------------------------------------------------------------

  fetch_json_cached <- function(path, verb = c("GET", "POST"), url,
                                body = NULL, pause_seconds = 0, retries = 5) {
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
                url, body = body, encode = "json",
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
          last_error <- sprintf("USAspending request failed [%s] %s",
                                httr::status_code(resp), url)
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

  fetch_award_detail <- function(award_id_string) {
    safe_filename <- gsub("[^A-Za-z0-9_\\-]", "_", award_id_string)
    cache_path    <- file.path(cache_dir, "details", paste0(safe_filename, ".json"))
    url           <- sprintf(
      "https://api.usaspending.gov/api/v2/awards/%s/",
      utils::URLencode(award_id_string, reserved = TRUE)
    )
    fetch_json_cached(cache_path, verb = "GET", url = url,
                     pause_seconds = pause_seconds)
  }

  # Fetch all pages of a paged endpoint (transactions), returning one tibble.
  # Each page is cached individually so interrupted runs can resume.
  fetch_paged_endpoint <- function(subdir, award_id_string, internal_award_id,
                                  endpoint, sort_field) {
    page  <- 1L
    parts <- list()

    repeat {
      safe_filename <- gsub("[^A-Za-z0-9_\\-]", "_", award_id_string)
      cache_path    <- file.path(cache_dir, subdir,
                                 paste0(safe_filename, "_page_", page, ".json"))

      payload <- list(
        award_id = internal_award_id,
        page     = page,
        limit    = 100L,
        sort     = sort_field,
        order    = "desc"
      )

      parsed <- fetch_json_cached(
        cache_path, verb = "POST", url = endpoint, body = payload,
        pause_seconds = pause_seconds
      )

      result_rows <- parsed$results
      if (is.null(result_rows) || length(result_rows) == 0L) {
        if (page == 1L) return(tibble::tibble())
        break
      }

      if (!is.data.frame(result_rows)) {
        result_rows <- tibble::as_tibble(result_rows)
      } else {
        result_rows <- tibble::as_tibble(result_rows)
      }

      parts[[length(parts) + 1L]] <- result_rows

      page_metadata <- parsed$page_metadata
      next_page     <- page_metadata[["next"]] %||% NA
      has_next      <- isTRUE(page_metadata[["hasNext"]]) ||
                       (!is.null(next_page) && !is.na(next_page))
      if (!has_next) break
      page <- page + 1L
    }

    dplyr::bind_rows(parts)
  }

  # -------------------------------------------------------------------
  # Per-award analysis
  # -------------------------------------------------------------------

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
      post_termination_positive_cont_rev_total          = NA_real_,
      post_termination_positive_cont_rev_jul_dec_2025 = NA_real_,
      post_termination_positive_cont_rev_2026           = NA_real_,
      transaction_rows = NA_integer_,
      api_error = NA_character_
    )

    detail <- tryCatch(
      fetch_award_detail(award_id_string),
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
        subdir            = "transactions",
        award_id_string  = award_id_string,
        internal_award_id = internal_award_id,
        endpoint          = "https://api.usaspending.gov/api/v2/transactions/",
        sort_field        = "action_date"
      ),
      error = function(e) {
        error_stub$api_error <- paste("transactions:", conditionMessage(e))
        tibble::tibble()
      }
    )

    termination_date <- parse_date_safe(row$termination_date[[1]])
    gw_outlay        <- safe_num(row$award_outlaid[[1]])
    gw_remaining     <- safe_num(row$award_remaining[[1]])

    # Positive post-termination continuations/revisions are the clearest signal
    # that money may still be flowing even though Grant Witness marked the
    # grant disrupted.
    if (nrow(transactions) == 0L) {
      cont_rev_summary <- tibble::tibble(
        post_termination_positive_cont_rev_total          = 0,
        post_termination_positive_cont_rev_jul_dec_2025 = 0,
        post_termination_positive_cont_rev_2026          = 0
      )
    } else {
      transactions <- transactions |>
        dplyr::mutate(
          action_date = parse_date_safe(action_date),
          federal_action_obligation = safe_num(federal_action_obligation),
          action_type_description   = toupper(as.character(action_type_description))
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
              action_date >= as.Date("2025-07-01") &
                action_date <= as.Date("2025-12-31"),
              federal_action_obligation, 0
            ),
            na.rm = TRUE
          ),
          post_termination_positive_cont_rev_2026 = sum(
            dplyr::if_else(
              format(action_date, "%Y") == "2026",
              federal_action_obligation, 0
            ),
            na.rm = TRUE
          )
        )
    }

    live_outlay     <- safe_num(detail$total_account_outlay %||%
                                detail$total_outlay %||% NA_real_)
    live_obligation <- safe_num(detail$total_obligation %||%
                                detail$total_account_obligation %||% NA_real_)
    pop_end_date    <- parse_date_safe(
      detail$period_of_performance$end_date %||% NA_character_
    )
    last_modified   <- parse_date_safe(
      detail$period_of_performance$last_modified_date %||% NA_character_
    )

    tibble::tibble(
      award_id_string = award_id_string,
      usaspending_internal_award_id = internal_award_id,
      usaspending_live_outlay = live_outlay,
      usaspending_total_obligation = live_obligation,
      usaspending_period_end_date = pop_end_date,
      usaspending_last_modified_date = last_modified,
      period_extends_past_termination = !is.na(pop_end_date) & pop_end_date > termination_date,
      period_into_recent = !is.na(pop_end_date) & pop_end_date >= as.Date("2025-07-01"),
      period_recent_or_future = (!is.na(pop_end_date) & pop_end_date > termination_date) |
                                (!is.na(pop_end_date) & pop_end_date >= as.Date("2025-07-01")),
      live_outlay_minus_gw_outlay = live_outlay - gw_outlay,
      live_outlay_ratio_to_gw_outlay = dplyr::case_when(
        is.na(live_outlay) | is.na(gw_outlay)         ~ NA_real_,
        gw_outlay > 0                                  ~ live_outlay / gw_outlay,
        gw_outlay == 0 & live_outlay > 0               ~ Inf,
        gw_outlay == 0 & live_outlay == 0              ~ 1,
        TRUE                                           ~ NA_real_
      ),
      post_termination_positive_cont_rev_total          = cont_rev_summary$post_termination_positive_cont_rev_total[[1]],
      post_termination_positive_cont_rev_jul_dec_2025   = cont_rev_summary$post_termination_positive_cont_rev_jul_dec_2025[[1]],
      post_termination_positive_cont_rev_2026           = cont_rev_summary$post_termination_positive_cont_rev_2026[[1]],
      transaction_rows = nrow(transactions),
      api_error = error_stub$api_error[[1]]
    )
  }

  # -------------------------------------------------------------------
  # Load and filter grants
  # -------------------------------------------------------------------

  grants <- readr::read_csv(grant_input, show_col_types = FALSE) |>
    dplyr::mutate(
      currently_disrupted = as.logical(currently_disrupted),
      likely_higher_ed     = as.logical(likely_higher_ed),
      termination_date     = parse_date_safe(termination_date),
      award_outlaid        = safe_num(award_outlaid),
      award_remaining      = safe_num(award_remaining),
      award_value         = safe_num(award_value),
      award_id_string     = stringr::str_match(source_url, "award/([^/?#]+)")[, 2]
    ) |>
    dplyr::filter(
      currently_disrupted,
      likely_higher_ed,
      !is.na(award_id_string),
      !is.na(termination_date)
    ) |>
    dplyr::distinct(award_id_string, .keep_all = TRUE) |>
    dplyr::arrange(dplyr::desc(award_remaining), tracker_institution_name, project_title)

  limit_awards <- suppressWarnings(as.integer(get_arg_value("--limit-awards", NA_character_)))
  if (!is.na(limit_awards) && limit_awards > 0) {
    grants <- dplyr::slice_head(grants, n = limit_awards)
  }

  if (nrow(grants) == 0) {
    stop("No eligible currently disrupted higher-ed grants with USAspending award ids were found.")
  }

  # -------------------------------------------------------------------
  # Build comparison table
  # -------------------------------------------------------------------

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
        paste0(
          stringr::str_to_lower(organization_name_display), "|",
          stringr::str_to_lower(organization_state)
        )
      )
    )

  # -------------------------------------------------------------------
  # Apply risky-continuation filter
  # -------------------------------------------------------------------

  # If Grant Witness marks a grant disrupted but USAspending shows positive
  # continuation or revision activity after the termination date, treat the
  # grant as too risky to keep in the disrupted totals.
  flagged <- comparison |>
    dplyr::mutate(
      proposal           = "risky_continuation_filter",
      continuation_signal = !is.na(post_termination_positive_cont_rev_total) &
                             post_termination_positive_cont_rev_total > 0,
      proposal_flag      = continuation_signal
    )

  # -------------------------------------------------------------------
  # Write output
  # -------------------------------------------------------------------

  # Only the filtered award list is needed by the interactive.
  # build_grant_witness_join.R reads this file and excludes matching grants.
  write_csv_atomic(
    flagged |> dplyr::filter(proposal_flag),
    file.path(output_dir, "grant_witness_usaspending_risky_continuation_filter.csv")
  )

  cat(sprintf(
    "Saved risky continuation filter to %s\n",
    file.path(output_dir, "grant_witness_usaspending_risky_continuation_filter.csv")
  ))
}

if (sys.nframe() == 0) {
  main()
}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               