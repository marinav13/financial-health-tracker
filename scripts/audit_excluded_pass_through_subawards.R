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

  ensure_packages(c("dplyr", "readr", "stringr", "purrr", "httr", "jsonlite", "tibble"))

  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

  write_csv_atomic <- function(df, path) {
    tmp <- paste0(path, ".tmp")
    on.exit(if (file.exists(tmp)) file.remove(tmp), add = TRUE)
    readr::write_csv(df, tmp, na = "")
    file.rename(tmp, path)
  }

  excluded_input <- get_arg_value(
    "--excluded-input",
    file.path(getwd(), "grant_witness", "grant_witness_excluded_pass_through_grants.csv")
  )
  output_dir <- get_arg_value(
    "--output-dir",
    file.path(getwd(), "grant_witness", "analysis")
  )
  cache_dir <- get_arg_value(
    "--cache-dir",
    file.path(getwd(), "grant_witness", "cache", "usaspending_subaward_audit")
  )
  pause_seconds <- suppressWarnings(as.numeric(get_arg_value("--pause-seconds", "0.05")))

  if (!file.exists(excluded_input)) {
    stop("Excluded grant file not found: ", excluded_input)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(cache_dir, "details"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(cache_dir, "subawards"), recursive = TRUE, showWarnings = FALSE)

  parse_date_safe <- function(x) {
    x <- as.character(x)
    x[!nzchar(x)] <- NA_character_
    as.Date(x)
  }

  safe_num <- function(x) suppressWarnings(as.numeric(x))

  normalize_name <- function(x) {
    x |>
      as.character() |>
      stringr::str_to_lower() |>
      stringr::str_replace_all("&", " and ") |>
      stringr::str_replace_all("[^a-z0-9 ]", " ") |>
      stringr::str_squish()
  }

  is_higher_ed_recipient_one <- function(name) {
    norm <- normalize_name(name)
    if (is.na(norm) || !nzchar(norm)) return(NA)

    special_exact <- c(
      "the administrators of tulane educational fund",
      "regents of the university of california",
      "regents of the university of michigan",
      "president and fellows of harvard college",
      "trustees of boston university",
      "board of trustees of the leland stanford junior university",
      "the trustees of columbia university in the city of new york",
      "the trustees of princeton university"
    )

    if (norm %in% special_exact) return(TRUE)

    higher_ed_patterns <- c(
      "\\buniversity\\b",
      "\\bcollege\\b",
      "\\bcommunity college\\b",
      "\\bpolytechnic\\b",
      "\\bschool of medicine\\b",
      "\\bmedical college\\b",
      "\\bmedical school\\b",
      "\\bstate university\\b",
      "\\binstitute of technology\\b",
      "\\bhealth sciences center\\b"
    )

    any(vapply(higher_ed_patterns, function(p) stringr::str_detect(norm, p), logical(1)))
  }

  is_higher_ed_recipient <- function(name) {
    vapply(name, is_higher_ed_recipient_one, logical(1))
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

        if (!is.null(resp) && !httr::http_error(resp)) break

        if (!is.null(resp)) {
          last_error <- sprintf("USAspending request failed [%s] %s", httr::status_code(resp), url)
        }

        if (attempt < retries) Sys.sleep(max(0.25, pause_seconds) * attempt)
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
    cache_path <- file.path(cache_dir, "details", paste0(gsub("[^A-Za-z0-9_\\-]", "_", award_id_string), ".json"))
    url <- sprintf("https://api.usaspending.gov/api/v2/awards/%s/", utils::URLencode(award_id_string, reserved = TRUE))
    fetch_json_cached(cache_path, verb = "GET", url = url, pause_seconds = pause_seconds)
  }

  fetch_subawards <- function(award_id_string, internal_award_id) {
    page <- 1L
    parts <- list()
    repeat {
      cache_path <- file.path(
        cache_dir,
        "subawards",
        paste0(gsub("[^A-Za-z0-9_\\-]", "_", award_id_string), "_page_", page, ".json")
      )
      payload <- list(
        award_id = internal_award_id,
        page = page,
        limit = 100,
        sort = "action_date",
        order = "desc"
      )
      parsed <- fetch_json_cached(
        cache_path,
        verb = "POST",
        url = "https://api.usaspending.gov/api/v2/subawards/",
        body = payload,
        pause_seconds = pause_seconds
      )

      rows <- parsed$results
      if (is.null(rows) || length(rows) == 0) {
        if (page == 1L) return(tibble::tibble())
        break
      }

      parts[[length(parts) + 1L]] <- tibble::as_tibble(rows)
      page_meta <- parsed$page_metadata
      next_page <- page_meta[["next"]] %||% NA
      has_next <- isTRUE(page_meta[["hasNext"]]) || (!is.null(next_page) && !is.na(next_page))
      if (!has_next) break
      page <- page + 1L
    }

    dplyr::bind_rows(parts)
  }

  excluded <- readr::read_csv(excluded_input, show_col_types = FALSE) |>
    dplyr::mutate(
      award_remaining = safe_num(award_remaining),
      award_id_string = stringr::str_match(source_url, "award/([^/?#]+)")[, 2]
    )

  audit_one <- function(row) {
    award_id_string <- row$award_id_string[[1]]
    base <- tibble::tibble(
      organization_name_display = row$organization_name_display[[1]],
      agency = row$agency[[1]],
      grant_id = row$grant_id[[1]],
      award_id_string = award_id_string,
      award_remaining = row$award_remaining[[1]]
    )

    detail <- tryCatch(fetch_award_detail(award_id_string), error = function(e) e)
    if (inherits(detail, "error")) {
      return(list(
        award = base |>
          dplyr::mutate(
            internal_award_id = NA_integer_,
            subaward_count = NA_integer_,
            total_subaward_amount = NA_real_,
            recipient_mix = "api_error",
            api_error = conditionMessage(detail)
          ),
        recipients = tibble::tibble()
      ))
    }

    internal_award_id <- detail$id %||% NA_integer_
    subaward_count <- detail$subaward_count %||% NA_integer_
    total_subaward_amount <- safe_num(detail$total_subaward_amount %||% NA_real_)

    subawards <- tryCatch(fetch_subawards(award_id_string, internal_award_id), error = function(e) e)
    if (inherits(subawards, "error")) {
      return(list(
        award = base |>
          dplyr::mutate(
            internal_award_id = internal_award_id,
            subaward_count = subaward_count,
            total_subaward_amount = total_subaward_amount,
            recipient_mix = "api_error",
            api_error = conditionMessage(subawards)
          ),
        recipients = tibble::tibble()
      ))
    }

    if (nrow(subawards) == 0) {
      return(list(
        award = base |>
          dplyr::mutate(
            internal_award_id = internal_award_id,
            subaward_count = subaward_count,
            total_subaward_amount = total_subaward_amount,
            recipient_mix = "no_subawards",
            api_error = NA_character_
          ),
        recipients = tibble::tibble()
      ))
    }

    recipients <- subawards |>
      dplyr::transmute(
        organization_name_display = row$organization_name_display[[1]],
        agency = row$agency[[1]],
        grant_id = row$grant_id[[1]],
        award_id_string = award_id_string,
        subaward_number = subaward_number,
        action_date = parse_date_safe(action_date),
        amount = safe_num(amount),
        recipient_name = as.character(recipient_name),
        recipient_class = dplyr::case_when(
          is_higher_ed_recipient(recipient_name) ~ "higher_ed",
          !is_higher_ed_recipient(recipient_name) ~ "non_higher_ed",
          TRUE ~ "unknown"
        ),
        description = as.character(description)
      )

    recipient_rollup <- recipients |>
      dplyr::group_by(recipient_name, recipient_class) |>
      dplyr::summarise(
        subaward_rows = dplyr::n(),
        subaward_amount = sum(amount, na.rm = TRUE),
        .groups = "drop"
      )

    classes <- unique(recipient_rollup$recipient_class)
    recipient_mix <- dplyr::case_when(
      all(classes == "higher_ed") ~ "higher_ed_only",
      all(classes == "non_higher_ed") ~ "non_higher_ed_only",
      all(classes == "unknown") ~ "unknown_only",
      "higher_ed" %in% classes & "non_higher_ed" %in% classes ~ "mixed",
      TRUE ~ "mixed_or_unknown"
    )

    award_row <- base |>
      dplyr::mutate(
        internal_award_id = internal_award_id,
        subaward_count = subaward_count,
        total_subaward_amount = total_subaward_amount,
        unique_recipients = nrow(recipient_rollup),
        higher_ed_recipients = sum(recipient_rollup$recipient_class == "higher_ed"),
        non_higher_ed_recipients = sum(recipient_rollup$recipient_class == "non_higher_ed"),
        unknown_recipients = sum(recipient_rollup$recipient_class == "unknown"),
        higher_ed_subaward_amount = sum(recipient_rollup$subaward_amount[recipient_rollup$recipient_class == "higher_ed"], na.rm = TRUE),
        non_higher_ed_subaward_amount = sum(recipient_rollup$subaward_amount[recipient_rollup$recipient_class == "non_higher_ed"], na.rm = TRUE),
        recipient_mix = recipient_mix,
        api_error = NA_character_
      )

    list(award = award_row, recipients = recipient_rollup |>
      dplyr::mutate(
        award_id_string = award_id_string,
        prime_recipient = row$organization_name_display[[1]],
        agency = row$agency[[1]],
        grant_id = row$grant_id[[1]],
        award_remaining = row$award_remaining[[1]]
      ) |>
      dplyr::select(prime_recipient, agency, grant_id, award_id_string, recipient_name, recipient_class, subaward_rows, subaward_amount, award_remaining))
  }

  audit_results <- purrr::map(seq_len(nrow(excluded)), function(i) {
    if (i %% 5 == 0) cat(sprintf("Audited %s / %s excluded awards\n", i, nrow(excluded)))
    audit_one(excluded[i, , drop = FALSE])
  })

  award_audit <- dplyr::bind_rows(purrr::map(audit_results, "award"))
  recipient_audit <- dplyr::bind_rows(purrr::map(audit_results, "recipients"))
  summary <- award_audit |>
    dplyr::count(recipient_mix, name = "awards") |>
    dplyr::left_join(
      award_audit |>
        dplyr::group_by(recipient_mix) |>
        dplyr::summarise(
          award_remaining = sum(award_remaining, na.rm = TRUE),
          total_subaward_amount = sum(total_subaward_amount, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "recipient_mix"
    ) |>
    dplyr::arrange(dplyr::desc(award_remaining))

  write_csv_atomic(award_audit, file.path(output_dir, "grant_witness_excluded_pass_through_award_audit.csv"))
  write_csv_atomic(recipient_audit, file.path(output_dir, "grant_witness_excluded_pass_through_recipient_audit.csv"))
  write_csv_atomic(summary, file.path(output_dir, "grant_witness_excluded_pass_through_summary.csv"))

  cat(sprintf("Saved award audit to %s\n", file.path(output_dir, "grant_witness_excluded_pass_through_award_audit.csv")))
  cat(sprintf("Saved recipient audit to %s\n", file.path(output_dir, "grant_witness_excluded_pass_through_recipient_audit.csv")))
  cat(sprintf("Saved summary to %s\n", file.path(output_dir, "grant_witness_excluded_pass_through_summary.csv")))
}

if (sys.nframe() == 0) {
  main()
}
