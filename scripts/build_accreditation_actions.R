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

  ensure_packages(c("dplyr", "httr2", "openxlsx", "purrr", "readr", "stringr", "tidyr", "xml2"))

    financial_input <- get_arg_value(
      "--financial-input",
      file.path(getwd(), "ipeds", "ipeds_financial_health_dataset_2014_2024.csv")
    )
  output_prefix <- get_arg_value(
    "--output-prefix",
    file.path(getwd(), "accreditation", "accreditation_tracker")
  )
  refresh <- tolower(get_arg_value("--refresh", "true")) %in% c("true", "1", "yes", "y")

  if (!file.exists(financial_input)) {
    stop("Financial input file not found: ", financial_input)
  }

  cache_dir <- file.path(getwd(), "accreditation", "cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(output_prefix), recursive = TRUE, showWarnings = FALSE)

  decode_html <- function(x) {
    x <- as.character(x)
    vapply(
      x,
      function(one) {
        if (is.na(one) || !nzchar(one)) return(NA_character_)
        one <- gsub("<br\\s*/?>", "\n", one, ignore.case = TRUE)
        doc <- xml2::read_html(paste0("<html><body>", one, "</body></html>"))
        xml2::xml_text(xml2::xml_find_first(doc, "//body"))
      },
      character(1)
    )
  }

  clean_text <- function(x) {
    x |>
      decode_html() |>
      stringr::str_replace_all("[\r\n\t]+", " ") |>
      stringr::str_replace_all("\u00a0", " ") |>
      stringr::str_squish()
  }

  normalize_name <- function(x) {
    x |>
      as.character() |>
      stringr::str_to_lower() |>
      stringr::str_replace_all("&", " and ") |>
      stringr::str_replace_all("\\bsaint\\b", "st") |>
      stringr::str_replace_all("[^a-z0-9 ]", " ") |>
      stringr::str_squish()
  }

  state_lookup <- c(
    setNames(state.name, state.abb),
    DC = "District of Columbia",
    PR = "Puerto Rico",
    VI = "Virgin Islands",
    GU = "Guam",
    MP = "Northern Mariana Islands",
    AS = "American Samoa"
  )

  state_name <- function(x) {
    x_chr <- as.character(x)
    out <- unname(state_lookup[toupper(x_chr)])
    out[is.na(out)] <- x_chr[is.na(out)]
    out
  }

  fetch_html_text <- function(url, cache_name, refresh = TRUE) {
    cache_path <- file.path(cache_dir, cache_name)
    if (!refresh && file.exists(cache_path)) {
      return(readr::read_file(cache_path))
    }

    tryCatch(
      {
        resp <- httr2::request(url) |>
          httr2::req_user_agent("FinancialHealthProject/1.0") |>
          httr2::req_perform()
        body <- httr2::resp_body_string(resp)
        readr::write_file(body, cache_path)
        body
      },
      error = function(e) {
        if (file.exists(cache_path)) {
          message("Falling back to cached copy for ", url)
          readr::read_file(cache_path)
        } else {
          stop("Failed to fetch ", url, ": ", e$message)
        }
      }
    )
  }

  extract_page_modified_date <- function(html) {
    match <- stringr::str_match(
      html,
      "article:modified_time\" content=\"([0-9]{4}-[0-9]{2}-[0-9]{2})"
    )
    match[, 2]
  }

  extract_page_title <- function(html) {
    title_match <- stringr::str_match(html, "<title>(.*?)</title>")
    clean_text(title_match[, 2])
  }

  classify_action <- function(raw_action, accreditor = NA_character_) {
    txt <- stringr::str_to_lower(as.character(raw_action))
    out <- dplyr::case_when(
      stringr::str_detect(txt, "removed from warning|remove notice of concern|removal of sanction") ~ "removed",
      stringr::str_detect(txt, "removed from probation|removed from probation for good cause") ~ "removed",
      stringr::str_detect(txt, "show cause") ~ "show_cause",
      stringr::str_detect(txt, "closure") ~ "adverse_action",
      stringr::str_detect(txt, "removed from membership|withdrawal|withdraws from membership|withdraw candidate|withdraw accreditation") ~ "adverse_action",
      stringr::str_detect(txt, "probation") ~ "probation",
      stringr::str_detect(txt, "warning") ~ "warning",
      stringr::str_detect(txt, "notice of concern") ~ "notice",
      stringr::str_detect(txt, "notice") ~ "notice",
      stringr::str_detect(txt, "monitor") ~ "monitoring",
      stringr::str_detect(txt, "adverse") ~ "adverse_action",
      TRUE ~ "other"
    )
    ifelse(is.na(accreditor), out, out)
  }

  classify_status <- function(raw_action) {
    txt <- stringr::str_to_lower(as.character(raw_action))
    dplyr::case_when(
      stringr::str_detect(txt, "removed from warning|removed from probation|removal of sanction|remove notice of concern|removed notice of concern") ~ "resolved",
      stringr::str_detect(txt, "placed on|continued on|on probation|on notice|warning|show cause|monitor") ~ "active",
      stringr::str_detect(txt, "removed from membership|withdrawal|withdraws from membership") ~ "active",
      TRUE ~ "active"
    )
  }

  has_public_action_keywords <- function(x) {
    txt <- stringr::str_to_lower(as.character(x))
    stringr::str_detect(
      txt,
      "warning|probation|show cause|notice of concern|notice|withdrawal of accreditation|withdraws from membership|closure|adverse"
    )
  }

  extract_name_state_from_item <- function(x) {
    item <- clean_text(x)
    item <- stringr::str_remove(item, "\\s*\\((next review|letter dated|closure of|two proposals).*?\\)$")
    item <- stringr::str_squish(item)

    match <- stringr::str_match(item, "^(.*?),\\s*[^,]+,\\s*([A-Z]{2})$")
    if (!is.na(match[1, 1])) {
      return(list(
        institution_name_raw = clean_text(match[1, 2]),
        institution_state_raw = state_name(match[1, 3])
      ))
    }

    match <- stringr::str_match(item, "^(.*?),\\s*([A-Z]{2})$")
    if (!is.na(match[1, 1])) {
      return(list(
        institution_name_raw = clean_text(match[1, 2]),
        institution_state_raw = state_name(match[1, 3])
      ))
    }

    match <- stringr::str_match(item, "^(.*?)\\s*\\(([^)]+)\\)$")
    if (!is.na(match[1, 1])) {
      state_abbr <- stringr::str_match(match[1, 3], "([A-Z]{2})$")[, 2]
      return(list(
        institution_name_raw = clean_text(match[1, 2]),
        institution_state_raw = state_name(state_abbr)
      ))
    }

    list(
      institution_name_raw = item,
      institution_state_raw = NA_character_
    )
  }

  parse_msche <- function() {
    url <- "https://www.msche.org/non-compliance-and-adverse-actions-by-status/"
    html <- fetch_html_text(url, "msche_status.html", refresh = refresh)
    page_title <- extract_page_title(html)
    page_modified <- extract_page_modified_date(html)

    section_matches <- stringr::str_match_all(
      html,
      "(?s)<h3>(Non-Compliance Warning|Non-Compliance Probation|Non-Compliance Show Cause|Adverse Action)</h3>(.*?)(?=<h3>|<div class=\"single-share\"|</article>)"
    )[[1]]

    current_status_rows <- if (nrow(section_matches) == 0) {
      tibble::tibble()
    } else {
      purrr::map_dfr(seq_len(nrow(section_matches)), function(i) {
        heading <- clean_text(section_matches[i, 2])
        body <- section_matches[i, 3]
        if (stringr::str_detect(body, "No institutions in this status")) {
          return(tibble::tibble())
        }

        link_matches <- stringr::str_match_all(body, "<a[^>]*href=\"([^\"]+)\"[^>]*>(.*?)</a>")[[1]]
        if (nrow(link_matches) == 0) {
          return(tibble::tibble())
        }

        action_label <- dplyr::case_when(
          heading == "Non-Compliance Warning" ~ "warning",
          heading == "Non-Compliance Probation" ~ "probation",
          heading == "Non-Compliance Show Cause" ~ "show_cause",
          heading == "Adverse Action" ~ "adverse_action",
          TRUE ~ "other"
        )

        tibble::tibble(
          institution_name_raw = clean_text(link_matches[, 3]),
          institution_state_raw = NA_character_,
          accreditor = "MSCHE",
          action_type = action_label,
          action_label_raw = heading,
          action_status = "active",
          action_date = as.Date(NA),
          action_year = NA_integer_,
          source_url = link_matches[, 2],
          source_title = page_title,
          notes = heading,
          last_seen_at = Sys.time(),
          source_page_url = url,
          source_page_modified = page_modified
        )
      })
    }

    parse_msche_month_page <- function(month_url, month_label) {
      cache_name <- paste0("msche_", gsub("[^a-z0-9]+", "_", tolower(month_label)), ".html")
      month_html <- fetch_html_text(month_url, cache_name, refresh = refresh)
      page_modified_month <- extract_page_modified_date(month_html)
      doc <- xml2::read_html(month_html)

      state_sections <- xml2::xml_find_all(doc, "//h3[starts-with(@id,'state-')]")
      if (length(state_sections) == 0) {
        return(tibble::tibble())
      }

      purrr::map_dfr(state_sections, function(state_node) {
        state_label <- clean_text(xml2::xml_text(state_node))
        list_node <- xml2::xml_find_first(state_node, "following-sibling::*[1][self::ul]")
        if (inherits(list_node, "xml_missing")) {
          return(tibble::tibble())
        }

        item_nodes <- xml2::xml_find_all(list_node, "./li[a[contains(@href, '/institution/')]]")
        if (length(item_nodes) == 0) {
          return(tibble::tibble())
        }

        purrr::map_dfr(item_nodes, function(item_node) {
          inst_link <- xml2::xml_find_first(item_node, ".//a[contains(@href, '/institution/')]")
          inst_name_raw <- clean_text(xml2::xml_text(inst_link))
          inst_name_clean <- stringr::str_remove(inst_name_raw, ",\\s*[A-Z]{2}$")
          date_text <- clean_text(xml2::xml_text(item_node))
          date_match <- stringr::str_match(date_text, "\\((January|February|March|April|May|June|July|August|September|October|November|December)\\s+[0-9]{1,2},\\s+[0-9]{4}\\)")
          entry_date <- suppressWarnings(as.Date(stringr::str_remove_all(date_match[, 1], "[()]"), format = "%B %d, %Y"))

          tibble::tibble(
            institution_name_raw = inst_name_clean,
            institution_state_raw = state_name(state_label),
            accreditor = "MSCHE",
            action_type = "commission_action",
            action_label_raw = "Commission action",
            action_status = NA_character_,
            action_date = entry_date,
            action_year = suppressWarnings(as.integer(format(entry_date, "%Y"))),
            source_url = xml2::xml_attr(inst_link, "href"),
            source_title = paste("MSCHE Statement of Accreditation Status -", inst_name_clean),
            notes = paste("Recent Commission Action:", month_label),
            last_seen_at = Sys.time(),
            source_page_url = month_url,
            source_page_modified = page_modified_month
          )
        })
      })
    }

    parse_msche_recent_actions <- function() {
      years <- seq.int(2017L, max(2017L, as.integer(format(Sys.Date(), "%Y"))))

      month_links <- purrr::map_dfr(years, function(one_year) {
        recent_url <- if (one_year == as.integer(format(Sys.Date(), "%Y"))) {
          "https://www.msche.org/recent-commission-actions/"
        } else {
          paste0("https://www.msche.org/recent-commission-actions/?my=", one_year)
        }

        recent_html <- fetch_html_text(
          recent_url,
          paste0("msche_recent_actions_", one_year, ".html"),
          refresh = refresh
        )

        links <- stringr::str_match_all(
          recent_html,
          "<a href=\"(https://www\\.msche\\.org/commission-actions/\\?fd=[0-9]+(?:&amp;|&)ld=[0-9]+)\">(January|February|March|April|May|June|July|August|September|October|November|December)\\s+([0-9]{4})</a>"
        )[[1]]

        if (nrow(links) == 0) {
          return(tibble::tibble())
        }

        tibble::tibble(
          month_url = gsub("&amp;", "&", links[, 2], fixed = TRUE),
          month_label = paste(links[, 3], links[, 4])
        )
      }) |>
        dplyr::distinct()

      if (nrow(month_links) == 0) {
        return(tibble::tibble())
      }

      purrr::map_dfr(seq_len(nrow(month_links)), function(i) {
        parse_msche_month_page(month_links$month_url[[i]], month_links$month_label[[i]])
      })
    }

    dplyr::bind_rows(current_status_rows, parse_msche_recent_actions()) |>
      dplyr::distinct()
  }

  parse_hlc <- function() {
    url <- "https://www.hlcommission.org/for-students/accreditation-actions/"
    html <- fetch_html_text(url, "hlc_actions.html", refresh = refresh)
    page_title <- extract_page_title(html)
    page_modified <- extract_page_modified_date(html)

    current_notice_block <- stringr::str_match(
      html,
      "(?s)<h3 class=\"wp-block-heading\">Current Public Disclosure Notices</h3>(.*?)(?=<h3 class=\"kt-adv-heading|</main>)"
    )[, 2]

    current_notice_rows <- if (is.na(current_notice_block)) {
      tibble::tibble()
    } else {
      pane_matches <- stringr::str_match_all(
        current_notice_block,
        "(?s)<span class=\"kt-blocks-accordion-title\">(On Notice|On Probation|Removal of Sanction \\(past six months\\)|Withdrawal of Accreditation)</span>.*?<ul class=\"kt-svg-icon-list\">(.*?)</ul>"
      )[[1]]

      if (nrow(pane_matches) == 0) {
        tibble::tibble()
      } else {
        purrr::map_dfr(seq_len(nrow(pane_matches)), function(i) {
          heading <- clean_text(pane_matches[i, 2])
          body <- pane_matches[i, 3]
          li_matches <- stringr::str_match_all(
            body,
            "(?s)<li[^>]*>.*?<a[^>]*href=\"([^\"]+)\"[^>]*>(.*?)</a>\\s*(?:&nbsp;)?\\s*\\(([^)]+)\\).*?</li>"
          )[[1]]

          if (nrow(li_matches) == 0) {
            return(tibble::tibble())
          }

          raw_action <- dplyr::case_when(
            heading == "On Notice" ~ "On Notice",
            heading == "On Probation" ~ "On Probation",
            heading == "Removal of Sanction (past six months)" ~ "Removal of Sanction",
            heading == "Withdrawal of Accreditation" ~ "Withdrawal of Accreditation",
            TRUE ~ heading
          )

          tibble::tibble(
            institution_name_raw = clean_text(li_matches[, 3]),
            institution_state_raw = clean_text(li_matches[, 4]),
            accreditor = "HLC",
            action_type = classify_action(raw_action),
            action_label_raw = raw_action,
            action_status = classify_status(raw_action),
            action_date = as.Date(NA),
            action_year = NA_integer_,
            source_url = li_matches[, 2],
            source_title = paste(page_title, "-", heading),
            notes = heading,
            last_seen_at = Sys.time(),
            source_page_url = url,
            source_page_modified = page_modified
          )
        })
      }
    }

    parse_hlc_detail_page <- function(detail_url) {
      detail_html <- fetch_html_text(
        detail_url,
        paste0("hlc_", basename(gsub("/$", "", detail_url)), ".html"),
        refresh = refresh
      )
      detail_title <- extract_page_title(detail_html)
      detail_modified <- extract_page_modified_date(detail_html)
      doc <- xml2::read_html(detail_html)

      date_match <- stringr::str_match(
        clean_text(detail_title),
        "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+([0-9]{4})"
      )
      action_date <- if (!is.na(date_match[1, 1])) {
        as.Date(paste0(date_match[1, 2], " 01 ", date_match[1, 3]), format = "%B %d %Y")
      } else {
        as.Date(NA)
      }

      content_nodes <- xml2::xml_find_all(doc, "//*[contains(@class,'entry-content')]//*[self::p or self::ul]")
      if (length(content_nodes) == 0) {
        return(tibble::tibble())
      }

      current_institution <- NULL
      current_state <- NULL
      rows <- list()

      add_action_rows <- function(inst_name, inst_state, actions) {
        if (is.null(inst_name) || !nzchar(inst_name)) {
          return(NULL)
        }
        actions <- clean_text(actions)
        actions <- actions[
          stringr::str_detect(
            actions,
            "^(Approved|Accepted|Affirmed|Denied|Placed|Continued|Removed|Withdrew|Withdrawn|Issued|Extended|Required)"
          )
        ]
        if (length(actions) == 0) {
          return(NULL)
        }
        purrr::map(actions, function(action_txt) {
          tibble::tibble(
            institution_name_raw = inst_name,
            institution_state_raw = inst_state,
            accreditor = "HLC",
            action_type = classify_action(action_txt, "HLC"),
            action_label_raw = action_txt,
            action_status = classify_status(action_txt),
            action_date = action_date,
            action_year = suppressWarnings(as.integer(format(action_date, "%Y"))),
            source_url = detail_url,
            source_title = detail_title,
            notes = action_txt,
            last_seen_at = Sys.time(),
            source_page_url = detail_url,
            source_page_modified = detail_modified
          )
        })
      }

      for (node in content_nodes) {
        node_name <- xml2::xml_name(node)
        if (identical(node_name, "p")) {
          p_text <- clean_text(xml2::xml_text(node))
          if (!nzchar(p_text)) next

          p_link <- xml2::xml_find_first(node, ".//a[contains(@href, '/institution/')]")
          if (!inherits(p_link, "xml_missing")) {
            inst_name <- clean_text(xml2::xml_text(p_link))
            rest_text <- clean_text(stringr::str_remove(p_text, paste0("^", stringr::fixed(inst_name), "\\s*,\\s*")))
            if (stringr::str_detect(rest_text, ",\\s*[A-Z]{2}$")) {
              parsed <- extract_name_state_from_item(rest_text)
              current_institution <- parsed$institution_name_raw
              current_state <- parsed$institution_state_raw
            } else if (stringr::str_detect(p_text, ",\\s*[A-Z]{2}$")) {
              parsed <- extract_name_state_from_item(p_text)
              current_institution <- parsed$institution_name_raw
              current_state <- parsed$institution_state_raw
            } else {
              action_txt <- clean_text(stringr::str_remove(p_text, paste0("^", stringr::fixed(inst_name), "\\s*,\\s*")))
              rows <- c(rows, add_action_rows(inst_name, current_state, action_txt))
            }
          } else if (stringr::str_detect(p_text, ",\\s*[A-Z]{2}$")) {
            parsed <- extract_name_state_from_item(p_text)
            current_institution <- parsed$institution_name_raw
            current_state <- parsed$institution_state_raw
          }
        } else if (identical(node_name, "ul")) {
          li_text <- xml2::xml_find_all(node, ".//li") |> xml2::xml_text() |> clean_text()
          rows <- c(rows, add_action_rows(current_institution, current_state, li_text))
        }
      }

      dplyr::bind_rows(rows)
    }

    link_matches <- stringr::str_match_all(
      html,
      "href=\"(https://www\\.hlcommission\\.org/for-students/accreditation-actions/[a-z]+-20[0-9]{2}(?:-actions)?/?)\">([A-Za-z]+)\\s+([0-9]{4})</a>"
    )[[1]]

    historical_rows <- if (nrow(link_matches) == 0) {
      tibble::tibble()
    } else {
      detail_urls <- unique(link_matches[as.integer(link_matches[, 4]) >= 2024, 2])
      purrr::map_dfr(detail_urls, parse_hlc_detail_page)
    }

    dplyr::bind_rows(current_notice_rows, historical_rows) |>
      dplyr::distinct()
  }

  parse_sacscoc_detail_page <- function(url) {
    html <- fetch_html_text(
      url,
      paste0("sacscoc_", basename(gsub("/$", "", url)), ".html"),
      refresh = refresh
    )
    page_title <- extract_page_title(html)

    date_match <- stringr::str_match(clean_text(page_title), "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+([0-9]{4})")
    action_date <- if (!is.na(date_match[1, 1])) {
      as.Date(paste0(date_match[1, 2], " 01 ", date_match[1, 3]), format = "%B %d %Y")
    } else {
      as.Date(NA)
    }

    li_matches <- stringr::str_match_all(html, "(?s)<li>(.*?)</li>")[[1]]
    sanction_rows <- if (nrow(li_matches) == 0) {
      tibble::tibble()
    } else {
      raw_items <- clean_text(li_matches[, 2])
      raw_items <- raw_items[
        stringr::str_detect(raw_items, "\\bplaced on\\b|\\bcontinued on\\b|\\bremoved from\\b|withdraws from membership|withdrawal")
      ]

      purrr::map_dfr(raw_items, function(item) {
        item_clean <- clean_text(item)
        item_no_pdf <- stringr::str_remove(item_clean, "\\s*\\[?PDF\\]?$")
        item_no_pdf <- stringr::str_squish(item_no_pdf)

        inst_match <- stringr::str_match(
          item_no_pdf,
          "^(.*?),\\s*([^,]+),\\s*([^,(]+)\\s*\\((.*?)\\)$"
        )

        if (!is.na(inst_match[1, 1])) {
          return(tibble::tibble(
            institution_name_raw = clean_text(inst_match[1, 2]),
            institution_state_raw = clean_text(inst_match[1, 4]),
            accreditor = "SACSCOC",
            action_type = classify_action(inst_match[1, 5]),
            action_label_raw = clean_text(inst_match[1, 5]),
            action_status = classify_status(inst_match[1, 5]),
            action_date = action_date,
            action_year = as.integer(format(action_date, "%Y")),
            source_url = url,
            source_title = page_title,
            notes = item_clean,
            last_seen_at = Sys.time(),
            source_page_url = url,
            source_page_modified = NA_character_
          ))
        }

        withdrawal_match <- stringr::str_match(
          item_no_pdf,
          "^(.*?)\\s*\\(([^)]+)\\)\\s*(withdraws from membership)$"
        )
        if (!is.na(withdrawal_match[1, 1])) {
          state_abbr <- stringr::str_match(withdrawal_match[1, 3], ",\\s*([A-Z]{2})$")[, 2]
          return(tibble::tibble(
            institution_name_raw = clean_text(withdrawal_match[1, 2]),
            institution_state_raw = state_name(state_abbr),
            accreditor = "SACSCOC",
            action_type = "adverse_action",
            action_label_raw = clean_text(withdrawal_match[1, 4]),
            action_status = "active",
            action_date = action_date,
            action_year = as.integer(format(action_date, "%Y")),
            source_url = url,
            source_title = page_title,
            notes = item_clean,
            last_seen_at = Sys.time(),
            source_page_url = url,
            source_page_modified = NA_character_
          ))
        }

        tibble::tibble()
      })
    }

    disclosure_matches <- stringr::str_match_all(
      html,
      paste0(
        "(?s)<(?:p|li)>\\s*",
        "<a href=\"(https://sacscoc\\.box\\.com/s/[^\"]+)\">(.*?)</a>,\\s*",
        "([^,<]+),\\s*",
        "([A-Z]{2}|[A-Za-z ]+)",
        ".*?",
        "</(?:p|li)>"
      )
    )[[1]]

    disclosure_rows <- if (nrow(disclosure_matches) == 0) {
      tibble::tibble()
    } else {
      purrr::map_dfr(seq_len(nrow(disclosure_matches)), function(i) {
        tibble::tibble(
          institution_name_raw = clean_text(disclosure_matches[i, 3]),
          institution_state_raw = state_name(clean_text(disclosure_matches[i, 5])),
          accreditor = "SACSCOC",
          action_type = "other",
          action_label_raw = "Public Disclosure Statement",
          action_status = "active",
          action_date = action_date,
          action_year = as.integer(format(action_date, "%Y")),
          source_url = disclosure_matches[i, 2],
          source_title = paste(page_title, "- Public Disclosure Statement"),
          notes = clean_text(paste(disclosure_matches[i, 3], disclosure_matches[i, 4], disclosure_matches[i, 5])),
          last_seen_at = Sys.time(),
          source_page_url = url,
          source_page_modified = NA_character_
        )
      }) |>
        dplyr::filter(
          !stringr::str_detect(
            institution_name_raw,
            "Accreditation Actions|Public Disclosure Statements"
          )
        ) |>
        dplyr::distinct(
          accreditor,
          source_url,
          institution_name_normalized = normalize_name(institution_name_raw),
          action_label_raw,
          .keep_all = TRUE
        ) |>
        dplyr::select(-institution_name_normalized)
    }

    dplyr::bind_rows(sanction_rows, disclosure_rows) |>
      dplyr::distinct()
  }

  parse_sacscoc <- function() {
    landing_url <- "https://sacscoc.org/institutions/accreditation-actions-and-disclosures/"
    html <- fetch_html_text(landing_url, "sacscoc_disclosures.html", refresh = refresh)

    links <- stringr::str_match_all(
      html,
      "<a href=\"(https://sacscoc.org/institutions/accreditation-actions-and-disclosures/[^\"#]+?)\">(December [0-9]{4} Accreditation Actions and Public Disclosure Statements|June [0-9]{4} Accreditation Actions and Public Disclosure Statements)</a>"
    )[[1]]

    if (nrow(links) == 0) {
      return(tibble::tibble())
    }

    detail_urls <- unique(links[, 2])
    purrr::map_dfr(detail_urls, parse_sacscoc_detail_page)
  }

  parse_neche <- function() {
    url <- "https://www.neche.org/recent-commission-actions/"
    html <- fetch_html_text(url, "neche_actions.html", refresh = refresh)
    page_title <- extract_page_title(html)
    page_modified <- extract_page_modified_date(html)

    matches <- stringr::str_match_all(
      html,
      "(?s)<a class=\"elementor-toggle-title\"[^>]*>(.*?)</a>.*?<div id=\"elementor-tab-content-[^\"]+\" class=\"elementor-tab-content[^\"]*\"[^>]*>(.*?)</div>"
    )[[1]]

    if (nrow(matches) == 0) {
      return(tibble::tibble())
    }

    purrr::map_dfr(seq_len(nrow(matches)), function(i) {
      heading <- clean_text(matches[i, 2])
      body <- matches[i, 3]

      if (!has_public_action_keywords(heading)) {
        return(tibble::tibble())
      }

      items <- stringr::str_match_all(body, "(?s)<li[^>]*>(.*?)</li>")[[1]]
      if (nrow(items) == 0) {
        return(tibble::tibble())
      }

      raw_items <- clean_text(items[, 2])
      raw_items <- raw_items[nzchar(raw_items)]
      if (length(raw_items) == 0) {
        return(tibble::tibble())
      }

      purrr::map_dfr(raw_items, function(item) {
        parsed <- extract_name_state_from_item(item)
        tibble::tibble(
          institution_name_raw = parsed$institution_name_raw,
          institution_state_raw = parsed$institution_state_raw,
          accreditor = "NECHE",
          action_type = classify_action(heading),
          action_label_raw = heading,
          action_status = classify_status(heading),
          action_date = as.Date(NA),
          action_year = suppressWarnings(as.integer(stringr::str_extract(page_modified, "^[0-9]{4}"))),
          source_url = url,
          source_title = page_title,
          notes = item,
          last_seen_at = Sys.time(),
          source_page_url = url,
          source_page_modified = page_modified
        )
      })
    })
  }

  parse_wscuc_detail_page <- function(url) {
    html <- fetch_html_text(
      url,
      paste0("wscuc_", basename(gsub("/$", "", url)), ".html"),
      refresh = refresh
    )
    page_title <- extract_page_title(html)
    page_modified <- extract_page_modified_date(html)

    date_match <- stringr::str_match(
      clean_text(page_title),
      "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+([0-9]{4})"
    )
    action_date <- if (!is.na(date_match[1, 1])) {
      as.Date(paste0(date_match[1, 2], " 01 ", date_match[1, 3]), format = "%B %d %Y")
    } else {
      as.Date(NA)
    }

    heading_matches <- stringr::str_match_all(html, "(?s)<h2>(.*?)</h2>")[[1]]
    heading_locs <- stringr::str_locate_all(html, "(?s)<h2>.*?</h2>")[[1]]
    if (nrow(heading_matches) == 0 || nrow(heading_locs) == 0) {
      return(tibble::tibble())
    }

    headings <- clean_text(heading_matches[, 2])

    purrr::map_dfr(seq_along(headings), function(i) {
      heading <- headings[[i]]
      if (!has_public_action_keywords(heading)) {
        return(tibble::tibble())
      }

      start_pos <- heading_locs[i, 2] + 1L
      end_pos <- if (i < nrow(heading_locs)) heading_locs[i + 1, 1] - 1L else nchar(html)
      block <- substr(html, start_pos, end_pos)

      li_matches <- stringr::str_match_all(block, "(?s)<li[^>]*>(.*?)</li>")[[1]]
      if (nrow(li_matches) == 0) {
        return(tibble::tibble())
      }

      raw_items <- clean_text(li_matches[, 2])
      raw_items <- raw_items[nzchar(raw_items)]
      if (length(raw_items) == 0) {
        return(tibble::tibble())
      }

      purrr::map_dfr(raw_items, function(item) {
        parsed <- extract_name_state_from_item(item)
        tibble::tibble(
          institution_name_raw = parsed$institution_name_raw,
          institution_state_raw = parsed$institution_state_raw,
          accreditor = "WSCUC",
          action_type = classify_action(heading),
          action_label_raw = heading,
          action_status = classify_status(heading),
          action_date = action_date,
          action_year = suppressWarnings(as.integer(format(action_date, "%Y"))),
          source_url = url,
          source_title = page_title,
          notes = item,
          last_seen_at = Sys.time(),
          source_page_url = url,
          source_page_modified = page_modified
        )
      })
    })
  }

  parse_wscuc <- function() {
    archive_urls <- c(
      "https://www.wscuc.org/post/category/commission-actions/",
      "https://www.wscuc.org/post/category/commission-actions/page/2/"
    )

    detail_urls <- purrr::map_dfr(seq_along(archive_urls), function(i) {
      archive_html <- fetch_html_text(
        archive_urls[[i]],
        paste0("wscuc_commission_actions_archive_", i, ".html"),
        refresh = refresh
      )

      links <- stringr::str_match_all(
        archive_html,
        "href=\"(https://www\\.wscuc\\.org/post/[^\"#]+commission-actions/?)\""
      )[[1]]

      if (nrow(links) == 0) {
        return(tibble::tibble(url = character()))
      }

      tibble::tibble(url = links[, 2])
    }) |>
      dplyr::distinct() |>
      dplyr::filter(stringr::str_detect(url, "20(24|25|26)")) |>
      dplyr::pull(url)

    if (length(detail_urls) == 0) {
      return(tibble::tibble())
    }

    purrr::map_dfr(detail_urls, parse_wscuc_detail_page)
  }

  message("Reading financial tracker data ...")
  financial_all <- readr::read_csv(financial_input, show_col_types = FALSE, progress = FALSE)
  latest_year <- suppressWarnings(max(financial_all$year, na.rm = TRUE))
  financial_latest <- financial_all |>
    dplyr::filter(year == latest_year) |>
    dplyr::mutate(
      institution_name_tracker = institution_name,
      state_full = state_name(state),
      norm_name = normalize_name(institution_name)
    )

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

  message("Fetching accreditation actions ...")
  raw_actions <- dplyr::bind_rows(
    parse_msche(),
    parse_hlc(),
    parse_sacscoc(),
    parse_neche(),
    parse_wscuc()
  ) |>
    dplyr::mutate(
      institution_name_raw = clean_text(institution_name_raw),
      institution_state_raw = clean_text(institution_state_raw),
      institution_name_normalized = normalize_name(institution_name_raw),
      institution_state_normalized = state_name(institution_state_raw),
      accreditation_warning = action_type %in% c("warning", "probation", "show_cause"),
      accreditation_warning_or_notice = action_type %in% c("notice", "warning", "probation", "show_cause")
    ) |>
    dplyr::left_join(
      lookup_exact,
      by = c(
        "institution_name_normalized" = "norm_name",
        "institution_state_normalized" = "state_match"
      )
    ) |>
    dplyr::rename(
      exact_unitid = matched_unitid,
      exact_tracker_name = tracker_name,
      exact_tracker_state = tracker_state
    ) |>
    dplyr::left_join(
      lookup_name_only,
      by = c("institution_name_normalized" = "norm_name")
    ) |>
    dplyr::rename(
      name_only_unitid = matched_unitid,
      name_only_tracker_name = tracker_name,
      name_only_tracker_state = tracker_state
    ) |>
    dplyr::mutate(
      unitid = dplyr::coalesce(exact_unitid, name_only_unitid),
      tracker_name = dplyr::coalesce(exact_tracker_name, name_only_tracker_name),
      tracker_state = dplyr::coalesce(exact_tracker_state, name_only_tracker_state),
      match_method = dplyr::case_when(
        !is.na(exact_unitid) ~ "normalized_name_plus_state",
        is.na(exact_unitid) & !is.na(name_only_unitid) ~ "normalized_name_only",
        TRUE ~ "unmatched"
      )
    ) |>
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

  actions_joined <- raw_actions |>
    dplyr::left_join(financial_join, by = "unitid")

  collapse_unique <- function(x) {
    vals <- unique(stats::na.omit(as.character(x)))
    if (length(vals) == 0) NA_character_ else paste(sort(vals), collapse = "; ")
  }

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

  current_status <- institution_summary |>
    dplyr::filter(has_active_warning_or_notice | has_active_adverse_action) |>
    dplyr::arrange(
      dplyr::desc(has_active_warning_or_notice),
      dplyr::desc(has_active_adverse_action),
      tracker_name
    )

  build_match_suggestions <- function(unmatched_df, candidates_df, max_candidates = 3L) {
    if (nrow(unmatched_df) == 0) {
      return(unmatched_df)
    }

    purrr::map_dfr(seq_len(nrow(unmatched_df)), function(i) {
      row <- unmatched_df[i, ]
      same_state <- candidates_df
      if (!is.na(row$institution_state_normalized) && nzchar(row$institution_state_normalized)) {
        same_state <- candidates_df |>
          dplyr::filter(state_full == row$institution_state_normalized)
      }
      pool <- if (nrow(same_state) > 0) same_state else candidates_df
      dists <- utils::adist(row$institution_name_normalized, pool$norm_name)
      ord <- order(dists[1, ], pool$institution_name)
      top_n <- head(ord, max_candidates)
      suggestions <- paste0(
        pool$institution_name[top_n],
        " (",
        pool$state_full[top_n],
        "; UNITID ",
        pool$unitid[top_n],
        "; distance ",
        as.integer(dists[1, top_n]),
        ")"
      )
      dplyr::bind_cols(
        row,
        tibble::tibble(suggested_matches = paste(suggestions, collapse = " | "))
      )
    })
  }

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

  source_coverage <- actions_joined |>
    dplyr::count(accreditor, action_type, action_status, sort = TRUE)

  outputs <- list(
    actions = paste0(output_prefix, "_actions_joined.csv"),
    summary = paste0(output_prefix, "_institution_summary.csv"),
    current = paste0(output_prefix, "_current_status.csv"),
    unmatched = paste0(output_prefix, "_unmatched_for_review.csv"),
    coverage = paste0(output_prefix, "_source_coverage.csv"),
    workbook = paste0(output_prefix, "_workbook.xlsx")
  )

  readr::write_csv(actions_joined, outputs$actions, na = "")
  readr::write_csv(institution_summary, outputs$summary, na = "")
  readr::write_csv(current_status, outputs$current, na = "")
  readr::write_csv(unmatched_for_review, outputs$unmatched, na = "")
  readr::write_csv(source_coverage, outputs$coverage, na = "")

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "actions")
  openxlsx::writeData(wb, "actions", actions_joined)
  openxlsx::addWorksheet(wb, "summary")
  openxlsx::writeData(wb, "summary", institution_summary)
  openxlsx::addWorksheet(wb, "current")
  openxlsx::writeData(wb, "current", current_status)
  openxlsx::addWorksheet(wb, "unmatched")
  openxlsx::writeData(wb, "unmatched", unmatched_for_review)
  openxlsx::addWorksheet(wb, "coverage")
  openxlsx::writeData(wb, "coverage", source_coverage)
  openxlsx::addWorksheet(wb, "notes")
  openxlsx::writeData(
    wb,
    "notes",
    data.frame(
      notes = c(
        "This first version is intentionally partial rather than pretending to be comprehensive.",
        "Covered accreditors in the current scraper: MSCHE current sanctions page, HLC current public disclosure notices, SACSCOC latest public action/disclosure pages with explicit sanction language, NECHE recent commission actions, and WSCUC commission action posts.",
        "Still not covered in this build: NWCCU and ACCJC. Their public action data needs a separate scraper or manual curation workflow.",
        "HLC uses Notice as a public sanction. Because that is not literally named Warning, the file includes both accreditation_warning and accreditation_warning_or_notice.",
        "WSCUC commission action posts include many routine reaffirmation and report items. This scraper intentionally keeps only action headings with warning, probation, notice, show cause, withdrawal, closure, or adverse language.",
        "Unmatched institutions are often schools outside the four-year tracker scope or schools whose public accreditor name does not exactly match IPEDS naming. Review the unmatched sheet before publication."
      ),
      stringsAsFactors = FALSE
    )
  )
  openxlsx::saveWorkbook(wb, outputs$workbook, overwrite = TRUE)

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
