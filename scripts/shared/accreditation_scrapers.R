################################################################################
# scripts/shared/accreditation_scrapers.R
#
# Per-accreditor HTML scraper functions that extract accreditation action data
# from six major accreditors' websites (MSCHE, HLC, SACSCOC, NECHE, WSCUC).
# Each accreditor has a different site structure, so this file contains custom
# scraping logic for each one, all converting output to a standardized table.
#
################################################################################

# ---------------------------------------------------------------------------
# SHARED INNER-LOOP PRIMITIVES FOR HTML PARSING
# ---------------------------------------------------------------------------

# Converts a heading + list of institution items into standardized action rows.
# Shared by accreditors that use the same pattern: a heading describing the action
# (e.g., "Probation"), followed by <li> items for each institution.
parse_items_to_rows <- function(raw_items, accreditor, heading,
                                action_date, action_year,
                                source_url, source_title,
                                source_page_url, source_page_modified) {
  purrr::map_dfr(raw_items, function(item) {
    # Parse the institution name and state from the list item text
    parsed <- extract_name_state_from_item(item)
    tibble::tibble(
      institution_name_raw  = parsed$institution_name_raw,
      institution_state_raw = parsed$institution_state_raw,
      accreditor            = accreditor,
      action_type           = classify_action(heading),
      action_label_raw      = heading,
      action_status         = classify_status(heading),
      action_date           = action_date,
      action_year           = action_year,
      source_url            = source_url,
      source_title          = source_title,
      notes                 = item,
      last_seen_at          = Sys.time(),
      source_page_url       = source_page_url,
      source_page_modified  = source_page_modified
    )
  })
}

# Extracts all <li> items from an HTML block and returns their cleaned text content.
# Used to pull institution names from bulleted lists on accreditor pages.
extract_list_items_from_html <- function(html_block, item_pattern = "(?s)<li[^>]*>(.*?)</li>") {
  # Find all <li> elements and capture their content (group 2)
  item_matches <- stringr::str_match_all(html_block, item_pattern)[[1]]
  # Return empty character vector if no matches
  if (nrow(item_matches) == 0) {
    return(character())
  }
  # Clean the extracted text (decode HTML, normalize whitespace)
  raw_items <- clean_text(item_matches[, 2])
  # Return only non-empty items
  raw_items[nzchar(raw_items)]
}

# Splits HTML into sections based on a regex pattern that captures heading + body.
extract_regex_heading_sections <- function(html, section_pattern) {
  matches <- stringr::str_match_all(html, section_pattern)[[1]]
  if (nrow(matches) == 0) {
    return(tibble::tibble(heading = character(), body = character()))
  }
  tibble::tibble(
    heading = clean_text(matches[, 2]),
    body = matches[, 3]
  )
}

# Splits HTML into sections using HTML heading tags (default h2) to delimit sections.
extract_tag_heading_sections <- function(html, heading_tag = "h2") {
  heading_pattern <- paste0("(?s)<", heading_tag, ">(.*?)</", heading_tag, ">")
  heading_matches <- stringr::str_match_all(html, heading_pattern)[[1]]
  heading_locs <- stringr::str_locate_all(html, heading_pattern)[[1]]
  if (nrow(heading_matches) == 0 || nrow(heading_locs) == 0) {
    return(tibble::tibble(heading = character(), body = character()))
  }

  headings <- clean_text(heading_matches[, 2])
  bodies <- purrr::map_chr(seq_along(headings), function(i) {
    start_pos <- heading_locs[i, 2] + 1L
    end_pos <- if (i < nrow(heading_locs)) heading_locs[i + 1, 1] - 1L else nchar(html)
    substr(html, start_pos, end_pos)
  })

  tibble::tibble(heading = headings, body = bodies)
}

# Iterates over HTML sections and extracts public action items (filtering by keywords).
# Only sections whose headings contain public action keywords are processed.
parse_public_action_sections <- function(sections, accreditor,
                                         action_date, action_year,
                                         source_url, source_title,
                                         source_page_url, source_page_modified) {
  if (nrow(sections) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(seq_len(nrow(sections)), function(i) {
    heading <- sections$heading[[i]]
    if (!has_public_action_keywords(heading)) {
      return(tibble::tibble())
    }

    raw_items <- extract_list_items_from_html(sections$body[[i]])
    if (length(raw_items) == 0) {
      return(tibble::tibble())
    }

    parse_items_to_rows(
      raw_items = raw_items,
      accreditor = accreditor,
      heading = heading,
      action_date = action_date,
      action_year = action_year,
      source_url = source_url,
      source_title = source_title,
      source_page_url = source_page_url,
      source_page_modified = source_page_modified
    )
  })
}

MSCHE_CURRENT_STATUS_ACTION_TYPES <- c(
  "Non-Compliance Warning" = "warning",
  "Non-Compliance Probation" = "probation",
  "Non-Compliance Show Cause" = "show_cause",
  "Adverse Action" = "adverse_action"
)
MSCHE_CURRENT_STATUS_URL <- "https://www.msche.org/non-compliance-and-adverse-actions-by-status/"
MSCHE_RECENT_ACTIONS_URL <- "https://www.msche.org/recent-commission-actions/"
MSCHE_RECENT_ACTIONS_LINK_PATTERN <- paste0(
  "<a href=\"(https://www\\.msche\\.org/commission-actions/\\?fd=[0-9]+(?:&amp;|&)ld=[0-9]+)\">",
  "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+([0-9]{4})</a>"
)

HLC_CURRENT_NOTICE_ACTION_LABELS <- c(
  "On Notice" = "On Notice",
  "On Probation" = "On Probation",
  "Removal of Sanction (past six months)" = "Removal of Sanction",
  "Withdrawal of Accreditation" = "Withdrawal of Accreditation"
)
HLC_ACTIONS_URL <- "https://www.hlcommission.org/for-students/accreditation-actions/"
HLC_CURRENT_NOTICE_BLOCK_PATTERN <- "(?s)<h3 class=\"wp-block-heading\">Current Public Disclosure Notices</h3>(.*?)(?=<h3 class=\"kt-adv-heading|</main>)"
HLC_NOTICE_PANE_PATTERN <- "(?s)<span class=\"kt-blocks-accordion-title\">(On Notice|On Probation|Removal of Sanction \\(past six months\\)|Withdrawal of Accreditation)</span>.*?<ul class=\"kt-svg-icon-list\">(.*?)</ul>"
HLC_DETAIL_LINK_PATTERN <- "href=\"(https://www\\.hlcommission\\.org/for-students/accreditation-actions/[a-z]+-20[0-9]{2}(?:-actions)?/?)\">([A-Za-z]+)\\s+([0-9]{4})</a>"

NECHE_TOGGLE_SECTION_PATTERN <- paste0(
  "(?s)<a class=\"elementor-toggle-title\"[^>]*>(.*?)</a>",
  ".*?<div id=\"elementor-tab-content-[^\"]+\" class=\"elementor-tab-content[^\"]*\"[^>]*>(.*?)</div>"
)
NECHE_ACTIONS_URL <- "https://www.neche.org/recent-commission-actions/"

WSCUC_ARCHIVE_URLS <- c(
  "https://www.wscuc.org/post/category/commission-actions/",
  "https://www.wscuc.org/post/category/commission-actions/page/2/"
)
WSCUC_DETAIL_LINK_PATTERN <- "href=\"(https://www\\.wscuc\\.org/post/[^\"#]+commission-actions/?)\""

SACSCOC_SANCTION_KEYWORD_PATTERN <- "\\bplaced on\\b|\\bcontinued on\\b|\\bremoved from\\b|withdraws from membership|withdrawal"
SACSCOC_STANDARD_ITEM_PATTERN <- "^(.*?),\\s*([^,]+),\\s*([^,(]+)\\s*\\((.*?)\\)$"
SACSCOC_WITHDRAWAL_ITEM_PATTERN <- "^(.*?)\\s*\\(([^)]+)\\)\\s*(withdraws from membership)$"
SACSCOC_LANDING_URL <- "https://sacscoc.org/institutions/accreditation-actions-and-disclosures/"
SACSCOC_LANDING_LINK_PATTERN <- "<a href=\"(https://sacscoc.org/institutions/accreditation-actions-and-disclosures/[^\"#]+?)\">(December [0-9]{4} Accreditation Actions and Public Disclosure Statements|June [0-9]{4} Accreditation Actions and Public Disclosure Statements)</a>"
SACSCOC_DISCLOSURE_ITEM_PATTERN <- paste0(
  "(?s)<(?:p|li)>\\s*",
  "<a href=\"(https://sacscoc\\.box\\.com/s/[^\"]+)\">(.*?)</a>,\\s*",
  "([^,<]+),\\s*",
  "([A-Z]{2}|[A-Za-z ]+)",
  ".*?",
  "</(?:p|li)>"
)

# Looks up a key in a named vector, returning the value or a default if not found.
lookup_or_default <- function(key, lookup, default = key) {
  value <- unname(lookup[[key]])
  if (is.null(value) || is.na(value)) default else value
}

# Parses a single SACSCOC sanction item, extracting institution name, state, and action details.
parse_sacscoc_sanction_item <- function(item, action_date, url, page_title) {
  item_clean <- clean_text(item)
  item_no_pdf <- stringr::str_remove(item_clean, "\\s*\\[?PDF\\]?$")
  item_no_pdf <- stringr::str_squish(item_no_pdf)

  standard_match <- stringr::str_match(item_no_pdf, SACSCOC_STANDARD_ITEM_PATTERN)
  if (!is.na(standard_match[1, 1])) {
    action_label <- clean_text(standard_match[1, 5])
    return(tibble::tibble(
      institution_name_raw = clean_text(standard_match[1, 2]),
      institution_state_raw = clean_text(standard_match[1, 4]),
      accreditor = "SACSCOC",
      action_type = classify_action(action_label),
      action_label_raw = action_label,
      action_status = classify_status(action_label),
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

  withdrawal_match <- stringr::str_match(item_no_pdf, SACSCOC_WITHDRAWAL_ITEM_PATTERN)
  if (!is.na(withdrawal_match[1, 1])) {
    state_abbr <- stringr::str_match(withdrawal_match[1, 3], ",\\s*([A-Z]{2})$")[, 2]
    action_label <- clean_text(withdrawal_match[1, 4])
    return(tibble::tibble(
      institution_name_raw = clean_text(withdrawal_match[1, 2]),
      institution_state_raw = state_name(state_abbr),
      accreditor = "SACSCOC",
      action_type = "adverse_action",
      action_label_raw = action_label,
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
}

# Converts regex matches for public disclosure statements into standardized rows.
build_sacscoc_disclosure_rows <- function(disclosure_matches, action_date, url, page_title) {
  if (nrow(disclosure_matches) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(seq_len(nrow(disclosure_matches)), function(i) {
    tibble::tibble(
      institution_name_raw = clean_text(disclosure_matches[i, 4]),
      institution_state_raw = state_name(clean_text(disclosure_matches[i, 5])),
      accreditor = "SACSCOC",
      action_type = "other",
      action_label_raw = "Public Disclosure Statement",
      action_status = "active",
      action_date = action_date,
      action_year = as.integer(format(action_date, "%Y")),
      source_url = disclosure_matches[i, 2],
      source_title = paste(page_title, "- Public Disclosure Statement"),
      notes = clean_text(paste(disclosure_matches[i, 4], disclosure_matches[i, 5])),
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

HLC_DETAIL_ACTION_PREFIX_PATTERN <- "^(Approved|Accepted|Affirmed|Denied|Placed|Continued|Removed|Withdrew|Withdrawn|Issued|Extended|Required)"
HLC_INSTITUTION_STATE_SUFFIX_PATTERN <- ",\\s*[A-Z]{2}$"

# Builds action rows for HLC from institution name, state, and action text strings.
build_hlc_action_rows <- function(inst_name, inst_state, actions, action_date, detail_url, detail_title, detail_modified) {
  if (is.null(inst_name) || !nzchar(inst_name)) {
    return(NULL)
  }
  actions <- clean_text(actions)
  actions <- actions[stringr::str_detect(actions, HLC_DETAIL_ACTION_PREFIX_PATTERN)]
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

# Tracks the current institution while parsing HLC paragraphs, handling different text layouts.
# If a paragraph has a link, uses the link text as institution name; otherwise parses from paragraph.
update_hlc_current_institution <- function(p_text, p_link) {
  current_institution <- NULL
  current_state <- NULL
  action_text <- NULL

  if (!inherits(p_link, "xml_missing")) {
    inst_name <- unname(clean_text(xml2::xml_text(p_link)))
    rest_text <- clean_text(stringr::str_remove(p_text, paste0("^", stringr::fixed(inst_name), "\\s*,\\s*")))
    if (stringr::str_detect(rest_text, HLC_INSTITUTION_STATE_SUFFIX_PATTERN)) {
      parsed <- extract_name_state_from_item(rest_text)
      current_institution <- inst_name
      current_state <- unname(parsed$institution_state_raw)
    } else if (stringr::str_detect(p_text, HLC_INSTITUTION_STATE_SUFFIX_PATTERN)) {
      parsed <- extract_name_state_from_item(p_text)
      current_institution <- unname(parsed$institution_name_raw)
      current_state <- unname(parsed$institution_state_raw)
    } else {
      current_institution <- inst_name
      action_text <- unname(clean_text(stringr::str_remove(p_text, paste0("^", stringr::fixed(inst_name), "\\s*,\\s*"))))
    }
  } else if (stringr::str_detect(p_text, HLC_INSTITUTION_STATE_SUFFIX_PATTERN)) {
    parsed <- extract_name_state_from_item(p_text)
    current_institution <- unname(parsed$institution_name_raw)
    current_state <- unname(parsed$institution_state_raw)
  }

  list(
    institution = current_institution,
    state = current_state,
    action_text = action_text
  )
}

# Parses HLC detail page content nodes (p and ul elements), tracking institution context.
parse_hlc_content_nodes <- function(content_nodes, action_date, detail_url, detail_title, detail_modified) {
  current_institution <- NULL
  current_state <- NULL
  rows <- list()

  for (node in content_nodes) {
    node_name <- xml2::xml_name(node)
    if (identical(node_name, "p")) {
      p_text <- clean_text(xml2::xml_text(node))
      if (!nzchar(p_text)) next

      updated <- update_hlc_current_institution(
        p_text = p_text,
        p_link = xml2::xml_find_first(node, ".//a[contains(@href, '/institution/')]")
      )

      if (!is.null(updated$institution)) {
        current_institution <- updated$institution
      }
      if (!is.null(updated$state)) {
        current_state <- updated$state
      }
      if (!is.null(updated$action_text) && nzchar(updated$action_text)) {
        rows <- c(
          rows,
          build_hlc_action_rows(
            inst_name = current_institution,
            inst_state = current_state,
            actions = updated$action_text,
            action_date = action_date,
            detail_url = detail_url,
            detail_title = detail_title,
            detail_modified = detail_modified
          )
        )
      }
    } else if (identical(node_name, "ul")) {
      li_text <- xml2::xml_find_all(node, ".//li") |> xml2::xml_text() |> clean_text()
      rows <- c(
        rows,
        build_hlc_action_rows(
          inst_name = current_institution,
          inst_state = current_state,
          actions = li_text,
          action_date = action_date,
          detail_url = detail_url,
          detail_title = detail_title,
          detail_modified = detail_modified
        )
      )
    }
  }

  dplyr::bind_rows(rows)
}

# Scrapes MSCHE accreditation data from two sources: current non-compliance status
# (from a single status page with H3 sections) and recent commission actions
# (by iterating monthly pages from 2017 to present, organized by state).
parse_msche <- function(cache_dir, refresh) {
  url <- MSCHE_CURRENT_STATUS_URL
  html <- fetch_html_text(url, "msche_status.html", cache_dir, refresh = refresh)
  page_title <- extract_page_title(html)
  page_modified <- extract_page_modified_date(html)

  section_matches <- stringr::str_match_all(
    html,
    "(?s)<h3>(Non-Compliance Warning|Non-Compliance Probation|Non-Compliance Show Cause|Adverse Action)</h3>(.*?)(?=<h3>|<div class=\"single-share\"|</article>)"
  )[[1]]

  # Guard: if the page returned non-empty HTML but none of the expected H3
  # action headings are present, the page was likely rendered by JavaScript and
  # httr2 received only the pre-JS shell.  Emit a warning so callers know the
  # 0-row result is suspicious rather than a genuine "no actions" outcome.
  if (nrow(section_matches) == 0 && nzchar(trimws(html))) {
    has_any_heading <- any(vapply(
      names(MSCHE_CURRENT_STATUS_ACTION_TYPES),
      function(lbl) grepl(lbl, html, fixed = TRUE),
      logical(1)
    ))
    if (!has_any_heading) {
      warning(sprintf(
        paste(
          "parse_msche: fetched %s but found none of the expected H3 action",
          "headings (%s). The page may be JavaScript-rendered and unavailable",
          "to httr2. Returning 0 current-status rows \u2014 validate output."
        ),
        url,
        paste(names(MSCHE_CURRENT_STATUS_ACTION_TYPES), collapse = ", ")
      ))
    }
  }

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

      action_label <- lookup_or_default(heading, MSCHE_CURRENT_STATUS_ACTION_TYPES, default = "other")

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

  # Parses a single MSCHE monthly action page, extracting institutions organized by state.
  parse_msche_month_page <- function(month_url, month_label) {
    cache_name <- paste0("msche_", gsub("[^a-z0-9]+", "_", tolower(month_label)), ".html")
    month_html <- fetch_html_text(month_url, cache_name, cache_dir, refresh = refresh)
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

  # Scrapes all recent MSCHE commission actions by iterating monthly pages from 2017 to present.
  parse_msche_recent_actions <- function() {
    years <- seq.int(2017L, max(2017L, as.integer(format(Sys.Date(), "%Y"))))

    month_links <- purrr::map_dfr(years, function(one_year) {
      recent_url <- if (one_year == as.integer(format(Sys.Date(), "%Y"))) {
        MSCHE_RECENT_ACTIONS_URL
      } else {
        paste0(MSCHE_RECENT_ACTIONS_URL, "?my=", one_year)
      }

      recent_html <- fetch_html_text(
        recent_url,
        paste0("msche_recent_actions_", one_year, ".html"),
        cache_dir,
        refresh = refresh
      )

      links <- stringr::str_match_all(
        recent_html,
        MSCHE_RECENT_ACTIONS_LINK_PATTERN
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

# Scrapes HLC accreditation data from two sources: current public disclosure notices
# (from accordion panels on the main page) and historical action detail pages
# (links to monthly action pages from 2024 onward).
parse_hlc <- function(cache_dir, refresh) {
  url <- HLC_ACTIONS_URL
  html <- fetch_html_text(url, "hlc_actions.html", cache_dir, refresh = refresh)
  page_title <- extract_page_title(html)
  page_modified <- extract_page_modified_date(html)

  current_notice_block <- stringr::str_match(
    html,
    HLC_CURRENT_NOTICE_BLOCK_PATTERN
  )[, 2]

  current_notice_rows <- if (is.na(current_notice_block)) {
    tibble::tibble()
  } else {
    pane_matches <- stringr::str_match_all(
      current_notice_block,
      HLC_NOTICE_PANE_PATTERN
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

        raw_action <- lookup_or_default(heading, HLC_CURRENT_NOTICE_ACTION_LABELS)

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

  # Parses an HLC monthly action detail page, extracting institution names and their actions.
  parse_hlc_detail_page <- function(detail_url) {
    detail_html <- fetch_html_text(
      detail_url,
      paste0("hlc_", basename(gsub("/$", "", detail_url)), ".html"),
      cache_dir,
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
    parse_hlc_content_nodes(
      content_nodes = content_nodes,
      action_date = action_date,
      detail_url = detail_url,
      detail_title = detail_title,
      detail_modified = detail_modified
    )
  }

  link_matches <- stringr::str_match_all(
    html,
    HLC_DETAIL_LINK_PATTERN
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

# Scrapes a SACSCOC detail page, extracting both sanction actions and public disclosure statements.
parse_sacscoc_detail_page <- function(url, cache_dir, refresh) {
  html <- fetch_html_text(
    url,
    paste0("sacscoc_", basename(gsub("/$", "", url)), ".html"),
    cache_dir,
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
    raw_items <- raw_items[stringr::str_detect(raw_items, SACSCOC_SANCTION_KEYWORD_PATTERN)]
    purrr::map_dfr(raw_items, parse_sacscoc_sanction_item, action_date = action_date, url = url, page_title = page_title)
  }

  disclosure_matches <- stringr::str_match_all(html, SACSCOC_DISCLOSURE_ITEM_PATTERN)[[1]]
  disclosure_rows <- build_sacscoc_disclosure_rows(disclosure_matches, action_date = action_date, url = url, page_title = page_title)

  dplyr::bind_rows(sanction_rows, disclosure_rows) |>
    dplyr::distinct()
}

# Scrapes SACSCOC by first finding links to June/December action pages from the landing page,
# then fetching each detail page to extract sanctions and disclosure statements.
parse_sacscoc <- function(cache_dir, refresh) {
  landing_url <- SACSCOC_LANDING_URL
  html <- fetch_html_text(landing_url, "sacscoc_disclosures.html", cache_dir, refresh = refresh)

  links <- stringr::str_match_all(
    html,
    SACSCOC_LANDING_LINK_PATTERN
  )[[1]]

  if (nrow(links) == 0) {
    return(tibble::tibble())
  }

  detail_urls <- unique(links[, 2])
  purrr::map_dfr(detail_urls, function(u) parse_sacscoc_detail_page(u, cache_dir, refresh))
}

# Scrapes NECHE accreditation actions from their recent actions page, extracting
# institutions grouped by action type from toggle/accordion sections.
parse_neche <- function(cache_dir, refresh) {
  url <- NECHE_ACTIONS_URL
  html <- fetch_html_text(url, "neche_actions.html", cache_dir, refresh = refresh)
  page_title <- extract_page_title(html)
  page_modified <- extract_page_modified_date(html)

  sections <- extract_regex_heading_sections(html, NECHE_TOGGLE_SECTION_PATTERN)
  parse_public_action_sections(
    sections = sections,
    accreditor = "NECHE",
    action_date = as.Date(NA),
    action_year = suppressWarnings(as.integer(stringr::str_extract(page_modified, "^[0-9]{4}"))),
    source_url = url,
    source_title = page_title,
    source_page_url = url,
    source_page_modified = page_modified
  )
}

parse_wscuc_detail_page <- function(url, cache_dir, refresh) {
  html <- fetch_html_text(
    url,
    paste0("wscuc_", basename(gsub("/$", "", url)), ".html"),
    cache_dir,
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

  sections <- extract_tag_heading_sections(html, heading_tag = "h2")
  parse_public_action_sections(
    sections = sections,
    accreditor = "WSCUC",
    action_date = action_date,
    action_year = suppressWarnings(as.integer(format(action_date, "%Y"))),
    source_url = url,
    source_title = page_title,
    source_page_url = url,
    source_page_modified = page_modified
  )
}

parse_wscuc <- function(cache_dir, refresh) {
  detail_urls <- purrr::map_dfr(seq_along(WSCUC_ARCHIVE_URLS), function(i) {
    archive_html <- fetch_html_text(
      WSCUC_ARCHIVE_URLS[[i]],
      paste0("wscuc_commission_actions_archive_", i, ".html"),
      cache_dir,
      refresh = refresh
    )

    links <- stringr::str_match_all(
      archive_html,
      WSCUC_DETAIL_LINK_PATTERN
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

  purrr::map_dfr(detail_urls, function(u) parse_wscuc_detail_page(u, cache_dir, refresh))
}

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

# ---------------------------------------------------------------------------
# SCRAPE-COUNT REGRESSION GUARD
# ---------------------------------------------------------------------------

# Compares freshly scraped row counts per accreditor against the previously
# written output CSV.  Emits a warning for any accreditor whose count has
# dropped by more than `drop_fraction` (default 40%) relative to the prior
# run, as long as the prior run produced at least `min_prior_rows` rows for
# that accreditor.  This catches silent failures where a site's HTML changed
# and the scraper returned an empty result instead of an error.
#
# @param fresh_df      Data frame returned by the combined scrape (must have
#                      an `accreditor` column).
# @param prior_csv     Path to the previously written actions CSV.  If the
#                      file does not exist, the check is skipped silently.
# @param drop_fraction Fractional drop threshold (0–1) that triggers a
#                      warning.  Default 0.4 = warn if count fell by >= 40%.
# @param min_prior_rows Minimum prior-run row count for an accreditor before
#                       the check is applied.  Avoids false positives for
#                       accreditors that routinely have very few actions.
warn_if_scrape_count_dropped <- function(fresh_df,
                                         prior_csv,
                                         drop_fraction = 0.4,
                                         min_prior_rows = 5L) {
  if (!file.exists(prior_csv)) {
    return(invisible(NULL))
  }
  prior_df <- tryCatch(
    readr::read_csv(prior_csv, show_col_types = FALSE),
    error = function(e) NULL
  )
  if (is.null(prior_df) || !("accreditor" %in% names(prior_df))) {
    return(invisible(NULL))
  }

  prior_counts <- table(prior_df$accreditor)
  fresh_counts <- table(fresh_df$accreditor)

  for (acc in names(prior_counts)) {
    prior_n <- as.integer(prior_counts[[acc]])
    if (prior_n < min_prior_rows) next
    fresh_n  <- as.integer(fresh_counts[[acc]] %||% 0L)
    if (fresh_n == 0L || (prior_n - fresh_n) / prior_n >= drop_fraction) {
      warning(sprintf(
        paste(
          "warn_if_scrape_count_dropped: %s row count dropped from %d to %d",
          "(%.0f%% decrease). The accreditor's site structure may have changed,",
          "or the scraper served a stale cache. Validate output before publishing."
        ),
        acc, prior_n, fresh_n,
        100 * (prior_n - fresh_n) / prior_n
      ), call. = FALSE)
    }
  }
  invisible(NULL)
}