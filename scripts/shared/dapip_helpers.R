# scripts/shared/dapip_helpers.R
#
# Helper functions for the DAPIP ingestion pipeline.
# These helpers keep the initial DAPIP work isolated from the existing
# accreditation scraper stack so we can audit coverage before changing any
# published exports.

if (!exists("%||%", mode = "function")) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
}

if (!exists("normalize_accreditation_name", mode = "function")) {
  source(file.path(getwd(), "scripts", "shared", "accreditation_helpers.R"))
}

DAPIP_SEARCH_COLUMNS <- c(
  "dapip_id",
  "dapip_institution_name",
  "dapip_state",
  "dapip_opeid",
  "dapip_ipeds_unitid",
  "active_status",
  "institution_type",
  "search_variant",
  "raw_payload"
)

DAPIP_SEARCH_API_BASE <- "https://surveys.ope.ed.gov/dapip/api"
DAPIP_DATA_API_BASE <- "https://ope.ed.gov/dapip/api"
DAPIP_FILES_API_BASE <- "https://surveys.ope.ed.gov/dapip/api"
DAPIP_SEARCH_CACHE_VERSION <- "v2"

empty_dapip_search_results <- function() {
  tibble::tibble(
    dapip_id = integer(),
    dapip_institution_name = character(),
    dapip_state = character(),
    dapip_opeid = character(),
    dapip_ipeds_unitid = character(),
    active_status = character(),
    institution_type = character(),
    search_variant = character(),
    raw_payload = character()
  )
}

normalize_opeid <- function(value) {
  values <- as.character(value %||% "")
  vapply(values, function(one) {
    text <- trimws(as.character(one %||% ""))
    if (!nzchar(text)) return("")
    digits <- gsub("[^0-9]", "", text)
    if (!nzchar(digits)) return("")
    stringr::str_pad(digits, width = 8, side = "left", pad = "0")
  }, character(1), USE.NAMES = FALSE)
}

normalize_opeid_base <- function(value) {
  normalized <- normalize_opeid(value)
  ifelse(nchar(normalized) >= 6L, substr(normalized, 1L, 6L), normalized)
}

slugify_dapip_component <- function(value) {
  text <- trimws(as.character(value %||% ""))
  if (!nzchar(text)) return("unknown")
  text <- tolower(iconv(text, from = "", to = "ASCII//TRANSLIT"))
  text <- gsub("[^a-z0-9]+", "-", text)
  text <- gsub("^-+|-+$", "", text)
  if (!nzchar(text)) "unknown" else text
}

ensure_dapip_cache_dirs <- function(cache_dir) {
  subdirs <- c("search", "institutions", "records", "actions", "files", "pdfs", "text")
  invisible(lapply(file.path(cache_dir, subdirs), dir.create, recursive = TRUE, showWarnings = FALSE))
}

dapip_search_cache_key <- function(unitid = NULL, opeid = NULL, institution_name = NULL, state = NULL) {
  norm_opeid <- normalize_opeid(opeid)
  if (nzchar(norm_opeid)) return(paste0(DAPIP_SEARCH_CACHE_VERSION, "_opeid_", norm_opeid))

  unitid_chr <- trimws(as.character(unitid %||% ""))
  if (nzchar(unitid_chr)) return(paste0(DAPIP_SEARCH_CACHE_VERSION, "_unitid_", unitid_chr))

  paste0(
    DAPIP_SEARCH_CACHE_VERSION,
    "_name_state_",
    slugify_dapip_component(institution_name),
    "__",
    slugify_dapip_component(state)
  )
}

dapip_search_cache_path <- function(cache_dir, unitid = NULL, opeid = NULL, institution_name = NULL, state = NULL) {
  file.path(
    cache_dir,
    "search",
    paste0(dapip_search_cache_key(unitid, opeid, institution_name, state), ".json")
  )
}

dapip_institution_cache_path <- function(cache_dir, dapip_id) {
  file.path(cache_dir, "institutions", paste0(as.character(dapip_id), ".json"))
}

dapip_records_cache_path <- function(cache_dir, dapip_id) {
  file.path(cache_dir, "records", paste0(as.character(dapip_id), ".json"))
}

dapip_actions_cache_path <- function(cache_dir, unitid, agency_id, program_id, sequential_id) {
  file.path(
    cache_dir,
    "actions",
    sprintf(
      "%s_agency-%s_program-%s_seq-%s.json",
      as.character(unitid),
      as.character(agency_id),
      as.character(program_id),
      as.character(sequential_id)
    )
  )
}

dapip_file_cache_path <- function(cache_dir, unitid, agency_id, program_id, sequential_id, action_id) {
  file.path(
    cache_dir,
    "files",
    sprintf(
      "%s_agency-%s_program-%s_seq-%s_action-%s.json",
      as.character(unitid),
      as.character(agency_id),
      as.character(program_id),
      as.character(sequential_id),
      as.character(action_id)
    )
  )
}

dapip_pdf_cache_path <- function(cache_dir, dapip_id, action_id, file_id, file_name = NULL) {
  ext <- tools::file_ext(file_name %||% "")
  if (!nzchar(ext)) ext <- "pdf"
  file.path(
    cache_dir,
    "pdfs",
    sprintf("%s_action-%s_file-%s.%s", as.character(dapip_id), as.character(action_id), as.character(file_id), ext)
  )
}

dapip_text_cache_path <- function(cache_dir, dapip_id, action_id, file_id) {
  file.path(
    cache_dir,
    "text",
    sprintf("%s_action-%s_file-%s.txt", as.character(dapip_id), as.character(action_id), as.character(file_id))
  )
}

dapip_json_cache_read <- function(path) {
  if (!file.exists(path)) return(NULL)
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

dapip_json_cache_write <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(value, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(path)
}

dapip_parse_search_response <- function(payload, search_variant, requested_unitid = NULL, requested_opeid = NULL) {
  if (is.null(payload)) return(empty_dapip_search_results())

  if (is.list(payload) && !is.null(payload$Results) && is.list(payload$Results)) {
    payload <- payload$Results
  }
  if (is.list(payload) && !is.null(payload$results) && is.list(payload$results)) {
    payload <- payload$results
  }
  if (is.list(payload) && !is.null(payload$Items) && is.list(payload$Items)) {
    payload <- payload$Items
  }
  if (!is.list(payload) || length(payload) == 0L) {
    return(empty_dapip_search_results())
  }

  requested_unitid_chr <- trimws(as.character(requested_unitid %||% ""))
  requested_opeid_chr <- normalize_opeid(requested_opeid)

  rows <- purrr::map_dfr(payload, function(item) {
    dapip_id <- item$DapipID %||% item$UnitId %||% item$unitid %||% item$unitId %||% NA
    name <- item$InstitutionName %||% item$institutionName %||% item$LocationName %||% item$locationName %||% NA
    state <- item$State %||% item$state %||% NA
    opeid <- item$OpeID %||% item$OpeId %||% item$opeID %||% item$opeid %||% NA
    ipeds <- item$IpedsUnitId %||% item$IPEDSUnitId %||% item$IpedsUnitID %||% item$ipedsUnitId %||% NA
    active <- item$ActiveStatus %||% item$activeStatus %||% item$Status %||% item$status %||% NA
    institution_type <- item$InstitutionType %||% item$institutionType %||% NA

    parsed_opeid <- normalize_opeid(opeid)
    parsed_ipeds <- trimws(dplyr::coalesce(as.character(ipeds), ""))
    if (!nzchar(parsed_ipeds) && identical(search_variant, "advanced_ipeds_unitid") && nzchar(requested_unitid_chr)) {
      parsed_ipeds <- requested_unitid_chr
    }
    if (!nzchar(parsed_opeid) && identical(search_variant, "advanced_opeid") && nzchar(requested_opeid_chr)) {
      parsed_opeid <- requested_opeid_chr
    }

    tibble::tibble(
      dapip_id = suppressWarnings(as.integer(dapip_id)),
      dapip_institution_name = as.character(name %||% NA_character_),
      dapip_state = as.character(state %||% NA_character_),
      dapip_opeid = parsed_opeid,
      dapip_ipeds_unitid = parsed_ipeds,
      active_status = as.character(active %||% NA_character_),
      institution_type = as.character(institution_type %||% NA_character_),
      search_variant = search_variant,
      raw_payload = as.character(jsonlite::toJSON(item, auto_unbox = TRUE, null = "null"))
    )
  })

  rows |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(dapip_id))
}

dapip_build_search_payloads <- function(unitid = NULL, opeid = NULL, institution_name = NULL, state = NULL) {
  unitid_chr <- trimws(as.character(unitid %||% ""))
  opeid_chr <- normalize_opeid(opeid)

  payloads <- list()

  if (nzchar(opeid_chr)) {
    payloads[[length(payloads) + 1L]] <- list(
      variant = "advanced_opeid",
      endpoint = sprintf("%s/search/advanced", DAPIP_SEARCH_API_BASE),
      body = list(
        OpeID = opeid_chr,
        PageNumber = 1L
      )
    )
  }

  if (nzchar(unitid_chr)) {
    payloads[[length(payloads) + 1L]] <- list(
      variant = "advanced_ipeds_unitid",
      endpoint = sprintf("%s/search/advanced", DAPIP_SEARCH_API_BASE),
      body = list(
        IpedsUnitId = unitid_chr,
        PageNumber = 1L
      )
    )
  }

  # Deliberately omit name-based search payloads here. Manual probes against the
  # DAPIP `search/advanced` endpoint on 2026-04-29 returned an unfiltered 15-row
  # global list for InstitutionName/State lookups, while OPEID and IpedsUnitId
  # lookups on the `surveys.ope.ed.gov` host returned the expected institution.

  payloads
}

dapip_post_json <- function(url, body, timeout = 60) {
  primary_err <- NULL
  result <- tryCatch(
    {
      resp <- httr2::request(url) |>
        httr2::req_user_agent("FinancialHealthProject/1.0") |>
        httr2::req_body_json(body, auto_unbox = TRUE) |>
        httr2::req_timeout(timeout) |>
        httr2::req_perform()
      httr2::resp_body_json(resp, simplifyVector = FALSE)
    },
    error = function(e) {
      primary_err <<- e
      NULL
    }
  )
  if (!is.null(result)) return(result)
  dapip_python_json_request(url, method = "POST", body = body, timeout = timeout, primary_err = primary_err)
}

dapip_get_json <- function(url, timeout = 60) {
  primary_err <- NULL
  result <- tryCatch(
    {
      resp <- httr2::request(url) |>
        httr2::req_user_agent("FinancialHealthProject/1.0") |>
        httr2::req_timeout(timeout) |>
        httr2::req_perform()
      httr2::resp_body_json(resp, simplifyVector = FALSE)
    },
    error = function(e) {
      primary_err <<- e
      NULL
    }
  )
  if (!is.null(result)) return(result)
  dapip_python_json_request(url, method = "GET", body = NULL, timeout = timeout, primary_err = primary_err)
}

dapip_python_json_request <- function(url, method = "GET", body = NULL, timeout = 60, primary_err = NULL) {
  python_bin <- Sys.which(c("python", "python3"))
  python_bin <- python_bin[nzchar(python_bin)][1]
  if (is.na(python_bin) || !nzchar(python_bin)) {
    stop(
      "Failed to fetch ", url, ": ", conditionMessage(primary_err),
      "; python fallback unavailable because no python interpreter was found.",
      call. = FALSE
    )
  }

  py_script <- tempfile("dapip-json-", fileext = ".py")
  response_path <- tempfile("dapip-json-response-", fileext = ".json")
  body_path <- tempfile("dapip-json-body-", fileext = ".json")
  on.exit(unlink(c(py_script, response_path, body_path)), add = TRUE)

  if (!is.null(body)) {
    jsonlite::write_json(body, body_path, auto_unbox = TRUE, pretty = FALSE, null = "null")
  } else {
    writeLines("", body_path, useBytes = TRUE)
  }

  writeLines(
    c(
      "import json",
      "import pathlib",
      "import sys",
      "import urllib.request",
      "method, url, response_path, body_path, timeout_s = sys.argv[1:6]",
      "headers = {'User-Agent': 'FinancialHealthProject/1.0'}",
      "data = None",
      "if method.upper() == 'POST':",
      "    headers['Content-Type'] = 'application/json'",
      "    raw = pathlib.Path(body_path).read_bytes()",
      "    data = raw if raw.strip() else b'{}'",
      "req = urllib.request.Request(url, data=data, headers=headers, method=method.upper())",
      "with urllib.request.urlopen(req, timeout=float(timeout_s)) as resp:",
      "    pathlib.Path(response_path).write_bytes(resp.read())"
    ),
    py_script,
    useBytes = TRUE
  )

  py_out <- tryCatch(
    system2(
      python_bin,
      c(py_script, method, url, response_path, body_path, as.character(timeout)),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(e) structure(conditionMessage(e), status = 1L)
  )
  py_status <- attr(py_out, "status", exact = TRUE)
  if (!is.null(py_status) || !file.exists(response_path)) {
    python_err <- paste(py_out, collapse = "\n")
    stop(
      "Failed to fetch ", url, ": ", conditionMessage(primary_err),
      "; python fallback failed: ", python_err,
      call. = FALSE
    )
  }

  jsonlite::fromJSON(response_path, simplifyVector = FALSE)
}

dapip_search_candidates <- function(unitid = NULL, opeid = NULL,
                                    institution_name = NULL, state = NULL,
                                    cache_dir, refresh = TRUE, verbose = FALSE) {
  ensure_dapip_cache_dirs(cache_dir)
  cache_path <- dapip_search_cache_path(cache_dir, unitid, opeid, institution_name, state)
  if (!refresh && file.exists(cache_path)) {
    cached <- dapip_json_cache_read(cache_path)
    return(dapip_parse_search_response(cached, "cache", unitid, opeid))
  }

  payloads <- dapip_build_search_payloads(unitid, opeid, institution_name, state)
  if (length(payloads) == 0L) {
    return(empty_dapip_search_results())
  }

  last_error <- NULL
  for (payload in payloads) {
    result <- tryCatch(
      dapip_post_json(payload$endpoint, payload$body),
      error = function(e) {
        last_error <<- conditionMessage(e)
        NULL
      }
    )
    if (!is.null(result)) {
      dapip_json_cache_write(result, cache_path)
      parsed <- dapip_parse_search_response(result, payload$variant, unitid, opeid)
      if (nrow(parsed) > 0L) return(parsed)
    } else if (isTRUE(verbose)) {
      message(sprintf("DAPIP search variant failed: %s", payload$variant))
    }
  }

  if (file.exists(cache_path)) {
    cached <- dapip_json_cache_read(cache_path)
    return(dapip_parse_search_response(cached, "cache_fallback", unitid, opeid))
  }

  attr_result <- empty_dapip_search_results()
  attr(attr_result, "dapip_search_error") <- last_error
  attr_result
}

dapip_fetch_institution <- function(dapip_id, cache_dir, refresh = TRUE) {
  ensure_dapip_cache_dirs(cache_dir)
  cache_path <- dapip_institution_cache_path(cache_dir, dapip_id)
  if (!refresh && file.exists(cache_path)) {
    return(dapip_json_cache_read(cache_path))
  }

  url <- sprintf("%s/institutions/%s", DAPIP_DATA_API_BASE, as.character(dapip_id))
  result <- dapip_get_json(url)
  dapip_json_cache_write(result, cache_path)
  result
}

dapip_fetch_institutional_records <- function(dapip_id, cache_dir, refresh = TRUE) {
  ensure_dapip_cache_dirs(cache_dir)
  cache_path <- dapip_records_cache_path(cache_dir, dapip_id)
  if (!refresh && file.exists(cache_path)) {
    return(dapip_json_cache_read(cache_path))
  }

  url <- sprintf("%s/records/institutional/profile/%s", DAPIP_DATA_API_BASE, as.character(dapip_id))
  result <- dapip_get_json(url)
  dapip_json_cache_write(result, cache_path)
  result
}

dapip_fetch_action_rows <- function(unitid, agency_id, program_id, sequential_id, cache_dir, refresh = TRUE) {
  ensure_dapip_cache_dirs(cache_dir)
  cache_path <- dapip_actions_cache_path(cache_dir, unitid, agency_id, program_id, sequential_id)
  if (!refresh && file.exists(cache_path)) {
    return(dapip_json_cache_read(cache_path))
  }

  query <- sprintf(
    "UnitId=%s&AgencyId=%s&ProgramId=%s&SequentialId=%s",
    utils::URLencode(as.character(unitid), reserved = TRUE),
    utils::URLencode(as.character(agency_id), reserved = TRUE),
    utils::URLencode(as.character(program_id), reserved = TRUE),
    utils::URLencode(as.character(sequential_id), reserved = TRUE)
  )
  url <- paste0(sprintf("%s/actions/record/search?", DAPIP_DATA_API_BASE), query)
  result <- dapip_get_json(url)
  dapip_json_cache_write(result, cache_path)
  result
}

dapip_fetch_action_file_payload <- function(unitid, agency_id, program_id, sequential_id, action_id,
                                            cache_dir, refresh = TRUE) {
  ensure_dapip_cache_dirs(cache_dir)
  cache_path <- dapip_file_cache_path(cache_dir, unitid, agency_id, program_id, sequential_id, action_id)
  if (!refresh && file.exists(cache_path)) {
    return(dapip_json_cache_read(cache_path))
  }

  query <- sprintf(
    "UnitId=%s&AgencyId=%s&ProgramId=%s&SequentialId=%s&ActionId=%s",
    utils::URLencode(as.character(unitid), reserved = TRUE),
    utils::URLencode(as.character(agency_id), reserved = TRUE),
    utils::URLencode(as.character(program_id), reserved = TRUE),
    utils::URLencode(as.character(sequential_id), reserved = TRUE),
    utils::URLencode(as.character(action_id), reserved = TRUE)
  )
  url <- paste0(sprintf("%s/actions/files/search?", DAPIP_FILES_API_BASE), query)
  result <- dapip_get_json(url)
  dapip_json_cache_write(result, cache_path)
  result
}

dapip_fetch_dropdown_lookup <- function(endpoint_name, cache_dir, refresh = TRUE) {
  ensure_dapip_cache_dirs(cache_dir)
  cache_path <- file.path(cache_dir, sprintf("%s.json", endpoint_name))
  if (!refresh && file.exists(cache_path)) {
    return(dapip_json_cache_read(cache_path))
  }

  url <- sprintf("%s/dropdowns/%s", DAPIP_DATA_API_BASE, endpoint_name)
  result <- dapip_get_json(url)
  dapip_json_cache_write(result, cache_path)
  result
}

dapip_fetch_action_code_lookup <- function(cache_dir, refresh = TRUE) {
  raw <- dapip_fetch_dropdown_lookup("actionCodes/current", cache_dir, refresh)
  if (!is.list(raw) || length(raw) == 0L) {
    return(tibble::tibble(
      action_code = character(),
      action_description = character(),
      action_type_code = integer()
    ))
  }
  purrr::map_dfr(raw, function(item) {
    tibble::tibble(
      action_code = as.character(item$Code %||% NA_character_),
      action_description = as.character(item$ActionDescription %||% item$Text %||% NA_character_),
      action_type_code = suppressWarnings(as.integer(item$ActionType %||% NA_integer_))
    )
  }) |>
    dplyr::distinct(action_code, .keep_all = TRUE)
}

dapip_fetch_justification_code_lookup <- function(cache_dir, refresh = TRUE) {
  raw <- dapip_fetch_dropdown_lookup("justificationCodes", cache_dir, refresh)
  if (!is.list(raw) || length(raw) == 0L) {
    return(tibble::tibble(
      justification_code = integer(),
      justification_description = character()
    ))
  }
  purrr::map_dfr(raw, function(item) {
    tibble::tibble(
      justification_code = suppressWarnings(as.integer(item$Code %||% item$Id %||% NA_integer_)),
      justification_description = as.character(item$Text %||% item$Description %||% NA_character_)
    )
  }) |>
    dplyr::distinct(justification_code, .keep_all = TRUE)
}

dapip_parse_date <- function(value) {
  text <- trimws(as.character(value %||% ""))
  if (!nzchar(text)) return(as.Date(NA))

  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", text)) {
    return(suppressWarnings(as.Date(text, format = "%Y-%m-%d")))
  }
  if (grepl("^\\d{2}/\\d{2}/\\d{4}$", text)) {
    return(suppressWarnings(as.Date(text, format = "%m/%d/%Y")))
  }
  if (grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}$", text)) {
    return(suppressWarnings(as.Date(text, format = "%Y-%m-%dT%H:%M:%S")))
  }
  if (grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(?:\\.\\d+)?$", text)) {
    return(suppressWarnings(as.Date(text, format = "%Y-%m-%dT%H:%M:%OS")))
  }

  as.Date(NA)
}

dapip_cache_uploaded_file <- function(file_payload, cache_dir, dapip_id, action_id, file_id,
                                      refresh_files = FALSE) {
  uploaded <- file_payload$UploadedFile
  linked <- file_payload$LinkedFile
  file_name <- uploaded$FileName %||% linked$FileUrl %||% sprintf("%s.pdf", file_id)
  pdf_path <- dapip_pdf_cache_path(cache_dir, dapip_id, action_id, file_id, file_name)
  text_path <- dapip_text_cache_path(cache_dir, dapip_id, action_id, file_id)

  cache_status <- "missing"
  if (file.exists(pdf_path) && !refresh_files) {
    cache_status <- "cache_reused"
  } else if (!is.null(uploaded) && !is.null(uploaded$FileData)) {
    dir.create(dirname(pdf_path), recursive = TRUE, showWarnings = FALSE)
    writeBin(jsonlite::base64_dec(uploaded$FileData), pdf_path)
    cache_status <- "downloaded"
  } else if (!is.null(linked) && nzchar(as.character(linked$FileUrl %||% ""))) {
    dir.create(dirname(pdf_path), recursive = TRUE, showWarnings = FALSE)
    tryCatch(
      {
        download_with_retry(linked$FileUrl, pdf_path, mode = "wb", quiet = TRUE, timeout = 60L, retries = 2L)
        cache_status <- "downloaded_linked"
      },
      error = function(e) {
        cache_status <<- paste0("linked_download_failed: ", conditionMessage(e))
      }
    )
  }

  list(
    pdf_path = if (file.exists(pdf_path)) pdf_path else NA_character_,
    text_path = text_path,
    file_name = uploaded$FileName %||% basename(as.character(linked$FileUrl %||% file_name)),
    mime_type = uploaded$FileType %||% file_payload$FileType %||% NA_character_,
    cache_status = cache_status
  )
}

dapip_extract_pdf_text <- function(pdf_path, text_path, refresh_files = FALSE) {
  if (is.na(pdf_path) || !nzchar(pdf_path) || !file.exists(pdf_path)) {
    return(list(text = NA_character_, text_path = text_path, status = "missing_pdf", chars = NA_integer_))
  }

  if (!refresh_files && file.exists(text_path)) {
    text <- readr::read_file(text_path)
    return(list(text = text, text_path = text_path, status = "cache_reused", chars = nchar(text, type = "chars")))
  }

  extracted <- tryCatch(
    {
      pages <- pdftools::pdf_text(pdf_path)
      text <- stringr::str_squish(paste(pages, collapse = " "))
      dir.create(dirname(text_path), recursive = TRUE, showWarnings = FALSE)
      writeLines(text, text_path, useBytes = TRUE)
      list(text = text, text_path = text_path, status = "extracted", chars = nchar(text, type = "chars"))
    },
    error = function(e) {
      list(text = NA_character_, text_path = text_path, status = paste0("extract_failed: ", conditionMessage(e)), chars = NA_integer_)
    }
  )
  extracted
}

dapip_extract_action_label_from_text <- function(text, fallback_label) {
  text_chr <- trimws(as.character(text %||% ""))
  if (!nzchar(text_chr)) return(list(label = fallback_label, label_source = "dapip_action_description"))

  text_chr <- stringr::str_replace_all(text_chr, "\\s+", " ")
  sentences <- unlist(stringr::str_split(text_chr, "(?<=[.!?])\\s+"))
  sentences <- sentences[nchar(sentences) >= 25]
  if (length(sentences) == 0L) {
    return(list(label = fallback_label, label_source = "dapip_action_description"))
  }

  keyword_idx <- which(stringr::str_detect(
    sentences,
    stringr::regex(
      "warning|probation|show cause|withdraw|withdrawal|denied|reaffirmation|monitoring|continued|placed on|surrender|loss of accreditation|closure|cease",
      ignore_case = TRUE
    )
  ))[1]

  if (is.na(keyword_idx)) {
    return(list(label = fallback_label, label_source = "dapip_action_description"))
  }

  label <- sentences[[keyword_idx]]
  if (keyword_idx < length(sentences)) {
    next_sentence <- sentences[[keyword_idx + 1L]]
    if (stringr::str_detect(next_sentence, stringr::regex("^(For |Additional oversight|Fails to meet|Anticipated closure|A Special Committee)", ignore_case = TRUE))) {
      label <- paste(label, next_sentence)
    }
  }

  list(label = stringr::str_squish(label), label_source = "dapip_file_text")
}

dapip_classify_action_code <- function(action_code, action_description = NA_character_) {
  code <- toupper(trimws(as.character(action_code %||% "")))
  desc <- trimws(as.character(action_description %||% ""))

  keep_codes <- c("P", "PW", "SC", "HM", "PN", "WE", "PO", "R", "VR", "LD", "LA", "LO", "AD", "SD", "PR", "WR", "RM", "RS")
  review_codes <- c("DP", "DA")

  action_type <- dplyr::case_when(
    code == "P" ~ "probation",
    code == "PW" ~ "warning",
    code == "SC" ~ "show_cause",
    code %in% c("HM", "PN", "WE", "PO") ~ "notice",
    code %in% c("PR", "WR", "RM", "RS") ~ "removed",
    code %in% c("R", "VR", "LD", "LA", "LO", "AD", "SD", "DA", "DP") ~ "adverse_action",
    TRUE ~ "other"
  )

  action_status <- dplyr::case_when(
    code %in% c("PR", "WR", "RM", "RS") ~ "resolved",
    TRUE ~ "active"
  )

  list(
    action_type = action_type,
    action_status = action_status,
    keep = code %in% keep_codes,
    review_required = code %in% review_codes,
    mapped_action_family = dplyr::case_when(
      code == "P" ~ "probation",
      code == "PW" ~ "warning",
      code == "SC" ~ "show_cause",
      code %in% c("HM", "PN", "WE", "PO") ~ "monitoring_or_notice",
      code %in% c("PR", "WR", "RM", "RS") ~ "removed",
      code %in% c("R", "VR", "LD", "LA", "LO", "AD", "SD", "DA", "DP") ~ "withdrawal_or_loss",
      TRUE ~ "other"
    ),
    keep_reason = dplyr::case_when(
      code %in% keep_codes ~ "public_action_code",
      code %in% review_codes ~ "review_code",
      TRUE ~ "routine_or_unmapped_code"
    )
  )
}

dapip_build_filtered_action_row <- function(crosswalk_row, record_row, action_row,
                                            action_lookup, justification_lookup,
                                            file_meta = NULL, extracted_text = NULL) {
  action_code <- toupper(trimws(as.character(action_row$ActionCode %||% "")))
  action_desc <- action_lookup$action_description[match(action_code, action_lookup$action_code)][[1]] %||% NA_character_
  justification_code <- suppressWarnings(as.integer(action_row$JustificationCode %||% NA_integer_))
  justification_desc <- justification_lookup$justification_description[
    match(justification_code, justification_lookup$justification_code)
  ][[1]] %||% NA_character_
  classed <- dapip_classify_action_code(action_code, action_desc)
  label <- dapip_extract_action_label_from_text(extracted_text$text %||% NA_character_, action_desc %||% action_code)

  tibble::tibble(
    unitid = crosswalk_row$unitid[[1]],
    institution_name_raw = crosswalk_row$tracker_institution_name[[1]],
    institution_state_raw = crosswalk_row$tracker_state[[1]],
    accreditor = as.character(record_row$AgencyName %||% NA_character_),
    action_type = classed$action_type,
    action_label_raw = label$label,
    action_status = classed$action_status,
    action_date = dapip_parse_date(action_row$ActionDate),
    action_year = as.integer(format(dapip_parse_date(action_row$ActionDate), "%Y")),
    action_scope = NA_character_,
    source_url = file_meta$linked_file_url %||% NA_character_,
    source_title = "DAPIP Institutional Accreditation Action",
    notes = stringr::str_squish(paste(
      na.omit(c(
        action_desc,
        justification_desc,
        as.character(action_row$JustificationOther %||% NA_character_)
      )),
      collapse = " | "
    )),
    last_seen_at = as.character(Sys.time()),
    source_page_url = sprintf("https://ope.ed.gov/dapip/#/institution-profile/%s", as.character(crosswalk_row$dapip_id[[1]])),
    source_page_modified = NA_character_,
    dapip_id = suppressWarnings(as.integer(crosswalk_row$dapip_id[[1]])),
    opeid = crosswalk_row$opeid_normalized[[1]],
    agency_id = suppressWarnings(as.integer(record_row$AgencyId %||% NA_integer_)),
    program_id = suppressWarnings(as.integer(record_row$ProgramId %||% NA_integer_)),
    sequential_id = suppressWarnings(as.integer(record_row$SequentialId %||% NA_integer_)),
    action_id = suppressWarnings(as.integer(action_row$ActionId %||% NA_integer_)),
    action_code = action_code,
    justification_code = justification_code,
    file_id = suppressWarnings(as.integer(action_row$FileId %||% NA_integer_)),
    label_source = label$label_source,
    review_required = classed$review_required,
    mapped_action_family = classed$mapped_action_family,
    keep_reason = classed$keep_reason,
    file_cache_path = file_meta$pdf_path %||% NA_character_,
    file_text_path = extracted_text$text_path %||% NA_character_
  )
}

dapip_pick_best_match <- function(candidates, unitid = NULL, opeid = NULL,
                                  institution_name = NULL, state = NULL) {
  if (is.null(candidates) || !is.data.frame(candidates) || nrow(candidates) == 0L) {
    return(NULL)
  }
  if (!"institution_type" %in% names(candidates)) {
    candidates$institution_type <- NA_character_
  }

  norm_name <- normalize_accreditation_name(institution_name %||% "")
  norm_state <- state_name(state %||% "")
  unitid_chr <- trimws(as.character(unitid %||% ""))
  opeid_chr <- normalize_opeid(opeid)
  opeid_base_chr <- normalize_opeid_base(opeid)

  ranked <- candidates |>
    dplyr::mutate(
      candidate_norm_name = normalize_accreditation_name(dapip_institution_name),
      candidate_norm_state = state_name(dapip_state),
      candidate_institution_type = tolower(trimws(dplyr::coalesce(as.character(institution_type), ""))),
      candidate_active_status = tolower(trimws(dplyr::coalesce(as.character(active_status), ""))),
      candidate_opeid_base = normalize_opeid_base(dapip_opeid),
      score = 0L,
      score = score + dplyr::if_else(!is.na(dapip_opeid) & dapip_opeid == opeid_chr & nzchar(opeid_chr), 100L, 0L),
      score = score + dplyr::if_else(
        !is.na(dapip_opeid) &
          dapip_opeid != opeid_chr &
          candidate_opeid_base == opeid_base_chr &
          nzchar(opeid_base_chr),
        95L,
        0L
      ),
      score = score + dplyr::if_else(!is.na(dapip_ipeds_unitid) & dapip_ipeds_unitid == unitid_chr & nzchar(unitid_chr), 90L, 0L),
      score = score + dplyr::if_else(!is.na(candidate_norm_name) & candidate_norm_name == norm_name & nzchar(norm_name), 25L, 0L),
      score = score + dplyr::if_else(!is.na(candidate_norm_state) & candidate_norm_state == norm_state & nzchar(norm_state), 10L, 0L),
      score = score + dplyr::if_else(candidate_active_status %in% c("active", "a"), 2L, 0L),
      score = score + dplyr::if_else(candidate_institution_type == "institution", 5L, 0L),
      score = score - dplyr::if_else(candidate_institution_type == "site", 25L, 0L)
    ) |>
    dplyr::arrange(dplyr::desc(score), dapip_institution_name, dapip_id)

  best <- ranked[1, , drop = FALSE]
  confidence <- dplyr::case_when(
    best$score[[1]] >= 100L ~ "high",
    best$score[[1]] >= 35L ~ "medium",
    best$score[[1]] >= 10L ~ "low",
    TRUE ~ "none"
  )

  best$match_confidence <- confidence
  best
}

dapip_load_manual_aliases <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble(
      unitid = character(),
      opeid = character(),
      tracker_institution_name = character(),
      tracker_state = character(),
      dapip_id = integer(),
      dapip_institution_name = character(),
      dapip_state = character(),
      notes = character()
    ))
  }

  aliases <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  aliases |>
    dplyr::mutate(
      unitid = trimws(as.character(unitid %||% "")),
      opeid = normalize_opeid(opeid),
      tracker_institution_name = as.character(tracker_institution_name %||% ""),
      tracker_state = state_name(tracker_state %||% ""),
      dapip_id = suppressWarnings(as.integer(dapip_id)),
      dapip_institution_name = as.character(dapip_institution_name %||% NA_character_),
      dapip_state = as.character(dapip_state %||% NA_character_)
    )
}

dapip_find_manual_alias <- function(unitid = NULL, opeid = NULL,
                                    institution_name = NULL, state = NULL,
                                    aliases) {
  if (is.null(aliases) || !is.data.frame(aliases) || nrow(aliases) == 0L) return(NULL)

  unitid_chr <- trimws(as.character(unitid %||% ""))
  opeid_chr <- normalize_opeid(opeid)
  norm_name <- normalize_accreditation_name(institution_name %||% "")
  norm_state <- state_name(state %||% "")

  alias_norm_name <- normalize_accreditation_name(aliases$tracker_institution_name %||% "")
  alias_state <- state_name(aliases$tracker_state %||% "")

  matched <- aliases |>
    dplyr::filter(
      (nzchar(unitid_chr) & unitid == unitid_chr) |
        (nzchar(opeid_chr) & opeid == opeid_chr) |
        (alias_norm_name == norm_name & alias_state == norm_state)
    )

  if (nrow(matched) == 0L) return(NULL)
  matched[1, , drop = FALSE]
}

dapip_resolve_crosswalk_row <- function(unitid, opeid, institution_name, state,
                                        aliases, cache_dir, refresh = TRUE,
                                        verbose = FALSE,
                                        search_fn = dapip_search_candidates) {
  manual <- dapip_find_manual_alias(unitid, opeid, institution_name, state, aliases)
  if (!is.null(manual)) {
    return(list(
      matched = TRUE,
      match_method = "manual_alias",
      match_confidence = "high",
      manual_override_used = TRUE,
      dapip_id = suppressWarnings(as.integer(manual$dapip_id[[1]])),
      dapip_institution_name = manual$dapip_institution_name[[1]] %||% NA_character_,
      dapip_state = manual$dapip_state[[1]] %||% NA_character_,
      match_notes = manual$notes[[1]] %||% NA_character_,
      search_error = NA_character_
    ))
  }

  candidates <- search_fn(
    unitid = unitid,
    opeid = opeid,
    institution_name = institution_name,
    state = state,
    cache_dir = cache_dir,
    refresh = refresh,
    verbose = verbose
  )
  best <- dapip_pick_best_match(candidates, unitid, opeid, institution_name, state)
  best_confidence <- if (is.null(best)) "none" else as.character(best$match_confidence[[1]] %||% "none")
  rejected_low_confidence <- identical(best_confidence, "low")
  best_state <- if (is.null(best)) "" else state_name(best$dapip_state[[1]] %||% "")
  target_state <- state_name(state %||% "")
  rejected_state_mismatch <- nzchar(target_state) && nzchar(best_state) && !identical(best_state, target_state)
  if (is.null(best) || best_confidence %in% c("none", "low")) {
    search_error <- attr(candidates, "dapip_search_error", exact = TRUE) %||% NA_character_
    match_notes <- if (rejected_low_confidence) {
      sprintf(
        "Rejected low-confidence DAPIP candidate %s for %s (%s).",
        as.character(best$dapip_id[[1]] %||% NA_character_),
        as.character(institution_name %||% NA_character_),
        as.character(state %||% NA_character_)
      )
    } else {
      NA_character_
    }
    return(list(
      matched = FALSE,
      match_method = "unmatched",
      match_confidence = "none",
      manual_override_used = FALSE,
      dapip_id = NA_integer_,
      dapip_institution_name = NA_character_,
      dapip_state = NA_character_,
      match_notes = match_notes,
      search_error = search_error
    ))
  }

  if (rejected_state_mismatch) {
    return(list(
      matched = FALSE,
      match_method = "unmatched",
      match_confidence = "none",
      manual_override_used = FALSE,
      dapip_id = NA_integer_,
      dapip_institution_name = NA_character_,
      dapip_state = NA_character_,
      match_notes = sprintf(
        "Rejected DAPIP candidate %s because state %s did not match tracker state %s.",
        as.character(best$dapip_id[[1]] %||% NA_character_),
        as.character(best$dapip_state[[1]] %||% NA_character_),
        as.character(state %||% NA_character_)
      ),
      search_error = attr(candidates, "dapip_search_error", exact = TRUE) %||% NA_character_
    ))
  }

  match_method <- dplyr::case_when(
    !is.na(best$dapip_opeid[[1]]) &&
      (
        (best$dapip_opeid[[1]] == normalize_opeid(opeid) && nzchar(normalize_opeid(opeid))) ||
        (normalize_opeid_base(best$dapip_opeid[[1]]) == normalize_opeid_base(opeid) && nzchar(normalize_opeid_base(opeid)))
      ) ~ "opeid",
    !is.na(best$dapip_ipeds_unitid[[1]]) && best$dapip_ipeds_unitid[[1]] == trimws(as.character(unitid %||% "")) && nzchar(trimws(as.character(unitid %||% ""))) ~ "unitid",
    TRUE ~ "name_state"
  )

  list(
    matched = TRUE,
    match_method = match_method,
    match_confidence = best$match_confidence[[1]],
    manual_override_used = FALSE,
    dapip_id = suppressWarnings(as.integer(best$dapip_id[[1]])),
    dapip_institution_name = best$dapip_institution_name[[1]] %||% NA_character_,
    dapip_state = best$dapip_state[[1]] %||% NA_character_,
    match_notes = NA_character_,
    search_error = NA_character_
  )
}
