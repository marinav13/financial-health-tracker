main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args <- parse_cli_args(cli_args)
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)

  ensure_packages(c("dplyr", "httr2", "jsonlite", "pdftools", "purrr", "readr", "stringr", "xml2"))
  source(file.path(getwd(), "scripts", "shared", "accreditation_helpers.R"))
  source(file.path(getwd(), "scripts", "shared", "dapip_helpers.R"))
  source(file.path(getwd(), "scripts", "shared", "export_helpers.R"))

  crosswalk_input <- get_arg_value(
    "--crosswalk-input",
    file.path(getwd(), "data_pipelines", "accreditation", "dapip_institution_map.csv")
  )
  output_prefix <- get_arg_value(
    "--output-prefix",
    file.path(getwd(), "data_pipelines", "accreditation", "dapip")
  )
  cache_dir <- get_arg_value(
    "--cache-dir",
    file.path(getwd(), "data_pipelines", "accreditation", "cache", "dapip")
  )
  refresh <- tolower(get_arg_value("--refresh", "true")) %in% c("true", "1", "yes", "y")
  refresh_files <- tolower(get_arg_value("--refresh-files", "false")) %in% c("true", "1", "yes", "y")
  include_review_codes <- arg_has(args, "--include-review-codes")
  min_action_date <- as.Date(get_arg_value("--min-action-date", "2019-01-01"))
  limit <- suppressWarnings(as.integer(get_arg_value("--limit", NA_character_)))
  verbose <- arg_has(args, "--verbose")

  require_existing_local_file(
    crosswalk_input,
    "DAPIP crosswalk input",
    "Run `Rscript --vanilla ./scripts/build_dapip_crosswalk.R` first."
  )

  dir.create(dirname(output_prefix), recursive = TRUE, showWarnings = FALSE)
  ensure_dapip_cache_dirs(cache_dir)

  empty_records <- function() {
    tibble::tibble(
      unitid = character(),
      dapip_id = integer(),
      agency_id = integer(),
      agency_name = character(),
      program_id = integer(),
      program_name = character(),
      sequential_id = integer(),
      accreditation_date = as.Date(character()),
      review_date = as.Date(character()),
      accreditation_end_date = as.Date(character()),
      active_cd = character(),
      existing_action = character(),
      ending_action_id = integer(),
      source_profile_url = character(),
      last_seen_at = character()
    )
  }

  empty_raw_actions <- function() {
    tibble::tibble(
      unitid = character(),
      opeid = character(),
      dapip_id = integer(),
      tracker_institution_name = character(),
      agency_id = integer(),
      agency_name = character(),
      program_id = integer(),
      program_name = character(),
      sequential_id = integer(),
      action_id = integer(),
      action_code = character(),
      action_description = character(),
      action_date = as.Date(character()),
      justification_code = integer(),
      justification_description = character(),
      justification_other = character(),
      is_ongoing = logical(),
      is_ending = logical(),
      action_end_date = as.Date(character()),
      file_id = integer(),
      source_profile_url = character(),
      last_seen_at = character()
    )
  }

  empty_file_rows <- function() {
    tibble::tibble(
      unitid = character(),
      dapip_id = integer(),
      agency_id = integer(),
      program_id = integer(),
      sequential_id = integer(),
      action_id = integer(),
      file_id = integer(),
      file_name = character(),
      file_type = character(),
      linked_file_url = character(),
      cache_pdf_path = character(),
      cache_text_path = character(),
      pdf_md5 = character(),
      file_cache_status = character(),
      text_extract_status = character(),
      text_extract_chars = integer(),
      last_seen_at = character()
    )
  }

  empty_filtered <- function() {
    tibble::tibble(
      unitid = character(),
      institution_name_raw = character(),
      institution_state_raw = character(),
      accreditor = character(),
      action_type = character(),
      action_label_raw = character(),
      action_status = character(),
      action_date = as.Date(character()),
      action_year = integer(),
      action_scope = character(),
      source_url = character(),
      source_title = character(),
      notes = character(),
      last_seen_at = character(),
      source_page_url = character(),
      source_page_modified = character(),
      dapip_id = integer(),
      opeid = character(),
      agency_id = integer(),
      program_id = integer(),
      sequential_id = integer(),
      action_id = integer(),
      action_code = character(),
      justification_code = integer(),
      file_id = integer(),
      label_source = character(),
      review_required = logical(),
      mapped_action_family = character(),
      keep_reason = character(),
      file_cache_path = character(),
      file_text_path = character(),
      parsed_reason_source = character(),
      parsed_reason_snippet = character()
    )
  }

  crosswalk <- readr::read_csv(crosswalk_input, show_col_types = FALSE, progress = FALSE) |>
    dplyr::filter(!is.na(dapip_id)) |>
    dplyr::mutate(
      unitid = as.character(unitid),
      opeid_normalized = normalize_opeid(opeid_normalized %||% opeid),
      dapip_id = suppressWarnings(as.integer(dapip_id))
    )
  if (!is.na(limit) && limit > 0L) {
    crosswalk <- crosswalk |> dplyr::slice_head(n = limit)
  }

  message("Fetching DAPIP action code lookups ...")
  action_lookup <- dapip_fetch_action_code_lookup(cache_dir, refresh = refresh)
  justification_lookup <- dapip_fetch_justification_code_lookup(cache_dir, refresh = refresh)

  record_rows <- list()
  raw_action_rows <- list()
  file_rows <- list()
  filtered_rows <- list()

  message("Fetching DAPIP institutional records and actions ...")
  for (i in seq_len(nrow(crosswalk))) {
    cw <- crosswalk[i, , drop = FALSE]
    if (isTRUE(verbose)) {
      message(sprintf("  %s (%s)", cw$tracker_institution_name[[1]], cw$dapip_id[[1]]))
    }

    records <- tryCatch(
      dapip_fetch_institutional_records(cw$dapip_id[[1]], cache_dir, refresh = refresh),
      error = function(e) {
        warning(sprintf("DAPIP records fetch failed for %s: %s", cw$tracker_institution_name[[1]], conditionMessage(e)), call. = FALSE)
        list()
      }
    )
    if (!is.list(records) || length(records) == 0L) next

    for (record in records) {
      record_rows[[length(record_rows) + 1L]] <- tibble::tibble(
        unitid = cw$unitid[[1]],
        dapip_id = cw$dapip_id[[1]],
        agency_id = suppressWarnings(as.integer(record$AgencyId %||% NA_integer_)),
        agency_name = as.character(record$AgencyName %||% NA_character_),
        program_id = suppressWarnings(as.integer(record$ProgramId %||% NA_integer_)),
        program_name = as.character(record$ProgramName %||% NA_character_),
        sequential_id = suppressWarnings(as.integer(record$SequentialId %||% NA_integer_)),
        accreditation_date = dapip_parse_date(record$AccreditationDate),
        review_date = dapip_parse_date(record$ReviewDate),
        accreditation_end_date = dapip_parse_date(record$AccreditationEndDate),
        active_cd = as.character(record$ActiveCD %||% NA_character_),
        existing_action = as.character(record$ExistingAction %||% NA_character_),
        ending_action_id = suppressWarnings(as.integer(record$EndingActionId %||% NA_integer_)),
        source_profile_url = sprintf("https://ope.ed.gov/dapip/#/institution-profile/%s", as.character(cw$dapip_id[[1]])),
        last_seen_at = as.character(Sys.time())
      )

      actions <- tryCatch(
        dapip_fetch_action_rows(
          unitid = record$UnitId %||% cw$dapip_id[[1]],
          agency_id = record$AgencyId,
          program_id = record$ProgramId,
          sequential_id = record$SequentialId,
          cache_dir = cache_dir,
          refresh = refresh
        ),
        error = function(e) {
          warning(sprintf("DAPIP actions fetch failed for %s / %s: %s", cw$tracker_institution_name[[1]], record$AgencyName %||% "unknown agency", conditionMessage(e)), call. = FALSE)
          list()
        }
      )
      if (!is.list(actions) || length(actions) == 0L) next

      for (action in actions) {
        parsed_action_date <- dapip_parse_date(action$ActionDate)
        if (is.na(parsed_action_date) || parsed_action_date < min_action_date) {
          next
        }

        action_code <- toupper(trimws(as.character(action$ActionCode %||% "")))
        action_desc <- action_lookup$action_description[match(action_code, action_lookup$action_code)][[1]] %||% NA_character_
        justification_code <- suppressWarnings(as.integer(action$JustificationCode %||% NA_integer_))
        justification_desc <- justification_lookup$justification_description[
          match(justification_code, justification_lookup$justification_code)
        ][[1]] %||% NA_character_

        raw_action_rows[[length(raw_action_rows) + 1L]] <- tibble::tibble(
          unitid = cw$unitid[[1]],
          opeid = cw$opeid_normalized[[1]],
          dapip_id = cw$dapip_id[[1]],
          tracker_institution_name = cw$tracker_institution_name[[1]],
          agency_id = suppressWarnings(as.integer(record$AgencyId %||% NA_integer_)),
          agency_name = as.character(record$AgencyName %||% NA_character_),
          program_id = suppressWarnings(as.integer(record$ProgramId %||% NA_integer_)),
          program_name = as.character(record$ProgramName %||% NA_character_),
          sequential_id = suppressWarnings(as.integer(record$SequentialId %||% NA_integer_)),
          action_id = suppressWarnings(as.integer(action$ActionId %||% NA_integer_)),
          action_code = action_code,
          action_description = action_desc,
          action_date = parsed_action_date,
          justification_code = justification_code,
          justification_description = justification_desc,
          justification_other = as.character(action$JustificationOther %||% NA_character_),
          is_ongoing = as.logical(action$IsOngoing %||% NA),
          is_ending = as.logical(action$IsEnding %||% NA),
          action_end_date = dapip_parse_date(action$ActionEndDate),
          file_id = suppressWarnings(as.integer(action$FileId %||% NA_integer_)),
          source_profile_url = sprintf("https://ope.ed.gov/dapip/#/institution-profile/%s", as.character(cw$dapip_id[[1]])),
          last_seen_at = as.character(Sys.time())
        )

        file_meta <- list(
          pdf_path = NA_character_,
          linked_file_url = NA_character_,
          file_name = NA_character_,
          mime_type = NA_character_,
          cache_status = "no_file"
        )
        extracted_text <- list(text = NA_character_, text_path = NA_character_, status = "no_file", chars = NA_integer_)

        file_id <- suppressWarnings(as.integer(action$FileId %||% NA_integer_))
        if (!is.na(file_id) && file_id > 0L) {
          payload <- tryCatch(
            dapip_fetch_action_file_payload(
              unitid = record$UnitId %||% cw$dapip_id[[1]],
              agency_id = record$AgencyId,
              program_id = record$ProgramId,
              sequential_id = record$SequentialId,
              action_id = action$ActionId,
              cache_dir = cache_dir,
              refresh = refresh_files
            ),
            error = function(e) {
              warning(sprintf("DAPIP action file fetch failed for %s action %s: %s", cw$tracker_institution_name[[1]], action$ActionId %||% "unknown", conditionMessage(e)), call. = FALSE)
              NULL
            }
          )

          if (!is.null(payload)) {
            file_meta <- dapip_cache_uploaded_file(
              file_payload = payload,
              cache_dir = cache_dir,
              dapip_id = cw$dapip_id[[1]],
              action_id = action$ActionId,
              file_id = file_id,
              refresh_files = refresh_files
            )
            file_meta$linked_file_url <- payload$LinkedFile$FileUrl %||% NA_character_
            extracted_text <- dapip_extract_pdf_text(
              pdf_path = file_meta$pdf_path,
              text_path = file_meta$text_path,
              refresh_files = refresh_files
            )

            file_rows[[length(file_rows) + 1L]] <- tibble::tibble(
              unitid = cw$unitid[[1]],
              dapip_id = cw$dapip_id[[1]],
              agency_id = suppressWarnings(as.integer(record$AgencyId %||% NA_integer_)),
              program_id = suppressWarnings(as.integer(record$ProgramId %||% NA_integer_)),
              sequential_id = suppressWarnings(as.integer(record$SequentialId %||% NA_integer_)),
              action_id = suppressWarnings(as.integer(action$ActionId %||% NA_integer_)),
              file_id = file_id,
              file_name = file_meta$file_name %||% NA_character_,
              file_type = file_meta$mime_type %||% NA_character_,
              linked_file_url = file_meta$linked_file_url %||% NA_character_,
              cache_pdf_path = file_meta$pdf_path %||% NA_character_,
              cache_text_path = extracted_text$text_path %||% NA_character_,
              pdf_md5 = if (!is.na(file_meta$pdf_path %||% NA_character_) && file.exists(file_meta$pdf_path)) unname(tools::md5sum(file_meta$pdf_path)) else NA_character_,
              file_cache_status = file_meta$cache_status %||% NA_character_,
              text_extract_status = extracted_text$status %||% NA_character_,
              text_extract_chars = suppressWarnings(as.integer(extracted_text$chars %||% NA_integer_)),
              last_seen_at = as.character(Sys.time())
            )
          }
        }

        filtered <- dapip_build_filtered_action_row(
          crosswalk_row = cw,
          record_row = record,
          action_row = action,
          action_lookup = action_lookup,
          justification_lookup = justification_lookup,
          file_meta = file_meta,
          extracted_text = extracted_text
        )
        summary_text <- .select_action_summary_source_text(
          action_label_raw = filtered$action_label_raw[[1]],
          file_text_path = filtered$file_text_path[[1]],
          action_type = filtered$action_type[[1]],
          accreditor = normalize_accreditor_code(filtered$accreditor[[1]]),
          notes = filtered$notes[[1]],
          action_label_source_hint = filtered$label_source[[1]]
        )
        filtered$parsed_reason_source <- .select_action_summary_source_kind(
          action_label_raw = filtered$action_label_raw[[1]],
          file_text_path = filtered$file_text_path[[1]],
          action_type = filtered$action_type[[1]],
          accreditor = normalize_accreditor_code(filtered$accreditor[[1]]),
          notes = filtered$notes[[1]],
          action_label_source_hint = filtered$label_source[[1]]
        )
        filtered$parsed_reason_snippet <- derive_action_label_short(
          action_type = filtered$action_type[[1]],
          action_label_raw = summary_text,
          accreditor = normalize_accreditor_code(filtered$accreditor[[1]]),
          notes = filtered$notes[[1]]
        )
        classed <- dapip_classify_action_code(action_code, action_desc)
        if (isTRUE(classed$keep) || (isTRUE(classed$review_required) && include_review_codes)) {
          filtered_rows[[length(filtered_rows) + 1L]] <- filtered
        }
      }
    }
  }

  records_df <- if (length(record_rows)) dplyr::bind_rows(record_rows) else empty_records()
  raw_df <- if (length(raw_action_rows)) dplyr::bind_rows(raw_action_rows) else empty_raw_actions()
  files_df <- if (length(file_rows)) dplyr::bind_rows(file_rows) else empty_file_rows()
  filtered_df <- if (length(filtered_rows)) dplyr::bind_rows(filtered_rows) else empty_filtered()

  outputs <- list(
    records = paste0(output_prefix, "_institutional_records.csv"),
    raw = paste0(output_prefix, "_action_rows_raw.csv"),
    files = paste0(output_prefix, "_action_files.csv"),
    filtered = paste0(output_prefix, "_action_rows_filtered.csv")
  )

  write_csv_atomic(records_df, outputs$records)
  write_csv_atomic(raw_df, outputs$raw)
  write_csv_atomic(files_df, outputs$files)
  write_csv_atomic(filtered_df, outputs$filtered)

  message(sprintf("Saved DAPIP institutional records to %s", outputs$records))
  message(sprintf("Saved DAPIP raw action rows to %s", outputs$raw))
  message(sprintf("Saved DAPIP action file metadata to %s", outputs$files))
  message(sprintf("Saved DAPIP filtered action rows to %s", outputs$filtered))
  message(sprintf(
    "DAPIP action summary: %d records, %d raw action rows, %d filtered action rows, %d file rows",
    nrow(records_df),
    nrow(raw_df),
    nrow(filtered_df),
    nrow(files_df)
  ))
}

if (sys.nframe() == 0) {
  main()
}
