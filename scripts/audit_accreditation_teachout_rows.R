main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args <- parse_cli_args(cli_args)
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)
  has_flag <- function(flag) arg_has(args, flag)

  ensure_packages(c("dplyr", "readr"))
  source(file.path(getwd(), "scripts", "shared", "editorial_review_helpers.R"))

  review_candidates_path <- get_arg_value(
    "--review-candidates",
    file.path(getwd(), "data_pipelines", "accreditation", "accreditation_review_candidates.csv")
  )
  overrides_path <- get_arg_value(
    "--overrides",
    file.path(getwd(), "data_pipelines", "accreditation", "editorial_overrides.csv")
  )
  tracker_actions_path <- get_arg_value(
    "--tracker-actions",
    file.path(getwd(), "data_pipelines", "accreditation", "accreditation_tracker_actions_joined.csv")
  )
  output_path <- get_arg_value(
    "--output",
    file.path(getwd(), "data_pipelines", "accreditation", "accreditation_teachout_audit.csv")
  )
  sheet_id_or_url <- get_arg_value("--sheet", Sys.getenv("ACCREDITATION_REVIEW_SHEET_ID", unset = NA_character_))
  sheet_tab <- get_arg_value("--tab", Sys.getenv("ACCREDITATION_REVIEW_SHEET_TAB", unset = "accreditation_review"))
  auth_json <- get_arg_value("--auth-json", Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = NA_character_))
  email <- get_arg_value("--email", NA_character_)
  cache_dir <- get_arg_value("--cache", file.path(getwd(), ".secrets", "googlesheets4"))
  verbose <- has_flag("--verbose")

  empty_audit_rows <- function() {
    data.frame(
      dataset = character(),
      row_reference = character(),
      sheet_row_number = integer(),
      action_id = character(),
      review_status = character(),
      row_origin = character(),
      unitid = character(),
      institution_name = character(),
      accreditor = character(),
      action_date = character(),
      action_type = character(),
      action_label_raw = character(),
      action_label_short = character(),
      notes = character(),
      source_url = character(),
      source_title = character(),
      stringsAsFactors = FALSE
    )
  }

  build_audit_rows <- function(df,
                               dataset,
                               row_reference,
                               action_id,
                               review_status = NULL,
                               row_origin = NULL,
                               unitid = NULL,
                               institution_name = NULL,
                               accreditor = NULL,
                               action_date = NULL,
                               action_type = "action_type",
                               action_label_raw = "action_label_raw",
                               action_label_short = NULL,
                               notes = NULL,
                               source_url = NULL,
                               source_title = NULL,
                               sheet_row_number = NULL) {
    if (is.null(df) || !nrow(df)) {
      return(empty_audit_rows())
    }

    drop_mask <- compute_accreditation_teachout_process_mask(
      df,
      action_type_col = action_type,
      action_label_raw_col = action_label_raw,
      action_label_short_col = action_label_short,
      notes_col = notes
    )
    if (!any(drop_mask)) {
      return(empty_audit_rows())
    }

    value_or_blank <- function(column_name) {
      if (is.null(column_name) || !(column_name %in% names(df))) {
        return(rep("", nrow(df)))
      }
      values <- trim_optional_text(df[[column_name]])
      values[is.na(values)] <- ""
      values
    }

    value_or_blank_int <- function(column_name) {
      if (is.null(column_name) || !(column_name %in% names(df))) {
        return(rep(NA_integer_, nrow(df)))
      }
      suppressWarnings(as.integer(df[[column_name]]))
    }

    audit_rows <- data.frame(
      dataset = rep(dataset, nrow(df)),
      row_reference = value_or_blank(row_reference),
      sheet_row_number = value_or_blank_int(sheet_row_number),
      action_id = value_or_blank(action_id),
      review_status = value_or_blank(review_status),
      row_origin = value_or_blank(row_origin),
      unitid = value_or_blank(unitid),
      institution_name = value_or_blank(institution_name),
      accreditor = value_or_blank(accreditor),
      action_date = value_or_blank(action_date),
      action_type = value_or_blank(action_type),
      action_label_raw = value_or_blank(action_label_raw),
      action_label_short = value_or_blank(action_label_short),
      notes = value_or_blank(notes),
      source_url = value_or_blank(source_url),
      source_title = value_or_blank(source_title),
      stringsAsFactors = FALSE
    )

    audit_rows[drop_mask, , drop = FALSE]
  }

  audit_rows <- list()

  if (file.exists(review_candidates_path)) {
    review_candidates <- read_accreditation_review_candidates(review_candidates_path)
    review_candidates$row_reference <- trim_text(review_candidates$action_id)
    audit_rows[["review_candidates"]] <- build_audit_rows(
      review_candidates,
      dataset = "local_review_candidates",
      row_reference = "row_reference",
      action_id = "action_id",
      row_origin = "row_origin",
      unitid = "unitid",
      institution_name = "institution_name",
      accreditor = "accreditor",
      action_date = "action_date",
      action_type = "action_type",
      action_label_raw = "action_label_raw",
      action_label_short = "generated_statement",
      source_url = "source_url",
      source_title = "source_title"
    )
  }

  if (file.exists(overrides_path)) {
    overrides <- read_accreditation_editorial_overrides(overrides_path)
    override_sheet_view <- build_accreditation_review_sheet_rows(overrides)
    override_sheet_view$row_reference <- trim_text(override_sheet_view$action_id)
    audit_rows[["overrides"]] <- build_audit_rows(
      override_sheet_view,
      dataset = "local_editorial_overrides_effective",
      row_reference = "row_reference",
      action_id = "action_id",
      review_status = "review_status",
      row_origin = "row_origin",
      unitid = "unitid",
      institution_name = "institution_name",
      accreditor = "accreditor",
      action_date = "action_date",
      action_type = "action_type",
      action_label_raw = "action_label_raw",
      action_label_short = "generated_statement",
      source_url = "source_url",
      source_title = "source_title"
    )
  }

  if (file.exists(tracker_actions_path)) {
    tracker_actions <- readr::read_csv(
      tracker_actions_path,
      show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character())
    )
    if (nrow(tracker_actions)) {
      tracker_actions$row_reference <- vapply(
        seq_len(nrow(tracker_actions)),
        function(i) {
          compute_accreditation_action_id(
            unitid = tracker_actions$unitid[[i]],
            accreditor = tracker_actions$accreditor[[i]],
            action_date = tracker_actions$action_date[[i]],
            action_label_raw = tracker_actions$action_label_raw[[i]],
            institution_name = tracker_actions$institution_name[[i]]
          )
        },
        character(1)
      )
    }
    audit_rows[["tracker_actions"]] <- build_audit_rows(
      tracker_actions,
      dataset = "local_tracker_actions_joined",
      row_reference = "row_reference",
      action_id = "row_reference",
      unitid = "unitid",
      institution_name = "institution_name",
      accreditor = "accreditor",
      action_date = "action_date",
      action_type = "action_type",
      action_label_raw = "action_label_raw",
      notes = "notes",
      source_url = "source_url",
      source_title = "source_title"
    )
  }

  sheet_target <- trimws(as.character(sheet_id_or_url %||% ""))
  if (nzchar(sheet_target)) {
    ensure_packages(c("googlesheets4"))
    source(file.path(getwd(), "scripts", "shared", "google_sheets_helpers.R"))

    authenticate_google_sheets(
      auth_json = auth_json,
      email = email,
      cache_dir = cache_dir,
      scopes = "spreadsheets",
      verbose = verbose
    )

    sheet_target <- extract_google_sheet_id(sheet_target)
    sheet_rows <- read_google_sheet_table(
      ss = sheet_target,
      sheet_name = sheet_tab,
      verbose = verbose
    )
    assert_accreditation_review_sheet_header(sheet_rows)

    if (nrow(sheet_rows)) {
      sheet_rows$sheet_row_number <- seq_len(nrow(sheet_rows)) + 1L
    }
    sheet_rows <- coerce_accreditation_review_sheet_rows(sheet_rows)
    if (nrow(sheet_rows)) {
      sheet_rows$sheet_row_number <- seq_len(nrow(sheet_rows)) + 1L
      sheet_rows$row_reference <- paste0("sheet-row-", sheet_rows$sheet_row_number)
    }
    audit_rows[["google_sheet"]] <- build_audit_rows(
      sheet_rows,
      dataset = "google_sheet_accreditation_review",
      row_reference = "row_reference",
      sheet_row_number = "sheet_row_number",
      action_id = "action_id",
      review_status = "review_status",
      row_origin = "row_origin",
      unitid = "unitid",
      institution_name = "institution_name",
      accreditor = "accreditor",
      action_date = "action_date",
      action_type = "action_type",
      action_label_raw = "action_label_raw",
      action_label_short = "generated_statement",
      source_url = "source_url",
      source_title = "source_title"
    )
  }

  detail_rows <- dplyr::bind_rows(audit_rows)
  if (!nrow(detail_rows)) {
    detail_rows <- empty_audit_rows()
  } else {
    detail_rows <- detail_rows %>%
      dplyr::arrange(dataset, accreditor, institution_name, action_date, action_type, action_label_raw)
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write_csv_atomic(detail_rows, output_path)

  if (!nrow(detail_rows)) {
    message("Teach-out audit found 0 accreditation review rows to remove.")
  } else {
    summary_counts <- detail_rows %>%
      dplyr::count(dataset, review_status, name = "teachout_rows", .drop = FALSE)
    for (i in seq_len(nrow(summary_counts))) {
      dataset_label <- summary_counts$dataset[[i]]
      review_label <- trimws(as.character(summary_counts$review_status[[i]] %||% ""))
      if (!nzchar(review_label)) review_label <- "<blank>"
      message(
        sprintf(
          "Teach-out audit: %s / review_status=%s -> %d row(s)",
          dataset_label,
          review_label,
          summary_counts$teachout_rows[[i]]
        )
      )
    }
  }
  message("Saved accreditation teach-out audit to: ", output_path)

  invisible(list(
    rows = nrow(detail_rows),
    output = output_path
  ))
}

if (sys.nframe() == 0) {
  main()
}
