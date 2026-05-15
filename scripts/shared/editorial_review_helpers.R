ACCREDITATION_REVIEW_CANDIDATE_COLUMNS <- c(
  "action_id",
  "unitid",
  "institution_name",
  "accreditor",
  "action_date",
  "action_type",
  "action_label_raw",
  "generated_statement",
  "source_url",
  "source_title",
  "row_origin"
)

ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS <- c(
  ACCREDITATION_REVIEW_CANDIDATE_COLUMNS,
  "first_seen",
  "review_status",
  "editor_action_label_short",
  "editor_action_date",
  "editor_action_type",
  "editor_source_url",
  "editor_source_title",
  "editor_notes",
  "reviewer",
  "reviewed_at",
  "grandfathered"
)

ACCREDITATION_REVIEW_EDITOR_COLUMNS <- c(
  "review_status",
  "editor_action_label_short",
  "editor_action_date",
  "editor_action_type",
  "editor_source_url",
  "editor_source_title",
  "editor_notes",
  "reviewer",
  "reviewed_at"
)

ACCREDITATION_REVIEW_SYSTEM_COLUMNS <- setdiff(
  ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS,
  ACCREDITATION_REVIEW_EDITOR_COLUMNS
)

ACCREDITATION_REVIEW_CANDIDATE_COL_TYPES <- readr::cols(
  .default = readr::col_character()
)

ACCREDITATION_EDITORIAL_OVERRIDE_COL_TYPES <- readr::cols(
  .default = readr::col_character(),
  grandfathered = readr::col_logical()
)

empty_accreditation_review_candidates <- function() {
  data.frame(
    action_id = character(),
    unitid = character(),
    institution_name = character(),
    accreditor = character(),
    action_date = character(),
    action_type = character(),
    action_label_raw = character(),
    generated_statement = character(),
    source_url = character(),
    source_title = character(),
    row_origin = character(),
    stringsAsFactors = FALSE
  )
}

empty_accreditation_editorial_overrides <- function() {
  data.frame(
    action_id = character(),
    unitid = character(),
    institution_name = character(),
    accreditor = character(),
    action_date = character(),
    action_type = character(),
    action_label_raw = character(),
    generated_statement = character(),
    source_url = character(),
    source_title = character(),
    row_origin = character(),
    first_seen = character(),
    review_status = character(),
    editor_action_label_short = character(),
    editor_action_date = character(),
    editor_action_type = character(),
    editor_source_url = character(),
    editor_source_title = character(),
    editor_notes = character(),
    reviewer = character(),
    reviewed_at = character(),
    grandfathered = logical(),
    stringsAsFactors = FALSE
  )
}

read_accreditation_review_candidates <- function(path) {
  coerce_accreditation_review_candidates(
    readr::read_csv(
      path,
      show_col_types = FALSE,
      col_types = ACCREDITATION_REVIEW_CANDIDATE_COL_TYPES
    )
  )
}

read_accreditation_editorial_overrides <- function(path) {
  coerce_accreditation_editorial_overrides(
    readr::read_csv(
      path,
      show_col_types = FALSE,
      col_types = ACCREDITATION_EDITORIAL_OVERRIDE_COL_TYPES
    )
  )
}

normalize_review_identity_text <- function(x) {
  value <- tolower(trimws(as.character(x %||% "")))
  value <- gsub("[^a-z0-9]+", " ", value, perl = TRUE)
  trimws(gsub("\\s+", " ", value, perl = TRUE))
}

compute_accreditation_action_id <- function(unitid,
                                            accreditor,
                                            action_date,
                                            action_label_raw,
                                            export_unitid = NA_character_) {
  identity_unitid <- trimws(as.character(unitid %||% ""))
  if (!nzchar(identity_unitid)) {
    identity_unitid <- trimws(as.character(export_unitid %||% ""))
  }

  seed <- paste(
    normalize_review_identity_text(identity_unitid),
    normalize_review_identity_text(accreditor),
    normalize_review_identity_text(action_date),
    normalize_review_identity_text(action_label_raw),
    sep = "|"
  )

  substr(digest::digest(seed, algo = "sha1", serialize = FALSE), 1L, 12L)
}

assert_unique_action_ids <- function(df, label) {
  if (!"action_id" %in% names(df) || !nrow(df)) return(invisible(df))
  ids <- trimws(as.character(df$action_id %||% ""))
  duplicate_ids <- unique(ids[nzchar(ids) & duplicated(ids)])
  if (length(duplicate_ids) > 0L) {
    stop(
      sprintf(
        "%s contains duplicate action_id values: %s",
        label,
        paste(duplicate_ids, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(df)
}

coerce_accreditation_review_candidates <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(empty_accreditation_review_candidates())
  }

  missing_columns <- setdiff(ACCREDITATION_REVIEW_CANDIDATE_COLUMNS, names(df))
  if ("generated_statement" %in% missing_columns && "visible_statement" %in% names(df)) {
    df$generated_statement <- df$visible_statement
    missing_columns <- setdiff(ACCREDITATION_REVIEW_CANDIDATE_COLUMNS, names(df))
  }
  if (length(missing_columns) > 0L) {
    stop(
      sprintf(
        "Accreditation review candidates are missing required columns: %s",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  candidates <- data.frame(
    action_id = trimws(as.character(df$action_id %||% "")),
    unitid = dplyr::na_if(trimws(as.character(df$unitid %||% "")), ""),
    institution_name = dplyr::na_if(trimws(as.character(df$institution_name %||% "")), ""),
    accreditor = dplyr::na_if(trimws(as.character(df$accreditor %||% "")), ""),
    action_date = dplyr::na_if(trimws(as.character(df$action_date %||% "")), ""),
    action_type = dplyr::na_if(trimws(as.character(df$action_type %||% "")), ""),
    action_label_raw = dplyr::na_if(trimws(as.character(df$action_label_raw %||% "")), ""),
    generated_statement = dplyr::na_if(trimws(as.character(df$generated_statement %||% "")), ""),
    source_url = dplyr::na_if(trimws(as.character(df$source_url %||% "")), ""),
    source_title = dplyr::na_if(trimws(as.character(df$source_title %||% "")), ""),
    row_origin = dplyr::na_if(trimws(as.character(df$row_origin %||% "")), ""),
    stringsAsFactors = FALSE
  )

  assert_unique_action_ids(candidates, "Accreditation review candidates")
  candidates
}

build_accreditation_review_candidates <- function(actions_df) {
  if (is.null(actions_df) || !nrow(actions_df)) {
    return(empty_accreditation_review_candidates())
  }

  required_columns <- c(
    "export_unitid",
    "unitid",
    "export_institution_name",
    "accreditor",
    "action_date",
    "action_type",
    "action_label_raw",
    "action_label_short",
    "source_url",
    "source_title",
    "source_page_url"
  )
  missing_columns <- setdiff(required_columns, names(actions_df))
  if (length(missing_columns) > 0L) {
    stop(
      sprintf(
        "build_accreditation_review_candidates requires these columns: %s",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  candidates <- data.frame(
    action_id = vapply(
      seq_len(nrow(actions_df)),
      function(i) compute_accreditation_action_id(
        unitid = actions_df$unitid[[i]],
        accreditor = actions_df$accreditor[[i]],
        action_date = actions_df$action_date[[i]],
        action_label_raw = actions_df$action_label_raw[[i]],
        export_unitid = actions_df$export_unitid[[i]]
      ),
      character(1)
    ),
    unitid = dplyr::na_if(trimws(as.character(actions_df$unitid %||% "")), ""),
    institution_name = dplyr::na_if(trimws(as.character(actions_df$export_institution_name %||% "")), ""),
    accreditor = dplyr::na_if(trimws(as.character(actions_df$accreditor %||% "")), ""),
    action_date = dplyr::na_if(trimws(as.character(actions_df$action_date %||% "")), ""),
    action_type = dplyr::na_if(trimws(as.character(actions_df$action_type %||% "")), ""),
    action_label_raw = dplyr::na_if(trimws(as.character(actions_df$action_label_raw %||% "")), ""),
    generated_statement = dplyr::coalesce(
      dplyr::na_if(trimws(as.character(actions_df$action_label_short %||% "")), ""),
      dplyr::na_if(trimws(as.character(actions_df$action_label_raw %||% "")), "")
    ),
    source_url = dplyr::coalesce(
      dplyr::na_if(trimws(as.character(actions_df$source_url %||% "")), ""),
      dplyr::na_if(trimws(as.character(actions_df$source_page_url %||% "")), "")
    ),
    source_title = dplyr::na_if(trimws(as.character(actions_df$source_title %||% "")), ""),
    row_origin = rep("scraper", nrow(actions_df)),
    stringsAsFactors = FALSE
  )

  candidates <- candidates[!duplicated(candidates$action_id), ACCREDITATION_REVIEW_CANDIDATE_COLUMNS, drop = FALSE]
  assert_unique_action_ids(candidates, "Accreditation review candidates")
  candidates
}

coerce_accreditation_editorial_overrides <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(empty_accreditation_editorial_overrides())
  }

  overrides <- df
  defaults <- list(
    action_id = NA_character_,
    unitid = NA_character_,
    institution_name = NA_character_,
    accreditor = NA_character_,
    action_date = NA_character_,
    action_type = NA_character_,
    action_label_raw = NA_character_,
    generated_statement = NA_character_,
    source_url = NA_character_,
    source_title = NA_character_,
    row_origin = NA_character_,
    first_seen = NA_character_,
    review_status = NA_character_,
    editor_action_label_short = NA_character_,
    editor_action_date = NA_character_,
    editor_action_type = NA_character_,
    editor_source_url = NA_character_,
    editor_source_title = NA_character_,
    editor_notes = NA_character_,
    reviewer = NA_character_,
    reviewed_at = NA_character_,
    grandfathered = FALSE
  )
  for (column_name in ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS) {
    if (column_name %in% names(overrides)) next
    overrides[[column_name]] <- defaults[[column_name]]
  }

  if (!"generated_statement" %in% names(overrides) && "visible_statement" %in% names(overrides)) {
    overrides$generated_statement <- overrides$visible_statement
  }

  if ("editor_rewrite" %in% names(overrides)) {
    legacy_rewrite <- dplyr::na_if(trimws(as.character(overrides$editor_rewrite %||% "")), "")
    current_short <- dplyr::na_if(trimws(as.character(overrides$editor_action_label_short %||% "")), "")
    overrides$editor_action_label_short <- dplyr::coalesce(current_short, legacy_rewrite)
  }

  overrides <- overrides[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]
  character_columns <- setdiff(ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, "grandfathered")
  for (column_name in character_columns) {
    overrides[[column_name]] <- dplyr::na_if(trimws(as.character(overrides[[column_name]] %||% "")), "")
  }
  overrides$action_id[is.na(overrides$action_id)] <- ""
  overrides$grandfathered <- as.logical(overrides$grandfathered)
  overrides$grandfathered[is.na(overrides$grandfathered)] <- FALSE
  assert_unique_action_ids(overrides, "Accreditation editorial overrides")
  overrides
}

assert_accreditation_review_sheet_header <- function(df) {
  if (is.null(df) || !ncol(df)) {
    return(invisible(df))
  }

  header_names <- names(df)
  if (!"generated_statement" %in% header_names && "visible_statement" %in% header_names) {
    header_names <- union(setdiff(header_names, "visible_statement"), "generated_statement")
  }

  missing_columns <- setdiff(ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, header_names)
  if (length(missing_columns) > 0L) {
    stop(
      sprintf(
        "Google Sheet tab is missing required columns: %s",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(df)
}

stage_accreditation_editorial_overrides <- function(candidates,
                                                    existing = NULL,
                                                    first_seen = as.character(Sys.Date())) {
  review_candidates <- coerce_accreditation_review_candidates(candidates)
  overrides <- coerce_accreditation_editorial_overrides(existing)

  if (!nrow(review_candidates)) {
    return(overrides)
  }

  if (nrow(overrides)) {
    existing_ids <- trimws(as.character(overrides$action_id %||% ""))
    candidate_index <- match(existing_ids, review_candidates$action_id)
    matched <- !is.na(candidate_index)

    if (any(matched)) {
      for (column_name in ACCREDITATION_REVIEW_CANDIDATE_COLUMNS) {
        overrides[[column_name]][matched] <- review_candidates[[column_name]][candidate_index[matched]]
      }
    }
  }

  new_rows <- review_candidates[!(review_candidates$action_id %in% overrides$action_id), , drop = FALSE]
  if (!nrow(new_rows)) {
    overrides <- coerce_accreditation_editorial_overrides(overrides)
    return(overrides[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])
  }
  new_rows$first_seen <- first_seen
  new_rows$review_status <- "unreviewed"
  new_rows$editor_action_label_short <- NA_character_
  new_rows$editor_action_date <- NA_character_
  new_rows$editor_action_type <- NA_character_
  new_rows$editor_source_url <- NA_character_
  new_rows$editor_source_title <- NA_character_
  new_rows$editor_notes <- NA_character_
  new_rows$reviewer <- NA_character_
  new_rows$reviewed_at <- NA_character_
  new_rows$grandfathered <- FALSE

  combined <- if (nrow(new_rows)) {
    dplyr::bind_rows(overrides, new_rows)
  } else {
    overrides
  }
  combined <- coerce_accreditation_editorial_overrides(combined)
  combined[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]
}

build_accreditation_review_sheet_append_rows <- function(overrides, existing_sheet = NULL) {
  local_rows <- coerce_accreditation_editorial_overrides(overrides)
  sheet_rows <- coerce_accreditation_editorial_overrides(existing_sheet)

  if (!nrow(local_rows)) {
    return(local_rows)
  }

  if (!nrow(sheet_rows)) {
    return(local_rows[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])
  }

  local_rows[
    !(local_rows$action_id %in% sheet_rows$action_id),
    ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS,
    drop = FALSE
  ]
}

merge_accreditation_review_sheet_editor_columns <- function(overrides,
                                                            sheet_rows,
                                                            allow_editor_added_rows = FALSE) {
  local_rows <- coerce_accreditation_editorial_overrides(overrides)
  sheet_data <- coerce_accreditation_editorial_overrides(sheet_rows)

  if (!nrow(local_rows) || !nrow(sheet_data)) {
    return(local_rows[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])
  }

  sheet_ids <- trimws(as.character(sheet_data$action_id %||% ""))
  local_ids <- trimws(as.character(local_rows$action_id %||% ""))
  sheet_only <- sheet_data[nzchar(sheet_ids) & !(sheet_ids %in% local_ids), , drop = FALSE]

  if (nrow(sheet_only) > 0L && !isTRUE(allow_editor_added_rows)) {
    sample_ids <- paste(utils::head(sheet_only$action_id, 5L), collapse = ", ")
    stop(
      sprintf(
        paste(
          "Google Sheet contains %d action_id value(s) that are not present in editorial_overrides.csv.",
          "Editor-added rows are not supported by this sync path yet.",
          "Sample action_id values: %s"
        ),
        nrow(sheet_only),
        sample_ids
      ),
      call. = FALSE
    )
  }

  match_index <- match(local_rows$action_id, sheet_data$action_id)
  matched <- !is.na(match_index)
  if (!any(matched)) {
    return(local_rows[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])
  }

  for (column_name in ACCREDITATION_REVIEW_EDITOR_COLUMNS) {
    local_rows[[column_name]][matched] <- sheet_data[[column_name]][match_index[matched]]
  }

  local_rows[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]
}

grandfather_accreditation_editorial_overrides <- function(overrides,
                                                          reviewed_at = as.character(Sys.Date()),
                                                          reviewer = "grandfathered",
                                                          only_statuses = c(NA_character_, "", "unreviewed")) {
  local_rows <- coerce_accreditation_editorial_overrides(overrides)
  if (!nrow(local_rows)) {
    return(local_rows[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])
  }

  status_values <- trimws(as.character(local_rows$review_status %||% ""))
  status_values[is.na(local_rows$review_status)] <- ""
  eligible_statuses <- trimws(as.character(only_statuses %||% ""))
  eligible_statuses[is.na(eligible_statuses)] <- ""
  to_grandfather <- status_values %in% eligible_statuses

  if (!any(to_grandfather)) {
    return(local_rows[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])
  }

  local_rows$review_status[to_grandfather] <- "approved"
  local_rows$grandfathered[to_grandfather] <- TRUE

  reviewed_at_value <- trimws(as.character(reviewed_at %||% ""))
  reviewer_value <- trimws(as.character(reviewer %||% ""))

  if (nzchar(reviewed_at_value)) {
    local_rows$reviewed_at[to_grandfather] <- reviewed_at_value
  }
  if (nzchar(reviewer_value)) {
    local_rows$reviewer[to_grandfather] <- reviewer_value
  }

  local_rows[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]
}

apply_accreditation_editorial_overrides <- function(actions_df,
                                                    overrides = NULL,
                                                    enforce_review_gate = FALSE) {
  if (is.null(actions_df) || !nrow(actions_df)) {
    return(actions_df)
  }

  required_columns <- c(
    "unitid",
    "export_unitid",
    "accreditor",
    "action_date",
    "action_type",
    "action_label_raw",
    "action_label_short",
    "source_url",
    "source_title"
  )
  missing_columns <- setdiff(required_columns, names(actions_df))
  if (length(missing_columns) > 0L) {
    stop(
      sprintf(
        "apply_accreditation_editorial_overrides requires these columns: %s",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  review_actions <- actions_df
  review_actions$action_id <- vapply(
    seq_len(nrow(review_actions)),
    function(i) compute_accreditation_action_id(
      unitid = review_actions$unitid[[i]],
      accreditor = review_actions$accreditor[[i]],
      action_date = review_actions$action_date[[i]],
      action_label_raw = review_actions$action_label_raw[[i]],
      export_unitid = review_actions$export_unitid[[i]]
    ),
    character(1)
  )

  override_rows <- coerce_accreditation_editorial_overrides(overrides)
  if (!nrow(override_rows)) {
    if (isTRUE(enforce_review_gate)) {
      stop(
        "Review gate is enabled but editorial_overrides.csv is empty or missing.",
        call. = FALSE
      )
    }
    return(review_actions)
  }

  joined <- review_actions %>%
    dplyr::left_join(
      override_rows[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE],
      by = "action_id",
      suffix = c("", "_override")
    )

  if (isTRUE(enforce_review_gate)) {
    missing_override <- is.na(joined$review_status)
    if (any(missing_override)) {
      sample_ids <- paste(utils::head(joined$action_id[missing_override], 5L), collapse = ", ")
      stop(
        sprintf(
          paste(
            "Review gate is enabled but %d accreditation action(s) are missing editorial overrides.",
            "Sample action_id values: %s"
          ),
          sum(missing_override),
          sample_ids
        ),
        call. = FALSE
      )
    }
  }

  joined$action_label_short <- dplyr::coalesce(
    joined$editor_action_label_short,
    joined$generated_statement,
    joined$action_label_short
  )
  joined$action_date <- dplyr::coalesce(joined$editor_action_date, joined$action_date)
  joined$action_type <- dplyr::coalesce(joined$editor_action_type, joined$action_type)
  joined$source_url <- dplyr::coalesce(joined$editor_source_url, joined$source_url)
  joined$source_title <- dplyr::coalesce(joined$editor_source_title, joined$source_title)

  if (isTRUE(enforce_review_gate)) {
    joined <- joined[joined$review_status == "approved", , drop = FALSE]
  }

  joined
}

COLLEGE_CUTS_REVIEW_CANDIDATE_COLUMNS <- c(
  "cut_id",
  "unitid",
  "institution_name",
  "state",
  "announcement_date",
  "announcement_year",
  "cut_type",
  "program_name",
  "source_url",
  "source_title",
  "source_publication",
  "row_origin"
)

COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS <- c(
  COLLEGE_CUTS_REVIEW_CANDIDATE_COLUMNS,
  "first_seen",
  "review_status",
  "editor_program_name",
  "editor_announcement_date",
  "editor_cut_type",
  "editor_source_url",
  "editor_source_title",
  "editor_source_publication",
  "editor_notes",
  "reviewer",
  "reviewed_at",
  "grandfathered"
)

COLLEGE_CUTS_REVIEW_EDITOR_COLUMNS <- c(
  "review_status",
  "editor_program_name",
  "editor_announcement_date",
  "editor_cut_type",
  "editor_source_url",
  "editor_source_title",
  "editor_source_publication",
  "editor_notes",
  "reviewer",
  "reviewed_at"
)

COLLEGE_CUTS_REVIEW_SYSTEM_COLUMNS <- setdiff(
  COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS,
  COLLEGE_CUTS_REVIEW_EDITOR_COLUMNS
)

COLLEGE_CUTS_REVIEW_CANDIDATE_COL_TYPES <- readr::cols(
  .default = readr::col_character()
)

COLLEGE_CUTS_EDITORIAL_OVERRIDE_COL_TYPES <- readr::cols(
  .default = readr::col_character(),
  grandfathered = readr::col_logical()
)

COLLEGE_CUTS_SHEET_DISPLAY_NAMES <- c(
  program_name = "cut_description",
  editor_program_name = "editor_cut_description"
)

COLLEGE_CUTS_SHEET_HEADER_ALIASES <- stats::setNames(
  names(COLLEGE_CUTS_SHEET_DISPLAY_NAMES),
  COLLEGE_CUTS_SHEET_DISPLAY_NAMES
)

normalize_college_cuts_sheet_headers <- function(df) {
  if (is.null(df) || !ncol(df)) {
    return(df)
  }

  normalized <- df
  for (sheet_name in names(COLLEGE_CUTS_SHEET_HEADER_ALIASES)) {
    internal_name <- COLLEGE_CUTS_SHEET_HEADER_ALIASES[[sheet_name]]
    if (!(sheet_name %in% names(normalized))) next
    if (internal_name %in% names(normalized)) next
    names(normalized)[names(normalized) == sheet_name] <- internal_name
  }
  normalized
}

format_college_cuts_sheet_headers <- function(df) {
  if (is.null(df) || !ncol(df)) {
    return(df)
  }

  formatted <- df
  for (internal_name in names(COLLEGE_CUTS_SHEET_DISPLAY_NAMES)) {
    if (!(internal_name %in% names(formatted))) next
    names(formatted)[names(formatted) == internal_name] <- COLLEGE_CUTS_SHEET_DISPLAY_NAMES[[internal_name]]
  }
  formatted
}

empty_college_cuts_review_candidates <- function() {
  data.frame(
    cut_id = character(),
    unitid = character(),
    institution_name = character(),
    state = character(),
    announcement_date = character(),
    announcement_year = character(),
    cut_type = character(),
    program_name = character(),
    source_url = character(),
    source_title = character(),
    source_publication = character(),
    row_origin = character(),
    stringsAsFactors = FALSE
  )
}

empty_college_cuts_editorial_overrides <- function() {
  data.frame(
    cut_id = character(),
    unitid = character(),
    institution_name = character(),
    state = character(),
    announcement_date = character(),
    announcement_year = character(),
    cut_type = character(),
    program_name = character(),
    source_url = character(),
    source_title = character(),
    source_publication = character(),
    row_origin = character(),
    first_seen = character(),
    review_status = character(),
    editor_program_name = character(),
    editor_announcement_date = character(),
    editor_cut_type = character(),
    editor_source_url = character(),
    editor_source_title = character(),
    editor_source_publication = character(),
    editor_notes = character(),
    reviewer = character(),
    reviewed_at = character(),
    grandfathered = logical(),
    stringsAsFactors = FALSE
  )
}

read_college_cuts_review_candidates <- function(path) {
  coerce_college_cuts_review_candidates(
    readr::read_csv(
      path,
      show_col_types = FALSE,
      col_types = COLLEGE_CUTS_REVIEW_CANDIDATE_COL_TYPES
    )
  )
}

read_college_cuts_editorial_overrides <- function(path) {
  coerce_college_cuts_editorial_overrides(
    readr::read_csv(
      path,
      show_col_types = FALSE,
      col_types = COLLEGE_CUTS_EDITORIAL_OVERRIDE_COL_TYPES
    )
  )
}

compute_college_cuts_review_id <- function(cut_id,
                                           unitid = NA_character_,
                                           announcement_date = NA_character_,
                                           program_name = NA_character_) {
  cut_id_value <- trimws(as.character(cut_id %||% ""))
  if (nzchar(cut_id_value)) {
    return(cut_id_value)
  }

  seed <- paste(
    normalize_review_identity_text(unitid),
    normalize_review_identity_text(announcement_date),
    normalize_review_identity_text(program_name),
    sep = "|"
  )
  paste0("editor-", substr(digest::digest(seed, algo = "sha1", serialize = FALSE), 1L, 12L))
}

assert_unique_cut_ids <- function(df, label) {
  if (!"cut_id" %in% names(df) || !nrow(df)) return(invisible(df))
  ids <- trimws(as.character(df$cut_id %||% ""))
  duplicate_ids <- unique(ids[nzchar(ids) & duplicated(ids)])
  if (length(duplicate_ids) > 0L) {
    stop(
      sprintf(
        "%s contains duplicate cut_id values: %s",
        label,
        paste(duplicate_ids, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(df)
}

coerce_college_cuts_review_candidates <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(empty_college_cuts_review_candidates())
  }

  missing_columns <- setdiff(COLLEGE_CUTS_REVIEW_CANDIDATE_COLUMNS, names(df))
  if (length(missing_columns) > 0L) {
    stop(
      sprintf(
        "College cuts review candidates are missing required columns: %s",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  candidates <- data.frame(
    cut_id = trimws(as.character(df$cut_id %||% "")),
    unitid = dplyr::na_if(trimws(as.character(df$unitid %||% "")), ""),
    institution_name = dplyr::na_if(trimws(as.character(df$institution_name %||% "")), ""),
    state = dplyr::na_if(trimws(as.character(df$state %||% "")), ""),
    announcement_date = dplyr::na_if(trimws(as.character(df$announcement_date %||% "")), ""),
    announcement_year = dplyr::na_if(trimws(as.character(df$announcement_year %||% "")), ""),
    cut_type = dplyr::na_if(trimws(as.character(df$cut_type %||% "")), ""),
    program_name = dplyr::na_if(trimws(as.character(df$program_name %||% "")), ""),
    source_url = dplyr::na_if(trimws(as.character(df$source_url %||% "")), ""),
    source_title = dplyr::na_if(trimws(as.character(df$source_title %||% "")), ""),
    source_publication = dplyr::na_if(trimws(as.character(df$source_publication %||% "")), ""),
    row_origin = dplyr::na_if(trimws(as.character(df$row_origin %||% "")), ""),
    stringsAsFactors = FALSE
  )

  assert_unique_cut_ids(candidates, "College cuts review candidates")
  candidates
}

build_college_cuts_review_candidates <- function(cuts_df) {
  if (is.null(cuts_df) || !nrow(cuts_df)) {
    return(empty_college_cuts_review_candidates())
  }

  required_columns <- c(
    "cut_id",
    "matched_unitid",
    "export_unitid",
    "institution_name_display",
    "state_display",
    "announcement_date",
    "announcement_year",
    "cut_type",
    "program_name",
    "source_url",
    "source_title",
    "source_publication"
  )
  missing_columns <- setdiff(required_columns, names(cuts_df))
  if (length(missing_columns) > 0L) {
    stop(
      sprintf(
        "build_college_cuts_review_candidates requires these columns: %s",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  candidates <- data.frame(
    cut_id = vapply(
      seq_len(nrow(cuts_df)),
      function(i) compute_college_cuts_review_id(
        cut_id = cuts_df$cut_id[[i]],
        unitid = dplyr::coalesce(cuts_df$matched_unitid[[i]], cuts_df$export_unitid[[i]]),
        announcement_date = cuts_df$announcement_date[[i]],
        program_name = cuts_df$program_name[[i]]
      ),
      character(1)
    ),
    unitid = dplyr::na_if(trimws(as.character(dplyr::coalesce(cuts_df$matched_unitid, cuts_df$export_unitid) %||% "")), ""),
    institution_name = dplyr::na_if(trimws(as.character(cuts_df$institution_name_display %||% "")), ""),
    state = dplyr::na_if(trimws(as.character(cuts_df$state_display %||% "")), ""),
    announcement_date = dplyr::na_if(trimws(as.character(cuts_df$announcement_date %||% "")), ""),
    announcement_year = dplyr::na_if(trimws(as.character(cuts_df$announcement_year %||% "")), ""),
    cut_type = dplyr::na_if(trimws(as.character(cuts_df$cut_type %||% "")), ""),
    program_name = dplyr::na_if(trimws(as.character(cuts_df$program_name %||% "")), ""),
    source_url = dplyr::na_if(trimws(as.character(cuts_df$source_url %||% "")), ""),
    source_title = dplyr::na_if(trimws(as.character(cuts_df$source_title %||% "")), ""),
    source_publication = dplyr::na_if(trimws(as.character(cuts_df$source_publication %||% "")), ""),
    row_origin = rep("scraper", nrow(cuts_df)),
    stringsAsFactors = FALSE
  )

  candidates <- candidates[!duplicated(candidates$cut_id), , drop = FALSE]
  assert_unique_cut_ids(candidates, "College cuts review candidates")
  candidates
}

coerce_college_cuts_editorial_overrides <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(empty_college_cuts_editorial_overrides())
  }

  overrides <- normalize_college_cuts_sheet_headers(df)
  defaults <- list(
    cut_id = NA_character_,
    unitid = NA_character_,
    institution_name = NA_character_,
    state = NA_character_,
    announcement_date = NA_character_,
    announcement_year = NA_character_,
    cut_type = NA_character_,
    program_name = NA_character_,
    source_url = NA_character_,
    source_title = NA_character_,
    source_publication = NA_character_,
    row_origin = NA_character_,
    first_seen = NA_character_,
    review_status = NA_character_,
    editor_program_name = NA_character_,
    editor_announcement_date = NA_character_,
    editor_cut_type = NA_character_,
    editor_source_url = NA_character_,
    editor_source_title = NA_character_,
    editor_source_publication = NA_character_,
    editor_notes = NA_character_,
    reviewer = NA_character_,
    reviewed_at = NA_character_,
    grandfathered = FALSE
  )
  for (column_name in COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS) {
    if (column_name %in% names(overrides)) next
    overrides[[column_name]] <- defaults[[column_name]]
  }

  overrides <- overrides[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]
  character_columns <- setdiff(COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, "grandfathered")
  for (column_name in character_columns) {
    overrides[[column_name]] <- dplyr::na_if(trimws(as.character(overrides[[column_name]] %||% "")), "")
  }
  overrides$cut_id[is.na(overrides$cut_id)] <- ""
  overrides$grandfathered <- as.logical(overrides$grandfathered)
  overrides$grandfathered[is.na(overrides$grandfathered)] <- FALSE

  assert_unique_cut_ids(overrides, "College cuts editorial overrides")
  overrides
}

assert_college_cuts_review_sheet_header <- function(df) {
  if (is.null(df) || !ncol(df)) {
    return(invisible(df))
  }

  header_names <- names(normalize_college_cuts_sheet_headers(df))
  missing_columns <- setdiff(COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, header_names)
  if (length(missing_columns) > 0L) {
    stop(
      sprintf(
        "Google Sheet tab is missing required columns: %s",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(df)
}

stage_college_cuts_editorial_overrides <- function(candidates,
                                                   existing = NULL,
                                                   first_seen = as.character(Sys.Date())) {
  review_candidates <- coerce_college_cuts_review_candidates(candidates)
  overrides <- coerce_college_cuts_editorial_overrides(existing)

  if (!nrow(review_candidates)) {
    return(overrides)
  }

  if (nrow(overrides)) {
    existing_ids <- trimws(as.character(overrides$cut_id %||% ""))
    candidate_index <- match(existing_ids, review_candidates$cut_id)
    matched <- !is.na(candidate_index)

    if (any(matched)) {
      for (column_name in COLLEGE_CUTS_REVIEW_CANDIDATE_COLUMNS) {
        overrides[[column_name]][matched] <- review_candidates[[column_name]][candidate_index[matched]]
      }
    }
  }

  new_rows <- review_candidates[!(review_candidates$cut_id %in% overrides$cut_id), , drop = FALSE]
  if (!nrow(new_rows)) {
    overrides <- coerce_college_cuts_editorial_overrides(overrides)
    return(overrides[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])
  }

  new_rows$first_seen <- first_seen
  new_rows$review_status <- "unreviewed"
  new_rows$editor_program_name <- NA_character_
  new_rows$editor_announcement_date <- NA_character_
  new_rows$editor_cut_type <- NA_character_
  new_rows$editor_source_url <- NA_character_
  new_rows$editor_source_title <- NA_character_
  new_rows$editor_source_publication <- NA_character_
  new_rows$editor_notes <- NA_character_
  new_rows$reviewer <- NA_character_
  new_rows$reviewed_at <- NA_character_
  new_rows$grandfathered <- FALSE

  combined <- dplyr::bind_rows(overrides, new_rows)
  combined <- coerce_college_cuts_editorial_overrides(combined)
  combined[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]
}

build_college_cuts_review_sheet_append_rows <- function(overrides, existing_sheet = NULL) {
  local_rows <- coerce_college_cuts_editorial_overrides(overrides)
  sheet_rows <- coerce_college_cuts_editorial_overrides(existing_sheet)

  if (!nrow(local_rows)) {
    return(local_rows)
  }

  if (!nrow(sheet_rows)) {
    return(local_rows[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])
  }

  local_rows[
    !(local_rows$cut_id %in% sheet_rows$cut_id),
    COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS,
    drop = FALSE
  ]
}

merge_college_cuts_review_sheet_editor_columns <- function(overrides,
                                                           sheet_rows,
                                                           allow_editor_added_rows = FALSE) {
  local_rows <- coerce_college_cuts_editorial_overrides(overrides)
  sheet_data <- coerce_college_cuts_editorial_overrides(sheet_rows)

  if (!nrow(local_rows) || !nrow(sheet_data)) {
    return(local_rows[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])
  }

  sheet_ids <- trimws(as.character(sheet_data$cut_id %||% ""))
  local_ids <- trimws(as.character(local_rows$cut_id %||% ""))
  sheet_only <- sheet_data[nzchar(sheet_ids) & !(sheet_ids %in% local_ids), , drop = FALSE]

  if (nrow(sheet_only) > 0L && !isTRUE(allow_editor_added_rows)) {
    sample_ids <- paste(utils::head(sheet_only$cut_id, 5L), collapse = ", ")
    stop(
      sprintf(
        paste(
          "Google Sheet contains %d cut_id value(s) that are not present in editorial_overrides.csv.",
          "Editor-added rows are not supported by this sync path yet.",
          "Sample cut_id values: %s"
        ),
        nrow(sheet_only),
        sample_ids
      ),
      call. = FALSE
    )
  }

  match_index <- match(local_rows$cut_id, sheet_data$cut_id)
  matched <- !is.na(match_index)
  if (!any(matched)) {
    return(local_rows[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])
  }

  for (column_name in COLLEGE_CUTS_REVIEW_EDITOR_COLUMNS) {
    local_rows[[column_name]][matched] <- sheet_data[[column_name]][match_index[matched]]
  }

  local_rows[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]
}

grandfather_college_cuts_editorial_overrides <- function(overrides,
                                                         reviewed_at = as.character(Sys.Date()),
                                                         reviewer = "grandfathered",
                                                         only_statuses = c(NA_character_, "", "unreviewed")) {
  local_rows <- coerce_college_cuts_editorial_overrides(overrides)
  if (!nrow(local_rows)) {
    return(local_rows[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])
  }

  status_values <- trimws(as.character(local_rows$review_status %||% ""))
  status_values[is.na(local_rows$review_status)] <- ""
  eligible_statuses <- trimws(as.character(only_statuses %||% ""))
  eligible_statuses[is.na(eligible_statuses)] <- ""
  to_grandfather <- status_values %in% eligible_statuses

  if (!any(to_grandfather)) {
    return(local_rows[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])
  }

  local_rows$review_status[to_grandfather] <- "approved"
  local_rows$grandfathered[to_grandfather] <- TRUE

  reviewed_at_value <- trimws(as.character(reviewed_at %||% ""))
  reviewer_value <- trimws(as.character(reviewer %||% ""))

  if (nzchar(reviewed_at_value)) {
    local_rows$reviewed_at[to_grandfather] <- reviewed_at_value
  }
  if (nzchar(reviewer_value)) {
    local_rows$reviewer[to_grandfather] <- reviewer_value
  }

  local_rows[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]
}

apply_college_cuts_editorial_overrides <- function(cuts_df,
                                                   overrides = NULL,
                                                   enforce_review_gate = FALSE) {
  if (is.null(cuts_df) || !nrow(cuts_df)) {
    return(cuts_df)
  }

  required_columns <- c(
    "cut_id",
    "matched_unitid",
    "export_unitid",
    "institution_name_display",
    "state_display",
    "announcement_date",
    "announcement_year",
    "cut_type",
    "program_name",
    "source_url",
    "source_title",
    "source_publication"
  )
  missing_columns <- setdiff(required_columns, names(cuts_df))
  if (length(missing_columns) > 0L) {
    stop(
      sprintf(
        "apply_college_cuts_editorial_overrides requires these columns: %s",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  review_cuts <- cuts_df
  review_cuts$cut_id <- vapply(
    seq_len(nrow(review_cuts)),
    function(i) compute_college_cuts_review_id(
      cut_id = review_cuts$cut_id[[i]],
      unitid = dplyr::coalesce(review_cuts$matched_unitid[[i]], review_cuts$export_unitid[[i]]),
      announcement_date = review_cuts$announcement_date[[i]],
      program_name = review_cuts$program_name[[i]]
    ),
    character(1)
  )

  override_rows <- coerce_college_cuts_editorial_overrides(overrides)
  if (!nrow(override_rows)) {
    if (isTRUE(enforce_review_gate)) {
      stop(
        "College cuts review gate is enabled but editorial_overrides.csv is empty or missing.",
        call. = FALSE
      )
    }
    return(review_cuts)
  }

  joined <- review_cuts %>%
    dplyr::left_join(
      override_rows[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE],
      by = "cut_id",
      suffix = c("", "_override")
    )

  if (isTRUE(enforce_review_gate)) {
    missing_override <- is.na(joined$review_status)
    if (any(missing_override)) {
      sample_ids <- paste(utils::head(joined$cut_id[missing_override], 5L), collapse = ", ")
      stop(
        sprintf(
          paste(
            "College cuts review gate is enabled but %d cut row(s) are missing editorial overrides.",
            "Sample cut_id values: %s"
          ),
          sum(missing_override),
          sample_ids
        ),
        call. = FALSE
      )
    }
  }

  joined$program_name <- dplyr::coalesce(joined$editor_program_name, joined$program_name)
  joined$announcement_date <- dplyr::coalesce(joined$editor_announcement_date, joined$announcement_date)
  joined$cut_type <- dplyr::coalesce(joined$editor_cut_type, joined$cut_type)
  joined$source_url <- dplyr::coalesce(joined$editor_source_url, joined$source_url)
  joined$source_title <- dplyr::coalesce(joined$editor_source_title, joined$source_title)
  joined$source_publication <- dplyr::coalesce(joined$editor_source_publication, joined$source_publication)

  if (isTRUE(enforce_review_gate)) {
    joined <- joined[joined$review_status == "approved", , drop = FALSE]
  }

  joined
}
