trim_optional_text <- function(x) {
  dplyr::na_if(trimws(as.character(x %||% "")), "")
}

trim_text <- function(x) {
  trimws(as.character(x %||% ""))
}

coerce_false_default_logical <- function(x) {
  values <- as.logical(x)
  values[is.na(values)] <- FALSE
  values
}

normalize_review_row_origin <- function(x) {
  values <- tolower(trim_text(x))
  values[values == ""] <- NA_character_
  values
}

derive_year_from_date_string <- function(x) {
  value <- trim_optional_text(x)
  ifelse(
    is.na(value) | nchar(value) < 4L,
    NA_character_,
    substr(value, 1L, 4L)
  )
}

effective_override_values <- function(source_values, override_values) {
  resolved <- source_values
  override_present <- !is.na(override_values)
  resolved[override_present] <- override_values[override_present]
  resolved
}

compute_override_delta <- function(sheet_values, source_values) {
  sheet_clean <- trim_optional_text(sheet_values)
  source_clean <- trim_optional_text(source_values)
  same_values <- (is.na(sheet_clean) & is.na(source_clean)) |
    (!is.na(sheet_clean) & !is.na(source_clean) & sheet_clean == source_clean)
  sheet_clean[same_values] <- NA_character_
  sheet_clean
}

blank_like_row <- function(template_df) {
  if (!ncol(template_df)) {
    return(template_df[0, , drop = FALSE])
  }

  rep_like_template_rows(template_df, 1L)
}

rep_like_template_rows <- function(template_df, n) {
  if (n <= 0L) {
    return(template_df[0, , drop = FALSE])
  }

  values <- lapply(
    template_df,
    function(column) {
      if (is.logical(column)) {
        rep(NA, n)
      } else if (is.integer(column)) {
        rep(NA_integer_, n)
      } else if (is.numeric(column)) {
        rep(NA_real_, n)
      } else {
        rep(NA_character_, n)
      }
    }
  )

  data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
}

assert_valid_review_row_origins <- function(df,
                                            id_column,
                                            row_origin_column = "row_origin",
                                            context = "Google Sheet rows") {
  if (is.null(df) || !nrow(df) || !(row_origin_column %in% names(df))) {
    return(invisible(df))
  }

  row_origin <- normalize_review_row_origin(df[[row_origin_column]])
  valid_mask <- is.na(row_origin) | row_origin %in% c("scraper", "manual")
  if (all(valid_mask)) {
    return(invisible(df))
  }

  bad_rows <- which(!valid_mask)
  sample_ids <- trim_text(df[[id_column]][bad_rows])
  sample_ids <- sample_ids[nzchar(sample_ids)]
  sample_label <- if (length(sample_ids)) {
    paste(utils::head(sample_ids, 5L), collapse = ", ")
  } else {
    paste(utils::head(bad_rows, 5L), collapse = ", ")
  }

  stop(
    sprintf(
      "%s contain unsupported row_origin values. Supported values: scraper, manual. Sample rows: %s",
      context,
      sample_label
    ),
    call. = FALSE
  )
}

assert_blank_ids_only_for_manual_rows <- function(df,
                                                  id_column,
                                                  row_origin_column = "row_origin",
                                                  context = "Google Sheet rows") {
  if (is.null(df) || !nrow(df) || !(id_column %in% names(df))) {
    return(invisible(df))
  }

  ids <- trim_text(df[[id_column]])
  row_origin <- normalize_review_row_origin(df[[row_origin_column]])
  bad_rows <- which(!nzchar(ids) & (is.na(row_origin) | row_origin != "manual"))
  if (!length(bad_rows)) {
    return(invisible(df))
  }

  stop(
    sprintf(
      "%s allow blank %s values only when row_origin = manual. Bad row numbers: %s",
      context,
      id_column,
      paste(utils::head(bad_rows, 5L), collapse = ", ")
    ),
    call. = FALSE
  )
}

assert_manual_review_required_fields <- function(df,
                                                 id_column,
                                                 required_fields,
                                                 context = "Google Sheet rows") {
  if (is.null(df) || !nrow(df)) {
    return(invisible(df))
  }

  row_origin <- normalize_review_row_origin(df$row_origin)
  manual_rows <- which(!is.na(row_origin) & row_origin == "manual")
  if (!length(manual_rows)) {
    return(invisible(df))
  }

  missing_messages <- character()
  for (row_index in manual_rows) {
    missing_fields <- required_fields[vapply(
      required_fields,
      function(field_name) is.na(trim_optional_text(df[[field_name]][[row_index]])),
      logical(1)
    )]
    if (!length(missing_fields)) next

    row_id <- trim_optional_text(df[[id_column]][[row_index]])
    row_label <- if (!is.na(row_id)) row_id else paste0("row ", row_index)
    missing_messages <- c(
      missing_messages,
      sprintf("%s missing %s", row_label, paste(missing_fields, collapse = ", "))
    )
  }

  if (!length(missing_messages)) {
    return(invisible(df))
  }

  stop(
    sprintf(
      "%s contain manual rows missing required fields: %s",
      context,
      paste(utils::head(missing_messages, 5L), collapse = "; ")
    ),
    call. = FALSE
  )
}

normalize_review_identity_text <- function(x) {
  value <- tolower(trim_text(x))
  value <- gsub("[^a-z0-9]+", " ", value, perl = TRUE)
  trimws(gsub("\\s+", " ", value, perl = TRUE))
}

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

ACCREDITATION_REVIEW_SHEET_COLUMNS <- c(
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
  "row_origin",
  "first_seen",
  "review_status",
  "reviewer",
  "reviewer_notes",
  "reviewed_at",
  "grandfathered"
)

ACCREDITATION_SOURCE_FIELD_MAP <- c(
  unitid = "source_unitid",
  institution_name = "source_institution_name",
  accreditor = "source_accreditor",
  action_date = "source_action_date",
  action_type = "source_action_type",
  action_label_raw = "source_action_label_raw",
  generated_statement = "source_generated_statement",
  source_url = "source_source_url",
  source_title = "source_source_title",
  row_origin = "source_row_origin"
)

ACCREDITATION_OVERRIDE_FIELD_MAP <- c(
  unitid = "override_unitid",
  institution_name = "override_institution_name",
  accreditor = "override_accreditor",
  action_date = "override_action_date",
  action_type = "override_action_type",
  action_label_raw = "override_action_label_raw",
  generated_statement = "override_generated_statement",
  source_url = "override_source_url",
  source_title = "override_source_title"
)

ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS <- c(
  "action_id",
  unname(ACCREDITATION_SOURCE_FIELD_MAP),
  unname(ACCREDITATION_OVERRIDE_FIELD_MAP),
  "first_seen",
  "review_status",
  "reviewer",
  "reviewer_notes",
  "reviewed_at",
  "grandfathered"
)

ACCREDITATION_REQUIRED_MANUAL_FIELDS <- c(
  "institution_name",
  "accreditor",
  "action_date",
  "action_type",
  "action_label_raw",
  "generated_statement",
  "source_url",
  "source_title"
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

empty_accreditation_review_sheet_rows <- function() {
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
    reviewer = character(),
    reviewer_notes = character(),
    reviewed_at = character(),
    grandfathered = logical(),
    stringsAsFactors = FALSE
  )
}

empty_accreditation_editorial_overrides <- function() {
  data.frame(
    action_id = character(),
    source_unitid = character(),
    source_institution_name = character(),
    source_accreditor = character(),
    source_action_date = character(),
    source_action_type = character(),
    source_action_label_raw = character(),
    source_generated_statement = character(),
    source_source_url = character(),
    source_source_title = character(),
    source_row_origin = character(),
    override_unitid = character(),
    override_institution_name = character(),
    override_accreditor = character(),
    override_action_date = character(),
    override_action_type = character(),
    override_action_label_raw = character(),
    override_generated_statement = character(),
    override_source_url = character(),
    override_source_title = character(),
    first_seen = character(),
    review_status = character(),
    reviewer = character(),
    reviewer_notes = character(),
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

compute_accreditation_action_id <- function(unitid,
                                            accreditor,
                                            action_date,
                                            action_label_raw,
                                            export_unitid = NA_character_,
                                            institution_name = NA_character_) {
  identity_value <- trim_text(unitid)
  if (!nzchar(identity_value)) {
    identity_value <- trim_text(export_unitid)
  }
  if (!nzchar(identity_value)) {
    identity_value <- trim_text(institution_name)
  }

  seed <- paste(
    normalize_review_identity_text(identity_value),
    normalize_review_identity_text(accreditor),
    normalize_review_identity_text(action_date),
    normalize_review_identity_text(action_label_raw),
    sep = "|"
  )

  substr(digest::digest(seed, algo = "sha1", serialize = FALSE), 1L, 12L)
}

assert_unique_action_ids <- function(df, label) {
  if (!"action_id" %in% names(df) || !nrow(df)) return(invisible(df))
  ids <- trim_text(df$action_id)
  duplicate_ids <- unique(ids[nzchar(ids) & duplicated(ids)])
  if (length(duplicate_ids) > 0L) {
    stop(
      sprintf("%s contains duplicate action_id values: %s", label, paste(duplicate_ids, collapse = ", ")),
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
      sprintf("Accreditation review candidates are missing required columns: %s", paste(missing_columns, collapse = ", ")),
      call. = FALSE
    )
  }

  candidates <- data.frame(
    action_id = trim_text(df$action_id),
    unitid = trim_optional_text(df$unitid),
    institution_name = trim_optional_text(df$institution_name),
    accreditor = trim_optional_text(df$accreditor),
    action_date = trim_optional_text(df$action_date),
    action_type = trim_optional_text(df$action_type),
    action_label_raw = trim_optional_text(df$action_label_raw),
    generated_statement = trim_optional_text(df$generated_statement),
    source_url = trim_optional_text(df$source_url),
    source_title = trim_optional_text(df$source_title),
    row_origin = normalize_review_row_origin(df$row_origin),
    stringsAsFactors = FALSE
  )
  candidates$row_origin[is.na(candidates$row_origin)] <- "scraper"
  assert_unique_action_ids(candidates, "Accreditation review candidates")
  candidates
}

build_accreditation_review_candidates <- function(actions_df) {
  if (is.null(actions_df) || !nrow(actions_df)) {
    return(empty_accreditation_review_candidates())
  }

  required_columns <- c("export_unitid", "unitid", "export_institution_name", "accreditor", "action_date", "action_type", "action_label_raw", "action_label_short", "source_url", "source_title", "source_page_url")
  missing_columns <- setdiff(required_columns, names(actions_df))
  if (length(missing_columns) > 0L) {
    stop(
      sprintf("build_accreditation_review_candidates requires these columns: %s", paste(missing_columns, collapse = ", ")),
      call. = FALSE
    )
  }

  candidates <- data.frame(
    action_id = vapply(
      seq_len(nrow(actions_df)),
      function(i) compute_accreditation_action_id(actions_df$unitid[[i]], actions_df$accreditor[[i]], actions_df$action_date[[i]], actions_df$action_label_raw[[i]], actions_df$export_unitid[[i]], actions_df$export_institution_name[[i]]),
      character(1)
    ),
    unitid = trim_optional_text(actions_df$unitid),
    institution_name = trim_optional_text(actions_df$export_institution_name),
    accreditor = trim_optional_text(actions_df$accreditor),
    action_date = trim_optional_text(actions_df$action_date),
    action_type = trim_optional_text(actions_df$action_type),
    action_label_raw = trim_optional_text(actions_df$action_label_raw),
    generated_statement = dplyr::coalesce(trim_optional_text(actions_df$action_label_short), trim_optional_text(actions_df$action_label_raw)),
    source_url = dplyr::coalesce(trim_optional_text(actions_df$source_url), trim_optional_text(actions_df$source_page_url)),
    source_title = trim_optional_text(actions_df$source_title),
    row_origin = rep("scraper", nrow(actions_df)),
    stringsAsFactors = FALSE
  )
  candidates <- candidates[!duplicated(candidates$action_id), ACCREDITATION_REVIEW_CANDIDATE_COLUMNS, drop = FALSE]
  assert_unique_action_ids(candidates, "Accreditation review candidates")
  candidates
}

normalize_accreditation_review_sheet_headers <- function(df) {
  if (is.null(df) || !ncol(df)) {
    return(df)
  }

  normalized <- df
  alias_map <- c(visible_statement = "generated_statement", editor_notes = "reviewer_notes")
  for (old_name in names(alias_map)) {
    new_name <- alias_map[[old_name]]
    if (!(old_name %in% names(normalized))) next
    if (new_name %in% names(normalized)) next
    names(normalized)[names(normalized) == old_name] <- new_name
  }
  normalized
}

coerce_accreditation_review_sheet_rows <- function(df,
                                                   default_first_seen = as.character(Sys.Date())) {
  if (is.null(df) || !nrow(df)) {
    return(empty_accreditation_review_sheet_rows())
  }

  raw_rows <- normalize_accreditation_review_sheet_headers(df)
  assert_valid_review_row_origins(raw_rows, id_column = "action_id", context = "Accreditation review sheet rows")
  assert_blank_ids_only_for_manual_rows(raw_rows, id_column = "action_id", context = "Accreditation review sheet rows")

  sheet_rows <- rep_like_template_rows(empty_accreditation_review_sheet_rows(), nrow(raw_rows))
  for (column_name in setdiff(ACCREDITATION_REVIEW_SHEET_COLUMNS, "grandfathered")) {
    sheet_rows[[column_name]] <- if (column_name %in% names(raw_rows)) trim_optional_text(raw_rows[[column_name]]) else NA_character_
  }
  sheet_rows$grandfathered <- if ("grandfathered" %in% names(raw_rows)) coerce_false_default_logical(raw_rows$grandfathered) else FALSE

  legacy_generated <- dplyr::coalesce(
    if ("editor_action_label_short" %in% names(raw_rows)) trim_optional_text(raw_rows$editor_action_label_short) else rep(NA_character_, nrow(raw_rows)),
    if ("editor_rewrite" %in% names(raw_rows)) trim_optional_text(raw_rows$editor_rewrite) else rep(NA_character_, nrow(raw_rows))
  )
  if ("editor_action_date" %in% names(raw_rows)) sheet_rows$action_date <- dplyr::coalesce(trim_optional_text(raw_rows$editor_action_date), sheet_rows$action_date)
  if ("editor_action_type" %in% names(raw_rows)) sheet_rows$action_type <- dplyr::coalesce(trim_optional_text(raw_rows$editor_action_type), sheet_rows$action_type)
  if ("editor_source_url" %in% names(raw_rows)) sheet_rows$source_url <- dplyr::coalesce(trim_optional_text(raw_rows$editor_source_url), sheet_rows$source_url)
  if ("editor_source_title" %in% names(raw_rows)) sheet_rows$source_title <- dplyr::coalesce(trim_optional_text(raw_rows$editor_source_title), sheet_rows$source_title)
  sheet_rows$generated_statement <- dplyr::coalesce(legacy_generated, sheet_rows$generated_statement)
  sheet_rows$row_origin <- normalize_review_row_origin(sheet_rows$row_origin)
  sheet_rows$row_origin[is.na(sheet_rows$row_origin)] <- "scraper"

  missing_manual_ids <- which(!nzchar(trim_text(sheet_rows$action_id)) & sheet_rows$row_origin == "manual")
  if (length(missing_manual_ids)) {
    sheet_rows$action_id[missing_manual_ids] <- vapply(
      missing_manual_ids,
      function(i) compute_accreditation_action_id(sheet_rows$unitid[[i]], sheet_rows$accreditor[[i]], sheet_rows$action_date[[i]], sheet_rows$action_label_raw[[i]], institution_name = sheet_rows$institution_name[[i]]),
      character(1)
    )
  }
  sheet_rows$first_seen[is.na(sheet_rows$first_seen) & sheet_rows$row_origin == "manual"] <- default_first_seen

  assert_manual_review_required_fields(sheet_rows, "action_id", ACCREDITATION_REQUIRED_MANUAL_FIELDS, "Accreditation review sheet rows")
  assert_unique_action_ids(sheet_rows, "Accreditation review sheet rows")
  sheet_rows[, ACCREDITATION_REVIEW_SHEET_COLUMNS, drop = FALSE]
}

coerce_accreditation_editorial_overrides <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(empty_accreditation_editorial_overrides())
  }

  normalized <- normalize_accreditation_review_sheet_headers(df)
  overrides <- rep_like_template_rows(empty_accreditation_editorial_overrides(), nrow(normalized))
  overrides$action_id <- if ("action_id" %in% names(normalized)) trim_text(normalized$action_id) else ""

  for (field_name in names(ACCREDITATION_SOURCE_FIELD_MAP)) {
    source_column <- ACCREDITATION_SOURCE_FIELD_MAP[[field_name]]
    overrides[[source_column]] <- if (source_column %in% names(normalized)) trim_optional_text(normalized[[source_column]]) else if (field_name %in% names(normalized)) trim_optional_text(normalized[[field_name]]) else NA_character_
  }

  legacy_override_map <- c(generated_statement = "editor_action_label_short", action_date = "editor_action_date", action_type = "editor_action_type", source_url = "editor_source_url", source_title = "editor_source_title")
  for (field_name in names(ACCREDITATION_OVERRIDE_FIELD_MAP)) {
    override_column <- ACCREDITATION_OVERRIDE_FIELD_MAP[[field_name]]
    legacy_override_column <- if (field_name %in% names(legacy_override_map)) legacy_override_map[[field_name]] else NA_character_
    overrides[[override_column]] <- if (override_column %in% names(normalized)) {
      trim_optional_text(normalized[[override_column]])
    } else if (field_name == "generated_statement" && "editor_rewrite" %in% names(normalized)) {
      trim_optional_text(normalized$editor_rewrite)
    } else if (!is.na(legacy_override_column) && legacy_override_column %in% names(normalized)) {
      trim_optional_text(normalized[[legacy_override_column]])
    } else {
      NA_character_
    }
  }

  overrides$first_seen <- if ("first_seen" %in% names(normalized)) trim_optional_text(normalized$first_seen) else NA_character_
  overrides$review_status <- if ("review_status" %in% names(normalized)) trim_optional_text(normalized$review_status) else NA_character_
  overrides$reviewer <- if ("reviewer" %in% names(normalized)) trim_optional_text(normalized$reviewer) else NA_character_
  overrides$reviewer_notes <- if ("reviewer_notes" %in% names(normalized)) trim_optional_text(normalized$reviewer_notes) else NA_character_
  overrides$reviewed_at <- if ("reviewed_at" %in% names(normalized)) trim_optional_text(normalized$reviewed_at) else NA_character_
  overrides$grandfathered <- if ("grandfathered" %in% names(normalized)) coerce_false_default_logical(normalized$grandfathered) else FALSE
  overrides$source_row_origin <- normalize_review_row_origin(overrides$source_row_origin)
  overrides$source_row_origin[is.na(overrides$source_row_origin)] <- "scraper"

  missing_manual_ids <- which(!nzchar(trim_text(overrides$action_id)) & overrides$source_row_origin == "manual")
  if (length(missing_manual_ids)) {
    overrides$action_id[missing_manual_ids] <- vapply(
      missing_manual_ids,
      function(i) {
        compute_accreditation_action_id(
          overrides$source_unitid[[i]],
          overrides$source_accreditor[[i]],
          overrides$source_action_date[[i]],
          overrides$source_action_label_raw[[i]],
          institution_name = overrides$source_institution_name[[i]]
        )
      },
      character(1)
    )
    missing_first_seen <- is.na(overrides$first_seen[missing_manual_ids])
    if (any(missing_first_seen)) {
      overrides$first_seen[missing_manual_ids[missing_first_seen]] <- as.character(Sys.Date())
    }
  }

  assert_unique_action_ids(overrides, "Accreditation editorial overrides")
  overrides[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]
}

build_accreditation_review_sheet_rows <- function(overrides) {
  local_rows <- coerce_accreditation_editorial_overrides(overrides)
  if (!nrow(local_rows)) {
    return(empty_accreditation_review_sheet_rows())
  }

  sheet_rows <- rep_like_template_rows(empty_accreditation_review_sheet_rows(), nrow(local_rows))
  sheet_rows$action_id <- trim_text(local_rows$action_id)
  for (field_name in names(ACCREDITATION_SOURCE_FIELD_MAP)) {
    source_column <- ACCREDITATION_SOURCE_FIELD_MAP[[field_name]]
    sheet_rows[[field_name]] <- if (field_name %in% names(ACCREDITATION_OVERRIDE_FIELD_MAP)) {
      effective_override_values(local_rows[[source_column]], local_rows[[ACCREDITATION_OVERRIDE_FIELD_MAP[[field_name]]]])
    } else {
      local_rows[[source_column]]
    }
  }
  sheet_rows$first_seen <- local_rows$first_seen
  sheet_rows$review_status <- local_rows$review_status
  sheet_rows$reviewer <- local_rows$reviewer
  sheet_rows$reviewer_notes <- local_rows$reviewer_notes
  sheet_rows$reviewed_at <- local_rows$reviewed_at
  sheet_rows$grandfathered <- local_rows$grandfathered
  sheet_rows[, ACCREDITATION_REVIEW_SHEET_COLUMNS, drop = FALSE]
}

assert_accreditation_review_sheet_header <- function(df) {
  if (is.null(df) || !ncol(df)) return(invisible(df))
  header_names <- names(normalize_accreditation_review_sheet_headers(df))
  missing_columns <- setdiff(ACCREDITATION_REVIEW_SHEET_COLUMNS, header_names)
  if (length(missing_columns) > 0L) {
    stop(sprintf("Google Sheet tab is missing required columns: %s", paste(missing_columns, collapse = ", ")), call. = FALSE)
  }
  invisible(df)
}

stage_accreditation_editorial_overrides <- function(candidates,
                                                    existing = NULL,
                                                    first_seen = as.character(Sys.Date())) {
  review_candidates <- coerce_accreditation_review_candidates(candidates)
  overrides <- coerce_accreditation_editorial_overrides(existing)
  if (!nrow(review_candidates)) return(overrides[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])

  if (nrow(overrides)) {
    candidate_index <- match(trim_text(overrides$action_id), review_candidates$action_id)
    matched <- !is.na(candidate_index)
    if (any(matched)) {
      for (field_name in names(ACCREDITATION_SOURCE_FIELD_MAP)) {
        source_column <- ACCREDITATION_SOURCE_FIELD_MAP[[field_name]]
        overrides[[source_column]][matched] <- review_candidates[[field_name]][candidate_index[matched]]
      }
    }
  }

  new_rows <- review_candidates[!(review_candidates$action_id %in% trim_text(overrides$action_id)), , drop = FALSE]
  if (!nrow(new_rows)) return(coerce_accreditation_editorial_overrides(overrides))

  new_internal_rows <- rep_like_template_rows(empty_accreditation_editorial_overrides(), nrow(new_rows))
  new_internal_rows$action_id <- new_rows$action_id
  for (field_name in names(ACCREDITATION_SOURCE_FIELD_MAP)) {
    new_internal_rows[[ACCREDITATION_SOURCE_FIELD_MAP[[field_name]]]] <- new_rows[[field_name]]
  }
  for (override_column in unname(ACCREDITATION_OVERRIDE_FIELD_MAP)) new_internal_rows[[override_column]] <- NA_character_
  new_internal_rows$first_seen <- first_seen
  new_internal_rows$review_status <- "unreviewed"
  new_internal_rows$reviewer <- NA_character_
  new_internal_rows$reviewer_notes <- NA_character_
  new_internal_rows$reviewed_at <- NA_character_
  new_internal_rows$grandfathered <- FALSE
  coerce_accreditation_editorial_overrides(dplyr::bind_rows(overrides, new_internal_rows))
}

build_accreditation_review_sheet_append_rows <- function(overrides, existing_sheet = NULL) {
  local_sheet_rows <- build_accreditation_review_sheet_rows(overrides)
  sheet_rows <- coerce_accreditation_review_sheet_rows(existing_sheet)
  if (!nrow(local_sheet_rows)) return(local_sheet_rows)
  if (!nrow(sheet_rows)) return(local_sheet_rows)
  local_sheet_rows[!(trim_text(local_sheet_rows$action_id) %in% trim_text(sheet_rows$action_id)), ACCREDITATION_REVIEW_SHEET_COLUMNS, drop = FALSE]
}

merge_accreditation_review_sheet_editor_columns <- function(overrides,
                                                            sheet_rows,
                                                            allow_editor_added_rows = FALSE,
                                                            first_seen = as.character(Sys.Date())) {
  local_rows <- coerce_accreditation_editorial_overrides(overrides)
  sheet_data <- coerce_accreditation_review_sheet_rows(sheet_rows, default_first_seen = first_seen)
  if (!nrow(sheet_data)) return(local_rows[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])

  local_ids <- trim_text(local_rows$action_id)
  sheet_ids <- trim_text(sheet_data$action_id)
  sheet_only <- sheet_data[!(sheet_ids %in% local_ids), , drop = FALSE]
  if (nrow(sheet_only) > 0L) {
    non_manual <- sheet_only[normalize_review_row_origin(sheet_only$row_origin) != "manual", , drop = FALSE]
    if (nrow(non_manual) > 0L && !isTRUE(allow_editor_added_rows)) {
      sample_ids <- paste(utils::head(non_manual$action_id, 5L), collapse = ", ")
      stop(sprintf(paste("Google Sheet contains %d action_id value(s) that are not present in editorial_overrides.csv.", "Only row_origin = manual rows may exist only in the sheet.", "Sample action_id values: %s"), nrow(non_manual), sample_ids), call. = FALSE)
    }
  }

  if (nrow(local_rows)) {
    match_index <- match(local_ids, sheet_ids)
    matched <- !is.na(match_index)
    if (any(matched)) {
      matched_sheet_rows <- sheet_data[match_index[matched], , drop = FALSE]
      matched_rows <- which(matched)
      matched_manual <- local_rows$source_row_origin[matched] == "manual"
      matched_manual_rows <- matched_rows[matched_manual]
      for (field_name in names(ACCREDITATION_OVERRIDE_FIELD_MAP)) {
        source_column <- ACCREDITATION_SOURCE_FIELD_MAP[[field_name]]
        override_column <- ACCREDITATION_OVERRIDE_FIELD_MAP[[field_name]]
        local_rows[[override_column]][matched] <- compute_override_delta(matched_sheet_rows[[field_name]], local_rows[[source_column]][matched])
        if (length(matched_manual_rows)) {
          local_rows[[source_column]][matched_manual_rows] <- matched_sheet_rows[[field_name]][matched_manual]
          local_rows[[override_column]][matched_manual_rows] <- NA_character_
        }
      }
      local_rows$review_status[matched] <- matched_sheet_rows$review_status
      local_rows$reviewer[matched] <- matched_sheet_rows$reviewer
      local_rows$reviewer_notes[matched] <- matched_sheet_rows$reviewer_notes
      local_rows$reviewed_at[matched] <- matched_sheet_rows$reviewed_at
      local_rows$grandfathered[matched] <- matched_sheet_rows$grandfathered
      new_first_seen <- matched_sheet_rows$first_seen
      keep_existing_first_seen <- is.na(new_first_seen)
      if (any(!keep_existing_first_seen)) {
        local_rows$first_seen[matched_rows[!keep_existing_first_seen]] <- new_first_seen[!keep_existing_first_seen]
      }
    }
  }

  sheet_only_manual <- sheet_only[normalize_review_row_origin(sheet_only$row_origin) == "manual", , drop = FALSE]
  if (nrow(sheet_only_manual) > 0L) {
    manual_rows <- rep_like_template_rows(empty_accreditation_editorial_overrides(), nrow(sheet_only_manual))
    manual_rows$action_id <- sheet_only_manual$action_id
    for (field_name in names(ACCREDITATION_SOURCE_FIELD_MAP)) {
      manual_rows[[ACCREDITATION_SOURCE_FIELD_MAP[[field_name]]]] <- sheet_only_manual[[field_name]]
    }
    for (override_column in unname(ACCREDITATION_OVERRIDE_FIELD_MAP)) manual_rows[[override_column]] <- NA_character_
    manual_rows$first_seen <- dplyr::coalesce(sheet_only_manual$first_seen, first_seen)
    manual_rows$review_status <- sheet_only_manual$review_status
    manual_rows$reviewer <- sheet_only_manual$reviewer
    manual_rows$reviewer_notes <- sheet_only_manual$reviewer_notes
    manual_rows$reviewed_at <- sheet_only_manual$reviewed_at
    manual_rows$grandfathered <- sheet_only_manual$grandfathered
    local_rows <- dplyr::bind_rows(local_rows, manual_rows)
  }

  coerce_accreditation_editorial_overrides(local_rows)
}

grandfather_accreditation_editorial_overrides <- function(overrides,
                                                          reviewed_at = as.character(Sys.Date()),
                                                          reviewer = "grandfathered",
                                                          only_statuses = c(NA_character_, "", "unreviewed")) {
  local_rows <- coerce_accreditation_editorial_overrides(overrides)
  if (!nrow(local_rows)) return(local_rows[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])

  status_values <- trim_text(local_rows$review_status)
  eligible_statuses <- trim_text(only_statuses)
  eligible_statuses[is.na(only_statuses)] <- ""
  to_grandfather <- status_values %in% eligible_statuses
  if (!any(to_grandfather)) return(local_rows[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])

  local_rows$review_status[to_grandfather] <- "approved"
  local_rows$grandfathered[to_grandfather] <- TRUE
  reviewed_at_value <- trim_optional_text(reviewed_at)
  reviewer_value <- trim_optional_text(reviewer)
  if (!is.na(reviewed_at_value)) local_rows$reviewed_at[to_grandfather] <- reviewed_at_value
  if (!is.na(reviewer_value)) local_rows$reviewer[to_grandfather] <- reviewer_value
  local_rows[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]
}

build_manual_accreditation_export_row <- function(override_row, template_df) {
  internal_row <- coerce_accreditation_editorial_overrides(override_row)
  if (!nrow(internal_row)) return(template_df[0, , drop = FALSE])
  effective_row <- build_accreditation_review_sheet_rows(internal_row)[1, , drop = FALSE]
  manual_row <- blank_like_row(template_df)
  if (!("row_origin" %in% names(manual_row))) manual_row$row_origin <- NA_character_
  export_unitid <- if (is.na(effective_row$unitid[[1]]) || !nzchar(effective_row$unitid[[1]])) paste0("manual-accred-", effective_row$action_id[[1]]) else effective_row$unitid[[1]]

  if ("action_id" %in% names(manual_row)) manual_row$action_id[[1]] <- effective_row$action_id[[1]]
  if ("unitid" %in% names(manual_row)) manual_row$unitid[[1]] <- effective_row$unitid[[1]]
  if ("export_unitid" %in% names(manual_row)) manual_row$export_unitid[[1]] <- export_unitid
  if ("export_institution_name" %in% names(manual_row)) manual_row$export_institution_name[[1]] <- effective_row$institution_name[[1]]
  if ("institution_name" %in% names(manual_row)) manual_row$institution_name[[1]] <- effective_row$institution_name[[1]]
  if ("accreditor" %in% names(manual_row)) manual_row$accreditor[[1]] <- effective_row$accreditor[[1]]
  if ("action_date" %in% names(manual_row)) manual_row$action_date[[1]] <- effective_row$action_date[[1]]
  if ("action_year" %in% names(manual_row)) manual_row$action_year[[1]] <- derive_year_from_date_string(effective_row$action_date[[1]])
  if ("action_type" %in% names(manual_row)) manual_row$action_type[[1]] <- effective_row$action_type[[1]]
  if ("action_label_raw" %in% names(manual_row)) manual_row$action_label_raw[[1]] <- effective_row$action_label_raw[[1]]
  if ("action_label_short" %in% names(manual_row)) manual_row$action_label_short[[1]] <- effective_row$generated_statement[[1]]
  if ("source_url" %in% names(manual_row)) manual_row$source_url[[1]] <- effective_row$source_url[[1]]
  if ("source_title" %in% names(manual_row)) manual_row$source_title[[1]] <- effective_row$source_title[[1]]
  if ("source_page_url" %in% names(manual_row)) manual_row$source_page_url[[1]] <- effective_row$source_url[[1]]
  manual_row$row_origin[[1]] <- "manual"
  if ("has_financial_profile" %in% names(manual_row)) manual_row$has_financial_profile[[1]] <- !is.na(effective_row$unitid[[1]]) && nzchar(effective_row$unitid[[1]])
  if ("is_primary_tracker" %in% names(manual_row)) manual_row$is_primary_tracker[[1]] <- FALSE
  manual_row
}

apply_accreditation_editorial_overrides <- function(actions_df,
                                                    overrides = NULL,
                                                    enforce_review_gate = FALSE,
                                                    allowed_action_ids = NULL,
                                                    drop_unlisted = FALSE) {
  override_rows <- coerce_accreditation_editorial_overrides(overrides)
  manual_origin_mask <- trim_text(override_rows$source_row_origin) == "manual"
  approved_review_mask <- trim_text(override_rows$review_status) == "approved"
  manual_origin_mask[is.na(manual_origin_mask)] <- FALSE
  approved_review_mask[is.na(approved_review_mask)] <- FALSE
  published_override_rows <- override_rows[approved_review_mask, , drop = FALSE]
  approved_manual_rows <- published_override_rows[trim_text(published_override_rows$source_row_origin) == "manual", , drop = FALSE]
  if (is.null(actions_df)) {
    if (!nrow(approved_manual_rows)) return(actions_df)
    stop("Approved manual accreditation rows need an actions data frame template during export.", call. = FALSE)
  }

  required_columns <- c("unitid", "export_unitid", "export_institution_name", "accreditor", "action_date", "action_type", "action_label_raw", "action_label_short", "source_url", "source_title")
  missing_columns <- setdiff(required_columns, names(actions_df))
  if (length(missing_columns) > 0L) stop(sprintf("apply_accreditation_editorial_overrides requires these columns: %s", paste(missing_columns, collapse = ", ")), call. = FALSE)

  if (!nrow(actions_df)) {
    if (!nrow(approved_manual_rows)) return(actions_df)
    return(dplyr::bind_rows(actions_df, dplyr::bind_rows(lapply(seq_len(nrow(approved_manual_rows)), function(i) build_manual_accreditation_export_row(approved_manual_rows[i, , drop = FALSE], actions_df)))))
  }

  review_actions <- actions_df
  review_actions$action_id <- vapply(seq_len(nrow(review_actions)), function(i) compute_accreditation_action_id(review_actions$unitid[[i]], review_actions$accreditor[[i]], review_actions$action_date[[i]], review_actions$action_label_raw[[i]], review_actions$export_unitid[[i]], review_actions$export_institution_name[[i]]), character(1))

  allowed_ids <- trim_text(allowed_action_ids)
  allowed_ids <- unique(allowed_ids[nzchar(allowed_ids)])
  if (length(allowed_ids) > 0L && isTRUE(drop_unlisted)) {
    unexpected_rows <- !(review_actions$action_id %in% allowed_ids)
    if (any(unexpected_rows)) {
      message(sprintf(paste("Apply-only accreditation review gate: ignoring %d recomputed action(s)", "that are not present in the committed review candidate snapshot."), sum(unexpected_rows)))
      review_actions <- review_actions[!unexpected_rows, , drop = FALSE]
    }
  }

  if (!nrow(override_rows)) {
    if (isTRUE(enforce_review_gate)) stop("Review gate is enabled but editorial_overrides.csv is empty or missing.", call. = FALSE)
    return(review_actions)
  }
  if (!nrow(published_override_rows) && !isTRUE(enforce_review_gate)) {
    if ("row_origin" %in% names(review_actions)) {
      review_actions$row_origin <- dplyr::coalesce(trim_optional_text(review_actions$row_origin), rep("scraper", nrow(review_actions)))
    } else {
      review_actions$row_origin <- rep("scraper", nrow(review_actions))
    }
    return(review_actions)
  }

  joined <- review_actions %>% dplyr::left_join(published_override_rows[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE], by = "action_id")
  if (isTRUE(enforce_review_gate)) {
    if (length(allowed_ids) > 0L) {
      missing_snapshot_override_ids <- allowed_ids[!(allowed_ids %in% trim_text(override_rows$action_id))]
      if (length(missing_snapshot_override_ids) > 0L) stop(sprintf(paste("Review gate is enabled but %d committed accreditation review candidate(s) are missing editorial overrides.", "Sample action_id values: %s"), length(missing_snapshot_override_ids), paste(utils::head(missing_snapshot_override_ids, 5L), collapse = ", ")), call. = FALSE)
    } else {
      missing_override <- is.na(joined$review_status)
      if (any(missing_override)) stop(sprintf(paste("Review gate is enabled but %d accreditation action(s) are missing editorial overrides.", "Sample action_id values: %s"), sum(missing_override), paste(utils::head(joined$action_id[missing_override], 5L), collapse = ", ")), call. = FALSE)
    }
  }

  joined_approved_mask <- trim_text(joined$review_status) == "approved"
  joined_approved_mask[is.na(joined_approved_mask)] <- FALSE
  approved_override_values <- function(values) {
    approved_values <- values
    approved_values[!joined_approved_mask] <- NA_character_
    approved_values
  }

  joined$unitid <- effective_override_values(joined$unitid, approved_override_values(joined$override_unitid))
  if ("export_institution_name" %in% names(joined)) joined$export_institution_name <- effective_override_values(joined$export_institution_name, approved_override_values(joined$override_institution_name))
  joined$accreditor <- effective_override_values(joined$accreditor, approved_override_values(joined$override_accreditor))
  joined$action_date <- effective_override_values(joined$action_date, approved_override_values(joined$override_action_date))
  joined$action_type <- effective_override_values(joined$action_type, approved_override_values(joined$override_action_type))
  joined$action_label_raw <- effective_override_values(joined$action_label_raw, approved_override_values(joined$override_action_label_raw))
  joined$action_label_short <- effective_override_values(dplyr::coalesce(joined$action_label_short, joined$action_label_raw), approved_override_values(joined$override_generated_statement))
  joined$source_url <- effective_override_values(joined$source_url, approved_override_values(joined$override_source_url))
  joined$source_title <- effective_override_values(joined$source_title, approved_override_values(joined$override_source_title))
  if ("source_page_url" %in% names(joined)) joined$source_page_url <- dplyr::coalesce(joined$source_url, joined$source_page_url)
  if ("action_year" %in% names(joined)) joined$action_year <- dplyr::coalesce(derive_year_from_date_string(joined$action_date), trim_optional_text(joined$action_year))
  if ("row_origin" %in% names(joined)) {
    joined$row_origin <- dplyr::coalesce(trim_optional_text(joined$row_origin), trim_optional_text(joined$source_row_origin), rep("scraper", nrow(joined)))
  } else {
    joined$row_origin <- dplyr::coalesce(trim_optional_text(joined$source_row_origin), rep("scraper", nrow(joined)))
  }
  if (isTRUE(enforce_review_gate)) joined <- joined[joined_approved_mask, , drop = FALSE]

  manual_rows <- approved_manual_rows[!(trim_text(approved_manual_rows$action_id) %in% trim_text(review_actions$action_id)), , drop = FALSE]
  if (nrow(manual_rows)) joined <- dplyr::bind_rows(joined, dplyr::bind_rows(lapply(seq_len(nrow(manual_rows)), function(i) build_manual_accreditation_export_row(manual_rows[i, , drop = FALSE], review_actions))))
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

COLLEGE_CUTS_REVIEW_SHEET_COLUMNS <- c(
  "cut_id", "unitid", "institution_name", "state", "announcement_date", "announcement_year", "cut_type", "cut_description", "source_url", "source_publication", "row_origin", "first_seen", "review_status", "reviewer", "reviewer_notes", "reviewed_at", "grandfathered"
)

COLLEGE_CUTS_CANDIDATE_SOURCE_MAP <- c(
  unitid = "source_unitid",
  institution_name = "source_institution_name",
  state = "source_state",
  announcement_date = "source_announcement_date",
  announcement_year = "source_announcement_year",
  cut_type = "source_cut_type",
  program_name = "source_cut_description",
  source_url = "source_source_url",
  source_title = "source_source_title",
  source_publication = "source_source_publication",
  row_origin = "source_row_origin"
)

COLLEGE_CUTS_SHEET_SOURCE_MAP <- c(
  unitid = "source_unitid",
  institution_name = "source_institution_name",
  state = "source_state",
  announcement_date = "source_announcement_date",
  announcement_year = "source_announcement_year",
  cut_type = "source_cut_type",
  cut_description = "source_cut_description",
  source_url = "source_source_url",
  source_publication = "source_source_publication",
  row_origin = "source_row_origin"
)

COLLEGE_CUTS_SHEET_OVERRIDE_MAP <- c(
  unitid = "override_unitid",
  institution_name = "override_institution_name",
  state = "override_state",
  announcement_date = "override_announcement_date",
  announcement_year = "override_announcement_year",
  cut_type = "override_cut_type",
  cut_description = "override_cut_description",
  source_url = "override_source_url",
  source_publication = "override_source_publication"
)

COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS <- c(
  "cut_id", "source_unitid", "source_institution_name", "source_state", "source_announcement_date", "source_announcement_year", "source_cut_type", "source_cut_description", "source_source_url", "source_source_title", "source_source_publication", "source_row_origin", "override_unitid", "override_institution_name", "override_state", "override_announcement_date", "override_announcement_year", "override_cut_type", "override_cut_description", "override_source_url", "override_source_title", "override_source_publication", "first_seen", "review_status", "reviewer", "reviewer_notes", "reviewed_at", "grandfathered"
)

COLLEGE_CUTS_REQUIRED_MANUAL_FIELDS <- c("institution_name", "state", "announcement_date", "cut_type", "cut_description", "source_url", "source_publication")

COLLEGE_CUTS_REVIEW_CANDIDATE_COL_TYPES <- readr::cols(.default = readr::col_character())
COLLEGE_CUTS_EDITORIAL_OVERRIDE_COL_TYPES <- readr::cols(.default = readr::col_character(), grandfathered = readr::col_logical())

normalize_college_cuts_sheet_headers <- function(df) {
  if (is.null(df) || !ncol(df)) return(df)
  normalized <- df
  alias_map <- c(program_name = "cut_description", editor_program_name = "editor_cut_description", editor_notes = "reviewer_notes")
  for (old_name in names(alias_map)) {
    new_name <- alias_map[[old_name]]
    if (!(old_name %in% names(normalized))) next
    if (new_name %in% names(normalized)) next
    names(normalized)[names(normalized) == old_name] <- new_name
  }
  normalized
}

format_college_cuts_sheet_headers <- function(df) df
format_accreditation_review_sheet_headers <- function(df) df

empty_college_cuts_review_candidates <- function() {
  data.frame(
    cut_id = character(), unitid = character(), institution_name = character(), state = character(), announcement_date = character(), announcement_year = character(), cut_type = character(), program_name = character(), source_url = character(), source_title = character(), source_publication = character(), row_origin = character(), stringsAsFactors = FALSE
  )
}

empty_college_cuts_review_sheet_rows <- function() {
  data.frame(
    cut_id = character(), unitid = character(), institution_name = character(), state = character(), announcement_date = character(), announcement_year = character(), cut_type = character(), cut_description = character(), source_url = character(), source_publication = character(), row_origin = character(), first_seen = character(), review_status = character(), reviewer = character(), reviewer_notes = character(), reviewed_at = character(), grandfathered = logical(), stringsAsFactors = FALSE
  )
}

empty_college_cuts_editorial_overrides <- function() {
  data.frame(
    cut_id = character(), source_unitid = character(), source_institution_name = character(), source_state = character(), source_announcement_date = character(), source_announcement_year = character(), source_cut_type = character(), source_cut_description = character(), source_source_url = character(), source_source_title = character(), source_source_publication = character(), source_row_origin = character(), override_unitid = character(), override_institution_name = character(), override_state = character(), override_announcement_date = character(), override_announcement_year = character(), override_cut_type = character(), override_cut_description = character(), override_source_url = character(), override_source_title = character(), override_source_publication = character(), first_seen = character(), review_status = character(), reviewer = character(), reviewer_notes = character(), reviewed_at = character(), grandfathered = logical(), stringsAsFactors = FALSE
  )
}

read_college_cuts_review_candidates <- function(path) {
  coerce_college_cuts_review_candidates(readr::read_csv(path, show_col_types = FALSE, col_types = COLLEGE_CUTS_REVIEW_CANDIDATE_COL_TYPES))
}

read_college_cuts_editorial_overrides <- function(path) {
  coerce_college_cuts_editorial_overrides(readr::read_csv(path, show_col_types = FALSE, col_types = COLLEGE_CUTS_EDITORIAL_OVERRIDE_COL_TYPES))
}

compute_college_cuts_review_id <- function(cut_id,
                                           unitid = NA_character_,
                                           announcement_date = NA_character_,
                                           program_name = NA_character_,
                                           institution_name = NA_character_,
                                           state = NA_character_) {
  cut_id_value <- trim_text(cut_id)
  if (nzchar(cut_id_value)) return(cut_id_value)
  identity_value <- trim_text(unitid)
  if (!nzchar(identity_value)) identity_value <- paste(trim_text(institution_name), trim_text(state), sep = "|")
  seed <- paste(normalize_review_identity_text(identity_value), normalize_review_identity_text(announcement_date), normalize_review_identity_text(program_name), sep = "|")
  paste0("editor-", substr(digest::digest(seed, algo = "sha1", serialize = FALSE), 1L, 12L))
}

assert_unique_cut_ids <- function(df, label) {
  if (!"cut_id" %in% names(df) || !nrow(df)) return(invisible(df))
  ids <- trim_text(df$cut_id)
  duplicate_ids <- unique(ids[nzchar(ids) & duplicated(ids)])
  if (length(duplicate_ids) > 0L) {
    stop(sprintf("%s contains duplicate cut_id values: %s", label, paste(duplicate_ids, collapse = ", ")), call. = FALSE)
  }
  invisible(df)
}

coerce_college_cuts_review_candidates <- function(df) {
  if (is.null(df) || !nrow(df)) return(empty_college_cuts_review_candidates())
  missing_columns <- setdiff(COLLEGE_CUTS_REVIEW_CANDIDATE_COLUMNS, names(df))
  if (length(missing_columns) > 0L) stop(sprintf("College cuts review candidates are missing required columns: %s", paste(missing_columns, collapse = ", ")), call. = FALSE)

  candidates <- data.frame(
    cut_id = trim_text(df$cut_id),
    unitid = trim_optional_text(df$unitid),
    institution_name = trim_optional_text(df$institution_name),
    state = trim_optional_text(df$state),
    announcement_date = trim_optional_text(df$announcement_date),
    announcement_year = trim_optional_text(df$announcement_year),
    cut_type = trim_optional_text(df$cut_type),
    program_name = trim_optional_text(df$program_name),
    source_url = trim_optional_text(df$source_url),
    source_title = trim_optional_text(df$source_title),
    source_publication = trim_optional_text(df$source_publication),
    row_origin = normalize_review_row_origin(df$row_origin),
    stringsAsFactors = FALSE
  )
  candidates$row_origin[is.na(candidates$row_origin)] <- "scraper"
  assert_unique_cut_ids(candidates, "College cuts review candidates")
  candidates
}

build_college_cuts_review_candidates <- function(cuts_df) {
  if (is.null(cuts_df) || !nrow(cuts_df)) return(empty_college_cuts_review_candidates())
  required_columns <- c("cut_id", "matched_unitid", "export_unitid", "institution_name_display", "state_display", "announcement_date", "announcement_year", "cut_type", "program_name", "source_url", "source_title", "source_publication")
  missing_columns <- setdiff(required_columns, names(cuts_df))
  if (length(missing_columns) > 0L) stop(sprintf("build_college_cuts_review_candidates requires these columns: %s", paste(missing_columns, collapse = ", ")), call. = FALSE)

  candidates <- data.frame(
    cut_id = vapply(seq_len(nrow(cuts_df)), function(i) compute_college_cuts_review_id(cuts_df$cut_id[[i]], dplyr::coalesce(cuts_df$matched_unitid[[i]], cuts_df$export_unitid[[i]]), cuts_df$announcement_date[[i]], cuts_df$program_name[[i]], cuts_df$institution_name_display[[i]], cuts_df$state_display[[i]]), character(1)),
    unitid = trim_optional_text(dplyr::coalesce(cuts_df$matched_unitid, cuts_df$export_unitid)),
    institution_name = trim_optional_text(cuts_df$institution_name_display),
    state = trim_optional_text(cuts_df$state_display),
    announcement_date = trim_optional_text(cuts_df$announcement_date),
    announcement_year = trim_optional_text(cuts_df$announcement_year),
    cut_type = trim_optional_text(cuts_df$cut_type),
    program_name = trim_optional_text(cuts_df$program_name),
    source_url = trim_optional_text(cuts_df$source_url),
    source_title = trim_optional_text(cuts_df$source_title),
    source_publication = trim_optional_text(cuts_df$source_publication),
    row_origin = rep("scraper", nrow(cuts_df)),
    stringsAsFactors = FALSE
  )
  candidates <- candidates[!duplicated(candidates$cut_id), COLLEGE_CUTS_REVIEW_CANDIDATE_COLUMNS, drop = FALSE]
  assert_unique_cut_ids(candidates, "College cuts review candidates")
  candidates
}

coerce_college_cuts_review_sheet_rows <- function(df,
                                                  default_first_seen = as.character(Sys.Date())) {
  if (is.null(df) || !nrow(df)) return(empty_college_cuts_review_sheet_rows())
  raw_rows <- normalize_college_cuts_sheet_headers(df)
  assert_valid_review_row_origins(raw_rows, id_column = "cut_id", context = "College cuts review sheet rows")
  assert_blank_ids_only_for_manual_rows(raw_rows, id_column = "cut_id", context = "College cuts review sheet rows")

  sheet_rows <- rep_like_template_rows(empty_college_cuts_review_sheet_rows(), nrow(raw_rows))
  for (column_name in setdiff(COLLEGE_CUTS_REVIEW_SHEET_COLUMNS, "grandfathered")) {
    sheet_rows[[column_name]] <- if (column_name %in% names(raw_rows)) trim_optional_text(raw_rows[[column_name]]) else NA_character_
  }
  sheet_rows$grandfathered <- if ("grandfathered" %in% names(raw_rows)) coerce_false_default_logical(raw_rows$grandfathered) else FALSE
  if ("editor_cut_description" %in% names(raw_rows)) sheet_rows$cut_description <- dplyr::coalesce(trim_optional_text(raw_rows$editor_cut_description), sheet_rows$cut_description)
  if ("editor_announcement_date" %in% names(raw_rows)) sheet_rows$announcement_date <- dplyr::coalesce(trim_optional_text(raw_rows$editor_announcement_date), sheet_rows$announcement_date)
  if ("editor_cut_type" %in% names(raw_rows)) sheet_rows$cut_type <- dplyr::coalesce(trim_optional_text(raw_rows$editor_cut_type), sheet_rows$cut_type)
  if ("editor_source_url" %in% names(raw_rows)) sheet_rows$source_url <- dplyr::coalesce(trim_optional_text(raw_rows$editor_source_url), sheet_rows$source_url)
  if ("editor_source_publication" %in% names(raw_rows)) sheet_rows$source_publication <- dplyr::coalesce(trim_optional_text(raw_rows$editor_source_publication), sheet_rows$source_publication)
  sheet_rows$announcement_year <- dplyr::coalesce(sheet_rows$announcement_year, derive_year_from_date_string(sheet_rows$announcement_date))
  sheet_rows$row_origin <- normalize_review_row_origin(sheet_rows$row_origin)
  sheet_rows$row_origin[is.na(sheet_rows$row_origin)] <- "scraper"

  missing_manual_ids <- which(!nzchar(trim_text(sheet_rows$cut_id)) & sheet_rows$row_origin == "manual")
  if (length(missing_manual_ids)) {
    sheet_rows$cut_id[missing_manual_ids] <- vapply(missing_manual_ids, function(i) compute_college_cuts_review_id(sheet_rows$cut_id[[i]], sheet_rows$unitid[[i]], sheet_rows$announcement_date[[i]], sheet_rows$cut_description[[i]], sheet_rows$institution_name[[i]], sheet_rows$state[[i]]), character(1))
  }
  sheet_rows$first_seen[is.na(sheet_rows$first_seen) & sheet_rows$row_origin == "manual"] <- default_first_seen
  assert_manual_review_required_fields(sheet_rows, "cut_id", COLLEGE_CUTS_REQUIRED_MANUAL_FIELDS, "College cuts review sheet rows")
  assert_unique_cut_ids(sheet_rows, "College cuts review sheet rows")
  sheet_rows[, COLLEGE_CUTS_REVIEW_SHEET_COLUMNS, drop = FALSE]
}

coerce_college_cuts_editorial_overrides <- function(df) {
  if (is.null(df) || !nrow(df)) return(empty_college_cuts_editorial_overrides())
  normalized <- normalize_college_cuts_sheet_headers(df)
  overrides <- rep_like_template_rows(empty_college_cuts_editorial_overrides(), nrow(normalized))
  overrides$cut_id <- if ("cut_id" %in% names(normalized)) trim_text(normalized$cut_id) else ""

  direct_source_map <- c(unitid = "source_unitid", institution_name = "source_institution_name", state = "source_state", announcement_date = "source_announcement_date", announcement_year = "source_announcement_year", cut_type = "source_cut_type", cut_description = "source_cut_description", source_url = "source_source_url", source_publication = "source_source_publication", row_origin = "source_row_origin")
  for (field_name in names(direct_source_map)) {
    source_column <- direct_source_map[[field_name]]
    overrides[[source_column]] <- if (source_column %in% names(normalized)) trim_optional_text(normalized[[source_column]]) else if (field_name %in% names(normalized)) trim_optional_text(normalized[[field_name]]) else NA_character_
  }
  overrides$source_source_title <- if ("source_source_title" %in% names(normalized)) trim_optional_text(normalized$source_source_title) else if ("source_title" %in% names(normalized)) trim_optional_text(normalized$source_title) else NA_character_

  new_override_map <- c(unitid = "override_unitid", institution_name = "override_institution_name", state = "override_state", announcement_date = "override_announcement_date", announcement_year = "override_announcement_year", cut_type = "override_cut_type", cut_description = "override_cut_description", source_url = "override_source_url", source_title = "override_source_title", source_publication = "override_source_publication")
  legacy_override_map <- c(cut_description = "editor_cut_description", announcement_date = "editor_announcement_date", cut_type = "editor_cut_type", source_url = "editor_source_url", source_title = "editor_source_title", source_publication = "editor_source_publication")
  for (field_name in names(new_override_map)) {
    override_column <- new_override_map[[field_name]]
    legacy_override_column <- if (field_name %in% names(legacy_override_map)) legacy_override_map[[field_name]] else NA_character_
    overrides[[override_column]] <- if (override_column %in% names(normalized)) trim_optional_text(normalized[[override_column]]) else if (!is.na(legacy_override_column) && legacy_override_column %in% names(normalized)) trim_optional_text(normalized[[legacy_override_column]]) else NA_character_
  }

  overrides$first_seen <- if ("first_seen" %in% names(normalized)) trim_optional_text(normalized$first_seen) else NA_character_
  overrides$review_status <- if ("review_status" %in% names(normalized)) trim_optional_text(normalized$review_status) else NA_character_
  overrides$reviewer <- if ("reviewer" %in% names(normalized)) trim_optional_text(normalized$reviewer) else NA_character_
  overrides$reviewer_notes <- if ("reviewer_notes" %in% names(normalized)) trim_optional_text(normalized$reviewer_notes) else NA_character_
  overrides$reviewed_at <- if ("reviewed_at" %in% names(normalized)) trim_optional_text(normalized$reviewed_at) else NA_character_
  overrides$grandfathered <- if ("grandfathered" %in% names(normalized)) coerce_false_default_logical(normalized$grandfathered) else FALSE
  overrides$source_row_origin <- normalize_review_row_origin(overrides$source_row_origin)
  overrides$source_row_origin[is.na(overrides$source_row_origin)] <- "scraper"
  overrides$source_announcement_year <- dplyr::coalesce(overrides$source_announcement_year, derive_year_from_date_string(overrides$source_announcement_date))

  missing_manual_ids <- which(!nzchar(trim_text(overrides$cut_id)) & overrides$source_row_origin == "manual")
  if (length(missing_manual_ids)) {
    overrides$cut_id[missing_manual_ids] <- vapply(
      missing_manual_ids,
      function(i) {
        compute_college_cuts_review_id(
          overrides$cut_id[[i]],
          overrides$source_unitid[[i]],
          overrides$source_announcement_date[[i]],
          overrides$source_cut_description[[i]],
          overrides$source_institution_name[[i]],
          overrides$source_state[[i]]
        )
      },
      character(1)
    )
    missing_first_seen <- is.na(overrides$first_seen[missing_manual_ids])
    if (any(missing_first_seen)) {
      overrides$first_seen[missing_manual_ids[missing_first_seen]] <- as.character(Sys.Date())
    }
  }
  assert_unique_cut_ids(overrides, "College cuts editorial overrides")
  overrides[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]
}

build_college_cuts_review_sheet_rows <- function(overrides) {
  local_rows <- coerce_college_cuts_editorial_overrides(overrides)
  if (!nrow(local_rows)) return(empty_college_cuts_review_sheet_rows())
  sheet_rows <- rep_like_template_rows(empty_college_cuts_review_sheet_rows(), nrow(local_rows))
  sheet_rows$cut_id <- trim_text(local_rows$cut_id)
  for (field_name in names(COLLEGE_CUTS_SHEET_SOURCE_MAP)) {
    source_column <- COLLEGE_CUTS_SHEET_SOURCE_MAP[[field_name]]
    sheet_rows[[field_name]] <- if (field_name %in% names(COLLEGE_CUTS_SHEET_OVERRIDE_MAP)) effective_override_values(local_rows[[source_column]], local_rows[[COLLEGE_CUTS_SHEET_OVERRIDE_MAP[[field_name]]]]) else local_rows[[source_column]]
  }
  sheet_rows$first_seen <- local_rows$first_seen
  sheet_rows$review_status <- local_rows$review_status
  sheet_rows$reviewer <- local_rows$reviewer
  sheet_rows$reviewer_notes <- local_rows$reviewer_notes
  sheet_rows$reviewed_at <- local_rows$reviewed_at
  sheet_rows$grandfathered <- local_rows$grandfathered
  sheet_rows$announcement_year <- dplyr::coalesce(sheet_rows$announcement_year, derive_year_from_date_string(sheet_rows$announcement_date))
  sheet_rows[, COLLEGE_CUTS_REVIEW_SHEET_COLUMNS, drop = FALSE]
}

assert_college_cuts_review_sheet_header <- function(df) {
  if (is.null(df) || !ncol(df)) return(invisible(df))
  header_names <- names(normalize_college_cuts_sheet_headers(df))
  missing_columns <- setdiff(COLLEGE_CUTS_REVIEW_SHEET_COLUMNS, header_names)
  if (length(missing_columns) > 0L) stop(sprintf("Google Sheet tab is missing required columns: %s", paste(missing_columns, collapse = ", ")), call. = FALSE)
  invisible(df)
}

stage_college_cuts_editorial_overrides <- function(candidates,
                                                   existing = NULL,
                                                   first_seen = as.character(Sys.Date())) {
  review_candidates <- coerce_college_cuts_review_candidates(candidates)
  overrides <- coerce_college_cuts_editorial_overrides(existing)
  if (!nrow(review_candidates)) return(overrides[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])
  if (nrow(overrides)) {
    candidate_index <- match(trim_text(overrides$cut_id), review_candidates$cut_id)
    matched <- !is.na(candidate_index)
    if (any(matched)) {
      for (field_name in names(COLLEGE_CUTS_CANDIDATE_SOURCE_MAP)) {
        overrides[[COLLEGE_CUTS_CANDIDATE_SOURCE_MAP[[field_name]]]][matched] <- review_candidates[[field_name]][candidate_index[matched]]
      }
    }
  }
  new_rows <- review_candidates[!(review_candidates$cut_id %in% trim_text(overrides$cut_id)), , drop = FALSE]
  if (!nrow(new_rows)) return(coerce_college_cuts_editorial_overrides(overrides))
  new_internal_rows <- rep_like_template_rows(empty_college_cuts_editorial_overrides(), nrow(new_rows))
  new_internal_rows$cut_id <- new_rows$cut_id
  for (field_name in names(COLLEGE_CUTS_CANDIDATE_SOURCE_MAP)) new_internal_rows[[COLLEGE_CUTS_CANDIDATE_SOURCE_MAP[[field_name]]]] <- new_rows[[field_name]]
  new_internal_rows$first_seen <- first_seen
  new_internal_rows$review_status <- "unreviewed"
  new_internal_rows$grandfathered <- FALSE
  coerce_college_cuts_editorial_overrides(dplyr::bind_rows(overrides, new_internal_rows))
}

build_college_cuts_review_sheet_append_rows <- function(overrides, existing_sheet = NULL) {
  local_sheet_rows <- build_college_cuts_review_sheet_rows(overrides)
  sheet_rows <- coerce_college_cuts_review_sheet_rows(existing_sheet)
  if (!nrow(local_sheet_rows)) return(local_sheet_rows)
  if (!nrow(sheet_rows)) return(local_sheet_rows)
  local_sheet_rows[!(trim_text(local_sheet_rows$cut_id) %in% trim_text(sheet_rows$cut_id)), COLLEGE_CUTS_REVIEW_SHEET_COLUMNS, drop = FALSE]
}

merge_college_cuts_review_sheet_editor_columns <- function(overrides,
                                                           sheet_rows,
                                                           allow_editor_added_rows = FALSE,
                                                           first_seen = as.character(Sys.Date())) {
  local_rows <- coerce_college_cuts_editorial_overrides(overrides)
  sheet_data <- coerce_college_cuts_review_sheet_rows(sheet_rows, default_first_seen = first_seen)
  if (!nrow(sheet_data)) return(local_rows[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])

  local_ids <- trim_text(local_rows$cut_id)
  sheet_ids <- trim_text(sheet_data$cut_id)
  sheet_only <- sheet_data[!(sheet_ids %in% local_ids), , drop = FALSE]
  if (nrow(sheet_only) > 0L) {
    non_manual <- sheet_only[normalize_review_row_origin(sheet_only$row_origin) != "manual", , drop = FALSE]
    if (nrow(non_manual) > 0L && !isTRUE(allow_editor_added_rows)) {
      sample_ids <- paste(utils::head(non_manual$cut_id, 5L), collapse = ", ")
      stop(sprintf(paste("Google Sheet contains %d cut_id value(s) that are not present in editorial_overrides.csv.", "Only row_origin = manual rows may exist only in the sheet.", "Sample cut_id values: %s"), nrow(non_manual), sample_ids), call. = FALSE)
    }
  }

  if (nrow(local_rows)) {
    match_index <- match(local_ids, sheet_ids)
    matched <- !is.na(match_index)
    if (any(matched)) {
      matched_sheet_rows <- sheet_data[match_index[matched], , drop = FALSE]
      matched_rows <- which(matched)
      matched_manual <- local_rows$source_row_origin[matched] == "manual"
      matched_manual_rows <- matched_rows[matched_manual]
      for (field_name in names(COLLEGE_CUTS_SHEET_OVERRIDE_MAP)) {
        source_column <- COLLEGE_CUTS_SHEET_SOURCE_MAP[[field_name]]
        override_column <- COLLEGE_CUTS_SHEET_OVERRIDE_MAP[[field_name]]
        local_rows[[override_column]][matched] <- compute_override_delta(matched_sheet_rows[[field_name]], local_rows[[source_column]][matched])
        if (length(matched_manual_rows)) {
          local_rows[[source_column]][matched_manual_rows] <- matched_sheet_rows[[field_name]][matched_manual]
          local_rows[[override_column]][matched_manual_rows] <- NA_character_
        }
      }
      local_rows$review_status[matched] <- matched_sheet_rows$review_status
      local_rows$reviewer[matched] <- matched_sheet_rows$reviewer
      local_rows$reviewer_notes[matched] <- matched_sheet_rows$reviewer_notes
      local_rows$reviewed_at[matched] <- matched_sheet_rows$reviewed_at
      local_rows$grandfathered[matched] <- matched_sheet_rows$grandfathered
      new_first_seen <- matched_sheet_rows$first_seen
      keep_existing_first_seen <- is.na(new_first_seen)
      if (any(!keep_existing_first_seen)) {
        local_rows$first_seen[matched_rows[!keep_existing_first_seen]] <- new_first_seen[!keep_existing_first_seen]
      }
    }
  }

  sheet_only_manual <- sheet_only[normalize_review_row_origin(sheet_only$row_origin) == "manual", , drop = FALSE]
  if (nrow(sheet_only_manual) > 0L) {
    manual_rows <- rep_like_template_rows(empty_college_cuts_editorial_overrides(), nrow(sheet_only_manual))
    manual_rows$cut_id <- sheet_only_manual$cut_id
    for (field_name in names(COLLEGE_CUTS_SHEET_SOURCE_MAP)) manual_rows[[COLLEGE_CUTS_SHEET_SOURCE_MAP[[field_name]]]] <- sheet_only_manual[[field_name]]
    manual_rows$source_source_title <- dplyr::coalesce(if ("source_title" %in% names(sheet_only)) trim_optional_text(sheet_only_manual$source_title) else rep(NA_character_, nrow(sheet_only_manual)), sheet_only_manual$source_publication)
    manual_rows$first_seen <- dplyr::coalesce(sheet_only_manual$first_seen, first_seen)
    manual_rows$review_status <- sheet_only_manual$review_status
    manual_rows$reviewer <- sheet_only_manual$reviewer
    manual_rows$reviewer_notes <- sheet_only_manual$reviewer_notes
    manual_rows$reviewed_at <- sheet_only_manual$reviewed_at
    manual_rows$grandfathered <- sheet_only_manual$grandfathered
    local_rows <- dplyr::bind_rows(local_rows, manual_rows)
  }

  coerce_college_cuts_editorial_overrides(local_rows)
}

grandfather_college_cuts_editorial_overrides <- function(overrides,
                                                         reviewed_at = as.character(Sys.Date()),
                                                         reviewer = "grandfathered",
                                                         only_statuses = c(NA_character_, "", "unreviewed")) {
  local_rows <- coerce_college_cuts_editorial_overrides(overrides)
  if (!nrow(local_rows)) return(local_rows[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])
  status_values <- trim_text(local_rows$review_status)
  eligible_statuses <- trim_text(only_statuses)
  eligible_statuses[is.na(only_statuses)] <- ""
  to_grandfather <- status_values %in% eligible_statuses
  if (!any(to_grandfather)) return(local_rows[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])
  local_rows$review_status[to_grandfather] <- "approved"
  local_rows$grandfathered[to_grandfather] <- TRUE
  reviewed_at_value <- trim_optional_text(reviewed_at)
  reviewer_value <- trim_optional_text(reviewer)
  if (!is.na(reviewed_at_value)) local_rows$reviewed_at[to_grandfather] <- reviewed_at_value
  if (!is.na(reviewer_value)) local_rows$reviewer[to_grandfather] <- reviewer_value
  local_rows[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]
}

build_manual_college_cuts_export_row <- function(override_row, template_df) {
  internal_row <- coerce_college_cuts_editorial_overrides(override_row)
  if (!nrow(internal_row)) return(template_df[0, , drop = FALSE])
  effective_row <- build_college_cuts_review_sheet_rows(internal_row)[1, , drop = FALSE]
  manual_row <- blank_like_row(template_df)
  if (!("row_origin" %in% names(manual_row))) manual_row$row_origin <- NA_character_
  export_unitid <- if (is.na(effective_row$unitid[[1]]) || !nzchar(effective_row$unitid[[1]])) paste0("manual-cut-", effective_row$cut_id[[1]]) else effective_row$unitid[[1]]
  source_title_value <- dplyr::coalesce(internal_row$override_source_title[[1]], internal_row$source_source_title[[1]], effective_row$source_publication[[1]])

  if ("cut_id" %in% names(manual_row)) manual_row$cut_id[[1]] <- effective_row$cut_id[[1]]
  if ("matched_unitid" %in% names(manual_row)) manual_row$matched_unitid[[1]] <- effective_row$unitid[[1]]
  if ("export_unitid" %in% names(manual_row)) manual_row$export_unitid[[1]] <- export_unitid
  if ("institution_name_display" %in% names(manual_row)) manual_row$institution_name_display[[1]] <- effective_row$institution_name[[1]]
  if ("state_display" %in% names(manual_row)) manual_row$state_display[[1]] <- effective_row$state[[1]]
  if ("announcement_date" %in% names(manual_row)) manual_row$announcement_date[[1]] <- effective_row$announcement_date[[1]]
  if ("announcement_year" %in% names(manual_row)) manual_row$announcement_year[[1]] <- suppressWarnings(as.integer(dplyr::coalesce(effective_row$announcement_year[[1]], derive_year_from_date_string(effective_row$announcement_date[[1]]))))
  if ("cut_type" %in% names(manual_row)) manual_row$cut_type[[1]] <- effective_row$cut_type[[1]]
  if ("program_name" %in% names(manual_row)) manual_row$program_name[[1]] <- effective_row$cut_description[[1]]
  if ("source_url" %in% names(manual_row)) manual_row$source_url[[1]] <- effective_row$source_url[[1]]
  if ("source_title" %in% names(manual_row)) manual_row$source_title[[1]] <- source_title_value
  if ("source_publication" %in% names(manual_row)) manual_row$source_publication[[1]] <- effective_row$source_publication[[1]]
  manual_row$row_origin[[1]] <- "manual"
  if ("has_financial_profile" %in% names(manual_row)) manual_row$has_financial_profile[[1]] <- !is.na(effective_row$unitid[[1]]) && nzchar(effective_row$unitid[[1]])
  if ("is_primary_tracker" %in% names(manual_row)) manual_row$is_primary_tracker[[1]] <- FALSE
  if ("in_financial_tracker" %in% names(manual_row)) manual_row$in_financial_tracker[[1]] <- "FALSE"
  manual_row
}

apply_college_cuts_editorial_overrides <- function(cuts_df,
                                                   overrides = NULL,
                                                   enforce_review_gate = FALSE,
                                                   allowed_cut_ids = NULL,
                                                   drop_unlisted = FALSE) {
  override_rows <- coerce_college_cuts_editorial_overrides(overrides)
  manual_origin_mask <- trim_text(override_rows$source_row_origin) == "manual"
  approved_review_mask <- trim_text(override_rows$review_status) == "approved"
  manual_origin_mask[is.na(manual_origin_mask)] <- FALSE
  approved_review_mask[is.na(approved_review_mask)] <- FALSE
  published_override_rows <- override_rows[approved_review_mask, , drop = FALSE]
  approved_manual_rows <- published_override_rows[trim_text(published_override_rows$source_row_origin) == "manual", , drop = FALSE]
  if (is.null(cuts_df)) {
    if (!nrow(approved_manual_rows)) return(cuts_df)
    stop("Approved manual college cuts rows need a cuts data frame template during export.", call. = FALSE)
  }

  required_columns <- c("cut_id", "matched_unitid", "export_unitid", "institution_name_display", "state_display", "announcement_date", "announcement_year", "cut_type", "program_name", "source_url", "source_title", "source_publication")
  missing_columns <- setdiff(required_columns, names(cuts_df))
  if (length(missing_columns) > 0L) stop(sprintf("apply_college_cuts_editorial_overrides requires these columns: %s", paste(missing_columns, collapse = ", ")), call. = FALSE)

  if (!nrow(cuts_df)) {
    if (!nrow(approved_manual_rows)) return(cuts_df)
    return(dplyr::bind_rows(cuts_df, dplyr::bind_rows(lapply(seq_len(nrow(approved_manual_rows)), function(i) build_manual_college_cuts_export_row(approved_manual_rows[i, , drop = FALSE], cuts_df)))))
  }

  review_cuts <- cuts_df
  review_cuts$cut_id <- vapply(seq_len(nrow(review_cuts)), function(i) compute_college_cuts_review_id(review_cuts$cut_id[[i]], dplyr::coalesce(review_cuts$matched_unitid[[i]], review_cuts$export_unitid[[i]]), review_cuts$announcement_date[[i]], review_cuts$program_name[[i]], review_cuts$institution_name_display[[i]], review_cuts$state_display[[i]]), character(1))
  allowed_ids <- trim_text(allowed_cut_ids)
  allowed_ids <- unique(allowed_ids[nzchar(allowed_ids)])
  if (length(allowed_ids) > 0L && isTRUE(drop_unlisted)) {
    unexpected_rows <- !(review_cuts$cut_id %in% allowed_ids)
    if (any(unexpected_rows)) {
      message(sprintf(paste("Apply-only college cuts review gate: ignoring %d recomputed cut row(s)", "that are not present in the committed review candidate snapshot."), sum(unexpected_rows)))
      review_cuts <- review_cuts[!unexpected_rows, , drop = FALSE]
    }
  }

  if (!nrow(override_rows)) {
    if (isTRUE(enforce_review_gate)) stop("College cuts review gate is enabled but editorial_overrides.csv is empty or missing.", call. = FALSE)
    return(review_cuts)
  }
  if (!nrow(published_override_rows) && !isTRUE(enforce_review_gate)) {
    if ("row_origin" %in% names(review_cuts)) {
      review_cuts$row_origin <- dplyr::coalesce(trim_optional_text(review_cuts$row_origin), rep("scraper", nrow(review_cuts)))
    } else {
      review_cuts$row_origin <- rep("scraper", nrow(review_cuts))
    }
    return(review_cuts)
  }

  joined <- review_cuts %>% dplyr::left_join(published_override_rows[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE], by = "cut_id")
  if (isTRUE(enforce_review_gate)) {
    if (length(allowed_ids) > 0L) {
      missing_snapshot_override_ids <- allowed_ids[!(allowed_ids %in% trim_text(override_rows$cut_id))]
      if (length(missing_snapshot_override_ids) > 0L) stop(sprintf(paste("College cuts review gate is enabled but %d committed college cuts review candidate(s) are missing editorial overrides.", "Sample cut_id values: %s"), length(missing_snapshot_override_ids), paste(utils::head(missing_snapshot_override_ids, 5L), collapse = ", ")), call. = FALSE)
    } else {
      missing_override <- is.na(joined$review_status)
      if (any(missing_override)) stop(sprintf(paste("College cuts review gate is enabled but %d cut row(s) are missing editorial overrides.", "Sample cut_id values: %s"), sum(missing_override), paste(utils::head(joined$cut_id[missing_override], 5L), collapse = ", ")), call. = FALSE)
    }
  }

  joined_approved_mask <- trim_text(joined$review_status) == "approved"
  joined_approved_mask[is.na(joined_approved_mask)] <- FALSE
  approved_override_values <- function(values) {
    approved_values <- values
    approved_values[!joined_approved_mask] <- NA_character_
    approved_values
  }

  joined$matched_unitid <- effective_override_values(joined$matched_unitid, approved_override_values(joined$override_unitid))
  joined$institution_name_display <- effective_override_values(joined$institution_name_display, approved_override_values(joined$override_institution_name))
  joined$state_display <- effective_override_values(joined$state_display, approved_override_values(joined$override_state))
  joined$announcement_date <- effective_override_values(joined$announcement_date, approved_override_values(joined$override_announcement_date))
  if ("announcement_year" %in% names(joined)) joined$announcement_year <- suppressWarnings(as.integer(dplyr::coalesce(effective_override_values(trim_optional_text(joined$announcement_year), approved_override_values(joined$override_announcement_year)), derive_year_from_date_string(joined$announcement_date))))
  joined$cut_type <- effective_override_values(joined$cut_type, approved_override_values(joined$override_cut_type))
  joined$program_name <- effective_override_values(joined$program_name, approved_override_values(joined$override_cut_description))
  joined$source_url <- effective_override_values(joined$source_url, approved_override_values(joined$override_source_url))
  joined$source_title <- effective_override_values(joined$source_title, approved_override_values(joined$override_source_title))
  joined$source_publication <- effective_override_values(joined$source_publication, approved_override_values(joined$override_source_publication))
  if ("export_unitid" %in% names(joined)) joined$export_unitid <- dplyr::if_else(!is.na(joined$matched_unitid) & trim_text(joined$matched_unitid) != "", trim_text(joined$matched_unitid), joined$export_unitid)
  if ("has_financial_profile" %in% names(joined)) joined$has_financial_profile <- !is.na(joined$matched_unitid) & trim_text(joined$matched_unitid) != ""
  if ("row_origin" %in% names(joined)) {
    joined$row_origin <- dplyr::coalesce(trim_optional_text(joined$row_origin), trim_optional_text(joined$source_row_origin), rep("scraper", nrow(joined)))
  } else {
    joined$row_origin <- dplyr::coalesce(trim_optional_text(joined$source_row_origin), rep("scraper", nrow(joined)))
  }
  if (isTRUE(enforce_review_gate)) joined <- joined[joined_approved_mask, , drop = FALSE]

  manual_rows <- approved_manual_rows[!(trim_text(approved_manual_rows$cut_id) %in% trim_text(review_cuts$cut_id)), , drop = FALSE]
  if (nrow(manual_rows)) joined <- dplyr::bind_rows(joined, dplyr::bind_rows(lapply(seq_len(nrow(manual_rows)), function(i) build_manual_college_cuts_export_row(manual_rows[i, , drop = FALSE], review_cuts))))
  joined
}
