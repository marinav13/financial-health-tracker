trim_optional_text <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  if (length(x) == 0L) {
    return(character(0))
  }
  values <- trimws(as.character(x))
  values[!is.na(values) & values == ""] <- NA_character_
  values
}

trim_text <- function(x) {
  if (is.null(x)) {
    return("")
  }
  if (length(x) == 0L) {
    return(character(0))
  }
  values <- as.character(x)
  values[is.na(values)] <- ""
  trimws(values)
}

is_terminal_review_decision <- function(x) {
  trim_text(x) %in% c("approved", "reject")
}

REVIEW_GATE_IGNORED_ROWS_WARN_THRESHOLD <- 20L

# A review row "carries an editorial decision" when an editor has touched
# it. Any non-blank review_status other than the staging default
# "unreviewed" counts -- deliberately including unexpected vocabulary, so
# unknown statuses block destructive paths instead of passing them.
# Populated reviewer metadata also counts even when review_status was left
# at the default. Guards call this before any path that could discard
# sheet rows (stale-row dropping, full-tab rewrites).
review_sheet_row_has_decision <- function(review_status,
                                          reviewer = NULL,
                                          reviewer_notes = NULL,
                                          reviewed_at = NULL) {
  status_values <- trim_text(review_status)
  mask <- nzchar(status_values) & status_values != "unreviewed"
  for (values in list(reviewer, reviewer_notes, reviewed_at)) {
    if (!is.null(values)) {
      mask <- mask | nzchar(trim_text(values))
    }
  }
  mask
}

# Rows in the live review tab that a full-tab rewrite would destroy:
# decision-carrying rows missing from the rewrite payload entirely, or
# present but with the decision reverted to unreviewed. The rewrite
# scripts refuse to overwrite the tab while this returns rows unless
# --force-discard-decisions is passed.
find_review_rows_lost_by_rewrite <- function(current_rows, payload_rows, id_column) {
  if (is.null(current_rows) || !nrow(current_rows)) {
    return(current_rows)
  }
  current_ids <- trim_text(current_rows[[id_column]])
  payload_ids <- trim_text(payload_rows[[id_column]])
  decision_mask <- review_sheet_row_has_decision(
    current_rows$review_status,
    current_rows$reviewer,
    current_rows$reviewer_notes,
    current_rows$reviewed_at
  )
  payload_index <- match(current_ids, payload_ids)
  missing_mask <- decision_mask & (!nzchar(current_ids) | is.na(payload_index))
  payload_status <- trim_text(payload_rows$review_status[payload_index])
  status_values <- trim_text(current_rows$review_status)
  reverted_mask <- nzchar(status_values) & status_values != "unreviewed" &
    !is.na(payload_index) &
    (!nzchar(payload_status) | payload_status == "unreviewed")
  current_rows[missing_mask | reverted_mask, , drop = FALSE]
}

# Durable preservation for decision-carrying sheet rows that a pull had to
# exclude from the merge (stale sheet-only rows). Appends to the committed
# quarantine CSV, deduplicating on id + quarantined_at so repeated pulls on
# the same day do not grow the file.
append_review_quarantine_rows <- function(rows, path, id_column) {
  if (is.null(rows) || !nrow(rows)) {
    return(invisible(NULL))
  }
  quarantined <- as.data.frame(
    lapply(rows, function(x) as.character(x)),
    stringsAsFactors = FALSE
  )
  quarantined$quarantined_at <- as.character(Sys.Date())
  existing <- if (file.exists(path)) {
    readr::read_csv(
      path,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    )
  } else {
    NULL
  }
  combined <- dplyr::bind_rows(existing, quarantined)
  dedupe_key <- paste(trim_text(combined[[id_column]]), trim_text(combined$quarantined_at))
  combined <- combined[!duplicated(dedupe_key), , drop = FALSE]
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_csv_atomic(combined, path)
  invisible(combined)
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

looks_like_iso_date <- function(x) {
  values <- trim_optional_text(x)
  if (length(values) == 0L) {
    return(logical(0))
  }
  !is.na(values) & grepl("^\\d{4}-\\d{2}-\\d{2}$", values)
}

looks_like_http_url <- function(x) {
  values <- trim_optional_text(x)
  if (length(values) == 0L) {
    return(logical(0))
  }
  !is.na(values) & grepl("^(https?://|\\[https?://)", values, ignore.case = TRUE)
}

ACCREDITATION_REVIEW_ROW_ORIGINS <- c("scraper", "manual")
COLLEGE_CUTS_REVIEW_ROW_ORIGINS <- c("scraper", "manual", "hechinger")
COLLEGE_CUTS_HUMAN_ROW_ORIGINS <- c("manual", "hechinger")

row_origin_in <- function(x, allowed_values) {
  values <- normalize_review_row_origin(x)
  !is.na(values) & values %in% allowed_values
}

is_college_cuts_human_row_origin <- function(x) {
  row_origin_in(x, COLLEGE_CUTS_HUMAN_ROW_ORIGINS)
}

derive_year_from_date_string <- function(x) {
  value <- trim_optional_text(x)
  if (length(value) == 0L) {
    return(character(0))
  }
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

  template_df[rep(NA_integer_, n), , drop = FALSE]
}

assert_valid_review_row_origins <- function(df,
                                            id_column,
                                            row_origin_column = "row_origin",
                                            context = "Google Sheet rows",
                                            valid_values = ACCREDITATION_REVIEW_ROW_ORIGINS) {
  if (is.null(df) || !nrow(df) || !(row_origin_column %in% names(df))) {
    return(invisible(df))
  }

  row_origin <- normalize_review_row_origin(df[[row_origin_column]])
  valid_mask <- is.na(row_origin) | row_origin %in% valid_values
  if (all(valid_mask)) {
    return(invisible(df))
  }

  bad_rows <- which(!valid_mask)
  sample_label <- vapply(
    utils::head(bad_rows, 5L),
    function(row_index) {
      row_id <- trim_text(df[[id_column]][[row_index]])
      row_origin_value <- trim_text(df[[row_origin_column]][[row_index]])
      row_prefix <- if (nzchar(row_id)) {
        paste0(id_column, " ", row_id)
      } else {
        paste0("row ", row_index)
      }
      sprintf("%s has %s='%s'", row_prefix, row_origin_column, row_origin_value)
    },
    character(1)
  )

  stop(
    sprintf(
      "%s contain unsupported %s values. Supported values: %s. Sample rows: %s",
      context,
      row_origin_column,
      paste(valid_values, collapse = ", "),
      paste(sample_label, collapse = "; ")
    ),
    call. = FALSE
  )
}

assert_blank_ids_only_for_manual_rows <- function(df,
                                                  id_column,
                                                  row_origin_column = "row_origin",
                                                  context = "Google Sheet rows",
                                                  blank_id_row_origins = "manual") {
  if (is.null(df) || !nrow(df) || !(id_column %in% names(df))) {
    return(invisible(df))
  }

  ids <- trim_text(df[[id_column]])
  allowed_blank_origins <- row_origin_in(df[[row_origin_column]], blank_id_row_origins)
  bad_rows <- which(!nzchar(ids) & !allowed_blank_origins)
  if (!length(bad_rows)) {
    return(invisible(df))
  }

  stop(
    sprintf(
      "%s allow blank %s values only when %s is one of: %s. Bad row numbers: %s",
      context,
      id_column,
      row_origin_column,
      paste(blank_id_row_origins, collapse = ", "),
      paste(utils::head(bad_rows, 5L), collapse = ", ")
    ),
    call. = FALSE
  )
}

assert_manual_review_required_fields <- function(df,
                                                 id_column,
                                                 required_fields,
                                                 context = "Google Sheet rows",
                                                 required_row_origins = "manual") {
  if (is.null(df) || !nrow(df)) {
    return(invisible(df))
  }

  required_origin_rows <- which(row_origin_in(df$row_origin, required_row_origins))
  if (!length(required_origin_rows)) {
    return(invisible(df))
  }

  missing_messages <- character()
  for (row_index in required_origin_rows) {
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
      "%s contain rows with row_origin in (%s) that are missing required fields: %s",
      context,
      paste(required_row_origins, collapse = ", "),
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

normalize_accreditation_review_text <- function(x) {
  value <- as.character(x %||% "")
  value[is.na(value)] <- ""
  value <- gsub("[\u2018\u2019\u201b\u2032\u00b4`]", "'", value, perl = TRUE)
  value <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2212]", "-", value, perl = TRUE)
  value <- tolower(trimws(value))
  gsub("\\s+", " ", value)
}

is_accreditation_teachout_process_action <- function(action_type,
                                                     action_label_raw,
                                                     action_label_short = NA_character_,
                                                     notes = NA_character_) {
  type <- normalize_accreditation_review_text(action_type)
  label <- normalize_accreditation_review_text(action_label_raw)
  label_short <- normalize_accreditation_review_text(action_label_short)
  notes_text <- normalize_accreditation_review_text(notes)
  content <- trimws(paste(label_short, label, notes_text))
  if (!nzchar(content) || !grepl("teach-?out|teach out", content, ignore.case = TRUE, perl = TRUE)) {
    return(FALSE)
  }

  if (type %in% c("warning", "notice", "probation", "show_cause", "removed", "monitoring")) {
    return(FALSE)
  }

  is_requirement_or_follow_up <- grepl(
    paste(
      "require(?:d)? .*teach-?out plan",
      "request(?:ed)? .*teach-?out plan",
      "teach-?out plan[^.]{0,120}?no longer required",
      "reject(?:ed)? .*teach-?out plan",
      sep = "|"
    ),
    content,
    ignore.case = TRUE,
    perl = TRUE
  )
  if (is_requirement_or_follow_up) {
    return(FALSE)
  }

  starts_as_process_approval <- grepl(
    paste(
      "^accepted teach-?out plans?\\b",
      "^accepted teach out plans?\\b",
      "^deferred teach-?out plans?\\b",
      "^approve(?:d)?\\b",
      "^to approve\\b",
      "^to acknowledge receipt of .*? to approve\\b",
      sep = "|"
    ),
    content,
    ignore.case = TRUE,
    perl = TRUE
  )

  mentions_teachout_process <- grepl(
    paste(
      "teach-?out (?:plan|plans|agreement|agreements|arrangement|arrangements|date)\\b",
      "teach-?out of\\b",
      "provisional plan (?:for|to) teach-?out\\b",
      "provisional plan to teach out\\b",
      "provisional teach-?out plan\\b",
      "teach out students\\b",
      "teach-?out receiving institution",
      "conduct(?:ed)?(?: and complete(?:d)?)? (?:its|their|the institution'?s) own teach-?out(?: plan)?",
      "teach-?out agreements? (?:are|were) not required",
      sep = "|"
    ),
    content,
    ignore.case = TRUE,
    perl = TRUE
  )

  starts_as_process_approval && mentions_teachout_process
}

compute_accreditation_teachout_process_mask <- function(df,
                                                        action_type_col = "action_type",
                                                        action_label_raw_col = "action_label_raw",
                                                        action_label_short_col = NULL,
                                                        notes_col = NULL) {
  if (is.null(df) || !nrow(df)) {
    return(logical(0))
  }

  required_columns <- c(action_type_col, action_label_raw_col)
  missing_columns <- setdiff(required_columns, names(df))
  if (length(missing_columns) > 0L) {
    stop(
      sprintf(
        "compute_accreditation_teachout_process_mask requires these columns: %s",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  action_label_short_values <- if (!is.null(action_label_short_col) && action_label_short_col %in% names(df)) {
    df[[action_label_short_col]]
  } else {
    rep(NA_character_, nrow(df))
  }
  notes_values <- if (!is.null(notes_col) && notes_col %in% names(df)) {
    df[[notes_col]]
  } else {
    rep(NA_character_, nrow(df))
  }

  vapply(
    seq_len(nrow(df)),
    function(i) {
      is_accreditation_teachout_process_action(
        action_type = df[[action_type_col]][[i]],
        action_label_raw = df[[action_label_raw_col]][[i]],
        action_label_short = action_label_short_values[[i]],
        notes = notes_values[[i]]
      )
    },
    logical(1)
  )
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

  drop_teachout_rows <- compute_accreditation_teachout_process_mask(
    actions_df,
    action_type_col = "action_type",
    action_label_raw_col = "action_label_raw",
    action_label_short_col = "action_label_short",
    notes_col = "notes"
  )
  actions_df <- actions_df[!drop_teachout_rows, , drop = FALSE]
  if (!nrow(actions_df)) {
    return(empty_accreditation_review_candidates())
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
  alias_map <- c(
    visible_statement = "generated_statement",
    editor_notes = "reviewer_notes",
    action_edited = "generated_statement",
    action_raw = "action_label_raw"
  )
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

filter_accreditation_overrides_for_tracker_scope <- function(overrides,
                                                             tracker_unitids = NULL,
                                                             context = "Accreditation editorial overrides") {
  local_rows <- coerce_accreditation_editorial_overrides(overrides)
  tracker_unitids <- trim_text(tracker_unitids)
  tracker_unitids <- unique(tracker_unitids[nzchar(tracker_unitids)])
  if (!nrow(local_rows) || !length(tracker_unitids)) {
    return(local_rows[, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])
  }

  row_origin <- normalize_review_row_origin(local_rows$source_row_origin)
  source_unitid <- trim_optional_text(local_rows$source_unitid)
  override_unitid <- trim_optional_text(local_rows$override_unitid)
  effective_unitid <- dplyr::coalesce(override_unitid, source_unitid)
  manual_mask <- !is.na(row_origin) & row_origin == "manual"
  invalid_manual <- manual_mask & (is.na(effective_unitid) | !(effective_unitid %in% tracker_unitids))
  if (any(invalid_manual)) {
    sample_rows <- local_rows[invalid_manual, , drop = FALSE]
    sample_labels <- paste(
      utils::head(trim_text(sample_rows$action_id), 5L),
      utils::head(trim_text(sample_rows$source_institution_name), 5L),
      sep = " / "
    )
    stop(
      sprintf(
        paste(
          "%s contains %d manual row(s) outside the tracker roster.",
          "Manual accreditation review rows must carry a tracker unitid.",
          "Sample rows: %s"
        ),
        context,
        sum(invalid_manual),
        paste(sample_labels, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  keep_rows <- !is.na(effective_unitid) & effective_unitid %in% tracker_unitids
  local_rows[keep_rows, ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]
}

filter_accreditation_overrides_for_review_sheet <- function(overrides,
                                                            candidate_action_ids = NULL) {
  local_rows <- coerce_accreditation_editorial_overrides(overrides)
  if (!nrow(local_rows)) {
    return(local_rows)
  }

  candidate_ids <- trim_text(candidate_action_ids)
  candidate_ids <- unique(candidate_ids[nzchar(candidate_ids)])
  row_origin <- normalize_review_row_origin(local_rows$source_row_origin)
  keep_rows <- (!is.na(row_origin) & row_origin == "manual")
  if (length(candidate_ids) > 0L) {
    keep_rows <- keep_rows | (trim_text(local_rows$action_id) %in% candidate_ids)
  }

  sheet_view <- build_accreditation_review_sheet_rows(local_rows)
  drop_teachout_rows <- compute_accreditation_teachout_process_mask(
    sheet_view,
    action_type_col = "action_type",
    action_label_raw_col = "action_label_raw",
    action_label_short_col = "generated_statement"
  )
  keep_rows <- keep_rows & !drop_teachout_rows

  local_rows[keep_rows, , drop = FALSE]
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

# Returns the action_id of an existing override row that describes the same
# real-world event as the candidate, or NA if no match.
# Matches on: same unitid + accreditor + date within 30 days + compatible action_type.
# All existing rows (approved, reject, unreviewed) suppress the new candidate.
# HLC institution pages expose a bare status badge ("On Probation",
# "On Notice") alongside the real board-action listings. Staging never
# creates override rows for these badge rows, so the apply-only review
# gate must apply the same exemption or every gated export fails on
# committed candidates that can never have overrides.
is_hlc_institution_status_page_row <- function(accreditor, source_url, action_label_raw) {
  mask <- (
    trim_text(accreditor) == "HLC" &
    stringr::str_detect(
      dplyr::coalesce(source_url, ""),
      "hlcommission\\.org/institution/"
    ) &
    stringr::str_detect(
      dplyr::coalesce(action_label_raw, ""),
      stringr::regex("^\\s*On\\s+(Probation|Warning|Notice|Show\\s+Cause)\\s*$", ignore_case = TRUE)
    )
  )
  mask[is.na(mask)] <- FALSE
  mask
}

find_cross_source_duplicate_id <- function(candidate_row, overrides) {
  if (!nrow(overrides)) return(NA_character_)

  c_unitid   <- trim_text(candidate_row$unitid[[1]])
  c_accred   <- trim_text(candidate_row$accreditor[[1]])
  c_type     <- trim_text(candidate_row$action_type[[1]])
  c_date     <- suppressWarnings(as.Date(trim_text(candidate_row$action_date[[1]])))
  c_is_teachout_process <- is_accreditation_teachout_process_action(
    action_type = candidate_row$action_type[[1]],
    action_label_raw = candidate_row$action_label_raw[[1]],
    action_label_short = candidate_row$generated_statement[[1]],
    notes = NA_character_
  )

  if (!nzchar(c_unitid) || !nzchar(c_accred) || is.na(c_date)) return(NA_character_)

  for (i in seq_len(nrow(overrides))) {
    o_unitid <- trim_text(overrides$source_unitid[[i]])
    o_accred <- trim_text(overrides$source_accreditor[[i]])
    o_type   <- trim_text(overrides$source_action_type[[i]])
    o_date   <- suppressWarnings(as.Date(trim_text(overrides$source_action_date[[i]])))
    o_is_teachout_process <- is_accreditation_teachout_process_action(
      action_type = overrides$source_action_type[[i]],
      action_label_raw = overrides$source_action_label_raw[[i]],
      action_label_short = overrides$source_generated_statement[[i]],
      notes = NA_character_
    )

    if (o_unitid != c_unitid || o_accred != c_accred) next
    if (is.na(o_date) || abs(as.integer(c_date - o_date)) > 30L) next
    # Treat teach-out paperwork as distinct from the institution's actual
    # resignation/withdrawal action even when both occur in the same month.
    if (xor(isTRUE(c_is_teachout_process), isTRUE(o_is_teachout_process))) next

    type_match <- identical(c_type, o_type) || (
      c_type == "warning" && o_type == "adverse_action" &&
      stringr::str_detect(
        tolower(dplyr::coalesce(overrides$source_action_label_raw[[i]], "")),
        "placed.{0,60}warning"
      )
    ) || (
      c_type == "notice" && o_type %in% c("notice", "monitoring")
    ) || (
      o_type == "notice" && c_type %in% c("notice", "monitoring")
    )
    if (!type_match) next

    return(trim_text(overrides$action_id[[i]]))
  }
  NA_character_
}

canonicalize_accreditation_review_gate_action_ids <- function(candidates, overrides) {
  review_candidates <- coerce_accreditation_review_candidates(candidates)
  override_rows <- coerce_accreditation_editorial_overrides(overrides)
  if (!nrow(review_candidates)) return(character())

  candidate_ids <- trim_text(review_candidates$action_id)

  status_page_candidate_mask <- is_hlc_institution_status_page_row(
    review_candidates$accreditor,
    review_candidates$source_url,
    review_candidates$action_label_raw
  )
  if (any(status_page_candidate_mask)) {
    message(sprintf(
      paste(
        "Apply-only accreditation review gate: exempting %d HLC institution-page",
        "status candidate(s) that staging deliberately leaves unstaged."
      ),
      sum(status_page_candidate_mask)
    ))
    review_candidates <- review_candidates[!status_page_candidate_mask, , drop = FALSE]
    candidate_ids <- candidate_ids[!status_page_candidate_mask]
  }

  if (!nrow(override_rows)) {
    return(unique(candidate_ids[nzchar(candidate_ids)]))
  }

  # Stage-time cross-source duplicate suppression intentionally keeps the
  # existing override row and discards the new candidate action_id. Apply-only
  # publish rebuilds must map those committed snapshot ids back to the
  # canonical override row or the review gate will falsely fail.
  override_ids <- trim_text(override_rows$action_id)
  canonical_ids <- vapply(
    seq_len(nrow(review_candidates)),
    function(i) {
      action_id <- candidate_ids[[i]]
      if (!nzchar(action_id)) return(NA_character_)
      if (action_id %in% override_ids) return(action_id)

      duplicate_id <- find_cross_source_duplicate_id(
        review_candidates[i, , drop = FALSE],
        override_rows
      )
      if (!is.na(duplicate_id) && nzchar(duplicate_id)) {
        return(duplicate_id)
      }

      action_id
    },
    character(1)
  )

  remapped_count <- sum(
    nzchar(candidate_ids) &
      nzchar(canonical_ids) &
      candidate_ids != canonical_ids
  )
  if (remapped_count > 0L) {
    message(sprintf(
      paste(
        "Apply-only accreditation review gate: canonicalized %d committed review",
        "candidate action_id value(s) to existing override rows."
      ),
      remapped_count
    ))
  }

  unique(canonical_ids[nzchar(canonical_ids)])
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

  if (nrow(new_rows)) {
    hlc_status_page_mask <- is_hlc_institution_status_page_row(
      new_rows$accreditor,
      new_rows$source_url,
      new_rows$action_label_raw
    )
    if (any(hlc_status_page_mask)) {
      suppressed <- new_rows[hlc_status_page_mask, , drop = FALSE]
      for (i in seq_len(nrow(suppressed))) {
        message(sprintf(
          "Suppressing HLC institution-page status row: %s (%s / %s / %s)",
          suppressed$action_id[[i]],
          dplyr::coalesce(suppressed$institution_name[[i]], ""),
          dplyr::coalesce(suppressed$action_label_raw[[i]], ""),
          dplyr::coalesce(suppressed$source_url[[i]], "")
        ))
      }
      new_rows <- new_rows[!hlc_status_page_mask, , drop = FALSE]
    }
  }

  if (!nrow(new_rows)) return(coerce_accreditation_editorial_overrides(overrides))

  if (nrow(overrides)) {
    cross_dup_ids <- vapply(
      seq_len(nrow(new_rows)),
      function(i) find_cross_source_duplicate_id(new_rows[i, , drop = FALSE], overrides),
      character(1)
    )
    is_cross_dup <- !is.na(cross_dup_ids)
    if (any(is_cross_dup)) {
      dup_rows <- new_rows[is_cross_dup, , drop = FALSE]
      dup_matched <- cross_dup_ids[is_cross_dup]
      dup_match_index <- match(dup_matched, trim_text(overrides$action_id))
      valid_dup_match <- !is.na(dup_match_index)
      if (any(valid_dup_match)) {
        for (field_name in names(ACCREDITATION_SOURCE_FIELD_MAP)) {
          source_column <- ACCREDITATION_SOURCE_FIELD_MAP[[field_name]]
          overrides[[source_column]][dup_match_index[valid_dup_match]] <- dup_rows[[field_name]][valid_dup_match]
        }
      }
      for (i in seq_len(nrow(dup_rows))) {
        message(sprintf(
          "Suppressing cross-source duplicate: %s (%s / %s / %s) matches existing %s",
          dup_rows$action_id[[i]],
          dplyr::coalesce(dup_rows$institution_name[[i]], ""),
          dplyr::coalesce(dup_rows$accreditor[[i]], ""),
          dplyr::coalesce(dup_rows$action_date[[i]], ""),
          dup_matched[[i]]
        ))
      }
      new_rows <- new_rows[!is_cross_dup, , drop = FALSE]
    }
  }

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
    if (nrow(non_manual) > 0L) {
      sample_ids <- paste(utils::head(non_manual$action_id, 5L), collapse = ", ")
      if (!isTRUE(allow_editor_added_rows)) {
        stop(sprintf(paste("Google Sheet contains %d action_id value(s) that are not present in editorial_overrides.csv.", "Only row_origin = manual rows may exist only in the sheet.", "Sample action_id values: %s"), nrow(non_manual), sample_ids), call. = FALSE)
      }
      message(sprintf("allow_editor_added_rows: importing %d sheet-only non-manual row(s) into accreditation editorial overrides. Sample action_id values: %s", nrow(non_manual), sample_ids))
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

  sheet_only_manual <- if (isTRUE(allow_editor_added_rows)) {
    sheet_only
  } else {
    sheet_only[normalize_review_row_origin(sheet_only$row_origin) == "manual", , drop = FALSE]
  }
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

build_review_backed_accreditation_export_row <- function(override_row, template_df) {
  internal_row <- coerce_accreditation_editorial_overrides(override_row)
  if (!nrow(internal_row)) return(template_df[0, , drop = FALSE])
  effective_row <- build_accreditation_review_sheet_rows(internal_row)[1, , drop = FALSE]
  export_row <- blank_like_row(template_df)
  if (!("row_origin" %in% names(export_row))) export_row$row_origin <- NA_character_
  row_origin_value <- trim_optional_text(internal_row$source_row_origin[[1]])
  if (is.na(row_origin_value) || !nzchar(row_origin_value)) {
    row_origin_value <- trim_optional_text(effective_row$row_origin[[1]])
  }
  if (is.na(row_origin_value) || !nzchar(row_origin_value)) row_origin_value <- "manual"
  export_id_prefix <- if (identical(row_origin_value, "manual")) "manual-accred-" else "review-accred-"
  export_unitid <- if (is.na(effective_row$unitid[[1]]) || !nzchar(effective_row$unitid[[1]])) paste0(export_id_prefix, effective_row$action_id[[1]]) else effective_row$unitid[[1]]

  if ("action_id" %in% names(export_row)) export_row$action_id[[1]] <- effective_row$action_id[[1]]
  if ("unitid" %in% names(export_row)) export_row$unitid[[1]] <- effective_row$unitid[[1]]
  if ("export_unitid" %in% names(export_row)) export_row$export_unitid[[1]] <- export_unitid
  if ("export_institution_name" %in% names(export_row)) export_row$export_institution_name[[1]] <- effective_row$institution_name[[1]]
  if ("institution_name" %in% names(export_row)) export_row$institution_name[[1]] <- effective_row$institution_name[[1]]
  if ("accreditor" %in% names(export_row)) export_row$accreditor[[1]] <- effective_row$accreditor[[1]]
  if ("action_date" %in% names(export_row)) export_row$action_date[[1]] <- effective_row$action_date[[1]]
  if ("action_year" %in% names(export_row)) export_row$action_year[[1]] <- derive_year_from_date_string(effective_row$action_date[[1]])
  if ("action_type" %in% names(export_row)) export_row$action_type[[1]] <- effective_row$action_type[[1]]
  if ("action_label_raw" %in% names(export_row)) export_row$action_label_raw[[1]] <- effective_row$action_label_raw[[1]]
  if ("action_label_short" %in% names(export_row)) export_row$action_label_short[[1]] <- effective_row$generated_statement[[1]]
  if ("source_url" %in% names(export_row)) export_row$source_url[[1]] <- effective_row$source_url[[1]]
  if ("source_title" %in% names(export_row)) export_row$source_title[[1]] <- effective_row$source_title[[1]]
  if ("source_page_url" %in% names(export_row)) export_row$source_page_url[[1]] <- effective_row$source_url[[1]]
  if ("display_action" %in% names(export_row)) export_row$display_action[[1]] <- TRUE
  export_row$row_origin[[1]] <- row_origin_value
  if ("has_financial_profile" %in% names(export_row)) export_row$has_financial_profile[[1]] <- !is.na(effective_row$unitid[[1]]) && nzchar(effective_row$unitid[[1]])
  if ("is_primary_tracker" %in% names(export_row)) export_row$is_primary_tracker[[1]] <- FALSE
  export_row
}

build_manual_accreditation_export_row <- function(override_row, template_df) {
  build_review_backed_accreditation_export_row(override_row, template_df)
}

apply_accreditation_editorial_overrides <- function(actions_df,
                                                    overrides = NULL,
                                                    enforce_review_gate = FALSE,
                                                    allowed_action_ids = NULL,
                                                    drop_unlisted = FALSE,
                                                    gate_mask = NULL) {
  override_rows <- coerce_accreditation_editorial_overrides(overrides)
  approved_review_mask <- trim_text(override_rows$review_status) == "approved"
  approved_review_mask[is.na(approved_review_mask)] <- FALSE
  published_override_rows <- override_rows[approved_review_mask, , drop = FALSE]
  if (is.null(actions_df)) {
    if (!nrow(published_override_rows)) return(actions_df)
    stop("Approved accreditation review rows need an actions data frame template during export.", call. = FALSE)
  }

  required_columns <- c("unitid", "export_unitid", "export_institution_name", "accreditor", "action_date", "action_type", "action_label_raw", "action_label_short", "source_url", "source_title")
  missing_columns <- setdiff(required_columns, names(actions_df))
  if (length(missing_columns) > 0L) stop(sprintf("apply_accreditation_editorial_overrides requires these columns: %s", paste(missing_columns, collapse = ", ")), call. = FALSE)

  if (!nrow(actions_df)) {
    if (!nrow(published_override_rows)) return(actions_df)
    return(dplyr::bind_rows(actions_df, dplyr::bind_rows(lapply(seq_len(nrow(published_override_rows)), function(i) build_review_backed_accreditation_export_row(published_override_rows[i, , drop = FALSE], actions_df)))))
  }

  review_actions <- actions_df
  review_actions$action_id <- vapply(seq_len(nrow(review_actions)), function(i) compute_accreditation_action_id(review_actions$unitid[[i]], review_actions$accreditor[[i]], review_actions$action_date[[i]], review_actions$action_label_raw[[i]], review_actions$export_unitid[[i]], review_actions$export_institution_name[[i]]), character(1))
  gate_rows <- if (is.null(gate_mask)) {
    rep(TRUE, nrow(review_actions))
  } else {
    as.logical(gate_mask)
  }
  if (length(gate_rows) != nrow(review_actions)) {
    stop("apply_accreditation_editorial_overrides gate_mask must have one value per action row.", call. = FALSE)
  }
  gate_rows[is.na(gate_rows)] <- FALSE

  allowed_ids <- trim_text(allowed_action_ids)
  allowed_ids <- unique(allowed_ids[nzchar(allowed_ids)])
  if (length(allowed_ids) > 0L && isTRUE(drop_unlisted)) {
    unexpected_rows <- gate_rows & !(review_actions$action_id %in% allowed_ids)
    if (any(unexpected_rows)) {
      unexpected_ids <- unique(trim_text(review_actions$action_id[unexpected_rows]))
      message(sprintf(paste("Apply-only accreditation review gate: ignoring %d recomputed action(s)", "that are not present in the committed review candidate snapshot.", "Sample action_id values: %s"), sum(unexpected_rows), paste(utils::head(unexpected_ids, 5L), collapse = ", ")))
      if (sum(unexpected_rows) > REVIEW_GATE_IGNORED_ROWS_WARN_THRESHOLD) {
        warning(sprintf(paste("Apply-only accreditation review gate ignored %d recomputed action(s),", "above the %d-row threshold: the committed snapshot and recomputed actions may have drifted."), sum(unexpected_rows), REVIEW_GATE_IGNORED_ROWS_WARN_THRESHOLD), call. = FALSE)
      }
      review_actions <- review_actions[!unexpected_rows, , drop = FALSE]
      gate_rows <- gate_rows[!unexpected_rows]
    }
  }

  if (!nrow(override_rows)) {
    if (isTRUE(enforce_review_gate) && any(gate_rows)) stop("Review gate is enabled but editorial_overrides.csv is empty or missing.", call. = FALSE)
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
      decided_ids <- unique(trim_text(override_rows$action_id)[is_terminal_review_decision(override_rows$review_status)])
      decided_ids <- decided_ids[nzchar(decided_ids)]
      missing_override <- gate_rows & !(trim_text(joined$action_id) %in% decided_ids)
      if (any(missing_override)) stop(sprintf(paste("Review gate is enabled but %d accreditation action(s) are missing an editorial decision (approved/reject).", "Sample action_id values: %s"), sum(missing_override), paste(utils::head(joined$action_id[missing_override], 5L), collapse = ", ")), call. = FALSE)
    }
  }

  joined_approved_mask <- trim_text(joined$review_status) == "approved"
  joined_approved_mask[is.na(joined_approved_mask)] <- FALSE
  approved_override_values <- function(values) {
    approved_values <- values
    approved_values[!joined_approved_mask] <- NA_character_
    approved_values
  }
  approved_reviewed_values <- function(source_values, override_values) {
    reviewed_values <- effective_override_values(source_values, override_values)
    reviewed_values[!joined_approved_mask] <- NA_character_
    reviewed_values
  }
  use_reviewed_snapshot_values <- length(allowed_ids) > 0L && isTRUE(drop_unlisted)
  published_field_values <- function(source_values, override_values) {
    if (isTRUE(use_reviewed_snapshot_values)) {
      return(approved_reviewed_values(source_values, override_values))
    }
    approved_override_values(override_values)
  }

  # Apply-only publish rebuilds reuse a committed tracker snapshot that can be
  # less detailed than the approved review-sheet row. In that mode, treat the
  # approved reviewed row (sheet-visible source fields plus any editor deltas)
  # as the authoritative public values for listed actions.
  joined$unitid <- effective_override_values(joined$unitid, published_field_values(joined$source_unitid, joined$override_unitid))
  if ("export_institution_name" %in% names(joined)) joined$export_institution_name <- effective_override_values(joined$export_institution_name, published_field_values(joined$source_institution_name, joined$override_institution_name))
  joined$accreditor <- effective_override_values(joined$accreditor, published_field_values(joined$source_accreditor, joined$override_accreditor))
  joined$action_date <- effective_override_values(joined$action_date, published_field_values(joined$source_action_date, joined$override_action_date))
  joined$action_type <- effective_override_values(joined$action_type, published_field_values(joined$source_action_type, joined$override_action_type))
  joined$action_label_raw <- effective_override_values(joined$action_label_raw, published_field_values(joined$source_action_label_raw, joined$override_action_label_raw))
  joined$action_label_short <- effective_override_values(dplyr::coalesce(joined$action_label_short, joined$action_label_raw), published_field_values(joined$source_generated_statement, joined$override_generated_statement))
  joined$source_url <- effective_override_values(joined$source_url, published_field_values(joined$source_source_url, joined$override_source_url))
  joined$source_title <- effective_override_values(joined$source_title, published_field_values(joined$source_source_title, joined$override_source_title))
  if ("source_page_url" %in% names(joined)) joined$source_page_url <- dplyr::coalesce(joined$source_url, joined$source_page_url)
  if ("action_year" %in% names(joined)) joined$action_year <- dplyr::coalesce(derive_year_from_date_string(joined$action_date), trim_optional_text(joined$action_year))
  if ("row_origin" %in% names(joined)) {
    joined$row_origin <- dplyr::coalesce(trim_optional_text(joined$row_origin), trim_optional_text(joined$source_row_origin), rep("scraper", nrow(joined)))
  } else {
    joined$row_origin <- dplyr::coalesce(trim_optional_text(joined$source_row_origin), rep("scraper", nrow(joined)))
  }
  if (isTRUE(enforce_review_gate)) joined <- joined[joined_approved_mask | !gate_rows, , drop = FALSE]

  missing_review_rows <- published_override_rows[!(trim_text(published_override_rows$action_id) %in% trim_text(review_actions$action_id)), , drop = FALSE]
  if (nrow(missing_review_rows)) joined <- dplyr::bind_rows(joined, dplyr::bind_rows(lapply(seq_len(nrow(missing_review_rows)), function(i) build_review_backed_accreditation_export_row(missing_review_rows[i, , drop = FALSE], review_actions))))
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
  "generated_cut_label",
  "generated_cut_summary",
  "source_url",
  "source_title",
  "source_publication",
  "row_origin"
)

COLLEGE_CUTS_REVIEW_SHEET_COLUMNS <- c(
  "cut_id", "unitid", "institution_name", "state", "announcement_date", "announcement_year",
  "cut_type", "display_categories", "edited_cut_text", "raw_cut_text",
  "source_url", "source_publication", "row_origin",
  "first_seen", "review_status", "reviewer", "reviewer_notes", "reviewed_at", "grandfathered"
)

COLLEGE_CUTS_CANDIDATE_SOURCE_MAP <- c(
  unitid = "source_unitid",
  institution_name = "source_institution_name",
  state = "source_state",
  announcement_date = "source_announcement_date",
  announcement_year = "source_announcement_year",
  cut_type = "source_cut_type",
  program_name = "source_cut_description",
  generated_cut_label = "source_generated_cut_label",
  generated_cut_summary = "source_generated_cut_summary",
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
  edited_cut_text = "source_cut_description",
  raw_cut_text = "source_generated_cut_summary",
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
  source_url = "override_source_url",
  source_publication = "override_source_publication"
)

COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS <- c(
  "cut_id",
  "source_unitid", "source_institution_name", "source_state",
  "source_announcement_date", "source_announcement_year",
  "source_cut_type", "source_cut_description",
  "source_generated_cut_label", "source_generated_cut_summary",
  "source_source_url", "source_source_title", "source_source_publication", "source_row_origin",
  "override_unitid", "override_institution_name", "override_state",
  "override_announcement_date", "override_announcement_year",
  "override_cut_type", "override_cut_description",
  "override_cut_label", "override_cut_summary",
  "override_source_url", "override_source_title", "override_source_publication",
  "first_seen", "review_status", "reviewer", "reviewer_notes", "reviewed_at", "grandfathered"
)

COLLEGE_CUTS_REQUIRED_MANUAL_FIELDS <- c("institution_name", "state", "announcement_date", "cut_type", "edited_cut_text", "source_url", "source_publication")

COLLEGE_CUTS_REVIEW_CANDIDATE_COL_TYPES <- readr::cols(.default = readr::col_character())
COLLEGE_CUTS_EDITORIAL_OVERRIDE_COL_TYPES <- readr::cols(.default = readr::col_character(), grandfathered = readr::col_logical())

normalize_college_cuts_sheet_headers <- function(df) {
  if (is.null(df) || !ncol(df)) return(df)
  normalized <- df
  alias_map <- c(
    program_name = "edited_cut_text",
    editor_program_name = "editor_edited_cut_text",
    editor_notes = "reviewer_notes",
    cut_description = "edited_cut_text",
    cut_summary = "raw_cut_text",
    editor_cut_description = "editor_edited_cut_text",
    editor_cut_label = "editor_edited_cut_text",
    editor_cut_summary = "editor_raw_cut_text"
  )
  for (old_name in names(alias_map)) {
    new_name <- alias_map[[old_name]]
    if (!(old_name %in% names(normalized))) next
    if (new_name %in% names(normalized)) next
    names(normalized)[names(normalized) == old_name] <- new_name
  }
  if (!("edited_cut_text" %in% names(normalized))) {
    row_count <- nrow(normalized)
    pick_column <- function(column_name) {
      if (column_name %in% names(normalized)) trim_optional_text(normalized[[column_name]]) else rep(NA_character_, row_count)
    }
    normalized$edited_cut_text <- dplyr::coalesce(
      pick_column("edited_cut_text"),
      pick_column("cut_description"),
      pick_column("cut_label")
    )
  }
  if (!("raw_cut_text" %in% names(normalized))) {
    row_count <- nrow(normalized)
    pick_column <- function(column_name) {
      if (column_name %in% names(normalized)) trim_optional_text(normalized[[column_name]]) else rep(NA_character_, row_count)
    }
    normalized$raw_cut_text <- dplyr::coalesce(
      pick_column("raw_cut_text"),
      pick_column("cut_summary")
    )
  }
  normalized
}

resolve_college_cuts_sheet_edited_cut_text <- function(df) {
  if (is.null(df) || !nrow(df)) return(character(0))
  row_count <- nrow(df)
  pick_column <- function(column_name) {
    if (column_name %in% names(df)) trim_optional_text(df[[column_name]]) else rep(NA_character_, row_count)
  }
  dplyr::coalesce(
    pick_column("edited_cut_text"),
    pick_column("cut_description"),
    pick_column("cut_label")
  )
}

resolve_college_cuts_sheet_raw_cut_text <- function(df) {
  if (is.null(df) || !nrow(df)) return(character(0))
  row_count <- nrow(df)
  pick_column <- function(column_name) {
    if (column_name %in% names(df)) trim_optional_text(df[[column_name]]) else rep(NA_character_, row_count)
  }
  dplyr::coalesce(
    pick_column("raw_cut_text"),
    pick_column("cut_summary")
  )
}

resolve_college_cuts_sheet_cut_description <- function(df,
                                                       edited_cut_text = resolve_college_cuts_sheet_edited_cut_text(df)) {
  if (is.null(df) || !nrow(df)) return(character(0))
  description_values <- if ("cut_description" %in% names(df)) trim_optional_text(df$cut_description) else rep(NA_character_, nrow(df))
  dplyr::coalesce(description_values, edited_cut_text)
}

repair_shifted_college_cuts_review_sheet_rows <- function(df) {
  required_columns <- c(
    "display_categories", "edited_cut_text", "cut_label", "raw_cut_text",
    "source_url", "source_publication", "row_origin", "first_seen",
    "review_status", "reviewer", "reviewer_notes", "reviewed_at", "grandfathered"
  )
  if (is.null(df) || !nrow(df) || !all(required_columns %in% names(df))) {
    return(df)
  }

  row_origin_values <- if ("row_origin" %in% names(df)) normalize_review_row_origin(df$row_origin) else rep(NA_character_, nrow(df))
  source_publication_values <- if ("source_publication" %in% names(df)) normalize_review_row_origin(df$source_publication) else rep(NA_character_, nrow(df))
  misaligned_rows <- !is.na(row_origin_values) &
    !(row_origin_values %in% COLLEGE_CUTS_REVIEW_ROW_ORIGINS) &
    looks_like_iso_date(df$row_origin) &
    !is.na(source_publication_values) &
    source_publication_values %in% COLLEGE_CUTS_REVIEW_ROW_ORIGINS &
    looks_like_http_url(df$raw_cut_text) &
    !looks_like_http_url(df$source_url)

  if (!any(misaligned_rows)) {
    return(df)
  }

  repaired <- df
  repaired$display_categories[misaligned_rows] <- NA_character_
  repaired$edited_cut_text[misaligned_rows] <- trim_optional_text(df$display_categories[misaligned_rows])
  repaired$raw_cut_text[misaligned_rows] <- trim_optional_text(df$cut_label[misaligned_rows])
  repaired$source_url[misaligned_rows] <- trim_optional_text(df$raw_cut_text[misaligned_rows])
  repaired$source_publication[misaligned_rows] <- trim_optional_text(df$source_url[misaligned_rows])
  repaired$row_origin[misaligned_rows] <- trim_optional_text(df$source_publication[misaligned_rows])
  repaired$first_seen[misaligned_rows] <- trim_optional_text(df$row_origin[misaligned_rows])
  repaired$review_status[misaligned_rows] <- trim_optional_text(df$first_seen[misaligned_rows])
  repaired$reviewer[misaligned_rows] <- trim_optional_text(df$review_status[misaligned_rows])
  repaired$reviewer_notes[misaligned_rows] <- trim_optional_text(df$reviewer[misaligned_rows])
  repaired$reviewed_at[misaligned_rows] <- trim_optional_text(df$reviewer_notes[misaligned_rows])
  if ("grandfathered" %in% names(repaired)) {
    repaired$grandfathered[misaligned_rows] <- coerce_false_default_logical(df$reviewed_at[misaligned_rows])
  }

  repaired
}

format_college_cuts_sheet_headers <- function(df) df
format_accreditation_review_sheet_headers <- function(df) {
  if (is.null(df) || !ncol(df)) return(df)
  rename_map <- c(generated_statement = "action_edited", action_label_raw = "action_raw")
  for (old_name in names(rename_map)) {
    new_name <- rename_map[[old_name]]
    if (!(old_name %in% names(df))) next
    names(df)[names(df) == old_name] <- new_name
  }
  df
}

derive_college_cuts_review_display_categories <- function(cut_type,
                                                          cut_description,
                                                          cut_label = NA_character_,
                                                          cut_summary = NA_character_) {
  if (!nzchar(trim_text(cut_type)) &&
      !nzchar(trim_text(cut_description)) &&
      !nzchar(trim_text(cut_label)) &&
      !nzchar(trim_text(cut_summary))) {
    return(NA_character_)
  }

  categories <- derive_cut_display_categories(
    cut_type = cut_type,
    program_name = cut_description,
    cut_label_public = cut_label,
    cut_summary_public = cut_summary
  )
  categories <- trim_text(as.character(categories %||% character()))
  categories <- unique(categories[nzchar(categories)])
  if (!length(categories)) return(NA_character_)
  paste(categories, collapse = "; ")
}

empty_college_cuts_review_candidates <- function() {
  data.frame(
    cut_id = character(), unitid = character(), institution_name = character(),
    state = character(), announcement_date = character(), announcement_year = character(),
    cut_type = character(), program_name = character(),
    generated_cut_label = character(), generated_cut_summary = character(),
    source_url = character(), source_title = character(),
    source_publication = character(), row_origin = character(),
    stringsAsFactors = FALSE
  )
}

empty_college_cuts_review_sheet_rows <- function() {
  data.frame(
    cut_id = character(), unitid = character(), institution_name = character(),
    state = character(), announcement_date = character(), announcement_year = character(),
    cut_type = character(), display_categories = character(), edited_cut_text = character(), raw_cut_text = character(),
    source_url = character(), source_publication = character(),
    row_origin = character(), first_seen = character(), review_status = character(),
    reviewer = character(), reviewer_notes = character(), reviewed_at = character(),
    grandfathered = logical(), stringsAsFactors = FALSE
  )
}

empty_college_cuts_editorial_overrides <- function() {
  data.frame(
    cut_id = character(),
    source_unitid = character(), source_institution_name = character(), source_state = character(),
    source_announcement_date = character(), source_announcement_year = character(),
    source_cut_type = character(), source_cut_description = character(),
    source_generated_cut_label = character(), source_generated_cut_summary = character(),
    source_source_url = character(), source_source_title = character(),
    source_source_publication = character(), source_row_origin = character(),
    override_unitid = character(), override_institution_name = character(), override_state = character(),
    override_announcement_date = character(), override_announcement_year = character(),
    override_cut_type = character(), override_cut_description = character(),
    override_cut_label = character(), override_cut_summary = character(),
    override_source_url = character(), override_source_title = character(),
    override_source_publication = character(),
    first_seen = character(), review_status = character(), reviewer = character(),
    reviewer_notes = character(), reviewed_at = character(), grandfathered = logical(),
    stringsAsFactors = FALSE
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
  # Allow older CSVs that lack the generated label/summary columns.
  required_core <- setdiff(COLLEGE_CUTS_REVIEW_CANDIDATE_COLUMNS,
                           c("generated_cut_label", "generated_cut_summary"))
  missing_core <- setdiff(required_core, names(df))
  if (length(missing_core) > 0L) {
    stop(sprintf("College cuts review candidates are missing required columns: %s",
                 paste(missing_core, collapse = ", ")), call. = FALSE)
  }

  candidates <- data.frame(
    cut_id = trim_text(df$cut_id),
    unitid = trim_optional_text(df$unitid),
    institution_name = trim_optional_text(df$institution_name),
    state = trim_optional_text(df$state),
    announcement_date = trim_optional_text(df$announcement_date),
    announcement_year = trim_optional_text(df$announcement_year),
    cut_type = trim_optional_text(df$cut_type),
    program_name = trim_optional_text(df$program_name),
    generated_cut_label = if ("generated_cut_label" %in% names(df)) trim_optional_text(df$generated_cut_label) else rep(NA_character_, nrow(df)),
    generated_cut_summary = if ("generated_cut_summary" %in% names(df)) trim_optional_text(df$generated_cut_summary) else rep(NA_character_, nrow(df)),
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

build_college_cuts_review_candidates <- function(cuts_df,
                                                 tracker_unitids = NULL) {
  if (is.null(cuts_df) || !nrow(cuts_df)) return(empty_college_cuts_review_candidates())
  required_columns <- c("cut_id", "matched_unitid", "export_unitid", "institution_name_display", "state_display", "announcement_date", "announcement_year", "cut_type", "program_name", "source_url", "source_title", "source_publication")
  missing_columns <- setdiff(required_columns, names(cuts_df))
  if (length(missing_columns) > 0L) stop(sprintf("build_college_cuts_review_candidates requires these columns: %s", paste(missing_columns, collapse = ", ")), call. = FALSE)

  tracker_unitids <- trim_text(tracker_unitids)
  tracker_unitids <- unique(tracker_unitids[nzchar(tracker_unitids)])
  filtered_cuts <- cuts_df
  if ("is_primary_tracker" %in% names(filtered_cuts)) {
    filtered_cuts <- filtered_cuts[filtered_cuts$is_primary_tracker %in% TRUE, , drop = FALSE]
  } else if (!length(tracker_unitids)) {
    stop(
      paste(
        "build_college_cuts_review_candidates requires tracker_unitids",
        "or an is_primary_tracker column to enforce tracker-only review scope."
      ),
      call. = FALSE
    )
  }

  if (length(tracker_unitids)) {
    filtered_unitids <- trim_text(filtered_cuts$matched_unitid)
    filtered_cuts <- filtered_cuts[filtered_unitids %in% tracker_unitids, , drop = FALSE]
  }

  if (!nrow(filtered_cuts)) return(empty_college_cuts_review_candidates())

  candidates <- data.frame(
    cut_id = vapply(seq_len(nrow(filtered_cuts)), function(i) compute_college_cuts_review_id(filtered_cuts$cut_id[[i]], dplyr::coalesce(filtered_cuts$matched_unitid[[i]], filtered_cuts$export_unitid[[i]]), filtered_cuts$announcement_date[[i]], filtered_cuts$program_name[[i]], filtered_cuts$institution_name_display[[i]], filtered_cuts$state_display[[i]]), character(1)),
    unitid = trim_optional_text(dplyr::coalesce(filtered_cuts$matched_unitid, filtered_cuts$export_unitid)),
    institution_name = trim_optional_text(filtered_cuts$institution_name_display),
    state = trim_optional_text(filtered_cuts$state_display),
    announcement_date = trim_optional_text(filtered_cuts$announcement_date),
    announcement_year = trim_optional_text(filtered_cuts$announcement_year),
    cut_type = trim_optional_text(filtered_cuts$cut_type),
    program_name = trim_optional_text(filtered_cuts$program_name),
    generated_cut_label = if ("generated_cut_label" %in% names(filtered_cuts)) trim_optional_text(filtered_cuts$generated_cut_label) else rep(NA_character_, nrow(filtered_cuts)),
    generated_cut_summary = if ("generated_cut_summary" %in% names(filtered_cuts)) trim_optional_text(filtered_cuts$generated_cut_summary) else rep(NA_character_, nrow(filtered_cuts)),
    source_url = trim_optional_text(filtered_cuts$source_url),
    source_title = trim_optional_text(filtered_cuts$source_title),
    source_publication = trim_optional_text(filtered_cuts$source_publication),
    row_origin = rep("scraper", nrow(filtered_cuts)),
    stringsAsFactors = FALSE
  )
  candidates <- candidates[!duplicated(candidates$cut_id), COLLEGE_CUTS_REVIEW_CANDIDATE_COLUMNS, drop = FALSE]
  assert_unique_cut_ids(candidates, "College cuts review candidates")
  candidates
}

derive_default_college_cuts_edited_text <- function(source_cut_description,
                                                    source_generated_cut_label) {
  description_values <- trim_optional_text(source_cut_description)
  generated_label_values <- trim_optional_text(source_generated_cut_label)

  row_count <- max(length(description_values), length(generated_label_values))
  if (row_count == 0L) return(character(0))
  if (!length(description_values)) description_values <- rep(NA_character_, row_count)
  if (!length(generated_label_values)) generated_label_values <- rep(NA_character_, row_count)
  if (length(description_values) != row_count) description_values <- rep_len(description_values, row_count)
  if (length(generated_label_values) != row_count) generated_label_values <- rep_len(generated_label_values, row_count)

  resolved <- description_values
  use_generated_label <- !is.na(generated_label_values) &
    nzchar(generated_label_values) &
    (is.na(description_values) | !nzchar(trimws(description_values)) |
       description_values %in% .GENERIC_CUT_LABELS)
  if (any(use_generated_label)) {
    resolved[use_generated_label] <- generated_label_values[use_generated_label]
  }

  trim_optional_text(resolved)
}

derive_college_cuts_sheet_default_edited_text <- function(source_cut_description,
                                                          source_generated_cut_label) {
  description_values <- trim_optional_text(source_cut_description)
  generated_label_values <- trim_optional_text(source_generated_cut_label)

  row_count <- max(length(description_values), length(generated_label_values))
  if (row_count == 0L) return(character(0))
  if (!length(description_values)) description_values <- rep(NA_character_, row_count)
  if (!length(generated_label_values)) generated_label_values <- rep(NA_character_, row_count)
  if (length(description_values) != row_count) description_values <- rep_len(description_values, row_count)
  if (length(generated_label_values) != row_count) generated_label_values <- rep_len(generated_label_values, row_count)

  resolved <- description_values
  use_generated_label <- !is.na(generated_label_values) &
    nzchar(generated_label_values) &
    (is.na(description_values) | !nzchar(trimws(description_values)) |
       description_values %in% .GENERIC_CUT_LABELS)
  if (any(use_generated_label)) {
    resolved[use_generated_label] <- generated_label_values[use_generated_label]
  }

  trim_optional_text(resolved)
}

coerce_college_cuts_review_sheet_rows <- function(df,
                                                  default_first_seen = as.character(Sys.Date())) {
  if (is.null(df) || !nrow(df)) return(empty_college_cuts_review_sheet_rows())
  raw_rows <- repair_shifted_college_cuts_review_sheet_rows(normalize_college_cuts_sheet_headers(df))
  raw_rows$edited_cut_text <- resolve_college_cuts_sheet_edited_cut_text(raw_rows)
  raw_rows$raw_cut_text <- resolve_college_cuts_sheet_raw_cut_text(raw_rows)
  assert_valid_review_row_origins(
    raw_rows,
    id_column = "cut_id",
    context = "College cuts review sheet rows",
    valid_values = COLLEGE_CUTS_REVIEW_ROW_ORIGINS
  )
  assert_blank_ids_only_for_manual_rows(
    raw_rows,
    id_column = "cut_id",
    context = "College cuts review sheet rows",
    blank_id_row_origins = COLLEGE_CUTS_HUMAN_ROW_ORIGINS
  )

  sheet_rows <- rep_like_template_rows(empty_college_cuts_review_sheet_rows(), nrow(raw_rows))
  for (column_name in setdiff(COLLEGE_CUTS_REVIEW_SHEET_COLUMNS, "grandfathered")) {
    sheet_rows[[column_name]] <- if (column_name %in% names(raw_rows)) trim_optional_text(raw_rows[[column_name]]) else NA_character_
  }
  sheet_rows$grandfathered <- if ("grandfathered" %in% names(raw_rows)) coerce_false_default_logical(raw_rows$grandfathered) else FALSE
  sheet_rows$edited_cut_text <- dplyr::coalesce(resolve_college_cuts_sheet_edited_cut_text(raw_rows), sheet_rows$edited_cut_text)
  sheet_rows$raw_cut_text <- dplyr::coalesce(resolve_college_cuts_sheet_raw_cut_text(raw_rows), sheet_rows$raw_cut_text)
  if ("editor_edited_cut_text" %in% names(raw_rows)) sheet_rows$edited_cut_text <- dplyr::coalesce(trim_optional_text(raw_rows$editor_edited_cut_text), sheet_rows$edited_cut_text)
  if ("editor_raw_cut_text" %in% names(raw_rows)) sheet_rows$raw_cut_text <- dplyr::coalesce(trim_optional_text(raw_rows$editor_raw_cut_text), sheet_rows$raw_cut_text)
  if ("editor_announcement_date" %in% names(raw_rows)) sheet_rows$announcement_date <- dplyr::coalesce(trim_optional_text(raw_rows$editor_announcement_date), sheet_rows$announcement_date)
  if ("editor_cut_type" %in% names(raw_rows)) sheet_rows$cut_type <- dplyr::coalesce(trim_optional_text(raw_rows$editor_cut_type), sheet_rows$cut_type)
  if ("editor_source_url" %in% names(raw_rows)) sheet_rows$source_url <- dplyr::coalesce(trim_optional_text(raw_rows$editor_source_url), sheet_rows$source_url)
  if ("editor_source_publication" %in% names(raw_rows)) sheet_rows$source_publication <- dplyr::coalesce(trim_optional_text(raw_rows$editor_source_publication), sheet_rows$source_publication)
  cut_description_values <- resolve_college_cuts_sheet_cut_description(raw_rows, edited_cut_text = sheet_rows$edited_cut_text)
  cut_summary_values <- sheet_rows$raw_cut_text
  sheet_rows$announcement_year <- dplyr::coalesce(sheet_rows$announcement_year, derive_year_from_date_string(sheet_rows$announcement_date))
  sheet_rows$display_categories <- vapply(
    seq_len(nrow(sheet_rows)),
    function(i) derive_college_cuts_review_display_categories(
      cut_type = sheet_rows$cut_type[[i]],
      cut_description = cut_description_values[[i]],
      cut_label = sheet_rows$edited_cut_text[[i]],
      cut_summary = cut_summary_values[[i]]
    ),
    character(1)
  )
  sheet_rows$row_origin <- normalize_review_row_origin(sheet_rows$row_origin)
  sheet_rows$row_origin[is.na(sheet_rows$row_origin)] <- "scraper"

  missing_human_ids <- which(!nzchar(trim_text(sheet_rows$cut_id)) & is_college_cuts_human_row_origin(sheet_rows$row_origin))
  if (length(missing_human_ids)) {
    sheet_rows$cut_id[missing_human_ids] <- vapply(missing_human_ids, function(i) compute_college_cuts_review_id(sheet_rows$cut_id[[i]], sheet_rows$unitid[[i]], sheet_rows$announcement_date[[i]], cut_description_values[[i]], sheet_rows$institution_name[[i]], sheet_rows$state[[i]]), character(1))
  }
  sheet_rows$first_seen[is.na(sheet_rows$first_seen) & is_college_cuts_human_row_origin(sheet_rows$row_origin)] <- default_first_seen
  assert_manual_review_required_fields(
    sheet_rows,
    "cut_id",
    COLLEGE_CUTS_REQUIRED_MANUAL_FIELDS,
    "College cuts review sheet rows",
    required_row_origins = COLLEGE_CUTS_HUMAN_ROW_ORIGINS
  )
  assert_unique_cut_ids(sheet_rows, "College cuts review sheet rows")
  sheet_rows[, COLLEGE_CUTS_REVIEW_SHEET_COLUMNS, drop = FALSE]
}

coerce_college_cuts_editorial_overrides <- function(df) {
  if (is.null(df) || !nrow(df)) return(empty_college_cuts_editorial_overrides())
  normalized <- normalize_college_cuts_sheet_headers(df)
  overrides <- rep_like_template_rows(empty_college_cuts_editorial_overrides(), nrow(normalized))
  overrides$cut_id <- if ("cut_id" %in% names(normalized)) trim_text(normalized$cut_id) else ""

  direct_source_map <- c(unitid = "source_unitid", institution_name = "source_institution_name", state = "source_state", announcement_date = "source_announcement_date", announcement_year = "source_announcement_year", cut_type = "source_cut_type", cut_description = "source_cut_description", generated_cut_label = "source_generated_cut_label", generated_cut_summary = "source_generated_cut_summary", source_url = "source_source_url", source_publication = "source_source_publication", row_origin = "source_row_origin")
  for (field_name in names(direct_source_map)) {
    source_column <- direct_source_map[[field_name]]
    overrides[[source_column]] <- if (source_column %in% names(normalized)) trim_optional_text(normalized[[source_column]]) else if (field_name %in% names(normalized)) trim_optional_text(normalized[[field_name]]) else NA_character_
  }
  overrides$source_source_title <- if ("source_source_title" %in% names(normalized)) trim_optional_text(normalized$source_source_title) else if ("source_title" %in% names(normalized)) trim_optional_text(normalized$source_title) else NA_character_

  new_override_map <- c(unitid = "override_unitid", institution_name = "override_institution_name", state = "override_state", announcement_date = "override_announcement_date", announcement_year = "override_announcement_year", cut_type = "override_cut_type", cut_description = "override_cut_description", cut_label = "override_cut_label", cut_summary = "override_cut_summary", source_url = "override_source_url", source_title = "override_source_title", source_publication = "override_source_publication")
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

  missing_human_ids <- which(!nzchar(trim_text(overrides$cut_id)) & is_college_cuts_human_row_origin(overrides$source_row_origin))
  if (length(missing_human_ids)) {
    overrides$cut_id[missing_human_ids] <- vapply(
      missing_human_ids,
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
    missing_first_seen <- is.na(overrides$first_seen[missing_human_ids])
    if (any(missing_first_seen)) {
      overrides$first_seen[missing_human_ids[missing_first_seen]] <- as.character(Sys.Date())
    }
  }
  assert_unique_cut_ids(overrides, "College cuts editorial overrides")
  overrides[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]
}

filter_college_cuts_overrides_for_tracker_scope <- function(overrides,
                                                            tracker_unitids = NULL,
                                                            context = "College cuts editorial overrides") {
  local_rows <- coerce_college_cuts_editorial_overrides(overrides)
  tracker_unitids <- trim_text(tracker_unitids)
  tracker_unitids <- unique(tracker_unitids[nzchar(tracker_unitids)])
  if (!nrow(local_rows) || !length(tracker_unitids)) {
    return(local_rows[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])
  }

  row_origin <- normalize_review_row_origin(local_rows$source_row_origin)
  source_unitid <- trim_optional_text(local_rows$source_unitid)
  override_unitid <- trim_optional_text(local_rows$override_unitid)
  effective_unitid <- dplyr::coalesce(override_unitid, source_unitid)
  human_mask <- is_college_cuts_human_row_origin(row_origin)
  invalid_human_rows <- human_mask & (is.na(effective_unitid) | !(effective_unitid %in% tracker_unitids))
  if (any(invalid_human_rows)) {
    sample_rows <- local_rows[invalid_human_rows, , drop = FALSE]
    sample_labels <- paste(
      utils::head(trim_text(sample_rows$cut_id), 5L),
      utils::head(trim_text(sample_rows$source_institution_name), 5L),
      sep = " / "
    )
    stop(
      sprintf(
        paste(
          "%s contains %d human-authored row(s) outside the tracker roster.",
          "College cuts review rows with row_origin in (%s) must carry a tracker unitid.",
          "Sample rows: %s"
        ),
        context,
        sum(invalid_human_rows),
        paste(COLLEGE_CUTS_HUMAN_ROW_ORIGINS, collapse = ", "),
        paste(sample_labels, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  keep_rows <- !is.na(effective_unitid) & effective_unitid %in% tracker_unitids
  local_rows[keep_rows, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]
}

build_college_cuts_review_sheet_rows <- function(overrides,
                                                 tracker_unitids = NULL) {
  local_rows <- filter_college_cuts_overrides_for_tracker_scope(overrides, tracker_unitids = tracker_unitids, context = "College cuts review sheet rows")
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
  default_edited_cut_text <- derive_college_cuts_sheet_default_edited_text(
    local_rows$source_cut_description,
    local_rows$source_generated_cut_label
  )
  effective_cut_description <- dplyr::coalesce(
    trim_optional_text(local_rows$override_cut_description),
    trim_optional_text(local_rows$override_cut_label),
    default_edited_cut_text
  )
  source_raw_cut_text <- local_rows$source_generated_cut_summary
  sheet_rows$edited_cut_text <- effective_cut_description
  sheet_rows$raw_cut_text <- dplyr::coalesce(sheet_rows$raw_cut_text, source_raw_cut_text)
  sheet_rows$announcement_year <- dplyr::coalesce(sheet_rows$announcement_year, derive_year_from_date_string(sheet_rows$announcement_date))
  sheet_rows$display_categories <- vapply(
    seq_len(nrow(sheet_rows)),
    function(i) derive_college_cuts_review_display_categories(
      cut_type = sheet_rows$cut_type[[i]],
      cut_description = effective_cut_description[[i]],
      cut_label = sheet_rows$edited_cut_text[[i]],
      cut_summary = source_raw_cut_text[[i]]
    ),
    character(1)
  )
  sheet_rows[, COLLEGE_CUTS_REVIEW_SHEET_COLUMNS, drop = FALSE]
}

filter_college_cuts_overrides_for_review_sheet <- function(overrides,
                                                           candidate_cut_ids = NULL,
                                                           tracker_unitids = NULL) {
  local_rows <- filter_college_cuts_overrides_for_tracker_scope(overrides, tracker_unitids = tracker_unitids, context = "College cuts review sheet rows")
  if (!nrow(local_rows)) {
    return(local_rows)
  }

  candidate_ids <- trim_text(candidate_cut_ids)
  candidate_ids <- unique(candidate_ids[nzchar(candidate_ids)])
  row_origin <- normalize_review_row_origin(local_rows$source_row_origin)
  keep_rows <- is_college_cuts_human_row_origin(row_origin)
  if (length(candidate_ids) > 0L) {
    keep_rows <- keep_rows | (trim_text(local_rows$cut_id) %in% candidate_ids)
  }

  local_rows[keep_rows, , drop = FALSE]
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
                                                   first_seen = as.character(Sys.Date()),
                                                   tracker_unitids = NULL) {
  review_candidates <- coerce_college_cuts_review_candidates(candidates)
  tracker_unitids <- c(trim_text(tracker_unitids), trim_text(review_candidates$unitid))
  tracker_unitids <- unique(tracker_unitids[nzchar(tracker_unitids)])
  overrides <- filter_college_cuts_overrides_for_tracker_scope(existing, tracker_unitids = tracker_unitids)
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
  filter_college_cuts_overrides_for_tracker_scope(
    dplyr::bind_rows(overrides, new_internal_rows),
    tracker_unitids = tracker_unitids,
    context = "Staged college cuts editorial overrides"
  )
}

build_college_cuts_review_sheet_append_rows <- function(overrides,
                                                        existing_sheet = NULL,
                                                        tracker_unitids = NULL) {
  local_sheet_rows <- build_college_cuts_review_sheet_rows(overrides, tracker_unitids = tracker_unitids)
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
    non_human <- sheet_only[!is_college_cuts_human_row_origin(sheet_only$row_origin), , drop = FALSE]
    if (nrow(non_human) > 0L) {
      sample_ids <- paste(utils::head(non_human$cut_id, 5L), collapse = ", ")
      if (!isTRUE(allow_editor_added_rows)) {
        stop(
          sprintf(
            paste(
              "Google Sheet contains %d cut_id value(s) that are not present in editorial_overrides.csv.",
              "Only row_origin values in (%s) may exist only in the sheet.",
              "Sample cut_id values: %s"
            ),
            nrow(non_human),
            paste(COLLEGE_CUTS_HUMAN_ROW_ORIGINS, collapse = ", "),
            sample_ids
          ),
          call. = FALSE
        )
      }
      message(sprintf("allow_editor_added_rows: importing %d sheet-only non-human row(s) into college cuts editorial overrides. Sample cut_id values: %s", nrow(non_human), sample_ids))
    }
  }

  if (nrow(local_rows)) {
    match_index <- match(local_ids, sheet_ids)
    matched <- !is.na(match_index)
    if (any(matched)) {
      matched_sheet_rows <- sheet_data[match_index[matched], , drop = FALSE]
      matched_rows <- which(matched)
      matched_human <- is_college_cuts_human_row_origin(local_rows$source_row_origin[matched])
      matched_human_rows <- matched_rows[matched_human]
      default_edited_cut_text <- derive_college_cuts_sheet_default_edited_text(
        local_rows$source_cut_description,
        local_rows$source_generated_cut_label
      )
      for (field_name in names(COLLEGE_CUTS_SHEET_OVERRIDE_MAP)) {
        source_column <- COLLEGE_CUTS_SHEET_SOURCE_MAP[[field_name]]
        override_column <- COLLEGE_CUTS_SHEET_OVERRIDE_MAP[[field_name]]
        local_rows[[override_column]][matched] <- compute_override_delta(matched_sheet_rows[[field_name]], local_rows[[source_column]][matched])
        if (length(matched_human_rows)) {
          local_rows[[source_column]][matched_human_rows] <- matched_sheet_rows[[field_name]][matched_human]
          local_rows[[override_column]][matched_human_rows] <- NA_character_
        }
      }
      edited_cut_text_delta <- compute_override_delta(
        matched_sheet_rows$edited_cut_text,
        default_edited_cut_text[matched]
      )
      local_rows$override_cut_description[matched] <- edited_cut_text_delta
      local_rows$override_cut_label[matched] <- edited_cut_text_delta
      if (length(matched_human_rows)) {
        local_rows$source_cut_description[matched_human_rows] <- matched_sheet_rows$edited_cut_text[matched_human]
        local_rows$source_generated_cut_summary[matched_human_rows] <- matched_sheet_rows$raw_cut_text[matched_human]
        local_rows$override_cut_description[matched_human_rows] <- NA_character_
        local_rows$override_cut_label[matched_human_rows] <- matched_sheet_rows$edited_cut_text[matched_human]
        local_rows$override_cut_summary[matched_human_rows] <- NA_character_
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

  sheet_only_human <- if (isTRUE(allow_editor_added_rows)) {
    sheet_only
  } else {
    sheet_only[is_college_cuts_human_row_origin(sheet_only$row_origin), , drop = FALSE]
  }
  if (nrow(sheet_only_human) > 0L) {
    human_rows <- rep_like_template_rows(empty_college_cuts_editorial_overrides(), nrow(sheet_only_human))
    human_rows$cut_id <- sheet_only_human$cut_id
    for (field_name in names(COLLEGE_CUTS_SHEET_SOURCE_MAP)) human_rows[[COLLEGE_CUTS_SHEET_SOURCE_MAP[[field_name]]]] <- sheet_only_human[[field_name]]
    human_rows$source_cut_description <- sheet_only_human$edited_cut_text
    human_rows$source_generated_cut_summary <- sheet_only_human$raw_cut_text
    human_rows$override_cut_label <- sheet_only_human$edited_cut_text
    human_rows$source_source_title <- dplyr::coalesce(if ("source_title" %in% names(sheet_only)) trim_optional_text(sheet_only_human$source_title) else rep(NA_character_, nrow(sheet_only_human)), sheet_only_human$source_publication)
    human_rows$first_seen <- dplyr::coalesce(sheet_only_human$first_seen, first_seen)
    human_rows$review_status <- sheet_only_human$review_status
    human_rows$reviewer <- sheet_only_human$reviewer
    human_rows$reviewer_notes <- sheet_only_human$reviewer_notes
    human_rows$reviewed_at <- sheet_only_human$reviewed_at
    human_rows$grandfathered <- sheet_only_human$grandfathered
    local_rows <- dplyr::bind_rows(local_rows, human_rows)
  }

  coerce_college_cuts_editorial_overrides(local_rows)
}

drop_stale_college_cuts_sheet_rows <- function(sheet_rows,
                                               local_cut_ids,
                                               candidate_cut_ids = NULL) {
  sheet_data <- coerce_college_cuts_review_sheet_rows(sheet_rows)
  if (!nrow(sheet_data)) {
    return(list(
      kept_rows = sheet_data,
      dropped_rows = sheet_data,
      quarantined_rows = sheet_data
    ))
  }

  local_ids <- unique(trim_text(local_cut_ids))
  local_ids <- local_ids[nzchar(local_ids)]
  candidate_ids <- unique(trim_text(candidate_cut_ids))
  candidate_ids <- candidate_ids[nzchar(candidate_ids)]

  sheet_ids <- trim_text(sheet_data$cut_id)
  human_mask <- is_college_cuts_human_row_origin(sheet_data$row_origin)
  local_mask <- nzchar(sheet_ids) & (sheet_ids %in% local_ids)
  current_candidate_mask <- nzchar(sheet_ids) & (sheet_ids %in% candidate_ids)
  stale_non_human_mask <- !human_mask & !local_mask & !current_candidate_mask
  # Stale rows that carry editorial decisions are never silently dropped:
  # they are excluded from the merge (they have no local row to merge into)
  # but returned separately so the caller can preserve them durably.
  decision_mask <- review_sheet_row_has_decision(
    sheet_data$review_status,
    sheet_data$reviewer,
    sheet_data$reviewer_notes,
    sheet_data$reviewed_at
  )
  quarantine_mask <- stale_non_human_mask & decision_mask
  drop_mask <- stale_non_human_mask & !decision_mask

  list(
    kept_rows = sheet_data[!stale_non_human_mask, , drop = FALSE],
    dropped_rows = sheet_data[drop_mask, , drop = FALSE],
    quarantined_rows = sheet_data[quarantine_mask, , drop = FALSE]
  )
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

backfill_college_cuts_grandfathered_generic_labels <- function(overrides,
                                                               review_statuses = "approved",
                                                               grandfathered_only = TRUE) {
  local_rows <- coerce_college_cuts_editorial_overrides(overrides)
  if (!nrow(local_rows)) return(local_rows[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])

  preferred_edited_text <- derive_default_college_cuts_edited_text(
    local_rows$source_cut_description,
    local_rows$source_generated_cut_label
  )
  status_values <- trim_text(local_rows$review_status)
  eligible_statuses <- trim_text(review_statuses)
  source_description <- trim_optional_text(local_rows$source_cut_description)
  override_description <- trim_optional_text(local_rows$override_cut_description)
  override_label <- trim_optional_text(local_rows$override_cut_label)

  to_repair <- status_values %in% eligible_statuses &
    !is.na(preferred_edited_text) &
    nzchar(preferred_edited_text) &
    (isFALSE(grandfathered_only) | local_rows$grandfathered %in% TRUE) &
    (is.na(override_description) | !nzchar(override_description)) &
    !is.na(override_label) &
    !is.na(source_description) &
    override_label == source_description &
    preferred_edited_text != override_label

  if (!any(to_repair)) {
    return(local_rows[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE])
  }

  local_rows$override_cut_description[to_repair] <- preferred_edited_text[to_repair]
  local_rows$override_cut_label[to_repair] <- preferred_edited_text[to_repair]
  local_rows[, COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS, drop = FALSE]
}

build_review_backed_college_cuts_export_row <- function(override_row, template_df) {
  internal_row <- coerce_college_cuts_editorial_overrides(override_row)
  if (!nrow(internal_row)) return(template_df[0, , drop = FALSE])
  effective_row <- build_college_cuts_review_sheet_rows(internal_row)[1, , drop = FALSE]
  default_edited_cut_text <- derive_default_college_cuts_edited_text(
    internal_row$source_cut_description[[1]],
    internal_row$source_generated_cut_label[[1]]
  )
  effective_cut_description <- dplyr::coalesce(
    internal_row$override_cut_description[[1]],
    internal_row$override_cut_label[[1]],
    default_edited_cut_text[[1]],
    effective_row$edited_cut_text[[1]]
  )
  export_row <- blank_like_row(template_df)
  if (!("row_origin" %in% names(export_row))) export_row$row_origin <- NA_character_
  row_origin_value <- trim_optional_text(internal_row$source_row_origin[[1]])
  if (is.na(row_origin_value) || !nzchar(row_origin_value)) {
    row_origin_value <- trim_optional_text(effective_row$row_origin[[1]])
  }
  if (is.na(row_origin_value) || !nzchar(row_origin_value)) row_origin_value <- "manual"
  export_id_prefix <- if (identical(row_origin_value, "manual")) "manual-cut-" else "review-cut-"
  export_unitid <- if (is.na(effective_row$unitid[[1]]) || !nzchar(effective_row$unitid[[1]])) paste0(export_id_prefix, effective_row$cut_id[[1]]) else effective_row$unitid[[1]]
  source_title_value <- dplyr::coalesce(internal_row$override_source_title[[1]], internal_row$source_source_title[[1]], effective_row$source_publication[[1]])

  if ("cut_label_override_effective" %in% names(export_row)) export_row$cut_label_override_effective[[1]] <- dplyr::coalesce(internal_row$override_cut_label[[1]], NA_character_)
  if ("cut_summary_override_effective" %in% names(export_row)) export_row$cut_summary_override_effective[[1]] <- dplyr::coalesce(internal_row$override_cut_summary[[1]], NA_character_)
  if ("cut_id" %in% names(export_row)) export_row$cut_id[[1]] <- effective_row$cut_id[[1]]
  if ("matched_unitid" %in% names(export_row)) export_row$matched_unitid[[1]] <- effective_row$unitid[[1]]
  if ("export_unitid" %in% names(export_row)) export_row$export_unitid[[1]] <- export_unitid
  if ("institution_name_display" %in% names(export_row)) export_row$institution_name_display[[1]] <- effective_row$institution_name[[1]]
  if ("state_display" %in% names(export_row)) export_row$state_display[[1]] <- effective_row$state[[1]]
  if ("announcement_date" %in% names(export_row)) export_row$announcement_date[[1]] <- effective_row$announcement_date[[1]]
  if ("announcement_year" %in% names(export_row)) export_row$announcement_year[[1]] <- suppressWarnings(as.integer(dplyr::coalesce(effective_row$announcement_year[[1]], derive_year_from_date_string(effective_row$announcement_date[[1]]))))
  if ("cut_type" %in% names(export_row)) export_row$cut_type[[1]] <- effective_row$cut_type[[1]]
  if ("program_name" %in% names(export_row)) export_row$program_name[[1]] <- effective_cut_description
  if ("source_url" %in% names(export_row)) export_row$source_url[[1]] <- effective_row$source_url[[1]]
  if ("source_title" %in% names(export_row)) export_row$source_title[[1]] <- source_title_value
  if ("source_publication" %in% names(export_row)) export_row$source_publication[[1]] <- effective_row$source_publication[[1]]
  export_row$row_origin[[1]] <- row_origin_value
  if ("has_financial_profile" %in% names(export_row)) export_row$has_financial_profile[[1]] <- !is.na(effective_row$unitid[[1]]) && nzchar(effective_row$unitid[[1]])
  if ("is_primary_tracker" %in% names(export_row)) export_row$is_primary_tracker[[1]] <- FALSE
  if ("in_financial_tracker" %in% names(export_row)) export_row$in_financial_tracker[[1]] <- if (!is.na(effective_row$unitid[[1]]) && nzchar(effective_row$unitid[[1]])) "TRUE" else "FALSE"
  export_row
}

build_manual_college_cuts_export_row <- function(override_row, template_df) {
  build_review_backed_college_cuts_export_row(override_row, template_df)
}

apply_college_cuts_editorial_overrides <- function(cuts_df,
                                                   overrides = NULL,
                                                   enforce_review_gate = FALSE,
                                                   allowed_cut_ids = NULL,
                                                   drop_unlisted = FALSE,
                                                   gate_mask = NULL) {
  override_rows <- coerce_college_cuts_editorial_overrides(overrides)
  approved_review_mask <- trim_text(override_rows$review_status) == "approved"
  approved_review_mask[is.na(approved_review_mask)] <- FALSE
  published_override_rows <- override_rows[approved_review_mask, , drop = FALSE]
  if (is.null(cuts_df)) {
    if (!nrow(published_override_rows)) return(cuts_df)
    stop("Approved college cuts review rows need a cuts data frame template during export.", call. = FALSE)
  }

  required_columns <- c("cut_id", "matched_unitid", "export_unitid", "institution_name_display", "state_display", "announcement_date", "announcement_year", "cut_type", "program_name", "source_url", "source_title", "source_publication")
  missing_columns <- setdiff(required_columns, names(cuts_df))
  if (length(missing_columns) > 0L) stop(sprintf("apply_college_cuts_editorial_overrides requires these columns: %s", paste(missing_columns, collapse = ", ")), call. = FALSE)

  if (!nrow(cuts_df)) {
    if (!nrow(published_override_rows)) return(cuts_df)
    return(dplyr::bind_rows(cuts_df, dplyr::bind_rows(lapply(seq_len(nrow(published_override_rows)), function(i) build_review_backed_college_cuts_export_row(published_override_rows[i, , drop = FALSE], cuts_df)))))
  }

  review_cuts <- cuts_df
  review_cuts$cut_id <- vapply(seq_len(nrow(review_cuts)), function(i) compute_college_cuts_review_id(review_cuts$cut_id[[i]], dplyr::coalesce(review_cuts$matched_unitid[[i]], review_cuts$export_unitid[[i]]), review_cuts$announcement_date[[i]], review_cuts$program_name[[i]], review_cuts$institution_name_display[[i]], review_cuts$state_display[[i]]), character(1))
  gate_rows <- if (is.null(gate_mask)) {
    rep(TRUE, nrow(review_cuts))
  } else {
    as.logical(gate_mask)
  }
  if (length(gate_rows) != nrow(review_cuts)) {
    stop("apply_college_cuts_editorial_overrides gate_mask must have one value per cut row.", call. = FALSE)
  }
  gate_rows[is.na(gate_rows)] <- FALSE
  allowed_ids <- trim_text(allowed_cut_ids)
  allowed_ids <- unique(allowed_ids[nzchar(allowed_ids)])
  if (length(allowed_ids) > 0L && isTRUE(drop_unlisted)) {
    unexpected_rows <- gate_rows & !(review_cuts$cut_id %in% allowed_ids)
    if (any(unexpected_rows)) {
      unexpected_ids <- unique(trim_text(review_cuts$cut_id[unexpected_rows]))
      message(sprintf(paste("Apply-only college cuts review gate: ignoring %d recomputed cut row(s)", "that are not present in the committed review candidate snapshot.", "Sample cut_id values: %s"), sum(unexpected_rows), paste(utils::head(unexpected_ids, 5L), collapse = ", ")))
      if (sum(unexpected_rows) > REVIEW_GATE_IGNORED_ROWS_WARN_THRESHOLD) {
        warning(sprintf(paste("Apply-only college cuts review gate ignored %d recomputed cut row(s),", "above the %d-row threshold: the committed snapshot and recomputed cuts may have drifted."), sum(unexpected_rows), REVIEW_GATE_IGNORED_ROWS_WARN_THRESHOLD), call. = FALSE)
      }
      review_cuts <- review_cuts[!unexpected_rows, , drop = FALSE]
      gate_rows <- gate_rows[!unexpected_rows]
    }
  }

  if (!nrow(override_rows)) {
    if (isTRUE(enforce_review_gate) && any(gate_rows)) stop("College cuts review gate is enabled but editorial_overrides.csv is empty or missing.", call. = FALSE)
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
      decided_ids <- unique(trim_text(override_rows$cut_id)[is_terminal_review_decision(override_rows$review_status)])
      decided_ids <- decided_ids[nzchar(decided_ids)]
      missing_override <- gate_rows & !(trim_text(joined$cut_id) %in% decided_ids)
      if (any(missing_override)) stop(sprintf(paste("College cuts review gate is enabled but %d cut row(s) are missing an editorial decision (approved/reject).", "Sample cut_id values: %s"), sum(missing_override), paste(utils::head(joined$cut_id[missing_override], 5L), collapse = ", ")), call. = FALSE)
    }
  }

  joined_approved_mask <- trim_text(joined$review_status) == "approved"
  joined_approved_mask[is.na(joined_approved_mask)] <- FALSE
  approved_override_values <- function(values) {
    approved_values <- values
    approved_values[!joined_approved_mask] <- NA_character_
    approved_values
  }

  approved_default_edited_cut_text <- derive_default_college_cuts_edited_text(
    joined$source_cut_description,
    joined$source_generated_cut_label
  )
  approved_default_edited_cut_text[!joined_approved_mask] <- NA_character_
  joined$matched_unitid <- effective_override_values(joined$matched_unitid, approved_override_values(joined$override_unitid))
  joined$institution_name_display <- effective_override_values(joined$institution_name_display, approved_override_values(joined$override_institution_name))
  joined$state_display <- effective_override_values(joined$state_display, approved_override_values(joined$override_state))
  joined$announcement_date <- effective_override_values(joined$announcement_date, approved_override_values(joined$override_announcement_date))
  if ("announcement_year" %in% names(joined)) joined$announcement_year <- suppressWarnings(as.integer(dplyr::coalesce(effective_override_values(trim_optional_text(joined$announcement_year), approved_override_values(joined$override_announcement_year)), derive_year_from_date_string(joined$announcement_date))))
  joined$cut_type <- effective_override_values(joined$cut_type, approved_override_values(joined$override_cut_type))
  approved_edited_cut_text <- dplyr::coalesce(
    approved_override_values(joined$override_cut_description),
    approved_override_values(joined$override_cut_label),
    approved_default_edited_cut_text
  )
  joined$program_name <- effective_override_values(joined$program_name, approved_edited_cut_text)
  # Expose approved label/summary overrides for downstream public field derivation.
  if ("override_cut_label" %in% names(joined)) {
    joined$cut_label_override_effective <- dplyr::coalesce(
      approved_override_values(joined$override_cut_label),
      approved_default_edited_cut_text
    )
  } else {
    joined$cut_label_override_effective <- approved_default_edited_cut_text
  }
  if ("override_cut_summary" %in% names(joined)) {
    joined$cut_summary_override_effective <- approved_override_values(joined$override_cut_summary)
  } else {
    joined$cut_summary_override_effective <- NA_character_
  }
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
  if (isTRUE(enforce_review_gate)) joined <- joined[joined_approved_mask | !gate_rows, , drop = FALSE]

  missing_review_rows <- published_override_rows[!(trim_text(published_override_rows$cut_id) %in% trim_text(review_cuts$cut_id)), , drop = FALSE]
  if (nrow(missing_review_rows)) joined <- dplyr::bind_rows(joined, dplyr::bind_rows(lapply(seq_len(nrow(missing_review_rows)), function(i) build_review_backed_college_cuts_export_row(missing_review_rows[i, , drop = FALSE], review_cuts))))
  joined
}
