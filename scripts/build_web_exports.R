main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args          <- parse_cli_args(cli_args)
  ipeds         <- load_ipeds_paths()
  ipeds_layout  <- ipeds$ipeds_layout
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)

# This script turns the canonical IPEDS dataset plus the joined cuts,
# accreditation, research, and scorecard files into the JSON payloads used by
# the static site.
# The helpers below focus on making those exports resilient to missing values so
# a sparse field in one source does not break the generated pages.

ensure_packages(c("dplyr", "jsonlite", "readr"))
source(file.path(getwd(), "scripts", "shared", "export_helpers.R"))
source(file.path(getwd(), "scripts", "shared", "contracts.R"))

validate_multi_year_web_input <- function(df, input_path) {
  years <- sort(unique(stats::na.omit(as.integer(df$year))))
  if (length(years) < 2L) {
    stop(
      paste(
        "build_web_exports requires the multi-year canonical IPEDS dataset so",
        "school detail charts can show decade trends. The selected input has",
        length(years),
        "year(s):",
        paste(years, collapse = ", "),
        sprintf("Input: %s", input_path),
        "Use the multi-year file in ipeds/derived/ or",
        "rebuild the canonical dataset before exporting the static site."
      ),
      call. = FALSE
    )
  }
}

input_csv  <- get_arg_value("--input", ipeds_layout(root = ".")$dataset_csv)
output_dir <- get_arg_value("--output-dir", ".")

root <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
input_path <- normalizePath(input_csv, winslash = "/", mustWork = TRUE)
data_dir <- file.path(root, "data")
schools_dir <- file.path(data_dir, "schools")
downloads_dir <- file.path(data_dir, "downloads")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(schools_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(downloads_dir, recursive = TRUE, showWarnings = FALSE)

cuts_path <- file.path(root, "data_pipelines", "college_cuts", "college_cuts_financial_tracker_cut_level_joined.csv")
accreditation_summary_path <- file.path(root, "data_pipelines", "accreditation", "accreditation_tracker_institution_summary.csv")
accreditation_actions_path <- file.path(root, "data_pipelines", "accreditation", "accreditation_tracker_actions_joined.csv")
accreditation_coverage_path <- file.path(root, "data_pipelines", "accreditation", "accreditation_tracker_source_coverage.csv")
research_summary_path <- file.path(root, "data_pipelines", "grant_witness", "grant_witness_higher_ed_institution_summary.csv")
research_grants_path <- file.path(root, "data_pipelines", "grant_witness", "grant_witness_grant_level_joined.csv")
outcomes_summary_path <- file.path(root, "data_pipelines", "scorecard", "tracker_outcomes_joined.csv")
closure_status_json_path <- file.path(data_dir, "closure_status_by_unitid.json")
hcm_json_path <- file.path(data_dir, "hcm2_by_unitid.json")
federal_composite_json_path <- file.path(data_dir, "federal_composite_scores_by_unitid.json")

research_min_public_award_remaining <- 100


# Helper functions (require_local_file, ensure_columns, null_if_empty,
# write_json_file, build_series, etc.) are in scripts/shared/export_helpers.R

build_cuts_export <- function() {
  # Build the site payload for the college cuts page and related downloads.
  # This keeps the landing-page summaries and school-level cut tables derived
  # from the same joined cuts file.
  require_local_file(
    cuts_path,
    "college cuts joined dataset",
    "Run `Rscript --vanilla ./scripts/build_college_cuts_join.R` first."
  )

  cuts <- readr::read_csv(cuts_path, show_col_types = FALSE) %>%
    mutate(
      matched_unitid = as.character(matched_unitid),
      announcement_date = as.character(announcement_date),
      announcement_year = as.integer(announcement_year),   # integer in JSON, not string
      in_financial_tracker = as.character(in_financial_tracker),
      export_unitid = vapply(
        seq_len(n()),
        function(i) make_export_id(
          "cut",
          matched_unitid[[i]],
          tracker_institution_name[[i]] %||% institution_name_collegecuts[[i]],
          institution_state_full[[i]]
        ),
        character(1)
      ),
      institution_name_display = dplyr::coalesce(tracker_institution_name, institution_name_collegecuts),
      city_display = institution_city,
      state_display = institution_state_full,
      control_label_display = vapply(
        dplyr::coalesce(tracker_control_label, institution_control),
        normalize_control_label,
        character(1)
      ),
      category_display = tracker_category,
      has_financial_profile = !is.na(matched_unitid) & matched_unitid != "" & in_financial_tracker == "TRUE",
      is_primary_tracker = has_financial_profile & is_primary_bachelors_category(category_display),
      positions_affected = vapply(
        seq_len(n()),
        function(i) derive_positions_affected(
          faculty_affected[[i]],
          notes[[i]],
          source_title[[i]],
          program_name[[i]],
          cut_type[[i]]
        ),
        integer(1)
      )
    )

  if (nrow(cuts) == 0) return(NULL)

  recent <- cuts %>%
    arrange(desc(announcement_date), desc(announcement_year)) %>%
    slice_head(n = 25) %>%
    transmute(
      unitid = export_unitid,
      financial_unitid = ifelse(has_financial_profile, matched_unitid, NA_character_),
      institution_name = institution_name_display,
      city = city_display,
      state = state_display,
      control_label = control_label_display,
      category = category_display,
      is_primary_tracker = is_primary_tracker,
      has_financial_profile = has_financial_profile,
      announcement_date = announcement_date,
      announcement_year = announcement_year,
      program_name = program_name,
      cut_type = cut_type,
      status = status,
      effective_term = effective_term,
      positions_affected = positions_affected,
      notes = notes,
      source_url = source_url,
      source_title = source_title,
      source_publication = source_publication
    )

  schools <- lapply(split(cuts, cuts$export_unitid), function(df) {
    df <- df %>% arrange(desc(announcement_date), desc(announcement_year))
    latest <- df %>% slice(1)
    list(
      unitid = as.character(latest$export_unitid[[1]]),
      financial_unitid = if (isTRUE(latest$has_financial_profile[[1]])) latest$matched_unitid[[1]] else NA_character_,
      has_financial_profile = isTRUE(latest$has_financial_profile[[1]]),
      is_primary_tracker = isTRUE(latest$is_primary_tracker[[1]]),
      institution_name = latest$institution_name_display[[1]],
      city = latest$city_display[[1]],
      state = latest$state_display[[1]],
      control_label = latest$control_label_display[[1]],
      category = latest$category_display[[1]],
      latest_cut_date = or_null(latest$announcement_date),
      latest_cut_label = or_null(latest$program_name),
      cut_count = nrow(df),
      cuts = lapply(seq_len(nrow(df)), function(i) {
        list(
          announcement_date = or_null(df$announcement_date[i]),
          announcement_year = or_null(df$announcement_year[i]),
          program_name = or_null(df$program_name[i]),
          cut_type = or_null(df$cut_type[i]),
          status = or_null(df$status[i]),
          effective_term = or_null(df$effective_term[i]),
          students_affected = or_null(df$students_affected[i]),
          faculty_affected = or_null(df$faculty_affected[i]),
          positions_affected = or_null(df$positions_affected[i]),
          notes = or_null(df$notes[i]),
          source_url = or_null(df$source_url[i]),
          source_title = or_null(df$source_title[i]),
          source_publication = or_null(df$source_publication[i]),
          source_published_at = or_null(df$source_published_at[i])
        )
      })
    )
  })

  list(
    generated_at = as.character(Sys.Date()),
    recent = recent,
    schools = schools
  )
}

build_accreditation_export <- function() {
  # Build the accreditation landing-page and school-level payloads from the
  # joined accreditation summary and actions files.
  # Coverage notes are carried through so missing source support can be surfaced
  # explicitly instead of showing silent blanks.
  require_local_file(
    accreditation_summary_path,
    "accreditation institution summary",
    "Run `Rscript --vanilla ./scripts/build_accreditation_actions.R` first."
  )
  require_local_file(
    accreditation_actions_path,
    "accreditation joined actions file",
    "Run `Rscript --vanilla ./scripts/build_accreditation_actions.R` first."
  )

  # Phase 2: derive_action_label_short() is now defined at module scope
  # in scripts/shared/export_helpers.R (sourced near the top of this
  # file). Non-MSCHE accreditors continue to pass through their
  # scrape-time action_label_raw verbatim. MSCHE rows now run through a
  # small set of verb+noun anchored summarization patterns (approved
  # teach-out with scope, voluntarily surrendered accreditation, warning
  # with optional Standard reference, removed-from-probation, continued-
  # on-warning with duration) and a clean fallback that strips the
  # "acknowledge receipt of ..." preamble and returns the first
  # remaining sentence. Patterns are pinned by tests in
  # tests/test_export_helpers.R; do not modify here.

  normalize_accreditation_date <- function(x) {
    vapply(x, function(value) {
      if (length(value) == 0 || is.na(value)) return(NA_character_)
      text <- trimws(as.character(value))
      if (!nzchar(text)) return(NA_character_)
      if (grepl("^\\d{4}-\\d{2}-\\d{2}$", text)) return(text)
      if (grepl("^\\d{4}-\\d{2}$", text)) return(paste0(text, "-01"))

      month_formats <- c(
        "%B %d, %Y",
        "%b %d, %Y",
        "%d %B %Y",
        "%d %b %Y"
      )
      for (fmt in month_formats) {
        parsed <- as.Date(text, format = fmt)
        if (!is.na(parsed)) return(as.character(parsed))
      }

      month_year_formats <- c("%B %Y", "%b %Y")
      for (fmt in month_year_formats) {
        parsed <- as.Date(paste("1", text), format = paste("%d", fmt))
        if (!is.na(parsed)) return(as.character(parsed))
      }

      text
    }, character(1), USE.NAMES = FALSE)
  }

  MIN_PUBLIC_ACTION_YEAR <- 2019L
  TODAY <- Sys.Date()
  TRUSTED_ACTION_TYPES <- c("adverse_action", "warning", "probation", "show_cause", "removed", "notice")
  MSCHE_PROCEDURAL_DROP_PATTERNS <- c(
    "^\\s*(?:staff acted on behalf of the commission )?to request (?:a |an )?supplemental information report",
    "^\\s*(?:staff acted on behalf of the commission )?to request (?:a |an )?monitoring report",
    "^\\s*(?:staff acted on behalf of the commission )?to request (?:a |an )?candidate assessment",
    "^\\s*(?:staff acted on behalf of the commission )?to request an? updated teach-?out plan",
    "^\\s*to require [^.]{0,200}?teach-?out plan",
    "^\\s*to request [^.]{0,200}?teach-?out plan",
    "^\\s*to note the follow-up team visit",
    "^\\s*to note that the complex substantive change visit occurred",
    "^\\s*to note that an? updated teach-?out plan [^.]{0,80}? will not be required",
    "^\\s*(?:staff acted on behalf of the commission )?to temporarily waive substantive change policy",
    "^\\s*to approve the teach-?out plan as required of candidate",
    "^\\s*to reject the teach-?out plan",
    "^\\s*to note that the supplemental information report was not conducive",
    "^\\s*(?:staff acted (?:on behalf of the commission )?)?to acknowledge receipt of",
    "^\\s*to note the (?:show cause |follow-?up |on-site |virtual )?visit by the commission'?s representatives",
    "^\\s*to note that .* hosted a virtual site visit",
    "^\\s*to note that .* (?:will not be continuing as|is now due|are now due|was not received)",
    "^\\s*to note that the institution received the notification of adverse action",
    "^\\s*to note that the administrator of the appeal",
    "^\\s*to postpone a decision on",
    "^\\s*to reject the supplemental information report",
    "^\\s*to request submission of signed teach-?out agreements",
    "^\\s*to request an updated accreditation readiness report",
    "^\\s*to remind the institution of",
    "^\\s*to grant a delay of the monitoring report",
    "^\\s*to grant accreditation because the institution has met the requirements of the addition or change of primary accreditor"
  )
  MSCHE_SUBSTANTIVE_KEEP_PATTERN <- paste0(
    "^\\s*(?:merger of|accepted teach-?out plan|",
    "to approve the (?:updated )?teach-?out plan(?! as required of candidate)|",
    "to approve the teach-?out agreements?|approved teach-?out plan|approved teach-?out agreements?)"
  )
  MSCHE_PROCEDURAL_CONTENT_PATTERNS <- c(
    "addition or change of primary accreditor to msche procedures",
    "candidate assessment report",
    "reasonable cause determination for multiple accreditation"
  )

  normalize_accreditation_text <- function(x) {
    value <- tolower(trimws(as.character(x %||% "")))
    gsub("\\s+", " ", value)
  }

  parse_public_action_date <- function(x) {
    if (length(x) == 0 || is.na(x)) return(as.Date(NA))
    text <- trimws(as.character(x))
    if (!nzchar(text)) return(as.Date(NA))
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", text)) return(as.Date(text))
    if (grepl("^\\d{4}-\\d{2}$", text)) return(as.Date(paste0(text, "-01")))
    parsed <- suppressWarnings(as.Date(text))
    if (!is.na(parsed)) return(parsed)
    as.Date(NA)
  }

  get_public_action_year <- function(action_year, action_date) {
    explicit_year <- suppressWarnings(as.integer(action_year))
    if (!is.na(explicit_year) && explicit_year > 0L) return(explicit_year)
    date_text <- trimws(as.character(action_date %||% ""))
    match <- regmatches(date_text, regexpr("\\b(19|20)\\d{2}\\b", date_text, perl = TRUE))
    if (length(match) == 0L || is.na(match) || !nzchar(match)) return(NA_integer_)
    suppressWarnings(as.integer(match))
  }

  public_action_has_occurred <- function(action_year, action_date) {
    parsed_date <- parse_public_action_date(action_date)
    if (!is.na(parsed_date)) return(parsed_date <= TODAY)
    year <- get_public_action_year(action_year, action_date)
    !is.na(year) && year >= MIN_PUBLIC_ACTION_YEAR && year <= as.integer(format(TODAY, "%Y"))
  }

  is_tracked_public_action <- function(action_type, accreditor, action_label_raw, notes) {
    type <- normalize_accreditation_text(action_type)
    accreditor_code <- toupper(trimws(as.character(accreditor %||% "")))
    label <- normalize_accreditation_text(action_label_raw)
    label_short <- normalize_accreditation_text(
      derive_action_label_short(action_type, action_label_raw, accreditor)
    )
    notes_text <- normalize_accreditation_text(notes)
    haystack <- trimws(paste(type, label, notes_text))
    content_only <- trimws(paste(label, notes_text))

    if (accreditor_code == "MSCHE" && identical(type, "monitoring")) return(FALSE)

    if (accreditor_code == "MSCHE") {
      candidate_labels <- unique(Filter(nzchar, c(label_short, label)))
      combined_label_text <- paste(candidate_labels, collapse = " ")
      short_label <- if (length(candidate_labels) > 0L) candidate_labels[[1]] else ""
      has_explicit_keep_label <- any(vapply(candidate_labels, function(candidate) {
        grepl(MSCHE_SUBSTANTIVE_KEEP_PATTERN, candidate, ignore.case = TRUE, perl = TRUE)
      }, logical(1)))
      has_merger_legal_status <-
        grepl("^\\s*change of legal status", short_label, ignore.case = TRUE, perl = TRUE) &&
        grepl("merger|surviving institution|anticipated date of the transaction", combined_label_text, ignore.case = TRUE, perl = TRUE)
      if (
        grepl("^\\s*change of legal status", combined_label_text, ignore.case = TRUE, perl = TRUE) &&
        !has_merger_legal_status
      ) {
        return(FALSE)
      }
      for (pattern in MSCHE_PROCEDURAL_DROP_PATTERNS) {
        if (
          any(vapply(candidate_labels, function(candidate) {
            grepl(pattern, candidate, ignore.case = TRUE, perl = TRUE)
          }, logical(1))) &&
          !has_explicit_keep_label &&
          !has_merger_legal_status
        ) {
          return(FALSE)
        }
      }
      if (
        any(vapply(MSCHE_PROCEDURAL_CONTENT_PATTERNS, function(pattern) {
          grepl(pattern, combined_label_text, ignore.case = TRUE, perl = TRUE)
        }, logical(1))) &&
        !has_explicit_keep_label &&
        !has_merger_legal_status
      ) {
        return(FALSE)
      }
      if (has_explicit_keep_label || has_merger_legal_status) return(TRUE)
    }

    if (
      accreditor_code == "NWCCU" &&
      grepl("policies, regulations, and financial review", content_only, ignore.case = TRUE, perl = TRUE) &&
      grepl("substantially compliant", notes_text, ignore.case = TRUE, perl = TRUE)
    ) {
      return(FALSE)
    }

    if (
      grepl("substantive change|program addition", haystack, ignore.case = TRUE, perl = TRUE) &&
      !type %in% TRUSTED_ACTION_TYPES
    ) {
      return(FALSE)
    }

    status_action_pattern <- paste(
      "warning|probation|formal notice of concern|notice of concern|\\bmonitoring\\b|",
      "removed from (warning|probation|formal notice of concern|notice of concern|notice|monitoring)|",
      "removed from membership|placed on probation|issue a notice of concern|continue a warning|",
      "continued on warning|continued on probation|denied reaffirmation",
      sep = ""
    )
    closure_action_pattern <- "accepted notification of institutional closure|accept(?:ed)? teach-?out plan|approve(?:d)? (?:the )?(?:updated )?teach-?out (?:plan|agreement|agreements)|teach out plan|teach-?out plan|removed from membership|will transfer from .* to "
    required_report_pattern <- "require (?:the institution to provide )?(?:an )?(?:interim|progress|follow-?up|monitoring) report"
    standalone_low_signal_pattern <- "^(special visit|interim report|progress report|accepted progress report|accepted interim report|follow-?up report|monitoring report|second monitoring report|third monitoring report)$"

    has_special_visit <- grepl("special visit", haystack, ignore.case = TRUE, perl = TRUE)
    has_sanction_decision <-
      grepl(status_action_pattern, content_only, ignore.case = TRUE, perl = TRUE) ||
      grepl(closure_action_pattern, content_only, ignore.case = TRUE, perl = TRUE) ||
      grepl(required_report_pattern, content_only, ignore.case = TRUE, perl = TRUE)

    if (has_special_visit && !has_sanction_decision) return(FALSE)

    if (
      grepl(status_action_pattern, content_only, ignore.case = TRUE, perl = TRUE) ||
      grepl(closure_action_pattern, content_only, ignore.case = TRUE, perl = TRUE) ||
      grepl(required_report_pattern, content_only, ignore.case = TRUE, perl = TRUE)
    ) {
      return(TRUE)
    }

    if (grepl(standalone_low_signal_pattern, label, ignore.case = TRUE, perl = TRUE)) return(FALSE)

    type %in% c("warning", "probation", "monitoring", "notice") ||
      grepl("removed from membership|teach-?out|institutional closure", haystack, ignore.case = TRUE, perl = TRUE)
  }

  is_recent_public_action <- function(action_type, accreditor, action_label_raw, notes, action_year, action_date, display_action) {
    year <- get_public_action_year(action_year, action_date)
    isTRUE(display_action) &&
      is_tracked_public_action(action_type, accreditor, action_label_raw, notes) &&
      !is.na(year) &&
      year >= MIN_PUBLIC_ACTION_YEAR &&
      public_action_has_occurred(action_year, action_date)
  }

  summary_df <- readr::read_csv(accreditation_summary_path, show_col_types = FALSE) %>%
    ensure_columns(list(
      accreditors = NA_character_,
      latest_action_date = NA_character_,
      latest_action_year = NA_character_,
      action_labels = NA_character_,
      active_actions = NA_character_,
      has_active_warning = NA,
      has_active_warning_or_notice = NA,
      has_active_adverse_action = NA,
      action_count = NA_integer_
    )) %>%
    mutate(
      unitid = as.character(unitid),
      latest_action_date = normalize_accreditation_date(latest_action_date),
      latest_action_year = na_if(as.character(latest_action_year), "")
    )
  # action_scope is populated by NECHE program-level rows only (a handful of
  # rows in a CSV that's >7000 rows long, all sitting after the >5000 MSCHE
  # rows that have empty action_scope). readr's default guess_max = 1000
  # types the column as logical from those leading empties, then silently
  # coerces the real strings that appear later to NA -- which is exactly
  # what surfaced the "BU NECHE row has no scope subtitle" UI bug. Pin the
  # type explicitly when the column is present. Some test fixtures use
  # older CSVs that pre-date the action_scope column; emitting a named
  # col_types entry for a column that doesn't exist warns "named parsers
  # don't match the column names", so peek the header first and only pin
  # the type when the column is actually there.
  actions_header <- readr::read_csv(
    accreditation_actions_path,
    n_max = 0L,
    show_col_types = FALSE
  )
  actions_col_types <- if ("action_scope" %in% names(actions_header)) {
    readr::cols(action_scope = readr::col_character(), .default = readr::col_guess())
  } else {
    readr::cols(.default = readr::col_guess())
  }
  actions_df <- readr::read_csv(
    accreditation_actions_path,
    show_col_types = FALSE,
    col_types = actions_col_types
  ) %>%
    ensure_columns(list(
      accreditor = NA_character_,
      action_status = NA_character_,
      action_date = NA_character_,
      action_year = NA_character_,
      action_scope = NA_character_,
      source_page_modified = NA_character_,
      display_action = TRUE,
      accreditors = NA_character_,
      latest_action_date = NA_character_,
      latest_action_year = NA_character_,
      action_labels = NA_character_,
      active_actions = NA_character_,
      has_active_warning = NA,
      has_active_warning_or_notice = NA,
      has_active_adverse_action = NA,
      action_count = NA_integer_,
      action_label_raw = NA_character_
    )) %>%
    mutate(
      unitid = as.character(unitid),
      action_date = normalize_accreditation_date(action_date),
      action_year = na_if(as.character(action_year), ""),
      source_page_modified = na_if(as.character(source_page_modified), ""),
      # Some accreditor sources (MSCHE's non-compliance status page, HLC's
      # monitoring list) publish the institution's current status without a
      # per-row action date. Without a date the frontend's isRecentTrackedAction
      # filter silently drops the row. For active statuses only, fall back to
      # the source page's last-modified date so these rows remain visible.
      action_date = dplyr::if_else(
        is.na(action_date) & !is.na(action_status) & action_status == "active" & !is.na(source_page_modified),
        source_page_modified,
        action_date
      ),
      action_year = dplyr::if_else(
        is.na(action_year) & !is.na(action_status) & action_status == "active" & !is.na(source_page_modified),
        substr(source_page_modified, 1L, 4L),
        action_year
      ),
      display_action = as.logical(display_action),
      accreditors = dplyr::coalesce(accreditors, accreditor),
      latest_action_date = dplyr::coalesce(latest_action_date, action_date),
      latest_action_year = dplyr::coalesce(latest_action_year, action_year),
      action_labels = dplyr::coalesce(action_labels, action_label_raw),
      action_count = dplyr::coalesce(suppressWarnings(as.integer(action_count)), 1L)
    )
  coverage_df <- if (file.exists(accreditation_coverage_path)) {
    readr::read_csv(accreditation_coverage_path, show_col_types = FALSE)
  } else {
    tibble::tibble()
  }

  # Accreditation export IDs follow the same stability rules as make_export_id
  # in export_helpers.R: numeric unitid when available, normalised slug otherwise.
  # Accreditation slugs additionally include the accreditor code so that the same
  # institution's HLC and MSCHE entries get distinct IDs when both are unmatched.
  make_accreditation_export_id <- function(unitid, institution_name, state, accreditor) {
    raw_unitid <- trimws(as.character(unitid %||% ""))
    if (!identical(raw_unitid, "")) return(raw_unitid)

    # Use the same name normalisation as slug_institution_name() in export_helpers.R
    name_slug <- tolower(trimws(institution_name %||% ""))
    name_slug <- sub("^the +", "", name_slug)
    name_slug <- gsub("\\bst\\.?\\b", "saint", name_slug)
    name_slug <- gsub("&", "and", name_slug, fixed = TRUE)
    name_slug <- gsub("[^a-z0-9]+", "-", name_slug)
    name_slug <- gsub("^-+|-+$", "", name_slug)

    state_slug <- gsub("[^a-z0-9]+", "-", tolower(trimws(state %||% "")))
    state_slug <- gsub("^-+|-+$", "", state_slug)

    accred_slug <- gsub("[^a-z0-9]+", "-", tolower(trimws(accreditor %||% "")))
    accred_slug <- gsub("^-+|-+$", "", accred_slug)

    parts <- Filter(nzchar, c(name_slug, state_slug, accred_slug))
    paste0("accred-", paste(parts, collapse = "--"))
  }

  summary_df <- summary_df %>%
    mutate(
      export_institution_name = pick_first_present(pick(dplyr::everything()), c("institution_name", "tracker_name", "institution_name_raw")),
      export_state = pick_first_present(pick(dplyr::everything()), c("state", "tracker_state", "institution_state_raw")),
      export_city = pick_first_present(pick(dplyr::everything()), c("city", "tracker_city")),
      export_control_label = pick_first_present(pick(dplyr::everything()), c("control_label", "tracker_control")),
      export_category = pick_first_present(pick(dplyr::everything()), c("category", "tracker_category")),
      export_unitid = vapply(
        seq_len(n()),
        function(i) make_accreditation_export_id(unitid[[i]], export_institution_name[[i]], export_state[[i]], accreditors[[i]]),
        character(1)
      )
    )
  actions_df <- actions_df %>%
    mutate(
      export_institution_name = pick_first_present(pick(dplyr::everything()), c("institution_name", "tracker_name", "institution_name_raw")),
      export_state = pick_first_present(pick(dplyr::everything()), c("state", "tracker_state", "institution_state_raw")),
      export_city = pick_first_present(pick(dplyr::everything()), c("city", "tracker_city")),
      export_control_label = pick_first_present(pick(dplyr::everything()), c("control_label", "tracker_control")),
      export_category = pick_first_present(pick(dplyr::everything()), c("category", "tracker_category")),
      export_unitid = vapply(
        seq_len(n()),
        function(i) make_accreditation_export_id(unitid[[i]], export_institution_name[[i]], export_state[[i]], accreditor[[i]]),
        character(1)
      ),
      has_financial_profile = !is.na(unitid) & unitid != "",
      is_primary_tracker = has_financial_profile & is_primary_bachelors_category(export_category)
    ) %>%
    filter(!is.na(export_unitid), export_unitid != "")

  actions_df <- actions_df %>%
    mutate(
      action_label_short = vapply(
        seq_len(n()),
        function(i) derive_action_label_short(action_type[[i]], action_label_raw[[i]], accreditor[[i]]),
        character(1)
      ),
      is_public_display_action = vapply(
        seq_len(n()),
        function(i) is_recent_public_action(
          action_type[[i]],
          accreditor[[i]],
          action_label_raw[[i]],
          notes[[i]],
          action_year[[i]],
          action_date[[i]],
          display_action[[i]]
        ),
        logical(1)
      )
    ) %>%
    filter(is_public_display_action %in% TRUE)

  # Always include all accreditors the project actively tracks, even if the
  # scraper returned zero rows for one of them (e.g. NWCCU with no qualifying
  # 4-year bachelor's institutions under action right now).
  ALL_TRACKED_ACCREDITORS <- c("HLC", "MSCHE", "NECHE", "NWCCU", "SACSCOC", "WSCUC")
  covered_accreditors <- sort(unique(c(ALL_TRACKED_ACCREDITORS, summary_df$accreditors, actions_df$accreditor)))
  covered_accreditors <- covered_accreditors[!is.na(covered_accreditors) & covered_accreditors != ""]

  not_covered <- list(
    list(name = "ACCJC", url = "https://accjc.org/", note = "Not yet integrated into the tracker workflow.")
  )

  schools <- lapply(split(actions_df, actions_df$export_unitid), function(df) {
    df <- df %>% arrange(desc(action_date), desc(action_year))
    summary_row <- summary_df %>% filter(export_unitid == df$export_unitid[[1]]) %>% slice(1)
    latest <- if (nrow(summary_row) > 0) summary_row else df %>% slice(1)
    active_types <- df$action_type[df$action_status == "active"]
    latest_action_year <- suppressWarnings(as.integer(df$action_year))

    sources <- df %>%
      distinct(accreditor, source_title, source_url, source_page_url, .keep_all = FALSE)

    list(
      unitid = as.character(df$export_unitid[[1]]),
      financial_unitid = if (isTRUE(df$has_financial_profile[[1]])) as.character(df$unitid[[1]]) else NA_character_,
      has_financial_profile = isTRUE(df$has_financial_profile[[1]]),
      is_primary_tracker = isTRUE(df$is_primary_tracker[[1]]),
      institution_name = or_null(latest$export_institution_name),
      city = or_null(latest$export_city),
      state = or_null(latest$export_state),
      control_label = or_null(latest$export_control_label),
      category = or_null(latest$export_category),
      latest_status = list(
        accreditors = or_null(collapse_unique_values(df$accreditor)),
        action_labels = or_null(collapse_unique_values(df$action_label_raw)),
        active_actions = or_null(collapse_unique_values(active_types)),
        has_active_warning = any(df$action_status == "active" & df$action_type == "warning", na.rm = TRUE),
        has_active_warning_or_notice = any(df$action_status == "active" & df$action_type %in% c("warning", "notice"), na.rm = TRUE),
        has_active_adverse_action = any(df$action_status == "active" & df$action_type == "adverse_action", na.rm = TRUE),
        # Use the rescued max date from the per-school action subset (df) so
        # institutions whose only actions are MSCHE non-compliance / HLC
        # current-status rows -- which arrive without a per-row date and are
        # backfilled from source_page_modified above -- still surface a date
        # on the school-detail header. The summary CSV's latest_action_date
        # is computed before that rescue runs and is therefore stale.
        latest_action_date = or_null_date(
          if (all(is.na(df$action_date))) NA_character_ else max(df$action_date, na.rm = TRUE)
        ),
        latest_action_year = or_null(
          if (all(is.na(latest_action_year))) NA_character_ else as.character(max(latest_action_year, na.rm = TRUE))
        ),
        action_count = nrow(df)
      ),
      actions = lapply(seq_len(nrow(df)), function(i) {
        list(
          accreditor = or_null(df$accreditor[i]),
          action_type = or_null(df$action_type[i]),
          action_label = or_null(df$action_label_raw[i]),
          # Compact display label for the global recent-actions table; the
          # per-school detail view continues to render the full
          # action_label above. Helper is module-scope in
          # scripts/shared/export_helpers.R; passing accreditor lets it
          # apply MSCHE-specific summarization while leaving every other
          # accreditor's label untouched.
          action_label_short = or_null(df$action_label_short[i]),
          action_scope = or_null(df$action_scope[i]),
          action_status = or_null(df$action_status[i]),
          action_date = or_null_date(df$action_date[i]),
          action_year = or_null(df$action_year[i]),
          notes = or_null(df$notes[i]),
          source_url = or_null(df$source_url[i]),
          source_title = or_null(df$source_title[i]),
          source_page_url = or_null(df$source_page_url[i]),
          source_page_modified = or_null(df$source_page_modified[i]),
          display_action = isTRUE(df$display_action[i]),
          has_financial_profile = isTRUE(df$has_financial_profile[i]),
          is_primary_tracker = isTRUE(df$is_primary_tracker[i])
        )
      }),
      sources = lapply(seq_len(nrow(sources)), function(i) {
        list(
          accreditor = or_null(sources$accreditor[i]),
          source_title = or_null(sources$source_title[i]),
          source_url = or_null(sources$source_url[i]),
          source_page_url = or_null(sources$source_page_url[i])
        )
      })
    )
  })

  list(
    generated_at = as.character(Sys.Date()),
    covered_accreditors = covered_accreditors,
    source_coverage = coverage_df,
    not_covered = not_covered,
    schools = schools
  )
}

build_research_export <- function() {
  # Build the Grant Witness research-funding payloads used on the research page.
  # This export reads the already-filtered research join, so Proposal G and the
  # pass-through exclusions are reflected everywhere the site shows research cuts.
  require_local_file(
    research_summary_path,
    "research institution summary",
    "Run `Rscript --vanilla ./scripts/build_grant_witness_join.R` first."
  )
  require_local_file(
    research_grants_path,
    "research grant-level join",
    "Run `Rscript --vanilla ./scripts/build_grant_witness_join.R` first."
  )

  summary_df <- readr::read_csv(research_summary_path, show_col_types = FALSE) %>%
    mutate(
      matched_unitid = as.character(matched_unitid),
      display_city = as.character(display_city),
      display_state = as.character(display_state),
      institution_key = as.character(institution_key),
      likely_higher_ed = as.logical(likely_higher_ed),
      export_unitid = vapply(
        seq_len(n()),
        function(i) make_export_id(
          "research",
          matched_unitid[[i]],
          display_name[[i]],
          display_state[[i]]
        ),
        character(1)
      ),
      has_financial_profile = !is.na(matched_unitid) & matched_unitid != "",
      # Keep the broader higher-ed research universe on the research page.
      # Schools can still carry a finance profile flag when they also appear in
      # the financial tracker, but research-only institutions should remain in
      # the export rather than being silently filtered out.
      is_primary_tracker = has_financial_profile & is_primary_bachelors_category(tracker_category)
    ) %>%
    filter(likely_higher_ed)

  grants_df <- readr::read_csv(
    research_grants_path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  ) %>%
    mutate(
      matched_unitid = as.character(matched_unitid),
      organization_state = as.character(organization_state),
      award_value = to_num(award_value),
      award_outlaid = to_num(award_outlaid),
      award_remaining = to_num(award_remaining),
      export_unitid = vapply(
        seq_len(n()),
        function(i) make_export_id(
          "research",
          matched_unitid[[i]],
          dplyr::coalesce(tracker_institution_name[[i]], organization_name[[i]]),
          dplyr::coalesce(tracker_state[[i]], organization_state[[i]])
        ),
        character(1)
      ),
      currently_disrupted = as.logical(currently_disrupted),  # normalise; CSV may be read as char or logical
      likely_higher_ed = as.logical(likely_higher_ed)
  ) %>%
    filter(
      currently_disrupted == TRUE,
      !is.na(award_remaining),
      award_remaining >= research_min_public_award_remaining
    )

  if (nrow(summary_df) == 0 || nrow(grants_df) == 0) return(NULL)

  summary_df <- summary_df %>%
    semi_join(grants_df %>% distinct(export_unitid), by = "export_unitid")

  if (nrow(summary_df) == 0) return(NULL)

  agencies <- c("nih", "nsf", "epa", "samhsa", "cdc")
  agency_labels <- c(
    nih = "NIH",
    nsf = "NSF",
    epa = "EPA",
    samhsa = "SAMHSA",
    cdc = "CDC"
  )

  schools <- lapply(split(summary_df, summary_df$export_unitid), function(df) {
    latest <- df %>% slice(1)
    school_grants <- grants_df %>%
      filter(export_unitid == latest$export_unitid[[1]]) %>%
      arrange(desc(termination_date), desc(award_remaining), agency, project_title)

    agency_summary <- lapply(agencies, function(agency) {
      agency_grants <- school_grants %>% filter(.data$agency == !!agency)
      list(
        agency = agency,
        agency_label = agency_labels[[agency]],
        disrupted_grants = nrow(agency_grants),
        disrupted_award_remaining = sum(agency_grants$award_remaining, na.rm = TRUE)
      )
    })

    latest_termination <- school_grants %>%
      filter(!is.na(termination_date), trimws(termination_date) != "") %>%
      slice_head(n = 1)

    list(
      unitid = as.character(latest$export_unitid[[1]]),
      financial_unitid = if (isTRUE(latest$has_financial_profile[[1]])) latest$matched_unitid[[1]] else NA_character_,
      has_financial_profile = isTRUE(latest$has_financial_profile[[1]]),
      is_primary_tracker = isTRUE(latest$is_primary_tracker[[1]]),
      likely_higher_ed = isTRUE(latest$likely_higher_ed[[1]]),
      institution_name = latest$display_name[[1]],
      city = or_null(latest$display_city),
      state = latest$display_state[[1]],
      control_label = or_null(latest$tracker_control_label),
      category = or_null(latest$tracker_category),
      latest_termination_date = or_null(latest_termination$termination_date),
      total_disrupted_grants = nrow(school_grants),
      total_disrupted_award_remaining = sum(school_grants$award_remaining, na.rm = TRUE),
      agency_summary = agency_summary,
      grants = lapply(seq_len(nrow(school_grants)), function(i) {
        list(
          agency = or_null(school_grants$agency[i]),
          agency_label = agency_labels[[school_grants$agency[i]]] %||% toupper(as.character(school_grants$agency[i])),
          grant_id = or_null(school_grants$grant_id[i]),
          grant_id_core = or_null(school_grants$grant_id_core[i]),
          status = or_null(school_grants$status[i]),
          organization_name = or_null(school_grants$organization_name[i]),
          organization_city = or_null(school_grants$organization_city[i]),
          organization_state = or_null(school_grants$organization_state[i]),
          organization_type = or_null(school_grants$organization_type[i]),
          project_title = or_null(school_grants$project_title[i]),
          project_abstract = or_null(school_grants$project_abstract[i]),
          start_date = or_null(school_grants$start_date[i]),
          original_end_date = or_null(school_grants$original_end_date[i]),
          termination_date = or_null(school_grants$termination_date[i]),
          award_value = or_null(school_grants$award_value[i]),
          award_outlaid = or_null(school_grants$award_outlaid[i]),
          award_remaining = or_null(school_grants$award_remaining[i]),
          remaining_field = or_null(school_grants$remaining_field[i]),
          source_url = or_null(school_grants$source_url[i]),
          detail_url = or_null(school_grants$detail_url[i])
        )
      })
    )
  })

  list(
    generated_at = as.character(Sys.Date()),
    agencies = unname(agency_labels),
    schools = schools
  )
}

build_school_file <- function(df) {
  latest <- df %>% filter(year == max(year, na.rm = TRUE)) %>% slice(1)
  pct_international_all <- scale_ratio_to_pct(latest$pct_international_all[[1]])
  pct_international_undergraduate <- scale_ratio_to_pct(latest$pct_international_undergraduate[[1]])
  pct_international_graduate <- scale_ratio_to_pct(latest$pct_international_graduate[[1]])
  international_students_sentence <- if ("international_students_sentence" %in% names(latest)) {
    stored_sentence <- null_if_empty(latest$international_students_sentence[[1]])
    if (!is.na(stored_sentence) && grepl("^In \\d{4},", stored_sentence)) stored_sentence else build_international_students_sentence(
      latest$year[[1]],
      latest$pct_international_all[[1]],
      latest$pct_international_undergraduate[[1]],
      latest$pct_international_graduate[[1]]
    )
  } else {
    build_international_students_sentence(
      latest$year[[1]],
      latest$pct_international_all[[1]],
      latest$pct_international_undergraduate[[1]],
      latest$pct_international_graduate[[1]]
    )
  }
  sector_key <- latest$sector[[1]]
  sector_loan_benchmark <- if (!is.null(sector_key) && !is.na(sector_key) && sector_key %in% names(sector_loan_benchmarks)) {
    unname(sector_loan_benchmarks[[sector_key]])
  } else {
    NA_real_
  }
  sector_grad_share_benchmark <- if (!is.null(sector_key) && !is.na(sector_key) && sector_key %in% names(sector_grad_share_benchmarks)) {
    unname(sector_grad_share_benchmarks[[sector_key]])
  } else {
    NA_real_
  }
  sector_grad_plus_benchmark <- if (!is.null(sector_key) && !is.na(sector_key) && sector_key %in% names(sector_grad_plus_benchmarks)) {
    unname(sector_grad_plus_benchmarks[[sector_key]])
  } else {
    NA_real_
  }

  list(
    unitid = as.character(latest$unitid[[1]]),
    generated_at = as.character(Sys.Date()),
    profile = list(
      institution_name = latest$institution_name[[1]],
      institution_unique_name = build_institution_unique_name(
        latest$institution_name[[1]],
        latest$city[[1]],
        latest$state[[1]]
      ),
      state = latest$state[[1]],
      city = latest$city[[1]],
      control_label = latest$control_label[[1]],
      sector = latest$sector[[1]],
      category = latest$category[[1]],
      urbanization = latest$urbanization[[1]],
      religious_affiliation = latest$religious_affiliation[[1]],
      all_programs_distance_education = latest$all_programs_distance_education[[1]]
    ),
    summary = list(
      latest_year = latest$year[[1]],
      enrollment_pct_change_5yr = latest$enrollment_pct_change_5yr[[1]],
      enrollment_decline_last_3_of_5 = latest$enrollment_decline_last_3_of_5[[1]],
      revenue_pct_change_5yr = latest$revenue_pct_change_5yr[[1]],
      net_tuition_per_fte_change_5yr = latest$net_tuition_per_fte_change_5yr[[1]],
      staff_total_headcount_pct_change_5yr = latest$staff_total_headcount_pct_change_5yr[[1]],
      staff_instructional_headcount_pct_change_5yr = latest$staff_instructional_headcount_pct_change_5yr[[1]],
      students_per_instructional_staff_fte = or_null(latest$students_per_instructional_staff_fte),
      sector_median_students_per_instructional_staff_fte = or_null(latest$sector_median_students_per_instructional_staff_fte),
      ended_year_at_loss = latest$ended_year_at_loss[[1]],
      losses_last_3_of_5 = latest$losses_last_3_of_5[[1]],
      loss_years_last_10 = latest$loss_years_last_10[[1]],
      tuition_dependence_pct = latest$tuition_dependence_pct[[1]],
      sector_median_tuition_dependence_pct = latest$sector_median_tuition_dependence_pct[[1]],
      tuition_dependence_vs_sector_median_sentence = null_if_empty(latest$tuition_dependence_vs_sector_median_sentence[[1]]),
      share_grad_students = scale_ratio_to_pct(latest$share_grad_students[[1]]),
      sector_avg_share_grad_students = scale_ratio_to_pct(sector_grad_share_benchmark),
      research_expense = latest$research_expense[[1]],
      research_expense_per_fte = latest$research_expense_per_fte[[1]],
      research_expense_pct_core_expenses = scale_ratio_to_pct(latest$research_expense_pct_core_expenses[[1]]),
      sector_research_spending_n = latest$sector_research_spending_n[[1]],
      sector_research_spending_positive_n = latest$sector_research_spending_positive_n[[1]],
      sector_research_spending_reporting_share_pct = latest$sector_research_spending_reporting_share_pct[[1]],
      sector_median_research_expense_per_fte_positive = latest$sector_median_research_expense_per_fte_positive[[1]],
      pct_international_all = pct_international_all,
      pct_international_undergraduate = pct_international_undergraduate,
      pct_international_graduate = pct_international_graduate,
      international_student_count_change_5yr = latest$international_student_count_change_5yr[[1]],
      international_enrollment_pct_change_5yr = latest$international_enrollment_pct_change_5yr[[1]],
      international_students_sentence = international_students_sentence,
      federal_loan_pct_most_recent = latest$federal_loan_pct_most_recent[[1]],
      sector_avg_federal_loan_pct_most_recent = sector_loan_benchmark,
      grad_plus_recipients = latest$grad_plus_recipients[[1]],
      grad_plus_disbursements_amt = latest$grad_plus_disbursements_amt[[1]],
      grad_plus_disbursements_per_recipient = latest$grad_plus_disbursements_per_recipient[[1]],
      sector_median_grad_plus_disbursements_per_recipient = sector_grad_plus_benchmark,
      federal_grants_contracts_pell_adjusted_pct_core_revenue = scale_ratio_to_pct(latest$federal_grants_contracts_pell_adjusted_pct_core_revenue[[1]]),
      state_funding_pct_core_revenue = scale_ratio_to_pct(latest$state_funding_pct_core_revenue[[1]]),
      federal_grants_contracts_pell_adjusted_pct_change_5yr = latest$federal_grants_contracts_pell_adjusted_pct_change_5yr[[1]],
      state_funding_pct_change_5yr = latest$state_funding_pct_change_5yr[[1]],
      endowment_pct_change_5yr = latest$endowment_pct_change_5yr[[1]],
      endowment_spending_current_use_pct_core_revenue = latest$endowment_spending_current_use_pct_core_revenue[[1]],
      graduation_rate_6yr = latest$graduation_rate_6yr[[1]],
      median_earnings_10yr = latest$median_earnings_10yr[[1]],
      median_debt_completers = latest$median_debt_completers[[1]],
      scorecard_data_updated = null_if_empty(latest$scorecard_data_updated[[1]]),
      ipeds_graduation_rate_year = latest$ipeds_graduation_rate_year[[1]],
      ipeds_graduation_rate_label = null_if_empty(latest$ipeds_graduation_rate_label[[1]])
    ),
    series = list(
      revenue_total_adjusted = build_series(df, "revenue_total_adjusted"),
      expenses_total_adjusted = build_series(df, "expenses_total_adjusted"),
      net_tuition_per_fte_adjusted = build_series(df, "net_tuition_per_fte_adjusted"),
      enrollment_headcount_total = build_series(df, "enrollment_headcount_total"),
      enrollment_nonresident_total = build_series(df, "enrollment_nonresident_total"),
      enrollment_nonresident_undergrad = build_series(df, "enrollment_nonresident_undergrad"),
      enrollment_nonresident_graduate = build_series(df, "enrollment_nonresident_graduate"),
      staff_headcount_total = build_series(df, "staff_headcount_total"),
      staff_headcount_instructional = build_series(df, "staff_headcount_instructional"),
      endowment_value_adjusted = build_series(df, "endowment_value_adjusted"),
      endowment_spending_current_use = build_series(df, "endowment_spending_current_use"),
      endowment_spending_current_use_pct_core_revenue = build_series(df, "endowment_spending_current_use_pct_core_revenue"),
      federal_grants_contracts_pell_adjusted_adjusted = build_series(df, "federal_grants_contracts_pell_adjusted_adjusted"),
      state_funding_adjusted = build_series(df, "state_funding_adjusted")
    )
  )
}

# Load the canonical finance dataset that serves as the backbone for school
# JSON files and the sitewide download.
df <- readr::read_csv(
  input_path,
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)

require_local_file(
  closure_status_json_path,
  "closure status JSON",
  "Run `python ./scripts/import_closure_sheet.py` first."
)
require_local_file(
  hcm_json_path,
  "HCM JSON lookup",
  "Run `python ./scripts/build_hcm_level2.py` first."
)
require_local_file(
  federal_composite_json_path,
  "federal composite score JSON lookup",
  "Run `python ./scripts/build_federal_composite_scores.py` first."
)

numeric_cols <- c(
  "year","enrollment_pct_change_5yr","revenue_pct_change_5yr","net_tuition_per_fte_change_5yr",
  "staff_total_headcount_pct_change_5yr","staff_instructional_headcount_pct_change_5yr","loss_years_last_10",
  "students_per_instructional_staff_fte","sector_median_students_per_instructional_staff_fte",
  "tuition_dependence_pct","sector_median_tuition_dependence_pct","share_grad_students","research_expense","research_expense_per_fte",
  "research_expense_pct_core_expenses",
  "sector_research_spending_n","sector_research_spending_positive_n","sector_research_spending_reporting_share_pct","sector_median_research_expense_per_fte_positive","pct_international_all",
  "pct_international_undergraduate","pct_international_graduate","international_student_count_change_5yr",
  "international_enrollment_pct_change_5yr",
  "federal_loan_pct_most_recent","grad_plus_recipients","grad_plus_disbursements_amt","grad_plus_disbursements_per_recipient","federal_grants_contracts_pell_adjusted_pct_core_revenue",
  "state_funding_pct_core_revenue","federal_grants_contracts_pell_adjusted_pct_change_5yr",
  "state_funding_pct_change_5yr","endowment_pct_change_5yr","endowment_spending_current_use_pct_core_revenue","revenue_total_adjusted",
  "expenses_total_adjusted","net_tuition_per_fte_adjusted","enrollment_headcount_total",
  "enrollment_nonresident_total","enrollment_nonresident_undergrad","enrollment_nonresident_graduate",
  "staff_headcount_total","staff_headcount_instructional",
  "endowment_value_adjusted","endowment_spending_current_use","endowment_spending_current_use_pct_core_revenue","federal_grants_contracts_pell_adjusted_adjusted","state_funding_adjusted"
)

for (nm in intersect(numeric_cols, names(df))) {
  df[[nm]] <- to_num(df[[nm]])
}

validate_export_input(df)
validate_multi_year_web_input(df, input_path)

df <- df %>% arrange(unitid, year)
# Join the latest outcomes fields onto the finance dataframe so the finance
# page can render the three outcomes blocks from the same school JSON.
latest_year <- max(df$year, na.rm = TRUE)
latest_financial <- df %>% filter(year == latest_year)
outcomes_summary <- readr::read_csv(outcomes_summary_path, show_col_types = FALSE) %>%
  mutate(unitid = as.character(unitid))
outcomes_join_fields <- c(
  "unitid",
  "graduation_rate_6yr",
  "median_earnings_10yr",
  "median_debt_completers",
  "grad_plus_recipients",
  "grad_plus_disbursements_amt",
  "grad_plus_disbursements_per_recipient",
  "outcomes_data_available",
  "scorecard_data_updated",
  "grad_plus_data_updated",
  "ipeds_graduation_rate_year",
  "ipeds_graduation_rate_label"
)
for (nm in c(
  "grad_plus_recipients",
  "grad_plus_disbursements_amt",
  "grad_plus_disbursements_per_recipient",
  "grad_plus_data_updated"
)) {
  if (!(nm %in% names(outcomes_summary))) outcomes_summary[[nm]] <- NA
}
outcomes_cols_to_join <- setdiff(outcomes_join_fields, "unitid")
df <- df %>%
  dplyr::select(-dplyr::any_of(outcomes_cols_to_join)) %>%
  mutate(unitid = as.character(unitid)) %>%
  left_join(
    outcomes_summary %>% dplyr::select(dplyr::all_of(outcomes_join_fields)),
    by = "unitid"
  )
latest_financial <- latest_financial %>%
  dplyr::select(-dplyr::any_of(outcomes_cols_to_join)) %>%
  mutate(unitid = as.character(unitid)) %>%
  left_join(
    outcomes_summary %>% dplyr::select(dplyr::all_of(outcomes_join_fields)),
    by = "unitid"
  )
benchmark_specs <- list(
  sector_loan_benchmarks = list(
    value_col = "federal_loan_pct_most_recent",
    summarizer = function(x) mean(x, na.rm = TRUE)
  ),
  sector_grad_share_benchmarks = list(
    value_col = "share_grad_students",
    summarizer = function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  ),
  sector_grad_plus_benchmarks = list(
    value_col = "grad_plus_disbursements_per_recipient",
    summarizer = function(x) if (all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE)
  )
)
benchmark_values <- lapply(benchmark_specs, function(spec) {
  build_group_value_lookup(
    latest_financial,
    group_col = "sector",
    value_col = spec$value_col,
    summarizer = spec$summarizer
  )
})
sector_loan_benchmarks <- benchmark_values$sector_loan_benchmarks
sector_grad_share_benchmarks <- benchmark_values$sector_grad_share_benchmarks
sector_grad_plus_benchmarks <- benchmark_values$sector_grad_plus_benchmarks

schools_index <- latest_financial %>%
  transmute(
    unitid = as.character(unitid),
    institution_name = vapply(institution_name, normalize_display_institution_name, character(1)),
    institution_unique_name = vapply(
      seq_len(n()),
      function(i) build_institution_unique_name(institution_name[[i]], city[[i]], state[[i]]),
      character(1)
    ),
    state = state,
    city = city,
    control_label = control_label,
    category = category,
    urbanization = urbanization,
    religious_affiliation = religious_affiliation
  ) %>%
  arrange(institution_name)

metadata <- list(
  generated_at = as.character(Sys.Date()),
  latest_year = latest_year,
  title = "College Financial Health Tracker",
  dataset = "IPEDS Financial Health Canonical Dataset",
  methodology_note = "This website prototype uses the filtered public-facing canonical IPEDS dataset from the project repo.",
  files = list(
    schools_index = "data/schools_index.json",
    college_cuts_index = "data/college_cuts_index.json",
    accreditation_index = "data/accreditation_index.json",
    research_funding_index = "data/research_funding_index.json",
    schools = "data/schools/{unitid}.json",
    download = "data/downloads/full_dataset.csv",
    college_cuts = "data/college_cuts.json",
    accreditation = "data/accreditation.json",
    research_funding = "data/research_funding.json",
    closure_status = "data/closure_status_by_unitid.json",
    hcm2 = "data/hcm2_by_unitid.json",
    federal_composite_scores = "data/federal_composite_scores_by_unitid.json"
  )
)

write_json_file(schools_index, file.path(data_dir, "schools_index.json"))
# Write the site metadata first, then each section export and the school-level
# JSON files consumed by the static frontend.
write_json_file(metadata, file.path(data_dir, "metadata.json"))
readr::write_csv(latest_financial, file.path(downloads_dir, "full_dataset.csv"), na = "")

export_bundle_specs <- list(
  cuts = list(
    builder = build_cuts_export,
    export_filename = "college_cuts.json",
    index_filename = "college_cuts_index.json",
    index_builder = function(school) list(
      latest_cut_date = school$latest_cut_date,
      latest_cut_label = school$latest_cut_label,
      cut_count = school$cut_count,
      landing_cuts = lapply(school$cuts %||% list(), function(cut) list(
        announcement_date = cut$announcement_date,
        announcement_year = cut$announcement_year,
        program_name = cut$program_name,
        positions_affected = cut$positions_affected,
        faculty_affected = cut$faculty_affected,
        source_url = cut$source_url
      ))
    )
  ),
  accreditation = list(
    builder = build_accreditation_export,
    export_filename = "accreditation.json",
    index_filename = "accreditation_index.json",
    index_builder = function(school) {
      latest_action_label <- if (!is.null(school$actions) && length(school$actions) > 0) {
        school$actions[[1]]$action_label_short %||% school$actions[[1]]$action_label
      } else {
        school$latest_status$action_labels
      }
      list(
        latest_action_date = school$latest_status$latest_action_date,
        latest_action_label = latest_action_label,
        action_count = school$latest_status$action_count,
        landing_actions = lapply(school$actions %||% list(), function(action) list(
          accreditor = action$accreditor,
          action_label = action$action_label,
          action_label_short = action$action_label_short,
          action_scope = action$action_scope,
          action_type = action$action_type,
          action_date = action$action_date,
          action_year = action$action_year,
          display_action = action$display_action,
          notes = action$notes,
          source_url = action$source_url %||% action$source_page_url,
          source_page_url = action$source_page_url,
          source_title = action$source_title
        ))
      )
    }
  ),
  research = list(
    builder = build_research_export,
    export_filename = "research_funding.json",
    index_filename = "research_funding_index.json",
    index_builder = function(school) list(
      likely_higher_ed = school$likely_higher_ed,
      latest_termination_date = school$latest_termination_date,
      total_disrupted_grants = school$total_disrupted_grants,
      total_disrupted_award_remaining = school$total_disrupted_award_remaining
    )
  )
)
export_paths <- write_export_bundles(export_bundle_specs, data_dir)
cuts_paths <- export_paths$cuts
accreditation_paths <- export_paths$accreditation
research_paths <- export_paths$research

# ── H1: Slug-ID stability diagnostic ─────────────────────────────────────────
# Count how many exported school IDs are name-based slugs (unmatched, potentially
# unstable) vs. numeric IPEDS unitids (matched, stable).  Reads from the small
# per-section index files so we don't have to parse the full export JSON.
# Logged each run so regressions — e.g. a batch of previously-matched schools
# losing their unitid — are immediately visible in CI logs.
{
  all_export_ids <- character(0)
  for (.ep in list(cuts_paths, accreditation_paths, research_paths)) {
    if (!is.null(.ep) && file.exists(.ep$index_path)) {
      .idx <- tryCatch(
        jsonlite::fromJSON(.ep$index_path, simplifyVector = FALSE),
        error = function(e) NULL
      )
      if (!is.null(.idx)) all_export_ids <- c(all_export_ids, names(.idx))
    }
  }
  n_slug_ids   <- sum(grepl("^(cut|accred|research)-", all_export_ids))
  n_unitid_ids <- sum(grepl("^[0-9]+$",               all_export_ids))
  n_other_ids  <- length(all_export_ids) - n_slug_ids - n_unitid_ids
  message(sprintf(
    "Export ID stability: %d numeric unitid IDs (stable), %d name-slug IDs (unmatched / potentially unstable URL), %d other",
    n_unitid_ids, n_slug_ids, n_other_ids
  ))
  if (n_slug_ids > 0) {
    slug_examples <- head(all_export_ids[grepl("^(cut|accred|research)-", all_export_ids)], 5)
    message("  Sample slug IDs: ", paste(slug_examples, collapse = ", "))
    message("  These schools have no matched IPEDS unitid. Their public URLs may change if the API name changes.")
    message("  To stabilise: add a manual alias or Supabase unitid for each institution.")
  }
  rm(.ep, .idx, all_export_ids, n_slug_ids, n_unitid_ids, n_other_ids)
}

by_school <- df %>%
  dplyr::group_by(unitid) %>%
  dplyr::group_split(.keep = TRUE)
for (school_df in by_school) {
  unitid <- as.character(school_df$unitid[[1]])
  school_json <- build_school_file(school_df)
  write_json_file(school_json, file.path(schools_dir, paste0(unitid, ".json")))
}

cat(sprintf("Saved schools index to %s\n", file.path(data_dir, "schools_index.json")))
cat(sprintf("Saved metadata to %s\n", file.path(data_dir, "metadata.json")))
cat(sprintf("Saved school files to %s\n", schools_dir))
cat(sprintf("Saved download CSV to %s\n", file.path(downloads_dir, "full_dataset.csv")))
if (!is.null(cuts_paths)) cat(sprintf("Saved college cuts export to %s\n", cuts_paths$export_path))
if (!is.null(accreditation_paths)) cat(sprintf("Saved accreditation export to %s\n", accreditation_paths$export_path))
if (!is.null(research_paths)) cat(sprintf("Saved research funding export to %s\n", research_paths$export_path))
if (!is.null(cuts_paths)) cat(sprintf("Saved college cuts index to %s\n", cuts_paths$index_path))
if (!is.null(accreditation_paths)) cat(sprintf("Saved accreditation index to %s\n", accreditation_paths$index_path))
if (!is.null(research_paths)) cat(sprintf("Saved research funding index to %s\n", research_paths$index_path))

# ── Post-write JSON validation ────────────────────────────────────────────────
# Re-read every output file and confirm it is valid JSON with the expected
# top-level keys and non-zero record counts.  Any failure stops the script so a
# broken export is never silently committed to the repo.
validate_json_output <- function(path, required_keys = character(0),
                                  min_length = 0L, schools_min = 0L) {
  if (!file.exists(path)) {
    stop("Expected output file is missing: ", path, call. = FALSE)
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) {
      stop("Invalid JSON in ", path, ": ", conditionMessage(e), call. = FALSE)
    }
  )
  if (min_length > 0L && length(parsed) < min_length) {
    stop(
      "JSON array in ", path, " has ", length(parsed),
      " entries (expected >= ", min_length, ")", call. = FALSE
    )
  }
  for (key in required_keys) {
    if (!key %in% names(parsed)) {
      stop("Missing required key '", key, "' in ", path, call. = FALSE)
    }
  }
  if (schools_min > 0L) {
    n_schools <- length(parsed[["schools"]])
    if (n_schools < schools_min) {
      stop(
        "'schools' count in ", path, " is ", n_schools,
        " (expected >= ", schools_min, ")", call. = FALSE
      )
    }
  }
  invisible(parsed)
}

cat("\nValidating JSON outputs...\n")

validate_json_output(
  file.path(data_dir, "schools_index.json"),
  min_length = 1L
)
validate_json_output(
  file.path(data_dir, "metadata.json"),
  required_keys = c("generated_at", "title", "files")
)
if (!is.null(cuts_paths)) {
  validate_json_output(
    cuts_paths$export_path,
    required_keys = c("generated_at", "schools"),
    schools_min = 1L
  )
  validate_json_output(cuts_paths$index_path, min_length = 1L)
}
if (!is.null(accreditation_paths)) {
  validate_json_output(
    accreditation_paths$export_path,
    required_keys = c("generated_at", "schools"),
    schools_min = 1L
  )
  validate_json_output(accreditation_paths$index_path, min_length = 1L)
}
if (!is.null(research_paths)) {
  validate_json_output(
    research_paths$export_path,
    required_keys = c("generated_at", "schools"),
    schools_min = 1L
  )
  validate_json_output(research_paths$index_path, min_length = 1L)
}

n_school_files <- length(list.files(schools_dir, pattern = "\\.json$"))
if (n_school_files == 0L) {
  stop("No school JSON files were written to ", schools_dir, call. = FALSE)
}
cat(sprintf(
  "Validation passed: %d school files written, all aggregate exports are valid JSON.\n",
  n_school_files
))

# H8: deep schema validation — catches field renames and structural regressions
validate_all_export_schemas(data_dir)

invisible(TRUE)
}

if (sys.nframe() == 0) {
  main()
}
