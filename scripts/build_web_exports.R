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
source(file.path(getwd(), "scripts", "shared", "accreditation_helpers.R"))
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
dapip_filtered_actions_path <- file.path(root, "data_pipelines", "accreditation", "dapip_action_rows_filtered.csv")
dapip_public_audit_path <- file.path(root, "data_pipelines", "accreditation", "dapip_vs_scraper_audit.csv")
research_summary_path <- file.path(root, "data_pipelines", "grant_witness", "grant_witness_higher_ed_institution_summary.csv")
research_grants_path <- file.path(root, "data_pipelines", "grant_witness", "grant_witness_grant_level_joined.csv")
outcomes_summary_path <- file.path(root, "data_pipelines", "scorecard", "tracker_outcomes_joined.csv")
closure_status_json_path <- file.path(data_dir, "closure_status_by_unitid.json")
hcm_json_path <- file.path(data_dir, "hcm2_by_unitid.json")
federal_composite_json_path <- file.path(data_dir, "federal_composite_scores_by_unitid.json")

research_min_public_award_remaining <- 100


# Helper functions (require_local_file, ensure_columns, null_if_empty,
# write_json_file, build_series, etc.) are in scripts/shared/export_helpers.R

max_int_when <- function(values, mask) {
  if (!any(mask, na.rm = TRUE)) return(NA_integer_)
  suppressWarnings(max(as.integer(values[mask]), na.rm = TRUE))
}

normalize_neche_compaction_family <- function(public_action_family, action_type) {
  family <- tolower(trimws(as.character(public_action_family %||% "")))
  type <- tolower(trimws(as.character(action_type %||% "")))
  if (!nzchar(family)) family <- type
  dplyr::case_when(
    family %in% c("monitoring_or_notice", "notice") ~ "monitoring_or_notice",
    family == "warning" ~ "warning",
    family == "show_cause" ~ "show_cause",
    family == "probation" ~ "probation",
    family == "removed" | type == "removed" ~ "removed",
    family %in% c("withdrawal_or_loss", "institutional_change_or_closure", "adverse_action") ~ "adverse_action",
    TRUE ~ family
  )
}

build_neche_compaction_signature_text <- function(action_label_raw, action_label_short, notes) {
  trimws(paste(
    as.character(action_label_raw %||% ""),
    as.character(action_label_short %||% ""),
    as.character(notes %||% "")
  ))
}

compact_neche_public_actions <- function(df) {
  if (!nrow(df)) return(df)

  work <- df %>%
    mutate(
      neche_compaction_family = vapply(
        seq_len(n()),
        function(i) normalize_neche_compaction_family(public_action_family[[i]], action_type[[i]]),
        character(1)
      ),
      neche_compaction_strength = vapply(
        neche_compaction_family,
        get_accreditation_sanction_strength,
        integer(1)
      ),
      neche_raw_concern_signature = vapply(
        seq_len(n()),
        function(i) {
          if (toupper(trimws(as.character(accreditor[[i]] %||% ""))) != "NECHE") return(NA_character_)
          get_neche_concern_signature(build_neche_compaction_signature_text(
            action_label_raw[[i]],
            action_label_short[[i]],
            notes[[i]]
          ))
        },
        character(1)
      ),
      neche_signature_present = !is.na(neche_raw_concern_signature) & nzchar(neche_raw_concern_signature),
      neche_detail_text_length = vapply(
        seq_len(n()),
        function(i) nchar(
          build_neche_compaction_signature_text(
            action_label_raw[[i]],
            action_label_short[[i]],
            notes[[i]]
          ),
          type = "chars",
          allowNA = FALSE,
          keepNA = FALSE
        ),
        integer(1)
      )
    )

  keep <- rep(TRUE, nrow(work))
  updated_short <- work$action_label_short
  neche_rows <- which(toupper(trimws(as.character(work$accreditor %||% ""))) == "NECHE")
  group_keys <- paste(
    work$export_unitid[neche_rows],
    work$accreditor[neche_rows],
    work$neche_compaction_family[neche_rows],
    sep = "||"
  )

  for (group_indices in split(neche_rows, group_keys)) {
    if (length(group_indices) < 2L) next
    group_family <- unique(work$neche_compaction_family[group_indices])
    if (length(group_family) != 1L || identical(group_family[[1]], "removed")) next

    group_dates <- suppressWarnings(as.Date(work$action_date[group_indices]))
    raw_signatures <- work$neche_raw_concern_signature[group_indices]
    filled_signatures <- raw_signatures

    for (local_i in seq_along(group_indices)) {
      if (!is.na(filled_signatures[[local_i]]) && nzchar(filled_signatures[[local_i]])) next
      if (is.na(group_dates[[local_i]])) next
      nearby_with_signature <- which(
        !is.na(group_dates) &
          !is.na(raw_signatures) &
          nzchar(raw_signatures) &
          abs(as.integer(group_dates - group_dates[[local_i]])) <= 60L
      )
      unique_nearby_signatures <- unique(raw_signatures[nearby_with_signature])
      if (length(unique_nearby_signatures) == 1L) {
        filled_signatures[[local_i]] <- unique_nearby_signatures[[1]]
      }
    }

    local_order <- order(group_dates, seq_along(group_indices), decreasing = TRUE, na.last = NA)
    for (local_rep in local_order) {
      rep_global <- group_indices[[local_rep]]
      if (!keep[[rep_global]]) next
      if (is.na(group_dates[[local_rep]])) next
      rep_signature <- filled_signatures[[local_rep]]
      if (is.na(rep_signature) || !nzchar(rep_signature)) next

      cluster_local <- which(
        keep[group_indices] &
          !is.na(group_dates) &
          !is.na(filled_signatures) &
          nzchar(filled_signatures) &
          filled_signatures == rep_signature &
          abs(as.integer(group_dates - group_dates[[local_rep]])) <= 60L &
          work$neche_compaction_strength[group_indices] <= work$neche_compaction_strength[[rep_global]]
      )
      if (length(cluster_local) <= 1L) next

      representative_local <- cluster_local[tail(order(
        group_dates[cluster_local],
        cluster_local,
        na.last = NA
      ), 1)]
      best_detail_local <- cluster_local[tail(order(
        as.integer(work$neche_signature_present[group_indices][cluster_local]),
        group_dates[cluster_local],
        work$neche_detail_text_length[group_indices][cluster_local],
        cluster_local,
        na.last = NA
      ), 1)]

      representative_global <- group_indices[[representative_local]]
      best_detail_global <- group_indices[[best_detail_local]]
      best_short <- updated_short[[best_detail_global]] %||% work$action_label_short[[best_detail_global]]
      if (!is.na(best_short) && nzchar(trimws(as.character(best_short)))) {
        updated_short[[representative_global]] <- best_short
      }

      drop_local <- setdiff(cluster_local, representative_local)
      if (length(drop_local)) {
        keep[group_indices[drop_local]] <- FALSE
      }
    }
  }

  work$action_label_short <- updated_short
  work %>%
    filter(keep) %>%
    select(
      -neche_compaction_family,
      -neche_compaction_strength,
      -neche_raw_concern_signature,
      -neche_signature_present,
      -neche_detail_text_length
    )
}

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
  require_local_file(
    dapip_filtered_actions_path,
    "DAPIP filtered accreditation actions",
    "Run `Rscript --vanilla ./scripts/build_dapip_accreditation_actions.R` first."
  )
  require_local_file(
    dapip_public_audit_path,
    "DAPIP-vs-scraper accreditation audit",
    "Run `Rscript --vanilla ./scripts/build_dapip_vs_scraper_audit.R` first."
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
  PUBLIC_ACCREDITOR_CODES <- c("HLC", "MSCHE", "NECHE", "NWCCU", "SACSCOC", "WSCUC")
  TRUSTED_ACTION_TYPES <- c("adverse_action", "warning", "probation", "show_cause", "removed", "notice")
  HLC_ACTIONS_LANDING_URL_PATTERN <- "^https://www\\.hlcommission\\.org/for-students/accreditation-actions/?$"
  HLC_GENERIC_CURRENT_STATUS_LABELS <- c(
    "on notice",
    "on probation",
    "removal of sanction",
    "withdrawal of accreditation"
  )
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
    "to approve the teach-?out agreements?|approved teach-?out plan|approved teach-?out agreements?|",
    "voluntar(?:ily|y) surrender(?:ed)? accreditation|",
    "voluntary surrender of (?:its )?accreditation)"
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

  hlc_action_family <- function(action_type, action_label_raw) {
    type <- normalize_accreditation_text(action_type)
    label <- normalize_accreditation_text(action_label_raw)
    text <- trimws(paste(type, label))

    if (grepl("removal of sanction|removed from (warning|probation|notice)", text, ignore.case = TRUE, perl = TRUE) ||
        identical(type, "removed")) {
      return("removed")
    }
    if (grepl("withdrawal of accreditation|withdraw", text, ignore.case = TRUE, perl = TRUE)) {
      return("withdrawal")
    }
    if (grepl("\\bprobation\\b", text, ignore.case = TRUE, perl = TRUE) || identical(type, "probation")) {
      return("probation")
    }
    if (grepl("\\bnotice\\b", text, ignore.case = TRUE, perl = TRUE) || identical(type, "notice")) {
      return("notice")
    }
    NA_character_
  }

  is_hlc_generic_current_status_row <- function(accreditor, source_page_url, action_label_raw) {
    accreditor_code <- toupper(trimws(as.character(accreditor %||% "")))
    page_url <- trimws(as.character(source_page_url %||% ""))
    label <- normalize_accreditation_text(action_label_raw)

    identical(accreditor_code, "HLC") &&
      nzchar(page_url) &&
      grepl(HLC_ACTIONS_LANDING_URL_PATTERN, page_url, ignore.case = TRUE, perl = TRUE) &&
      label %in% HLC_GENERIC_CURRENT_STATUS_LABELS
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
      derive_action_label_short(action_type, action_label_raw, accreditor, notes)
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
    closure_action_pattern <- paste(
      "accepted notification of institutional closure|",
      "accepted voluntary withdrawal of accreditation|",
      "voluntar(?:ily|y) withdraw(?:al)? of accreditation|",
      "withdraw(?:al)? of accreditation|",
      "accept(?:ed)? teach-?out plan|",
      "approve(?:d)? (?:the )?(?:updated )?teach-?out (?:plan|agreement|agreements)|",
      "teach out plan|teach-?out plan|",
      "removed from membership|",
      "will transfer from .* to ",
      sep = ""
    )
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
  scraper_actions_df <- readr::read_csv(
    accreditation_actions_path,
    show_col_types = FALSE,
    col_types = actions_col_types
  ) %>%
    ensure_columns(list(
      institution_name_raw = NA_character_,
      accreditor = NA_character_,
      action_status = NA_character_,
      action_date = NA_character_,
      action_year = NA_character_,
      action_scope = NA_character_,
      tracker_name = NA_character_,
      tracker_state = NA_character_,
      tracker_city = NA_character_,
      tracker_control = NA_character_,
      tracker_category = NA_character_,
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
  dapip_actions_df <- readr::read_csv(
    dapip_filtered_actions_path,
    show_col_types = FALSE
  ) %>%
    ensure_columns(list(
      unitid = NA_character_,
      institution_name_raw = NA_character_,
      institution_state_raw = NA_character_,
      tracker_name = NA_character_,
      tracker_state = NA_character_,
      city = NA_character_,
      control_label = NA_character_,
      category = NA_character_,
      accreditor = NA_character_,
      action_type = NA_character_,
      action_label_raw = NA_character_,
      action_status = NA_character_,
      action_date = NA_character_,
      action_year = NA_character_,
      action_scope = NA_character_,
      notes = NA_character_,
      source_url = NA_character_,
      source_title = NA_character_,
      source_page_url = NA_character_,
      source_page_modified = NA_character_,
      file_id = NA_character_
    ))
  audit_df <- readr::read_csv(
    dapip_public_audit_path,
    show_col_types = FALSE
  )
  required_audit_columns <- c(
    "unitid",
    "institution_name",
    "accreditor",
    "public_table_strategy",
    "hybrid_candidate",
    "hybrid_reason",
    "scraper_source_key",
    "dapip_source_key",
    "scraper_action_type",
    "scraper_action_label",
    "scraper_action_date",
    "scraper_source_url",
    "dapip_action_type",
    "dapip_action_label",
    "dapip_action_date",
    "dapip_source_page_url",
    "dapip_file_id"
  )
  missing_audit_columns <- setdiff(required_audit_columns, names(audit_df))
  if (length(missing_audit_columns) > 0L) {
    stop(
      paste(
        "The DAPIP accreditation audit is missing required columns:",
        paste(missing_audit_columns, collapse = ", "),
        "Re-run `Rscript --vanilla ./scripts/build_dapip_vs_scraper_audit.R` before exporting."
      ),
      call. = FALSE
    )
  }
  audit_df <- audit_df %>%
    mutate(
      unitid = as.character(unitid),
      institution_name = as.character(institution_name),
      accreditor = normalize_accreditor_code(accreditor),
      scraper_source_key = build_accreditation_action_source_key(
        unitid = unitid,
        institution_name = institution_name,
        accreditor = accreditor,
        action_type = scraper_action_type,
        action_label = scraper_action_label,
        action_date = scraper_action_date,
        source_url = scraper_source_url
      ),
      dapip_source_key = build_accreditation_action_source_key(
        unitid = unitid,
        institution_name = institution_name,
        accreditor = accreditor,
        action_type = dapip_action_type,
        action_label = dapip_action_label,
        action_date = dapip_action_date,
        source_page_url = dapip_source_page_url,
        file_id = dapip_file_id
      )
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

  canonical_tracker_lookup <- latest_financial %>%
    transmute(
      unitid = as.character(unitid),
      canonical_institution_name = vapply(institution_name, normalize_display_institution_name, character(1)),
      canonical_state = state,
      canonical_city = city,
      canonical_control_label = control_label,
      canonical_category = category,
      canonical_norm_name = normalize_name_accreditation(institution_name),
      canonical_state_norm = normalize_accreditation_text(state)
    ) %>%
    distinct(unitid, .keep_all = TRUE)

  resolve_canonical_accreditation_unitid <- function(unitid, institution_name, state) {
    existing <- trimws(as.character(unitid %||% ""))
    if (nzchar(existing)) return(existing)

    norm_name <- trimws(as.character(normalize_name_accreditation(institution_name) %||% ""))
    norm_state <- normalize_accreditation_text(state)
    if (!nzchar(norm_name)) return("")

    candidates <- canonical_tracker_lookup
    if (nzchar(norm_state)) {
      candidates <- candidates %>% filter(canonical_state_norm == norm_state)
    }
    if (nrow(candidates) == 0L) return("")

    exact <- unique(candidates$unitid[candidates$canonical_norm_name == norm_name])
    if (length(exact) == 1L) return(exact[[1]])

    containment <- unique(candidates$unitid[
      startsWith(candidates$canonical_norm_name, norm_name) |
        startsWith(norm_name, candidates$canonical_norm_name)
    ])
    if (length(containment) == 1L) return(containment[[1]])

    ""
  }

  reconcile_accreditation_tracker_metadata <- function(df, accreditor_col = "accreditor") {
    if (nrow(df) == 0L) return(df)
    accreditor_values <- as.character(df[[accreditor_col]] %||% rep(NA_character_, nrow(df)))

    df <- df %>%
      mutate(
        unitid = as.character(unitid),
        export_institution_name = pick_first_present(pick(dplyr::everything()), c("institution_name", "tracker_name", "institution_name_raw")),
        export_state = pick_first_present(pick(dplyr::everything()), c("state", "tracker_state", "institution_state_raw")),
        export_city = pick_first_present(pick(dplyr::everything()), c("city", "tracker_city")),
        export_control_label = pick_first_present(pick(dplyr::everything()), c("control_label", "tracker_control")),
        export_category = pick_first_present(pick(dplyr::everything()), c("category", "tracker_category")),
        resolved_unitid = vapply(
          seq_len(n()),
          function(i) resolve_canonical_accreditation_unitid(
            unitid[[i]],
            export_institution_name[[i]],
            export_state[[i]]
          ),
          character(1)
        ),
        unitid = dplyr::coalesce(dplyr::na_if(unitid, ""), dplyr::na_if(resolved_unitid, ""))
      ) %>%
      select(-resolved_unitid) %>%
      left_join(
        canonical_tracker_lookup %>%
          select(
            unitid,
            canonical_institution_name,
            canonical_state,
            canonical_city,
            canonical_control_label,
            canonical_category
          ),
        by = "unitid"
      ) %>%
      mutate(
        export_institution_name = dplyr::coalesce(canonical_institution_name, export_institution_name),
        export_state = dplyr::coalesce(canonical_state, export_state),
        export_city = dplyr::coalesce(canonical_city, export_city),
        export_control_label = dplyr::coalesce(canonical_control_label, export_control_label),
        export_category = dplyr::coalesce(canonical_category, export_category),
        export_unitid = vapply(
          seq_len(n()),
          function(i) make_accreditation_export_id(
            unitid[[i]],
            export_institution_name[[i]],
            export_state[[i]],
            accreditor_values[[i]]
          ),
          character(1)
        ),
        has_financial_profile = !is.na(unitid) & unitid != "",
        is_primary_tracker = has_financial_profile & is_primary_bachelors_category(export_category)
      ) %>%
      select(-canonical_institution_name, -canonical_state, -canonical_city, -canonical_control_label, -canonical_category)

    df
  }

  summary_df <- reconcile_accreditation_tracker_metadata(summary_df, accreditor_col = "accreditors")
  scraper_actions_df <- scraper_actions_df %>%
    mutate(
      source_page_modified = dplyr::na_if(as.character(source_page_modified), ""),
      action_date = dplyr::if_else(
        (is.na(action_date) | trimws(as.character(action_date %||% "")) == "") &
          !is.na(action_status) & action_status == "active" & !is.na(source_page_modified),
        source_page_modified,
        as.character(action_date)
      ),
      action_year = dplyr::if_else(
        (is.na(action_year) | trimws(as.character(action_year %||% "")) == "") &
          !is.na(action_status) & action_status == "active" & !is.na(source_page_modified),
        substr(source_page_modified, 1L, 4L),
        as.character(action_year)
      ),
      accreditor_norm = normalize_accreditor_code(accreditor),
      scraper_source_key = build_accreditation_action_source_key(
        unitid = unitid,
        institution_name = dplyr::coalesce(tracker_name, institution_name_raw, institution_name),
        accreditor = accreditor_norm,
        action_type = action_type,
        action_label = action_label_raw,
        action_date = action_date,
        source_url = source_url,
        source_page_url = source_page_url
      )
    )
  scraper_actions_df <- reconcile_accreditation_tracker_metadata(scraper_actions_df, accreditor_col = "accreditor")
  dapip_actions_df <- dapip_actions_df %>%
    mutate(
      accreditor_norm = normalize_accreditor_code(accreditor),
      accreditor = accreditor_norm,
      dapip_source_key = build_accreditation_action_source_key(
        unitid = unitid,
        institution_name = dplyr::coalesce(tracker_name, institution_name_raw),
        accreditor = accreditor_norm,
        action_type = action_type,
        action_label = action_label_raw,
        action_date = action_date,
        source_url = source_url,
        source_page_url = source_page_url,
        file_id = file_id
      )
    )
  dapip_actions_df <- reconcile_accreditation_tracker_metadata(dapip_actions_df, accreditor_col = "accreditor")
  selected_scraper_audit <- audit_df %>%
    filter(public_table_strategy %in% c("scraper_backed_keep", "hybrid_keep")) %>%
    mutate(
      strategy_rank = dplyr::case_when(
        public_table_strategy == "hybrid_keep" ~ 1L,
        public_table_strategy == "scraper_backed_keep" ~ 2L,
        TRUE ~ 3L
      )
    ) %>%
    arrange(strategy_rank) %>%
    group_by(scraper_source_key) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(
      scraper_source_key = as.character(scraper_source_key),
      public_table_strategy = as.character(public_table_strategy),
      hybrid_candidate = as.logical(hybrid_candidate),
      hybrid_reason = as.character(hybrid_reason),
      public_action_family = as.character(public_action_family),
      preferred_scraper_action_type = as.character(scraper_action_type),
      preferred_scraper_action_label = as.character(scraper_action_label),
      preferred_scraper_action_date = as.character(scraper_action_date)
    ) %>%
    filter(!is.na(scraper_source_key), scraper_source_key != "")
  selected_dapip_audit <- audit_df %>%
    filter(public_table_strategy == "dapip_backed_keep") %>%
    group_by(dapip_source_key) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(
      dapip_source_key = as.character(dapip_source_key),
      public_table_strategy = as.character(public_table_strategy),
      hybrid_candidate = as.logical(hybrid_candidate),
      hybrid_reason = as.character(hybrid_reason),
      public_action_family = as.character(public_action_family)
    ) %>%
    filter(!is.na(dapip_source_key), dapip_source_key != "")
  public_scraper_actions <- selected_scraper_audit %>%
    left_join(scraper_actions_df, by = "scraper_source_key")
  if (nrow(public_scraper_actions) > 0L && any(is.na(public_scraper_actions$action_label_raw))) {
    stop(
      "Accreditation export join bug: one or more audited scraper-backed rows no longer match the scraper action source data. Re-run the DAPIP audit before exporting.",
      call. = FALSE
    )
  }
  public_scraper_actions <- public_scraper_actions %>%
    mutate(
      unitid = as.character(unitid),
      action_date = as.character(action_date),
      action_year = as.character(action_year),
      file_id = NA_character_,
      scraper_source_key = as.character(scraper_source_key),
      dapip_source_key = NA_character_
    )
  public_dapip_actions <- selected_dapip_audit %>%
    left_join(dapip_actions_df, by = "dapip_source_key")
  if (nrow(public_dapip_actions) > 0L && any(is.na(public_dapip_actions$action_label_raw))) {
    stop(
      "Accreditation export join bug: one or more audited DAPIP-backed rows no longer match the DAPIP filtered action data. Re-run the DAPIP action build and audit before exporting.",
      call. = FALSE
    )
  }
  public_dapip_actions <- public_dapip_actions %>%
    mutate(
      unitid = as.character(unitid),
      action_date = as.character(action_date),
      action_year = as.character(action_year),
      file_id = as.character(file_id),
      scraper_source_key = NA_character_,
      dapip_source_key = as.character(dapip_source_key)
    )
  sacscoc_supplemental_drop_actions <- audit_df %>%
    filter(
      public_table_strategy == "drop_from_public_table",
      normalize_accreditor_code(accreditor) == "SACSCOC",
      !is.na(dapip_source_key),
      dapip_source_key != ""
    ) %>%
    group_by(dapip_source_key) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(
      dapip_source_key = as.character(dapip_source_key),
      public_table_strategy = "dapip_backed_keep",
      hybrid_candidate = FALSE,
      hybrid_reason = as.character(hybrid_reason),
      public_action_family = as.character(public_action_family)
    ) %>%
    left_join(dapip_actions_df, by = "dapip_source_key") %>%
    {
      if (!"file_text_path" %in% names(.)) {
        .$file_text_path <- NA_character_
      }
      .
    } %>%
    reconcile_accreditation_tracker_metadata(accreditor_col = "accreditor") %>%
    mutate(
      unitid = as.character(unitid),
      action_date = as.character(action_date),
      action_year = as.character(action_year),
      file_id = as.character(file_id),
      scraper_source_key = NA_character_,
      dapip_source_key = as.character(dapip_source_key),
      action_summary_source_text = vapply(
        seq_len(n()),
        function(i) .select_action_summary_source_text(
          action_label_raw[[i]],
          file_text_path[[i]],
          action_type[[i]],
          normalize_accreditor_code(accreditor[[i]]),
          notes[[i]]
        ),
        character(1)
      ),
      action_label_short = vapply(
        seq_len(n()),
        function(i) derive_action_label_short(
          action_type[[i]],
          action_summary_source_text[[i]],
          normalize_accreditor_code(accreditor[[i]]),
          notes[[i]]
        ),
        character(1)
      )
    ) %>%
    filter(
      grepl(
        "^Requested Referral Report\\b|^Requested to Submit a Monitoring Report\\b|^No additional report requested\\b",
        trimws(as.character(action_label_short %||% "")),
        ignore.case = TRUE,
        perl = TRUE
      )
    ) %>%
    select(-action_summary_source_text, -action_label_short)
  if (nrow(sacscoc_supplemental_drop_actions) > 0L) {
    public_dapip_actions <- bind_rows(public_dapip_actions, sacscoc_supplemental_drop_actions)
  }
  sacscoc_supplemental_force_keep_keys <- unique(as.character(sacscoc_supplemental_drop_actions$dapip_source_key %||% character()))
  wscuc_dapip_enrichment_actions <- dapip_actions_df %>%
    filter(
      normalize_accreditor_code(accreditor) == "WSCUC",
      !is.na(action_label_raw)
    )
  if (nrow(wscuc_dapip_enrichment_actions) == 0L) {
    wscuc_dapip_enrichment_pool <- data.frame(
      unitid = character(),
      accreditor = character(),
      source_selection_period_key = character(),
      action_summary_source_text = character(),
      source_selection_specificity_score = integer(),
      source_selection_substantive_text_length = integer(),
      action_date = character(),
      stringsAsFactors = FALSE
    )
  } else {
    wscuc_dapip_enrichment_pool <- wscuc_dapip_enrichment_actions %>%
      mutate(
        unitid = as.character(unitid),
        action_date = normalize_accreditation_date(action_date),
        action_year = dplyr::if_else(
          (is.na(action_year) | trimws(as.character(action_year %||% "")) == "") & !is.na(action_date),
          substr(action_date, 1L, 4L),
          as.character(action_year)
        ),
        file_id = as.character(file_id)
      ) %>%
      reconcile_accreditation_tracker_metadata(accreditor_col = "accreditor") %>%
      {
        if (!"file_text_path" %in% names(.)) {
          .$file_text_path <- NA_character_
        }
        .
      } %>%
      mutate(
        accreditor = normalize_accreditor_code(accreditor),
        action_summary_source_text = vapply(
          seq_len(n()),
          function(i) .select_action_summary_source_text(
            action_label_raw[[i]],
            file_text_path[[i]],
            action_type[[i]],
            normalize_accreditor_code(accreditor[[i]]),
            notes[[i]]
          ),
          character(1)
        ),
        source_selection_specificity_score = vapply(
          seq_len(n()),
          function(i) get_action_summary_specificity_score(
            action_summary_source_text[[i]],
            accreditor[[i]]
          ),
          integer(1)
        ),
        source_selection_substantive_text_length = vapply(
          seq_len(n()),
          function(i) get_action_summary_substantive_text_length(
            action_summary_source_text[[i]],
            accreditor[[i]]
          ),
          integer(1)
        ),
        source_selection_period_key = substr(action_date, 1L, 7L)
      ) %>%
      filter(
        accreditor == "WSCUC",
        !is.na(unitid), unitid != "",
        !is.na(source_selection_period_key),
        nzchar(trimws(as.character(source_selection_period_key %||% "")))
      )
  }
  sacscoc_dapip_enrichment_actions <- dapip_actions_df %>%
    filter(
      normalize_accreditor_code(accreditor) == "SACSCOC",
      !is.na(action_label_raw)
    )
  if (nrow(sacscoc_dapip_enrichment_actions) == 0L) {
    sacscoc_dapip_enrichment_pool <- data.frame(
      unitid = character(),
      accreditor = character(),
      source_selection_period_key = character(),
      action_summary_source_text = character(),
      source_selection_specificity_score = integer(),
      source_selection_substantive_text_length = integer(),
      action_date = character(),
      stringsAsFactors = FALSE
    )
  } else {
    sacscoc_dapip_enrichment_pool <- sacscoc_dapip_enrichment_actions %>%
      mutate(
        unitid = as.character(unitid),
        action_date = normalize_accreditation_date(action_date),
        action_year = dplyr::if_else(
          (is.na(action_year) | trimws(as.character(action_year %||% "")) == "") & !is.na(action_date),
          substr(action_date, 1L, 4L),
          as.character(action_year)
        ),
        file_id = as.character(file_id)
      ) %>%
      reconcile_accreditation_tracker_metadata(accreditor_col = "accreditor") %>%
      {
        if (!"file_text_path" %in% names(.)) {
          .$file_text_path <- NA_character_
        }
        .
      } %>%
      mutate(
        accreditor = normalize_accreditor_code(accreditor),
        action_summary_source_text = vapply(
          seq_len(n()),
          function(i) .select_action_summary_source_text(
            action_label_raw[[i]],
            file_text_path[[i]],
            action_type[[i]],
            normalize_accreditor_code(accreditor[[i]]),
            notes[[i]]
          ),
          character(1)
        ),
        source_selection_specificity_score = vapply(
          seq_len(n()),
          function(i) get_action_summary_specificity_score(
            action_summary_source_text[[i]],
            accreditor[[i]]
          ),
          integer(1)
        ),
        source_selection_substantive_text_length = vapply(
          seq_len(n()),
          function(i) get_action_summary_substantive_text_length(
            action_summary_source_text[[i]],
            accreditor[[i]]
          ),
          integer(1)
        ),
        source_selection_period_key = substr(action_date, 1L, 7L)
      ) %>%
      filter(
        accreditor == "SACSCOC",
        !is.na(unitid), unitid != "",
        !is.na(source_selection_period_key),
        nzchar(trimws(as.character(source_selection_period_key %||% "")))
      )
  }
  actions_df <- bind_rows(
    public_scraper_actions %>%
      mutate(
        action_type = dplyr::coalesce(action_type, preferred_scraper_action_type),
        action_label_raw = dplyr::coalesce(action_label_raw, preferred_scraper_action_label),
        action_date = dplyr::coalesce(action_date, preferred_scraper_action_date),
        display_action = TRUE
      ),
    public_dapip_actions %>%
      mutate(display_action = TRUE)
  ) %>%
    mutate(
      unitid = as.character(unitid),
      action_date = normalize_accreditation_date(action_date),
      action_year = dplyr::if_else(
        (is.na(action_year) | trimws(as.character(action_year %||% "")) == "") & !is.na(action_date),
        substr(action_date, 1L, 4L),
        as.character(action_year)
      ),
      source_page_modified = na_if(as.character(source_page_modified), ""),
      display_action = TRUE,
      accreditors = dplyr::coalesce(accreditors, accreditor),
      latest_action_date = dplyr::coalesce(latest_action_date, action_date),
      latest_action_year = dplyr::coalesce(latest_action_year, action_year),
      action_labels = dplyr::coalesce(action_labels, action_label_raw),
      action_count = dplyr::coalesce(suppressWarnings(as.integer(action_count)), 1L),
      action_row_id = seq_len(n()),
      sacscoc_force_keep = !is.na(dapip_source_key) &
        dapip_source_key %in% sacscoc_supplemental_force_keep_keys
    ) %>%
    reconcile_accreditation_tracker_metadata(accreditor_col = "accreditor") %>%
    {
      if (!"file_text_path" %in% names(.)) {
        .$file_text_path <- NA_character_
      }
      .
    } %>%
    mutate(
      action_summary_source_text = vapply(
        seq_len(n()),
        function(i) .select_action_summary_source_text(
          action_label_raw[[i]],
          file_text_path[[i]],
          action_type[[i]],
          normalize_accreditor_code(accreditor[[i]]),
          notes[[i]]
        ),
        character(1)
      ),
      action_label_short = vapply(
        seq_len(n()),
        function(i) derive_action_label_short(
          action_type[[i]],
          action_summary_source_text[[i]],
          normalize_accreditor_code(accreditor[[i]]),
          notes[[i]]
        ),
        character(1)
      )
    ) %>%
    mutate(
      accreditor = normalize_accreditor_code(accreditor),
      source_selection_candidate_kind = dplyr::case_when(
        accreditor == "MSCHE" & public_table_strategy == "scraper_backed_keep" ~ "scraper",
        accreditor == "MSCHE" & public_table_strategy == "dapip_backed_keep" ~ "dapip",
        accreditor == "SACSCOC" & public_table_strategy == "scraper_backed_keep" ~ "scraper",
        accreditor == "SACSCOC" & public_table_strategy == "dapip_backed_keep" ~ "dapip",
        accreditor == "WSCUC" & public_table_strategy == "scraper_backed_keep" ~ "scraper",
        accreditor == "WSCUC" & public_table_strategy == "dapip_backed_keep" ~ "dapip",
        TRUE ~ NA_character_
      ),
      source_selection_specificity_score = dplyr::if_else(
        !is.na(source_selection_candidate_kind),
        vapply(
          seq_len(n()),
          function(i) get_action_summary_specificity_score(
            action_summary_source_text[[i]],
            accreditor[[i]]
          ),
          integer(1)
        ),
        NA_integer_
      ),
      source_selection_substantive_text_length = dplyr::if_else(
        !is.na(source_selection_candidate_kind),
        vapply(
          seq_len(n()),
          function(i) get_action_summary_substantive_text_length(
            action_summary_source_text[[i]],
            accreditor[[i]]
          ),
          integer(1)
        ),
        NA_integer_
      ),
      source_selection_source_rank = dplyr::case_when(
        source_selection_candidate_kind == "scraper" ~ 1L,
        source_selection_candidate_kind == "dapip" ~ 0L,
        TRUE ~ NA_integer_
      ),
      source_selection_period_key = dplyr::case_when(
        accreditor %in% c("WSCUC", "SACSCOC") & !is.na(action_date) ~ substr(action_date, 1L, 7L),
        TRUE ~ action_date
      )
    ) %>%
    filter(
      !(
        accreditor %in% c("SACSCOC", "NECHE") &
          trimws(as.character(action_label_short %||% "")) == "Heightened Monitoring or Focused Review"
      )
    ) %>%
    filter(
      !(
        accreditor == "SACSCOC" &
          trimws(as.character(action_label_short %||% "")) == "Removal of Monitoring Status"
      )
    ) %>%
    filter(
      !(
        accreditor == "SACSCOC" &
          trimws(as.character(action_label_short %||% "")) %in% c(
            "Requested Monitoring Report",
            "Requested to Submit a Monitoring Report"
          )
      )
    ) %>%
    filter(
      !(
        accreditor == "MSCHE" &
          grepl(
            paste(
              "^to reject the supplemental information report\\.?$",
              "^to reject the supplemental information report requested by the commission action[^.]*\\.?$",
              "^to note the supplemental information report(?:[^.]*)? is no longer required\\.?$",
              "^to note the supplemental information report due [^.]*\\.?$",
              sep = "|"
            ),
            tolower(trimws(as.character(action_label_short %||% ""))),
            perl = TRUE
          )
      )
    ) %>%
    filter(
      !(
        accreditor == "HLC" &
          identical(trimws(as.character(action_label_short %||% "")), "Required to provide an interim report")
      )
    ) %>%
    filter(
      !(
        accreditor == "SACSCOC" &
          grepl(
            "^since your institution has been continued on (probation|warning)",
            tolower(trimws(as.character(action_label_short %||% ""))),
            perl = TRUE
          )
      )
    ) %>%
    filter(
      !(
        accreditor == "SACSCOC" &
          grepl(
            "placed on probation for good cause by the sacscoc board of trustees in [a-z]+ \\d{4}\\.?$",
            tolower(trimws(as.character(action_label_short %||% ""))),
            perl = TRUE
          )
      )
    ) %>%
    filter(
      !(
        accreditor == "SACSCOC" &
          grepl(
            "^your institution'?s next reaffirmation",
            tolower(trimws(as.character(action_label_short %||% ""))),
            perl = TRUE
          )
      )
    ) %>%
    filter(
      !(
        accreditor == "SACSCOC" &
          action_type == "notice" &
          grepl(
            paste(
              "continued accreditation following (the )?review of membership at level",
              "continued accreditation following (the )?review of an off-?campus instructional site",
              "awarded membership at level",
              "approved the change in governance from",
              sep = "|"
            ),
            tolower(trimws(paste(action_label_short %||% "", action_label_raw %||% ""))),
            perl = TRUE
          )
      )
    ) %>%
    filter(
      !(
        accreditor == "SACSCOC" &
          !dplyr::coalesce(sacscoc_force_keep, FALSE) &
          grepl("reviewed .* monitoring (review|report)", tolower(trimws(as.character(action_label_raw %||% ""))), perl = TRUE) &
          !grepl(
            "requested (?:to submit )?(?:a|(?:first|second|third|fourth|fifth)\\s+)?monitoring report|requested referral report|no additional report was requested|placed the institution|continued .* on warning|continued .* on probation|removed from|warning|probation|show cause|good cause|additional oversight|resolution of compliance issues|denied reaffirmation",
            tolower(trimws(paste(action_label_short %||% "", action_label_raw %||% ""))),
            perl = TRUE
          )
      )
    ) %>%
    filter(
      !(
        accreditor == "SACSCOC" &
          !dplyr::coalesce(sacscoc_force_keep, FALSE) &
          vapply(
            seq_len(n()),
            function(i) is_sacscoc_public_table_row_to_drop(
              action_type[[i]],
              action_label_short[[i]],
              action_label_raw[[i]]
            ),
            logical(1)
          )
      )
    ) %>%
    filter(
      !grepl("\\b\\d+\\)\\s+in support of continued accreditation:", tolower(trimws(as.character(action_label_raw %||% ""))), perl = TRUE)
    ) %>%
    filter(accreditor %in% PUBLIC_ACCREDITOR_CODES) %>%
    filter(!is.na(export_unitid), export_unitid != "") %>%
    filter(vapply(seq_len(n()), function(i) public_action_has_occurred(action_year[[i]], action_date[[i]]), logical(1))) %>%
    group_by(export_unitid, accreditor, action_date) %>%
    mutate(
      has_msche_same_day_scraper_candidate = any(source_selection_candidate_kind == "scraper", na.rm = TRUE),
      has_msche_same_day_dapip_candidate = any(source_selection_candidate_kind == "dapip", na.rm = TRUE),
      has_msche_same_day_source_competition = accreditor == "MSCHE" &
        has_msche_same_day_scraper_candidate &
        has_msche_same_day_dapip_candidate,
      best_same_day_specificity_score = max_int_when(
        source_selection_specificity_score,
        !is.na(source_selection_candidate_kind)
      ),
      best_same_day_substantive_text_length = max_int_when(
        source_selection_substantive_text_length,
        !is.na(source_selection_candidate_kind) &
          source_selection_specificity_score == best_same_day_specificity_score
      ),
      best_same_day_source_rank = max_int_when(
        source_selection_source_rank,
        !is.na(source_selection_candidate_kind) &
          source_selection_specificity_score == best_same_day_specificity_score &
          source_selection_substantive_text_length == best_same_day_substantive_text_length
      ),
      drop_msche_same_day_source_competitor = !is.na(source_selection_candidate_kind) &
        has_msche_same_day_source_competition &
        (
          source_selection_specificity_score < best_same_day_specificity_score |
            (
              source_selection_specificity_score == best_same_day_specificity_score &
                source_selection_substantive_text_length < best_same_day_substantive_text_length
            ) |
            (
              source_selection_specificity_score == best_same_day_specificity_score &
                source_selection_substantive_text_length == best_same_day_substantive_text_length &
                source_selection_source_rank < best_same_day_source_rank
            )
        )
    ) %>%
    ungroup() %>%
    filter(!drop_msche_same_day_source_competitor) %>%
    select(
      -has_msche_same_day_scraper_candidate,
      -has_msche_same_day_dapip_candidate,
      -has_msche_same_day_source_competition,
      -best_same_day_specificity_score,
      -best_same_day_substantive_text_length,
      -best_same_day_source_rank,
      -drop_msche_same_day_source_competitor
    ) %>%
    {
      actions_df <- .
      if (!nrow(wscuc_dapip_enrichment_pool)) {
        actions_df
      } else {
        wscuc_best <- wscuc_dapip_enrichment_pool %>%
          group_by(unitid, accreditor, source_selection_period_key) %>%
          arrange(
            dplyr::desc(source_selection_specificity_score),
            dplyr::desc(source_selection_substantive_text_length),
            dplyr::desc(action_date),
            .by_group = TRUE
          ) %>%
          slice(1) %>%
          ungroup() %>%
          select(
            unitid,
            accreditor,
            source_selection_period_key,
            best_wscuc_dapip_text = action_summary_source_text,
            best_wscuc_dapip_specificity_score = source_selection_specificity_score
          )

        actions_df %>%
          left_join(
            wscuc_best,
            by = c("unitid", "accreditor", "source_selection_period_key")
          ) %>%
          mutate(
            action_summary_source_text = dplyr::case_when(
              accreditor == "WSCUC" &
                source_selection_candidate_kind == "scraper" &
                !is.na(best_wscuc_dapip_specificity_score) &
                best_wscuc_dapip_specificity_score > source_selection_specificity_score ~
                  best_wscuc_dapip_text,
              TRUE ~ action_summary_source_text
            )
          ) %>%
          mutate(
            action_label_short = vapply(
              seq_len(n()),
              function(i) derive_action_label_short(
                action_type[[i]],
                action_summary_source_text[[i]],
                normalize_accreditor_code(accreditor[[i]]),
                notes[[i]]
              ),
              character(1)
            )
          ) %>%
          select(
            -best_wscuc_dapip_text,
            -best_wscuc_dapip_specificity_score
          )
      }
    } %>%
    {
      actions_df <- .
      if (!nrow(sacscoc_dapip_enrichment_pool)) {
        actions_df
      } else {
        sacscoc_best <- sacscoc_dapip_enrichment_pool %>%
          group_by(unitid, accreditor, source_selection_period_key) %>%
          arrange(
            dplyr::desc(source_selection_specificity_score),
            dplyr::desc(source_selection_substantive_text_length),
            dplyr::desc(action_date),
            .by_group = TRUE
          ) %>%
          slice(1) %>%
          ungroup() %>%
          select(
            unitid,
            accreditor,
            source_selection_period_key,
            best_sacscoc_dapip_text = action_summary_source_text,
            best_sacscoc_dapip_specificity_score = source_selection_specificity_score,
            best_sacscoc_dapip_substantive_text_length = source_selection_substantive_text_length
          )

        actions_df %>%
          left_join(
            sacscoc_best,
            by = c("unitid", "accreditor", "source_selection_period_key")
          ) %>%
          mutate(
            action_summary_source_text = dplyr::case_when(
              accreditor == "SACSCOC" &
                source_selection_candidate_kind == "scraper" &
                !is.na(best_sacscoc_dapip_specificity_score) &
                (
                  best_sacscoc_dapip_specificity_score > source_selection_specificity_score |
                    (
                      best_sacscoc_dapip_specificity_score == source_selection_specificity_score &
                        dplyr::coalesce(best_sacscoc_dapip_substantive_text_length, 0L) >
                        dplyr::coalesce(source_selection_substantive_text_length, 0L)
                    )
                ) ~
                  best_sacscoc_dapip_text,
              TRUE ~ action_summary_source_text
            )
          ) %>%
          mutate(
            action_label_short = vapply(
              seq_len(n()),
              function(i) derive_action_label_short(
                action_type[[i]],
                action_summary_source_text[[i]],
                normalize_accreditor_code(accreditor[[i]]),
                notes[[i]]
              ),
              character(1)
            )
          ) %>%
          select(
            -best_sacscoc_dapip_text,
            -best_sacscoc_dapip_specificity_score,
            -best_sacscoc_dapip_substantive_text_length
          )
      }
    } %>%
    group_by(export_unitid, accreditor, action_date) %>%
    mutate(
      has_same_day_explicit_sanction = any(action_type %in% c("warning", "probation", "removed", "show_cause", "adverse_action"), na.rm = TRUE),
      notice_duplicates_sanction = action_type == "notice" &
        has_same_day_explicit_sanction &
        grepl(
          "warning|probation|show cause|removed from|denied reaffirmation|placed the institution",
          tolower(trimws(paste(action_label_short %||% "", action_label_raw %||% ""))),
          perl = TRUE
        )
    ) %>%
    ungroup() %>%
    filter(!notice_duplicates_sanction) %>%
    select(
      -has_same_day_explicit_sanction,
      -notice_duplicates_sanction,
      -action_summary_source_text,
      -source_selection_candidate_kind,
      -source_selection_specificity_score,
      -source_selection_substantive_text_length,
      -source_selection_source_rank,
      -source_selection_period_key,
      -action_row_id,
      -sacscoc_force_keep
    ) %>%
    group_by(export_unitid, accreditor, action_date) %>%
    mutate(
      has_same_day_substantive_family = any(
        public_action_family %in% c("warning", "probation", "show_cause", "removed", "withdrawal_or_loss", "institutional_change_or_closure"),
        na.rm = TRUE
      ),
      drop_same_day_monitoring_family = public_action_family == "monitoring_or_notice" & has_same_day_substantive_family,
      has_same_day_specific_removed = any(
        public_action_family == "removed" &
          grepl(
            "warning removed|probation removed|removed from warning|removed from probation|removed from notice",
            tolower(trimws(paste(action_label_short %||% "", action_label_raw %||% ""))),
            perl = TRUE
          ),
        na.rm = TRUE
      ),
      drop_generic_removed = public_action_family == "removed" &
        has_same_day_specific_removed &
        grepl(
          "removal of monitoring status|removed from sanction",
          tolower(trimws(paste(action_label_short %||% "", action_label_raw %||% ""))),
          perl = TRUE
        ),
      has_same_day_specific_monitoring_removal = any(
        grepl(
          "probation removed|warning removed|removed from probation|removed from warning|removed from notice",
          tolower(trimws(paste(action_label_short %||% "", action_label_raw %||% ""))),
          perl = TRUE
        ),
        na.rm = TRUE
      ),
      drop_textual_monitoring_removal = grepl(
        "removal of monitoring status",
        tolower(trimws(paste(action_label_short %||% "", action_label_raw %||% ""))),
        perl = TRUE
      ) &
        has_same_day_specific_monitoring_removal &
        any(grepl("removal of monitoring status", tolower(trimws(paste(action_label_short %||% "", action_label_raw %||% ""))), perl = TRUE), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(!drop_same_day_monitoring_family, !drop_generic_removed, !drop_textual_monitoring_removal) %>%
    select(
      -has_same_day_substantive_family,
      -drop_same_day_monitoring_family,
      -has_same_day_specific_removed,
      -drop_generic_removed,
      -has_same_day_specific_monitoring_removal,
      -drop_textual_monitoring_removal
    ) %>%
    group_by(export_unitid, accreditor, action_year, public_action_family) %>%
    mutate(
      is_generic_dapip_code_label = grepl(
        "^Probation or Equivalent or a More Severe Status: (Warning|Probation|Show Cause)$",
        trimws(as.character(action_label_short %||% "")),
        ignore.case = TRUE,
        perl = TRUE
      ),
      is_generic_sanction_family_label = grepl(
        "^(Placed on Warning|Placed on Probation(?: for Good Cause)?|Continued on Warning|Continued on Probation(?: for Good Cause)?|Removed from Warning|Removed from Probation(?: for Good Cause)?|Required to Show Cause)$",
        trimws(as.character(action_label_short %||% "")),
        ignore.case = TRUE,
        perl = TRUE
      ),
      has_detailed_same_year_family = any(
        !is_generic_dapip_code_label &
          !is_generic_sanction_family_label &
          nzchar(trimws(as.character(action_label_short %||% ""))),
        na.rm = TRUE
      ),
      drop_generic_same_year_family = (is_generic_dapip_code_label | is_generic_sanction_family_label) & has_detailed_same_year_family
    ) %>%
    ungroup() %>%
    filter(!drop_generic_same_year_family) %>%
    select(-is_generic_dapip_code_label, -is_generic_sanction_family_label, -has_detailed_same_year_family, -drop_generic_same_year_family) %>%
    group_by(export_unitid, accreditor, action_year) %>%
    mutate(
      has_wscuc_same_year_show_cause_warning_outcome = accreditor == "WSCUC" &
        any(
          public_table_strategy == "dapip_backed_keep" &
            grepl(
              "^Removed Show Cause and issued a Warning\\b",
              trimws(as.character(action_label_short %||% "")),
              ignore.case = TRUE,
              perl = TRUE
            ),
          na.rm = TRUE
        ),
      drop_wscuc_special_visit_overlap = accreditor == "WSCUC" &
        public_table_strategy == "hybrid_keep" &
        has_wscuc_same_year_show_cause_warning_outcome &
        grepl(
          "remove an order to show cause",
          tolower(trimws(as.character(action_label_raw %||% ""))),
          perl = TRUE
        ) &
        grepl(
          "issue a warning",
          tolower(trimws(as.character(action_label_raw %||% ""))),
          perl = TRUE
        )
    ) %>%
    ungroup() %>%
    filter(!drop_wscuc_special_visit_overlap) %>%
    select(-has_wscuc_same_year_show_cause_warning_outcome, -drop_wscuc_special_visit_overlap) %>%
    mutate(
      sacscoc_sanction_outcome = dplyr::case_when(
        toupper(trimws(as.character(accreditor %||% ""))) != "SACSCOC" ~ NA_character_,
        grepl("\\bremoved from probation\\b|\\bprobation removed\\b", trimws(as.character(action_label_short %||% "")), ignore.case = TRUE, perl = TRUE) ~ "removed_probation",
        grepl("\\bremoved from warning\\b|\\bwarning removed\\b", trimws(as.character(action_label_short %||% "")), ignore.case = TRUE, perl = TRUE) ~ "removed_warning",
        grepl("\\bshow cause\\b", trimws(as.character(action_label_short %||% "")), ignore.case = TRUE, perl = TRUE) ~ "show_cause",
        grepl("\\bprobation\\b", trimws(as.character(action_label_short %||% "")), ignore.case = TRUE, perl = TRUE) ~ "probation",
        grepl("\\bwarning\\b", trimws(as.character(action_label_short %||% "")), ignore.case = TRUE, perl = TRUE) ~ "warning",
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(export_unitid, accreditor, action_year, sacscoc_sanction_outcome) %>%
    mutate(
      sacscoc_is_generic_outcome_label = toupper(trimws(as.character(accreditor %||% ""))) == "SACSCOC" &
        grepl(
          "^(Placed on Warning(?: for Good Cause)?|Placed on Probation(?: for Good Cause)?|Continued on Warning(?: for Good Cause)?|Continued on Probation(?: for Good Cause)?|Removed from Warning|Removed from Probation(?: for Good Cause)?|Required to Show Cause)$",
          trimws(as.character(action_label_short %||% "")),
          ignore.case = TRUE,
          perl = TRUE
        ),
      sacscoc_has_detailed_same_year_outcome = any(
        !sacscoc_is_generic_outcome_label &
          !is.na(sacscoc_sanction_outcome) &
          nzchar(trimws(as.character(action_label_short %||% ""))),
        na.rm = TRUE
      ),
      drop_generic_same_year_sacscoc_outcome = sacscoc_is_generic_outcome_label & sacscoc_has_detailed_same_year_outcome
    ) %>%
    ungroup() %>%
    filter(!drop_generic_same_year_sacscoc_outcome) %>%
    select(-sacscoc_sanction_outcome, -sacscoc_is_generic_outcome_label, -sacscoc_has_detailed_same_year_outcome, -drop_generic_same_year_sacscoc_outcome) %>%
    mutate(
      duplicate_source_locator = dplyr::coalesce(source_page_url, source_url),
      duplicate_action_text = tolower(gsub("[^a-z0-9]+", " ", trimws(paste(action_label_short %||% "", action_label_raw %||% "")))),
      duplicate_name_text = tolower(gsub("[^a-z0-9]+", " ", trimws(as.character(export_institution_name %||% ""))))
    ) %>%
    group_by(accreditor, action_date, duplicate_source_locator, duplicate_action_text) %>%
    mutate(
      has_same_source_name_collision = accreditor == "NWCCU" &
        source_title == "DAPIP Institutional Accreditation Action" &
        nzchar(trimws(as.character(duplicate_source_locator %||% ""))) &
        dplyr::n() > 1L,
      action_mentions_export_name = has_same_source_name_collision &
        nzchar(duplicate_name_text) &
        stringr::str_detect(duplicate_action_text, stringr::fixed(duplicate_name_text)),
      has_matching_named_row = any(action_mentions_export_name, na.rm = TRUE),
      drop_same_source_name_mismatch = has_same_source_name_collision &
        has_matching_named_row &
        !action_mentions_export_name
    ) %>%
    ungroup() %>%
    filter(!drop_same_source_name_mismatch) %>%
    select(
      -duplicate_source_locator,
      -duplicate_action_text,
      -duplicate_name_text,
      -has_same_source_name_collision,
      -action_mentions_export_name,
      -has_matching_named_row,
      -drop_same_source_name_mismatch
    ) %>%
    mutate(dedupe_source_locator = dplyr::coalesce(source_url, source_page_url)) %>%
    distinct(
      export_unitid,
      accreditor,
      action_type,
      action_label_raw,
      action_date,
      dedupe_source_locator,
      public_table_strategy,
      .keep_all = TRUE
    ) %>%
    select(-dedupe_source_locator) %>%
    arrange(action_date, action_year) %>%
    group_by(export_unitid, accreditor, action_label_short, action_year) %>%
    slice(1) %>%
    ungroup()

  actions_df <- actions_df %>%
    mutate(
      hlc_action_family = vapply(
        seq_len(n()),
        function(i) hlc_action_family(action_type[[i]], action_label_raw[[i]]),
        character(1)
      ),
      hlc_is_generic_current_status = vapply(
        seq_len(n()),
        function(i) is_hlc_generic_current_status_row(
          accreditor[[i]],
          source_page_url[[i]],
          action_label_raw[[i]]
        ),
        logical(1)
      )
    ) %>%
    dplyr::group_by(export_unitid, accreditor, hlc_action_family) %>%
    dplyr::mutate(
      hlc_has_detailed_same_family = any(
        toupper(trimws(as.character(accreditor %||% ""))) == "HLC" &
          !hlc_is_generic_current_status &
          !is.na(hlc_action_family) &
          nzchar(hlc_action_family)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!(hlc_is_generic_current_status & hlc_has_detailed_same_family)) %>%
    dplyr::select(-hlc_action_family, -hlc_is_generic_current_status, -hlc_has_detailed_same_family)

  actions_df <- actions_df %>%
    mutate(
      hlc_sanction_cycle_family = dplyr::case_when(
        toupper(trimws(as.character(accreditor %||% ""))) != "HLC" ~ NA_character_,
        grepl(
          "warning removed|removed from warning|removed from notice|placed on notice|placed on warning|\\bon notice\\b|\\bon warning\\b",
          tolower(trimws(paste(action_label_short %||% "", action_label_raw %||% "", notes %||% ""))),
          perl = TRUE
        ) ~ "warning_notice",
        grepl(
          "probation removed|removed from probation|placed on probation|\\bon probation\\b",
          tolower(trimws(paste(action_label_short %||% "", action_label_raw %||% "", notes %||% ""))),
          perl = TRUE
        ) ~ "probation",
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(export_unitid, accreditor, hlc_sanction_cycle_family) %>%
    mutate(
      hlc_latest_removal_date = {
        removal_dates <- action_date[action_type == "removed"]
        removal_dates <- removal_dates[!is.na(removal_dates) & nzchar(trimws(as.character(removal_dates)))]
        if (length(removal_dates)) max(removal_dates) else NA_character_
      },
      action_status = dplyr::case_when(
        toupper(trimws(as.character(accreditor %||% ""))) == "HLC" &
          !is.na(hlc_sanction_cycle_family) &
          action_type %in% c("warning", "notice", "probation") &
          !is.na(hlc_latest_removal_date) &
          !is.na(action_date) &
          action_date < hlc_latest_removal_date ~ "resolved",
        TRUE ~ action_status
      )
    ) %>%
    ungroup() %>%
    select(-hlc_sanction_cycle_family, -hlc_latest_removal_date)

  actions_df <- compact_neche_public_actions(actions_df)

  # Always include all accreditors the project actively tracks, even if the
  # scraper returned zero rows for one of them (e.g. NWCCU with no qualifying
  # 4-year bachelor's institutions under action right now).
  ALL_TRACKED_ACCREDITORS <- PUBLIC_ACCREDITOR_CODES
  covered_accreditors <- sort(unique(c(ALL_TRACKED_ACCREDITORS, actions_df$accreditor)))
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
        action_labels = or_null(collapse_unique_values(dplyr::coalesce(df$action_label_short, df$action_label_raw))),
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
          public_table_strategy = or_null(df$public_table_strategy[i]),
          hybrid_candidate = isTRUE(df$hybrid_candidate[i]),
          hybrid_reason = or_null(df$hybrid_reason[i]),
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
      unfunded_discount_rate = scale_ratio_to_pct(latest$discount_rate[[1]]),
      unfunded_discount_pct_change_5yr = latest$discount_pct_change_5yr[[1]],
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
      unfunded_discount_rate = build_series(df, "discount_rate", scale = 100),
      enrollment_headcount_total = build_series(df, "enrollment_headcount_total"),
      enrollment_headcount_undergrad = build_series(df, "enrollment_headcount_undergrad"),
      enrollment_headcount_graduate = build_series(df, "enrollment_headcount_graduate"),
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
  "tuition_dependence_pct","sector_median_tuition_dependence_pct","discount_rate","discount_pct_change_5yr","share_grad_students","research_expense","research_expense_per_fte",
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
          public_table_strategy = action$public_table_strategy,
          hybrid_candidate = action$hybrid_candidate,
          hybrid_reason = action$hybrid_reason,
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
