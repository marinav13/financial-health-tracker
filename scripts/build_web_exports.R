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
      announcement_year = as.character(announcement_year),
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

  normalize_accreditor_name <- function(x) {
    dplyr::case_when(
      is.na(x) ~ x,
      x == "WSCUC" ~ "WASC Senior College and University Commission",
      TRUE ~ x
    )
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
      accreditors = normalize_accreditor_name(accreditors),
      latest_action_date = na_if(as.character(latest_action_date), ""),
      latest_action_year = na_if(as.character(latest_action_year), "")
    )
  actions_df <- readr::read_csv(accreditation_actions_path, show_col_types = FALSE) %>%
    ensure_columns(list(
      accreditor = NA_character_,
      action_date = NA_character_,
      action_year = NA_character_,
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
      accreditor = normalize_accreditor_name(accreditor),
      action_date = na_if(as.character(action_date), ""),
      action_year = na_if(as.character(action_year), ""),
      source_page_modified = na_if(as.character(source_page_modified), ""),
      display_action = as.logical(display_action),
      accreditors = dplyr::coalesce(accreditors, accreditor),
      latest_action_date = dplyr::coalesce(latest_action_date, action_date),
      latest_action_year = dplyr::coalesce(latest_action_year, action_year),
      action_labels = dplyr::coalesce(action_labels, action_label_raw),
      action_count = dplyr::coalesce(suppressWarnings(as.integer(action_count)), 1L)
    )
  coverage_df <- if (file.exists(accreditation_coverage_path)) {
    readr::read_csv(accreditation_coverage_path, show_col_types = FALSE) %>%
      mutate(accreditor = normalize_accreditor_name(accreditor))
  } else {
    tibble::tibble()
  }

  make_accreditation_export_id <- function(unitid, institution_name, state, accreditor) {
    raw_unitid <- trimws(as.character(unitid %||% ""))
    if (!identical(raw_unitid, "")) return(raw_unitid)
    base <- paste(institution_name %||% "", state %||% "", accreditor %||% "", sep = " | ")
    normalized <- tolower(base)
    normalized <- gsub("[^a-z0-9]+", "-", normalized)
    normalized <- gsub("^-+|-+$", "", normalized)
    paste0("accred-", normalized)
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
    filter(display_action %in% TRUE)

  covered_accreditors <- sort(unique(c(summary_df$accreditors, actions_df$accreditor)))
  covered_accreditors <- covered_accreditors[!is.na(covered_accreditors) & covered_accreditors != ""]

  not_covered <- list(
    list(name = "ACCJC", url = "https://accjc.org/", note = "Not yet integrated into the tracker workflow.")
  )

  schools <- lapply(split(actions_df, actions_df$export_unitid), function(df) {
    df <- df %>% arrange(desc(action_date), desc(action_year))
    summary_row <- summary_df %>% filter(export_unitid == df$export_unitid[[1]]) %>% slice(1)
    latest <- if (nrow(summary_row) > 0) summary_row else df %>% slice(1)

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
        accreditors = or_null(latest$accreditors),
        action_labels = or_null(latest$action_labels),
        active_actions = or_null(latest$active_actions),
        has_active_warning = or_null(latest$has_active_warning),
        has_active_warning_or_notice = or_null(latest$has_active_warning_or_notice),
        has_active_adverse_action = or_null(latest$has_active_adverse_action),
        latest_action_date = or_null_date(pick_first_present(latest, c("latest_action_date", "action_date"))),
        latest_action_year = or_null(pick_first_present(latest, c("latest_action_year", "action_year"))),
        action_count = or_null(latest$action_count)
      ),
      actions = lapply(seq_len(nrow(df)), function(i) {
        list(
          accreditor = or_null(df$accreditor[i]),
          action_type = or_null(df$action_type[i]),
          action_label = or_null(df$action_label_raw[i]),
          action_status = or_null(df$action_status[i]),
          action_date = or_null_date(df$action_date[i]),
          action_year = or_null(df$action_year[i]),
          notes = or_null(df$notes[i]),
          source_url = or_null(df$source_url[i]),
          source_title = or_null(df$source_title[i]),
          source_page_url = or_null(df$source_page_url[i]),
          source_page_modified = or_null(df$source_page_modified[i]),
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
    filter(likely_higher_ed, total_disrupted_award_remaining > 0)

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
      currently_disrupted = as.character(currently_disrupted),
      likely_higher_ed = as.logical(likely_higher_ed)
    ) %>%
    filter(currently_disrupted == "TRUE")

  if (nrow(summary_df) == 0 || nrow(grants_df) == 0) return(NULL)

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
      grant_col <- paste0(agency, "_disrupted_grants")
      amount_col <- paste0(agency, "_disrupted_award_remaining")
      list(
        agency = agency,
        agency_label = agency_labels[[agency]],
        disrupted_grants = unname(latest[[grant_col]][[1]] %||% 0),
        disrupted_award_remaining = unname(latest[[amount_col]][[1]] %||% 0)
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
      total_disrupted_grants = unname(latest$total_disrupted_grants[[1]] %||% 0),
      total_disrupted_award_remaining = unname(latest$total_disrupted_award_remaining[[1]] %||% 0),
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

build_outcomes_export <- function() {
  # Outcomes are now used mainly as finance-page blocks, but this helper still
  # assembles the joined payload while the repo transitions off the old page.
  require_local_file(
    outcomes_summary_path,
    "scorecard and graduation-rate join",
    "Run `Rscript --vanilla ./scripts/build_outcomes_join.R` first."
  )

  outcomes <- readr::read_csv(outcomes_summary_path, show_col_types = FALSE) %>%
    mutate(
      unitid = as.character(unitid),
      has_financial_profile = TRUE,
      is_primary_tracker = is_primary_bachelors_category(category)
    ) %>%
    filter(is_primary_tracker)

  if (nrow(outcomes) == 0) return(NULL)

  schools <- lapply(seq_len(nrow(outcomes)), function(i) {
    row <- outcomes[i, , drop = FALSE]
    list(
      unitid = row$unitid[[1]],
      financial_unitid = row$unitid[[1]],
      has_financial_profile = TRUE,
      is_primary_tracker = isTRUE(row$is_primary_tracker[[1]]),
      institution_name = row$institution_name[[1]],
      city = row$city[[1]],
      state = row$state[[1]],
      control_label = row$control_label[[1]],
      category = row$category[[1]],
      urbanization = row$urbanization[[1]],
      graduation_rate_6yr = row$graduation_rate_6yr[[1]],
      median_earnings_10yr = row$median_earnings_10yr[[1]],
      median_debt_completers = row$median_debt_completers[[1]],
      outcomes_data_available = isTRUE(row$outcomes_data_available[[1]]),
      scorecard_data_updated = row$scorecard_data_updated[[1]],
      ipeds_graduation_rate_year = row$ipeds_graduation_rate_year[[1]],
      ipeds_graduation_rate_label = row$ipeds_graduation_rate_label[[1]]
    )
  })

  list(
    schools = stats::setNames(schools, vapply(schools, function(item) item$unitid, character(1)))
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

df <- df %>% arrange(unitid, year)
# Join the latest outcomes fields onto the finance dataframe so the finance
# page can render the three outcomes blocks from the same school JSON.
latest_2024 <- df %>% filter(year == 2024)
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
df <- df %>%
  mutate(unitid = as.character(unitid)) %>%
  left_join(
    outcomes_summary %>% dplyr::select(dplyr::all_of(outcomes_join_fields)),
    by = "unitid"
  )
latest_2024 <- latest_2024 %>%
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
    latest_2024,
    group_col = "sector",
    value_col = spec$value_col,
    summarizer = spec$summarizer
  )
})
sector_loan_benchmarks <- benchmark_values$sector_loan_benchmarks
sector_grad_share_benchmarks <- benchmark_values$sector_grad_share_benchmarks
sector_grad_plus_benchmarks <- benchmark_values$sector_grad_plus_benchmarks

schools_index <- latest_2024 %>%
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
readr::write_csv(latest_2024, file.path(downloads_dir, "full_dataset.csv"), na = "")

export_bundle_specs <- list(
  cuts = list(
    builder = build_cuts_export,
    export_filename = "college_cuts.json",
    index_filename = "college_cuts_index.json",
    index_builder = function(school) list(
      latest_cut_date = school$latest_cut_date,
      latest_cut_label = school$latest_cut_label,
      cut_count = school$cut_count
    )
  ),
  accreditation = list(
    builder = build_accreditation_export,
    export_filename = "accreditation.json",
    index_filename = "accreditation_index.json",
    index_builder = function(school) {
      latest_action_label <- if (!is.null(school$actions) && length(school$actions) > 0) {
        school$actions[[1]]$action_label
      } else {
        school$latest_status$action_labels
      }
      list(
        latest_action_date = school$latest_status$latest_action_date,
        latest_action_label = latest_action_label,
        action_count = school$latest_status$action_count
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

invisible(TRUE)
}

if (sys.nframe() == 0) {
  main()
}
