run_test("DAPIP actions and audit pipeline fixture", function() {
  fixture_root <- tempfile("dapip-actions-fixture-")
  dir.create(fixture_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  dirs <- c(
    file.path(fixture_root, "scripts"),
    file.path(fixture_root, "scripts", "shared"),
    file.path(fixture_root, "data_pipelines"),
    file.path(fixture_root, "data_pipelines", "accreditation"),
    file.path(fixture_root, "data_pipelines", "accreditation", "cache")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  for (nm in c(
    "shared/utils.R",
    "shared/ipeds_paths.R",
    "shared/name_normalization.R",
    "shared/accreditation_helpers.R",
    "shared/dapip_helpers.R",
    "shared/export_helpers.R"
  )) {
    file.copy(
      file.path(root, "scripts", nm),
      file.path(fixture_root, "scripts", nm),
      overwrite = TRUE
    )
  }
  for (nm in c(
    "build_dapip_accreditation_actions.R",
    "build_dapip_vs_scraper_audit.R"
  )) {
    file.copy(
      file.path(root, "scripts", nm),
      file.path(fixture_root, "scripts", nm),
      overwrite = TRUE
    )
  }

  helper_lines <- readLines(file.path(fixture_root, "scripts", "shared", "dapip_helpers.R"), warn = FALSE)
  helper_override <- c(
    helper_lines,
    "",
    "dapip_fetch_action_code_lookup <- function(cache_dir, refresh = TRUE) {",
    "  tibble::tibble(",
    "    action_code = c('PW', 'HM', 'GP'),",
    "    action_description = c(",
    "      'Probation or Equivalent or a More Severe Status: Warning',",
    "      'Heightened Monitoring or Focused Review',",
    "      'Grant Substantive Change: Program'",
    "    ),",
    "    action_type_code = c(2L, 2L, 1L)",
    "  )",
    "}",
    "",
    "dapip_fetch_justification_code_lookup <- function(cache_dir, refresh = TRUE) {",
    "  tibble::tibble(",
    "    justification_code = c(6L, 9L),",
    "    justification_description = c(",
    "      'Additional oversight is required to ensure a resolution of compliance issues',",
    "      'Is in compliance with all of the agency\\'s accreditation standards'",
    "    )",
    "  )",
    "}",
    "",
    "dapip_fetch_institutional_records <- function(dapip_id, cache_dir, refresh = TRUE) {",
    "  list(list(",
    "    UnitId = dapip_id,",
    "    AgencyId = 7L,",
    "    AgencyName = 'Southern Association of Colleges and Schools, Commission on Colleges',",
    "    ProgramId = 1L,",
    "    ProgramName = NA_character_,",
    "    SequentialId = 1L,",
    "    AccreditationDate = '1927-01-01T00:00:00',",
    "    ActiveCD = 'A',",
    "    ExistingAction = NA_character_,",
    "    ReviewDate = '06/01/2030'",
    "  ))",
    "}",
    "",
    "dapip_fetch_action_rows <- function(unitid, agency_id, program_id, sequential_id, cache_dir, refresh = TRUE) {",
    "  list(",
    "    list(UnitId = unitid, AgencyId = agency_id, ProgramId = program_id, SequentialId = sequential_id, ActionId = 1L, ActionCode = 'PW', ActionDate = '2024-12-08T00:00:00', JustificationCode = 6L, JustificationOther = NA_character_, IsOngoing = TRUE, IsEnding = FALSE, ActionEndDate = NA_character_, FileId = 22474L),",
    "    list(UnitId = unitid, AgencyId = agency_id, ProgramId = program_id, SequentialId = sequential_id, ActionId = 2L, ActionCode = 'HM', ActionDate = '2025-12-07T00:00:00', JustificationCode = 6L, JustificationOther = NA_character_, IsOngoing = TRUE, IsEnding = FALSE, ActionEndDate = NA_character_, FileId = 28939L),",
    "    list(UnitId = unitid, AgencyId = agency_id, ProgramId = program_id, SequentialId = sequential_id, ActionId = 3L, ActionCode = 'GP', ActionDate = '2025-12-07T00:00:00', JustificationCode = 9L, JustificationOther = NA_character_, IsOngoing = FALSE, IsEnding = FALSE, ActionEndDate = NA_character_, FileId = 0L),",
    "    list(UnitId = unitid, AgencyId = agency_id, ProgramId = program_id, SequentialId = sequential_id, ActionId = 4L, ActionCode = 'PW', ActionDate = '2017-12-03T00:00:00', JustificationCode = 6L, JustificationOther = NA_character_, IsOngoing = TRUE, IsEnding = FALSE, ActionEndDate = NA_character_, FileId = 99999L)",
    "  )",
    "}",
    "",
    "dapip_fetch_action_file_payload <- function(unitid, agency_id, program_id, sequential_id, action_id, cache_dir, refresh = TRUE) {",
    "  list(",
    "    FileId = if (action_id == 1L) 22474L else 28939L,",
    "    FileType = 'Uploaded',",
    "    UploadedFile = list(FileId = if (action_id == 1L) 22474L else 28939L, FileName = paste0('file-', action_id, '.pdf'), FileType = 'application/pdf', FileData = 'JVBERi0xLjQKJcTl8uXrp/Og0MTGCjEgMCBvYmoKPDw+PgplbmRvYmoKdHJhaWxlcgo8PD4+CnN0YXJ0eHJlZgowCiUlRU9G'),",
    "    LinkedFile = list(FileId = NA, FileUrl = NA_character_)",
    "  )",
    "}",
    "",
    "dapip_cache_uploaded_file <- function(file_payload, cache_dir, dapip_id, action_id, file_id, refresh_files = FALSE) {",
    "  list(",
    "    pdf_path = file.path(cache_dir, 'pdfs', paste0(dapip_id, '_', action_id, '_', file_id, '.pdf')),",
    "    text_path = file.path(cache_dir, 'text', paste0(dapip_id, '_', action_id, '_', file_id, '.txt')),",
    "    file_name = paste0('file-', action_id, '.pdf'),",
    "    mime_type = 'application/pdf',",
    "    cache_status = 'fixture',",
    "    linked_file_url = NA_character_",
    "  )",
    "}",
    "",
    "dapip_extract_pdf_text <- function(pdf_path, text_path, refresh_files = FALSE) {",
    "  if (grepl('_1_22474', text_path, fixed = TRUE)) {",
    "    return(list(",
    "      text = 'The SACSCOC Board of Trustees denied reaffirmation of accreditation, continued accreditation, and continued the institution on Warning. For twelve months for failure to comply with Standards 8.2.a and 13.3. A Special Committee was not authorized to visit the institution.',",
    "      text_path = text_path,",
    "      status = 'fixture',",
    "      chars = 240L",
    "    ))",
    "  }",
    "  list(",
    "    text = 'Heightened monitoring or focused review is required due to unresolved compliance concerns.',",
    "    text_path = text_path,",
    "    status = 'fixture',",
    "    chars = 95L",
    "  )",
    "}"
  )
  writeLines(helper_override, file.path(fixture_root, "scripts", "shared", "dapip_helpers.R"), useBytes = TRUE)

  crosswalk_path <- file.path(fixture_root, "data_pipelines", "accreditation", "dapip_institution_map.csv")
  crosswalk_df <- data.frame(
    unitid = "232609",
    opeid = "00372000",
    opeid_normalized = "00372000",
    tracker_institution_name = "University of Lynchburg",
    tracker_state = "Virginia",
    tracker_city = "Lynchburg",
    dapip_id = 147730L,
    dapip_institution_name = "University of Lynchburg",
    dapip_state = "VA",
    match_method = "opeid",
    match_confidence = "high",
    manual_override_used = FALSE,
    match_notes = NA_character_,
    search_error = NA_character_,
    last_seen_at = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
  readr::write_csv(crosswalk_df, crosswalk_path, na = "")

  scraper_path <- file.path(fixture_root, "data_pipelines", "accreditation", "accreditation_tracker_actions_joined.csv")
  scraper_df <- data.frame(
    unitid = c("232609", "232609", "232609", "232609", "203368", "101435", "214777"),
    tracker_name = c("University of Lynchburg", "University of Lynchburg", "University of Lynchburg", "University of Lynchburg", "Spring Arbor University", "Talladega College", "Gwynedd Mercy University"),
    institution_name_raw = c("University of Lynchburg", "University of Lynchburg", "University of Lynchburg", "University of Lynchburg", "Spring Arbor University", "Talladega College", "Gwynedd Mercy University"),
    accreditor = c("Southern Association of Colleges and Schools, Commission on Colleges", "Southern Association of Colleges and Schools, Commission on Colleges", "MSCHE", "MSCHE", "Higher Learning Commission", "Southern Association of Colleges and Schools, Commission on Colleges", "MSCHE"),
    action_type = c("warning", "adverse_action", "other", "warning", "other", "notice", "adverse_action"),
    action_label_raw = c(
      "Denied reaffirmation, continued in accreditation, and placed on Warning for twelve months.",
      "Approved teach-out agreement with another institution.",
      "To acknowledge receipt of the self-study report.",
      "To warn the institution that its accreditation may be in jeopardy because of insufficient evidence that the institution is currently in compliance.",
      "Accepted the staff recommendation to require the institution to provide an interim report.",
      "Requested a Monitoring Report in twelve (12) months to provide required oversight and resolution of compliance issues.",
      "To note the supplemental information report, requested by the Commission action of June 6, 2025, is no longer required."
    ),
    action_date = c("2024-12-08", "2023-01-15", "2024-02-01", "2017-06-01", "2026-02-01", "2025-12-01", "2026-04-24"),
    source_url = c("https://example.org/warning", "https://example.org/teachout", "https://example.org/procedural", "https://example.org/old-warning", "https://example.org/hlc-interim", "https://example.org/talladega-monitoring", "https://example.org/msche-supplemental"),
    notes = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  readr::write_csv(scraper_df, scraper_path, na = "")

  setwd(fixture_root)
  env_actions <- new.env(parent = globalenv())
  sys.source(file.path(fixture_root, "scripts", "build_dapip_accreditation_actions.R"), envir = env_actions)
  env_actions$main(c(
    "--crosswalk-input", crosswalk_path,
    "--output-prefix", file.path(fixture_root, "data_pipelines", "accreditation", "dapip"),
    "--cache-dir", file.path(fixture_root, "data_pipelines", "accreditation", "cache", "dapip"),
    "--refresh", "false",
    "--refresh-files", "false"
  ))

  raw_path <- file.path(fixture_root, "data_pipelines", "accreditation", "dapip_action_rows_raw.csv")
  files_path <- file.path(fixture_root, "data_pipelines", "accreditation", "dapip_action_files.csv")
  filtered_path <- file.path(fixture_root, "data_pipelines", "accreditation", "dapip_action_rows_filtered.csv")
  raw_df <- readr::read_csv(raw_path, show_col_types = FALSE)
  files_df <- readr::read_csv(files_path, show_col_types = FALSE)
  filtered_df <- readr::read_csv(filtered_path, show_col_types = FALSE)

  assert_equal(nrow(raw_df), 3L)
  assert_equal(nrow(files_df), 2L)
  assert_equal(nrow(filtered_df), 2L)
  assert_true(any(filtered_df$action_code == "PW"), "Warning row should be kept.")
  assert_true(any(filtered_df$action_code == "HM"), "Monitoring row should be kept.")
  assert_true(!any(filtered_df$action_code == "GP"), "Routine grant-program row should be dropped.")
  warning_row <- filtered_df[filtered_df$action_code == "PW", , drop = FALSE]
  assert_true(grepl("denied reaffirmation", tolower(warning_row$action_label_raw[[1]])),
    "DAPIP filtered warning label should prefer extracted file text when available.")
  assert_equal(warning_row$parsed_reason_source[[1]], "pdf_body")
  assert_true(grepl("denied reaffirmation", tolower(warning_row$parsed_reason_snippet[[1]])))

  env_audit <- new.env(parent = globalenv())
  sys.source(file.path(fixture_root, "scripts", "build_dapip_vs_scraper_audit.R"), envir = env_audit)
  env_audit$main(c(
    "--dapip-input", filtered_path,
    "--raw-dapip-input", raw_path,
    "--scraper-input", scraper_path,
    "--output-prefix", file.path(fixture_root, "data_pipelines", "accreditation", "dapip"),
    "--date-tolerance-days", "45"
  ))

  audit_path <- file.path(fixture_root, "data_pipelines", "accreditation", "dapip_vs_scraper_audit.csv")
  coverage_path <- file.path(fixture_root, "data_pipelines", "accreditation", "dapip_code_coverage.csv")
  public_counts_path <- file.path(fixture_root, "data_pipelines", "accreditation", "dapip_public_table_policy_counts.csv")
  audit_df <- readr::read_csv(audit_path, show_col_types = FALSE)
  coverage_df <- readr::read_csv(coverage_path, show_col_types = FALSE)
  public_counts_df <- readr::read_csv(public_counts_path, show_col_types = FALSE)

  assert_true(any(audit_df$audit_result == "match"), "Audit should produce a matched warning row.")
  assert_true(any(audit_df$audit_result == "dapip_only"), "Audit should leave monitoring as DAPIP-only.")
  assert_true(sum(audit_df$audit_result == "scraper_only") >= 2L, "Audit should leave scraper-only rows unmatched.")
  warning_match <- audit_df[audit_df$audit_result == "match", , drop = FALSE]
  assert_equal(warning_match$public_table_strategy[[1]], "dapip_backed_keep")
  assert_equal(warning_match$public_display_reason_source[[1]], "pdf_body")
  assert_true(any(audit_df$public_table_strategy == "dapip_backed_keep"), "DAPIP-only monitoring row should be kept.")
  assert_true(any(audit_df$public_table_strategy == "scraper_backed_keep"), "Teach-out scraper row should remain scraper-backed.")
  assert_true(any(audit_df$public_table_strategy == "drop_from_public_table"), "Procedural scraper row should be dropped from the public-table policy.")
  assert_true(any(audit_df$scraper_public_reason == "outside_public_window"), "Pre-2019 scraper rows should be dropped at the audit policy layer.")
  spring_arbor_row <- audit_df[audit_df$institution_name == "Spring Arbor University", , drop = FALSE]
  assert_equal(spring_arbor_row$public_table_strategy[[1]], "drop_from_public_table")
  assert_equal(spring_arbor_row$scraper_public_reason[[1]], "hlc_interim_report_without_public_signal")
  talladega_row <- audit_df[audit_df$institution_name == "Talladega College", , drop = FALSE]
  assert_equal(talladega_row$public_table_strategy[[1]], "scraper_backed_keep")
  gwynedd_row <- audit_df[audit_df$institution_name == "Gwynedd Mercy University", , drop = FALSE]
  assert_equal(gwynedd_row$public_table_strategy[[1]], "drop_from_public_table")
  assert_equal(gwynedd_row$scraper_public_reason[[1]], "msche_procedural_admin")
  gp_row <- coverage_df[coverage_df$action_code == "GP", , drop = FALSE]
  assert_equal(gp_row$row_count[[1]], 1L)
  assert_equal(gp_row$kept_count[[1]], 0L)
  assert_true(any(public_counts_df$public_table_strategy == "drop_from_public_table"))
})
