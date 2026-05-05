main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  source(file.path(getwd(), "scripts", "shared", "dapip_helpers.R"))
  source(file.path(getwd(), "scripts", "shared", "accreditation_helpers.R"))
  source(file.path(getwd(), "scripts", "shared", "export_helpers.R"))
  args <- parse_cli_args(cli_args)
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)

  ensure_packages(c("dplyr", "readr", "stringr"))

  dapip_input <- get_arg_value(
    "--dapip-input",
    file.path(getwd(), "data_pipelines", "accreditation", "dapip_action_rows_filtered.csv")
  )
  raw_dapip_input <- get_arg_value(
    "--raw-dapip-input",
    file.path(getwd(), "data_pipelines", "accreditation", "dapip_action_rows_raw.csv")
  )
  scraper_input <- get_arg_value(
    "--scraper-input",
    file.path(getwd(), "data_pipelines", "accreditation", "accreditation_tracker_actions_joined.csv")
  )
  output_prefix <- get_arg_value(
    "--output-prefix",
    file.path(getwd(), "data_pipelines", "accreditation", "dapip")
  )
  date_tolerance_days <- suppressWarnings(as.integer(get_arg_value("--date-tolerance-days", "45")))
  min_public_action_date <- as.Date(get_arg_value("--min-public-action-date", "2019-01-01"))

  require_existing_local_file(
    dapip_input,
    "DAPIP filtered actions input",
    "Run `Rscript --vanilla ./scripts/build_dapip_accreditation_actions.R` first."
  )
  require_existing_local_file(
    scraper_input,
    "scraper accreditation actions input",
    "Run `Rscript --vanilla ./scripts/build_accreditation_actions.R` first."
  )

  normalize_audit_accreditor <- normalize_accreditor_code

  normalize_audit_text <- function(x) {
    stringr::str_squish(tolower(trimws(as.character(x %||% ""))))
  }

  value_or_na <- function(x) {
    value <- trimws(as.character(x %||% ""))
    if (!nzchar(value)) return(NA_character_)
    value
  }

  normalize_audit_date_text <- function(x) {
    vapply(x, function(value) {
      if (length(value) == 0L || is.na(value)) return(NA_character_)
      text <- trimws(as.character(value))
      if (!nzchar(text)) return(NA_character_)
      if (grepl("^\\d{4}-\\d{2}-\\d{2}$", text, perl = TRUE)) return(text)
      if (grepl("^\\d{4}-\\d{2}$", text, perl = TRUE)) return(paste0(text, "-01"))
      parsed <- suppressWarnings(as.Date(text))
      if (!is.na(parsed)) return(as.character(parsed))
      text
    }, character(1), USE.NAMES = FALSE)
  }

  ensure_audit_policy_columns <- function(df) {
    defaults <- list(
      unitid = character(),
      institution_name = character(),
      accreditor = character(),
      scraper_action_type = character(),
      scraper_action_label = character(),
      scraper_action_date = character(),
      dapip_action_type = character(),
      dapip_action_label = character(),
      dapip_action_date = character(),
      audit_result = character(),
      date_delta_days = integer(),
      scraper_source_url = character(),
      dapip_source_page_url = character(),
      dapip_file_id = character(),
      scraper_source_key = character(),
      dapip_source_key = character(),
      notes = character(),
      scraper_public_keep = logical(),
      scraper_public_reason = character(),
      dapip_public_keep = logical(),
      dapip_public_reason = character(),
      public_table_strategy = character(),
      hybrid_candidate = logical(),
      hybrid_reason = character(),
      public_action_family = character(),
      scraper_display_reason_source = character(),
      dapip_display_reason_source = character(),
      public_display_reason_source = character()
    )
    for (nm in names(defaults)) {
      if (!nm %in% names(df)) {
        df[[nm]] <- rep(defaults[[nm]], length.out = nrow(df))
      }
    }
    df
  }

  INSTITUTIONAL_ACCREDITORS <- c(
    "HLC",
    "MSCHE",
    "NECHE",
    "SACSCOC",
    "WSCUC",
    "NWCCU"
  )

  scraper_action_family <- function(action_type, label) {
    at <- trimws(tolower(as.character(action_type %||% "")))
    lbl <- trimws(tolower(as.character(label %||% "")))
    dplyr::case_when(
      stringr::str_detect(lbl, "merge|merger|surviving institution|change of control|change in control|change of legal status|accreditation will cease|accreditation cease date|cease operations|institutional closure|closing") ~ "institutional_change_or_closure",
      at == "probation" ~ "probation",
      at == "warning" ~ "warning",
      at == "show_cause" ~ "show_cause",
      at == "notice" ~ "monitoring_or_notice",
      at == "removed" ~ "removed",
      at == "adverse_action" & stringr::str_detect(lbl, "withdraw|withdrawal|surrender|loss of accreditation|denied|closure|teach-out|terminate") ~ "withdrawal_or_loss",
      at == "adverse_action" ~ "adverse_action_other",
      TRUE ~ "other"
    )
  }

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
    "^\\s*to note the supplemental information report(?:[^.]*)? is no longer required\\.?$",
    "^\\s*to note the supplemental information report due [^.]*\\.?$",
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

  MSCHE_SUBSTANTIVE_KEEP_PATTERN <- "^\\s*(?:merger of|accepted teach-?out plan|to approve the (?:updated )?teach-?out plan(?! as required of candidate)|to approve the teach-?out agreements?|approved teach-?out plan|approved teach-?out agreements?|voluntarily surrendered accreditation|to accept [^.]{0,160}?voluntar(?:ily|y)\\s+surrender)"
  MSCHE_PROCEDURAL_CONTENT_PATTERNS <- c(
    "addition or change of primary accreditor to msche procedures",
    "candidate assessment report",
    "reasonable cause determination for multiple accreditation"
  )

  HLC_ROUTINE_OTHER_PATTERNS <- c(
    "^continued and reaffirmed the institution.?s accreditation\\.?$",
    "^continued the institution.?s accreditation\\.?$",
    "^accepted the team report\\.?$",
    "^accepted the team report and continued the institution.?s accreditation\\.?$",
    "^accepted the staff recommendation\\.?$",
    "^approved the institution.?s request to offer",
    "^approved the institution.?s request for a change in student body",
    "^approved the institution.?s request to participate",
    "^approved the institution.?s participation in the notification program",
    "^approved the institution.?s request to gain access to hlc.?s notification program",
    "^approved the institution.?s request to offer correspondence education courses\\.?$",
    "^approved the institution.?s request to offer distance education",
    "^approved the institution.?s request to offer the following educational programs",
    "^approved the institution.?s request to offer the following certificate",
    "^approved the institution.?s request to offer the bachelor",
    "^approved the institution.?s request to offer the associate",
    "^approved the institution.?s request to offer the master",
    "^approved the institution.?s request to offer the doctor",
    "^approved the institution.?s request to offer the following program",
    "^approved the institution.?s request to offer the following degree",
    "^approved the institution.?s request to offer the following course",
    "^approved the institution.?s request to offer the following additional location",
    "^approved the institution.?s request to offer additional locations",
    "^approved the institution.?s request to add",
    "^approved the institution.?s request to participate in the notification program for additional locations",
    "^approved the institution.?s request to gain access to hlc.?s notification program for additional locations",
    "^accepted the team report and reaffirmed accreditation\\.?$",
    "^continued and reaffirmed accreditation\\.?$"
  )

  GENERIC_DAPIP_LABEL_PATTERNS <- c(
    "^voluntary withdrawal received$",
    "^heightened monitoring or focused review$",
    "^loss of accreditation or preaccreditation: voluntary withdrawal$",
    "^removal of monitoring status$",
    "^removal of show cause status$",
    "^accreditation reaffirmed:\\s+warning removed$",
    "^accreditation reaffirmed:\\s+probation removed$",
    "^loss of accreditation or preaccreditation"
  )

  is_substantive_monitoring <- function(accreditor, action_type, label, notes = "") {
    acc <- normalize_audit_accreditor(accreditor)
    type <- normalize_audit_text(action_type)
    label_text <- normalize_audit_text(label)
    notes_text <- normalize_audit_text(notes)
    content <- normalize_audit_text(paste(label, notes))
    risk_markers <- "financial responsibility|financial resources|financial documents|control of finances|federal and state responsibilities|institutional resources|title iv|composite score|audit|out of compliance|failure to comply|special committee|good cause|show cause|warning|probation|criteria for accreditation|standard vi"
    routine_markers <- "substantive change|off-?campus instructional site|policy and procedures on substantive change|program approved|following action on reaffirmation of accreditation|reaffirmed accreditation"

    if (acc == "MSCHE") {
      return(list(keep = FALSE, reason = "msche_monitoring_procedural"))
    }

    if (acc == "SACSCOC" &&
        identical(label_text, "heightened monitoring or focused review") &&
        (!nzchar(notes_text) || identical(notes_text, "heightened monitoring or focused review"))) {
      return(list(keep = FALSE, reason = "sacscoc_generic_monitoring_label"))
    }

    if (acc == "SACSCOC" &&
        stringr::str_detect(content, "reviewed the institution.?s (first )?monitoring (review|report)") &&
        !stringr::str_detect(content, "requested a monitoring report|placed the institution|continued .* on warning|continued .* on probation|removed from|warning|probation|show cause|good cause|additional oversight|resolution of compliance issues|denied reaffirmation")) {
      return(list(keep = FALSE, reason = "sacscoc_monitoring_review_only"))
    }

    if (!stringr::str_detect(content, "monitor|focused review|referral report|notice of concern|special visit|good cause")) {
      return(list(keep = FALSE, reason = "no_monitoring_signal"))
    }

    if (stringr::str_detect(content, "heightened monitoring|focused review|removal of monitoring status|warning removed|probation removed|removed from monitoring")) {
      return(list(keep = TRUE, reason = "heightened_or_removed_monitoring"))
    }

    if (stringr::str_detect(content, "failure to comply|out of compliance|compliance issues|show cause|warning|probation|special committee|additional oversight|resolution of compliance issues|denied reaffirmation|good cause")) {
      return(list(keep = TRUE, reason = "compliance_risk_monitoring"))
    }

    if (stringr::str_detect(content, "monitoring report")) {
      if (stringr::str_detect(content, risk_markers)) {
        return(list(keep = TRUE, reason = "followup_monitoring_report"))
      }
      if (stringr::str_detect(content, routine_markers)) {
        return(list(keep = FALSE, reason = "routine_monitoring_report"))
      }
      return(list(keep = FALSE, reason = "routine_monitoring_report"))
    }

    if (type %in% c("monitoring", "notice") && stringr::str_detect(content, "notice of concern|special visit")) {
      return(list(keep = TRUE, reason = "serious_notice_or_special_visit"))
    }

    list(keep = FALSE, reason = "routine_monitoring_or_followup")
  }

  classify_public_signal <- function(accreditor, action_type, label, notes = "", action_date = NA) {
    acc <- normalize_audit_accreditor(accreditor)
    type <- normalize_audit_text(action_type)
    raw_label <- trimws(as.character(label %||% ""))
    label_text <- normalize_audit_text(label)
    notes_text <- normalize_audit_text(notes)
    content <- normalize_audit_text(paste(label, notes))

    if (!is.na(action_date) && action_date < min_public_action_date) {
      return(list(keep = FALSE, reason = "outside_public_window"))
    }

    if (!nzchar(content) && !nzchar(type)) {
      return(list(keep = FALSE, reason = "empty_row"))
    }

    if (!(acc %in% INSTITUTIONAL_ACCREDITORS)) {
      return(list(keep = FALSE, reason = "non_institutional_accreditor"))
    }

    if (stringr::str_detect(content, "not an adverse action|action to rescind a substantive change request is not an adverse action|decided not to implement the substantive change")) {
      return(list(keep = FALSE, reason = "rescinded_or_non_adverse_substantive_change"))
    }

    if (stringr::str_detect(content, "show cause report .* no longer required|no longer required to show cause|show cause .* no longer required")) {
      return(list(keep = TRUE, reason = "left_distress_stage"))
    }

    if (stringr::str_detect(content, "\\b\\d+\\)\\s+in support of continued accreditation:")) {
      return(list(keep = FALSE, reason = "appeal_support_bullet"))
    }

    if (acc == "SACSCOC" &&
        stringr::str_detect(content, "reviewed .* monitoring (review|report)") &&
        !stringr::str_detect(content, "requested a monitoring report|placed the institution|continued .* on warning|continued .* on probation|removed from|warning|probation|show cause|good cause|additional oversight|resolution of compliance issues|denied reaffirmation")) {
      return(list(keep = FALSE, reason = "sacscoc_monitoring_review_only"))
    }

    if (acc == "NECHE" &&
        stringr::str_detect(content, "\\bnotation\\b") &&
        stringr::str_detect(content, "danger of not meeting|may no longer meet")) {
      return(list(keep = TRUE, reason = "notation_risk_signal"))
    }

    if (acc == "SACSCOC" && type == "notice") {
      if (stringr::str_detect(content, "we appreciate your continued support of sacscoc")) {
        return(list(keep = FALSE, reason = "sacscoc_boilerplate_monitoring"))
      }
      if (stringr::str_detect(content, "if the institution fails to document compliance .* may begin a two-year monitoring period")) {
        return(list(keep = FALSE, reason = "sacscoc_future_monitoring_threat"))
      }
      if (stringr::str_detect(content, "did not see any recent audits|does not publish the most current accreditation statement|and monitoring the results of these")) {
        return(list(keep = FALSE, reason = "sacscoc_observational_followup"))
      }
      if (stringr::str_detect(content, "off-?campus instructional site|policy and procedures on substantive change|approval of the master of|program \\(approved")) {
        return(list(keep = FALSE, reason = "sacscoc_routine_program_or_site_followup"))
      }
      if (stringr::str_detect(content, "following action on reaffirmation of accreditation") &&
          !stringr::str_detect(content, "financial|resources|responsibility|audit|title iv|out of compliance|failure to comply|warning|probation|show cause|good cause")) {
        return(list(keep = FALSE, reason = "sacscoc_reaffirmation_monitoring_only"))
      }
      if (stringr::str_detect(content, "policy and procedures for reporting substantive change must continue to address all types of substantive change")) {
        return(list(keep = FALSE, reason = "sacscoc_substantive_change_followup"))
      }
      if (stringr::str_detect(content, "failure to document compliance .* (next review|time of this review).*(placed on a sanction|result in .* sanction)")) {
        return(list(keep = FALSE, reason = "sacscoc_future_sanction_warning"))
      }
      if (stringr::str_detect(content, "reaffirmed accreditation") &&
          stringr::str_detect(content, "monitoring report") &&
          !stringr::str_detect(content, "warning removed|probation removed|removed from|warning|probation|show cause|good cause|denied reaffirmation")) {
        return(list(keep = FALSE, reason = "sacscoc_reaffirmation_with_monitoring_only"))
      }
      if (identical(label_text, "heightened monitoring or focused review") &&
          (!nzchar(notes_text) || identical(notes_text, "heightened monitoring or focused review"))) {
        return(list(keep = FALSE, reason = "sacscoc_generic_monitoring_label"))
      }
      if (stringr::str_detect(content, "requested to submit a (first|second|third|fourth|fifth)?\\s*monitoring report") &&
          !stringr::str_detect(content, "standard\\s+[0-9]+|core requirement\\s+[0-9]+|financial|resources|responsibility|control of finances|federal and state responsibilities|student achievement|student outcomes|publication of accreditation status|additional oversight|resolution of compliance issues")) {
        return(list(keep = FALSE, reason = "sacscoc_generic_monitoring_request"))
      }
    }

    if (acc == "SACSCOC" &&
        stringr::str_detect(content, "teach-?out plan is required because the institution was placed on (probation|warning)")) {
      return(list(keep = FALSE, reason = "sacscoc_derivative_teachout_requirement"))
    }

    if (acc == "SACSCOC" && type == "removed" &&
        stringr::str_detect(content, "we appreciate your continued support of sacscoc")) {
      return(list(keep = FALSE, reason = "sacscoc_boilerplate_closing_line"))
    }

    if (stringr::str_detect(content, "to remind the institution of its requirement to show cause by|to remind the institution of the monitoring report due|to remind the institution that it has been warned|to remind the institution of the commission'?s action")) {
      return(list(keep = FALSE, reason = "reminder_of_existing_sanction"))
    }

    if (stringr::str_detect(content, "provide further evidence of published information regarding student achievement|published information regarding student achievement|annual institutional update prior to the self-study")) {
      return(list(keep = FALSE, reason = "published_information_followup"))
    }

    if (stringr::str_detect(content, "joint review committee on education in radiologic technology|jrcert|programmatic accreditor|specialized accreditor") &&
        !stringr::str_detect(content, "institutional closure|teach-?out|merger|surviving institution|accreditation will cease|withdraw accreditation|withdrawal of accreditation")) {
      return(list(keep = FALSE, reason = "programmatic_accreditor_spillover"))
    }

    if (acc == "MSCHE") {
      if (type == "monitoring") {
        return(list(keep = FALSE, reason = "msche_monitoring_procedural"))
      }
      has_explicit_keep <- stringr::str_detect(raw_label, stringr::regex(MSCHE_SUBSTANTIVE_KEEP_PATTERN, ignore_case = TRUE))
      has_merger_legal_status <- stringr::str_detect(raw_label, stringr::regex("^\\s*change of legal status", ignore_case = TRUE)) &&
        stringr::str_detect(raw_label, stringr::regex("merger|surviving institution|anticipated date of the transaction", ignore_case = TRUE))
      if (!has_explicit_keep && !has_merger_legal_status) {
        if (any(vapply(MSCHE_PROCEDURAL_DROP_PATTERNS, function(pattern) stringr::str_detect(raw_label, stringr::regex(pattern, ignore_case = TRUE)), logical(1)))) {
          return(list(keep = FALSE, reason = "msche_procedural_admin"))
        }
        if (any(vapply(MSCHE_PROCEDURAL_CONTENT_PATTERNS, function(pattern) stringr::str_detect(raw_label, stringr::regex(pattern, ignore_case = TRUE)), logical(1)))) {
          return(list(keep = FALSE, reason = "msche_procedural_content"))
        }
      }
    }

    if (acc == "HLC" && type == "other" &&
        any(vapply(HLC_ROUTINE_OTHER_PATTERNS, function(pattern) stringr::str_detect(raw_label, stringr::regex(pattern, ignore_case = TRUE)), logical(1)))) {
      return(list(keep = FALSE, reason = "hlc_routine_admin"))
    }

    if (acc == "HLC" && stringr::str_detect(content, "require the institution to provide an interim report")) {
      if (stringr::str_detect(content, "warning|probation|show cause|notice|out of compliance|noncompliance|deficien|financial|title iv|heightened cash monitoring|additional oversight|resolution of compliance issues|fails to meet agency standards|teach-?out")) {
        return(list(keep = TRUE, reason = "hlc_interim_report_with_public_signal"))
      }
      return(list(keep = FALSE, reason = "hlc_interim_report_without_public_signal"))
    }

    if (type == "other" && stringr::str_detect(content, "substantive change|program addition|additional location|notification program|distance education|correspondence education")) {
      if (!stringr::str_detect(content, "teach-?out|closure|withdraw|merge|consolidat|removed from|warning|probation|show cause|notice of concern")) {
        return(list(keep = FALSE, reason = "routine_program_or_substantive_change"))
      }
    }

    if (stringr::str_detect(content, "teach-?out|institutional closure|accepted notification of institutional closure|cease academic operations|cease operations|closing|removed from membership|voluntar(?:ily|y) surrender|voluntary withdrawal|withdrawal of accreditation|loss of accreditation|merge|merger|consolidat|surviving institution|change of control")) {
      return(list(keep = TRUE, reason = "closure_teachout_or_exit_signal"))
    }

    if (stringr::str_detect(content, "removed from (warning|probation|monitoring|show cause|notice of concern)|warning removed|probation removed|removal of sanction|removal of monitoring status|removal of show cause status")) {
      return(list(keep = TRUE, reason = "left_distress_stage"))
    }

    if (type %in% c("warning", "probation", "show_cause", "notice", "removed", "adverse_action")) {
      if (type == "notice" || stringr::str_detect(content, "monitor|focused review|referral report")) {
        monitoring <- is_substantive_monitoring(acc, type, label, notes)
        if (isTRUE(monitoring$keep)) {
          return(list(keep = TRUE, reason = monitoring$reason))
        }
        if (type == "notice") {
          return(list(keep = FALSE, reason = monitoring$reason))
        }
      }
      return(list(keep = TRUE, reason = "core_sanction_signal"))
    }

    if (stringr::str_detect(content, "warning|probation|show cause|notice of concern|order to show cause|impose probation|continue the notice of concern|issue a warning|issue a notice of concern|denied reaffirmation")) {
      return(list(keep = TRUE, reason = "label_indicates_sanction"))
    }

    monitoring <- is_substantive_monitoring(acc, type, label, notes)
    if (isTRUE(monitoring$keep)) {
      return(list(keep = TRUE, reason = monitoring$reason))
    }

    list(keep = FALSE, reason = "routine_or_non_reader_signal")
  }

  needs_hybrid_text <- function(dapip_label, scraper_label, dapip_file_id = NA_character_) {
    dlabel <- trimws(as.character(dapip_label %||% ""))
    slabel <- trimws(as.character(scraper_label %||% ""))
    if (!nzchar(dlabel) || !nzchar(slabel)) return(FALSE)

    generic_dapip <- any(vapply(
      GENERIC_DAPIP_LABEL_PATTERNS,
      function(pattern) stringr::str_detect(dlabel, stringr::regex(pattern, ignore_case = TRUE)),
      logical(1)
    ))
    scraper_has_detail <- stringr::str_detect(
      normalize_audit_text(slabel),
      "teach-?out|closure|cease|students|surviving institution|merge|merger|consolidat|failure to comply|standards|denied reaffirmation|good cause|removed from"
    ) || (nchar(slabel) > (nchar(dlabel) + 60))
    no_file <- !nzchar(trimws(as.character(dapip_file_id %||% ""))) || trimws(as.character(dapip_file_id %||% "")) == "0"

    (generic_dapip || no_file) && scraper_has_detail
  }

  recommend_public_table_strategy <- function(audit_result, scraper_keep, dapip_keep, scraper_label, dapip_label, dapip_file_id = NA_character_) {
    if (!isTRUE(scraper_keep) && !isTRUE(dapip_keep)) {
      return(list(strategy = "drop_from_public_table", hybrid = FALSE, hybrid_reason = NA_character_))
    }
    if (isTRUE(scraper_keep) && !isTRUE(dapip_keep)) {
      return(list(strategy = "scraper_backed_keep", hybrid = FALSE, hybrid_reason = NA_character_))
    }
    if (!isTRUE(scraper_keep) && isTRUE(dapip_keep)) {
      return(list(strategy = "dapip_backed_keep", hybrid = FALSE, hybrid_reason = NA_character_))
    }

    if (audit_result %in% c("match", "family_match_date_mismatch", "date_overlap_family_mismatch") &&
        needs_hybrid_text(dapip_label, scraper_label, dapip_file_id)) {
      return(list(strategy = "hybrid_keep", hybrid = TRUE, hybrid_reason = "scraper_detail_enriches_generic_or_fileless_dapip"))
    }

    list(strategy = "dapip_backed_keep", hybrid = FALSE, hybrid_reason = NA_character_)
  }

  dapip <- readr::read_csv(dapip_input, show_col_types = FALSE, progress = FALSE) |>
    dplyr::mutate(
      dapip_row_id = dplyr::row_number(),
      unitid = as.character(unitid),
      accreditor_norm = normalize_audit_accreditor(accreditor),
      dapip_source_key = build_accreditation_action_source_key(
        unitid = unitid,
        institution_name = institution_name_raw,
        accreditor = accreditor_norm,
        action_type = action_type,
        action_label = action_label_raw,
        action_date = action_date,
        source_url = source_url,
        source_page_url = source_page_url,
        file_id = file_id
      ),
      dapip_family = as.character(mapped_action_family %||% scraper_action_family(action_type, action_label_raw)),
      action_date = as.Date(action_date)
    )

  scraper <- readr::read_csv(scraper_input, show_col_types = FALSE, progress = FALSE) |>
    (\(df) {
      if (!"notes" %in% names(df)) df$notes <- NA_character_
      if (!"source_url" %in% names(df)) df$source_url <- NA_character_
      if (!"source_page_url" %in% names(df)) df$source_page_url <- NA_character_
      if (!"source_page_modified" %in% names(df)) df$source_page_modified <- NA_character_
      if (!"action_status" %in% names(df)) df$action_status <- NA_character_
      if (!"action_year" %in% names(df)) df$action_year <- NA_character_
      if (!"tracker_name" %in% names(df)) df$tracker_name <- NA_character_
      if (!"institution_name_raw" %in% names(df)) df$institution_name_raw <- NA_character_
      if (!"action_label_source" %in% names(df)) df$action_label_source <- NA_character_
      if (!"file_text_path" %in% names(df)) df$file_text_path <- NA_character_
      df
    })() |>
    dplyr::mutate(
      scraper_row_id = dplyr::row_number(),
      unitid = as.character(unitid),
      action_date = normalize_audit_date_text(action_date),
      source_page_modified = dplyr::na_if(as.character(source_page_modified), ""),
      action_date = dplyr::if_else(
        is.na(action_date) & !is.na(action_status) & action_status == "active" & !is.na(source_page_modified),
        source_page_modified,
        action_date
      ),
      action_year = dplyr::if_else(
        (is.na(action_year) | trimws(as.character(action_year %||% "")) == "") &
          !is.na(action_status) & action_status == "active" & !is.na(source_page_modified),
        substr(source_page_modified, 1L, 4L),
        as.character(action_year)
      ),
      accreditor_norm = normalize_audit_accreditor(accreditor),
      scraper_source_key = build_accreditation_action_source_key(
        unitid = unitid,
        institution_name = dplyr::coalesce(tracker_name, institution_name_raw),
        accreditor = accreditor_norm,
        action_type = action_type,
        action_label = action_label_raw,
        action_date = action_date,
        source_url = source_url,
        source_page_url = source_page_url
      ),
      scraper_family = scraper_action_family(action_type, action_label_raw),
      action_date = as.Date(action_date)
    )

  best_pairs <- list()
  used_scraper_ids <- integer()

  for (i in seq_len(nrow(dapip))) {
    drow <- dapip[i, , drop = FALSE]
    candidates <- scraper |>
      dplyr::filter(unitid == drow$unitid[[1]], accreditor_norm == drow$accreditor_norm[[1]]) |>
      dplyr::mutate(
        same_family = scraper_family == drow$dapip_family[[1]],
        date_delta_days = suppressWarnings(as.integer(abs(as.numeric(action_date - drow$action_date[[1]])))),
        within_tolerance = !is.na(date_delta_days) & date_delta_days <= date_tolerance_days,
        score = dplyr::case_when(
          same_family & within_tolerance ~ 1L,
          same_family ~ 2L,
          within_tolerance ~ 3L,
          TRUE ~ 4L
        )
      ) |>
      dplyr::arrange(score, date_delta_days, scraper_row_id)

    if (nrow(candidates) == 0L) {
      best_pairs[[length(best_pairs) + 1L]] <- tibble::tibble(
        dapip_row_id = drow$dapip_row_id[[1]],
        scraper_row_id = NA_integer_,
        audit_result = "dapip_only",
        date_delta_days = NA_integer_
      )
      next
    }

    candidate <- candidates[1, , drop = FALSE]
    result <- dplyr::case_when(
      candidate$same_family[[1]] && isTRUE(candidate$within_tolerance[[1]]) ~ "match",
      candidate$same_family[[1]] ~ "family_match_date_mismatch",
      isTRUE(candidate$within_tolerance[[1]]) ~ "date_overlap_family_mismatch",
      TRUE ~ "dapip_only"
    )

    if (result != "dapip_only") {
      used_scraper_ids <- c(used_scraper_ids, candidate$scraper_row_id[[1]])
    }

    best_pairs[[length(best_pairs) + 1L]] <- tibble::tibble(
      dapip_row_id = drow$dapip_row_id[[1]],
      scraper_row_id = if (result == "dapip_only") NA_integer_ else candidate$scraper_row_id[[1]],
      audit_result = result,
      date_delta_days = if (result == "dapip_only") NA_integer_ else candidate$date_delta_days[[1]]
    )
  }

  pair_df <- if (length(best_pairs)) dplyr::bind_rows(best_pairs) else tibble::tibble(
    dapip_row_id = integer(),
    scraper_row_id = integer(),
    audit_result = character(),
    date_delta_days = integer()
  )

  matched_audit <- pair_df |>
    dplyr::left_join(dapip, by = "dapip_row_id") |>
    dplyr::left_join(
      scraper |>
        dplyr::select(
          scraper_row_id,
          scraper_action_type = action_type,
          scraper_action_label = action_label_raw,
          scraper_action_date = action_date,
          scraper_source_url = source_url,
          scraper_source_key,
          scraper_family,
          scraper_notes = notes,
          scraper_action_label_source = action_label_source,
          scraper_file_text_path = file_text_path
        ),
      by = "scraper_row_id"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      scraper_eval = list(classify_public_signal(accreditor_norm, scraper_action_type, scraper_action_label, scraper_notes, scraper_action_date)),
      dapip_eval = list(classify_public_signal(accreditor_norm, action_type, action_label_raw, notes, action_date)),
      strategy_eval = list(recommend_public_table_strategy(
        audit_result = audit_result,
        scraper_keep = scraper_eval$keep %||% FALSE,
        dapip_keep = dapip_eval$keep %||% FALSE,
        scraper_label = scraper_action_label,
        dapip_label = action_label_raw,
        dapip_file_id = file_id
      )),
      scraper_public_keep = scraper_eval$keep %||% FALSE,
      scraper_public_reason = value_or_na(scraper_eval$reason),
      dapip_public_keep = dapip_eval$keep %||% FALSE,
      dapip_public_reason = value_or_na(dapip_eval$reason),
      public_table_strategy = strategy_eval$strategy,
      hybrid_candidate = strategy_eval$hybrid %||% FALSE,
      hybrid_reason = value_or_na(strategy_eval$hybrid_reason),
      scraper_display_reason_source = value_or_na(.select_action_summary_source_kind(
        action_label_raw = scraper_action_label,
        file_text_path = scraper_file_text_path,
        action_type = scraper_action_type,
        accreditor = accreditor_norm,
        notes = scraper_notes,
        action_label_source_hint = scraper_action_label_source
      )),
      dapip_display_reason_source = value_or_na(.select_action_summary_source_kind(
        action_label_raw = action_label_raw,
        file_text_path = file_text_path,
        action_type = action_type,
        accreditor = accreditor_norm,
        notes = notes,
        action_label_source_hint = label_source
      )),
      public_display_reason_source = dplyr::case_when(
        public_table_strategy %in% c("scraper_backed_keep", "hybrid_keep") ~ scraper_display_reason_source,
        public_table_strategy == "dapip_backed_keep" ~ dapip_display_reason_source,
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      unitid,
      institution_name = institution_name_raw,
      accreditor = accreditor_norm,
      scraper_action_type,
      scraper_action_label,
      scraper_action_date = as.character(scraper_action_date),
      dapip_action_type = action_type,
      dapip_action_label = action_label_raw,
      dapip_action_date = as.character(action_date),
      audit_result,
      date_delta_days,
      scraper_source_url,
      dapip_source_page_url = source_page_url,
      dapip_file_id = as.character(file_id),
      scraper_source_key,
      dapip_source_key,
      notes = keep_reason,
      scraper_public_keep,
      scraper_public_reason,
      dapip_public_keep,
      dapip_public_reason,
      public_table_strategy,
      hybrid_candidate,
      hybrid_reason,
      public_action_family = dplyr::coalesce(dapip_family, scraper_family),
      scraper_display_reason_source,
      dapip_display_reason_source,
      public_display_reason_source
    )

  scraper_only <- scraper |>
    dplyr::filter(!scraper_row_id %in% used_scraper_ids) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      scraper_eval = list(classify_public_signal(accreditor_norm, action_type, action_label_raw, notes, action_date)),
      scraper_public_keep = scraper_eval$keep %||% FALSE,
      scraper_public_reason = value_or_na(scraper_eval$reason),
      public_table_strategy = if (isTRUE(scraper_eval$keep %||% FALSE)) "scraper_backed_keep" else "drop_from_public_table",
      public_display_reason_source = value_or_na(.select_action_summary_source_kind(
        action_label_raw = action_label_raw,
        file_text_path = file_text_path,
        action_type = action_type,
        accreditor = accreditor_norm,
        notes = notes,
        action_label_source_hint = action_label_source
      ))
    ) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      unitid,
      institution_name = dplyr::coalesce(tracker_name, institution_name_raw),
      accreditor = accreditor_norm,
      scraper_action_type = action_type,
      scraper_action_label = action_label_raw,
      scraper_action_date = as.character(action_date),
      dapip_action_type = NA_character_,
      dapip_action_label = NA_character_,
      dapip_action_date = NA_character_,
      audit_result = "scraper_only",
      date_delta_days = NA_integer_,
      scraper_source_url = source_url,
      dapip_source_page_url = NA_character_,
      dapip_file_id = NA_character_,
      scraper_source_key,
      dapip_source_key = NA_character_,
      notes = NA_character_,
      scraper_public_keep,
      scraper_public_reason,
      dapip_public_keep = FALSE,
      dapip_public_reason = NA_character_,
      public_table_strategy,
      hybrid_candidate = FALSE,
      hybrid_reason = NA_character_,
      public_action_family = scraper_family,
      scraper_display_reason_source = public_display_reason_source,
      dapip_display_reason_source = NA_character_,
      public_display_reason_source
    )

  matched_audit <- ensure_audit_policy_columns(matched_audit) |>
    dplyr::mutate(
      unitid = as.character(unitid),
      institution_name = as.character(institution_name),
      accreditor = as.character(accreditor),
      scraper_action_type = as.character(scraper_action_type),
      scraper_action_label = as.character(scraper_action_label),
      scraper_action_date = as.character(scraper_action_date),
      dapip_action_type = as.character(dapip_action_type),
      dapip_action_label = as.character(dapip_action_label),
      dapip_action_date = as.character(dapip_action_date),
      audit_result = as.character(audit_result),
      date_delta_days = suppressWarnings(as.integer(date_delta_days)),
      scraper_source_url = as.character(scraper_source_url),
      dapip_source_page_url = as.character(dapip_source_page_url),
      dapip_file_id = as.character(dapip_file_id),
      scraper_source_key = as.character(scraper_source_key),
      dapip_source_key = as.character(dapip_source_key),
      notes = as.character(notes),
      scraper_public_keep = as.logical(scraper_public_keep),
      scraper_public_reason = as.character(scraper_public_reason),
      dapip_public_keep = as.logical(dapip_public_keep),
      dapip_public_reason = as.character(dapip_public_reason),
      public_table_strategy = as.character(public_table_strategy),
      hybrid_candidate = as.logical(hybrid_candidate),
      hybrid_reason = as.character(hybrid_reason),
      public_action_family = as.character(public_action_family),
      scraper_display_reason_source = as.character(scraper_display_reason_source),
      dapip_display_reason_source = as.character(dapip_display_reason_source),
      public_display_reason_source = as.character(public_display_reason_source)
    )

  scraper_only <- ensure_audit_policy_columns(scraper_only) |>
    dplyr::mutate(
      unitid = as.character(unitid),
      institution_name = as.character(institution_name),
      accreditor = as.character(accreditor),
      scraper_action_type = as.character(scraper_action_type),
      scraper_action_label = as.character(scraper_action_label),
      scraper_action_date = as.character(scraper_action_date),
      dapip_action_type = as.character(dapip_action_type),
      dapip_action_label = as.character(dapip_action_label),
      dapip_action_date = as.character(dapip_action_date),
      audit_result = as.character(audit_result),
      date_delta_days = suppressWarnings(as.integer(date_delta_days)),
      scraper_source_url = as.character(scraper_source_url),
      dapip_source_page_url = as.character(dapip_source_page_url),
      dapip_file_id = as.character(dapip_file_id),
      scraper_source_key = as.character(scraper_source_key),
      dapip_source_key = as.character(dapip_source_key),
      notes = as.character(notes),
      scraper_public_keep = as.logical(scraper_public_keep),
      scraper_public_reason = as.character(scraper_public_reason),
      dapip_public_keep = as.logical(dapip_public_keep),
      dapip_public_reason = as.character(dapip_public_reason),
      public_table_strategy = as.character(public_table_strategy),
      hybrid_candidate = as.logical(hybrid_candidate),
      hybrid_reason = as.character(hybrid_reason),
      public_action_family = as.character(public_action_family),
      scraper_display_reason_source = as.character(scraper_display_reason_source),
      dapip_display_reason_source = as.character(dapip_display_reason_source),
      public_display_reason_source = as.character(public_display_reason_source)
    )

  audit_df <- dplyr::bind_rows(matched_audit, scraper_only)
  if (nrow(audit_df) > 0L && any(!nzchar(trimws(as.character(audit_df$public_table_strategy))))) {
    stop("Audit strategy computation bug: one or more rows are missing public_table_strategy.", call. = FALSE)
  }

  public_counts <- audit_df |>
    dplyr::count(accreditor, public_table_strategy, name = "row_count") |>
    dplyr::arrange(accreditor, public_table_strategy)

  public_family_counts <- audit_df |>
    dplyr::count(accreditor, public_table_strategy, public_action_family, name = "row_count") |>
    dplyr::arrange(accreditor, public_table_strategy, public_action_family)

  code_source <- if (file.exists(raw_dapip_input)) {
    readr::read_csv(raw_dapip_input, show_col_types = FALSE, progress = FALSE) |>
      dplyr::mutate(action_code = as.character(action_code))
  } else {
    dapip
  }

  filtered_counts <- dapip |>
    dplyr::count(action_code, name = "kept_count")

  code_coverage <- code_source |>
    dplyr::mutate(classed = purrr::map(action_code, ~ {
      helper <- dapip_classify_action_code(.x)
      list(
        mapped_action_type = helper$action_type,
        review_required = helper$review_required
      )
    })) |>
    dplyr::mutate(
      mapped_action_type = purrr::map_chr(classed, "mapped_action_type"),
      review_required = purrr::map_lgl(classed, "review_required")
    ) |>
    dplyr::select(-classed) |>
    dplyr::count(action_code, action_description, mapped_action_type, review_required, name = "row_count") |>
    dplyr::left_join(filtered_counts, by = "action_code") |>
    dplyr::mutate(
      kept_count = dplyr::coalesce(kept_count, 0L),
      dropped_count = row_count - kept_count,
      review_required_count = dplyr::if_else(review_required, row_count, 0L)
    ) |>
    dplyr::select(
      action_code,
      action_description,
      row_count,
      kept_count,
      dropped_count,
      review_required_count,
      mapped_action_type
    ) |>
    dplyr::arrange(action_code)

  outputs <- list(
    audit = paste0(output_prefix, "_vs_scraper_audit.csv"),
    coverage = paste0(output_prefix, "_code_coverage.csv"),
    public_counts = paste0(output_prefix, "_public_table_policy_counts.csv"),
    public_family_counts = paste0(output_prefix, "_public_table_policy_family_counts.csv")
  )

  write_csv_atomic(audit_df, outputs$audit)
  write_csv_atomic(code_coverage, outputs$coverage)
  write_csv_atomic(public_counts, outputs$public_counts)
  write_csv_atomic(public_family_counts, outputs$public_family_counts)

  message(sprintf("Saved DAPIP vs scraper audit to %s", outputs$audit))
  message(sprintf("Saved DAPIP code coverage to %s", outputs$coverage))
  message(sprintf("Saved DAPIP public-table policy counts to %s", outputs$public_counts))
  message(sprintf("Saved DAPIP public-table policy family counts to %s", outputs$public_family_counts))
  message(sprintf(
    "DAPIP audit summary: %d match rows, %d scraper-only rows, %d public keep rows, %d hybrid rows",
    sum(audit_df$audit_result == "match", na.rm = TRUE),
    sum(audit_df$audit_result == "scraper_only", na.rm = TRUE),
    sum(audit_df$public_table_strategy != "drop_from_public_table", na.rm = TRUE),
    sum(audit_df$public_table_strategy == "hybrid_keep", na.rm = TRUE)
  ))
}

if (sys.nframe() == 0) {
  main()
}
