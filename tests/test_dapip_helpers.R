source(file.path(root, "scripts", "shared", "dapip_helpers.R"))

if (!exists("run_test", mode = "function")) {
  source(file.path(getwd(), "tests", "test_support.R"))
}

run_test("DAPIP OPEID normalization and cache keys", function() {
  assert_equal(normalize_opeid("372000"), "00372000")
  assert_equal(normalize_opeid("003720-00"), "00372000")
  assert_equal(normalize_opeid(""), "")
  assert_equal(normalize_opeid_base("00372000"), "003720")
  assert_equal(normalize_opeid_base("00372018"), "003720")

  key <- dapip_search_cache_key(
    unitid = "232609",
    opeid = "00372000",
    institution_name = "University of Lynchburg",
    state = "Virginia"
  )
  assert_equal(key, "v2_opeid_00372000")

  key_name <- dapip_search_cache_key(
    unitid = "",
    opeid = "",
    institution_name = "University of Lynchburg",
    state = "Virginia"
  )
  assert_equal(key_name, "v2_name_state_university-of-lynchburg__virginia")
})

run_test("DAPIP payload builder prioritizes advanced OPEID then unitid", function() {
  payloads <- dapip_build_search_payloads(
    unitid = "232609",
    opeid = "00372000",
    institution_name = "University of Lynchburg",
    state = "Virginia"
  )
  assert_equal(length(payloads), 2L)
  assert_equal(payloads[[1]]$variant, "advanced_opeid")
  assert_equal(payloads[[1]]$body$OpeID, "00372000")
  assert_equal(payloads[[2]]$variant, "advanced_ipeds_unitid")
  assert_equal(payloads[[2]]$body$IpedsUnitId, "232609")
  assert_true(grepl("surveys\\.ope\\.ed\\.gov", payloads[[1]]$endpoint))
})

run_test("DAPIP search parser handles lowercase results payloads", function() {
  payload <- list(
    results = list(
      list(
        unitid = 147730L,
        opeID = "00372000",
        institutionType = "Institution",
        institutionName = "University of Lynchburg",
        state = "VA",
        activeStatus = "Active"
      )
    )
  )

  parsed <- dapip_parse_search_response(
    payload,
    search_variant = "advanced_opeid",
    requested_unitid = "232609",
    requested_opeid = "00372000"
  )

  assert_equal(nrow(parsed), 1L)
  assert_equal(parsed$dapip_id[[1]], 147730L)
  assert_equal(parsed$dapip_opeid[[1]], "00372000")
  assert_equal(parsed$institution_type[[1]], "Institution")
})

run_test("DAPIP search parser backfills requested unitid when response omits IPEDS field", function() {
  payload <- list(
    results = list(
      list(
        unitid = 241793L,
        opeID = NULL,
        institutionType = "Institution",
        institutionName = "Haven University",
        state = "CA",
        activeStatus = "Active"
      )
    )
  )

  parsed <- dapip_parse_search_response(
    payload,
    search_variant = "advanced_ipeds_unitid",
    requested_unitid = "111045",
    requested_opeid = ""
  )

  assert_equal(nrow(parsed), 1L)
  assert_equal(parsed$dapip_ipeds_unitid[[1]], "111045")
})

run_test("DAPIP best-match ranking prefers exact OPEID and state", function() {
  candidates <- tibble::tibble(
    dapip_id = c(999999L, 147730L),
    dapip_institution_name = c("University of Lynchburg West", "University of Lynchburg"),
    dapip_state = c("California", "Virginia"),
    dapip_opeid = c("00999999", "00372000"),
    dapip_ipeds_unitid = c("999999", "232609"),
    active_status = c("Active", "Active"),
    search_variant = c("fixture", "fixture"),
    raw_payload = c("{}", "{}")
  )

  best <- dapip_pick_best_match(
    candidates,
    unitid = "232609",
    opeid = "00372000",
    institution_name = "University of Lynchburg",
    state = "Virginia"
  )

  assert_equal(best$dapip_id[[1]], 147730L)
  assert_equal(best$match_confidence[[1]], "high")
})

run_test("DAPIP manual alias matching works by normalized name and state", function() {
  aliases <- tibble::tibble(
    unitid = "100",
    opeid = "00123456",
    tracker_institution_name = "Saint Example University",
    tracker_state = "Massachusetts",
    dapip_id = 123456L,
    dapip_institution_name = "St. Example University",
    dapip_state = "MA",
    notes = "Fixture alias"
  )

  matched <- dapip_find_manual_alias(
    unitid = "",
    opeid = "",
    institution_name = "St. Example University",
    state = "Massachusetts",
    aliases = aliases
  )

  assert_true(!is.null(matched), "Expected manual alias lookup to match by normalized name and state.")
  assert_equal(matched$dapip_id[[1]], 123456L)
})

run_test("DAPIP crosswalk resolver rejects low-confidence candidates", function() {
  aliases <- tibble::tibble(
    unitid = character(),
    opeid = character(),
    tracker_institution_name = character(),
    tracker_state = character(),
    dapip_id = integer(),
    dapip_institution_name = character(),
    dapip_state = character(),
    notes = character()
  )

  low_confidence_search <- function(unitid = NULL, opeid = NULL, institution_name = NULL, state = NULL,
                                    cache_dir, refresh = TRUE, verbose = FALSE) {
    tibble::tibble(
      dapip_id = 211972L,
      dapip_institution_name = "Completely Different Institution",
      dapip_state = state,
      dapip_opeid = "",
      dapip_ipeds_unitid = "",
      active_status = "Active",
      search_variant = "fixture",
      raw_payload = "{}"
    )
  }

  resolved <- dapip_resolve_crosswalk_row(
    unitid = "232609",
    opeid = "",
    institution_name = "University of Lynchburg",
    state = "Virginia",
    aliases = aliases,
    cache_dir = tempdir(),
    refresh = FALSE,
    verbose = FALSE,
    search_fn = low_confidence_search
  )

  assert_true(!isTRUE(resolved$matched), "Low-confidence DAPIP candidates should be rejected.")
  assert_equal(resolved$match_method, "unmatched")
  assert_equal(resolved$match_confidence, "none")
  assert_true(grepl("Rejected low-confidence DAPIP candidate", resolved$match_notes %||% ""))
})

run_test("DAPIP text extraction can surface ownership and merger action sentences", function() {
  text <- paste0(
    "During its February 11-13, 2026 meeting, the WASC Senior College and University Commission (WSCUC) considered a request submitted by Design Institute of San Diego (DISD) for a Change of Ownership to Torreyana College, LLC, an entity managed by Palm Ventures. ",
    "After deliberation, the Commission acted to: 1. Approve the Change of Ownership to Torreyana College, LLC, an entity managed by Palm Ventures. 2. Require a post-implementation visit within six months of the close of the transaction."
  )
  extracted <- dapip_extract_action_label_from_text(text, "Grant Substantive Change: Ownership")
  assert_equal(extracted$label_source, "dapip_file_text")
  assert_true(grepl("Change of Ownership to Torreyana College, LLC", extracted$label, fixed = TRUE))
})

run_test("DAPIP text extraction prefers referral-report action sentences over later boilerplate comments", function() {
  text <- paste0(
    "The Southern Association of Colleges and Schools Commission on Colleges (SACSCOC) Committee on Fifth-Year Interim Reports reviewed the institution's compliance with the select standards of the Principles of Accreditation outlined in the SACSCOC Fifth-Year Interim Report. ",
    "Based only on those reviewed standards, the institution is requested to submit a Referral Report due April 1, 2020, addressing the following referenced standards of the Principles: ",
    "Standard 10.7 (Policies for awarding credit). ",
    "Comments As per Dr. Belle S. Wheelan's email of July 3, 2018, institutions were prompted to select a baseline completion indicator."
  )
  extracted <- dapip_extract_action_label_from_text(text, "Heightened Monitoring or Focused Review")
  assert_equal(extracted$label_source, "dapip_file_text")
  assert_true(grepl("Referral Report due April 1, 2020", extracted$label, fixed = TRUE))
  assert_true(grepl("Standard 10.7 (Policies for awarding credit)", extracted$label, fixed = TRUE))
  assert_true(!grepl("baseline completion indicator", extracted$label, fixed = TRUE))
})

run_test("DAPIP text extraction prefers voluntary resignation sentences over downstream teach-out detail", function() {
  text <- paste0(
    "Martin University Voluntary Resignation of Accreditation Effective: December 31, 2025. ",
    "Martin University in Indianapolis, Indiana, voluntarily resigned its accreditation with the Higher Learning Commission effective December 31, 2025. ",
    "The institution has established a teach-out agreement that has been approved by HLC with University of Indianapolis in Indianapolis, Indiana."
  )
  extracted <- dapip_extract_action_label_from_text(text, "Loss of Accreditation or Preaccreditation: Voluntary Withdrawal")
  assert_equal(extracted$label_source, "dapip_file_text")
  assert_true(grepl("voluntarily resigned its accreditation", extracted$label, ignore.case = TRUE))
  assert_true(!grepl("teach-out agreement", extracted$label, ignore.case = TRUE))
})

run_test("DAPIP text extraction prefers clean-review outcome sentences over courtesy closings", function() {
  text <- paste0(
    "The Southern Association of Colleges and Schools Commission on Colleges (SACSCOC) Fifth-Year Interim Review Committee reviewed the institution's compliance with select standards of the Principles of Accreditation as outlined in the SACSCOC Fifth-Year Interim Report. ",
    "Based only on those reviewed standards, we are pleased to inform you that there are no referrals or adverse findings subsequent to this review. ",
    "We extend our sincere gratitude for your continued dedication and support of the accreditation process."
  )
  extracted <- dapip_extract_action_label_from_text(text, "Heightened Monitoring or Focused Review")
  assert_equal(extracted$label_source, "dapip_file_text")
  assert_true(grepl("no referrals or adverse findings subsequent to this review", extracted$label, ignore.case = TRUE))
  assert_true(!grepl("sincere gratitude", extracted$label, ignore.case = TRUE))
})

run_test("DAPIP ownership substantive change codes are classified as substantive transaction sources", function() {
  granted <- dapip_classify_action_code("GO", "Grant Substantive Change: Ownership")
  assert_equal(granted$action_type, "other")
  assert_true(isTRUE(granted$keep))
  assert_equal(granted$mapped_action_family, "ownership_change")
  assert_equal(granted$keep_reason, "public_action_code")

  denied <- dapip_classify_action_code("DO", "Deny Substantive Change: Ownership")
  assert_equal(denied$action_type, "adverse_action")
  assert_true(isTRUE(denied$review_required))
  assert_equal(denied$mapped_action_family, "ownership_change_denial")
  assert_equal(denied$keep_reason, "review_code")
})

run_test("DAPIP HM clean-review letters are dropped from public-action candidates", function() {
  clean_letter <- paste0(
    "The Southern Association of Colleges and Schools Commission on Colleges (SACSCOC) Fifth-Year Interim Review Committee reviewed the institution's compliance with select standards of the Principles of Accreditation as outlined in the SACSCOC Fifth-Year Interim Report. ",
    "Based only on those reviewed standards, we are pleased to inform you that there are no referrals or adverse findings subsequent to this review. ",
    "We extend our sincere gratitude for your continued dedication and support of the accreditation process."
  )
  classed <- dapip_classify_action_code(
    "HM",
    "Heightened Monitoring or Focused Review",
    action_label = "We extend our sincere gratitude for your continued dedication and support of the accreditation process.",
    full_text = clean_letter
  )
  assert_equal(classed$action_type, "other")
  assert_true(!isTRUE(classed$keep))
  assert_equal(classed$mapped_action_family, "routine_clean_review")
  assert_equal(classed$keep_reason, "routine_clean_review_letter")
})

run_test("DAPIP HM referral-report letters remain public-action candidates", function() {
  referral_letter <- paste0(
    "The Southern Association of Colleges and Schools Commission on Colleges (SACSCOC) Committee on Fifth-Year Interim Reports reviewed the institution's compliance with the select standards of the Principles of Accreditation outlined in the SACSCOC Fifth-Year Interim Report. ",
    "Based only on those reviewed standards, the institution is requested to submit a Referral Report due April 1, 2020, addressing the following referenced standards of the Principles: ",
    "Standard 10.7 (Policies for awarding credit)."
  )
  classed <- dapip_classify_action_code(
    "HM",
    "Heightened Monitoring or Focused Review",
    action_label = "Requested to submit a Referral Report due April 1, 2020, addressing the following referenced standards of the Principles: Standard 10.7 (Policies for awarding credit).",
    full_text = referral_letter
  )
  assert_equal(classed$action_type, "notice")
  assert_true(isTRUE(classed$keep))
  assert_equal(classed$mapped_action_family, "monitoring_or_notice")
  assert_equal(classed$keep_reason, "public_action_code")
})
