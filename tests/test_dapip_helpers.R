source(file.path(root, "scripts", "shared", "dapip_helpers.R"))

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
