run_test("Accreditation text and classification", function() {
  assert_identical(unname(clean_text("<p>Hello<br>World</p>")), "Hello World")
  html <- paste0(
    "<html><head><title> Example Title </title>",
    "<meta property=\"article:modified_time\" content=\"2025-02-03T12:00:00Z\">",
    "</head></html>"
  )
  assert_identical(unname(extract_page_title(html)), "Example Title")
  assert_identical(unname(extract_page_modified_date(html)), "2025-02-03")
  assert_identical(state_name("MA"), "Massachusetts")
  assert_identical(normalize_accreditation_name("University of Hawai’i at Hilo"), "university of hawaii at hilo")
  assert_identical(normalize_accreditation_name("University of Hawai’i, Hilo"), "university of hawaii at hilo")
  assert_identical(normalize_accreditation_name("Catholic University of America, The"), "catholic university of america")
  assert_identical(normalize_accreditation_name("The George Washington University"), "george washington university")
  parsed <- extract_name_state_from_item("Example College, Boston, MA")
  assert_identical(unname(parsed$institution_name_raw), "Example College")
  assert_identical(unname(parsed$institution_state_raw), "Massachusetts")
  assert_identical(classify_action("Placed on probation"), "probation")
  assert_identical(classify_action("Continued on High Probation"), "probation")
  assert_identical(classify_action("Issue a Notice of Concern"), "notice")
  assert_identical(classify_action("Accepted Teach-Out Plan"), "adverse_action")
  assert_identical(classify_action("Denied Reaffirmation"), "adverse_action")
  assert_identical(classify_status("Removed from probation"), "resolved")
  assert_true(has_public_action_keywords("Public Notice of Concern"))
  assert_true(has_public_action_keywords("Accepted teach-out plan"))
  assert_true(has_public_action_keywords("Denied reaffirmation"))

  # MSCHE adverse_action disambiguation: institutional vs. branch/location.
  # True institutional adverse actions must classify as adverse_action.
  saint_rose <- paste(
    "To acknowledge receipt of the institution's notification dated",
    "December 1, 2023, of its intention to cease instruction at all",
    "locations on June 30, 2024."
  )
  assert_identical(classify_action(saint_rose, "MSCHE"), "adverse_action")
  anna_maria <- paste(
    "To note that the institution announced on April 23, 2026 that its",
    "Board of Trustees voted to cease academic operations effective",
    "June 30, 2026. The institution will close all locations."
  )
  assert_identical(classify_action(anna_maria, "MSCHE"), "adverse_action")
  voluntary_surrender <- paste(
    "To accept the institution's voluntary surrender of accreditation",
    "effective immediately."
  )
  assert_identical(classify_action(voluntary_surrender, "MSCHE"), "adverse_action")

  # False positives that previously classified as adverse_action but are
  # really substantive-change paperwork tied to a sub-unit. These must
  # demote to "other" so the adverse_action bucket reflects only true
  # institutional signals.
  branch_close <- paste(
    "To acknowledge receipt of the substantive change request. To note",
    "the institution's decision to close the additional location at",
    "123 Main Street, Anytown, PA."
  )
  assert_identical(classify_action(branch_close, "MSCHE"), "other")
  branch_campus_close <- paste(
    "To acknowledge receipt of the substantive change request. To note",
    "the institution's decision to close the branch campus at 456 Elm",
    "Street, Othertown, NJ."
  )
  assert_identical(classify_action(branch_campus_close, "MSCHE"), "other")
  branch_teach_out <- paste(
    "To acknowledge receipt of the teach-out plan. To approve the",
    "teach-out plan for the closure of the additional location at",
    "789 Oak Avenue."
  )
  assert_identical(classify_action(branch_teach_out, "MSCHE"), "other")
  candidate_teach_out <- paste(
    "To acknowledge receipt of the teach-out plan. To approve the",
    "teach-out plan as required of candidate institutions."
  )
  assert_identical(classify_action(candidate_teach_out, "MSCHE"), "other")
})

run_test("MSCHE staff-preamble strip", function() {
  # Idempotent: no preamble -> unchanged.
  assert_identical(
    strip_msche_staff_preamble("To accept the progress report."),
    "To accept the progress report."
  )
  # Stripped form is "To " + remainder, regardless of original casing.
  assert_identical(
    strip_msche_staff_preamble(
      "Staff acted on behalf of the Commission to acknowledge receipt of X"
    ),
    "To acknowledge receipt of X"
  )
  assert_identical(
    strip_msche_staff_preamble(
      "STAFF ACTED ON BEHALF OF THE COMMISSION TO REQUEST a monitoring report"
    ),
    "To request a monitoring report"
  )
  # Preamble without trailing "to" still yields a leading "To ".
  assert_identical(
    strip_msche_staff_preamble(
      "Staff acted on behalf of the Commission acknowledge receipt of X"
    ),
    "To acknowledge receipt of X"
  )
  # NA pass-through.
  assert_true(is.na(strip_msche_staff_preamble(NA_character_)))
})

run_test("MSCHE procedural-drop classifier", function() {
  # Pure procedural rows -> dropped.
  assert_true(is_msche_procedural_drop(
    "To acknowledge receipt of the supplemental information report."
  ))
  assert_true(is_msche_procedural_drop(
    "To request a supplemental information report due January 9, 2023."
  ))
  assert_true(is_msche_procedural_drop(
    "To approve the teach-out plan as required of candidate institutions."
  ))
  assert_true(is_msche_procedural_drop(
    "To remind the institution of its obligations."
  ))
  assert_true(is_msche_procedural_drop(
    "To temporarily waive substantive change policy due to COVID-19."
  ))

  # Substantive-keep override: institution-level closures must NOT
  # be dropped even when the row's preamble matches a procedural
  # pattern. Saint Rose / Anna Maria-style rows.
  saint_rose <- paste(
    "To acknowledge receipt of the institution's notification dated",
    "December 1, 2023, of its intention to cease instruction at all",
    "locations on June 30, 2024."
  )
  assert_true(!is_msche_procedural_drop(saint_rose))
  voluntary_surrender_via_ack <- paste(
    "To acknowledge receipt of the institution's notice of voluntary",
    "surrender of accreditation effective June 30, 2024."
  )
  assert_true(!is_msche_procedural_drop(voluntary_surrender_via_ack))
  merger_via_ack <- paste(
    "To acknowledge receipt of the substantive change request. To",
    "approve the merger of College X into University Y, with",
    "University Y as the surviving institution."
  )
  assert_true(!is_msche_procedural_drop(merger_via_ack))

  # Status-page short labels (no procedural shape) -> not dropped.
  assert_true(!is_msche_procedural_drop("Non-Compliance Warning"))
  assert_true(!is_msche_procedural_drop("Non-Compliance Probation"))

  # NA / empty inputs are safe.
  assert_true(!is_msche_procedural_drop(""))
  assert_identical(length(is_msche_procedural_drop(character(0))), 0L)
})

run_test("Accreditation tracker matching", function() {
  actions_df <- data.frame(
    institution_name_normalized = c("example college", "name only match", "name only match", "same name different state"),
    institution_state_normalized = c("Massachusetts", NA, "California", "Texas"),
    stringsAsFactors = FALSE
  )
  lookup_exact <- data.frame(
    matched_unitid = "100",
    tracker_name = "Example College",
    tracker_state = "Massachusetts",
    norm_name = "example college",
    state_match = "Massachusetts",
    stringsAsFactors = FALSE
  )
  lookup_name_only <- data.frame(
    matched_unitid = c("200", "300"),
    tracker_name = c("Name Only Match University", "Same Name Different State"),
    tracker_state = c("California", "California"),
    norm_name = c("name only match", "same name different state"),
    stringsAsFactors = FALSE
  )

  matched <- match_institutions_to_tracker(actions_df, lookup_exact, lookup_name_only)
  assert_identical(matched$unitid[[1]], "100")
  assert_identical(matched$match_method[[1]], "normalized_name_plus_state")
  assert_identical(matched$unitid[[2]], "200")
  assert_identical(matched$match_method[[2]], "normalized_name_only")
  assert_identical(matched$unitid[[3]], "200")
  assert_identical(matched$match_method[[3]], "normalized_name_only")
  assert_true(is.na(matched$unitid[[4]]))
  assert_identical(matched$match_method[[4]], "unmatched")
})
