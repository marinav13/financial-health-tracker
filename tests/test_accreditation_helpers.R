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
