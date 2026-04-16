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
  parsed <- extract_name_state_from_item("Example College, Boston, MA")
  assert_identical(unname(parsed$institution_name_raw), "Example College")
  assert_identical(unname(parsed$institution_state_raw), "Massachusetts")
  assert_identical(classify_action("Placed on probation"), "probation")
  assert_identical(classify_status("Removed from probation"), "resolved")
  assert_true(has_public_action_keywords("Public Notice of Concern"))
})

run_test("Accreditation tracker matching", function() {
  actions_df <- data.frame(
    institution_name_normalized = c("example college", "name only match"),
    institution_state_normalized = c("Massachusetts", "Texas"),
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
    matched_unitid = "200",
    tracker_name = "Name Only Match University",
    tracker_state = "California",
    norm_name = "name only match",
    stringsAsFactors = FALSE
  )

  matched <- match_institutions_to_tracker(actions_df, lookup_exact, lookup_name_only)
  assert_identical(matched$unitid[[1]], "100")
  assert_identical(matched$match_method[[1]], "normalized_name_plus_state")
  assert_identical(matched$unitid[[2]], "200")
  assert_identical(matched$match_method[[2]], "normalized_name_only")
})
