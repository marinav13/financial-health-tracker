run_test("Grant Witness name helpers", function() {
  assert_identical(
    prettify_institution_name("TEXAS A&M UNIVERSITY"),
    "Texas A&M University"
  )
  assert_identical(
    simplify_institution_name("Board of Trustees of Southern Illinois University"),
    "southern illinois university"
  )
  assert_identical(
    simplify_institution_name("University of North Carolina at Greensboro"),
    "university of north carolina greensboro"
  )
  assert_identical(
    simplify_institution_name("University of North Carolina Greensboro"),
    "university of north carolina greensboro"
  )
  assert_true(
    grepl(
      "subaward",
      detect_pass_through_phrase("Regional grantmaker initiative", "Will administer subawards"),
      fixed = TRUE
    )
  )
  assert_identical(
    strip_legal_prefixes("Board of Trustees of Southern Illinois University"),
    "Southern Illinois University"
  )
  assert_identical(abbr_to_state("IL"), "Illinois")
  assert_identical(normalize_city("San Juan, PR"), "san juan pr")
  assert_true(is_currently_disrupted("nih", "Frozen Funding"))
  assert_true(!is_currently_disrupted("nsf", "Reinstated"))
  assert_equal(
    classify_status_bucket(
      c("nih", "cdc", "epa"),
      c("Unfrozen Funding", "At Risk", "Reinstated")
    ),
    c("not_currently_disrupted", "currently_disrupted", "not_currently_disrupted")
  )
})

run_test("Grant Witness standardization config", function() {
  sample_nsf <- data.frame(
    grant_id = "12345",
    status = "Terminated",
    org_name = "BOARD OF TRUSTEES OF SOUTHERN ILLINOIS UNIVERSITY",
    org_state = "IL",
    org_city = "Carbondale",
    award_type = "Research",
    project_title = "CAMPUS RESEARCH",
    abstract = "Study of campus finance",
    nsf_start_date = NA_character_,
    usasp_start_date = "2024-01-01",
    nsf_end_date = NA_character_,
    usasp_end_date = "2025-01-01",
    termination_date = "2024-10-01",
    reinstatement_date = NA_character_,
    estimated_budget = "$100,000",
    estimated_outlays = "50000",
    estimated_remaining = "50000",
    usaspending_url = "https://example.com/award/abc",
    nsf_url = "https://nsf.example/123",
    stringsAsFactors = FALSE
  )

  standardized <- standardize_grant_witness_rows("nsf", sample_nsf, "nsf_terminations.csv")
  assert_identical(standardized$grant_id[[1]], "12345")
  assert_identical(standardized$organization_state[[1]], "Illinois")
  assert_identical(standardized$start_date[[1]], "2024-01-01")
  assert_equal(standardized$award_value[[1]], 100000)
  assert_identical(standardized$remaining_field[[1]], "estimated_remaining")
  assert_identical(standardized$detail_url[[1]], "https://nsf.example/123")

  temp_file <- tempfile("grant-witness-cache-")
  writeLines("cached", temp_file)
  maybe_download("https://invalid.example/grants.csv", temp_file, skip_download = TRUE)
  assert_true(file.exists(temp_file), "Expected cached Grant Witness file to be reused.")
})
