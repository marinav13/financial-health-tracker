if (!exists("run_test", mode = "function")) {
  source(file.path(getwd(), "tests", "test_support.R"))
}

load_tracker_unitids <- function() {
  tracker_index_path <- file.path(root, "data", "schools_index.json")
  tracker_index <- jsonlite::fromJSON(tracker_index_path, simplifyVector = TRUE)
  unitids <- trim_text(as.character(tracker_index$unitid %||% character()))
  unique(unitids[nzchar(unitids)])
}

effective_override_unitid <- function(df,
                                      source_col,
                                      override_col) {
  dplyr::coalesce(
    trim_optional_text(df[[override_col]]),
    trim_optional_text(df[[source_col]])
  )
}

run_test("College cuts review candidates stay within tracker roster", function() {
  tracker_unitids <- load_tracker_unitids()
  candidates_path <- file.path(root, "data_pipelines", "college_cuts", "college_cuts_review_candidates.csv")
  candidates <- read_college_cuts_review_candidates(candidates_path)

  assert_true(nrow(candidates) > 0L, "Expected at least one college cuts review candidate.")
  out_of_scope <- candidates[!(trim_text(candidates$unitid) %in% tracker_unitids), , drop = FALSE]
  assert_identical(
    nrow(out_of_scope),
    0L,
    "College cuts review candidates should only include tracker institutions."
  )
})

run_test("Staged college cuts overrides keep only tracker-scoped review rows", function() {
  tracker_unitids <- load_tracker_unitids()
  candidates_path <- file.path(root, "data_pipelines", "college_cuts", "college_cuts_review_candidates.csv")
  overrides_path <- file.path(root, "data_pipelines", "college_cuts", "editorial_overrides.csv")
  candidates <- read_college_cuts_review_candidates(candidates_path)
  overrides <- read_college_cuts_editorial_overrides(overrides_path)

  staged <- stage_college_cuts_editorial_overrides(
    candidates = candidates,
    existing = overrides,
    tracker_unitids = tracker_unitids
  )
  row_origin <- normalize_review_row_origin(staged$source_row_origin)
  non_manual <- staged[is.na(row_origin) | row_origin != "manual", , drop = FALSE]
  effective_unitids <- effective_override_unitid(non_manual, "source_unitid", "override_unitid")
  out_of_scope <- non_manual[!(trim_text(effective_unitids) %in% tracker_unitids), , drop = FALSE]

  assert_true(nrow(staged) > 0L, "Expected staged college cuts overrides to be non-empty.")
  assert_identical(
    nrow(out_of_scope),
    0L,
    "Staged non-manual college cuts overrides should only include tracker institutions."
  )
})

run_test("Accreditation review candidates stay within tracker roster", function() {
  tracker_unitids <- load_tracker_unitids()
  candidates_path <- file.path(root, "data_pipelines", "accreditation", "accreditation_review_candidates.csv")
  candidates <- read_accreditation_review_candidates(candidates_path)

  assert_true(nrow(candidates) > 0L, "Expected at least one accreditation review candidate.")
  out_of_scope <- candidates[!(trim_text(candidates$unitid) %in% tracker_unitids), , drop = FALSE]
  assert_identical(
    nrow(out_of_scope),
    0L,
    "Accreditation review candidates should only include tracker institutions."
  )
})

run_test("Accreditation committed overrides stay within tracker roster", function() {
  tracker_unitids <- load_tracker_unitids()
  overrides_path <- file.path(root, "data_pipelines", "accreditation", "editorial_overrides.csv")
  overrides <- read_accreditation_editorial_overrides(overrides_path)
  effective_unitids <- effective_override_unitid(overrides, "source_unitid", "override_unitid")
  out_of_scope <- overrides[!(trim_text(effective_unitids) %in% tracker_unitids), , drop = FALSE]

  assert_true(nrow(overrides) > 0L, "Expected committed accreditation overrides to be non-empty.")
  assert_identical(
    nrow(out_of_scope),
    0L,
    "Accreditation committed overrides should only include tracker institutions."
  )
})
