# Drift guard + behavior lock for the three name normalizers in
# scripts/shared/name_normalization.R.
#
# The cuts fixture file (tests/fixtures/name_normalization_cuts.json) is
# loaded by BOTH this file and tests/test_import_supabase.py. If either
# side of the R <-> Python contract changes output for any fixture and the
# other side isn't updated, one of the two tests fails. This is the drift
# guard the audit asked for.

run_test("Name normalization: cuts fixtures match R implementation", function() {
  cuts_fixture <- jsonlite::fromJSON(
    file.path(root, "tests", "fixtures", "name_normalization_cuts.json"),
    simplifyVector = TRUE
  )
  cases <- cuts_fixture$cases
  for (i in seq_len(nrow(cases))) {
    input <- cases$input[[i]]
    expected <- cases$expected[[i]]
    actual <- normalize_name_cuts(input)
    assert_identical(
      actual,
      expected,
      sprintf(
        "normalize_name_cuts drifted from shared fixture: input=%s expected=%s actual=%s",
        shQuote(input), shQuote(expected), shQuote(actual)
      )
    )
  }
})

run_test("Name normalization: accreditation variant covers known quirks", function() {
  # These are the transformations the accreditation pipeline specifically
  # relies on. Locking them here means a shared refactor can't silently
  # break cross-accreditor matching.
  assert_identical(normalize_name_accreditation("University of Hawai\u2019i, Hilo"), "university of hawaii at hilo")
  assert_identical(normalize_name_accreditation("Catholic University of America, The"), "catholic university of america")
  assert_identical(normalize_name_accreditation("Saint Mary's College"), "st marys college")
  assert_identical(normalize_name_accreditation("Texas A&M University-College Station Main Campus"), "texas a and m university college station")
  assert_identical(normalize_name_accreditation("Saint Augustine\u2019s University"), "st augustines university")
  assert_identical(normalize_name_accreditation("Saint Augustine's University"), "st augustines university")
  assert_identical(normalize_name_accreditation("Women\u2019s Institute of Torah Seminary & College"), "womens institute of torah seminary and college")
  assert_identical(normalize_name_accreditation("Women's Institute of Torah Seminary and College"), "womens institute of torah seminary and college")
})

run_test("Name normalization: grant_witness variant stays minimal", function() {
  # Minimal-by-design: the grant-witness feed is already clean. Locking
  # this prevents a future author from pulling in accreditation- or
  # cuts-specific transformations that would produce false positives.
  assert_identical(normalize_name_grant_witness("Harvard Medical School"), "harvard medical school")
  assert_identical(normalize_name_grant_witness("SUNY Research Foundation"), "suny research foundation")
  assert_identical(normalize_name_grant_witness("St. Jude Children's Research Hospital"), "st jude children s research hospital")
  assert_identical(normalize_name_grant_witness("Cancer Research & Biostatistics"), "cancer research and biostatistics")
})
