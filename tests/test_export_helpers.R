run_test("Export helper basics", function() {
  temp_file <- tempfile("local-file-")
  writeLines("ok", temp_file)
  require_local_file(temp_file, "temp file", "unused")

  assert_identical(make_export_id("cut", "12345", "Name", "NY"), "12345")
  assert_identical(
    make_export_id("cut", "", "Example University", "NY"),
    "cut-example-university-ny"
  )
  assert_identical(
    normalize_display_institution_name("Arizona State University Campus Immersion"),
    "Arizona State University"
  )
  assert_identical(or_null(c("", "filled")), NA)
  assert_identical(normalize_control_label("private non-profit"), "Private not-for-profit")
  assert_identical(derive_positions_affected(NA, "The college laid off 23 employees", "", "", ""), 23L)
  assert_equal(
    build_series(data.frame(year = c(2024L, 2025L), value = c(1, NA_real_)), "value"),
    list(list(year = 2024L, value = 1))
  )
})

run_test("International sentence builder", function() {
  sentence <- build_international_students_sentence(2024, 0.12, 0.08, 0.21)
  assert_true(grepl("In 2024, 12", sentence, fixed = TRUE))
  assert_true(grepl("8", sentence, fixed = TRUE))
  assert_true(grepl("21", sentence, fixed = TRUE))
})

run_test("Export bundle helpers", function() {
  temp_dir <- tempfile("export-bundle-")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  school <- list(
    unitid = "123",
    financial_unitid = "123",
    has_financial_profile = TRUE,
    is_primary_tracker = TRUE,
    institution_name = "Arizona State University Campus Immersion",
    city = "Tempe",
    state = "AZ",
    control_label = "Public",
    category = "Research"
  )

  assert_identical(
    build_institution_unique_name(school$institution_name, school$city, school$state),
    "Arizona State University | Tempe | AZ"
  )

  picked <- pick_first_present(
    data.frame(primary = NA_character_, fallback = "kept", stringsAsFactors = FALSE),
    c("missing", "primary", "fallback")
  )
  assert_identical(picked[[1]], "kept")

  entry <- build_school_index_entry(school, extra_flag = TRUE)
  assert_identical(entry$institution_unique_name, "Arizona State University | Tempe | AZ")
  assert_true(isTRUE(entry$extra_flag))

  bundle_paths <- write_export_bundle(
    export_obj = list(schools = list(school)),
    data_dir = temp_dir,
    export_filename = "schools.json",
    index_filename = "schools-index.json",
    index_builder = function(one_school) list(extra_flag = TRUE)
  )

  assert_true(file.exists(bundle_paths$export_path))
  assert_true(file.exists(bundle_paths$index_path))

  index_rows <- jsonlite::read_json(bundle_paths$index_path, simplifyVector = TRUE)
  assert_identical(index_rows$institution_unique_name[[1]], "Arizona State University | Tempe | AZ")
  assert_true(isTRUE(index_rows$extra_flag[[1]]))
})
