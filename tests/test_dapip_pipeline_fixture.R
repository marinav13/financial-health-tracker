if (!exists("run_test", mode = "function")) {
  source(file.path(getwd(), "tests", "test_support.R"))
}

run_test("DAPIP crosswalk pipeline fixture", function() {
  fixture_root <- tempfile("dapip-fixture-")
  dir.create(fixture_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  dirs <- c(
    file.path(fixture_root, "scripts"),
    file.path(fixture_root, "scripts", "shared"),
    file.path(fixture_root, "data_pipelines"),
    file.path(fixture_root, "data_pipelines", "accreditation"),
    file.path(fixture_root, "data_pipelines", "accreditation", "cache")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  for (nm in c(
    "shared/utils.R",
    "shared/ipeds_paths.R",
    "shared/name_normalization.R",
    "shared/accreditation_helpers.R",
    "shared/dapip_helpers.R"
  )) {
    file.copy(
      file.path(root, "scripts", nm),
      file.path(fixture_root, "scripts", nm),
      overwrite = TRUE
    )
  }
  file.copy(
    file.path(root, "scripts", "build_dapip_crosswalk.R"),
    file.path(fixture_root, "scripts", "build_dapip_crosswalk.R"),
    overwrite = TRUE
  )

  manual_aliases <- "unitid,opeid,tracker_institution_name,tracker_state,dapip_id,dapip_institution_name,dapip_state,notes\n100,00123456,Alias University,Massachusetts,111111,Alias University,MA,Fixture alias\n"
  writeLines(
    manual_aliases,
    file.path(fixture_root, "data_pipelines", "accreditation", "dapip_manual_aliases.csv"),
    useBytes = TRUE
  )

  helper_lines <- readLines(file.path(fixture_root, "scripts", "shared", "dapip_helpers.R"), warn = FALSE)
  helper_override <- c(
    helper_lines,
    "",
    "dapip_search_candidates <- function(unitid = NULL, opeid = NULL, institution_name = NULL, state = NULL, cache_dir, refresh = TRUE, verbose = FALSE) {",
    "  if (identical(trimws(as.character(opeid %||% '')), '00372000')) {",
    "    return(tibble::tibble(",
    "      dapip_id = 147730L,",
    "      dapip_institution_name = 'University of Lynchburg',",
    "      dapip_state = 'VA',",
    "      dapip_opeid = '00372000',",
    "      dapip_ipeds_unitid = '232609',",
    "      active_status = 'Active',",
    "      search_variant = 'fixture',",
    "      raw_payload = '{}'",
    "    ))",
    "  }",
    "  result <- empty_dapip_search_results()",
    "  attr(result, 'dapip_search_error') <- 'fixture search miss'",
    "  result",
    "}"
  )
  writeLines(
    helper_override,
    file.path(fixture_root, "scripts", "shared", "dapip_helpers.R"),
    useBytes = TRUE
  )

  input_path <- file.path(fixture_root, "fixture_ipeds.csv")
  output_prefix <- file.path(fixture_root, "data_pipelines", "accreditation", "dapip")
  fixture_df <- data.frame(
    unitid = c("100", "200", "300"),
    institution_name = c("Alias University", "University of Lynchburg", "Missing University"),
    year = c(2024, 2024, 2024),
    state = c("Massachusetts", "Virginia", "Ohio"),
    city = c("Boston", "Lynchburg", "Columbus"),
    opeid = c("00123456", "00372000", ""),
    stringsAsFactors = FALSE
  )
  readr::write_csv(fixture_df, input_path, na = "")

  setwd(fixture_root)
  env <- new.env(parent = globalenv())
  sys.source(file.path(fixture_root, "scripts", "build_dapip_crosswalk.R"), envir = env)
  env$main(c(
    "--input", input_path,
    "--output-prefix", output_prefix,
    "--cache-dir", file.path(fixture_root, "data_pipelines", "accreditation", "cache", "dapip"),
    "--manual-aliases", file.path(fixture_root, "data_pipelines", "accreditation", "dapip_manual_aliases.csv"),
    "--refresh", "false"
  ))

  matched_path <- paste0(output_prefix, "_institution_map.csv")
  unmatched_path <- paste0(output_prefix, "_institution_map_unmatched.csv")
  assert_true(file.exists(matched_path), "DAPIP matched crosswalk CSV should exist.")
  assert_true(file.exists(unmatched_path), "DAPIP unmatched crosswalk CSV should exist.")

  matched <- readr::read_csv(matched_path, show_col_types = FALSE)
  unmatched <- readr::read_csv(unmatched_path, show_col_types = FALSE)

  assert_equal(nrow(matched), 2L)
  assert_true(any(matched$match_method == "manual_alias"), "Expected one manual alias match.")
  lynchburg <- matched[as.character(matched$unitid) == "200", , drop = FALSE]
  assert_equal(lynchburg$dapip_id[[1]], 147730L)
  assert_equal(lynchburg$match_method[[1]], "opeid")

  assert_equal(nrow(unmatched), 1L)
  assert_equal(as.character(unmatched$unitid[[1]]), "300")
})

run_test("DAPIP crosswalk fixture tolerates missing opeid column", function() {
  fixture_root <- tempfile("dapip-fixture-no-opeid-")
  dir.create(fixture_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  dirs <- c(
    file.path(fixture_root, "scripts"),
    file.path(fixture_root, "scripts", "shared"),
    file.path(fixture_root, "data_pipelines"),
    file.path(fixture_root, "data_pipelines", "accreditation"),
    file.path(fixture_root, "data_pipelines", "accreditation", "cache")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  for (nm in c(
    "shared/utils.R",
    "shared/ipeds_paths.R",
    "shared/name_normalization.R",
    "shared/accreditation_helpers.R",
    "shared/dapip_helpers.R"
  )) {
    file.copy(
      file.path(root, "scripts", nm),
      file.path(fixture_root, "scripts", nm),
      overwrite = TRUE
    )
  }
  file.copy(
    file.path(root, "scripts", "build_dapip_crosswalk.R"),
    file.path(fixture_root, "scripts", "build_dapip_crosswalk.R"),
    overwrite = TRUE
  )

  helper_lines <- readLines(file.path(fixture_root, "scripts", "shared", "dapip_helpers.R"), warn = FALSE)
  helper_override <- c(
    helper_lines,
    "",
    "dapip_search_candidates <- function(unitid = NULL, opeid = NULL, institution_name = NULL, state = NULL, cache_dir, refresh = TRUE, verbose = FALSE) {",
    "  if (identical(trimws(as.character(unitid %||% '')), '232609')) {",
    "    return(tibble::tibble(",
    "      dapip_id = 147730L,",
    "      dapip_institution_name = 'University of Lynchburg',",
    "      dapip_state = 'VA',",
    "      dapip_opeid = '',",
    "      dapip_ipeds_unitid = '232609',",
    "      active_status = 'Active',",
    "      search_variant = 'fixture',",
    "      raw_payload = '{}'",
    "    ))",
    "  }",
    "  empty_dapip_search_results()",
    "}"
  )
  writeLines(
    helper_override,
    file.path(fixture_root, "scripts", "shared", "dapip_helpers.R"),
    useBytes = TRUE
  )

  writeLines(
    "unitid,opeid,tracker_institution_name,tracker_state,dapip_id,dapip_institution_name,dapip_state,notes",
    file.path(fixture_root, "data_pipelines", "accreditation", "dapip_manual_aliases.csv"),
    useBytes = TRUE
  )

  input_path <- file.path(fixture_root, "fixture_ipeds.csv")
  output_prefix <- file.path(fixture_root, "data_pipelines", "accreditation", "dapip")
  fixture_df <- data.frame(
    unitid = "232609",
    institution_name = "University of Lynchburg",
    year = 2024,
    state = "Virginia",
    city = "Lynchburg",
    stringsAsFactors = FALSE
  )
  readr::write_csv(fixture_df, input_path, na = "")

  setwd(fixture_root)
  env <- new.env(parent = globalenv())
  sys.source(file.path(fixture_root, "scripts", "build_dapip_crosswalk.R"), envir = env)
  env$main(c(
    "--input", input_path,
    "--output-prefix", output_prefix,
    "--cache-dir", file.path(fixture_root, "data_pipelines", "accreditation", "cache", "dapip"),
    "--manual-aliases", file.path(fixture_root, "data_pipelines", "accreditation", "dapip_manual_aliases.csv"),
    "--refresh", "false"
  ))

  matched <- readr::read_csv(paste0(output_prefix, "_institution_map.csv"), show_col_types = FALSE)
  assert_equal(nrow(matched), 1L)
  assert_equal(matched$dapip_id[[1]], 147730L)
})

run_test("DAPIP crosswalk fixture sends low-confidence search hits to unmatched", function() {
  fixture_root <- tempfile("dapip-fixture-low-confidence-")
  dir.create(fixture_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  dirs <- c(
    file.path(fixture_root, "scripts"),
    file.path(fixture_root, "scripts", "shared"),
    file.path(fixture_root, "data_pipelines"),
    file.path(fixture_root, "data_pipelines", "accreditation"),
    file.path(fixture_root, "data_pipelines", "accreditation", "cache")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  for (nm in c(
    "shared/utils.R",
    "shared/ipeds_paths.R",
    "shared/name_normalization.R",
    "shared/accreditation_helpers.R",
    "shared/dapip_helpers.R"
  )) {
    file.copy(
      file.path(root, "scripts", nm),
      file.path(fixture_root, "scripts", nm),
      overwrite = TRUE
    )
  }
  file.copy(
    file.path(root, "scripts", "build_dapip_crosswalk.R"),
    file.path(fixture_root, "scripts", "build_dapip_crosswalk.R"),
    overwrite = TRUE
  )

  writeLines(
    "unitid,opeid,tracker_institution_name,tracker_state,dapip_id,dapip_institution_name,dapip_state,notes",
    file.path(fixture_root, "data_pipelines", "accreditation", "dapip_manual_aliases.csv"),
    useBytes = TRUE
  )

  helper_lines <- readLines(file.path(fixture_root, "scripts", "shared", "dapip_helpers.R"), warn = FALSE)
  helper_override <- c(
    helper_lines,
    "",
    "dapip_search_candidates <- function(unitid = NULL, opeid = NULL, institution_name = NULL, state = NULL, cache_dir, refresh = TRUE, verbose = FALSE) {",
    "  tibble::tibble(",
    "    dapip_id = 211972L,",
    "    dapip_institution_name = 'Different College',",
    "    dapip_state = state,",
    "    dapip_opeid = '',",
    "    dapip_ipeds_unitid = '',",
    "    active_status = 'Active',",
    "    search_variant = 'fixture',",
    "    raw_payload = '{}'",
    "  )",
    "}"
  )
  writeLines(
    helper_override,
    file.path(fixture_root, "scripts", "shared", "dapip_helpers.R"),
    useBytes = TRUE
  )

  input_path <- file.path(fixture_root, "fixture_ipeds.csv")
  output_prefix <- file.path(fixture_root, "data_pipelines", "accreditation", "dapip")
  fixture_df <- data.frame(
    unitid = "232609",
    institution_name = "University of Lynchburg",
    year = 2024,
    state = "Virginia",
    city = "Lynchburg",
    opeid = "",
    stringsAsFactors = FALSE
  )
  readr::write_csv(fixture_df, input_path, na = "")

  setwd(fixture_root)
  env <- new.env(parent = globalenv())
  sys.source(file.path(fixture_root, "scripts", "build_dapip_crosswalk.R"), envir = env)
  env$main(c(
    "--input", input_path,
    "--output-prefix", output_prefix,
    "--cache-dir", file.path(fixture_root, "data_pipelines", "accreditation", "cache", "dapip"),
    "--manual-aliases", file.path(fixture_root, "data_pipelines", "accreditation", "dapip_manual_aliases.csv"),
    "--refresh", "false"
  ))

  matched <- readr::read_csv(paste0(output_prefix, "_institution_map.csv"), show_col_types = FALSE)
  unmatched <- readr::read_csv(paste0(output_prefix, "_institution_map_unmatched.csv"), show_col_types = FALSE)

  assert_equal(nrow(matched), 0L)
  assert_equal(nrow(unmatched), 1L)
  assert_equal(as.character(unmatched$unitid[[1]]), "232609")
})
