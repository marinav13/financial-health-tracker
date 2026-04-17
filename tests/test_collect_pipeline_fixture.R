run_test("IPEDS collector pipeline fixture", function() {
  workspace <- file.path(tempdir(), paste0("collect-fixture-", as.hexmode(sample.int(10^7, 1))))
  dir.create(file.path(workspace, "scripts", "shared"), recursive = TRUE, showWarnings = FALSE)

  fixture_scripts <- c(
    file.path(root, "scripts", "collect_ipeds_data.R"),
    file.path(root, "scripts", "shared", "utils.R"),
    file.path(root, "scripts", "shared", "ipeds_paths.R"),
    file.path(root, "scripts", "shared", "ipeds_helpers.R"),
    file.path(root, "scripts", "shared", "ipeds_collector_helpers.R")
  )
  file.copy(
    fixture_scripts,
    c(
      file.path(workspace, "scripts", "collect_ipeds_data.R"),
      file.path(workspace, "scripts", "shared", "utils.R"),
      file.path(workspace, "scripts", "shared", "ipeds_paths.R"),
      file.path(workspace, "scripts", "shared", "ipeds_helpers.R"),
      file.path(workspace, "scripts", "shared", "ipeds_collector_helpers.R")
    ),
    overwrite = TRUE
  )

  write_fixture_archives <- function(data_zip_path, dict_zip_path) {
    assert_true(requireNamespace("openxlsx", quietly = TRUE), "openxlsx is required to build the collector fixture archives.")
    helper_old_wd <- getwd()
    on.exit(setwd(helper_old_wd), add = TRUE)

    data_dir <- tempfile(pattern = "collect-data-")
    dict_dir <- tempfile(pattern = "collect-dict-")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(dict_dir, recursive = TRUE, showWarnings = FALSE)

    data_csv_path <- file.path(data_dir, "HD2024.csv")
    readr::write_csv(
      tibble::tibble(
        UNITID = "100",
        INSTNM = "Fiction College",
        CITY = "Boston",
        STABBR = "MA"
      ),
      data_csv_path,
      na = ""
    )

    setwd(data_dir)
    utils::zip(zipfile = data_zip_path, files = "HD2024.csv")

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Varlist")
    openxlsx::writeData(
      wb,
      "Varlist",
      x = data.frame(
        A = c("1", "2", "3"),
        B = c("INSTNM", "CITY", "STABBR"),
        C = c("", "", ""),
        D = c("", "", ""),
        E = c("", "", ""),
        F = c("", "", ""),
        G = c("Institution (entity) name", "City location of institution", "State abbreviation"),
        stringsAsFactors = FALSE
      ),
      colNames = FALSE
    )
    dict_xlsx_path <- file.path(dict_dir, "HD2024.xlsx")
    openxlsx::saveWorkbook(wb, dict_xlsx_path, overwrite = TRUE)

    setwd(dict_dir)
    utils::zip(zipfile = dict_zip_path, files = "HD2024.xlsx")

    assert_true(file.exists(data_zip_path), "Expected fixture data ZIP to be created.")
    assert_true(file.exists(dict_zip_path), "Expected fixture dictionary ZIP to be created.")
  }

  old_wd <- setwd(workspace)
  on.exit(setwd(old_wd), add = TRUE)

  env <- new.env(parent = globalenv())
  sys.source(file.path(workspace, "scripts", "collect_ipeds_data.R"), envir = env)

  source(file.path(workspace, "scripts", "shared", "utils.R"))
  ipeds <- load_ipeds_paths()
  paths <- ipeds$ipeds_layout(root = ".", output_stem = "fixture_collect", start_year = 2024L, end_year = 2024L)
  ipeds$ensure_ipeds_layout_dirs(paths)

  catalog_html <- paste0(
    '<html><body><table>',
    '<tr class="idc_gridviewrow">',
    '<td>2024</td>',
    '<td>Institutional Characteristics</td>',
    '<td>Fixture HD table</td>',
    '<td><a href="https://example.com/data/HD2024.zip">HD2024</a></td>',
    '<td><a href="https://example.com/info/HD2024">Info</a></td>',
    '<td>unused</td>',
    '<td><a href="https://example.com/dict/HD2024.zip">Dictionary</a></td>',
    '</tr>',
    '</table></body></html>'
  )
  writeLines(catalog_html, paths$catalog_html, useBytes = TRUE)

  write_fixture_archives(
    data_zip_path = file.path(paths$cache_data_dir, "HD2024.zip"),
    dict_zip_path = file.path(paths$cache_dict_dir, "HD2024.zip")
  )

  result <- env$main(c(
    "--start-year", "2024",
    "--end-year", "2024",
    "--output-stem", "fixture_collect",
    "--force-rebuild", "TRUE"
  ))

  dataset <- suppressMessages(readr::read_csv(result$dataset, show_col_types = FALSE))
  audit <- suppressMessages(readr::read_csv(result$field_resolution_audit, show_col_types = FALSE))
  selected_catalog <- suppressMessages(readr::read_csv(paths$selected_file_catalog_csv, show_col_types = FALSE))

  assert_identical(nrow(dataset), 1L)
  assert_identical(as.character(dataset$unitid[[1]]), "100")
  assert_identical(dataset$institution_name[[1]], "Fiction College")
  assert_identical(dataset$city[[1]], "Boston")
  assert_identical(dataset$state[[1]], "MA")
  assert_identical(dataset$institution_unique_name[[1]], "Fiction College | Boston | MA")
  assert_identical(dataset$year[[1]], 2024)

  audit_subset <- audit[audit$output %in% c("institution_name", "city", "state"), , drop = FALSE]
  assert_identical(nrow(audit_subset), 3L)
  assert_identical(audit_subset$resolved_var_name[audit_subset$output == "institution_name"][[1]], "INSTNM")
  assert_identical(audit_subset$resolved_var_name[audit_subset$output == "city"][[1]], "CITY")
  assert_identical(audit_subset$resolved_var_name[audit_subset$