main <- function(cli_args = NULL) {
  args <- if (is.null(cli_args)) commandArgs(trailingOnly = TRUE) else cli_args

  shared_utils_path <- file.path(getwd(), "scripts", "shared", "script_utils.R")
  if (!file.exists(shared_utils_path)) {
    stop("Shared script helper not found: ", shared_utils_path)
  }
  sys.source(shared_utils_path, envir = environment())

  ensure_packages <- function(pkgs) {
    missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing) > 0) {
      install.packages(missing, repos = "https://cloud.r-project.org")
    }
    invisible(lapply(pkgs, library, character.only = TRUE))
  }

  ensure_packages(c("googlesheets4", "jsonlite", "readr", "readxl"))

  input_csv <- get_arg_value(
    args,
    "--input",
    file.path(getwd(), "data_pipelines", "federal_closure", "derived", "closure_status_tracker_matches.csv")
  )
  summary_json <- get_arg_value(
    args,
    "--summary-json",
    file.path(getwd(), "data", "closure_status_by_unitid.json")
  )
  weekly_zip <- get_arg_value(args, "--weekly-zip", NA_character_)
  weekly_metadata <- get_arg_value(args, "--weekly-metadata", NA_character_)
  sheet_id_or_url <- get_arg_value(args, "--sheet", NULL)
  create_name <- get_arg_value(args, "--create", NULL)
  default_tab <- if (!is.na(weekly_zip) && nzchar(weekly_zip)) "weekly_closed_school_report" else "tracker_closure_status"
  default_summary_tab <- if (!is.na(weekly_zip) && nzchar(weekly_zip)) "weekly_closed_school_summary" else "tracker_closure_summary"
  status_tab <- get_arg_value(args, "--tab", default_tab)
  summary_tab <- get_arg_value(args, "--summary-tab", default_summary_tab)
  email <- get_arg_value(args, "--email", NA_character_)
  auth_json <- get_arg_value(args, "--auth-json", Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = NA_character_))
  cache_dir <- get_arg_value(args, "--cache", file.path(getwd(), ".secrets", "googlesheets4"))
  verbose <- has_flag(args, "--verbose")

  weekly_mode <- !is.na(weekly_zip) && nzchar(weekly_zip)
  if (weekly_mode) {
    require_file(weekly_zip, "Weekly closed school search ZIP")
  } else {
    require_files(c(input_csv, summary_json), "Closure spreadsheet inputs")
  }

  if (is.null(sheet_id_or_url) && is.null(create_name)) {
    stop("Provide either --sheet <Google Sheet URL or ID> or --create <new spreadsheet name>.")
  }

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  auth_json <- if (!is.na(auth_json) && nzchar(auth_json) && file.exists(auth_json)) auth_json else NA_character_
  email <- if (!is.na(email) && nzchar(email)) email else NA_character_

  if (weekly_mode) {
    read_weekly_zip <- function(zip_path) {
      tmp_dir <- tempfile("weekly-closed-school-")
      dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
      on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
      utils::unzip(zip_path, exdir = tmp_dir)

      members <- list.files(tmp_dir, recursive = TRUE, full.names = TRUE)
      if (!length(members)) {
        stop("Weekly closed school ZIP did not contain any files: ", zip_path)
      }

      sheet_candidates <- members[grepl("\\.(xlsx|xls)$", members, ignore.case = TRUE)]
      csv_candidates <- members[grepl("\\.(csv|txt)$", members, ignore.case = TRUE)]

      if (length(sheet_candidates) > 0) {
        workbook_path <- sheet_candidates[[1]]
        sheets <- readxl::excel_sheets(workbook_path)
        if (!length(sheets)) {
          stop("No sheets found in weekly closed school workbook: ", workbook_path)
        }

        sheet_sizes <- lapply(sheets, function(sheet_name) {
          df <- readxl::read_excel(workbook_path, sheet = sheet_name, guess_max = 1000)
          list(name = sheet_name, rows = nrow(df), cols = ncol(df), data = df)
        })
        chosen <- sheet_sizes[[which.max(vapply(sheet_sizes, function(item) item$rows * max(item$cols, 1L), numeric(1)))]]
        if (verbose) {
          message("Read weekly workbook sheet: ", chosen$name, " (", chosen$rows, " rows)")
        }
        return(chosen$data)
      }

      if (length(csv_candidates) > 0) {
        csv_path <- csv_candidates[[1]]
        if (verbose) {
          message("Read weekly CSV file: ", csv_path)
        }
        return(readr::read_csv(csv_path, show_col_types = FALSE, progress = FALSE))
      }

      stop("Could not find an .xlsx, .xls, .csv, or .txt file inside the weekly closed school ZIP: ", zip_path)
    }

    if (verbose) {
      message("Reading weekly closed school ZIP: ", weekly_zip)
    }
    matches <- read_weekly_zip(weekly_zip)

    metadata_source <- if (!is.na(weekly_metadata) && nzchar(weekly_metadata) && file.exists(weekly_metadata)) {
      jsonlite::fromJSON(weekly_metadata, simplifyVector = TRUE)
    } else {
      list()
    }
    summary_rows <- data.frame(
      metric = c("source_file", "proc_date", "doc_storage_id", "matched_rows", "generated_on"),
      value = c(
        if (!is.null(metadata_source$filename)) as.character(metadata_source$filename) else basename(weekly_zip),
        if (!is.null(metadata_source$procDate)) as.character(metadata_source$procDate) else "",
        if (!is.null(metadata_source$docStorageId)) as.character(metadata_source$docStorageId) else "",
        nrow(matches),
        as.character(Sys.Date())
      ),
      stringsAsFactors = FALSE
    )
  } else {
    if (verbose) {
      message("Reading closure matches CSV: ", input_csv)
    }
    matches <- readr::read_csv(input_csv, show_col_types = FALSE, progress = FALSE)

    if (verbose) {
      message("Rows: ", nrow(matches), " | Columns: ", ncol(matches))
      message("Reading closure summary JSON: ", summary_json)
    }
    summary_source <- jsonlite::fromJSON(summary_json, simplifyVector = TRUE)
    source_file <- if (!is.null(summary_source$source_file)) as.character(summary_source$source_file) else ""
    as_of_date <- if (!is.null(summary_source$as_of_date)) as.character(summary_source$as_of_date) else ""
    matched_schools <- if (!is.null(summary_source$schools)) length(summary_source$schools) else nrow(matches)
    latest_close_year <- if (nrow(matches) > 0 && "close_year" %in% names(matches) && any(!is.na(matches$close_year))) {
      max(as.integer(matches$close_year), na.rm = TRUE)
    } else {
      NA_integer_
    }
    summary_rows <- data.frame(
      metric = c(
        "source_file",
        "as_of_date",
        "matched_rows",
        "matched_schools",
        "latest_close_year",
        "generated_on"
      ),
      value = c(
        source_file,
        as_of_date,
        nrow(matches),
        matched_schools,
        latest_close_year,
        as.character(Sys.Date())
      ),
      stringsAsFactors = FALSE
    )
  }

  if (!is.na(auth_json)) {
    if (verbose) {
      message("Authenticating with service account JSON: ", auth_json)
    }
    googlesheets4::gs4_auth(
      path = auth_json,
      scopes = "spreadsheets",
      cache = FALSE
    )
  } else {
    if (verbose) {
      message("Authenticating with interactive OAuth cache at: ", cache_dir)
    }
    googlesheets4::gs4_auth(
      email = email,
      scopes = "spreadsheets",
      cache = cache_dir
    )
  }

  target_sheet <- sheet_id_or_url
  if (is.null(target_sheet)) {
    if (verbose) {
      message("Creating new spreadsheet: ", create_name)
    }
    target_sheet <- googlesheets4::gs4_create(name = create_name)
  }

  if (verbose) {
    message("Writing closure status tab: ", status_tab)
  }
  googlesheets4::sheet_write(data = matches, ss = target_sheet, sheet = status_tab)

  if (verbose) {
    message("Writing closure summary tab: ", summary_tab)
  }
  googlesheets4::sheet_write(data = summary_rows, ss = target_sheet, sheet = summary_tab)

  info <- tryCatch(googlesheets4::gs4_get(target_sheet), error = function(e) NULL)
  if (!is.null(info) && "spreadsheet_url" %in% names(info)) {
    message("Published closure spreadsheet:")
    message(" - URL: ", info$spreadsheet_url[[1]])
  } else {
    message("Published closure spreadsheet target: ", as.character(target_sheet))
  }
  message(" - Status tab: ", status_tab)
  message(" - Summary tab: ", summary_tab)
  invisible(list(
    sheet = target_sheet,
    status_tab = status_tab,
    summary_tab = summary_tab,
    rows = nrow(matches)
  ))
}

if (sys.nframe() == 0) {
  main()
}
