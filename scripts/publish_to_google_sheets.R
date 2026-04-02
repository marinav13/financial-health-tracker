main <- function(cli_args = NULL) {
  args <- if (is.null(cli_args)) commandArgs(trailingOnly = TRUE) else cli_args

  get_arg_value <- function(flag, default = NULL) {
    idx <- match(flag, args)
    if (!is.na(idx) && idx < length(args)) args[[idx + 1L]] else default
  }

  has_flag <- function(flag) {
    flag %in% args
  }

  ensure_packages <- function(pkgs) {
    missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing) > 0) {
      install.packages(missing, repos = "https://cloud.r-project.org")
    }
    invisible(lapply(pkgs, library, character.only = TRUE))
  }

  ensure_packages(c("googlesheets4", "readr"))

  input_csv <- get_arg_value(
    "--input",
    file.path(getwd(), "ipeds", "ipeds_financial_health_dataset_2014_2024.csv")
  )
  sheet_id_or_url <- get_arg_value("--sheet", NULL)
  tab_name <- get_arg_value("--tab", "ipeds_dataset")
  create_name <- get_arg_value("--create", NULL)
  email <- get_arg_value("--email", NA_character_)
  auth_json <- get_arg_value("--auth-json", Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = NA_character_))
  cache_dir <- get_arg_value("--cache", file.path(getwd(), ".secrets", "googlesheets4"))
  verbose <- has_flag("--verbose")

  if (!file.exists(input_csv)) {
    stop("Input CSV not found: ", input_csv)
  }

  if (is.null(sheet_id_or_url) && is.null(create_name)) {
    stop("Provide either --sheet <Google Sheet URL or ID> or --create <new spreadsheet name>.")
  }

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  auth_json <- if (!is.na(auth_json) && nzchar(auth_json) && file.exists(auth_json)) auth_json else NA_character_
  email <- if (!is.na(email) && nzchar(email)) email else NA_character_

  if (verbose) {
    message("Reading input CSV: ", input_csv)
  }
  data <- readr::read_csv(input_csv, show_col_types = FALSE, progress = FALSE, guess_max = 50000)

  if (verbose) {
    message("Rows: ", nrow(data), " | Columns: ", ncol(data))
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
    message("Writing to worksheet tab: ", tab_name)
  }
  googlesheets4::sheet_write(data = data, ss = target_sheet, sheet = tab_name)

  info <- tryCatch(googlesheets4::gs4_get(target_sheet), error = function(e) NULL)
  if (!is.null(info) && "spreadsheet_url" %in% names(info)) {
    message("Published to Google Sheet:")
    message(" - URL: ", info$spreadsheet_url[[1]])
  } else {
    message("Published to Google Sheet target: ", as.character(target_sheet))
  }
  message(" - Tab: ", tab_name)
  invisible(list(sheet = target_sheet, tab = tab_name, rows = nrow(data), cols = ncol(data)))
}

if (sys.nframe() == 0) {
  main()
}
