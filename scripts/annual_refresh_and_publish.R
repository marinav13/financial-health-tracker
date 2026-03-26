main <- function(cli_args = NULL) {
  args <- if (is.null(cli_args)) commandArgs(trailingOnly = TRUE) else cli_args

  get_arg_value <- function(flag, default = NULL) {
    idx <- match(flag, args)
    if (!is.na(idx) && idx < length(args)) args[[idx + 1L]] else default
  }

  has_flag <- function(flag) {
    flag %in% args
  }

  load_main <- function(path) {
    env <- new.env(parent = globalenv())
    sys.source(path, envir = env)
    if (!exists("main", envir = env, inherits = FALSE)) {
      stop("No main() found in script: ", path)
    }
    get("main", envir = env, inherits = FALSE)
  }

  start_year <- get_arg_value("--start-year", "2014")
  end_year <- get_arg_value("--end-year", format(Sys.Date(), "%Y"))
  workbook_output <- get_arg_value("--workbook-output", "./workbooks/ipeds_financial_health_article_workbook_r.xml")
  looker_input <- get_arg_value(
    "--looker-input",
    sprintf("./looker_studio/ipeds_financial_health_looker_ready_%s_%s.csv", start_year, end_year)
  )
  reporting_input <- get_arg_value(
    "--reporting-input",
    sprintf("./reporting/ipeds_financial_health_reporting_%s_%s.csv", start_year, end_year)
  )
  sheet <- get_arg_value("--sheet", NULL)
  create <- get_arg_value("--create", NULL)
  tab <- get_arg_value("--tab", "looker_ready")
  email <- get_arg_value("--email", NA_character_)
  auth_json <- get_arg_value("--auth-json", NA_character_)
  skip_publish <- has_flag("--skip-publish")

  project_dir <- getwd()

  script_dir <- file.path(project_dir, "scripts")
  ipeds_main <- load_main(file.path(script_dir, "build_ipeds_tracker_dataset.R"))
  looker_main <- load_main(file.path(script_dir, "build_looker_ready_metrics.R"))
  workbook_main <- load_main(file.path(script_dir, "build_article_workbook.R"))
  publish_main <- load_main(file.path(script_dir, "publish_to_google_sheets.R"))

  message("Step 1: Rebuilding raw IPEDS data ...")
  ipeds_main(c("--start-year", start_year, "--end-year", end_year))

  message("Step 2: Rebuilding Looker and reporting datasets ...")
  looker_main()

  if (!file.exists(reporting_input)) {
    stop("Expected reporting file not found after rebuild: ", reporting_input)
  }

  message("Step 3: Rebuilding workbook ...")
  workbook_main(c(
    "--input", reporting_input,
    "--output", workbook_output
  ))

  if (!skip_publish) {
    if (is.null(sheet) && is.null(create)) {
      message("Step 4: Skipping Google Sheets publish because no --sheet or --create argument was provided.")
    } else {
      if (!file.exists(looker_input)) {
        stop("Expected Looker-ready file not found after rebuild: ", looker_input)
      }

      publish_args <- c("--input", looker_input, "--tab", tab)
      if (!is.null(sheet)) publish_args <- c(publish_args, "--sheet", sheet)
      if (!is.null(create)) publish_args <- c(publish_args, "--create", create)
      if (!is.na(email) && nzchar(email)) publish_args <- c(publish_args, "--email", email)
      if (!is.na(auth_json) && nzchar(auth_json)) publish_args <- c(publish_args, "--auth-json", auth_json)

      message("Step 4: Publishing Looker-ready data to Google Sheets ...")
      publish_main(publish_args)
    }
  } else {
    message("Step 4: Google Sheets publish skipped by --skip-publish.")
  }

  message("Done.")
  invisible(list(
    start_year = start_year,
    end_year = end_year,
    looker_input = normalizePath(looker_input, winslash = "/", mustWork = FALSE),
    reporting_input = normalizePath(reporting_input, winslash = "/", mustWork = FALSE),
    workbook_output = normalizePath(workbook_output, winslash = "/", mustWork = FALSE)
  ))
}

if (sys.nframe() == 0) {
  main()
}
