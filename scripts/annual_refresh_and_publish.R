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
  reporting_input <- get_arg_value(
    "--reporting-input",
    sprintf("./ipeds/ipeds_financial_health_dataset_%s_%s.csv", start_year, end_year)
  )
  sheet <- get_arg_value("--sheet", NULL)
  create <- get_arg_value("--create", NULL)
  tab <- get_arg_value("--tab", "ipeds_dataset")
  email <- get_arg_value("--email", NA_character_)
  auth_json <- get_arg_value("--auth-json", NA_character_)
  skip_publish <- has_flag("--skip-publish")

  project_dir <- getwd()

  script_dir <- file.path(project_dir, "scripts")
  ipeds_main <- load_main(file.path(script_dir, "build_ipeds_dataset.R"))
  outcomes_main <- load_main(file.path(script_dir, "build_outcomes_join.R"))
  workbook_main <- load_main(file.path(script_dir, "build_article_workbook.R"))

  message("Step 1: Rebuilding IPEDS datasets ...")
  ipeds_main(c("--start-year", start_year, "--end-year", end_year))

  if (!file.exists(reporting_input)) {
    stop("Expected canonical dataset not found after rebuild: ", reporting_input)
  }

  message("Step 2: Rebuilding Scorecard and graduation-rate joins ...")
  outcomes_main(c("--input", reporting_input))

  message("Step 3: Rebuilding workbook ...")
  workbook_main(c(
    "--input", reporting_input,
    "--output", workbook_output
  ))

  if (!skip_publish) {
    message("Step 4: Google Sheets publish is no longer part of the default refresh flow.")
  } else {
    message("Step 4: Google Sheets publish skipped by --skip-publish.")
  }

  message("Done.")
  invisible(list(
    start_year = start_year,
    end_year = end_year,
    reporting_input = normalizePath(reporting_input, winslash = "/", mustWork = FALSE),
    workbook_output = normalizePath(workbook_output, winslash = "/", mustWork = FALSE)
  ))
}

if (sys.nframe() == 0) {
  main()
}
