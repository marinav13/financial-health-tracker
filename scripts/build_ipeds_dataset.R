main <- function(cli_args = NULL) {
  args <- if (is.null(cli_args)) commandArgs(trailingOnly = TRUE) else cli_args

  get_arg_value <- function(flag, default = NULL) {
    idx <- match(flag, args)
    if (!is.na(idx) && idx < length(args)) args[[idx + 1L]] else default
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
  end_year <- get_arg_value("--end-year", "2024")
  output_stem <- get_arg_value("--output-stem", "ipeds_financial_health")
  force_rebuild <- get_arg_value("--force-rebuild", "FALSE")

  project_dir <- getwd()
  script_dir <- file.path(project_dir, "scripts")

  raw_main <- load_main(file.path(script_dir, "build_ipeds_tracker_dataset.R"))
  canonical_main <- load_main(file.path(script_dir, "build_ipeds_canonical_dataset.R"))

  message("Step 1: Building raw IPEDS tracker dataset ...")
  raw_main(c(
    "--start-year", start_year,
    "--end-year", end_year,
    "--output-stem", output_stem,
    "--force-rebuild", force_rebuild
  ))

  message("Step 2: Building canonical IPEDS dataset ...")
  canonical_main(c(
    "--raw", sprintf("./ipeds/%s_raw_%s_%s.csv", output_stem, start_year, end_year),
    "--catalog", sprintf("./ipeds/%s_selected_file_catalog.csv", output_stem),
    "--output", sprintf("./ipeds/%s_dataset_%s_%s.csv", output_stem, start_year, end_year),
    "--expanded-output", sprintf("./ipeds/%s_dataset_%s_%s.csv", output_stem, start_year, end_year)
  ))

  invisible(list(
    raw = normalizePath(sprintf("./ipeds/%s_raw_%s_%s.csv", output_stem, start_year, end_year), winslash = "/", mustWork = FALSE),
    canonical = normalizePath(sprintf("./ipeds/%s_dataset_%s_%s.csv", output_stem, start_year, end_year), winslash = "/", mustWork = FALSE)
  ))
}

if (sys.nframe() == 0) {
  main()
}
