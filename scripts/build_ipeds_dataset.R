main <- function(cli_args = NULL) {
  args <- if (is.null(cli_args)) commandArgs(trailingOnly = TRUE) else cli_args

  paths_env <- new.env(parent = baseenv())
  sys.source(file.path(getwd(), "scripts", "shared", "ipeds_paths.R"), envir = paths_env)
  ipeds_layout <- get("ipeds_layout", envir = paths_env, inherits = FALSE)

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
  paths <- ipeds_layout(
    root = ".",
    output_stem = output_stem,
    start_year = as.integer(start_year),
    end_year = as.integer(end_year)
  )

  project_dir <- getwd()
  script_dir <- file.path(project_dir, "scripts")

  raw_main <- load_main(file.path(script_dir, "collect_ipeds_data.R"))
  canonical_main <- load_main(file.path(script_dir, "build_ipeds_canonical_dataset.R"))

  message("Step 1: Building raw IPEDS dataset ...")
  raw_main(c(
    "--start-year", start_year,
    "--end-year", end_year,
    "--output-stem", output_stem,
    "--force-rebuild", force_rebuild
  ))

  message("Step 2: Building canonical IPEDS dataset ...")
  canonical_main(c(
    "--raw", paths$raw_csv,
    "--catalog", paths$selected_file_catalog_csv,
    "--output", paths$canonical_csv,
    "--expanded-output", paths$dataset_csv
  ))

  invisible(list(
    raw = normalizePath(paths$raw_csv, winslash = "/", mustWork = FALSE),
    canonical = normalizePath(paths$canonical_csv, winslash = "/", mustWork = FALSE),
    extended = normalizePath(paths$dataset_csv, winslash = "/", mustWork = FALSE)
  ))
}

if (sys.nframe() == 0) {
  main()
}
