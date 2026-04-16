# ====== BUILD IPEDS DATASET ORCHESTRATOR ============================================
# This is the top-level orchestration script that runs the complete IPEDS data pipeline.
#
# INPUT:  CLI arguments specifying year range and output naming
#         - Calls collect_ipeds_data.R to download and extract IPEDS tables from the
#           federal IPEDS database (https://nces.ed.gov/ipeds/datacenter/)
#         - Calls build_ipeds_canonical_dataset.R to transform raw data into the
#           final canonical dataset
#
# OUTPUT: Three CSV files
#         1. ipeds_financial_health_raw.csv - Raw institution-year data with all collected fields
#         2. ipeds_financial_health_canonical.csv - Cleaned, decoded dataset for the website
#         3. ipeds_financial_health_dataset.csv - Extended dataset with additional fields for analysis
#
# USAGE:  Run from project root with:
#         Rscript scripts/build_ipeds_dataset.R [--start-year YYYY] [--end-year YYYY] [--force-rebuild TRUE/FALSE]
#
#         Example: Rscript scripts/build_ipeds_dataset.R --start-year 2014 --end-year 2024 --force-rebuild FALSE
#
# DOMAIN CONCEPTS:
# - IPEDS: Integrated Postsecondary Education Data System, federal college database maintained by NCES
# - unitid: Unique identifier for each U.S. college/university in IPEDS
# - canonical: The authoritative merged dataset used consistently across the website and workbook
# - HCM2: Heightened Cash Monitoring Level 2 - federal oversight flag indicating a college
#   has mishandled federal aid funds and requires closer monitoring by the Department of Education
#
main <- function(cli_args = NULL) {
  # Load utility functions for argument parsing and project path management
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args          <- parse_cli_args(cli_args)
  ipeds         <- load_ipeds_paths()
  ipeds_layout  <- ipeds$ipeds_layout
  # Helper function: extract argument value by flag name, or return default
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)

  # ---------------------------------------------------------------------------
  # HELPER: Load a main() function from another R script in a fresh environment
  # to avoid namespace pollution.
  # Returns the main function so it can be called directly.
  # Exits with error if no main() is defined in the target script.
  load_main <- function(path) {
    env <- new.env(parent = globalenv())
    sys.source(path, envir = env)
    if (!exists("main", envir = env, inherits = FALSE)) {
      stop("No main() found in script: ", path)
    }
    get("main", envir = env, inherits = FALSE)
  }

  # ---------------------------------------------------------------------------
  # PARSE COMMAND-LINE ARGUMENTS
  # These control which years are processed and where output is written
  start_year <- get_arg_value("--start-year", "2014")
  end_year <- get_arg_value("--end-year", "2024")
  output_stem <- get_arg_value("--output-stem", "ipeds_financial_health")
  force_rebuild <- get_arg_value("--force-rebuild", "FALSE")
  # Create directory structure for output files
  paths <- ipeds_layout(
    root = ".",
    output_stem = output_stem,
    start_year = as.integer(start_year),
    end_year = as.integer(end_year)
  )

  project_dir <- getwd()
  script_dir <- file.path(project_dir, "scripts")

  # Load the main() functions from the two pipeline stages so we can call them directly
  raw_main <- load_main(file.path(script_dir, "collect_ipeds_data.R"))
  canonical_main <- load_main(file.path(script_dir, "build_ipeds_canonical_dataset.R"))

  # ---------------------------------------------------------------------------
  # STEP 1: COLLECT AND EXTRACT RAW IPEDS DATA
  # This stage downloads IPEDS tables from the federal database, extracts them,
  # matches variable names year-by-year, and writes one wide raw dataset
  # (ipeds_financial_health_raw.csv) with institution-years as rows and variables as columns.
  message("Step 1: Building raw IPEDS dataset ...")
  raw_main(c(
    "--start-year", start_year,
    "--end-year", end_year,
    "--output-stem", output_stem,
    "--force-rebuild", force_rebuild
  ))

  # ---------------------------------------------------------------------------
  # STEP 2: TRANSFORM RAW DATA INTO CANONICAL DATASET
  # This stage reads the raw dataset and applies transformations:
  # - Decodes numeric IPEDS codes into readable labels (e.g., sector code "1" -> "Public")
  # - Merges auxiliary tables (enrollment, staffing, loans) by year and unitid
  # - Computes derived fields (percentiles, ratios, change-over-time metrics)
  # - Filters to the eligible 2024 cohort (active, degree-granting schools)
  # - Outputs canonical.csv (for website/workbook) and dataset.csv (extended analysis export)
  message("Step 2: Building canonical IPEDS dataset ...")
  canonical_main(c(
    "--raw", paths$raw_csv,
    "--catalog", paths$selected_file_catalog_csv,
    "--output", paths$canonical_csv,
    "--expanded-output", paths$dataset_csv
  ))

  # Return the output file paths for downstream use
  invisible(list(
    raw = normalizePath(paths$raw_csv, winslash = "/", mustWork = FALSE),
    canonical = normalizePath(paths$canonical_csv, winslash = "/", mustWork = FALSE),
    extended = normalizePath(paths$dataset_csv, winslash = "/", mustWork = FALSE)
  ))
}

# Execute main() if this script is run directly from the command line
# (not sourced as a library into another script)
if (sys.nframe() == 0) {
  main()
}
