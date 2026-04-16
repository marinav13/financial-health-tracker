# scripts/shared/utils.R
#
# Common utilities shared across all pipeline scripts.
# Source this file at the top of each script's main() function:
#
#   source(file.path(getwd(), "scripts", "shared", "utils.R"))
#
# After sourcing, define lightweight local wrappers for the arg helpers
# so existing call sites inside main() don't need to change:
#
#   get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)
#   has_flag      <- function(flag)                 arg_has(args, flag)

# ---------------------------------------------------------------------------
# Null coalescing operator
# ---------------------------------------------------------------------------

# FUNCTION: %||%
# PURPOSE: Provides a null-coalescing operator (similar to ?? in other languages).
#          Returns y if x is NULL, has length 0, or is a single NA value.
#          Otherwise returns x.
#
# PARAMETERS:
#   x – The primary value to return (may be any R object)
#   y – The fallback value if x is null-like
#
# RETURNS: Either x or y
#
# EXAMPLES:
#   NULL %||% "default"          # Returns "default"
#   NA %||% "default"            # Returns "default"
#   "value" %||% "default"       # Returns "value"
#   c() %||% "default"           # Returns "default" (empty vector)
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1L && is.na(x))) y else x
}

# ---------------------------------------------------------------------------
# CLI argument parsing
# ---------------------------------------------------------------------------

# FUNCTION: parse_cli_args
# PURPOSE: Extracts command-line arguments passed to the script.
#          Allows scripts to be called programmatically (from tests or other scripts)
#          by accepting arguments directly, or falls back to parsing the actual
#          command-line arguments when called interactively.
#
# PARAMETERS:
#   cli_args – Optional vector of arguments (useful for testing or calling from
#              another script). If NULL, uses actual command-line arguments.
#
# RETURNS: Character vector of parsed arguments (without the script name)
#
# EXAMPLES:
#   # When called from command line: Rscript my_script.R --flag value
#   args <- parse_cli_args()  # Returns c("--flag", "value")
#
#   # When called programmatically:
#   args <- parse_cli_args(c("--flag", "value"))  # Returns c("--flag", "value")
parse_cli_args <- function(cli_args = NULL) {
  if (is.null(cli_args)) commandArgs(trailingOnly = TRUE) else cli_args
}

# FUNCTION: get_arg
# PURPOSE: Retrieves the value following a command-line flag.
#          Used to parse key-value arguments like --input myfile.csv
#
# PARAMETERS:
#   args – Character vector of arguments (typically from parse_cli_args())
#   flag – The flag to search for (e.g., "--input")
#   default – Value to return if flag is not found or has no value after it
#
# RETURNS: The value following the flag, or default if flag is absent
#
# EXAMPLES:
#   args <- c("--input", "data.csv", "--output", "results.csv")
#   get_arg(args, "--input", NA)      # Returns "data.csv"
#   get_arg(args, "--missing", NA)    # Returns NA (not found)
#   get_arg(args, "--output", "out")  # Returns "results.csv"
get_arg <- function(args, flag, default = NULL) {
  idx <- match(flag, args)
  if (!is.na(idx) && idx < length(args)) args[[idx + 1L]] else default
}

# FUNCTION: arg_has
# PURPOSE: Checks whether a boolean flag is present in the argument list.
#          Used for flags that don't take values, like --verbose or --dry-run
#
# PARAMETERS:
#   args – Character vector of arguments (typically from parse_cli_args())
#   flag – The flag to search for (e.g., "--verbose")
#
# RETURNS: TRUE if the flag is present, FALSE otherwise
#
# EXAMPLES:
#   args <- c("--input", "data.csv", "--verbose")
#   arg_has(args, "--verbose")  # Returns TRUE
#   arg_has(args, "--quiet")    # Returns FALSE
arg_has <- function(args, flag) {
  isTRUE(flag %in% args)
}

# ---------------------------------------------------------------------------
# R library path setup (for GitHub Actions / renv environments)
# ---------------------------------------------------------------------------

# FUNCTION: setup_r_libs
# PURPOSE: Configures R's library search path for GitHub Actions or renv-managed
#          environments. Creates and registers the user library path so packages
#          can be installed and loaded properly in CI/CD environments.
#
# PARAMETERS: None
#
# RETURNS: NULL (invisibly); modifies .libPaths() as a side effect
#
# NOTE: Reads from the R_LIBS_USER environment variable. If not set, does nothing.
#       This is typically used at the very beginning of scripts running in CI/CD.
#
# EXAMPLES:
#   setup_r_libs()  # Call once at the top of main()
setup_r_libs <- function() {
  user_lib <- Sys.getenv("R_LIBS_USER", unset = "")
  if (!identical(user_lib, "")) {
    dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
    .libPaths(unique(c(user_lib, .libPaths())))
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Package management
# ---------------------------------------------------------------------------

# FUNCTION: ensure_packages
# PURPOSE: Installs any missing packages and loads all required packages.
#          Checks each package first to avoid unnecessary installation.
#          Used to ensure reproducible environments across machines.
#
# PARAMETERS:
#   pkgs – Character vector of package names (e.g., c("dplyr", "readr"))
#
# RETURNS: NULL (invisibly); modifies the loaded package environment
#
# SIDE EFFECTS:
#   - Installs any missing packages from cloud.r-project.org
#   - Loads all specified packages
#
# EXAMPLES:
#   ensure_packages(c("dplyr", "readr", "jsonlite"))
ensure_packages <- function(pkgs) {
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(missing_pkgs) > 0L) {
    install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

# ---------------------------------------------------------------------------
# Safe file I/O
# ---------------------------------------------------------------------------

# FUNCTION: require_existing_local_file
# PURPOSE: Validates that a required source file exists before use.
#          Stops with a clear, actionable error message if the file is missing.
#          Prevents downstream errors from missing inputs.
#
# PARAMETERS:
#   path – Full file path to check (e.g., "./data/input.csv")
#   label – Human-readable description (used in error message)
#   how_to_fix – Instructions for fixing the problem (shown in error message)
#
# RETURNS: Invisibly returns path if file exists; stops if not
#
# EXAMPLES:
#   require_existing_local_file(
#     "./data/schools.csv",
#     "school list",
#     "Run scripts/01_download_data.R first"
#   )
require_existing_local_file <- function(path, label, how_to_fix) {
  if (file.exists(path)) return(invisible(path))
  stop(
    paste0(
      "Missing required local source file for ", label, ": ", path, "\n",
      how_to_fix
    ),
    call. = FALSE
  )
}

# FUNCTION: write_csv_atomic
# PURPOSE: Writes a data frame to CSV safely and atomically.
#          Writes to a temporary file first, then renames it to the target path.
#          This ensures a build failure never leaves a partial or corrupted CSV
#          in place of the original file.
#
# PARAMETERS:
#   df – Data frame to write
#   path – Target file path (e.g., "./output/results.csv")
#
# RETURNS: Invisibly returns NULL
#
# SIDE EFFECTS:
#   - Creates path with df contents (NA values rendered as empty strings)
#   - Cleans up .tmp file if write fails
#
# EXAMPLES:
#   write_csv_atomic(processed_data, "./output/final.csv")
write_csv_atomic <- function(df, path) {
  tmp <- paste0(path, ".tmp")
  on.exit(if (file.exists(tmp)) file.remove(tmp), add = TRUE)
  readr::write_csv(df, tmp, na = "")
  file.rename(tmp, path)
}

# FUNCTION: extract_single_file_from_zip
# PURPOSE: Extracts a single known file from a zip archive.
#          If the file is already extracted, returns the existing copy to avoid
#          redundant extraction. This is useful for caching downloaded data.
#
# PARAMETERS:
#   zip_path – Path to the .zip file
#   member_name – Exact name of the file within the zip (e.g., "data/schools.csv")
#   out_dir – Directory where the file will be extracted
#
# RETURNS: Full path to the extracted file
#
# EXAMPLES:
#   extracted_path <- extract_single_file_from_zip(
#     "archive.zip",
#     "schools_data.csv",
#     "./cache"
#   )
extract_single_file_from_zip <- function(zip_path, member_name, out_dir) {
  out_path <- file.path(out_dir, member_name)
  if (file.exists(out_path)) return(out_path)
  suppressWarnings(utils::unzip(zip_path, files = member_name, exdir = out_dir))
  out_path
}

# FUNCTION: extract_first_matching_file_from_zip
# PURPOSE: Extracts the first file from a zip archive that matches a regex pattern.
#          Useful when you don't know the exact file name but can describe it.
#          Caches extracted files to avoid re-extracting.
#
# PARAMETERS:
#   zip_path – Path to the .zip file
#   pattern – Regex pattern to match file names (case-insensitive)
#             Examples: ".*\\.csv$" for CSV files, "2024.*data" for year-specific files
#   out_dir – Directory where files will be extracted
#
# RETURNS: Full path to the first extracted file matching pattern
#
# SIDE EFFECTS:
#   - Extracts the entire zip if no matches are found locally
#   - Stops with an error if no file matches pattern after extraction
#
# EXAMPLES:
#   path <- extract_first_matching_file_from_zip(
#     "archive.zip",
#     "2024.*\\.csv$",
#     "./cache"
#   )
extract_first_matching_file_from_zip <- function(zip_path, pattern, out_dir) {
  # Check if any file matching the pattern was already extracted
  existing <- list.files(out_dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (length(existing) > 0L) return(existing[[1]])

  # If not, extract the entire zip and search again
  suppressWarnings(utils::unzip(zip_path, exdir = out_dir))
  extracted <- list.files(out_dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)

  # Raise error if still no matches
  if (length(extracted) == 0L) {
    stop(sprintf("No file matching %s found in %s", pattern, zip_path), call. = FALSE)
  }
  extracted[[1]]
}

# ---------------------------------------------------------------------------
# Shared IPEDS path loader
# ---------------------------------------------------------------------------

# FUNCTION: load_ipeds_paths
# PURPOSE: Loads path configuration from ipeds_paths.R and returns the
#          key functions as a named list. This avoids verbose environment
#          manipulation in every script that needs IPEDS paths.
#
# PARAMETERS:
#   root – Repository root directory (defaults to current working directory)
#
# RETURNS: Named list with two functions:
#   $ipeds_layout – Function to build a list of standardized IPEDS paths
#   $ensure_ipeds_layout_dirs – Function to create those directories
#
# EXAMPLES:
#   paths_module <- load_ipeds_paths()
#   paths <- paths_module$ipeds_layout()  # Get list of paths
#   paths_module$ensure_ipeds_layout_dirs(paths)  # Create directories
load_ipeds_paths <- function(root = ".") {
  env <- new.env(parent = baseenv())
  sys.source(
    file.path(getwd(), "scripts", "shared", "ipeds_paths.R"),
    envir = env
  )
  list(
    ipeds_layout = env$ipeds_layout,
    ensure_ipeds_layout_dirs = env$ensure_ipeds_layout_dirs
  )
}

# FUNCTION: to_num
# PURPOSE: Safely converts values to numeric, returning NA for unparseable inputs.
#          Used throughout the pipeline to handle inconsistent data types.
#
# PARAMETERS:
#   x – Vector or scalar value to convert
#
# RETURNS: Numeric vector with same length as x; NA for values that can't be converted
#
# EXAMPLES:
#   to_num("123")         # Returns 123
#   to_num("12.34")       # Returns 12.34
#   to_num("missing")     # Returns NA
#   to_num(c("1", "2", "x"))  # Returns c(1, 2, NA)
to_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}
