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

# Returns y if x is NULL, has length 0, or is a single NA value; otherwise returns x.
# Similar to the ?? operator in other languages (e.g., SQL, Kotlin).
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1L && is.na(x))) y else x
}

# ---------------------------------------------------------------------------
# CLI argument parsing
# ---------------------------------------------------------------------------

# Returns command-line arguments passed to the script. If cli_args is provided
# directly (useful for testing or programmatic calls), returns that instead.
parse_cli_args <- function(cli_args = NULL) {
  if (is.null(cli_args)) commandArgs(trailingOnly = TRUE) else cli_args
}

# Returns the value that follows a given flag in the argument list, or a
# default if the flag is absent or has no value after it.
# E.g. args=c("--input","data.csv") -> "data.csv"
get_arg <- function(args, flag, default = NULL) {
  idx <- match(flag, args)
  if (!is.na(idx) && idx < length(args)) args[[idx + 1L]] else default
}

# Returns TRUE if a boolean flag (no value) is present in the argument list.
# E.g. args=c("--input","data.csv","--verbose") -> TRUE for "--verbose"
arg_has <- function(args, flag) {
  isTRUE(flag %in% args)
}

# ---------------------------------------------------------------------------
# R library path setup (for GitHub Actions / renv environments)
# ---------------------------------------------------------------------------

# Sets up R's library search path for CI/CD environments where packages are
# installed to a user-specified directory. Reads from R_LIBS_USER env var.
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

# Installs any missing packages and loads all specified packages.
# Useful for ensuring reproducible environments across machines.
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

# Stops with an error if the required source file doesn't exist, preventing
# downstream failures from missing inputs.
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

# Writes a data frame to CSV atomically: first to a temp file, then renames
# to target. This prevents partial writes from leaving corrupted files.
write_csv_atomic <- function(df, path) {
  tmp <- paste0(path, ".tmp")
  on.exit(if (file.exists(tmp)) file.remove(tmp), add = TRUE)
  readr::write_csv(df, tmp, na = "")
  file.rename(tmp, path)
}

# Extracts a single file from a zip archive. If the file already exists,
# returns the existing copy to avoid redundant extraction.
extract_single_file_from_zip <- function(zip_path, member_name, out_dir) {
  out_path <- file.path(out_dir, member_name)
  if (file.exists(out_path)) return(out_path)
  suppressWarnings(utils::unzip(zip_path, files = member_name, exdir = out_dir))
  out_path
}

# Extracts the first file from a zip that matches a regex pattern (case-insensitive).
# Skips extraction if a matching file already exists in out_dir.
extract_first_matching_file_from_zip <- function(zip_path, pattern, out_dir) {
  existing <- list.files(out_dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (length(existing) > 0L) return(existing[[1]])

  suppressWarnings(utils::unzip(zip_path, exdir = out_dir))
  extracted <- list.files(out_dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)

  if (length(extracted) == 0L) {
    stop(sprintf("No file matching %s found in %s", pattern, zip_path), call. = FALSE)
  }
  extracted[[1]]
}

# ---------------------------------------------------------------------------
# Shared IPEDS path loader
# ---------------------------------------------------------------------------

# Loads path configuration from ipeds_paths.R and returns the key functions as
# a named list. This avoids verbose environment manipulation in every script.
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

# Safely converts values to numeric, returning NA for inputs that can't be parsed.
# Useful for handling inconsistent data types in raw data.
to_num <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
}

# ---------------------------------------------------------------------------
# Safe numeric arithmetic
# ---------------------------------------------------------------------------

# Divides x by y. Returns NA_real_ if y is zero, NA, or NULL, preventing
# "division by zero" errors in rate and percentage calculations.
safe_divide <- function(x, y) {
  if (is.null(y) || length(y) == 0 || is.na(y) || y == 0) return(NA_real_)
  x / y
}

# Computes ((new_val - old_val) / old_val) * 100 safely, returning NA_real_
# if old_val is zero or missing. Used for year-over-year / 5-year change fields.
safe_pct_change <- function(new_val, old_val) {
  safe_divide(new_val - old_val, old_val) * 100
}

# ---------------------------------------------------------------------------
# Data frame field extractors
# ---------------------------------------------------------------------------

# Extracts a single numeric value from column `col` of data frame `df`.
# Returns NA_real_ if the column is absent, the data frame is NULL,
# or the value cannot be coerced to numeric.
get_number <- function(df, col) {
  if (is.null(df) || is.null(col) || length(col) == 0 || is.na(col)) return(NA_real_)
  if (!col %in% names(df)) return(NA_real_)
  to_num(df[[col]][[1L]])
}

# Extracts a single string value from column `col` of data frame `df`.
# Returns NA_character_ if the column is absent, empty string, or the data
# frame is NULL.
get_string <- function(df, col) {
  if (is.null(df) || is.null(col) || length(col) == 0 || is.na(col)) return(NA_character_)
  if (!col %in% names(df)) return(NA_character_)
  val <- df[[col]][[1L]]
  if (is.na(val) || nchar(as.character(val)) == 0L) return(NA_character_)
  as.character(val)
}

# ---------------------------------------------------------------------------
# String helpers
# ---------------------------------------------------------------------------

# Collapses non-NA unique values of x into a single string separated by `sep`,
# sorted alphabetically. Returns NA_character_ if all values are NA.
# E.g. collapse_unique_values(c("b", NA, "a", "b")) -> "a; b"
collapse_unique_values <- function(x, sep = "; ") {
  vals <- sort(unique(x[!is.na(x)]))
  if (length(vals) == 0L) return(NA_character_)
  paste(vals, collapse = sep)
}
