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
  suppressWarnings(as.numeric(as.character(x)))
}
