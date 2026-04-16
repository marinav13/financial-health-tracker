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
# Null coalescing
# ---------------------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1L && is.na(x))) y else x
}

# ---------------------------------------------------------------------------
# CLI argument parsing
# ---------------------------------------------------------------------------

# Returns the parsed args vector, preferring cli_args when provided (useful
# for calling scripts programmatically from other scripts or tests).
parse_cli_args <- function(cli_args = NULL) {
  if (is.null(cli_args)) commandArgs(trailingOnly = TRUE) else cli_args
}

# Returns the value that follows `flag` in `args`, or `default` if absent.
get_arg <- function(args, flag, default = NULL) {
  idx <- match(flag, args)
  if (!is.na(idx) && idx < length(args)) args[[idx + 1L]] else default
}

# Returns TRUE if `flag` appears anywhere in `args`.
arg_has <- function(args, flag) {
  isTRUE(flag %in% args)
}

# ---------------------------------------------------------------------------
# R library path setup (for GitHub Actions / renv environments)
# ---------------------------------------------------------------------------

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

# Installs any missing packages then loads all of them.
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

# Stops with a clear message when a required local file is missing.
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

# Writes a data frame to a CSV atomically: writes to a .tmp file first, then
# renames, so a failed build never leaves a partial file in place of a good one.
write_csv_atomic <- function(df, path) {
  tmp <- paste0(path, ".tmp")
  on.exit(if (file.exists(tmp)) file.remove(tmp), add = TRUE)
  readr::write_csv(df, tmp, na = "")
  file.rename(tmp, path)
}

# Extracts one known member from a zip archive into out_dir and returns the
# extracted path. If the file is already present, the existing copy is reused.
extract_single_file_from_zip <- function(zip_path, member_name, out_dir) {
  out_path <- file.path(out_dir, member_name)
  if (file.exists(out_path)) return(out_path)
  suppressWarnings(utils::unzip(zip_path, files = member_name, exdir = out_dir))
  out_path
}

# Extracts the first file in a zip archive that matches `pattern`, returning
# the full path to the extracted file. Existing extracted matches are reused.
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

# Sources ipeds_paths.R and returns ipeds_layout() and ensure_ipeds_layout_dirs()
# as a named list, avoiding the verbose env/get() boilerplate in every script.
load_ipeds_paths <- function(root = ".") {
  env <- new.env(parent = baseenv())
  sys.source(
    file.path(getwd(), "scripts", "shared", "ipeds_paths.R"),
    envir = env
  )
  list(
    ipeds_layout            = get("ipeds_layout",            envir = env, inherits = FALSE),
    ensure_ipeds_layout_dirs = get("ensure_ipeds_layout_dirs", envir = env, inherits = FALSE)
  )
}

# ---------------------------------------------------------------------------
# Numeric conversion helpers (shared between canonical dataset and accreditation)
# ---------------------------------------------------------------------------

# Coerces x to numeric, treating blank strings, "NA", and "NULL" as NA.
# Strips commas from formatted numbers (e.g. "1,234" -> 1234).
to_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NULL")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", "", x, fixed = TRUE)))
}

# Returns the percentage change from old_value to new_value, or NA when either
# input is NA or when old_value is zero (avoids division by zero).
safe_pct_change <- function(new_value, old_value) {
  ifelse(
    is.na(new_value) | is.na(old_value) | old_value == 0,
    NA_real_,
    ((new_value - old_value) / abs(old_value)) * 100
  )
}

# Returns numerator / denominator, or NA when either is NA or denominator is zero.
safe_divide <- function(numerator, denominator) {
  ifelse(
    is.na(numerator) | is.na(denominator) | denominator == 0,
    NA_real_,
    numerator / denominator
  )
}

# Returns the unique non-missing character values in sorted order, collapsed to
# a single string, or NA when there are no non-missing values.
collapse_unique_values <- function(x, sep = "; ") {
  vals <- unique(stats::na.omit(as.character(x)))
  if (length(vals) == 0L) NA_character_ else paste(sort(vals), collapse = sep)
}

# ---------------------------------------------------------------------------
# IPEDS field access helpers (shared between collector and canonical dataset)
# ---------------------------------------------------------------------------

# Extracts a string field from a one-row IPEDS data frame by column name.
# Returns NA_character_ when the field is absent, blank, or the row is NULL.
get_string <- function(row, field_name) {
  field_name <- if (length(field_name) == 0) NA_character_ else as.character(field_name[[1]])
  if (is.null(row) || is.na(field_name) || identical(field_name, "")) return(NA_character_)
  if (!(field_name %in% names(row))) return(NA_character_)
  value <- row[[field_name]][[1]]
  if (is.null(value) || identical(as.character(value), "")) NA_character_ else as.character(value)
}

# Extracts a numeric field from a one-row IPEDS data frame by column name.
# Strips commas before parsing; returns NA_real_ for blank or non-numeric values.
get_number <- function(row, field_name) {
  value <- get_string(row, field_name)
  if (is.na(value)) return(NA_real_)
  suppressWarnings(as.numeric(gsub(",", "", trimws(value), fixed = TRUE)))
}

# Returns the first non-blank element in `values`, or NA when every element is
# absent or an empty string.
first_non_null <- function(values) {
  values <- values[!(is.na(values) | values == "")]
  if (length(values) == 0) return(NA)
  values[[1]]
}
