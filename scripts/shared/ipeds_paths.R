# scripts/shared/ipeds_paths.R
#
# Standardized directory structure and file path configuration for the IPEDS pipeline.
# This file defines how raw data, manifests, derived datasets, and caches are organized.
#
# USAGE:
#   This file is typically loaded via load_ipeds_paths() from utils.R:
#   source(file.path(getwd(), "scripts", "shared", "ipeds_paths.R"))
#   paths <- ipeds_layout()
#   ensure_ipeds_layout_dirs(paths)

# ---------------------------------------------------------------------------
# Path configuration builder
# ---------------------------------------------------------------------------

# Returns a named list of all directories and file paths used by the IPEDS pipeline.
ipeds_layout <- function(root = ".", output_stem = "ipeds_financial_health", start_year = as.integer(Sys.getenv("IPEDS_START_YEAR", "2014")), end_year = as.integer(Sys.getenv("IPEDS_END_YEAR", "2024"))) {
  # Normalize the repository root path (resolve symlinks, ensure forward slashes)
  repo_root <- normalizePath(root, winslash = "/", mustWork = TRUE)

  # Primary directory structure
  ipeds_root <- file.path(repo_root, "ipeds")
  raw_dir <- file.path(ipeds_root, "raw")
  manifests_dir <- file.path(ipeds_root, "manifests")
  derived_dir <- file.path(ipeds_root, "derived")

  # Caching directories (three-level hierarchy for organization)
  cache_dir <- file.path(ipeds_root, "cache")
  cache_downloads_dir <- file.path(cache_dir, "downloads")
  cache_data_dir <- file.path(cache_downloads_dir, "data")
  cache_dict_dir <- file.path(cache_downloads_dir, "dict")
  cache_extract_dir <- file.path(cache_downloads_dir, "extracted")
  cache_year_dir <- file.path(cache_dir, "year_cache")

  # Auxiliary data caching (for accreditation, closures, and other non-IPEDS sources)
  cache_aux_dir <- file.path(cache_dir, "aux")
  cache_aux_data_dir <- file.path(cache_aux_dir, "data")
  cache_aux_extract_dir <- file.path(cache_aux_dir, "extracted")

  # Return comprehensive list of paths
  list(
    # Directories
    repo_root = repo_root,
    ipeds_root = ipeds_root,
    raw_dir = raw_dir,
    manifests_dir = manifests_dir,
    derived_dir = derived_dir,
    cache_dir = cache_dir,
    cache_downloads_dir = cache_downloads_dir,
    cache_data_dir = cache_data_dir,
    cache_dict_dir = cache_dict_dir,
    cache_extract_dir = cache_extract_dir,
    cache_year_dir = cache_year_dir,
    cache_aux_dir = cache_aux_dir,
    cache_aux_data_dir = cache_aux_data_dir,
    cache_aux_extract_dir = cache_aux_extract_dir,

    # Files
    catalog_html = file.path(manifests_dir, "ipeds_datafiles.html"),
    selected_file_catalog_csv = file.path(manifests_dir, sprintf("%s_selected_file_catalog.csv", output_stem)),
    field_resolution_audit_csv = file.path(manifests_dir, sprintf("%s_field_resolution_audit_%s_%s.csv", output_stem, start_year, end_year)),
    raw_csv = file.path(raw_dir, sprintf("%s_raw_%s_%s.csv", output_stem, start_year, end_year)),
    canonical_csv = file.path(derived_dir, sprintf("%s_canonical_%s_%s.csv", output_stem, start_year, end_year)),
    dataset_csv = file.path(derived_dir, sprintf("%s_dataset_%s_%s.csv", output_stem, start_year, end_year)),
    legacy_catalog_html = file.path(ipeds_root, "ipeds_datafiles.html")
  )
}

# ---------------------------------------------------------------------------
# Directory creation utility
# ---------------------------------------------------------------------------

# Creates all directories in the paths list. Safe to call repeatedly (idempotent).
ensure_ipeds_layout_dirs <- function(paths) {
  # Create main IPEDS structure
  dir.create(paths$ipeds_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$raw_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$manifests_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$derived_dir, recursive = TRUE, showWarnings = FALSE)

  # Create cache hierarchy
  dir.create(paths$cache_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$cache_downloads_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$cache_data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$cache_dict_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$cache_extract_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$cache_year_dir, recursive = TRUE, showWarnings = FALSE)

  # Create auxiliary data cache structure
  dir.create(paths$cache_aux_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$cache_aux_data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$cache_aux_extract_dir, recursive = TRUE, showWarnings = FALSE)

  invisible(paths)
}
