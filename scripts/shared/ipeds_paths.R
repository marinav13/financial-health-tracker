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

# FUNCTION: ipeds_layout
# PURPOSE: Constructs a comprehensive list of all directory and file paths used
#          by the IPEDS data pipeline. Centralizes path management so that
#          changing the directory structure only requires updates here.
#
# PARAMETERS:
#   root – Repository root directory (defaults to current working directory ".")
#   output_stem – Base name for output files (default "ipeds_financial_health")
#   start_year – First year in the dataset (default 2014)
#   end_year – Last year in the dataset (default 2024)
#
# RETURNS: Named list containing:
#   DIRECTORY PATHS:
#     repo_root – Repository root directory
#     ipeds_root – Base directory for all IPEDS data
#     raw_dir – Raw IPEDS downloads (before processing)
#     manifests_dir – Manifests and metadata files
#     derived_dir – Cleaned and processed datasets
#     cache_dir – All cache directories
#     cache_downloads_dir – Downloaded zip files and extracted contents
#     cache_data_dir – Cache for raw IPEDS data files
#     cache_dict_dir – Cache for data dictionaries
#     cache_extract_dir – Temporary extracted files from zips
#     cache_year_dir – Year-level cache
#     cache_aux_dir – Auxiliary data (accreditation, closures, etc.)
#     cache_aux_data_dir – Cache for auxiliary data files
#     cache_aux_extract_dir – Temporary extracted auxiliary files
#
#   FILE PATHS (with start_year and end_year substituted):
#     catalog_html – Cached IPEDS data catalog HTML
#     selected_file_catalog_csv – Filtered list of data tables to use
#     field_resolution_audit_csv – Mapping of field names across years
#     raw_csv – Full raw IPEDS dataset CSV
#     canonical_csv – Canonical cleaned dataset CSV
#     dataset_csv – Export-ready dataset CSV
#     legacy_catalog_html – Legacy copy of catalog
#
# DIRECTORY STRUCTURE:
#   ipeds/
#     raw/                           # Raw downloads
#     manifests/                      # Metadata and catalogs
#     derived/                        # Processed datasets
#     cache/
#       downloads/
#         data/                       # Raw IPEDS data cache
#         dict/                       # Dictionary cache
#         extracted/                  # Extracted files from downloads
#       year_cache/                   # Year-level caching
#       aux/                          # Auxiliary data
#         data/                       # Aux data files
#         extracted/                  # Extracted aux files
#
# EXAMPLES:
#   # Standard usage with defaults
#   paths <- ipeds_layout()
#
#   # Custom date range for a specific report
#   paths <- ipeds_layout(start_year = 2020, end_year = 2024)
#
#   # Access specific paths
#   canonical_file <- paths$canonical_csv
#   mkdir -p paths$raw_dir
ipeds_layout <- function(root = ".", output_stem = "ipeds_financial_health", start_year = 2014L, end_year = 2024L) {
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

# FUNCTION: ensure_ipeds_layout_dirs
# PURPOSE: Creates all directories defined in the paths list.
#          Safe to call multiple times (doesn't fail if directories exist).
#          Should be called early in pipeline scripts to ensure the directory
#          structure exists before writing files.
#
# PARAMETERS:
#   paths – Named list of directory paths, typically from ipeds_layout()
#
# RETURNS: Invisibly returns paths (for chaining)
#
# SIDE EFFECTS:
#   - Creates all directories listed in paths with recursive=TRUE
#   - Suppresses warnings if directories already exist
#   - Ensures parent directories are created as needed
#
# DETAILS:
#   Creates the following directories (if not already present):
#   - Main directories: ipeds, raw, manifests, derived, cache
#   - Download caches: downloads, data, dict, extracted
#   - Year caching: year_cache
#   - Auxiliary data: aux, aux/data, aux/extracted
#
# EXAMPLES:
#   paths <- ipeds_layout()
#   ensure_ipeds_layout_dirs(paths)  # Create all directories
#   # Now paths$raw_dir, paths$derived_dir, etc. are ready to use
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
