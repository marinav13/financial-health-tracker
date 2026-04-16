# ============================================================================
# scripts/shared/ipeds_helpers.R
# ============================================================================
#
# PURPOSE:
#   Compatibility loader that sources all IPEDS helper modules into the
#   project environment. This file acts as the single entry point for the
#   split IPEDS helper layer.
#
# WHERE IT FITS:
#   - Source this AFTER utils.R
#   - Source this inside build_ipeds_canonical_dataset.R and tests
#   - This is a loader script, not a direct implementation
#
# WHAT IT SOURCES:
#   The actual implementations now live in separate, specialized modules:
#   - ipeds_row_builders.R: Row-building and canonical IPEDS transformations
#   - ipeds_enrichment_helpers.R: Per-institution trend/YoY enrichment
#   - ipeds_sector_benchmarks.R: Sector-level benchmark calculations
#
# ============================================================================

# Find the shared scripts directory (handles both root-relative and cwd-relative paths)
ipeds_shared_dir <- if (exists("root", inherits = TRUE)) {
  file.path(root, "scripts", "shared")
} else {
  file.path(getwd(), "scripts", "shared")
}

# Source all three helper modules
source(file.path(ipeds_shared_dir, "ipeds_row_builders.R"))
source(file.path(ipeds_shared_dir, "ipeds_enrichment_helpers.R"))
source(file.path(ipeds_shared_dir, "ipeds_sector_benchmarks.R"))
