# ============================================================================
# scripts/shared/ipeds_helpers.R
# ============================================================================
#
# Loads all IPEDS helper modules into the project environment. Acts as the
# single entry point for the split IPEDS helper layer. Sources this AFTER
# utils.R, inside build_ipeds_canonical_dataset.R and tests.
#
# Sources these specialized modules:
# - ipeds_row_builders.R: Row-building and canonical IPEDS transformations
# - ipeds_enrichment_helpers.R: Per-institution trend/YoY enrichment
# - ipeds_sector_benchmarks.R: Sector-level benchmark calculations
#
# ============================================================================

ipeds_shared_dir <- if (exists("root", inherits = TRUE)) {
  file.path(root, "scripts", "shared")
} else {
  file.path(getwd(), "scripts", "shared")
}

source(file.path(ipeds_shared_dir, "ipeds_row_builders.R"))
source(file.path(ipeds_shared_dir, "ipeds_enrichment_helpers.R"))
source(file.path(ipeds_shared_dir, "ipeds_sector_benchmarks.R"))
