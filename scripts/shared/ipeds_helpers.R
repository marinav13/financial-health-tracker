# scripts/shared/ipeds_helpers.R
#
# Compatibility loader for the split IPEDS helper layer.
# Source this after utils.R inside build_ipeds_canonical_dataset.R and tests.
#
# The actual implementations now live in:
# - ipeds_row_builders.R
# - ipeds_enrichment_helpers.R
# - ipeds_sector_benchmarks.R

ipeds_shared_dir <- if (exists("root", inherits = TRUE)) {
  file.path(root, "scripts", "shared")
} else {
  file.path(getwd(), "scripts", "shared")
}

source(file.path(ipeds_shared_dir, "ipeds_row_builders.R"))
source(file.path(ipeds_shared_dir, "ipeds_enrichment_helpers.R"))
source(file.path(ipeds_shared_dir, "ipeds_sector_benchmarks.R"))
