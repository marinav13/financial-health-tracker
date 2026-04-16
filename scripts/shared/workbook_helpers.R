# scripts/shared/workbook_helpers.R
#
# Compatibility loader for the split workbook helper layer.
# Source this inside main() in build_article_workbook.R after utils.R.
#
# The actual implementations now live in:
# - workbook_table_helpers.R
# - workbook_registry_helpers.R
# - workbook_xml_helpers.R

workbook_shared_dir <- if (exists("root", inherits = TRUE)) {
  file.path(root, "scripts", "shared")
} else {
  file.path(getwd(), "scripts", "shared")
}

source(file.path(workbook_shared_dir, "workbook_table_helpers.R"))
source(file.path(workbook_shared_dir, "workbook_registry_helpers.R"))
source(file.path(workbook_shared_dir, "workbook_xml_helpers.R"))
