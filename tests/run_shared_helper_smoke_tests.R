source(file.path(getwd(), "tests", "test_support.R"))

test_files <- c(
  "test_utils.R",
  "test_contracts.R",
  "test_export_helpers.R",
  "test_accreditation_helpers.R",
  "test_accreditation_scrapers.R",
  "test_grant_witness_helpers.R",
  "test_ipeds_helpers.R",
  "test_ipeds_collector_helpers.R",
  "test_usaspending_sensitivity_helpers.R",
  "test_canonical_pipeline_fixture.R",
  "test_canonical_pipeline_aux_fixture.R",
  "test_export_pipeline_fixture.R",
  "test_college_cuts_pipeline_fixture.R",
  "test_grant_witness_pipeline_fixture.R",
  "test_accreditation_a