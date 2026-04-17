# Scripts

This folder contains the active build scripts for the interactive.

The non-IPEDS domain datasets those scripts read and write live under
`data_pipelines/`, not at the repo root.

Most files inside `data_pipelines/` are local rebuild inputs or intermediate
outputs. The repo commits the code and shipped website assets in `data/`, not
the generated pipeline CSVs and cache folders.

IPEDS remains under `ipeds/`.

## Core Interactive Build

- `collect_ipeds_data.R`
  - downloads missing IPEDS zips and dictionaries
  - resolves requested variables from yearly files and codebooks
  - writes the wide raw decoded IPEDS dataset

- `build_ipeds_canonical_dataset.R`
  - reads the raw IPEDS dataset
  - computes the derived fields used by the interactive and optional local reporting
  - writes the canonical dataset

- `build_ipeds_dataset.R`
  - runs `collect_ipeds_data.R` and `build_ipeds_canonical_dataset.R` in order
  - this is the preferred IPEDS rebuild command

- `build_outcomes_join.R`
  - joins College Scorecard, graduation-rate, and Grad PLUS data

- `build_college_cuts_join.R`
  - joins the reported college cuts data

- `build_accreditation_actions.R`
  - joins accreditation actions and school-level summaries

- `build_grant_witness_join.R`
  - joins the disrupted research funding data

- `build_hcm_level2.py`
  - processes federal HCM status data

- `import_closure_sheet.py`
  - imports the closure outputs this repo needs from a published Google Sheet
  - refreshes `data_pipelines/federal_closure/derived/`
  - refreshes `data/closure_status_by_unitid.json`

- `build_federal_composite_scores.py`
  - processes the federal composite score source file

- `build_web_exports.R`
  - writes the site JSON, CSV downloads, and school-level files


## Shared Helper Layout

The main R scripts now follow a simple pattern:

- orchestrator scripts stay in `scripts/`
- reusable logic lives in `scripts/shared/`
- generated data stays outside `scripts/`

Current shared helper roles:

- `scripts/shared/utils.R`
  - CLI args, package loading, numeric helpers, atomic writes
- `scripts/shared/ipeds_paths.R`
  - canonical IPEDS path layout and directory creation
- `scripts/shared/ipeds_helpers.R`
  - canonical IPEDS row builders, enrichment helpers, decode helpers
- `scripts/shared/export_helpers.R`
  - JSON export, index, and bundle-writing helpers
- `scripts/shared/workbook_helpers.R`
  - workbook summary, benchmark, XML, and worksheet builders for optional local reporting
- `s