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
  - computes the derived fields used by the interactive and workbook
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

- `build_article_workbook.R`
  - writes the canonical Excel-compatible workbook for local reporting use

## Supporting Scripts

- `build_grant_witness_usaspending_sensitivity.R`
  - supporting research QA analysis used by the scheduled grant workflow

- `annual_refresh_and_publish.R`
  - convenience wrapper for a partial refresh

- `publish_to_google_sheets.R`
  - optional Google Sheets publisher for the site-ready CSV

## Closure Tabs Expected In Google Sheets

`import_closure_sheet.py` looks for these tab names:

- `closure_status_tracker_matches`
- `running_closures`
- `main_campus_closures`
- `branch_campus_closures`
- `mergers_consolidations`
- `private_sector_federal_main_closures`

## Legacy Material

Scripts that are not part of the active interactive build should stay out of
the main repo surface.
