# Scripts

This folder contains the public build entrypoints for the project.

The IPEDS implementations now live under `scripts/ipeds/`, while the root
files keep the old command-line paths working for existing docs and workflows.

## Source Data Pipeline

- `collect_ipeds_data.R`
  - wrapper that forwards to `scripts/ipeds/collect_ipeds_data.R`

- `build_ipeds_canonical_dataset.R`
  - wrapper that forwards to `scripts/ipeds/build_ipeds_canonical_dataset.R`

- `build_ipeds_tracker_dataset.R`
  - wrapper that forwards to `scripts/ipeds/build_ipeds_tracker_dataset.R`

## Domain Joins

- `build_outcomes_join.R`
  - joins College Scorecard and graduation-rate data

- `build_college_cuts_join.R`
  - joins the reported college cuts data

- `build_accreditation_actions.R`
  - joins accreditation actions and school-level summaries

- `build_grant_witness_join.R`
  - joins the disrupted research funding data

- `build_grant_witness_usaspending_sensitivity.R`
  - runs the USAspending comparison analysis used in the research workflow

- `build_hcm_level2.py`
  - processes federal HCM status data

- `build_closure_status.py`
  - processes federal closure status data

- `build_closure_outputs.py`
  - writes the closure outputs used by the profile and tab work

- `build_federal_closure_opeid_panel.py`
  - builds the federal closure OPEID panel

- `build_federal_closure_style_ipeds_closures.py`
  - builds the IPEDS-style closure outputs

- `build_federal_composite_scores.py`
  - processes the federal composite score file from Downloads

- `download_weekly_closed_school_search_file.py`
  - fetches the latest weekly closed-school report from FSA and saves it to a stable local path

## Site Outputs

- `build_web_exports.R`
  - writes the site JSON, CSV downloads, and school-level files

- `build_article_workbook.R`
  - writes the workbook XML

- `publish_to_google_sheets.R`
  - publishes the site-ready CSV to Google Sheets when that handoff is needed

- `publish_closure_status_sheet.R`
  - publishes the weekly closure spreadsheet from the downloaded FSA report

- `annual_refresh_and_publish.R`
  - runs the full refresh flow in one place

## Shared Helpers

- `shared/script_utils.R` contains the tiny command-line and script-loading helpers used by the entrypoints.

## IPEDS Implementations

- `ipeds/collect_ipeds_data.R`
  - downloads the needed IPEDS data zips and dictionaries only when they are missing
  - resolves variables from the yearly dictionaries and code mappings
  - writes the wide raw-but-decoded institution-year dataset

- `ipeds/build_ipeds_canonical_dataset.R`
  - reads the raw dataset from `ipeds/raw/`
  - computes the derived fields used by the interactive and workbook
  - writes the canonical dataset and the extended JSON-ready export

- `ipeds/build_ipeds_tracker_dataset.R`
  - runs the collector and canonical builder in the intended order
  - this is the preferred IPEDS orchestration script

## Archived Wrappers

- Legacy PowerShell wrappers now live under `archive/legacy_powershell/` so the active build surface stays R-first and easier to follow.
