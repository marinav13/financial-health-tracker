# Financial Health Tracker

This repository is the source of truth for the college financial health tracker.

It does four things:

1. Scrapes the IPEDS tables and dictionaries we need.
2. Builds one wide raw-but-decoded institution-year dataset.
3. Derives one authoritative canonical dataset plus the web exports.
4. Publishes the static files that power the website tabs and downloads.

If you are new to the project, start with:

- [WALKTHROUGH_GUIDE.txt](./WALKTHROUGH_GUIDE.txt)
- [WEBSITE_MANAGER_NOTE.txt](./WEBSITE_MANAGER_NOTE.txt)
- [`scripts/README.md`](./scripts/README.md)

## What Runs First

If you only remember one thing, remember this:

1. Run the IPEDS tracker build.
2. Run the supporting domain joins.
3. Run the web export.
4. Run the workbook.

That order matters because each step writes files used by the next one.

## Simple Rebuild Order

Here is the full rebuild order in plain English:

1. `scripts/build_ipeds_tracker_dataset.R`
   - downloads missing IPEDS data zips and dictionaries only when needed
   - resolves the requested variables from the yearly dictionaries and code mappings
   - writes one wide raw-but-decoded institution-year dataset
   - then reads that dataset back in and computes the derived IPEDS fields
   - writes the canonical dataset and the extended JSON-ready export

2. `scripts/build_outcomes_join.R`
   - joins College Scorecard outcomes, IPEDS graduation-rate data, and Grad PLUS data
   - writes the joined outcomes file used by the site and workbook

3. `scripts/build_college_cuts_join.R`
   - joins the reported college cuts data for the Cuts tab

4. `scripts/build_accreditation_actions.R`
   - joins accreditation actions and school-level status data

5. `scripts/build_grant_witness_join.R`
   - joins the disrupted research funding data for the Research tab

6. `scripts/build_hcm_level2.py`
   - processes federal HCM status data for school profiles

7. `scripts/build_closure_status.py`
   - processes the federal closure status data used on school profiles

8. `scripts/build_closure_outputs.py`
   - processes the federal closure event files and writes the profile/tab outputs

9. `scripts/build_federal_composite_scores.py`
   - processes the federal composite score file from Downloads

10. `scripts/build_web_exports.R`
    - combines the canonical dataset with the joined domain tables
    - writes the site JSON, CSV download, and school-level files

11. `scripts/build_article_workbook.R`
    - writes the workbook XML used for reporting and handoff

## Project Layout

- `scripts/` contains the build entrypoints and shared helpers
- `scripts/ipeds/` contains the real IPEDS implementations
- `scripts/shared/` contains the small helper functions used by the entrypoints
- `ipeds/raw/` contains the wide raw dataset
- `ipeds/cache/` contains downloaded IPEDS zips, extracted files, and caches
- `ipeds/manifests/` contains the file catalog, column contract, and audit files
- `ipeds/derived/` contains the canonical and extended IPEDS datasets
- `data_pipelines/` contains the domain-specific joins and derived outputs
- `data/` contains the site JSON exports and indexes
- `data/downloads/` contains the site-wide CSV download
- `data/schools/` contains the school-level JSON files
- `workbooks/` contains the workbook XML
- `docs/` contains handoff notes and the longer walkthrough

## External Inputs

Some source files are not committed to the repo and must be present locally
before the full pipeline can run.

Recommended locations:

- Federal closure raw files: `data_pipelines/federal_closure/raw/`
- Federal composite score source: `data_pipelines/federal_composite/ay_2022_2023_composite_scores.csv`

Required raw files:

- `data_pipelines/federal_closure/raw/closure_file_20250403.xlsx`
- `data_pipelines/federal_closure/raw/closures_sep24.xlsx`
- `data_pipelines/federal_closure/raw/MONTHLY_CLOSED_REPORT_SECTION_1_2026040107063586645.zip`
- `data_pipelines/federal_closure/raw/MONTHLY_CLOSED_REPORT_SECTION_2_2026040107103586655.zip`
- `data_pipelines/federal_closure/raw/schfile_access_format_20260402.dat.zip`
- `data_pipelines/federal_composite/ay_2022_2023_composite_scores.csv`

The IPEDS cache folder does not need to exist ahead of time. The first tracker
build creates `ipeds/cache/downloads/` and fills it with the required IPEDS
zips and dictionaries.

If you prefer a different location for the IPEDS cache or the closure files,
set these environment variables before running the scripts:

- `IPEDS_DOWNLOADS_DIR`
- `FEDERAL_CLOSURE_SEPT_2024_XLSX`
- `FEDERAL_CLOSURE_MONTHLY_SECTION_1_ZIP`
- `FEDERAL_CLOSURE_MONTHLY_SECTION_2_ZIP`
- `FEDERAL_SCHFILE_ZIP`

## Main Outputs

- `ipeds/raw/ipeds_financial_health_raw_2014_2024.csv`
- `ipeds/manifests/ipeds_financial_health_selected_file_catalog.csv`
- `ipeds/manifests/ipeds_financial_health_field_resolution_audit_2014_2024.csv`
- `ipeds/derived/ipeds_financial_health_canonical_2014_2024.csv`
- `ipeds/derived/ipeds_financial_health_dataset_2014_2024.csv`
- `data/downloads/full_dataset.csv`
- `data/schools_index.json`
- `data/college_cuts.json`
- `data/accreditation.json`
- `data/research_funding.json`
- `workbooks/ipeds_financial_health_article_workbook_r.xml`

## Website Handoff

The website manager should deploy the finished static files into the live interactive folder:

- `/interactives/fitness/`

The files that need to go there are:

- `index.html`
- `school.html`
- `styles.css`
- `js/`
- `data/`

The school page expects URLs like:

- `/interactives/fitness/school.html?unitid=172264`

Before launch, test:

- the landing page
- search
- several school pages
- charts and hover values
- the CSV download button
- mobile layout
- the methodology panel

## If Something Is Missing

If a later step fails because an input file is missing, go back one step and rebuild the file it depends on.

For example:

- if the web export fails, rebuild the outcomes join or the IPEDS tracker first
- if the workbook fails, rebuild the web export first
- if a tab-specific export is missing, rebuild that tab’s join script first

## Data Refresh

The repo includes GitHub Actions workflows that refresh the data on a schedule and commit the updated files back into the repository.

That means the project can stay as the source of truth while the website only publishes the finished static assets.

The weekly closure sheet uses `scripts/download_weekly_closed_school_search_file.py`
and `scripts/publish_closure_status_sheet.R` to fetch the latest federal
weekly closed-school report and write it into Google Sheets automatically.

It uses Google Cloud Workload Identity Federation, so there is no
service-account JSON key file and no GitHub secret to manage for this workflow.
The manual setup is just:

- create the Google Cloud pool and provider
- grant the GitHub repo access to the service account
- share the spreadsheet with the service account email
