# Financial Health Tracker

This repository builds the college financial health interactive.

It does four main things:

1. Collects the IPEDS variables we need for 2014-2024.
2. Builds one raw-but-decoded institution-year dataset.
3. Derives one authoritative canonical dataset.
4. Builds the web exports and workbook used by the site.

If you are new to the project, start with:

- [WALKTHROUGH_GUIDE.txt](./WALKTHROUGH_GUIDE.txt)
- [WEBSITE_MANAGER_NOTE.txt](./WEBSITE_MANAGER_NOTE.txt)
- [`scripts/README.md`](./scripts/README.md)

## What Changed For Closure Data

This repo no longer owns the federal closure scraper.

Instead:

- the closure scraper can live in a separate repo
- that separate repo publishes cleaned closure outputs into a Google Sheet
- this repo imports those published closure outputs with `scripts/import_closure_sheet.py`

That keeps this repo smaller and easier to rebuild.

## Main Build Order

If you only remember one thing, remember this order:

1. `scripts/build_ipeds_dataset.R`
2. supporting joins and profile inputs
3. `scripts/build_web_exports.R`
4. `scripts/build_article_workbook.R`

Each step writes files used by the next one.

## Full Rebuild Order

1. `scripts/build_ipeds_dataset.R`
   - downloads missing IPEDS zips and dictionaries only when needed
   - writes the raw decoded dataset and the canonical dataset

2. `scripts/build_outcomes_join.R`
   - joins College Scorecard outcomes, graduation-rate data, and Grad PLUS data

3. `scripts/build_college_cuts_join.R`
   - builds the college cuts join for the Cuts tab

4. `scripts/build_accreditation_actions.R`
   - builds accreditation actions and school-level summaries

5. `scripts/build_grant_witness_join.R`
   - builds the disrupted research funding join for the Research tab

6. `scripts/build_hcm_level2.py`
   - builds federal HCM profile data

7. `scripts/import_closure_sheet.py`
   - imports published closure outputs from Google Sheets
   - refreshes `data_pipelines/federal_closure/derived/`
   - refreshes `data/closure_status_by_unitid.json`

8. `scripts/build_federal_composite_scores.py`
   - builds the federal composite score profile lookup

9. `scripts/build_web_exports.R`
   - writes the site JSON, download CSV, and school-level files

10. `scripts/build_article_workbook.R`
   - writes the canonical workbook used for reporting and handoff

## Closure Sheet Contract

The closure Google Sheet should contain these tabs:

- `closure_status_tracker_matches`
- `running_closures`
- `main_campus_closures`
- `branch_campus_closures`
- `mergers_consolidations`
- `private_sector_federal_main_closures`

The import script downloads those tabs as CSV and writes them into the stable
paths this repo already uses downstream.

## Project Layout

- `scripts/` contains the active build scripts
- `data_pipelines/` contains the non-IPEDS local rebuild inputs and intermediate joins
- `ipeds/raw/` contains the wide raw IPEDS institution-year dataset you rebuild locally when needed
- `ipeds/manifests/` contains the selected-file catalog and field-resolution audit you keep locally
- `ipeds/derived/` contains the canonical IPEDS outputs you keep locally for reporting and rebuilds
- `ipeds/cache/` is the reusable local IPEDS download cache
- `data/` contains the site JSON exports and indexes
- `workbooks/` contains the local reporting workbook export

## External Inputs

Normal rebuilds now need far fewer local raw files.

Git in this repo is intentionally strict:

- commit code
- commit docs
- commit workflows
- commit finished site assets in `data/`
- keep rebuild caches, raw files, intermediate joins, and workbook files local

Required for local rebuilds:

- `data_pipelines/federal_composite/ay_2022_2023_composite_scores.csv`

By default, `scripts/import_closure_sheet.py` imports from the project's
authoritative closure sheet:

- `https://docs.google.com/spreadsheets/d/1TyVZlzfoD1sr0jID6Rt421-bN5wS_9JjWnCBofmbhi8/edit?gid=0#gid=0`

Only pass `--sheet` if you intentionally want to override that source.

Helpful but still local-only:

- the Scorecard cache files in `data_pipelines/scorecard/cache/`
- the IPEDS cache in `ipeds/cache/`

The IPEDS cache is local-only:

- keep it on your machine
- let the scripts reuse it
- do not commit it to Git

## Useful Commands

Build IPEDS:

```r
source("scripts/build_ipeds_dataset.R")
main(c("--start-year", "2014", "--end-year", "2024"))
```

Import closure outputs from Google Sheets:

```bash
python scripts/import_closure_sheet.py --sheet "YOUR_GOOGLE_SHEET_URL_OR_ID"
```

Offline smoke test of the closure import using a folder of CSV exports:

```bash
python scripts/import_closure_sheet.py --from-dir path/to/closure_csv_exports
```

Build the web exports:

```r
source("scripts/build_web_exports.R")
main()
```

Build the workbook:

```r
source("scripts/build_article_workbook.R")
main(c(
  "--input", "./ipeds/derived/ipeds_financial_health_dataset_2014_2024.csv",
  "--output", "./workbooks/ipeds_financial_health_article_workbook.xls"
))
```

## Main Outputs

Tracked website outputs:

- `data/downloads/full_dataset.csv`
- `data/schools_index.json`
- `data/college_cuts.json`
- `data/accreditation.json`
- `data/research_funding.json`
- `data/hcm2_by_unitid.json`
- `data/closure_status_by_unitid.json`
- `data/federal_composite_scores_by_unitid.json`

Local rebuild outputs that are useful for reporting but are not part of the committed site handoff:

- `ipeds/raw/ipeds_financial_health_raw_2014_2024.csv`
- `ipeds/manifests/ipeds_financial_health_selected_file_catalog.csv`
- `ipeds/manifests/ipeds_financial_health_field_resolution_audit_2014_2024.csv`
- `ipeds/derived/ipeds_financial_health_canonical_2014_2024.csv`
- `ipeds/derived/ipeds_financial_health_dataset_2014_2024.csv`
- `data_pipelines/accreditation/*.csv`
- `data_pipelines/college_cuts/*.csv`
- `data_pipelines/grant_witness/grant_witness_*.csv`
- `data_pipelines/scorecard/*.csv`
- `data_pipelines/federal_hcm/*.csv`
- `data_pipelines/federal_hcm/raw/*`
- `data_pipelines/federal_composite/*.csv`
- `data_pipelines/federal_closure/derived/*`
- `workbooks/ipeds_financial_health_article_workbook.xls`

## Website Handoff

The website manager should deploy these finished static files into:

- `/interactives/fitness/`

Files to copy:

- `index.html`
- `school.html`
- `styles.css`
- `js/`
- `data/`

The school page expects URLs like:

- `/interactives/fitness/school.html?unitid=172264`

## Automation

The scheduled site-data workflow now imports closure outputs from the published
Google Sheet instead of rebuilding them from federal raw files.

That means this repo no longer needs:

- Google Cloud closure-publishing setup in this repo
- weekly federal closed-school scraping
- local federal closure raw files for ordinary rebuilds
