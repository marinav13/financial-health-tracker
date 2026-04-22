# Project Walkthrough

This is the operational build guide. For architecture, rerun safety, and output
ownership, use [REFRESH_CYCLE.md](./REFRESH_CYCLE.md). For one-time package
setup, use [DEV_SETUP.md](./DEV_SETUP.md).

## What The Pipeline Does

1. Downloads and decodes IPEDS data into a canonical finance dataset.
2. Joins supporting source domains: outcomes, college cuts, accreditation,
   research cuts, closures, HCM status, and federal composite scores.
3. Writes static JSON, indexes, per-school files, and CSV downloads under
   `data/`.
4. Serves the site directly from static HTML, CSS, JavaScript, and JSON.

## First-Time Setup

Install local dependencies from the committed lock files:

```bash
npm ci
python -m pip install -r requirements.txt
```

For R package setup, see [DEV_SETUP.md](./DEV_SETUP.md). The GitHub Actions
workflows restore R packages through `renv/library`.

## Full Rebuild

### 1. Build IPEDS

Run this during an annual IPEDS refresh or when the canonical financial dataset
must change:

```r
source("scripts/build_ipeds_dataset.R")
main(c("--start-year", "2014", "--end-year", "2024"))
```

The first run downloads a large federal data cache. Later runs reuse local cache
files where possible.

### 2. Build Supporting Joins

These scripts read canonical IPEDS data plus their source-domain inputs:

```r
source("scripts/build_outcomes_join.R"); main()
source("scripts/build_college_cuts_join.R"); main()
source("scripts/build_accreditation_actions.R"); main()
source("scripts/build_grant_witness_join.R"); main()
```

Outcomes, HCM, and federal composite sources are source-versioned. When changing
their source years or workbook vintages, update the constants in the relevant
script and the notes in `data_pipelines/README.md`.

### 3. Run Python Imports

```bash
python scripts/build_hcm_level2.py
python scripts/import_closure_sheet.py --sheet "YOUR_GOOGLE_SHEET_URL_OR_ID"
python scripts/build_federal_composite_scores.py
```

To test the closure import without Google Sheets:

```bash
python scripts/import_closure_sheet.py --from-dir path/to/closure_csv_exports --output-root path/to/staging_output
```

### 4. Build Site Exports

```r
source("scripts/build_web_exports.R")
main()
```

This writes the committed static files under `data/`, including search indexes,
section JSON, per-school JSON, metadata, and the download CSV.

## Partial Refreshes

For weekly-style source refreshes, follow the same order as the scheduled
workflow:

```bash
Rscript ./scripts/build_accreditation_actions.R --financial-input data/downloads/full_dataset.csv
python ./scripts/import_supabase_institution_mapping.py --skip-stale-check
Rscript ./scripts/build_college_cuts_join.R --financial-input data/downloads/full_dataset.csv
Rscript ./scripts/build_grant_witness_join.R --financial-input data/downloads/full_dataset.csv --skip-usaspending-filter
Rscript ./scripts/build_grant_witness_usaspending_sensitivity.R
Rscript ./scripts/build_grant_witness_join.R --financial-input data/downloads/full_dataset.csv
python ./scripts/import_closure_sheet.py --sheet "YOUR_GOOGLE_SHEET_URL_OR_ID"
Rscript ./scripts/build_web_exports.R --input data/downloads/full_dataset.csv
```

The USAspending sensitivity step overwrites the risky-continuation filter used
by the research page. Run it only when you intend to refresh that filter.

## Verify Before Commit

```bash
Rscript ./tests/run_shared_helper_smoke_tests.R
npm run test:smoke
npm run test:e2e
npm run test:a11y
```

Useful focused checks:

```bash
python tests/test_build_hcm_level2.py
python tests/test_build_federal_composite_scores.py
node tests/test_refresh_workflows.js
```

## Publication Files

The static handoff is:

```text
index.html
school.html
cuts.html
research.html
accreditation.html
styles.css
js/
data/
robots.txt
```

The school page expects URLs such as:

```text
school.html?unitid=172264
```

## Spot Check After Rebuild

- Search returns institutions on the home page and section pages.
- Several school pages load with charts and metadata.
- Cuts, accreditation, and research landing tables populate.
- Filter, sort, pagination, and CSV download controls still work.
- HCM, closure, outcomes, and federal composite fields appear where expected.
- `npm run test:a11y` and the Playwright accessibility-state tests pass.

## If Something Breaks

Work backward from the failing output:

| Failure | First place to check |
|---|---|
| Site JSON/export failure | Missing or stale join input for `build_web_exports.R` |
| Join failure | Source cache/raw file for that domain |
| Closure import failure | Sheet URL, tab names, or local CSV export path |
| IPEDS failure | Download cache, source year range, or changed IPEDS table schema |
| Browser/a11y failure | `js/app.js` shared helpers and the affected page controller |

The scripts are designed to fail with readable missing-input messages. Do not
commit partial regenerated `data/` outputs if a refresh fails mid-run.
