# Operations Manual

This is the single operational reference for the public source repo behind The
Hechinger Report's College Financial Health Tracker.

Live site:
[https://hechingerreport.org/college-financial-health-tracker/](https://hechingerreport.org/college-financial-health-tracker/)

For the deployment-only handoff, use [docs/DEPLOY_HANDOFF.md](./DEPLOY_HANDOFF.md).

## Repo Model

This repo is a public source repository, not a deploy-only snapshot.

The browser-served runtime surface is:

- `index.html`
- `school.html`
- `cuts.html`
- `research.html`
- `accreditation.html`
- `methodology.html`
- `404.html`
- `styles.css`
- `js/`
- `assets/`
- `data/`
- `robots.txt`

The source/build/test surface kept in-tree is:

- `scripts/`
- `data_pipelines/`
- `tests/`
- `docs/`
- `.github/workflows/`
- `renv/`, `renv.lock`, `.Rprofile`
- `requirements.txt`, `package.json`, `package-lock.json`, `playwright.config.js`

The school page expects URLs such as `school.html?unitid=172264`.

## First-Time Setup

### Requirements

- R 4.0 or later
- Python 3
- Node.js
- Internet access for IPEDS and other live-source refreshes

### Node and Python

```bash
npm ci
python -m pip install -r requirements.txt
```

### R packages

Open RStudio, set the working directory to the project folder, and run:

```r
user_lib <- Sys.getenv("R_LIBS_USER")
if (!nzchar(user_lib)) {
  user_lib <- file.path(path.expand("~"), "R", paste0("library-", getRversion()))
}
dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(user_lib, .libPaths()))

install.packages(c(
  "dplyr",
  "googlesheets4",
  "openxlsx",
  "purrr",
  "readr",
  "readxl",
  "stringr",
  "tidyr",
  "xml2"
))
```

### Required local inputs

- `data_pipelines/federal_composite/ay_2022_2023_composite_scores.csv`

### Useful local caches

- `ipeds/cache/`
- `data_pipelines/scorecard/cache/Most-Recent-Cohorts-Institution_*.zip`
- `data_pipelines/scorecard/cache/DRVGR2024.zip`

Keep caches, scratch outputs, and private credentials out of Git. The main
ignored local-only paths include:

- `node_modules/`
- `renv/library/`
- `.renv_cache/`
- `ipeds/cache/`, `ipeds/raw/`, `ipeds/derived/`
- `data_pipelines/*/cache/`
- `test-results/`
- `workbooks/`
- `.secrets/`
- `.private_docs/`

### Closure import source

The default closure data comes from:

```text
https://docs.google.com/spreadsheets/d/1TyVZlzfoD1sr0jID6Rt421-bN5wS_9JjWnCBofmbhi8/edit?gid=0
```

To test the closure import without Google Sheets:

```bash
python ./scripts/import_closure_sheet.py --from-dir path/to/closure_csv_exports --output-root path/to/staging_output
```

## Build Architecture

### Canonical IPEDS build

- `scripts/collect_ipeds_data.R`
  - downloads and decodes IPEDS source files into local cache and raw outputs
- `scripts/build_ipeds_dataset.R`
  - builds the canonical multi-year finance dataset consumed by downstream
    joins

### Supporting joins

- `scripts/build_outcomes_join.R`
  - reads Scorecard, DRVGR, and Grad PLUS inputs
  - writes `data_pipelines/scorecard/tracker_outcomes_joined.csv`

- `scripts/build_college_cuts_join.R`
  - reads the public College Cuts API plus the Supabase institution mapping
  - writes joined outputs under `data_pipelines/college_cuts/`
  - also writes local-only analyst artifacts such as `_financial_trends.csv`
    and `_unmatched_for_review.csv`

- `scripts/build_accreditation_actions.R`
  - scrapes accreditor sources and joins actions to tracker schools
  - writes joined outputs under `data_pipelines/accreditation/`
  - also writes local-only review artifacts such as `_unmatched_for_review.csv`
    and `_source_coverage.csv`

- `scripts/build_grant_witness_join.R`
  - reads Grant Witness inputs and manual override tables
  - writes grant-level and institution-level joined outputs under
    `data_pipelines/grant_witness/`
  - treats Grant Witness status as the source of truth for whether a grant is
    currently disrupted

### External JSON builders

- `scripts/build_hcm_level2.py`
  - writes `data/hcm2_by_unitid.json`

- `scripts/build_federal_composite_scores.py`
  - writes `data/federal_composite_scores_by_unitid.json`

- `scripts/import_closure_sheet.py`
  - imports closure outputs into repo-owned JSON and derived files
  - the weekly workflow currently keeps this step disabled while the upstream
    closure tracker is rebuilt outside this repo

### Runtime export

- `scripts/build_web_exports.R`
  - reads the canonical IPEDS dataset plus all supporting joins
  - writes the committed runtime files under `data/`, including:
    - `data/schools/{unitid}.json`
    - `data/schools_index.json`
    - `data/college_cuts.json`
    - `data/college_cuts_index.json`
    - `data/accreditation.json`
    - `data/accreditation_index.json`
    - `data/research_funding.json`
    - `data/research_funding_index.json`
    - `data/downloads/full_dataset.csv`
    - `data/metadata.json`

Important contract:

- `build_web_exports.R` should read the canonical multi-year IPEDS dataset from
  `ipeds/derived/`
- do not point it at `data/downloads/full_dataset.csv`; that file is a public
  download artifact, not the export input

## Script Inventory

The `scripts/` directory is a source-repo surface, not part of the deployed
static site. Nothing there is served directly to the browser.

### Primary build entry points

- `build_ipeds_dataset.R`
  - preferred IPEDS rebuild command
  - runs `collect_ipeds_data.R` and `build_ipeds_canonical_dataset.R` in order

- `build_outcomes_join.R`
  - joins College Scorecard, graduation-rate, and Grad PLUS data

- `build_college_cuts_join.R`
  - joins the reported college cuts data

- `build_accreditation_actions.R`
  - joins accreditation actions and school-level summaries

- `build_grant_witness_join.R`
  - joins disrupted research funding data

- `build_hcm_level2.py`
  - processes federal HCM status data

- `build_federal_composite_scores.py`
  - processes the federal composite score source file

- `import_closure_sheet.py`
  - imports closure outputs this repo needs from a published Google Sheet
  - refreshes `data_pipelines/federal_closure/derived/`
  - refreshes `data/closure_status_by_unitid.json`

- `build_web_exports.R`
  - writes site JSON, CSV downloads, and school-level files

### Shared helper layout

The main R scripts follow a simple pattern:

- orchestrator scripts stay in `scripts/`
- reusable logic lives in `scripts/shared/`
- generated data stays outside `scripts/`

Important shared helpers:

- `scripts/shared/utils.R`
  - CLI args, package loading, numeric helpers, atomic writes

- `scripts/shared/ipeds_paths.R`
  - canonical IPEDS path layout and directory creation

- `scripts/shared/ipeds_helpers.R`
  - canonical IPEDS row builders, enrichment helpers, and decode helpers

- `scripts/shared/export_helpers.R`
  - JSON export, index, and bundle-writing helpers

- `scripts/shared/workbook_helpers.R`
  - workbook summary and worksheet builders for optional local reporting

- `scripts/shared/accreditation_helpers.R`
  - accreditation text cleanup, classification, and matching helpers

- `scripts/shared/accreditation_scrapers.R`
  - accreditor parser functions

- `scripts/shared/grant_witness_helpers.R`
  - Grant Witness normalization and standardization helpers

### Supporting or optional scripts

- `build_grant_witness_usaspending_sensitivity.R`
  - archived QA tooling retained for methodology and ad hoc analysis
  - not part of the production refresh

- `build_article_workbook.R`
  - optional local reporting script
  - not part of the shipped-output path or public smoke gate

- `annual_refresh_and_publish.R`
  - convenience wrapper for a partial refresh

- `publish_to_google_sheets.R`
  - optional Google Sheets publisher for the site-ready CSV

### Data contracts

`scripts/shared/contracts.R` contains validators that run at script
boundaries. They should fail early when a required column disappears or a key
duplicates.

Available validators:

| Validator | Where to call it |
|---|---|
| `validate_canonical_output(df)` | After `build_ipeds_canonical_dataset.R` finishes |
| `validate_export_input(df)` | After numeric coercion in `build_web_exports.R` |

`validate_workbook_input(df)` still exists for local workbook reporting, but it
is not part of the streamlined public production path.

When adding a required canonical column, update `CANONICAL_REQUIRED_COLS` in
`contracts.R` and add at least one fixture test that exercises it.

## Source-Domain Inputs

The `data_pipelines/` directory holds the supporting source-domain inputs that
feed the interactive outside the main IPEDS pipeline.

Nothing in `data_pipelines/` is served directly to the browser. The shipped
runtime artifacts live under `data/`.

### Subject areas

- `accreditation/`
  - accreditation scrapes, cache files, and derived school-level outputs

- `college_cuts/`
  - college cuts API inputs, cache files, and derived outputs for the Cuts tab

- `grant_witness/`
  - disrupted research funding inputs, cache files, methodology files, and
    derived outputs for the Research tab
  - the small manual include/match tables are important human-maintained source
    files worth keeping in Git

- `scorecard/`
  - College Scorecard and graduation-rate join files used during local rebuilds
  - outcomes sources are intentionally source-versioned, not auto-floating

- `federal_hcm/`
  - federal Heightened Cash Monitoring raw files and derived outputs used
    during local rebuilds
  - HCM is source-versioned to the committed quarterly raw workbooks listed in
    `scripts/build_hcm_level2.py`

- `federal_closure/`
  - imported closure outputs used downstream during local rebuilds
  - `derived/` holds CSVs consumed by workbook and site build steps
  - the active repo does not rebuild federal closure raw files directly

- `federal_composite/`
  - federal composite score source files and derived outputs used during local
    rebuilds
  - source vintages should be added deliberately and reflected in
    `scripts/build_federal_composite_scores.py`

### Versioning notes

- outcomes currently join the 2024 tracker cohort to the College Scorecard
  release dated `2026-03-23`, IPEDS `DRVGR2024`, and the Federal Student Aid
  Grad PLUS dashboard for AY `2025-2026 Q2`
- when changing source vintages, update both the committed source files and the
  constants in the corresponding script
- `data_pipelines/` does not replace `ipeds/`; IPEDS remains the primary
  longitudinal source dataset, while `data_pipelines/` holds supporting domain
  datasets joined onto the canonical build later

## Full Rebuild

### 1. Build IPEDS

Run this during an annual IPEDS refresh or whenever the canonical finance
dataset must change:

```r
source("scripts/build_ipeds_dataset.R")
main(c("--start-year", "2014", "--end-year", "2024"))
```

The first run downloads a large federal cache. Later runs reuse local cache
files where available.

### 2. Build supporting joins

```bash
Rscript --vanilla ./scripts/build_outcomes_join.R
Rscript --vanilla ./scripts/build_college_cuts_join.R
Rscript --vanilla ./scripts/build_accreditation_actions.R
Rscript --vanilla ./scripts/build_grant_witness_join.R
```

### 3. Run Python import/build steps

```bash
python ./scripts/build_hcm_level2.py
python ./scripts/import_closure_sheet.py --sheet "YOUR_GOOGLE_SHEET_URL_OR_ID"
python ./scripts/build_federal_composite_scores.py
```

### 4. Build site exports

```bash
Rscript --vanilla ./scripts/build_web_exports.R
```

This writes the committed site outputs under `data/`.

## Weekly Refresh Workflow

The scheduled workflow is
[`.github/workflows/refresh-ipeds-site-data.yml`](../.github/workflows/refresh-ipeds-site-data.yml).
It currently runs at `0 0 * * 1`, which is Monday 00:00 UTC, roughly Sunday
evening in U.S. Eastern time.

At a high level it:

1. Restores or rebuilds the canonical IPEDS dataset.
2. Refreshes accreditation outputs and DAPIP audit inputs.
3. Rebuilds the Supabase institution mapping and College Cuts join.
4. Refreshes research funding joins.
5. Builds candidate site exports.
6. Stages accreditation and college-cuts review queues to Google Sheets.
7. Pulls review decisions back into repo snapshots.
8. Rebuilds the site with the review gate enforced.
9. Validates outputs, reports scraper drift warnings, and commits updated data.

The closure import step is intentionally disabled in the current weekly
workflow. Re-enable it only when the upstream closure tracker is stable again.

### Weekly-style partial refresh sequence

```bash
Rscript --vanilla ./scripts/build_accreditation_actions.R
python ./scripts/import_supabase_institution_mapping.py --skip-stale-check
Rscript --vanilla ./scripts/build_college_cuts_join.R
Rscript --vanilla ./scripts/build_grant_witness_join.R
python ./scripts/import_closure_sheet.py --sheet "YOUR_GOOGLE_SHEET_URL_OR_ID"
Rscript --vanilla ./scripts/build_web_exports.R
```

## Rerun Safety

### Safe to rerun anytime

| Script | Why |
|---|---|
| `build_web_exports.R` | Reads committed/local inputs and rewrites `data/` only |
| `build_outcomes_join.R` | Rebuilds the outcomes join from local source files |
| `build_grant_witness_join.R` | Rebuilds research joins from cached/current source files |
| `build_federal_composite_scores.py` | Rebuilds JSON from the committed federal composite source file |
| `build_hcm_level2.py` | Rebuilds JSON from committed HCM source files |

### Usually safe, but may produce new public candidates

| Script | Caveat |
|---|---|
| `build_college_cuts_join.R` | May pull newly published cuts rows |
| `build_accreditation_actions.R` | May scrape newly published accreditation actions |
| DAPIP rebuild helpers | May expose newly matched or newly classified DAPIP actions |
| `import_closure_sheet.py` | Overwrites closure outputs from the current sheet contents |

### Expensive or high-impact

| Script | Caveat |
|---|---|
| `collect_ipeds_data.R` | Downloads and decodes large federal source tables |
| `build_ipeds_dataset.R` | Rebuilds the canonical dataset every downstream step consumes |

Treat the annual IPEDS rebuild as a separate refresh window, not part of the
routine weekly loop.

## Focused Rebuilds

If you only need one section, rebuild its join first and then rerun
`build_web_exports.R`.

Example: research funding only

```bash
Rscript --vanilla ./scripts/build_grant_witness_join.R
Rscript --vanilla ./scripts/build_web_exports.R
```

That updates the research pipeline plus the runtime exports. It does not
re-scrape accreditation, re-fetch College Cuts, or rebuild IPEDS.

## Validation Before Commit

### Automated checks

```bash
Rscript ./tests/run_shared_helper_smoke_tests.R
npm run test:smoke
npm run test:e2e
npm run test:a11y
```

Useful focused checks:

```bash
python ./tests/test_build_hcm_level2.py
python ./tests/test_build_federal_composite_scores.py
node ./tests/test_refresh_workflows.js
```

### Manual spot checks

- search returns institutions on the home page and section pages
- several school pages load with charts and metadata
- `cuts.html`, `accreditation.html`, and `research.html` populate
- filter, sort, pagination, and CSV download controls still work
- HCM, outcomes, and federal composite fields appear where expected

## Failure Triage

### First step

When the weekly refresh fails:

1. Open the repo on GitHub.
2. Go to the **Actions** tab.
3. Open the most recent **Refresh Source Data Weekly** run.
4. Expand the first red step and read the full error message before rerunning
   anything.

### Diagnose by step name

#### `Refresh accreditation actions with cache fallback`

Likely causes:

- an accreditor page changed its HTML structure
- the source site is temporarily down
- a scraper is returning zero rows from a page that still has actions

What to check:

- compare the failing source page against
  `scripts/shared/accreditation_scrapers.R`
- inspect scraper drift warnings in the refresh log
- verify the local cache fallback is recent enough to publish from

#### `Rebuild DAPIP institution crosswalk`
#### `Rebuild DAPIP institutional action rows`
#### `Rebuild DAPIP vs scraper audit`

Likely causes:

- the DAPIP host or response shape changed
- cached DAPIP search data no longer matches parser expectations
- a schema change broke the crosswalk or audit join

What to check:

- the DAPIP helpers are still using `surveys.ope.ed.gov`
- current DAPIP cache/version assumptions in the scripts
- intermediate DAPIP outputs before broadening the rerun

#### `Sync Supabase institution -> unitid mapping`
#### `Refresh college cuts from public API`

Likely causes:

- Supabase credentials are missing, stale, or rotated
- the public API changed shape
- the institution mapping import failed before the cuts join

What to check:

- GitHub Actions secrets for `COLLEGE_CUTS_SUPABASE_URL` and
  `COLLEGE_CUTS_SUPABASE_ANON_KEY`
- mapping sync output before the cuts join runs
- cached fallback behavior if the API is temporarily unavailable

#### `Refresh research cuts from Grant Witness`

Likely causes:

- Grant Witness returned unexpected data
- a manual override file is malformed
- a join contract now fails because multiple schools match the same row

What to check:

- `data_pipelines/grant_witness/manual_include.csv`
- `data_pipelines/grant_witness/manual_match_overrides.csv`
- new ambiguity under `data_pipelines/grant_witness/analysis/`

#### `Stage accreditation review queue`
#### `Stage college cuts review queue`
#### `Pull accreditation review decisions`
#### `Pull college cuts review decisions`

Likely causes:

- Google Sheets credentials are missing or invalid
- the configured tab names changed
- an editor altered required sheet headers or column types

What to check:

- `GOOGLE_SERVICE_ACCOUNT_JSON_B64`
- the review sheet tabs named in workflow env
- header consistency before rerunning the sheet sync steps

#### `Rebuild static web exports (candidate generation)`
#### `Rebuild static web exports (review gate)`

Likely causes:

- a required joined input is missing or malformed
- the editorial review snapshots no longer match the expected schema
- a downstream contract changed after an upstream step completed

What to check:

- the missing-file or missing-column error in the export log
- whether candidate generation succeeded before the review-gated build
- whether the pulled review CSVs still have the expected columns

#### `Commit and push updated data`

Likely causes:

- another run is already writing to the branch
- workflow permissions changed
- the job produced no clean working tree state for commit

What to check:

- whether another refresh run is still in progress
- whether the workflow still has `permissions: contents: write`
- the `git status` lines printed in the workflow log

### After fixing the cause

Before accepting a rerun, spot-check:

1. `school.html?unitid=172264`
2. `school.html?unitid=190150`
3. `cuts.html`
4. `accreditation.html`
5. `research.html`

If a page is blank, inspect the browser console for a failed `fetch()` call.
That usually points directly to the broken JSON file.

### What not to do

- do not hand-edit `data/` files and commit them as a shortcut
- do not jump straight to a full rerun without isolating the failing step
- do not disable the weekly schedule to hide a recurring failure
- do not assume the closure import is part of the current weekly workflow; it
  is intentionally disabled right now

## Scraper Drift Warnings

The weekly refresh workflow tees scraper stdout and stderr into
`refresh-logs/combined.log`. A final drift-warning step greps that log for
known warning patterns and surfaces them for human triage.

The refresh is allowed to continue because the public JSON rebuild is pinned to
the last committed reviewed snapshot. Drift still needs follow-up.

### `warn_on_empty_parse` / `Returning empty table`

Meaning:

- a page came back non-empty but the scraper parsed zero action rows
- that usually means the HTML structure changed

Triage:

1. identify the accreditor from the log line
2. open the URL and confirm actions are actually present
3. compare the page HTML against the relevant parser in
   `scripts/shared/accreditation_scrapers.R`
4. patch the selector or parsing logic
5. rerun the workflow after the parser fix lands

If the page is legitimately empty, the warning may be a false alarm that week.

### `warn_if_scrape_count_dropped`

Meaning:

- one accreditor's row count dropped substantially week over week
- that may mean a truncated parse, a legitimate source cleanup, or a changed
  action label that no longer matches classification rules

Triage:

1. read which accreditor dropped and by how much
2. diff this week's accreditation output against the previous committed version
3. spot-check a few disappeared rows against the live source page
4. if the rows are still live, treat it as parser drift and patch the parser

### `warn_if_action_type_dropped`

Meaning:

- a specific action type dropped from some positive count to zero
- this is often more suspicious than a total-count drop

Triage:

1. open the accreditor page and search for the missing action type manually
2. if the action type still exists on the page, patch the classifier or parser
3. if the action type is genuinely absent, treat it as a false alarm that week

### `SCRAPER RETURNED 0 ROWS: <accreditor>`

Meaning:

- the per-accreditor parser returned an empty result frame
- weekly refresh still continues because it runs with
  `--allow-partial-accreditation`

Triage:

1. inspect the source page
2. compare it against the matching parser function
3. patch the parser
4. rerun the refresh workflow and confirm the warning clears

### Suppression rule

If a warning is a known-good false alarm, suppress it at the producer or parser
call site, not in the CI grep step.

## Glossary Appendix

### `unitid`

The Department of Education identifier used as the primary school key across
tables and JSON files.

### `IPEDS`

The Integrated Postsecondary Education Data System, which provides the
longitudinal federal source data this project turns into its canonical finance
dataset.

### `canonical dataset`

The cleaned multi-year IPEDS dataset built by this repo and used as the base
table for downstream joins.

### `join`

A merge between datasets using a shared key such as `unitid`.

### `manifest`

The IPEDS file and field audit that records exactly which source files and
variables were used in the build.

### `raw` vs `derived`

`raw` means downloaded source data as received. `derived` means cleaned,
decoded, or computed outputs produced by the project.

### `smoke test`

A fast check that the code still runs and exposes the expected entry points.

### `fixture`

A small test input used to exercise pipeline logic without needing the full
real upstream datasets.

### `accreditor` and `accreditation action`

An accreditor is the body evaluating institutional quality. An accreditation
action is an official finding such as warning, probation, or show cause.

### `College Scorecard`

The Department of Education outcomes dataset joined into the tracker.

### `Grant Witness`

The research-disruption source used to identify colleges facing major federal
grant cuts.

### `USAspending.gov`

The federal grants and contracts spending site. It is used for verification and
context, but not as the production filter for whether a Grant Witness row is
currently disrupted.

### `HCM` / `HCM2`

Heightened Cash Monitoring, with HCM2 being the more serious tier tracked in
the public site.

### closure import

This repo no longer runs a closure scraper. It imports closure outputs from a
Google Sheet maintained elsewhere.
