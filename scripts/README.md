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
  - workbook summary, benchmark, XML, and worksheet builders
- `scripts/shared/accreditation_helpers.R`
  - accreditation text cleanup, classification, and matching helpers
- `scripts/shared/accreditation_scrapers.R`
  - accreditor parser functions
- `scripts/shared/grant_witness_helpers.R`
  - Grant Witness normalization and config-driven standardization helpers

## Supporting Scripts

- `build_grant_witness_usaspending_sensitivity.R`
  - supporting research QA analysis used by the scheduled grant workflow

- `annual_refresh_and_publish.R`
  - convenience wrapper for a partial refresh

- `publish_to_google_sheets.R`
  - optional Google Sheets publisher for the site-ready CSV

## Lightweight Tests

- `tests/run_shared_helper_smoke_tests.R`
  - quick smoke runner for the shared helper tests
  - sources focused test files under `tests/test_*.R`
  - runs without a dedicated test framework

- `tests/test_utils.R`
- `tests/test_export_helpers.R`
- `tests/test_accreditation_helpers.R`
- `tests/test_grant_witness_helpers.R`
- `tests/test_workbook_helpers.R`
- `tests/test_ipeds_helpers.R`
- `tests/test_pipeline_smoke.R`
  - focused regression coverage for each shared helper layer
  - plus a source-level smoke check for the main pipeline entry scripts
- `tests/test_export_pipeline_fixture.R`
  - tiny fixture-driven regression check for the website export pipeline
  - verifies generated JSON/CSV outputs from a minimal temporary dataset
- `tests/test_canonical_pipeline_fixture.R`
  - tiny fixture-driven regression check for the canonical IPEDS build
  - runs the canonical script in a temporary mini-workspace with stubbed dictionary lookups
  - intended to stay small and easy to extend as helpers change
- `tests/test_canonical_pipeline_aux_fixture.R`
  - tiny canonical regression that includes a real EFFY aux-table path
  - verifies enrollment backfill and derived international-share behavior
- `tests/test_end_to_end_pipeline_fixture.R`
  - reduced end-to-end pipeline check
  - runs canonical build first, then web exports from that generated canonical output

Run it with:

```bash
Rscript --vanilla ./tests/run_shared_helper_smoke_tests.R
```

What the test layers mean:

- helper tests
  - fast checks for pure shared functions
- fixture pipeline tests
  - small temp-workspace regressions for canonical and export contracts
- pipeline smoke
  - verifies the major scripts still source cleanly and expose `main()`

## Closure Tabs Expected In Google Sheets

`import_closure_sheet.py` looks for these tab names:

- `closure_status_tracker_matches`
- `running_closures`
- `main_campus_closures`
- `branch_campus_closures`
- `mergers_consolidations`
- `private_sector_federal_main_closures`

## Data Contracts

`scripts/shared/contracts.R` contains validators that run at script boundaries. They
fail early with readable errors when a required column disappears or a key
duplicates. Use them when making schema changes.

Available validators:

| Validator | Where to call it |
|---|---|
| `validate_canonical_output(df)` | After `build_ipeds_canonical_dataset.R` finishes |
| `validate_workbook_input(df)` | After numeric coercion in `build_article_workbook.R` |
| `validate_export_input(df)` | After numeric coercion in `build_web_exports.R` |

When adding a new required column to the canonical dataset, add it to
`CANONICAL_REQUIRED_COLS` in `contracts.R` and add at least one fixture test
case that exercises it before shipping.

## Orchestrators vs. Heavy Transforms

Understanding which scripts own architecture vs. which ones do heavy work helps
you know where to look when something breaks.

**Orchestrators** — mostly call other scripts or arrange data flow:
- `build_ipeds_dataset.R` — runs collector → canonical in order
- `annual_refresh_and_publish.R` — convenience wrapper for partial refresh
- `build_grant_witness_usaspending_sensitivity.R` — calls the API, computes proposals

**Heavy transforms** — do the actual data shaping:
- `collect_ipeds_data.R` — downloads IPEDS, resolves variable names, writes raw CSV
- `build_ipeds_canonical_dataset.R` — computes derived fields, writes canonical CSV
- `build_web_exports.R` — joins non-IPEDS data, writes JSON/CSV for the site
- `build_article_workbook.R` — assembles Excel workbook with summaries and benchmarks
- `build_accreditation_actions.R` — scrapes and joins accreditation data
- `build_grant_witness_join.R` — normalizes and joins Grant Witness data

**Joiners** — combine canonical with external data:
- `build_outcomes_join.R` — adds College Scorecard and graduation rate data
- `build_college_cuts_join.R` — adds reported college cuts data

When a schema change ripples across scripts, update the contracts validator in
the receiving script first (it will point at the break), then update the sender.

## How to Safely Refactor

1. **Change a helper function** — run the smoke tests first, change the helper,
   run the smoke tests again. If a fixture test fails, the failure message points
   at the exact seam that broke.
2. **Add a new required column** — add it to the relevant `REQUIRED_COLS` in
   `contracts.R`, add a fixture test that produces it, then change the code.
3. **Change a scraper** — add a test case for the new behavior in the relevant
   `test_accreditation_scrapers.R` section before changing the scraper itself.
4. **Refactor a large script body** — extract the section into a named helper in
   `scripts/shared/`, add direct tests for the helper, replace the old inline
   block with a call to the helper.
5. **Change the canonical dataset schema** — update `CANONICAL_REQUIRED_COLS` in
   `contracts.R`, add a test case in `test_canonical_pipeline_fixture.R`, then
   make the code change.

The smoke test runner is the gate: `Rscript --vanilla ./tests/run_shared_helper_smoke_tests.R`.
Keep it green.

## Legacy Material

Scripts that are not part of the active interactive build should stay out of
the main repo surface.
