Data Pipelines

This folder is the main home for the non-IPEDS source domains that feed the
interactive.

Most files here are local rebuild inputs, caches, or intermediate joins. The
main repo commits the shipped website assets in `data/`, while this folder is
primarily for local rebuild work.

Each subfolder groups one subject area so the repo stays readable:

- `accreditation/`
  - accreditation scrapes, cache files, and derived school-level outputs

- `college_cuts/`
  - college cuts API inputs, cache files, and derived outputs for the Cuts tab

- `grant_witness/`
  - disrupted research funding inputs, cache files, methodology files, and
    derived outputs for the Research tab
  - the small manual include/match tables and the chosen risky continuation
    filter file are the main human-maintained source files worth keeping in Git

- `scorecard/`
  - College Scorecard and graduation-rate join files used during local rebuilds
  - outcomes sources are intentionally source-versioned, not auto-floating:
    `scripts/build_outcomes_join.R` currently joins the 2024 tracker cohort to
    the College Scorecard release dated 2026-03-23, IPEDS `DRVGR2024`, and the
    Federal Student Aid Grad PLUS dashboard for AY 2025-2026 Q2. Bump those
    constants and cached source files together when changing source vintages.

- `federal_hcm/`
  - federal Heightened Cash Monitoring raw files and derived outputs used
    during local rebuilds
  - HCM is source-versioned to the committed quarterly raw workbooks listed in
    `scripts/build_hcm_level2.py`; adding a new quarter should update that
    script's `SOURCE_FILES`, output/version label, and regenerated JSON.

- `federal_closure/`
  - imported closure outputs used downstream during local rebuilds
  - `derived/` holds the CSVs consumed by the workbook and site build steps
  - the active interactive repo does not rebuild federal closure raw files
  - instead, `scripts/import_closure_sheet.py` refreshes these outputs from a
    published Google Sheet maintained elsewhere
  - those imported closure files are local rebuild inputs, not the main repo's
    shipped website assets

- `federal_composite/`
  - federal composite score source files and derived outputs used during local
    rebuilds
  - federal composite scores are source-versioned to the committed AY 2022-2023
    source CSV for institutions with fiscal years ending July 1, 2022 through
    June 30, 2023. A newer federal workbook should be added as a new source
    vintage and reflected in `scripts/build_federal_composite_scores.py`.

This folder does not replace `ipeds/`.

IPEDS remains separate because it is the primary longitudinal source dataset
for the project, while `data_pipelines/` holds the supporting domain datasets
that get joined onto the canonical IPEDS build later.
