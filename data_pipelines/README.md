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

- `federal_hcm/`
  - federal Heightened Cash Monitoring raw files and derived outputs used
    during local rebuilds

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

This folder does not replace `ipeds/`.

IPEDS remains separate because it is the primary longitudinal source dataset
for the project, while `data_pipelines/` holds the supporting domain datasets
that get joined onto the canonical IPEDS build