# Financial Health Tracker

Builds the college financial health interactive from federal data sources to the static site files that go on the Hechinger website.

## Quick start

```r
# 1. Build IPEDS (first time only — downloads ~2 GB of federal data)
source("scripts/build_ipeds_dataset.R")
main(c("--start-year", "2014", "--end-year", "2024"))

# 2. Build supporting joins
source("scripts/build_outcomes_join.R");          main()
source("scripts/build_college_cuts_join.R");       main()
source("scripts/build_accreditation_actions.R");   main()
source("scripts/build_grant_witness_join.R");      main()

# 3. Import closure data from Google Sheets
python scripts/import_closure_sheet.py --sheet "YOUR_SHEET_URL"

# 4. Build the site files
source("scripts/build_web_exports.R"); main()
```

That's it. Steps 2–4 are rerunnable anytime. Step 1 only needs to run during the IPEDS refresh window (January–April).

## What to read first

| If you are... | Read this first |
|---|---|
| New to the project | [WALKTHROUGH_GUIDE.txt](./WALKTHROUGH_GUIDE.txt) |
| Publishing the site | [docs/WEBSITE_MANAGER_NOTE.md](./docs/WEBSITE_MANAGER_NOTE.md) |
| Understanding the pipeline | [docs/REFRESH_CYCLE.md](./docs/REFRESH_CYCLE.md) |
| Understanding the code | [scripts/README.md](./scripts/README.md) |
| Finding a term | [docs/GLOSSARY.md](./docs/GLOSSARY.md) |

## Build order (the one thing to remember)

```
IPEDS raw download
    └─► build_ipeds_dataset.R

Supporting joins (each reads canonical + external data)
    └─► build_outcomes_join.R
    └─► build_college_cuts_join.R
    └─► build_accreditation_actions.R
    └─► build_grant_witness_join.R

External imports
    └─► import_closure_sheet.py
    └─► build_federal_composite_scores.py
    └─► build_hcm_level2.py

Site output
    └─► build_web_exports.R  →  data/
```

Each script writes files consumed by the next. Run them in order.

## What the repo commits

This repo is intentionally strict about what it commits:

**Yes — commit:** code, docs, workflows, and finished site assets in `data/`

**No — keep local:** rebuild caches (`ipeds/cache/`), raw IPEDS downloads, intermediate pipeline CSVs, and workbooks

See `docs/REFRESH_CYCLE.md` → "Outputs that are local-only" for the full list.

## Project layout

```
scripts/             — build entrypoints
scripts/shared/      — reusable helpers (args, exports, scrapers, etc.)
data_pipelines/      — non-IPEDS rebuild inputs and intermediate joins
ipeds/               — IPEDS raw, manifests, derived outputs, and download cache
data/                — shipped site JSON, CSVs, and indexes
workbooks/           — local reporting workbook (not part of site production)
docs/                — glossary, refresh cycle docs, and setup guides
```

## Testing

**R tests (smoke + regression):**
```bash
Rscript ./tests/run_shared_helper_smoke_tests.R
```

**JavaScript tests (structure + charts):**
```bash
npm run test:smoke
```

**Playwright e2e tests (search, navigation, charts):**
```bash
npm install
npm run test:e2e
```

**Accessibility tests (pa11y):**
```bash
npm run test:a11y
```

Always run at least the R smoke tests before committing.
