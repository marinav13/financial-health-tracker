# Developer Setup

One-time steps to get your local environment ready to run the build scripts.

## Requirements

- R (4.0 or later)
- Python 3
- Internet access (for the first IPEDS build)

## R package setup

Open RStudio and set the working directory to the project folder. Then run:

```r
# Set up a user library directory
user_lib <- Sys.getenv("R_LIBS_USER")
if (!nzchar(user_lib)) {
  user_lib <- file.path(path.expand("~"), "R", paste0("library-", getRversion()))
}
dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(user_lib, .libPaths()))

# Install all required packages
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

## What you need locally before rebuilding

**Required:**
- `data_pipelines/federal_composite/ay_2022_2023_composite_scores.csv`

**Optional (makes rebuilds faster):**
- IPEDS cache: `ipeds/cache/` — created automatically on first IPEDS build; keep it locally, don't commit it
- College Scorecard cache: `data_pipelines/scorecard/cache/Most-Recent-Cohorts-Institution_*.zip`
- DRVGR cache: `data_pipelines/scorecard/cache/DRVGR2024.zip`

## Closure Google Sheet

The default closure data comes from this Google Sheet:

```
https://docs.google.com/spreadsheets/d/1TyVZlzfoD1sr0jID6Rt421-bN5wS_9JjWnCBofmbhi8/edit?gid=0
```

Pass `--sheet "YOUR_URL"` to `import_closure_sheet.py` only if you want to override the source.

To test the import without using Google Sheets:

```bash
python scripts/import_closure_sheet.py --from-dir path/to/closure_csv_exports
```

## Node.js and e2e tests

This project uses Playwright for end-to-end tests (see `tests/e2e/`).

```bash
# Install dependencies
npm install

# Run e2e tests (headless)
npm run test:e2e

# Run e2e tests in browser (headed mode)
npm run test:e2e:headed

# Run smoke tests (JS structure + charts)
npm run test:smoke

# Run accessibility tests
npm run test:a11y
```
