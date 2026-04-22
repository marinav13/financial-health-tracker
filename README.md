# Financial Health Tracker

Static website and data pipeline for the college financial health interactive.
The site lets users search four-year, primarily bachelor's-degree-granting
institutions and inspect decade-scale financial trends, accreditation actions,
college cuts, research funding cuts, closures, HCM status, and federal composite
scores.

## Start Here

| Need | Read |
|---|---|
| Rebuild the project step by step | [docs/WALKTHROUGH.md](./docs/WALKTHROUGH.md) |
| Understand data flow and rerun safety | [docs/REFRESH_CYCLE.md](./docs/REFRESH_CYCLE.md) |
| Set up a local dev machine | [docs/DEV_SETUP.md](./docs/DEV_SETUP.md) |
| Publish or hand off the static site | [docs/WEBSITE_MANAGER_NOTE.md](./docs/WEBSITE_MANAGER_NOTE.md) |
| Diagnose weekly refresh failures | [docs/RUNBOOK_WEEKLY_REFRESH.md](./docs/RUNBOOK_WEEKLY_REFRESH.md) |
| Find definitions | [docs/GLOSSARY.md](./docs/GLOSSARY.md) |
| Work on build scripts | [scripts/README.md](./scripts/README.md) |

## Repository Shape

| Path | Purpose |
|---|---|
| `index.html`, `school.html`, `cuts.html`, `research.html`, `accreditation.html` | Static site entry points served by GitHub Pages |
| `styles.css`, `js/` | Browser UI code, no bundler required |
| `data/` | Committed site JSON, indexes, school files, and download CSVs |
| `scripts/` | R/Python build and import entry points |
| `scripts/shared/` | Reusable pipeline helpers and contracts |
| `data_pipelines/` | Supporting source domains, committed source-versioned inputs, and ignored local caches |
| `ipeds/` | Local IPEDS raw/cache/derived workspace, mostly ignored |
| `tests/` | R, Python, Node, Playwright, and accessibility checks |
| `docs/` | Walkthroughs, runbooks, setup notes, and glossary |

Root files that look like tooling files are intentionally root-level:
`package.json`, `package-lock.json`, `playwright.config.js`, `requirements.txt`,
`renv.lock`, `.Rprofile`, `.pa11yci.json`, and GitHub Pages files need to stay
where their tools expect them.

## What Gets Committed

Commit code, docs, workflows, tests, and finished site assets in `data/`.

Keep local caches, raw downloads, scratch outputs, and workbooks out of Git.
The main ignored local folders are `node_modules/`, `renv/library/`,
`ipeds/cache/`, `ipeds/raw/`, `ipeds/derived/`, `data_pipelines/*/cache/`,
`test-results/`, and `workbooks/`.

See [docs/REFRESH_CYCLE.md](./docs/REFRESH_CYCLE.md) for the longer output map.

## Fast Commands

```bash
# R smoke and fixture tests
Rscript ./tests/run_shared_helper_smoke_tests.R

# JS structure/security/data workflow tests
npm run test:smoke

# Browser interaction tests
npm run test:e2e

# Static pa11y URL checks
npm run test:a11y
```

Use `npm ci` on fresh machines or in CI. Use `npm install` only when you intend
to update `package-lock.json`.

## Build Entry Point

The full rebuild sequence lives in [docs/WALKTHROUGH.md](./docs/WALKTHROUGH.md).
The short version is:

1. Build or refresh the canonical IPEDS dataset.
2. Rebuild supporting joins for outcomes, cuts, accreditation, research, HCM,
   closures, and federal composite scores.
3. Run `scripts/build_web_exports.R` to write the committed static data files.
4. Run smoke, browser, and accessibility checks before pushing.
