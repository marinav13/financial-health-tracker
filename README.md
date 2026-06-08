# Financial Health Tracker

Public source repo for The Hechinger Report's College Financial Health Tracker.

Live site:
[https://hechingerreport.org/college-financial-health-tracker/](https://hechingerreport.org/college-financial-health-tracker/)

This repo is a public source repository, not a deploy-only snapshot. It keeps
the shipped static site in-tree alongside the scripts, workflows, tests, and
versioned source-domain inputs used to rebuild that site.

## At A Glance

- Runtime files served by the site:
  `index.html`, `school.html`, `cuts.html`, `research.html`,
  `accreditation.html`, `methodology.html`, `styles.css`, `js/`, `assets/`,
  `data/`, `404.html`, `robots.txt`
- Source/build/editorial infrastructure kept in the public repo:
  `scripts/`, `data_pipelines/`, `tests/`, `docs/`, `.github/workflows/`,
  `renv.lock`, `requirements.txt`, `package.json`, `playwright.config.js`
- Local-only caches and private credentials stay out of Git through
  [`.gitignore`](./.gitignore)

## Start Here

| Need | Read |
|---|---|
| Rebuild, refresh, test, or troubleshoot the project | [docs/OPERATIONS_MANUAL.md](./docs/OPERATIONS_MANUAL.md) |
| Deploy or hand off the static site | [docs/DEPLOY_HANDOFF.md](./docs/DEPLOY_HANDOFF.md) |

## Runtime Vs Source Repo

| Area | Role |
|---|---|
| `index.html`, `school.html`, `cuts.html`, `research.html`, `accreditation.html`, `methodology.html` | Static entry points served directly |
| `styles.css`, `js/`, `assets/` | Browser runtime code and visual assets |
| `data/` | Committed JSON, per-school files, indexes, and the public download CSV |
| `scripts/`, `scripts/shared/` | R/Python rebuild entry points and helpers |
| `data_pipelines/` | Versioned source-domain inputs plus ignored local caches |
| `tests/` | R, Python, Node, Playwright, and accessibility checks |
| `docs/` | Setup notes, walkthroughs, runbooks, and deployment notes |

Root tooling files such as `package.json`, `package-lock.json`,
`playwright.config.js`, `requirements.txt`, `renv.lock`, `.Rprofile`, and
`.pa11yci.json` remain at the repo root because their tools expect them there.

## Local-Only Paths

Keep local caches, scratch outputs, and credentials out of Git. The main
ignored paths are:

- `node_modules/`
- `renv/library/`
- `.renv_cache/`
- `ipeds/cache/`, `ipeds/raw/`, `ipeds/derived/`
- `data_pipelines/*/cache/`
- `test-results/`
- `workbooks/`
- `.secrets/`
- `.private_docs/`

See [docs/OPERATIONS_MANUAL.md](./docs/OPERATIONS_MANUAL.md) for the fuller
output map, script inventory, source-data overview, and rebuild flow.

## Common Commands

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

Use `npm ci` on fresh machines or in CI. Use `npm install` only when you
intend to update `package-lock.json`.

## Build Summary

The rebuild, refresh, and troubleshooting details live in
[docs/OPERATIONS_MANUAL.md](./docs/OPERATIONS_MANUAL.md). The short version is:

1. Build or refresh the canonical IPEDS dataset.
2. Rebuild supporting joins for outcomes, cuts, accreditation, research, HCM,
   closure source artifacts, and federal composite scores.
3. Run `scripts/build_web_exports.R` to write the committed static data files.
4. Run smoke, browser, and accessibility checks before pushing.
