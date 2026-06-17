# Financial Health Tracker

Public source repo for The Hechinger Report's College Financial Health Tracker.

Live site:
[https://hechingerreport.org/college-financial-health-tracker/](https://hechingerreport.org/college-financial-health-tracker/)

This repo is the canonical source for the public interactive.

## Source Of Truth

- Concise repo overview: `README.md`
- Rebuild, refresh, testing, workflows, and troubleshooting:
  [docs/OPERATIONS_MANUAL.md](./docs/OPERATIONS_MANUAL.md)
- Deployment-only handoff:
  [docs/DEPLOY_HANDOFF.md](./docs/DEPLOY_HANDOFF.md)

## What Ships

- `index.html`, `school.html`, `cuts.html`, `research.html`,
  `accreditation.html`, `methodology.html`, `404.html`
- `styles.css`
- `js/`
- `assets/`
- `data/`
- `robots.txt`

## What Stays In The Source Repo

- `scripts/`, including `scripts/archive/` for historical one-time utilities
- `data_pipelines/`
- `tests/`
- `docs/`
- `.github/workflows/`
- root tooling files such as `package.json`, `requirements.txt`,
  `playwright.config.js`, `renv.lock`, and `.pa11yci.json`

## Common Checks

```bash
Rscript ./tests/run_shared_helper_smoke_tests.R
npm run test:smoke
npm run test:e2e
npm run test:a11y
```

Keep local caches, scratch outputs, and private working files out of Git via
[`.gitignore`](./.gitignore). The operational details, output map, and refresh
flow live in [docs/OPERATIONS_MANUAL.md](./docs/OPERATIONS_MANUAL.md).
