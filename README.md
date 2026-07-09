# Financial Health Tracker — Pipeline

Private source-and-pipeline repo for The Hechinger Report's College
Financial Health Tracker.

Live site:
[https://financialtracker.hechingerreport.org/](https://financialtracker.hechingerreport.org/)

The site is served by GitHub Pages from the public site repo,
[hechinger/FinancialHealth](https://github.com/hechinger/FinancialHealth).
This repo runs every scheduled workflow and holds the pipeline code, the
working `data_pipelines/` CSVs (including review and editorial-override
records), and the internal docs. Its refresh workflows publish the
finished `data/` exports and a regenerated sitemap to the public repo;
the public repo cannot run the pipeline.

## Source Of Truth

- Concise repo overview: `README.md`
- Rebuild, refresh, testing, workflows, and troubleshooting:
  [docs/OPERATIONS_MANUAL.md](./docs/OPERATIONS_MANUAL.md)
- How the site deploys and how data reaches the public repo:
  [docs/DEPLOY_HANDOFF.md](./docs/DEPLOY_HANDOFF.md)
- Two-repo architecture and launch runbook:
  [docs/LAUNCH_CHECKLIST.md](./docs/LAUNCH_CHECKLIST.md)

## What Ships (via the publish step and the public repo)

- `index.html`, `school.html`, `cuts.html`, `research.html`,
  `accreditation.html`, `methodology.html`, `404.html`
- `styles.css`
- `js/`
- `assets/`
- `data/`

Code changes are made here first, then ported to the public repo (which
carries its own README, licenses, robots.txt, and metadata URLs). `data/`
in the public repo is written only by this repo's publish step. This
repo's `robots.txt` intentionally stays `Disallow: /`.

## What Stays In This Repo Only

- `scripts/`, including `scripts/archive/` for historical one-time utilities
- `data_pipelines/`
- `docs/`
- pipeline workflows (`refresh-ipeds-site-data.yml`, `refresh-ipeds-full.yml`,
  `publish-editorial-overrides.yml`)
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
