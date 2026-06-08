# Deploy Handoff

This is the deploy-only handoff for the live interactive:

[https://hechingerreport.org/college-financial-health-tracker/](https://hechingerreport.org/college-financial-health-tracker/)

For rebuilds, refreshes, tests, and failure triage, use
[docs/OPERATIONS_MANUAL.md](./OPERATIONS_MANUAL.md).

## Deploy This Runtime Surface

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

## Do Not Treat These As Deploy Artifacts

- `scripts/`
- `data_pipelines/`
- `tests/`
- `docs/`
- `.github/workflows/`
- `renv/`, `renv.lock`, `.Rprofile`
- `requirements.txt`, `package.json`, `package-lock.json`, `playwright.config.js`

## Publication Notes

- `robots.txt` is intentionally restrictive right now and should be relaxed as
  a final publication step.
- `LICENSE` has not been added yet and should be decided before or at launch.
- Most routine refreshes change files under `data/`.
- HTML, CSS, JS, and image assets change only when the interactive itself
  changes.

## Asset Notes

- Shared social preview image: `assets/og-image.png`
- Shared section-page illustration: `assets/college-financial-health-school.png`

## Deployment Checks

After deployment, confirm:

1. the home page loads
2. `school.html?unitid=172264` loads correctly
3. `cuts.html`, `accreditation.html`, and `research.html` populate
4. social cards resolve `assets/og-image.png`
