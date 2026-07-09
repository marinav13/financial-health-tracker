# Deploy Handoff

How The Hechinger Report's College Financial Health Tracker gets deployed.
For rebuilds, refreshes, tests, and failure triage, use
[docs/OPERATIONS_MANUAL.md](./OPERATIONS_MANUAL.md). For the launch runbook
and two-repo architecture detail, use
[docs/LAUNCH_CHECKLIST.md](./LAUNCH_CHECKLIST.md).

Live site:
[https://financialtracker.hechingerreport.org/](https://financialtracker.hechingerreport.org/)

## How the site is served

Nobody deploys files by hand. The site is served by **GitHub Pages** from
the public repo, [hechinger/FinancialHealth](https://github.com/hechinger/FinancialHealth)
(`main` branch, root), with the custom domain set by that repo's `CNAME`
file. This private repo is never served anywhere; its `robots.txt`
intentionally stays `Disallow: /`.

## How data reaches the site

Both refresh workflows in this repo (`refresh-ipeds-site-data.yml` weekly,
`refresh-ipeds-full.yml` three times a year) end with a **"Publish site
data to public repo"** step: it clones the public repo, syncs `data/`
(`rsync --delete`), regenerates `sitemap.xml`, and pushes as
`The Hechinger Report <noreply@hechingerreport.org>`.

- The step requires the `PUBLIC_SITE_DEPLOY_TOKEN` secret (a fine-grained
  PAT with Contents read/write on `hechinger/FinancialHealth`). Until the
  secret exists, the step skips with a notice and the public repo's data
  stays a static snapshot.
- Never edit `data/` in the public repo by hand — the next publish
  overwrites it.

## How code changes reach the site

HTML, CSS, and JS changes are made in this repo first, then ported to the
public repo in a matching commit. The public repo differs deliberately:
no `data_pipelines/`, no `docs/`, no agent files, no pipeline workflows,
and its own `README.md`, licenses (`LICENSE*`), `robots.txt` (allow +
sitemap), and subdomain metadata URLs. Keep those differences intact when
porting.

## Post-publish checks

After a publish (or any site-affecting change), confirm on the live site:

1. the home page loads
2. `school.html?unitid=172264` loads correctly
3. `cuts.html`, `accreditation.html`, and `research.html` populate
4. social cards resolve `assets/og-image.png`

## Asset notes

- Shared social preview image: `assets/og-image.png`
- Shared section-page illustration: `assets/college-financial-health-school.png`
- Brand fonts are not distributed in either repo; the live site loads them
  from hechingerreport.org.
