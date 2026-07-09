# Launch Checklist — financialtracker.hechingerreport.org

**Updated:** 2026-07-09 (rev 3 — cutover complete; cuts form live)
**Public repo:** `hechinger/FinancialHealth` — site only, will be **public**
**Private repo:** `hechinger/Private_Financial_Health_Tracker` — the pipeline
lives here permanently (migrated 2026-07-08; full history). The old
`marinav13/financial-health-tracker` repo is retired — every workflow in it
was disabled on 2026-07-09 after a green end-to-end test refresh in the
private repo. Its Pages preview gets unpublished at launch, then archive it.
**Launch date:** TBD

---

## How the two repos work now (read this first)

The public repo contains the static site, `data/` exports, and the pipeline
code (for transparency) — but **no `data_pipelines/` CSVs, no internal docs,
no secrets, and no pipeline workflows**. It cannot run the pipeline at all.

The private repo runs every workflow on its existing schedule, exactly as
today. Its two refresh workflows end with a **"Publish site data to public
repo"** step that clones the public repo, syncs `data/`, regenerates
`sitemap.xml`, and pushes as `The Hechinger Report <noreply@hechingerreport.org>`.
That step **skips quietly until the `PUBLIC_SITE_DEPLOY_TOKEN` secret exists**
(see pre-launch tasks) — so until then, the public repo's data is a static
snapshot of 2026-07-08.

There is no launch-day workflow flip anymore: the private cron never moves.

## Already done

- [x] Pipeline cutover complete (2026-07-09): end-to-end test refresh green
  in the private repo — data commit landed, the one new review-sheet row
  was the intended Virginia Wesleyan/Sentara merger candidate, publish
  step skipped pending the deploy token — and every workflow in
  `marinav13/financial-health-tracker` disabled
- [x] NWCCU rendered-directory snapshot committed under
  `data_pipelines/accreditation/cache_seed/` and copied into the scrape
  cache by both refresh workflows when missing (the page is JS-rendered
  and can't be fetched fresh; Actions caches don't survive repo migration
  or 7-day eviction)
- [x] Cuts submission form built and `cuts.html` submit link repointed to
  it in both repos (2026-07-09); CollegeCuts attribution links kept
- [x] Public repo rebuilt (single clean commit `2deafc0`): no proprietary
  fonts, no `data_pipelines/`, no `docs/`, no CLAUDE/AGENTS files, no
  personal emails — verified by automated scan before push
- [x] History secret scan: clean (only R-package maintainer emails in
  `renv.lock`, which is normal public metadata)
- [x] Secrets deleted from the public repo (it needs none)
- [x] Split rights in place of MIT: `LICENSE` (umbrella), `LICENSE-DATA.md`
  (CC BY 4.0), `LICENSE-CODE.md` (all rights reserved),
  `LICENSE-ASSETS.md` (all rights reserved); public README has a rights
  section
- [x] Google Analytics `G-E7R2SH9Q9K` on all six pages + 404.html (both repos)
- [x] Mojibake repaired at the export write boundary (word-validated ligature
  and apostrophe recovery; pipeline identity untouched); publishes with the
  next data refresh
- [x] Subdomain metadata, CNAME, robots.txt (allow + sitemap), sitemap.xml
  (auto-regenerated at every publish), per-school SEO tags
- [x] Public-repo CI trimmed to what runs without pipeline data (JS, Python,
  E2E, accessibility); workflows disabled until launch
- [x] DNS + HTTPS certificate proven working during the test publish
- [x] Ownership: **Marina** owns refresh failures, triages cuts submissions,
  and reviews weekly accreditation/cuts data

## Do before launch

- [ ] **Create the deploy token** so the private pipeline can publish to the
  public repo. Log in as **hechinger** (both repos live under it now):
  Settings → Developer settings → Personal access tokens → **Fine-grained
  tokens** → Generate: repository access **only FinancialHealth**,
  permissions **Contents: Read and write**, expiration 1 year (calendar a
  renewal). Then in `hechinger/Private_Financial_Health_Tracker`:
  Settings → Secrets and variables → Actions → new secret
  **`PUBLIC_SITE_DEPLOY_TOKEN`** with the token value. The next weekly
  refresh publishes automatically; or dispatch one to test.
- [ ] **Legal review of the license texts** (`LICENSE*`) — drafted to the
  agreed model (data CC BY 4.0, code/assets reserved) but not lawyer-reviewed.
  Must be settled before the repo goes public; a license can't be retracted
  from people who already received it.
- [ ] **Google Search Console**: get added to Hechinger's existing
  `hechingerreport.org` Domain property (covers the subdomain automatically),
  or create a Domain property for the subdomain via a DNS TXT record in the
  same panel as the CNAME.
- [ ] **Line up the redirect** from the previous tracker at
  `https://hechinger.org/interactives/fitness/` (URL confirmed 2026-07-08) —
  identify who manages that host so the 301 can flip on launch day.
- [x] ~~SECURITY.md / CODEOWNERS / issue templates~~ — **decided against**
  (2026-07-08). Do not re-raise.

## Launch day — repo side (Claude can run these)

1. Verify the private repo's last "Publish site data to public repo" step
   succeeded (data current in the public repo); dispatch a refresh if stale.
2. Enable the three site workflows in the public repo:
   ```
   gh workflow enable "Tests"                 -R hechinger/FinancialHealth
   gh workflow enable "Accessibility Checks"  -R hechinger/FinancialHealth
   gh workflow enable "Deployed Pages Parity" -R hechinger/FinancialHealth
   ```

## Launch day — admin side (GitHub login)

3. Make `hechinger/FinancialHealth` **public**.
4. Settings → Pages: deploy from `main` / root; custom domain fills from
   CNAME; tick **Enforce HTTPS**.
5. Fill the About sidebar (website + description).
6. Verify: site over HTTPS, a school page renders, GA4 Realtime shows your
   visit.

## Launch day — old site handoff

7. Unpublish the old repo's Pages site (`marinav13.github.io/...`).
8. Archive or make private `marinav13/financial-health-tracker` — it is
   retired (the pipeline moved to the hechinger account 2026-07-08); going
   non-public also removes the historical commits carrying a personal email
   from public view.
9. **301-redirect** `https://hechinger.org/interactives/fitness/` to
   `https://financialtracker.hechingerreport.org/` — server-side on whatever
   hosts that page (WordPress Redirection plugin or hosting config); hand
   source + destination to the web team.
10. Update tracker links in hechingerreport.org articles.

## Launch day + after — search & social

11. Search Console → Sitemaps → submit `sitemap.xml`.
12. Facebook Sharing Debugger + an X card check against the homepage
    (busts stale unpublished-era cache; confirms og-image).
13. Watch Search Console → Pages over the following weeks for school pages
    indexing (the per-school canonicals are what make them eligible).
14. Submit the property to Bing Webmaster Tools too.

---

## Open items not tied to launch

- [x] ~~Cuts submission CTA~~ — done 2026-07-09. Live form:
  <https://docs.google.com/forms/d/e/1FAIpQLSdKz0SCWdCtA2XJvDBuLmsnUZBQ5eOGukKJTbcbrehsRQpvhw/viewform?usp=dialog>
  (spec: [docs/CUTS_FORM_SPEC.md](CUTS_FORM_SPEC.md)). CollegeCuts
  *attribution* links stay.
- [ ] Move the Google review Sheet and the GCP service account under
  Hechinger-controlled accounts (both currently personal). When this
  happens: re-share the Sheet with the new service account, update
  `GOOGLE_SERVICE_ACCOUNT_JSON_B64` in the private repo.
- [ ] PAT renewal reminder for `PUBLIC_SITE_DEPLOY_TOKEN` (expires per the
  chosen lifetime; the publish step degrades to a skip-with-notice, so the
  site silently stops updating — put it on a calendar).

## Standing cautions

- The public repo's `data/` is written **only** by the private pipeline's
  publish step. Never edit data in the public repo by hand — the next
  publish overwrites it (`rsync --delete`).
- Code changes must be made in the private repo and ported to the public one
  (the site files are the same; the public repo lacks `data_pipelines/`,
  `docs/`, agent files, pipeline workflows, and carries its own README,
  licenses, robots.txt, and metadata URLs).
- The pipeline repo's `robots.txt` stays `Disallow: /` (its content is
  never served anywhere once the old preview site is unpublished).

---

## Appendix: current license texts (drafts pending legal review)

Exactly what is in the public repo today. Legal should review these before
the repo goes public.

### `LICENSE` (umbrella)

```
College Financial Health Tracker - Rights

Copyright (c) 2026 The Hechinger Report

This repository is public for transparency and reference. Different
rights apply to different parts of it:

- Data (the data/ directory and public downloadable datasets):
  reusable with attribution under Creative Commons Attribution 4.0
  International (CC BY 4.0). See LICENSE-DATA.md.

- Code (HTML, CSS, JavaScript, R, Python, workflow definitions, and
  tests): all rights reserved. Published for transparency; not
  licensed for reuse without written permission. See LICENSE-CODE.md.

- Assets (logos, branding, illustrations, and images under assets/):
  all rights reserved. See LICENSE-ASSETS.md.

For permissions and questions: https://hechingerreport.org/contact/
```

### `LICENSE-DATA.md`

```
# Data License (CC BY 4.0)

The datasets in the `data/` directory, and the CSV downloads offered by
the live site, are licensed under the Creative Commons Attribution 4.0
International license (CC BY 4.0):
https://creativecommons.org/licenses/by/4.0/

You may copy, redistribute, and adapt this data for any purpose,
including commercially, provided you give appropriate credit.

Suggested attribution:

> Source: College Financial Health Tracker, The Hechinger Report
> (https://financialtracker.hechingerreport.org/)

## Third-party source note

The tracker combines records from U.S. Department of Education sources
(IPEDS, DAPIP, College Scorecard, Federal Student Aid), which are U.S.
government works, with entries derived from the CollegeCuts Tracker and
Grant Witness. Reusers who redistribute at scale should review those
projects' own terms; this license covers The Hechinger Report's
compilation, curation, and derived fields.
```

### `LICENSE-CODE.md`

```
# Code License

Copyright (c) 2026 The Hechinger Report. All rights reserved.

The code in this repository (HTML, CSS, JavaScript, R, Python, GitHub
Actions workflow definitions, and tests) is published so readers can
inspect how the College Financial Health Tracker is built. It is NOT
licensed for reuse: you may view it here, but you may not copy, modify,
redistribute, or use it in other projects without written permission
from The Hechinger Report.

For permissions: https://hechingerreport.org/contact/
```

### `LICENSE-ASSETS.md`

```
# Assets License

Copyright (c) 2026 The Hechinger Report. All rights reserved.

Logos, branding, wordmarks, illustrations, screenshots, and other
images in this repository (primarily under `assets/`) are not licensed
for reuse. The Hechinger Report name and logo identify The Hechinger
Report and may not be used in ways that suggest endorsement or
affiliation.

Brand fonts are not distributed in this repository.

For permissions: https://hechingerreport.org/contact/
```
