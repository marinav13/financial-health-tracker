# Launch Checklist — financialtracker.hechingerreport.org

**Updated:** 2026-07-08 (rev 2 — architecture changed: derived CSVs are not public)
**Public repo:** `hechinger/FinancialHealth` — site only, will be **public**
**Private repo:** `marinav13/financial-health-tracker` — the pipeline lives here
**permanently**; goes **private** at launch and keeps running everything
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
  public repo. As a user with write access to `hechinger/FinancialHealth`:
  GitHub → Settings → Developer settings → Personal access tokens →
  **Fine-grained tokens** → Generate: Resource owner **hechinger**, repository
  access **only FinancialHealth**, permissions **Contents: Read and write**,
  expiration 1 year (calendar a renewal). If the org doesn't allow
  fine-grained PATs, a classic token with `repo` scope works but is broader.
  Then in the **private** repo: Settings → Secrets and variables → Actions →
  new secret **`PUBLIC_SITE_DEPLOY_TOKEN`** with the token value. The next
  weekly refresh will publish automatically; or dispatch one to test.
- [ ] **Legal review of the license texts** (`LICENSE*`) — drafted to the
  agreed model (data CC BY 4.0, code/assets reserved) but not lawyer-reviewed.
  Must be settled before the repo goes public; a license can't be retracted
  from people who already received it.
- [ ] **Google Search Console**: get added to Hechinger's existing
  `hechingerreport.org` Domain property (covers the subdomain automatically),
  or create a Domain property for the subdomain via a DNS TXT record in the
  same panel as the CNAME.
- [ ] **Confirm the old tracker's exact URL** for the redirect (noted as
  `https://hechinger.org/interactives/fitness/` — confirm hechinger.org vs
  hechingerreport.org) and identify who manages that host.
- [ ] Decide on `SECURITY.md`, `CODEOWNERS`, and Issues policy for the public
  repo (explanations provided 2026-07-08; quick to add once decided).

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

7. Unpublish the dev repo's Pages site (`marinav13.github.io/...`).
8. Make `marinav13/financial-health-tracker` **private** (Actions keep
   running on private repos — the pipeline is unaffected). This also removes
   the historical commits carrying a personal email from public view.
9. **301-redirect the previous tracker** (URL above, once confirmed) to
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

- [ ] Cuts submission CTA: create a Hechinger-owned Google Form and repoint
  the submit link in `cuts.html` (CollegeCuts *attribution* links stay).
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
- The dev repo's `robots.txt` stays `Disallow: /` while its preview site
  exists; the dev preview site disappears at launch anyway.
