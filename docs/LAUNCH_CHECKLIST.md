# Launch Checklist — financialtracker.hechingerreport.org

**Updated:** 2026-07-08
**Production repo:** `hechinger/FinancialHealth` (will be **public**)
**Dev/preview repo:** `marinav13/financial-health-tracker` (goes **private** at launch)
**Launch date:** TBD

---

## Already done (no action needed)

- [x] Repo copied to `hechinger/FinancialHealth` with fresh history — no
  proprietary fonts (National/Tiempos) in tree or history; safe to make public
- [x] MIT LICENSE (© 2026 The Hechinger Report)
- [x] All metadata (canonical, og:url, og:image, twitter:image, structured
  data) points at `https://financialtracker.hechingerreport.org`
- [x] `CNAME` file in repo root — Pages custom-domain field auto-fills from it
- [x] DNS: `financialtracker` CNAME → `hechinger.github.io` (verified working;
  HTTPS certificate was issued during the test publish)
- [x] Actions secrets set on the new repo: `GOOGLE_SERVICE_ACCOUNT_JSON_B64`,
  `ACCREDITATION_REVIEW_SHEET_ID`
- [x] All workflows in the new repo **disabled** until launch (prevents the
  weekly pipeline running in two repos against the same review Sheet)
- [x] `robots.txt` allows crawling and references the sitemap (the dev repo's
  copy stays `Disallow: /` on purpose)
- [x] `sitemap.xml` committed (6 pages + all per-school URLs) and regenerates
  automatically on the IPEDS release cadence: the Refresh IPEDS Full Dataset
  workflow (Jan 7 / Sep 24 / Dec 10) rebuilds it via `scripts/build_sitemap.py`
- [x] Per-school SEO: `school.js` sets title, canonical, og:url, og:title,
  twitter:title per school at render time, so the ~1,890 `?unitid=` pages can
  index individually
- [x] Commit history attributed to `The Hechinger Report
  <noreply@hechingerreport.org>` — no personal emails

---

## Do now (before launch, any time)

- [ ] **Google Search Console access.** Easiest path: Hechinger almost
  certainly has a GSC *Domain property* for `hechingerreport.org`, which
  automatically covers the subdomain — ask the admin to add you as a user
  (Settings → Users and permissions). Otherwise create a Domain property for
  `financialtracker.hechingerreport.org` yourself: GSC shows a
  `google-site-verification=...` string; add it as a **TXT** record named
  `financialtracker` in the same DNS panel where the CNAME lives; click
  Verify (retry after 30–60 min if needed). Leave the TXT record in place
  permanently.
- [ ] **Google Analytics stream** (waiting on credentials). See the GA section
  below for the exact steps once access exists. The site currently has **no
  analytics at all** — without this, day-one traffic is invisible.
- [ ] **Confirm the old tracker's exact URL** for the redirect (written down as
  `https://hechinger.org/interactives/fitness/` — confirm whether it is
  hechinger.org or hechingerreport.org) and find who manages that host, so the
  redirect (below) is ready to flip on launch day.

---

## Launch day — repo side (Claude can run all of these on request)

1. Re-enable the six workflows in `hechinger/FinancialHealth`:
   ```
   gh workflow enable "Refresh Source Data Weekly"      -R hechinger/FinancialHealth
   gh workflow enable "Refresh IPEDS Full Dataset"      -R hechinger/FinancialHealth
   gh workflow enable "Publish Editorial Overrides"     -R hechinger/FinancialHealth
   gh workflow enable "Deployed Pages Parity"           -R hechinger/FinancialHealth
   gh workflow enable "Tests"                           -R hechinger/FinancialHealth
   gh workflow enable "Accessibility Checks"            -R hechinger/FinancialHealth
   ```
2. Dispatch one **Refresh Source Data Weekly** run there to bring data current.
3. Disable the weekly cron in the dev repo so only one pipeline writes to the
   review Sheet (the commit is already written — cherry-pick `7fd6c427f`).

## Launch day — admin side (GitHub login)

4. Make `hechinger/FinancialHealth` **public** (Settings → General → Danger
   Zone → Change visibility).
5. Settings → Pages: deploy from `main` / root. Custom domain fills from the
   CNAME file. Tick **Enforce HTTPS** (cert was issued before; should be
   available immediately or within minutes).
6. Fill the **About sidebar**: website `https://financialtracker.hechingerreport.org`,
   short description.
7. Verify the site loads over HTTPS and a school page renders.

## Launch day — hand-off to the old site

8. **Unpublish** `marinav13.github.io/financial-health-tracker` (Settings →
   Pages → Unpublish site in the dev repo).
9. Make `marinav13/financial-health-tracker` **private** — this also removes
   the 515 historical commits carrying a personal email from public view.
10. **Redirect the previous tracker** at `https://hechinger.org/interactives/fitness/`
    (confirm URL, see above) to `https://financialtracker.hechingerreport.org/`.
    This is a server-side 301 on whatever hosts that page — for a WordPress
    site, the Redirection plugin or an entry in the hosting config; hand the
    source and destination URLs to the web team. A 301 preserves any
    search-engine standing the old tracker URL has.
11. Update links on hechingerreport.org articles/pages that point at the old
    tracker or the preview URL.

## Launch day + after — search & social

12. Submit the sitemap in Search Console (Sitemaps → enter `sitemap.xml` →
    Submit). Only possible once the site is live.
13. Run the social-card validators against the homepage to bust any stale
    cache: Facebook Sharing Debugger (developers.facebook.com/tools/debug)
    and a test post/card check for X. Confirms the og-image renders.
14. Over the following weeks, watch Search Console → Pages for the school
    pages getting indexed.

---

## Google Analytics setup (when credentials arrive)

1. In the Hechinger GA4 property (analytics.google.com → Admin), under
   **Data streams**, add a **Web** stream: URL
   `https://financialtracker.hechingerreport.org`, name it
   "Financial Health Tracker". GA shows a **Measurement ID** like `G-XXXXXXXXXX`.
   (A separate stream keeps tracker traffic distinguishable from the main
   site; it can live in the same property.)
2. Give the Measurement ID to Claude — the gtag snippet then gets added to the
   `<head>` of all six pages plus `404.html` in one commit (both repos, so
   they stay in sync).
3. After launch, confirm hits appear in GA4 Realtime while visiting the site.

---

## Folded in from PRE_LAUNCH_CHECKLIST.md (June 30) — still open

Items from the earlier root-level checklist not yet done, updated for
decisions made since:

### Needs an editorial/legal decision

- [ ] **Licensing conflict to resolve.** The repo now carries a plain MIT
  LICENSE (added 2026-07-08 on request). The June 30 plan recommended split
  rights instead: data reusable (CC BY 4.0), code all-rights-reserved,
  assets/branding all-rights-reserved, plus a README rights section and
  LICENSE-DATA/CODE/ASSETS files. MIT grants broad reuse of everything in the
  repo. Decide which model Hechinger actually wants before the repo goes
  public; if split rights, the MIT file must be replaced before launch, not
  after.
- [ ] **Cuts submission CTA.** `cuts.html` sends readers to college-cuts.com
  to submit cuts. Create a Hechinger-owned Google Form and repoint the
  submission link (the *attribution* links to CollegeCuts in cuts.html and
  methodology.html should stay — that's source credit). Confirm all other
  contact/correction links point at Hechinger-owned destinations.
- [ ] **Which derived CSVs stay public.** `data_pipelines/` ships review
  candidates, editorial overrides (including reviewer initials and notes
  columns), audit CSVs, etc. Decide what belongs in a public repo vs
  internal-only — before flipping visibility, since removing files later
  leaves them in history.

### Repo governance (public-repo hygiene)

- [ ] Add `SECURITY.md` (how to report a vulnerability) — does not exist yet.
- [ ] Add `CODEOWNERS` — does not exist yet.
- [ ] Decide whether GitHub Issues are enabled on the public repo; if yes,
  add issue templates. Note the pipeline auto-files `pipeline-drift` issues,
  so Issues should probably stay on.
- [ ] Run a history secret scanner (Gitleaks, or enable GitHub secret
  scanning, which is free on public repos) against `hechinger/FinancialHealth`.
  Low risk — the history is 8 fresh commits — but cheap to confirm. A June 30
  local scan of the dev repo found nothing.

### Content quality

- [ ] **Fix mojibake in public-facing text.** Encoding artifacts (a
  replacement character where apostrophes should be) exist in scraped data
  that reaches the site, e.g. in `data/accreditation.json`
  ("the institution�s accreditation"). Root cause is upstream PDF/HTML
  extraction encoding; fix in the pipeline text-cleaning layer, not by hand.

### Launch verification (from the June 30 list, still the right drill)

- [ ] `Rscript ./tests/run_shared_helper_smoke_tests.R`
- [ ] `npm run test:smoke` / `npm run test:e2e` / `npm run test:a11y`
- [ ] Manual pass: home page, several `school.html?unitid=...` pages,
  cuts/accreditation/research/methodology pages, CSV downloads, mobile
  layout, chart tooltips, social preview cards.

### After launch (ownership)

- [ ] Assign an owner for weekly-refresh failures (the pipeline-drift GitHub
  Issues need a subscriber who acts on them).
- [ ] Assign editorial triage ownership for the cuts submission workflow.
- [ ] Move Google Sheets review queue + the GCP service account under
  Hechinger-controlled accounts (both currently live in a personal GCP
  project/Google account).
- [ ] Submit the property to Bing Webmaster Tools as well as Google Search
  Console.

### Superseded June 30 items (no action)

- "Do not get institution detail pages indexed before launch" — superseded
  by the explicit 2026-07-08 decision to ship the full sitemap and
  per-school canonicals at launch.
- Font decision — resolved: font binaries excluded from the public repo and
  its history; site loads fonts from hechingerreport.org.
- Repo move, secrets, Pages-parity URLs, robots/sitemap, canonical/social
  metadata — all done (see top of this document).
- "No named slug routes before launch" — unchanged behavior; `unitid` remains
  the lookup key. Readable slugs can be added later without breaking it.

## Standing cautions

- **One pipeline at a time.** The weekly refresh must only be enabled in one
  repo (both share the same Google review Sheet). Today: dev repo runs it.
  After launch: production runs it, dev's cron off.
- **Code changes made in the dev repo do not propagate automatically** to
  `hechinger/FinancialHealth` — port commits explicitly (this has been done
  for everything through 2026-07-08).
- The dev repo's `robots.txt` must stay `Disallow: /` as long as its preview
  site exists.
