# Website Manager Note

This repo contains everything needed to publish the interactive — HTML, CSS, JavaScript, and data files. No build step is required. The files are ready to serve statically.

**Target URL:** `https://hechinger.org/interactives/fitness/`

---

## What's currently at that URL

`hechinger.org/interactives/fitness/` currently redirects to `tuitiontracker.org/fitness/`, which is a completely different, older codebase built with jQuery 2.2.3 and Bootstrap 3.3.7. That old site uses bundled JavaScript (`js/scripts-bundle.js`) and has no tab navigation, accreditation data, research funding cuts, or college cuts sections.

The new site in this repo **replaces** it entirely. There are no shared files, no shared dependencies, and no compatibility concerns. The new site is vanilla JavaScript with no external framework dependencies.

---

## Files to deploy

Copy these from the repo into the `interactives/fitness/` directory on the web server:

```
index.html          — main landing/search page
school.html         — individual school profile page
accreditation.html  — accreditation actions browser
cuts.html           — college cuts/layoffs browser
research.html       — research funding cuts browser
styles.css          — all styles (one file, no preprocessor)
js/                 — all JavaScript (five files, no bundler needed)
data/               — all JSON data (schools/, plus top-level index files)
robots.txt          — already set to noindex during soft launch
```

Do **not** deploy: `scripts/`, `tests/`, `data_pipelines/`, `docs/`, `renv/`, or any `.R`/`.py` files. Those are backend pipeline tools, not web assets.

The school page expects URLs like:
```
/interactives/fitness/school.html?unitid=172264
```

All asset paths in the HTML are relative, so the files work correctly at any base path as long as the folder structure above is preserved.

---

## Recommended launch steps

1. Clone or download the repo from `main`.
2. Copy the static files listed above into the server's `interactives/fitness/` directory.
3. Remove or repoint the existing redirect from `hechinger.org/interactives/fitness/` to `tuitiontracker.org/fitness/`.
4. **Test on staging first** before cutting over the live URL.
5. Once satisfied, swap staging to production.

---

## Pre-launch checklist

- [ ] Landing page loads at the new URL
- [ ] School search returns results and navigates correctly
- [ ] Several school pages open (check profile cards, charts, tabs)
- [ ] Charts load and show hover/tooltip values
- [ ] CSV download button produces a file
- [ ] Accreditation, Cuts, and Research Funding tabs load data
- [ ] Tuition tab link opens tuitiontracker.org in a new tab
- [ ] Layout looks correct on mobile (375px width)
- [ ] Methodology panels expand
- [ ] Methodology text is final on every public page
- [ ] Page-level `meta name="robots"` tags are switched from `noindex, nofollow, noarchive, nosnippet` to the production indexing policy
- [ ] `robots.txt` is updated from `noindex` to `index, follow` once ready to go public

### Launch Gates Still Intentional

The repository currently keeps noindex directives in both `robots.txt` and the
HTML page metadata so unfinished staging pages are not indexed. The methodology
panels also remain editorial launch gates. Do not remove those blockers until
the replacement methodology copy is complete and the site is cleared for public
indexing.

**Test schools to verify breadth:**
- A private nonprofit (e.g., Columbia University, unitid=190150)
- A public flagship (e.g., University of Michigan, unitid=170976)
- A school with research funding cuts data
- A school with accreditation actions
- A school with college cuts/layoffs data

---

## Dependencies the new site does NOT need

The old site at tuitiontracker.org required jQuery, Bootstrap, jQuery UI, and Font Awesome. The new site has **zero external JavaScript or CSS dependencies**. There is nothing to install or bundle. Serve the files as-is.

---

## Data refresh after launch

The data auto-refreshes via GitHub Actions on a weekly schedule (`.github/workflows/refresh-ipeds-site-data.yml` runs every Monday at noon). It rebuilds the data files and commits them back to the repo. Your deploy pipeline should pull from the repo and re-copy the `data/` directory to the server after each refresh commit.

The five static asset files (`index.html`, `school.html`, `styles.css`, `js/`, `robots.txt`) only change when the codebase changes, not on every data refresh.

### What the weekly refresh does

1. Scrapes current accreditation actions from six accreditor websites
2. Fetches current college cuts from Supabase
3. Fetches current grant witness (research funding cuts) data
4. Imports federal closure dates from the Google Sheet
5. Rebuilds all `data/` JSON files
6. Commits updated `data/` back to the repo

### What it does NOT do

- Does not deploy to the web server (your deploy system handles that)
- Does not require Google Cloud credentials
- Does not re-run the full IPEDS pipeline (that only runs 1–2× per year manually, when federal data refreshes)

---

## Annual IPEDS refresh (once a year, manual)

Once a year (typically March–April when the new IPEDS data is released), someone with R installed runs the full pipeline:

```r
source("scripts/build_ipeds_dataset.R")
main(c("--start-year", "2014", "--end-year", "2025"))  # update year
source("scripts/build_web_exports.R"); main()
```

See `docs/REFRESH_CYCLE.md` for the complete refresh playbook.

---

## Repo and branch

- **Repo:** `https://github.com/marinav13/financial-health-tracker`
- **Branch for this release:** `codex/refresh-closure-wif` (merge to `main` before final deploy)
- **Latest data as of:** April 2026
