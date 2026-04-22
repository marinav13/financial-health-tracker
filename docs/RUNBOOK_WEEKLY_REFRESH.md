# Runbook: Weekly Refresh Failure

The weekly refresh runs every Monday at noon via GitHub Actions
(`.github/workflows/refresh-ipeds-site-data.yml`). This document tells you
what to do when it fails.

---

## Step 1 — Find the failed step

1. Go to the repo on GitHub → **Actions** tab.
2. Click the most recent **Refresh Source Data Weekly** run.
3. Expand the failing step (red ✗). Read the error message — most failures
   are self-explanatory once you find them.

---

## Step 2 — Diagnose by which step failed

### "Refresh accreditation actions"
**Likely cause:** One of the six accreditor websites changed its HTML
structure, or the site was temporarily down.

**Fix:**
- Re-run the step manually: `Rscript --vanilla ./scripts/build_accreditation_actions.R`
- If it fails locally, open the relevant accreditor's webpage and compare
  its HTML to the parsing logic in `scripts/shared/accreditation_scrapers.R`.
- The scraper for MSCHE (`parse_msche`) will warn if the page is
  JavaScript-rendered — check console output for that warning.
- If the site is just temporarily down, re-run the workflow in 24 hours.
  The scraper will fall back to its local cache and the run will succeed
  without fresh accreditation data. Check the output to confirm the cache
  date is recent enough to publish.

---

### "Refresh college cuts"
**Likely cause:** The Supabase credentials are missing or expired, or the
CollegeCuts API is down.

**Fix:**
- Check that the `COLLEGE_CUTS_SUPABASE_URL` and
  `COLLEGE_CUTS_SUPABASE_ANON_KEY` secrets are set in the repo:
  GitHub → Settings → Secrets and variables → Actions.
- If the secrets are present but the run fails with an HTTP error (401 or
  403), the anon key may have been rotated. Get the new key from the
  CollegeCuts Supabase project and update the secret.
- If the Supabase service is down, re-run the workflow later. The script
  uses a local cache fallback — if the cache is recent, the run will
  complete using cached data.

---

### "Build research join" / "Refresh USAspending analysis"
**Likely cause:** The Grant Witness API returned unexpected data, or
`manual_include.csv` / `manual_match_overrides.csv` has a malformed row.

**Fix:**
- Run locally: `Rscript --vanilla ./scripts/build_grant_witness_join.R`
- If it fails on a join contract (e.g. "duplicate unitids"), open the
  review file at `data_pipelines/grant_witness/analysis/` and look for
  newly added grants that matched to multiple schools.
- Fix duplicates by adding a row to `data_pipelines/grant_witness/manual_match_overrides.csv`.

---

### "Import closure outputs from Google Sheet"
**Likely cause:** The Google Sheet URL changed, the sheet structure changed,
or the sheet is not publicly readable.

**Fix:**
- Open the sheet URL from the workflow env var (`CLOSURE_SHEET_URL`) in a
  browser. Confirm it loads and the expected tabs are present
  (`running_closures`, `confirmed_closures`, `not_a_closure`).
- If the URL changed, update `CLOSURE_SHEET_URL` in the workflow file.
- If tab names changed, update `scripts/import_closure_sheet.py` to match.

---

### "Rebuild static web exports"
**Likely cause:** A data file produced by an earlier step is malformed or
missing, causing the R export to fail.

**Fix:**
- Run locally: `Rscript --vanilla ./scripts/build_web_exports.R`
- The error message will name the missing file. Trace back to the step that
  should have produced it and re-run that step in isolation.

---

### "Commit and push updated data"
**Likely cause:** A concurrent workflow run is already holding the branch,
or the bot's push permissions were changed.

**Fix:**
- Check if another run of the same workflow is in progress — GitHub will
  queue the second one, so wait for the first to finish.
- If permissions are the problem, check that the workflow has
  `permissions: contents: write` (it does by default in this repo).

---

## Step 3 — After fixing, verify the output

Before the site goes live with refreshed data, spot-check at least two
schools and the browse pages:

1. Open `school.html?unitid=172264` (University of Michigan — public) and
   confirm financial charts load.
2. Open `school.html?unitid=190150` (Columbia University — private) and
   confirm research funding section loads.
3. Open `cuts.html` and confirm the Cuts table has rows.
4. Open `accreditation.html` and confirm the accreditation table has rows.
5. Open `research.html` and confirm the research funding table has rows.

If any page is blank or shows an error, check the browser console for
a failed `fetch()` call — that points directly to the broken data file.

---

## Step 4 — What NOT to do

- **Do not manually edit files in `data/`** and commit them without running
  the pipeline. The JSON files are machine-generated and will be overwritten
  on the next successful run.
- **Do not cancel a running workflow** that has already started the
  "Commit and push" step — partial commits can leave the `data/` folder in
  an inconsistent state.
- **Do not disable the weekly schedule** to silence a failing job. Fix the
  underlying problem; disabling the schedule means the published data goes
  stale silently.

---

## Contacts and resources

| Resource | Location |
|---|---|
| Full pipeline walkthrough | `docs/WALKTHROUGH.md` |
| Refresh cycle and data sources | `docs/REFRESH_CYCLE.md` |
| Accreditor scraper logic | `scripts/shared/accreditation_scrapers.R` |
| CollegeCuts Supabase project | Set in repo secrets — ask the data team for access |
| Grant Witness API | `data_pipelines/grant_witness/` — see README there |
| Google closure sheet | `CLOSURE_SHEET_URL` env var in the workflow file |
