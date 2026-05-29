# Scraper Drift Runbook

The weekly refresh workflow
(`.github/workflows/refresh-ipeds-site-data.yml`) tees every scraper step's
stdout and stderr into `refresh-logs/combined.log`. A final **Report
scraper drift warnings** step greps that log for the warning patterns below.
If any appear, the workflow surfaces them as warnings for human triage while
continuing the weekly refresh.

The weekly workflow now rebuilds public JSON from the last committed reviewed
snapshot, so fresh scraper anomalies no longer block the run just because a
source changed shape. Use this document to decide what to do about each
warning pattern.

The full log is attached as the `refresh-logs` CI artifact (retention: 30
days).

---

## Warning patterns and remediation

### `warn_on_empty_parse` / `Returning empty table`

**What it means.** An accreditor page came back non-empty (HTML > 2 KB) but
the scraper parsed zero action rows out of it. That almost always means the
site changed its HTML structure — an XPath or CSS selector in
`scripts/shared/accreditation_scrapers.R` no longer matches.

**Triage steps.**
1. Identify the accreditor from the log line (e.g. `HLC: parsed 0 rows
   from https://www.hlcommission.org/...`).
2. Open that URL in a browser and confirm there ARE actions listed (some
   accreditors go quiet for weeks and a genuine zero is possible).
3. If actions are present, inspect the HTML and compare it against the
   parser function (`parse_hlc`, `parse_msche`, `parse_sacscoc`,
   `parse_neche`, `parse_nwccu`, `parse_wscuc`, `parse_accjc`) in
   `scripts/shared/accreditation_scrapers.R`.
4. Patch the selector or parsing logic. Run
   `Rscript ./scripts/build_accreditation_actions.R --financial-input
   data/downloads/full_dataset.csv` locally to verify the fix.
5. Commit the parser change and re-run the refresh workflow from the
   Actions tab.

**If the page is legitimately empty,** the warning is a false alarm this
week. Options:
- Lower the `threshold_bytes` argument on the `warn_on_empty_parse` call
  so small-but-valid pages don't trip the gate.
- Or (preferred) leave the gate strict and re-run next week.

---

### `warn_if_scrape_count_dropped`

**What it means.** The count of action rows for one accreditor dropped
substantially week-over-week. Possible causes, in descending likelihood:
1. The scraper is silently returning a truncated parse (partial drift —
   worse than a full-empty because fewer alarms fire).
2. The accreditor pulled old actions off the live page (they do this on
   purpose, periodically). Legitimate but worth verifying.
3. An action type was renamed and now doesn't match the classifier.

**Triage steps.**
1. Read the log line — it tells you which accreditor and the old/new row
   counts.
2. Diff this week's `data/accreditation.json` against the previous
   committed version to see which rows disappeared:
   `git diff HEAD~1 -- data/accreditation.json | head -200`.
3. Spot-check 3–5 disappeared rows against the live accreditor page. If
   they are still there, the parser is broken — see the
   `warn_on_empty_parse` section above for fix steps.
4. If they are legitimately gone, the warning is a false alarm. You can
   either:
   - Raise the tolerance threshold in `warn_if_scrape_count_dropped`'s
     call site, or
   - Commit a new baseline and re-run.

---

### `warn_if_action_type_dropped`

**What it means.** A specific action type (e.g. "Probation", "Warning",
"Show Cause") went from N rows to 0. This is the most suspicious pattern
because total row counts can mask category-level regressions.

**Triage steps.**
1. Log line format:
   `warn_if_action_type_dropped: <accreditor> / <action_type> dropped from
   <N> rows to 0.`
2. Open the accreditor's page and search for that action type manually.
3. If the action type is present in the page but absent in the parse, the
   classifier in `scripts/shared/accreditation_scrapers.R` stopped
   matching it — often because the accreditor changed the label wording
   (e.g. "On Probation" → "Probationary Status"). Patch the classifier
   and re-run.
4. If the action type is genuinely absent this cycle, the warning is a
   false alarm this week.

---

### `SCRAPER RETURNED 0 ROWS: <accreditor>`

**What it means.** `scripts/build_accreditation_actions.R` ran one of the
per-accreditor parsers (HLC, SACSCOC, NECHE, WSCUC, MSCHE) and got an empty
result frame. NWCCU is allow-listed because zero rows there is the normal
case (no qualifying institutions under action).

**In the weekly refresh, this now surfaces as a warning.** The workflow runs
`build_accreditation_actions.R` with `--allow-partial-accreditation` so the
run can keep staging review rows and preserve the last approved public
snapshot while a human fixes the scraper.

**Triage steps.**
1. Follow the same fix path as `warn_on_empty_parse` above: open the
   accreditor URL, confirm the page has actions listed, inspect the
   HTML, and compare against the parser function in
   `scripts/shared/accreditation_scrapers.R`.
2. Patch the parser and re-run the refresh workflow to confirm the warning
   clears.

---

## Re-running after a fix

After patching the parser and verifying locally:

1. Commit the parser change to `main`.
2. Go to Actions → **Refresh Source Data Weekly** → **Run workflow**.
3. The refresh should complete through the **Check for scraper drift
   warnings** step without finding any matches.

## Intentionally suppressing a known-good warning

If a warning is a persistent false alarm (e.g., an accreditor's page is
legitimately near-empty for an extended period), add a guard at the call
site in the R source — not in the CI grep. The CI gate is intentionally
a blunt instrument; nuanced suppression belongs in the producer, not the
consumer.

## See also

- `docs/RUNBOOK_WEEKLY_REFRESH.md` — step-by-step recovery for non-drift
  refresh failures.
- `scripts/shared/accreditation_scrapers.R` — scraper implementations and
  the three `warn_*` helpers.
