# Refresh Cycle

This document describes how the pipeline works — what each step produces, what consumes it, and which steps are safe to rerun.

---

## Data flow overview

```
IPEDS raw download
    └─► collect_ipeds_data.R
             └─► build_ipeds_dataset.R
                      ├─► ipeds_financial_health_dataset.csv  ← canonical IPEDS
                      └─► (also reads: EAP, EFFY, SFA, DRVF tables)

College Scorecard download
Grad PLUS XLS download
IPEDS DRVGR download
    └─► build_outcomes_join.R
             └─► data_pipelines/scorecard/tracker_outcomes_joined.csv  ← outcomes

Grant Witness API
    └─► build_grant_witness_join.R  (first pass: no filter)
             └─► grant_witness_grant_level_joined.csv
             └─► grant_witness_higher_ed_institution_summary.csv

    └─► build_grant_witness_usaspending_sensitivity.R
             └─► grant_witness_usaspending_risky_continuation_filter.csv  ← Proposal G / risky continuation

    └─► build_grant_witness_join.R  (second pass: with filter)
             └─► grant_witness_grant_level_joined.csv  (filtered)
             └─► grant_witness_higher_ed_institution_summary.csv  (filtered)

College Cuts (Supabase)
    └─► build_college_cuts_join.R
             └─► data_pipelines/college_cuts/
                      ├─► _cut_level_joined.csv
                      ├─► _institution_summary.csv
                      ├─► _financial_trends.csv       ← local-only review
                      └─► _unmatched_for_review.csv   ← local-only review

Accreditation actions (scrapers)
    └─► build_accreditation_actions.R
             └─► data_pipelines/accreditation/
                      ├─► _actions_joined.csv
                      ├─► _institution_summary.csv
                      ├─► _current_status.csv
                      ├─► _unmatched_for_review.csv   ← local-only review
                      └─► _source_coverage.csv       ← local-only review

Federal closure (Google Sheet)
    └─► import_closure_sheet.py
             └─► data/closure_status_by_unitid.json
             └─► data_pipelines/federal_closure/derived/closure_pipeline_summary.json

External JSON
    ├─► data/hcm2_by_unitid.json          ← build_hcm_level2.py
    └─► data/federal_composite_scores_by_unitid.json  ← build_federal_composite_scores.py

All above outputs
    └─► build_web_exports.R
             └─► data/
                      ├─► schools/{unitid}.json      ← one JSON per school
                      ├─► data/schools_index.json
                      ├─► data/college_cuts.json
                      ├─► data/college_cuts_index.json
                      ├─► data/accreditation.json
                      ├─► data/accreditation_index.json
                      ├─► data/research_funding.json
                      ├─► data/research_funding_index.json
                      ├─► data/downloads/full_dataset.csv
                      └─► data/metadata.json
```

---

## What the weekly CI does

The GitHub Actions workflow (`refresh-ipeds-site-data.yml`) runs this sequence every Monday at noon:

```
1. build_accreditation_actions.R
2. build_college_cuts_join.R
3. build_grant_witness_join.R --skip-usaspending-filter   ← unfiltered join
4. build_grant_witness_usaspending_sensitivity.R           ← sensitivity analysis
5. build_grant_witness_join.R                               ← filtered join
6. import_closure_sheet.py
7. build_web_exports.R
```

Steps 1–2 fetch live data (with local cache fallback) and are idempotent.
Step 3 must run before step 4 (it produces the input).
Step 4 is the only step that makes network calls to USAspending.gov.
Steps 5–7 are deterministic rebuilds from the joined datasets.

---

## Rerun safety

### Safe to rerun anytime

| Script | What it does | Safe? |
|---|---|---|
| `build_web_exports.R` | Reads all joined datasets, writes JSON. No network calls. | ✅ Always safe |
| `build_outcomes_join.R` | Reads local scorecard/DRVGR cache, writes joined outcomes CSV. | ✅ Always safe |
| `build_grant_witness_join.R` | Reads cached Grant Witness data, writes joins. | ✅ Always safe |
| `build_college_cuts_join.R` | Fetches from Supabase with cache fallback. Reruns will refresh from live data. | ✅ Safe; may add new cuts |

### Conditionally safe

| Script | What it does | Notes |
|---|---|---|
| `build_accreditation_actions.R` | Scrapes accreditor websites with cache. Reruns refresh from live sources. | ⚠️ Safe; may add new actions |
| `import_closure_sheet.py` | Imports from Google Sheet. Reruns overwrite the closure JSON. | ⚠️ Safe; overwrites `closure_status_by_unitid.json` |
| `build_grant_witness_join.R --skip-usaspending-filter` | Runs first pass before sensitivity analysis. | ⚠️ Must run before step 4 |
| `build_grant_witness_usaspending_sensitivity.R` | Makes live USAspending API calls. Reruns refresh from API. | ⚠️ Safe; makes real API requests |

### Destructive

| Script | What it does | Notes |
|---|---|---|
| `build_grant_witness_usaspending_sensitivity.R` | **Overwrites** `grant_witness_usaspending_risky_continuation_filter.csv`. This changes which grants are excluded from the research cuts page. | ⚠️ Run only when you intend to refresh the filter |
| `collect_ipeds_data.R` | Downloads fresh IPEDS data. May change which schools/variables are in the canonical dataset. | ⚠️ Run during the IPEDS refresh window (typically January–April) |

---

## Outputs that are local-only

These files are generated by pipeline scripts but are not committed to git. They are review/analyst artefacts and are ignored by `.gitignore`.

| Script | File | Purpose |
|---|---|---|
| `build_college_cuts_join.R` | `_financial_trends.csv` | 10-year time series for schools with cuts — analyst review |
| `build_college_cuts_join.R` | `_unmatched_for_review.csv` | Schools with cuts not matched to tracker — analyst review |
| `build_college_cuts_join.R` | `_workbook.xlsx` | Analyst workbook for cuts data |
| `build_accreditation_actions.R` | `_unmatched_for_review.csv` | Accreditation actions not matched to tracker — analyst review |
| `build_accreditation_actions.R` | `_source_coverage.csv` | Accreditior/action type coverage audit |
| `build_accreditation_actions.R` | `_workbook.xlsx` | Analyst workbook for accreditation data |
| `build_article_workbook.R` | `workbooks/*.xls` | Full article workbook (49 sheets) — local reporting only, not part of site production |
| `annual_refresh_and_publish.R` | `workbooks/*.xls` | Same workbook, generated via orchestrator |

The site production path does **not** include any workbook files.

---

## Skipping steps

- **`--skip-usaspending-filter`** in `build_grant_witness_join.R` skips the USAspending filter entirely, keeping all grants in the join. Use for a full grant-level export without the risky-continuation filter.
- **`--skip-publish`** in `annual_refresh_and_publish.R` skips Google Sheets publishing (Google Sheets publishing is no longer part of the default flow).
- **`--refresh=false`** in `build_accreditation_actions.R` uses only cached data without re-scraping accreditor sites.

---

## How to rebuild just one section

If you only need to refresh one section of the site (e.g., research cuts) without touching anything else:

```sh
# 1. Rebuild research joins (reads cached Grant Witness data)
Rscript --vanilla ./scripts/build_grant_witness_join.R --skip-usaspending-filter
Rscript --vanilla ./scripts/build_grant_witness_usaspending_sensitivity.R
Rscript --vanilla ./scripts/build_grant_witness_join.R

# 2. Rebuild web exports (reads all joins, writes all JSON)
Rscript --vanilla ./scripts/build_web_exports.R
```

This touches only the research pipeline and the JSON output. It does not re-scrape accreditation, re-fetch College Cuts, or re-download IPEDS.

To rebuild just one section, all preceding joins in the pipeline must already exist. `build_web_exports.R` will fail if any of its required input files are missing — it will tell you which script to run.
