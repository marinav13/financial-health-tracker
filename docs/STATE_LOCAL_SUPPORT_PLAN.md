# Plan: Switch publics' state-aid metric to "State & Local Support"

**Status:** Approved for implementation. Nothing is published yet, so the public-facing
metric can be replaced outright (no dual-display or migration shims). Keep the old
appropriations-only fields in the canonical dataset as a diagnostic.

## Why

Our current `state_funding` = the single IPEDS "state appropriations" line
(`state_appropriations_gasb`, mapped to F1B17 in `scripts/collect_ipeds_data.R`).
Comparing our state-level aggregates against SHEEO SHEF (FY24 report, Tables 3.1/3.2,
same FY2019-FY2024 window, constant dollars) gives correlation 0.55 with huge artifacts:

- **Colorado: repo +100.5% vs SHEF +28.1%.** CO funds institutions through College
  Opportunity Fund vouchers and fee-for-service contracts, which land in the state
  grants/contracts lines, not appropriations. Money moving between lines shows up
  as a phantom doubling.
- **Connecticut: repo -44.5% vs SHEF +36.7%.** CT pays CSCU fringe benefits centrally;
  that support never appears in campus IPEDS filings. NOT fixable from IPEDS - document
  in methodology, do not attempt to patch.
- Median absolute state-level gap: 7.6 points.

A composite "state & local support" (appropriations + state/local grants and
contracts, operating and nonoperating) fixes the Colorado class of artifact and
aligns conceptually with SHEF while staying institution-level.

## Decision rationale (recorded 2026-07-14)

Three options were compared for the public-facing metric on school.html (the
"state aid increased/decreased" block and the appropriations chart):

1. **State appropriations only (current, F1B11).** Purest policy signal, easy to
   explain, but blind to funding mechanism: states that deliver money as per-student
   stipends or fee-for-service contracts (Colorado) look like phantom collapses or
   explosions. Produces false alarms in exactly the states with unusual structures.
2. **State + local appropriations (F1B11 + F1B12).** Nearly cosmetic for this
   tracker's universe: local appropriations flow overwhelmingly to community
   colleges, which we don't track. Does NOT fix Colorado. Rejected.
3. **State + local appropriations, grants, and contracts (chosen).** Robust to
   reclassification (money moving between lines nets out), closest conceptual match
   to SHEEO SHEF, and has published precedent: the Urban Institute's IPEDS Finance
   User Guide constructs exactly this composite, and NCES Digest reports combined
   state+local measures.

Known trade-off of option 3, accepted: state student financial aid follows
students, so the composite dips when enrollment falls even if the legislature cut
nothing. The page shows the enrollment trend alongside, and the retained
appropriations-only fields let us distinguish a policy cut from an aid-follows-
students effect case by case. Put one sentence about this in the methodology page.

Dropping the state-aid display entirely was considered and rejected: state money is
the second-largest revenue source for most tracked publics and the main explanation
for staffing/program cuts; campus-level funding trends are also this site's most
differentiated data (SHEEO only publishes state-level numbers).

**Display label: "State & local funding."** Do not use "state & local aid" ("aid"
reads as student financial aid) or "appropriations" (no longer accurate). Internal
field names stay `state_local_support*`.

## Definition

For GASB publics (the overwhelming case):

```
state_local_support =
    state appropriations                    F1B11
  + local appropriations                    F1B12
  + state operating grants and contracts    F1B03
  + local operating grants and contracts    F1B04A  (see caveat below)
  + state nonoperating grants               F1B14
  + local nonoperating grants               F1B15
```

Codes verified against `ipeds/cache/downloads/extracted/dict_F2324_F1A/f2324_f1a.xlsx`
(Varlist sheet). EXCLUDE capital appropriations (F1B20) and capital grants and gifts
(F1B21) - SHEF excludes capital too.

**F1B04A caveat:** in earlier survey years the local and private operating
grants/contracts were a single combined line (F1B04, "Local/private operating grants
and contracts"). Use F1B04A (local only) where present; in years where only combined
F1B04 exists, EXCLUDE the local-operating component rather than pull private money
into the composite. Check each year's dictionary 2014-2024 and record availability
in a comment.

- All components zero-if-null EXCEPT: if state appropriations AND all grant lines are
  NA, the composite is NA (mirror the `government_funding` NA pattern at
  `ipeds_row_builders.R:218-222`).
- FASB publics (rare) and privates: fall back to the existing appropriations-only
  value (`state_approps_fasb` / F2D04, `state_appropriations_pfp` / F3D05). Privates'
  display is unaffected; this metric is only surfaced for publics.
- Do NOT include federal stimulus passthroughs separately; they already flow through
  the nonoperating grant lines where states booked them that way. Note this in
  methodology.

## Verified codes and two latent bugs to fix FIRST

The FY2324 F1A dictionary confirms the codes above, and in the process exposed two
wrong exact-code fallbacks in `scripts/collect_ipeds_data.R` (`exact_field_overrides`,
~line 473). Resolution order is title-pattern first, exact code only as fallback
(`resolve_year_fields`, ~line 205), so these have never fired - verified empirically:
state_appropriations series for U Alabama, U Michigan, and UT Austin are smooth and
plausible 2014-2024, consistent with F1B11. But they are landmines if a future
dictionary changes title text:

- `state_appropriations_gasb = "F1B17"` is WRONG. F1B17 = Investment income.
  Correct fallback: `"F1B11"`.
- `federal_operating_grants_contracts_gasb = "F1B04"` is WRONG. F1B04 =
  Local/private operating grants and contracts. Correct fallback: `"F1B02"`.

Also: the resolution audit manifest
(`ipeds/manifests/ipeds_financial_health_field_resolution_audit_2014_2024.csv`) is
EMPTY (header only), which is why these were never visible. Find out why
`resolution_audit` rows are not being written out and fix it, so any future
resolution change leaves a trail.

Make these three fixes the first, separate commit before any composite work.

When adding the five new GASB raw fields, resolve each BOTH by dictionary title
pattern AND exact code override, and re-verify titles in every year's dictionary
2014-2024 (titles and line splits change across years). Expected titles:

- "Local appropriations, education district taxes, and similar support"  (F1B12)
- "State operating grants and contracts"                                 (F1B03)
- "Local operating grants and contracts"                                 (F1B04A; combined F1B04 in early years - see caveat)
- "State nonoperating grants"                                            (F1B14)
- "Local nonoperating grants"                                            (F1B15)

## Implementation steps (in pipeline order)

### 0. `scripts/collect_ipeds_data.R` - bug fixes (FIRST, separate commit)
Fix the two wrong fallback codes (F1B17 -> F1B11, F1B04 -> F1B02) and repair the
empty resolution-audit manifest as described above. No behavior change expected;
confirm by diffing regenerated raw output for a handful of institutions.

### 1. `scripts/collect_ipeds_data.R`
Add the five new GASB raw fields: title patterns in the finance field list plus
entries in `exact_field_overrides` (verified codes). Follow the existing
`state_appropriations_gasb` naming convention: `local_appropriations_gasb`,
`state_operating_grants_contracts_gasb`, `local_operating_grants_contracts_gasb`,
`state_nonoperating_grants_gasb`, `local_nonoperating_grants_gasb`.

### 2. `scripts/shared/ipeds_row_builders.R`  (PROTECTED PATH - bash+Python write only)
In the finance builder (~line 171 where `state_funding` is picked):
- read the five new columns
- compute `state_local_support` per the definition above
- add `state_local_support_adjusted = inflate_to_base_year(state_local_support, context$year)`
  next to the existing `state_funding_adjusted` (~line 343)
- expose both in the returned row list (~lines 419-423), alongside a
  `state_local_support_pct_core_revenue = safe_divide(state_local_support, core_revenue)`

### 3. `scripts/shared/ipeds_enrichment_helpers.R`
Mirror the state_funding block (lines ~100-102) exactly:
```
state_local_support_pct_change_5yr_nominal  = safe_pct_change(state_local_support, lag5)
state_local_support_pct_change_5yr          = safe_pct_change(state_local_support_adjusted, lag5_adj)
state_local_support_pct_change_5yr_adjusted = state_local_support_pct_change_5yr
```
(The unsuffixed name is CPI-U-adjusted, matching every other 5yr field. Add the
lookup series for the two new lag lookups where `lookups$state` / `lookups$state_adj`
are built.)

### 4. `scripts/build_ipeds_canonical_dataset.R`  (PROTECTED PATH)
Add new columns to `canonical_columns` (~line 848) and `extended_columns`:
`state_local_support`, `state_local_support_adjusted`,
`state_local_support_pct_core_revenue`, `state_local_support_pct_change_5yr`,
`state_local_support_pct_change_5yr_adjusted`, `state_local_support_pct_change_5yr_nominal`.
Keep all existing `state_funding*` columns - they stay as the diagnostic.

### 5. `scripts/build_web_exports.R`  (PROTECTED PATH)
This is the display switch. For public institutions:
- summary block (~line 3419): export `state_local_support_pct_core_revenue` and
  `state_local_support_pct_change_5yr` as NEW keys. Do NOT reuse the old key names
  with new semantics - that invites silent drift. Keep exporting the old
  `state_funding_*` keys too (cheap, useful for the diagnostic gap).
- series block (~line 3450): add `state_local_support_adjusted` series next to
  `state_funding_adjusted`.
- sector benchmark (~lines 3683-3706): add
  `sector_median_state_local_support_pct_change_5yr`; keep the old benchmark.
- For privates, the new fields may be NA; exporters must tolerate that (existing
  `safe()` pattern).

### 6. JS display - `js/school.js` (PROTECTED PATH), possibly `js/app.js`
- `js/school.js` ~lines 1712, 1989-2094, 2240: switch the state-aid card, sentence,
  sentiment/trend, and chart series to the new keys. Wording: "state & local funding"
  instead of "state appropriations" (do NOT use "aid" - see Decision rationale). Chart label "State Funding" -> "State & Local Funding".
- Keep the `hasMeaningfulData` guard pattern; wire it to the new series.
- Grep all five page JS files for `state_funding` to catch stragglers; centralize any
  shared label/format logic per repo conventions (no near-duplicate helpers).

### 7. Methodology note
Add to the school-page methodology text (wherever state appropriations is currently
described): definition of the composite, why (funding-mechanism differences across
states), the Connecticut central-fringe caveat, and that dollars are CPI-U adjusted
to 2024. Do NOT add methodology text anywhere else without being asked (repo rule).

### 8. Workbooks (optional, low priority)
`scripts/build_article_workbook.R` and `scripts/shared/workbook_table_helpers.R`
reference `state_funding_pct_change_5yr` (~lines 116, 326-327, 371; 247, 1172-1173).
Add parallel state_local_support rows/flags; keep existing ones.

## Validation (required before merge)

1. **Composite >= appropriations-only** for essentially all GASB publics (grants are
   non-negative). Flag any institution where composite < appropriations (data bug).
2. **Colorado sanity check:** aggregate 5-yr change for CO publics should move from
   ~+100% to roughly +25-35%, near SHEF's +28.1%.
3. **Connecticut check:** gap vs SHEF persists (expected; central fringe). Document.
4. **Re-run the SHEF comparison:** sum `state_local_support_adjusted` by state,
   2019 vs 2024, compare against SHEF FY24 implied totals
   ((1+perFTE)*(1+FTE_enrollment)-1 from Tables 3.1/3.2). Expect correlation to rise
   materially above the current 0.55. Record before/after in the PR description.
5. Standard repo gates: `lintr` clean (it blocks smoke on CI), R smoke tests,
   `python3 scripts/check_file_integrity.py --staged --quiet` clean, pre-commit hook on.
6. Spot-check 3 school JSONs (one CO, one CT, one stable state like MT) - series
   present, summary keys present, no nulls where numbers expected.

## Environment cautions (from CLAUDE.md - binding)

- PowerShell: no `&&`; avoid `Rscript -e`. Ask the user to run R regeneration scripts
  and paste output back; they hang when run from the agent.
- Protected paths above: bash+Python `pathlib.write_text()` or /tmp + `cp` writes only;
  never the Edit/Write tool. Verify every write via `wc -c`, tail, null-byte count.
- Per-fix commits on a `claude-wip` branch; snapshot byte sizes before editing.

## Explicit non-goals

- Do not replicate SHEF exactly (central fringe, sector-allocated financial aid, and
  research/hospital/medical netting are not derivable from IPEDS).
- Do not remove or rename existing `state_funding*` fields.
- Do not change privates' display.
- Do not touch the accreditation, cuts, or research pipelines.
