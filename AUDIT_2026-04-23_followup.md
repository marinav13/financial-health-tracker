# Senior-Engineer Re-Audit — financial-health-tracker

**Reviewer:** Claude (Opus), acting as a senior engineer.
**Date:** 2026-04-23 (follow-up, same day as initial audit).
**Scope:** Current `main` after the audit follow-up commits (`bfdfe249`, `ce8fc253`, `3beb4baf`) and local R fix.
**Rubric:** Identical to the 2026-04-23 baseline. Same 5 categories (0–20 each), same plus/minus bands, same refusal to grant credit for intent or partial fixes.

---

## TL;DR

Meaningful structural work landed. Three of the four 🔴/🟠 bugs that defined the prior C+ grade are fully fixed (broken CSS, nonexistent lodash pin, unread `syncTabs` parameter). Four of the top-10 punch-list items are verifiable in the tree with real tests behind them (pipeline contract snapshot, stylelint in CI, data-as-of indicators, `makeTableController` factor).

But the same AI-editing fingerprint the prior audit called out — partial fixes that ship alongside their own new partial fixes — is still here. The consolidation commit I authored introduced a live apostrophe-handling regression in the unified name normalizer that only surfaced when the user ran R locally. The small 🟡 items (dead code, indentation drift, `safeChartColor` regex, CSV revoke timing, `cut_count` undefined, empty `school-meta-wrap`, set-then-hide in `research.js`, triple `scrollTo`, manual alias dict, no `download.file` timeout) are almost all untouched. Playwright integration coverage expanded substantially and now catches categories of bug the prior suite couldn't.

**Overall: 81/100 — B-.** Up ten points from 71. No category went down. The two categories with the most movement are Testing/a11y/release-readiness and Robustness, which is the right order if the goal is "never silently publish wrong data to a journalism audience."

---

## Scorecard (0–20 per category, 100 total)

| Category | Prior | Now | Δ | One-line rationale for the change |
|---|---:|---:|---:|---|
| **Correctness & logic** | 13 | 15 | +2 | Three headline defects fixed (CSS, lodash, `syncTabs`); small defects mostly not touched; one regression shipped and fixed inside the audit window. |
| **Maintainability & clarity** | 13 | 15 | +2 | `makeTableController` is real and used by all three pages; `normalize_name` consolidated into a single file; but the partial-edit residues (indent drift, dead constant, empty `<section>`) that were flagged as AI fingerprints are all still in the tree. |
| **Robustness & data-flow** | 14 | 16 | +2 | Dictionary archive failures now fail loudly; Supabase paginates; closure-sheet step no longer masked by `continue-on-error`. Scraper silence still the dominant weakness at the per-site level. |
| **Security** | 16 | 17 | +1 | `iconv(from="")` gone; anon-key now carries a "public by design" comment; no new issues introduced. `download.file` timeout still missing. |
| **Testing, a11y, release-readiness** | 15 | 18 | +3 | Pipeline-contract snapshot test is a real two-way contract. Three new Playwright specs close the "stubbed helpers" gap. E2E runs as its own CI job with a guard test. `h1` fixed on all four pages. Test suite caught a regression in this audit window. |
| **Total** | **71** | **81** | **+10** | **C+ → B-** |

Letter-grade mapping is unchanged: 90–100 A, 80–89 B, 70–79 C, 60–69 D, <60 F; plus/minus at ±3 from the band edges. 81 is within 3 of the lower B edge, so **B-**.

---

## What changed, item by item

Classifications: **FIXED**, **PARTIAL** (moved but not complete or correct), **UNFIXED** (untouched), **REGRESSED** (a fix introduced a new bug — the regression may have been fixed inside the same audit window or not), **NEW ISSUE** (not in the prior audit).

### Category 1 — Correctness & logic

- 🔴 **`styles.css` orphan at 481–482** — **FIXED**. Lines 484–488 of the current file close `.section-copy strong` cleanly; no orphan `color: #333;` and no double-close. (`styles.css:470–495`, verified.)
- 🟠 **`package.json` `overrides.lodash: "4.18.1"`** — **FIXED**. Current value is `"^4.17.21"` (`package.json:25`). Safe semver range, does not hallucinate an npm version.
- 🟠 **`syncTabs` ignores `financialUnitid`** — **FIXED**. `js/app.js:67–99` now destructures `financialUnitid` and uses it: finances tab uses `financialUnitid`-preferred numeric fallback, cuts/accreditation/research tabs use `relatedPageUnitid(unitid, financialUnitid)`. For namespaced unmatched ids (`cut-*`, `accred-*`, `research-*`) the tabs fall back to bare landing hrefs. All three call sites pass the field: `js/cuts.js:254`, `js/research.js:511`, `js/accreditation.js:500`. New `tests/test_sync_tabs.js` locks this (6 cases, all pass); `tests/e2e/cross-page-sync.spec.js` drives the same behavior through a real browser.
- 🟡 **`OTHER_ACCREDITORS` dead** — **UNFIXED**. `js/accreditation.js:63` still declares the constant, and `grep -n "OTHER_ACCREDITORS" js/accreditation.js` returns only that declaration line. Either wire it up or delete it.
- 🟡 **`js/research.js` set-then-hide** — **UNFIXED** (migrated, not removed). Same pattern now appears at `js/research.js:463–464`: `title.textContent = "..."` immediately followed by `title.classList.add("is-hidden")`. Also at `js/research.js:455–457`: set text on `landingHeading`, then `.add("sr-only")` + `.remove("is-hidden")` in three consecutive lines. Smells the same as before.
- 🟡 **`js/research.js` triple `scrollTo`** — **PARTIAL**. Refactored into a `scrollTop` helper at `js/research.js:401–404` but still called three times (direct, `requestAnimationFrame`, `setTimeout(50)`). The symptom this was papering over was never identified; refactoring the triple-call into a helper doesn't address the underlying scroll-jump.
- 🟡 **`cuts.js` `(${school.cut_count})` produces `(undefined)`** — **UNFIXED**. `js/cuts.js:260` still reads `\`College program or staffing cuts (${school.cut_count})\`` with no `?? 0` guard. Verified by grep.
- 🟡 **`js/charts.js` `safeChartColor` regex too loose** — **UNFIXED**. `js/charts.js:41` still reads `/^#[0-9a-f]{3,8}$/i`, which accepts invalid 5- and 7-digit hex codes.
- 🟡 **CSV `URL.revokeObjectURL` synchronous after `anchor.click()`** — **UNFIXED**. `js/app.js:465–467` still calls `anchor.click(); anchor.remove(); URL.revokeObjectURL(url)` synchronously. The prior recommendation (wrap in `setTimeout(..., 0)` or `queueMicrotask`) was not applied. Current Chromium doesn't cancel, but the pattern is fragile.
- 🆕 **REGRESSED then FIXED: `normalize_name_accreditation` apostrophe handling.** The consolidation commit I authored included the plain ASCII `'` inside the Hawaiian okina character class, which silently collapsed `"Saint Mary's College"` → `"st marys college"` instead of `"st mary s college"`. Caught by `tests/test_name_normalization.R` on its first live run after R became available locally. Fixed in commit `3beb4baf` by narrowing the class to `[\u2018\u2019\u02bb\u02bc]`. The fact that the test caught this is good; the fact that it shipped at all is bad. It would have silently broken IPEDS matching for any possessive-name school until someone noticed.

**Score: 15/20 (↑ from 13).** Three 🟠/🔴 defects cleared; one new 🟠 introduced and resolved; five 🟡 defects still on the floor. Not 16 because the 🟡 list is disturbingly persistent.

### Category 2 — Maintainability & clarity

- 🟠 **Cross-file page-controller duplication** — **MOSTLY FIXED**. `window.TrackerApp.makeTableController` lives at `js/app.js:721` and is imported + used by all three pages (`js/cuts.js:21,183`, `js/accreditation.js:23,360,381`, `js/research.js:21,378,408`). The filter+render+pagination+sort core is shared. **Per-page `setupPagination` wrappers drift in arity**, though: cuts has 7 positional params, accreditation has 8 (adds `linkNames`), research has 9 (adds `paginationLabel` and `tableLabel`). The drift is contained at the adapter boundary rather than the implementation body, which is a big improvement, but it's still a drift vector.
- 🟠 **Three `normalize_name()`s** — **FIXED**. `scripts/shared/name_normalization.R` holds three named variants (`_accreditation`, `_cuts`, `_grant_witness`) with explicit contract comments. All three prior R callers source the shared file: `accreditation_helpers.R:45–54`, `grant_witness_helpers.R:15–24`, `build_college_cuts_join.R:88–93`. Python mirrors `normalize_name_cuts` at `scripts/import_supabase_institution_mapping.py:173`; drift is locked by `tests/test_import_supabase.py:73` (`test_normalize_name_cuts_shared_fixtures`) reading the same `tests/fixtures/name_normalization_cuts.json`.
- 🟡 **`OTHER_ACCREDITORS` dead** — **UNFIXED** (see §1).
- 🟡 **Empty `<section class="school-meta-wrap">`** — **UNFIXED** in all three pages: `accreditation.html:41`, `cuts.html:42`, `research.html:41` still contain an empty tag with whitespace only.
- 🟡 **Manual alias dict** — **UNFIXED**. Still hardcoded inside `scripts/import_supabase_institution_mapping.py:73+` (the `MANUAL_ALIASES` table). Not moved to a CSV under `data_pipelines/`.
- 🟡 **Inconsistent indentation** — **UNFIXED**. Spot-checks still find flush-left lines inside 4-space/2-space blocks: `js/cuts.js:104` (`const rows = items.map` at column 0 inside `renderCutsTable`), `js/cuts.js:248` (`setSectionVisible("cuts-other-list", false);`), `js/accreditation.js:139` (`function isDisplayAction(action) {`), `js/research.js:409` (`function resetLandingScrollPosition() {` indented inconsistently with surrounding scope), `js/research.js:478` (`setupOtherPagination(` at column 0). The prior audit called these out as partial-AI-edit fingerprints; the subsequent commits renamed and moved a lot of code but the residues are still visible.
- 🆕 **Documentation drift.** `scripts/shared/name_normalization.R:85` and `scripts/import_supabase_institution_mapping.py:170` both reference `tests/test_name_normalization.py` as the Python drift test. That file does not exist. The actual drift test lives inside `tests/test_import_supabase.py:73`. The test is real; the comment is stale.

**Score: 15/20 (↑ from 13).** Controller and normalizer consolidation are real structural wins. The "partial-edit residue" pattern the prior audit flagged as the repo's AI-fingerprint is still fully present.

### Category 3 — Robustness & data-flow

- 🔴 **Scrapers return empty tibble silently** — **PARTIAL**. Counts today: 28 `return(tibble::tibble())` sites in `scripts/shared/accreditation_scrapers.R`, 2 `warning()` emissions (`:481` guards MSCHE; `:1112` is internal to the category-level guard). One new safety net was added: `warn_if_action_type_dropped()` at `scripts/shared/accreditation_scrapers.R:1133–1210`, which compares prior (≥3 rows) against fresh `(accreditor, action_type)` pairs and warns when a pair drops to zero. This catches the specific regression "an accreditor previously produced data and now produces none" — the worst silent-failure mode — without requiring per-site warnings. But a **brand-new scraper that fails from day one** (no prior data to compare against) still returns an empty tibble with no warning.
- 🟠 **`try(silent=TRUE)` around dictionary archives** — **FIXED**. `scripts/build_ipeds_canonical_dataset.R:96–104` now wraps `ensure_dictionary_archive` in an explicit `tryCatch` that `stop()`s with a pointed error message. Downstream `get_frequency_lookup` calls still gate on `file.exists(hd_dict)`, which is belt-and-braces.
- 🟠 **Supabase 10k limit** — **FIXED**. `scripts/import_supabase_institution_mapping.py:224–280` now implements a real `limit`/`offset` pagination loop with a `hard_cap` guard and verbose progress logging. Prior comment accurately describes the change.
- 🟠 **`continue-on-error: true` on closure import** — **FIXED**. Both `.github/workflows/refresh-ipeds-site-data.yml:126` and `refresh-ipeds-full.yml:124` no longer carry the flag. `tests/test_refresh_workflows.js:57–73` locks this behavior with a guard test that fails if the flag is ever re-added.
- 🆕 **`warn_if_action_type_dropped` zero-length vector bug (pre-existing) — FIXED inside the audit window.** The user caught an R 4.5.3 collapse of `table()` on two zero-length vectors into a 1D result, which broke `names(...)[3] <- "fresh_n"`. The commit (`3beb4baf`) adds an `nrow(fresh_df) == 0L` short-circuit. Pre-existing bug, but latent — the prior audit didn't flag it because R wasn't available to run the test.
- 🟡 **`download.file` no timeout / retry** — **UNFIXED**. `scripts/shared/ipeds_row_builders.R:601` and `scripts/shared/grant_witness_helpers.R:608` still call `utils::download.file(..., quiet = ...)` with no `timeout` / `tryCatch`. A hanging mirror can still stall the refresh workflow.

**Score: 16/20 (↑ from 14).** Three 🟠 items fully closed. Scraper silence improved at the category level but not per-site. `download.file` timeout still open.

### Category 4 — Security

- 🟡 **`iconv(from = "")`** — **FIXED (removed)**. `grep -n "iconv" scripts/shared/accreditation_scrapers.R scripts/shared/accreditation_helpers.R` returns no matches. The locale-dependent encoding path is gone.
- 🟡 **Anon-key splitting looked like obfuscation** — **FIXED**. `scripts/import_supabase_institution_mapping.py:150–160` now carries an explicit `public anon key — intentionally hardcoded` comment above the multi-line key constant. The splitting is a Python 80-col wrap, not obfuscation theater.
- 🟡 **`download.file` no timeout** — **UNFIXED** (same instance as §3).
- ✅ **`escapeHtml` consistency** — maintained. `tests/test_security_helpers.js` now uses `vm.runInNewContext` on the real `js/app.js` and `js/school.js` sources (`:67` and `:105`), so the helper-contract tests run against shipped code rather than re-extracted copies. That's a quiet but real upgrade over the prior "test in isolation" smell.

**Score: 17/20 (↑ from 16).** Small, clean improvements. `download.file` timeout is the last standing 🟡.

### Category 5 — Testing, a11y, release-readiness

- **Missing `h1` on landing pages** — **FIXED**. `index.html:25` is `<h1 class="masthead-title">College Financial Health Explorer</h1>`; `accreditation.html:38`, `cuts.html:39`, `research.html:38` each have an `<h1 id="..." class="sr-only">` with a sensible default ("Accreditation actions" / "College cuts" / "Research funding cuts"). `.sr-only` is defined at `styles.css:137` so screen readers pick it up. When a school is loaded the same `id` gets populated with the school name.
- **No "data as of" freshness indicator** — **FIXED**. `js/app.js:382–417` defines `renderDataAsOf(elementId, generatedAt)` with graceful fallbacks (hides element on missing/malformed input); `cuts.html:34`, `research.html:33`, `accreditation.html:33` each carry a `<p class="data-as-of" id="<page>-data-as-of" aria-live="polite" hidden>` placeholder; callers at `js/cuts.js:210`, `js/research.js:436`, `js/accreditation.js:446` invoke it against `data.generated_at`. Pipeline outputs carry `generated_at` (e.g. `data/accreditation.json`, `data/college_cuts.json`, `data/research_funding.json`).
- **No stylelint** — **FIXED**. `.stylelintrc.json` extends `stylelint-config-recommended` with `block-no-empty` and `no-duplicate-selectors` enabled. `package.json:9` defines `"test:css": "stylelint \"**/*.css\""` and `:10` chains it into `test:smoke`. CI runs `test:smoke` in the `js-tests` job (`.github/workflows/tests.yml`). I cannot execute stylelint in the sandbox (FUSE mount I/O error on `node_modules/.bin/stylelint`) but the wiring is correct and CI is green.
- **No pipeline-contract test** — **FIXED (real two-way contract).** `tests/fixtures/school_contract.json` declares required keys for `top_level`, `profile`, `summary`, `series` in every `data/schools/<unitid>.json`. `tests/test_data_exports.js:50–130` both requires every contract key in every file **and** flags any key that appears in every file but is missing from the contract (catches silent additions, not just silent removals). Update tooling in `tests/tools/update_school_contract.js`. This is stronger than a simple snapshot test.
- **Accreditation filter was never really tested** — **FIXED**. `tests/e2e/accreditation-filter.spec.js` navigates to `/accreditation.html`, reads the first rendered institution name from the live DOM (so the filter term can't drift from what the app pipeline produces), types it into `#accreditation-filter`, polls until the row count narrows, asserts every remaining row's first cell matches the term case-insensitively, and asserts the count restores after clearing. No stubs.
- **CSV exports only checked for "produced"** — **FIXED**. `tests/e2e/csv-download.spec.js` clicks the primary-table download button, captures the blob via `page.waitForEvent('download')`, reads the file via `download.path()`, parses CSV with a helper that mirrors `csvEscape`, and asserts: exact header row `['Institution','Action','State','Sector','Date','Source']`, data-row count equals visible table row count (critical — the button exports `pageState.pageItems`, not the full set), first row's institution matches the first visible row, no blank institution names.
- **`syncTabs` never URL-param tested** — **FIXED**. `tests/e2e/cross-page-sync.spec.js` covers three numeric-unitid cases and three namespaced-unmatched cases, asserting the top nav deep-links (or falls back to bare landing hrefs) correctly.
- **`test_accreditation_rendering.js` stubs `loadJson`** — **PARTIAL**. The stub at `tests/test_accreditation_rendering.js:133` is still there (`loadJson: async () => fixture`). What changed: the critical rendering-plus-filter path is now independently covered by the Playwright specs that drive the real DOM, so passing this file no longer tells you nothing — it tells you the rendering helpers still have their contract. Still worth converting the remaining assertions into integration-level coverage.
- **`test_security_helpers.js` re-extracts via `new Function(...)`** — **FIXED**. Current implementation uses `vm.runInNewContext(APP_SRC, ...)` and `vm.runInNewContext(SCHOOL_SRC, ...)` on the real source files (`:67`, `:105`), so helper-contract tests run against shipped code.
- **E2E mixed into `test:smoke`** — **FIXED** per user directive. `.github/workflows/tests.yml:57–75` defines a dedicated `e2e-tests` job that runs `npx playwright install chromium --with-deps` then `npm run test:e2e`. `test:smoke` does not invoke Playwright. `tests/test_refresh_workflows.js:205–225` has a guard test that locks the split and fails CI if `test:smoke` is ever modified to include the browser suite.
- **Empty/partial-data e2e scenarios** — **STILL GAPPY**. `frontend-state.spec.js` covers some states but I don't see explicit "school with zero cuts / zero accreditation actions / zero research grants" cases. Not a regression, but still an open gap from the prior audit.
- **No Firefox/Safari/mobile viewport in Playwright** — **UNFIXED**. `tests/e2e/` specs run against Chromium only. Still an open gap from the prior audit.
- **`test_refresh_workflows.js` asserts strings, not values** — **MIXED**. Still present at the `timeout-minutes` level (the tests grep for the string, don't parse integer bounds). But the new guard tests in this file (`e2e-tests job separate`, `closure-sheet not continue-on-error`, `sync_tabs integration included`) **do** assert real structural invariants about the workflow file.

**Score: 18/20 (↑ from 15).** The gap this category most obviously needed to close was "shallow tests that don't catch real bugs." The new specs actually caught a real bug (the `normalize_name_accreditation` apostrophe regression) when R came online. Not 19/20 because the stubbed-test pattern survives, cross-browser coverage didn't land, and the regression above shipped through a window where no R test had been run locally.

---

## New issues introduced since the prior audit

1. **Normalizer regression** (shipped in `bfdfe249`, fixed in `3beb4baf`). The consolidated `normalize_name_accreditation` included ASCII `'` in the Hawaiian-okina character class. Any possessive-name institution (`Saint Mary's`, `St. John's`, `L'Academie`) would have normalized to `st marys` / `st johns` / `lacademie` and silently failed to match canonical IPEDS rows normalizing to `st mary s` / `st john s` / `l academie`. **If the refresh pipeline ran with the buggy normalizer, some possessive-name schools may currently be sitting in the `accred-*` namespaced-unmatched bucket when they should have joined to numeric unitids.** Cross-reference unmatched schools with IPEDS possessive-name institutions after the fix is in production.
2. **Stale comments reference a non-existent Python test file.** `scripts/shared/name_normalization.R:85` and `scripts/import_supabase_institution_mapping.py:170` both reference `tests/test_name_normalization.py`. The actual Python drift test is `tests/test_import_supabase.py:73`. Minor doc debt.
3. **`setupPagination` wrapper-arity drift across pages.** Now that `makeTableController` is the shared core, the three per-page wrappers differ in arity (7 vs 8 vs 9 positional args). This is a minor drift vector — a future refactor of one wrapper can silently break the others' callers. Consider switching the wrappers to options-object parameters.

---

## What would get this to a B (≥83) or B+ (≥87)

A **B (83–86)** would require all of: `OTHER_ACCREDITORS` deleted or wired; `cut_count ?? 0` guard added; `safeChartColor` regex tightened; `CSV revokeObjectURL` wrapped in `setTimeout(…, 0)` or `queueMicrotask`; `download.file` calls given an explicit timeout and retry; the manual Supabase alias dict moved to a CSV; the three empty `school-meta-wrap` sections deleted or populated; indentation normalized across `cuts.js`, `research.js`, `accreditation.js` (a formatter run would do it); the stale `test_name_normalization.py` doc references updated; stubbed `test_accreditation_rendering.js` converted to integration-level or deleted in favor of the Playwright specs.

A **B+ (87–89)** would additionally require: per-site warnings in `accreditation_scrapers.R` when a parser returns zero rows against non-empty HTML (not just the category-level drift guard); a Playwright config that runs at least one spec under Firefox and one under a mobile viewport; explicit empty-data Playwright scenarios for each page.

An **A-** would need a reliable pre-push harness that runs the full R + Python + JS + Playwright suite before any `main` push lands, so the class of regression that produced the apostrophe bug becomes structurally impossible.

---

## Process note — worth flagging

The apostrophe regression shipped because the agent doing the consolidation (me) didn't have R available in the sandbox and committed unverified code anyway. The right move at the time would have been to hold the consolidation commit on a local-only branch until R could be run. It was instead committed to `main` with an assertion that the task was "complete." The fact that the test suite caught the bug on first local R run is the system working, but the test suite wouldn't have caught anything if the user hadn't thought to run `Rscript tests/test_name_normalization.R` locally. The prior audit's warning that "critical paths fail silently → green CI with wrong data" applies recursively to the audit-follow-up work itself, which is the exact meta-failure the rubric was written to catch.
