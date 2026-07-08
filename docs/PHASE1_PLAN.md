# Phase 1 Implementation Plan — Tranches B, C, D

**Date:** 2026-07-07
**Prerequisite reading:** `docs/PHASE0_PIPELINE_AUDIT.md` (the diagnosis this
plan implements; section references like §3.1 point there).
**Status (final, 2026-07-08): Phase 1 is fully implemented and merged.**
Tranche A (PR #6) + HLC gate hotfix (`d57bff8d2`); Tranche B (PR #7,
plus `22ef1b185` aligning the full-refresh workflow into the umbrella);
C4 (PR #8 + PR #10 — #10 landed the workflow half that #8's edit-script
crash omitted); C2 (PR #9); C1 + C3 (PR #11, including the
tombstone-flip interaction guard); Tranche D (PR #12, merged
`24d20d14e`). The tranche sections below are retained as the design
record; the sole remaining step is the first supervised live run of the
weekly refresh, which exercises the new machinery end to end.

This document is written to be handed to an agent with no prior context.
Read the whole thing (especially "Operating rules") before touching code.
All design questions that required editor answers were resolved on
2026-07-07; the decisions are inlined below marked **[DECIDED]**. No
open design questions remain — but per-tranche sign-off to *start* each
tranche is still required.

---

## Operating rules (binding, not advisory)

These were negotiated with the editor during Tranche A and are standing
policy for all remaining tranches:

1. **Nothing is pushed, no workflow file goes live, and no CI run is
   triggered without the editor's explicit per-action approval.** Ask
   first, act second — never act-then-disclose. This includes `git push`,
   `gh pr merge`, `gh workflow run`, and anything that writes to the
   Google Sheet.
2. **Work on a `claude-wip` branch off `main`** (recreate it; it was
   deleted after PR #6 merged). Per-fix commits, not mega-commits.
   Deliver via pushed branch + PR — the editor prefers PR review over
   direct merges to main.
3. **No AI-attribution lines** in commit messages or PR bodies (no
   Co-Authored-By trailers, no "Generated with Claude Code" footers).
4. **Follow CLAUDE.md's protected-path write protocol** for every file
   listed there (most files this plan touches are protected). On this
   Windows host two extra pitfalls are confirmed: bash heredocs halve
   backslashes (never pass R regex content through a heredoc — write a
   real `.py` edit script instead), and Python's `write_text` converts
   LF to CRLF (write bytes: `open(f,'wb').write(text.encode('utf-8'))`,
   then verify `d.count(b'\r\n') == 0`).
5. **Verification floor for every commit:**
   `Rscript --vanilla tests/run_shared_helper_smoke_tests.R` (all green;
   422 tests as of Tranche A), pre-commit hook passes (parse + trailing
   newline + null bytes), `python scripts/check_file_integrity.py --quiet`
   unchanged vs session start. lintr runs in CI (`tests.yml`, `R tests`
   job) — it is not installed locally; do not install packages into the
   editor's R library without asking.
6. **R scripts that need live Google Sheets access are run by the
   editor**, not the agent (auth JSON lives outside the repo). Provide
   exact PowerShell commands (quote URLs — PowerShell reserves `<`/`>`;
   pass `--auth-json`) and ask the editor to paste output back.
7. **The editor decides editorial policy.** If a change would alter what
   appears on the public site (beyond fixing a defect), stop and ask.

Key files (all protected paths):

| File | Role |
|---|---|
| `scripts/shared/editorial_review_helpers_impl.R` | both review stacks: staging, pull merge, gates, droppers, guards |
| `scripts/pull_accreditation_overrides.R` | flat pull script (everything in `main()`) |
| `scripts/pull_college_cuts_overrides.R` | wrapped pull script (`main()` → `pull_college_cuts_overrides()`) |
| `scripts/sync_review_sheet_appends.R` | post-commit Sheet appends (Tranche A; both pipelines) |
| `scripts/build_web_exports.R` | export build + review gates (`--apply-only-review-gate`) |
| `scripts/build_dapip_vs_scraper_audit.R` | DAPIP↔scraper reconciliation + code-coverage block |
| `scripts/build_dapip_accreditation_actions.R` | DAPIP download → classified action rows |
| `.github/workflows/refresh-ipeds-site-data.yml` | weekly refresh (cron `0 0 * * 1` + dispatch) |
| `.github/workflows/publish-editorial-overrides.yml` | manual publish (dispatch only; modes: publish / teachout-audit / teachout-cleanup) |
| `tests/test_editorial_review_helpers.R` | unit tests for the review stacks (`run_test` harness, see `tests/test_support.R`) |

---

## Tranche B — shared push script + concurrency umbrella (audit §7 item 3, §2b)

**Problem.** Refresh and publish have divergent push implementations.
Refresh (`Commit and push updated data` step): stash → 3-attempt retry →
fetch/rebase → on conflict, path-scoped `checkout --theirs` for data
paths → hard abort on non-data conflicts. Publish (`Commit and push
editorial publish` step): stash → single fetch/rebase/push, **no retry,
no conflict handling**. The two workflows also use different concurrency
groups (`refresh-source-data-weekly` vs `publish-editorial-overrides`),
so they can run simultaneously and race each other's pushes to main.
The race has never fired (verified empirically in the audit — closest
gap 26 s, one run cancelled) but nothing prevents it, and publish runs
cluster around refresh on the same evenings. Additionally the two steps
maintain divergent `git add` lists, which is how the §3.4 blended-commit
hazard and the §2b `cannot rebase: unstaged changes` failures arose.

**B1 — one shared push script.**

- New `scripts/ci/commit_and_push.sh`, extracted from the refresh
  workflow's push step (the more complete implementation — keep its
  stash, retry loop, rebase, path-scoped conflict resolution, and
  non-data abort exactly; this is a refactor, not a redesign).
- Parameterize: commit message, list of paths to stage (repeated `--add`
  args or a single file-list arg), and the conflict-resolution path list
  (defaults to the staged-path list — today the two lists in the refresh
  step are the same set, maintained twice ~40 lines apart).
- Both workflows call it. Publish thereby gains retry + conflict
  handling it has never had.
- Keep the `[skip ci]` commit-message convention both workflows use.
- The quarantine-CSV conditional add from Tranche A
  (`data_pipelines/college_cuts/review_quarantine.csv`, staged only if
  present) must survive the extraction — make "stage if exists" a
  supported input, not a special case bolted on after.
- Publish's teachout-cleanup mode uses a *different* add list + commit
  message than publish mode (see the `if/else` around
  `commit_message=` in the publish workflow). The script must support
  both call sites; do not collapse them into one list.

**B2 — concurrency umbrella.**

- Both workflows join one shared concurrency group (e.g.
  `group: site-data-writes`, `cancel-in-progress: false`) so refresh and
  publish serialize instead of racing pushes.
- **Accepted caveat (already signed off in principle, restate in the
  PR):** GitHub keeps only one *queued* run per group — a publish queued
  behind a long refresh is superseded if a newer publish is dispatched.
  Harmless here (publish is idempotent; re-dispatch), but say it out
  loud in the PR body.
- **[DECIDED 2026-07-07]** `refresh-ipeds-full.yml` (the occasional full
  IPEDS rebuild, which also pushes to main) JOINS the same umbrella. All
  three main-pushing workflows serialize under one group. Accepted cost:
  a weekly refresh or publish dispatched mid-rebuild queues behind it.

**Verification.**

- `bash -n scripts/ci/commit_and_push.sh` + shellcheck if available.
- Dry-run mode in the script (`--dry-run` prints the git commands
  without executing) so it can be exercised in CI logs safely.
- The real proof is one editor-triggered refresh dispatch + one publish
  dispatch after merge, confirming both push steps behave identically to
  before (same commits, same messages, same staged paths).

**Risk: low.** Behavior-preserving extraction for refresh; strict
improvement for publish. The failure mode to guard in review is a path
staged by the old inline step but missed by the parameterized call.
Diff the effective `git add` set before/after, path by path.

---

## Tranche C — hardening (audit §7 items 4–7)

Four independent items. They can ship as separate PRs; suggested order
below reflects value and how much design is already settled. **Each item
still needs its own editor go** — the designs below were presented but
not individually approved.

### C1 — absorption-layer fail-closed (§7 item 4; §1a/§1d)

**Problem.** Two live layers absorb accreditation candidate/override ID
drift: stage-time cross-source duplicate suppression
(`find_cross_source_duplicate_id`, called from
`stage_accreditation_editorial_overrides`) and gate-time
canonicalization (`canonicalize_accreditation_review_gate_action_ids`).
Both use the same matcher: unitid + accreditor + ±30-day window +
compatible type. Audit §1d proved two precision exposures empirically:
(A) the bidirectional `notice↔monitoring` pair can swallow a formal
Notice sanction issued within 30 days of a routine monitoring row;
(B) same-type distinct actions within 30 days are suppressed regardless
of label text. Worse, suppression **folds the new candidate's text into
the matched row's `source_*` fields** (see the field-fold loop in
`stage_accreditation_editorial_overrides`) — if that row is approved
and has no `override_*` statement, the *published* text silently changes
without re-review.

**Constraint discovered in §1d:** the `notice↔monitoring` pair is
load-bearing — 3 of the 5 absorbed Jun-8 sample IDs are SACSCOC notice
rows absorbed by it today. It cannot be deleted; it must fail closed.

**Changes:**

1. **Label-similarity requirement before folding.** When the matcher
   hits, compare normalized labels (existing
   `normalize_review_identity_text()` is the right normalizer). If
   similarity is below a threshold, do NOT suppress — stage the
   candidate as a new row instead. Start with a conservative
   token-overlap metric; validate the threshold against the §1d
   scenario table (the audit's five test scenarios are already in
   `tests/test_editorial_review_helpers.R` under "Cross-source duplicate
   suppression" — extend, don't replace).
2. **One-directionalize `notice↔monitoring`:** a new `notice` candidate
   may match an existing `monitoring` row (routine paperwork absorbed
   by the sanction) but a new `monitoring` candidate must NOT match an
   existing `notice` row in a way that swallows a formal sanction.
   Check the current `type_match` expression in
   `find_cross_source_duplicate_id` — both directions are currently
   allowed.
3. **Never fold text into an approved row silently. [DECIDED
   2026-07-07: option (b)]** — when the matched override row is
   `approved` and the incoming text differs beyond the similarity
   threshold, fold AND flip `review_status` to `unreviewed` so the row
   re-enters the review queue.
   **Comparison contract (explicit, editor-verified):** the similarity
   comparison is raw-vs-raw — incoming candidate `action_label_raw`
   against the row's stored `source_action_label_raw` (the previous raw
   scrape). Human-written `override_*` / edited text is NEVER an input
   to the comparison; comparing against edited text would flip every
   hand-edited approved row on its next matching refresh. Note this
   flip can only fire through the cross-source matcher (different
   action_id within the ±30-day window): a same-ID re-scrape cannot
   present differing raw text because action_id hashes the raw label,
   and the cuts pipeline has no cross-source matcher at all (cut_id is
   a stable upstream UUID). Verified empirically 2026-07-07: a full
   re-stage of the committed cuts candidates against the committed
   overrides changes zero review_status values and zero columns on the
   UC Davis row.
4. **Log every suppression into the drift report.** Suppressions
   currently print `Suppressing cross-source duplicate: ...` messages;
   ensure the message reaches `refresh-logs/combined.log` (it does — the
   staging step tees) and add the pattern to the drift-report grep in
   the refresh workflow so weekly suppressions surface as annotations.
5. **Document both layers in `docs/OPERATIONS_MANUAL.md`** as
   load-bearing (the audit calls this out; the manual currently doesn't
   mention them).

**Verification:** the §1d scenario table becomes unit tests: escalation
survives, monitoring→notice no longer swallowed, same-type-distinct
respects the similarity gate, teach-out carve-out still works, approved
rows never silently change text. Then a full local
`stage_accreditation_editorial_overrides` run against committed
candidates/overrides confirming zero unexpected suppression-set changes.

**Risk: medium.** This changes which rows get staged. Wrong thresholds
create duplicate rows (annoying but visible and reviewable) or block
absorption (gate failures — visible). Prefer erring toward duplicates.

### C2 — tombstones instead of deletion (§7 item 5; §1b)

**Problem.** `filter_accreditation_overrides_for_tracker_scope()` and
`filter_college_cuts_overrides_for_tracker_scope()` silently DROP
override rows whose effective unitid leaves the tracker index; teach-out
cleanup deletes rows outright. A dropped row that still exists in the
Sheet becomes a strict-pull-guard orphan (§3.1 class) or, post-Tranche-A,
a quarantine entry. Deletion also destroys review history.

**Changes:**

1. Add an `inactive` / `inactive_reason` column pair to both override
   CSV schemas (`ACCREDITATION_EDITORIAL_OVERRIDE_COLUMNS`,
   `COLLEGE_CUTS_EDITORIAL_OVERRIDE_COLUMNS` and their `empty_*` /
   `coerce_*` constructors). Default empty = active.
2. Scope filters and teach-out cleanup SET the tombstone instead of
   dropping the row. Reasons: `out_of_tracker_scope`,
   `teachout_cleanup`, `upstream_removed`.
3. Every consumer that reads overrides must treat tombstoned rows as
   absent for publication but present for identity: the export apply
   functions skip them; the strict pull guard counts them as "known"
   (a Sheet row matching a tombstoned CSV row is NOT an orphan); staging
   dedup still sees them (so a tombstoned action does not get re-staged
   as new); the review-sheet append diff excludes them.
4. Schema migration: committed CSVs gain the columns on first write.
   The Sheet schema does NOT change (tombstones are a CSV-side concept;
   the sheet-append filter simply stops offering tombstoned rows).
   Verify `coerce_*` handles old CSVs without the columns (backfill
   empty) — there are existing patterns for exactly this in the coerce
   functions.

**Verification:** unit tests for each consumer behavior above; a full
export build (`--only cuts,accreditation`) diffing `data/` outputs
before/after — must be byte-identical while no row is tombstoned; then a
synthetic tombstone test confirming the row vanishes from exports but
does not trip the pull guard.

**Risk: medium-high** (touches every consumer of the override CSVs).
Ship alone, not bundled. The audit's teach-out machinery
(`audit_accreditation_teachout_rows.R`, `cleanup_accreditation_teachout_rows.R`,
publish's teachout modes) must be checked as consumers too.

### C3 — candidate-generation determinism (§7 item 6; §3.10) — reduced priority

**Problem (proven, currently harmless).** Candidate generation and
export assembly read a gitignored DAPIP PDF text cache
(`.select_action_summary_source()` in
`scripts/shared/export_helpers.R` prefers cached PDF text for SACSCOC
rows *when the file exists on disk*). The same commit produced a
656-row candidates file on the cache-warm runner and 657 locally
(cache-cold), proven in both directions (audit §3.10, runs 28889590630
etc.). Authoritative cache-present stranded count is **0** — no action
is currently harmed — but publish runners have no accreditation cache,
so publish-side rebuilds run in the divergent environment by design.

**Fix direction [DECIDED 2026-07-07]:** persist the text-derived
classification into `dapip_action_rows_filtered.csv` at DAPIP-build
time (`build_dapip_accreditation_actions.R`), so every downstream
consumer reads the same committed value and the PDF-text cache is
consulted exactly once, in one place. (The alternative — classifying
from committed fields only — was considered and rejected to preserve
PDF-text classification quality.)

Also reconcile the candidates builder with export assembly so no
`*_keep` audit row can be present in the export but absent from
candidates (the §3.10 "stranded between layers" class).

**Verification:** regenerate candidates twice — once with the DAPIP
cache present, once with it renamed away — and diff: must be identical
after the fix. The audit's gate-diagnostic recipe (PR #5's approach: a
runner-side diagnostic with a hard-fail cache-presence guard) is the CI
version of the same check.

**Risk: low-medium.** Changing where classification happens can change
individual rows' classifications; diff `accreditation_review_candidates.csv`
and the public exports before/after and account for every changed row.

### C4 — alerting + escalation (§7 item 7; §4)

**Problem.** Weekly refresh runs with `--allow-partial-accreditation`,
so scraper failures degrade to log lines; the drift-report step greps
`refresh-logs/combined.log` and emits `::warning::` annotations on a
GREEN run — nobody is notified. Guard-pattern coverage is
accreditor-scrapers-only: DAPIP 0-row weeks, cuts-API
HTTP-200-with-0-rows, and Grant Witness have no grep patterns at all.

**Constraints (already decided in the audit):** do NOT remove
`--allow-partial-accreditation` (refresh must complete despite backlog —
acceptance criterion #1); exit code stays 0 on drift; escalation channel
is a GitHub Issue via the built-in `GITHUB_TOKEN` (no Slack/email
secrets exist in this repo, and Issues email watchers for free).

**Changes:**

1. **Auto-file/update a tracked GitHub Issue on drift.** When the
   drift-report step finds matches, create (or comment on an existing
   open) issue labeled `pipeline-drift` summarizing the matched lines +
   run URL. `permissions: issues: write` must be added to the refresh
   workflow's permissions block. Idempotency: search for an open issue
   with the label first; comment, don't duplicate.
2. **Fail-after-N-consecutive-weeks state.** A small committed state
   file (e.g. `data_pipelines/drift_state.json`: per-source consecutive
   drift-week counts) updated by the drift step and staged by the push
   script. When a source crosses N (propose N=3), the step exits 1 —
   converting chronic ignored drift into a red run. Reset on a clean
   week.
3. **Extend coverage:** add grep patterns (or better, have the sources
   write structured markers) for DAPIP empty-download fallbacks
   (`empty_*()` paths in the DAPIP build), cuts-API zero-row pagination
   end, and Grant Witness zero-row results. The audit flagged Grant
   Witness as "not audited this pass" — audit its failure surface first,
   then guard it.
4. **Split the gate's "ignoring N" message** (both pipelines) into
   by-design withholding (IDs present in current committed candidates =
   new-this-week awaiting review) vs anomalies (everything else), with
   sample IDs for the anomaly bucket only. Tranche A already added
   sample IDs + a >20 warning; this item adds the classification.
   Requires passing the committed-candidate ID set into
   `apply_*_editorial_overrides` or precomputing the split at the call
   site in `build_web_exports.R`.

**Verification:** unit-test the split logic; for the Issue step, test
with a throwaway label on a dispatch run the editor triggers; for the
state file, unit-test the counter transitions (drift/clean/missing
file).

**Risk: low.** Additive observability. The one behavior change is the
fail-after-N red run — call it out in the PR.

### Editorial-policy decision folded into C3 scope (from §3.10)

**[DECIDED 2026-07-07]** Merger/absorption approvals (the VWU/Sentara
class — DAPIP `dapip_backed_keep` rows currently classified into the
monitoring/procedural family by PDF text and appearing nowhere) are to
be **routed to the review queue**: classify them as substantive so they
enter the review sheet as candidates, and the editor decides per-row
whether each publishes. They are NOT auto-published and NOT a new
public action type. Implement as part of C3 (it is a classification
change in the same code being made deterministic). Expect the
VWU/Sentara 2026-06-11 action (`3b4d2f5b81a8`) to appear as a new
unreviewed candidate when this lands — that is the intended effect,
not a regression.

---

## Tranche D — simplification (audit §7 items 8–9)

Lowest urgency. Do not start while any C item is mid-flight.

### D1 — unify the two pull scripts' shape (§7 item 8; §2a)

**Problem.** `pull_accreditation_overrides.R` is flat (all logic in
`main()`); `pull_college_cuts_overrides.R` wraps its logic in a
function. Applying "identical" patches to both is how the
`allow_editor_added_rows` lazy-evaluation bug (§2a) happened, and the
Tranche A dropper changes had to be cuts-only for the same reason.

**Direction:** restructure the accreditation script to the wrapped shape
(function + thin `main()`), matching cuts — the wrapped shape is the
better one (testable without CLI parsing). Then evaluate extracting the
genuinely shared skeleton (arg parsing, auth, tab read, header assert,
merge, atomic write) into a helper both call, with pipeline-specific
merge/dropper functions injected. Stop at the point where further
unification requires behavior changes — this tranche is
behavior-preserving by definition.

**Also fold in (from Tranche A review notes) [DECIDED 2026-07-07]:**
the accreditation side gains a stale-row dropper as part of D1,
mirroring the cuts side: sheet-only non-manual accreditation rows
absent from both local overrides and current candidates are dropped
from the merge, with the Tranche A guards from day one — unconditional
message with sample ids, and decision-carrying rows quarantined to a
committed `data_pipelines/accreditation/review_quarantine.csv` (add it
to both workflows' add lists, same pattern as the cuts quarantine)
rather than dropped. This is an approved behavior change shipping with
the unification, not a proposal.

**Verification:** the pull scripts have no direct unit tests; their
logic lives in impl.R helpers that do. After restructuring, run both
scripts against the live Sheet (editor runs, expects "0 changed"
no-op output on a synced state) and diff the CSVs — must be
byte-identical.

### D2 — publish stops regenerating artifacts it cannot build correctly (§7 item 9; §3.5)

**Problem.** Publish's DAPIP-audit rebuild regenerates
`dapip_code_coverage.csv` via a fallback path (publish never downloads
DAPIP, so `dapip_action_rows_raw.csv` is absent and the code-coverage
block backfills an all-NA `action_description` column) and commits that
degraded copy over refresh's good one. Verified in the audit: zero
degraded commits have actually happened yet and nothing consumes the
file downstream — this is future-proofing, not a live fix.

**Direction [DECIDED 2026-07-07: the skip]:** in
`build_dapip_vs_scraper_audit.R`, when `dapip_action_rows_raw.csv` is
absent, skip the coverage-CSV write entirely (leave the committed copy
untouched) instead of writing a degraded one; log one line saying so.
(The zero-code alternative — dropping the file from publish's add
list — was considered and rejected as weaker.)

**Verification:** run the audit script locally without the raw CSV
present; confirm the coverage file on disk is untouched and the audit
CSV itself is still produced correctly.

---

## Deferred (do not build without a fresh decision)

**Durable `action_id` migration (audit §7 item 10).** Only if the
hardened matcher (C1) still produces noisy gate failures in practice.
Ships with a migration table for Sheet + CSV or not at all. High risk;
the audit downgraded it deliberately (§1a verdict) — the absorption
layers plus Tranche A's sequencing fix removed most of its value.

---

## Suggested sequencing and sizing

| Order | Item | Size | Blocks/depends |
|---|---|---|---|
| 1 | B1+B2 (one PR) | S — extraction + yml | none; do first so C4's state-file staging rides the shared script |
| 2 | C4 alerting | M | B merged (stages `drift_state.json` via shared push script) |
| 3 | C1 fail-closed matcher | M | none; fold-vs-flip decided (fold + flip to unreviewed) |
| 4 | C2 tombstones | L | ship alone; touches every override consumer |
| 5 | C3 determinism | M | none; direction decided (persist at DAPIP-build) + merger routing to review queue included |
| 6 | D1 pull-script unification | M | after C1/C2 settle impl.R churn |
| 7 | D2 publish artifact hygiene | XS | none; can piggyback any PR |

Every PR: green CI (all five checks), the verification recipe from its
item above, and an audit-doc revision note recording what shipped.
