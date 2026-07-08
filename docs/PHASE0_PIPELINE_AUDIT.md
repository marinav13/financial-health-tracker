# Phase 0 Audit — Accreditation / College Cuts Pipeline

**Date:** 2026-07-07
**Scope:** `scripts/`, `data_pipelines/`, `refresh-ipeds-site-data.yml`, `publish-editorial-overrides.yml`
**Status:** Phase 1 underway. HLC gate hotfix merged to `main` (`d57bff8d2`); Tranche A (§7 items 1–2) merged to `main` (`4d775a181`, PR #6) and live. Remaining tranches are designed in `docs/PHASE1_PLAN.md` and await separate sign-off.
**Rev 2 (2026-07-07):** amended after log/artifact forensics on the actual failed runs — see §1a verdict, §1d, §2b, §3.1, §3.4, §7.
**Rev 3 (2026-07-07):** added §3.9 — a confirmed case of the §3.1 orphan loop destroying human editorial work (UC Davis), plus two newly found silent-failure gaps.
**Rev 4 (2026-07-07):** added §3.10 — the "47 ignored actions" settled: 46 transient (by-design review gating against the stale run-start snapshot), 1 persistent and environment-dependent (VWU/Sentara merger, permanently unreviewable due to runner-local PDF text cache).
**Rev 5 (2026-07-07):** pre-Phase-1 scope closures — see §3.11 (rewrite-script exposure: manual-only, never cron-wired; the full 8-row wiped batch identified; Q1 sheet-revision reconstruction blocked on Drive API being disabled in the GCP project), §3.10 addendum (full-population stranded scan: exactly 1), and §7 item 1 (fail-loudly semantics pinned: exit 0, quarantine, Issue-based escalation).
**Rev 6 (2026-07-07):** promoted the accreditation-tab rewrite to its own item (§3.12) with full forensics and an explicit verified/unverified split; the "exactly 1" stranded count is downgraded to provisional pending a cache-present runner diagnostic (PR #5, branch `phase0-gate-diagnostic`); cuts-side Q1 recovery is functionally closed (all 8 wiped rows re-reviewed and re-approved by the editor).
**Rev 7 (2026-07-07):** cache-present runner diagnostic completed (run 28889590630): **stranded count = 0**. The provisional "1" was itself a cache-absent measurement artifact. §3.10 finalized; §7 item 6 downgraded from live-defect fix to determinism/reproducibility fix.
**Rev 8 (2026-07-07):** accreditation-side recovery functionally closed — the editor confirmed the §3.12 destruction finding (27 re-derived accreditation rows + 8 cuts rows wiped and re-staged `unreviewed`) and has manually re-reviewed and re-approved all 27 rows, mirroring the cuts-side closure. Residual: the 11 never-re-derived IDs; the Drive-API revision-history pull is now of forensic interest only. PR #5 (`phase0-gate-diagnostic`) is closed and its branch deleted. An interrupted Phase 1 implementation attempt left no code changes (working tree and integrity scan verified clean 2026-07-07).
**Rev 9 (2026-07-07):** added §3.13 — a fourth suppression/consistency defect found live when the post-recovery Publish run failed: HLC institution-page status badges are excluded at staging but had no gate-side exemption; hotfixed and merged same day (`d57bff8d2`). §7 items 1–2 implemented on PR #6: post-commit sheet appends (§3.1 loop closed), decision-row guards with committed quarantine (§3.9/§3.12 hazard closed pending merge), and header-order append verification (§3.2 write side closed). The 35 re-approved rows were made durable ahead of Tranche A via local pulls committed in `97ceca714`.
**Rev 10 (2026-07-07):** Tranche A merged to `main` (`4d775a181`, PR #6; all five CI checks green including lintr). Live verification before merge: sync-script dry-runs returned 0-append clean against both real tabs (validating the header-order assert against the tab's formatted headers), and the rewrite guard's abort path was proven on a sacrificial tab copy (63 decision rows blocked, rescue CSV written, tab untouched — the UC Davis scenario replayed with the guard on). `claude-wip` and the stale `phase0-gate-diagnostic` tracking ref cleaned up. This audit doc is now committed to the repo; the Tranche B–D implementation plan is split into `docs/PHASE1_PLAN.md`.

---

## 1. ID stability verdict (the requested first check)

### 1a. `action_id` (accreditation) — deterministic but NOT durable. Confirmed unstable in practice.

Generated in `scripts/shared/editorial_review_helpers_impl.R` →
`compute_accreditation_action_id()` (line ~421):

```
sha1( identity | accreditor | action_date | action_label_raw )   # first 12 hex chars
```

where `identity` falls back `unitid → export_unitid → institution_name`.

Two volatility sources, both confirmed live:

1. **`action_label_raw` is the full scraped sentence.** Any upstream rewording,
   punctuation change, or scraper-extraction change produces a new hash.
   `normalize_review_identity_text()` strips case/whitespace/punctuation, which
   absorbs formatting noise but not word changes. DAPIP and accreditor sites do
   reword actions.
2. **The unitid fallback chain.** An institution that first scrapes with no
   unitid match hashes on its *name*; when the Supabase mapping or IPEDS match
   later resolves a unitid, the same action re-hashes to a different ID.

**Smoking gun that this is a known recurring problem:** the codebase already
contains remediation machinery for drifted IDs — canonicalization logic in the
apply-only review gate (log line: *"canonicalized 1 committed review candidate
action_id value(s) to existing override rows"*), which remaps a drifted
snapshot ID to an existing override row by matching unitid + accreditor +
±30-day date window + compatible type. Canonicalization only rescues rows it
can match; drifted rows with no match become the "missing editorial overrides"
gate failures.

### 1b. `cut_id` (college cuts) — stable upstream key. Orphans come from scope filtering, not ID drift.

`cut_id = id` straight from the College Cuts public API
(`build_college_cuts_join.R` line ~601) — a database primary key (UUID).
It does not drift between runs. Orphaning mechanisms are:

- **Tracker-scope filtering:** `filter_college_cuts_overrides_for_tracker_scope()`
  drops override rows whose effective unitid is no longer in the tracker index.
  If an institution leaves the tracker (or its unitid match changes), its rows
  vanish from `editorial_overrides.csv` while the Sheet still holds them.
- **Upstream record removal:** a cut deleted or de-confirmed in the College Cuts
  DB leaves the API feed; the stale-row dropper handles the candidates side,
  but sheet-only rows trip the strict pull guard.

### 1c. Bonus: a THIRD content-derived key with the same disease

`scraper_source_key` (`accreditation_helpers.R` →
`build_accreditation_action_source_key()`, line 85) hashes
unitid|name|accreditor|**action_type**|**action_label**|date|urls|file_id.
It is stored in the committed audit CSV and **recomputed at export time** in
`build_web_exports.R`. Any change to `classify_action()` keywords, label
normalization, or scraper text between audit-build and export makes stored and
recomputed keys diverge → the export guard fires (see §3.6).

**Verdict (revised after execution-path + forensic checks):** the instability
is real, but two live absorption layers already blunt most of it, so the
durable-ID migration is **downgraded from "single highest-leverage change" to
a medium-term option**:

- **Layer 1 — stage-time suppression** (`find_cross_source_duplicate_id` via
  `stage_accreditation_editorial_overrides`, impl.R:1061; live in every weekly
  refresh): a new candidate matching an existing override row on
  unitid+accreditor+±30d+compatible-type is discarded and its fresh text is
  folded into the existing row. Reworded labels on tracked actions never mint
  duplicates.
- **Layer 2 — gate-time canonicalization**
  (`canonicalize_accreditation_review_gate_action_ids`, called at
  `build_web_exports.R:2726`): runs on **every gated export in both
  workflows** — refresh (`--apply-only-review-gate`, yml line 393), publish
  `publish` mode, and `teachout-cleanup` mode. Empirically confirmed live: the
  five sample IDs from the Jun 8 "25 missing overrides" failure are *still*
  candidates-only today and are absorbed by canonicalization on every run.

A durable ID also would **not** have prevented the observed orphan errors
(see §3.1 — those rows were never in the CSV at all, so no ID scheme saves
them). The higher-leverage, low-risk work is hardening and documenting the
absorption layers (§1d) and fixing the append-then-fail accumulation loop
(§3.1). `cut_id` needs no change; its fix is to stop letting scope filters
silently delete override rows that the Sheet still references (tombstone
instead of delete).

### 1d. Matcher precision — empirically tested (Rev 2)

The suppression/canonicalization matcher was tested against five escalation
scenarios (real helper code, synthetic rows):

| Scenario (same school+accreditor) | Suppressed? |
|---|---|
| warning → probation, 18 days | no — escalation survives |
| monitoring → notice, 18 days | **yes — swallowed** |
| same-type distinct actions, 18 days | **yes — swallowed** |
| warning → adverse_action, 18 days | no — asymmetric case is protective |
| same-type, 31 days | no — window edge holds |

Two real precision exposures: (A) the bidirectional `notice↔monitoring` pair
can swallow a formal Notice sanction issued within 30 days of a routine
monitoring row; (B) same-type distinct actions within 30 days are suppressed
regardless of label text. The teach-out carve-out (impl.R:935) is existence
proof that class (B) has already bitten once and was patched case-by-case.
Worse, suppression **folds the new text into the matched row's `source_*`
fields** (impl.R:1071-1074) — if that row was already approved and has no
editor override statement, the *published* text silently changes without
re-review.

Live relevance: 3 of the 5 absorbed Jun-8 sample IDs are SACSCOC `notice`
rows — the notice↔monitoring pair is doing load-bearing absorption today, so
it cannot simply be deleted; it must be made fail-closed (label-similarity
requirement, one-directionalization, or flip-to-unreviewed on fold).

---

## 2. The two pre-identified bugs — confirmed

### 2a. `object 'allow_editor_added_rows' not found` — confirmed, root cause found, already fixed

- Commit `24b161edb` (Jul 6, 19:09) added the flag to **both** pull scripts with
  the *same* two edits: parse flag in `main()`, pass it at the merge call.
- `pull_accreditation_overrides.R` is **flat** — everything lives in `main()`,
  so both edits landed in one scope. Works.
- `pull_college_cuts_overrides.R` is **wrapped** — `main()` calls
  `pull_college_cuts_overrides()`, and the merge call is inside the wrapper.
  The edit referenced `allow_editor_added_rows` inside the wrapper without
  adding it as a formal parameter or passing it from `main()`. R's lazy
  evaluation deferred the failure to `isTRUE(allow_editor_added_rows)` inside
  the merge helper — hence the confusing traceback ending in `-> isTRUE`.
- Fixed 4 hours later in `4c7833955` (adds the formal + threads it through).

**Root cause class:** two "parallel" scripts with different internal shapes
receiving identical textual patches. This is the drift the brief predicted, and
it will recur while the two scripts remain structurally divergent.

### 2b. Git push parity — confirmed, with two aggravating factors

- **Refresh** (`Commit and push updated data`): stash → 3-attempt retry loop →
  rebase → on conflict, path-scoped `checkout --theirs` for data paths, hard
  abort on non-data conflicts.
- **Publish** (`Commit and push editorial publish`): stash (added in
  `3d306f8ea`, Jul 7 03:44 UTC — *not* 24b161edb as Rev 1 stated) → single
  `fetch/rebase/push`. **No retry, no conflict resolution.** A concurrent
  push landing mid-publish still kills the run. The causal chain for the
  final `cannot rebase` failure (run 28839482518, 03:38 UTC) is now exact:
  `244781c51` added the audit rebuild to publish, which regenerates 4 DAPIP
  CSVs that publish's add list omitted → unstaged files → rebase refused →
  `3d306f8ea` staged those 4 files and added the stash six minutes later.
- **Aggravator 1 — different concurrency groups:** refresh uses
  `refresh-source-data-weekly`, publish uses `publish-editorial-overrides`.
  The two workflows CAN run simultaneously and race each other's pushes to
  `main`. **Empirical check (2026-07-07, last ~15 refresh / 20 publish runs +
  push-step logs of every run that reached push):** no two run windows have
  actually overlapped (closest: 26 s apart, one of them cancelled), and every
  refresh push succeeded on attempt 1/3 — the conflict-resolution path has
  never executed. This is a config-permitted hazard, not an observed event.
  It remains worth closing because publish runs cluster tightly around
  refresh (same evenings, minutes apart) and nothing prevents the overlap.
- **Aggravator 2 — divergent `git add` lists.** Refresh stages 15 paths;
  publish stages 7. The historical
  `cannot rebase: You have unstaged changes` fired because publish's pipeline
  regenerates tracked files (e.g. review-candidate CSVs) that its add list
  omits; pre-stash, those dirty files blocked the rebase. The stash now masks
  this — but masking means publish silently discards regenerated tracked
  files, which is how blended/inconsistent snapshots get committed (see §3.4).

**Recommendation direction (Phase 1):** one shared push script (or composite
action) used by both workflows; a shared concurrency group (or explicit
cross-workflow mutex) so refresh and publish serialize on push.

---

## 3. Root cause of each pasted CI failure

### 3.1 Refresh: "Sheet contains N action_id values not in editorial_overrides.csv" (11, then 38 in publish)

Strict guard at `editorial_review_helpers_impl.R:1131`.

**Rev 2 — mechanism now proven by forensics, and it is NOT what Rev 1
guessed.** `git log -S` for all 9 sample IDs (11-list + 38-list) shows none
of them has *ever* existed in `editorial_overrides.csv` at any commit. They
were **sheet-only from birth**. That rules out teach-out cleanup deletion,
scope-filter deletion, and classic drift-of-a-committed-row for these IDs.

The proven mechanism — **append-then-fail accumulation**:

1. Refresh staging appends new candidate rows to the Google Sheet **mid-run**
   (an immediate, unrollback-able external write) and writes the same rows to
   the local CSV — but the CSV only becomes durable if the run survives to
   the final commit-and-push step.
2. Any downstream failure (that evening: the strict pull guard itself, plus
   mid-refactor code churn) kills the run → CSV changes are discarded → the
   Sheet keeps the appended rows → they are now orphans.
3. The next run's strict pull guard fails on the previous batch — but only
   *after* that run's staging has appended its own new batch. Each failed run
   grows the orphan pool. **Self-amplifying deadlock.**

Direct evidence from run 28823719699 (Jul 6, the "11" failure): its own log
shows `Appending 11 accreditation review row(s)` at the staging step, then
the pull step failing on 11 *pre-existing* orphans; the run died → its 11
became new orphans; publish 70 minutes later saw 38 accumulated. Publish
counts higher than refresh because publish compares the Sheet against the
*committed* CSV (no staging step), while refresh compares against
committed + its own fresh staging.

Rehashing the failed run's own uploaded
`accreditation_tracker_actions_joined.csv` (artifact) with an exact
reimplementation of the ID hash (validated 626/626 against committed
candidates) reproduces **none** of the 9 orphan IDs — the orphaned rows came
from earlier runs under different code/data (that evening included
scraper-label and layout changes, each of which flips the content hash). So
ID-input churn set the stage, but the accumulation loop is what manufactured
the failures.

Fix implications: make staging's Sheet append the *last* effect of a
successful run (or transactional: stage-to-sheet only after the CSV commit
lands), and keep the `--allow-editor-added-rows` valve for recovery. The
teach-out-cleanup and scope-filter deletion paths remain real but
undemonstrated contributors.

### 3.2 Refresh: `row_origin='2026-07-06'` (a DATE in row_origin)

Mechanism identified precisely. Staging appends new candidate rows with
`googlesheets4::sheet_append()` (`stage_college_cuts_review.R:136`), which
writes **positionally — it never checks the tab's actual header order.**
The local column contract is `COLLEGE_CUTS_REVIEW_SHEET_COLUMNS` (19 columns:
`..., source_publication, row_origin, first_seen, ...`). After the sheet layout
churn (new layout → `6fbc3f50f` "Revert to old Google Sheets layout"), the tab
had one column more/fewer than the constant → every appended cell shifted one
left → `first_seen` (= append date, `2026-07-06`) landed under `row_origin`.

`repair_shifted_college_cuts_review_sheet_rows()` exists to un-shift exactly
this signature on the **read** side — proof it has happened before. The
**write** side still has no header-order verification before appending.
(`assert_college_cuts_review_sheet_header` checks column *presence* on read,
not order, and is not consulted by the append path.)

### 3.3 Refresh: "1 cut_id not present in editorial_overrides.csv"

Same class as 3.1 via the cuts-side guard
(`editorial_review_helpers_impl.R:2113`): sheet row survived while the CSV row
was dropped by tracker-scope filtering or the upstream record left the API
feed. Additionally, rows corrupted by the 3.2 column shift can fail the
stale-row dropper's matching and surface as orphans.

### 3.4 Refresh: "Review gate enabled but 25 committed review candidates missing editorial overrides"

Guard at `editorial_review_helpers_impl.R:1316`.

**Rev 2 — fully explained, timeline exact.** The failing run was
27111355081 (Jun 8, 01:41 UTC start = Jun 7 21:41 EDT). It checked out code
**2 minutes after** manual commit `1fee9bdd6` (21:39 EDT, "fix accreditation
review pipeline and sync export contract") which committed a candidates CSV
containing 25 action_ids that stage-time cross-source suppression (added
Jun 5, `a94a6f4dd`) had *by design* kept out of the overrides CSV — each is
represented by an existing canonical override row instead. The gate had no
way to know that yet: `canonicalize_accreditation_review_gate_action_ids`
was written **as the response to this exact failure** and landed 85 minutes
later (`1f915baf5`, 23:06 EDT, "Address apply-only accreditation review
gate"). The next run passed this gate.

All five sample IDs **have unitids** (Florida Memorial, Martin, Kentucky
State, Notre Dame of Maryland, Ole Miss) — the no-unitid blind-spot theory is
refuted for this incident. Three of the five are SACSCOC `notice` rows,
i.e. the `notice↔monitoring` matcher pair (§1d) is what absorbs them today.
They remain candidates-only in the current tree, silently canonicalized on
every gated export — working as designed, but load-bearing and undocumented.

The blended-commit mechanism (`checkout --theirs` per-file resolution) stays
noted as latent: it has never executed (every refresh push landed on attempt
1/3) and played no role in this incident.

### 3.5 Publish: `dplyr::count()` — "Column action_description is not found"

`build_dapip_vs_scraper_audit.R:858`. The code-coverage block reads
`dapip_action_rows_raw.csv` if present, else falls back to the filtered CSV:

- `dapip_action_rows_raw.csv` is **untracked** — it exists only on refresh
  runners right after a DAPIP download.
- `dapip_action_rows_filtered.csv` (committed, what publish sees) has 33
  columns and **no `action_description`**.
- Publish never downloads DAPIP → falls back to filtered →
  `count(action_code, action_description, ...)` exploded. Refresh never hit
  it. A publish-only code path, invisible in refresh testing.

Guard added in `a124fe589` (backfill `NA` column). This stops the crash but
means every publish run now regenerates `dapip_code_coverage.csv` with all-NA
descriptions and **commits it over refresh's good copy**. Phase 1 should stop
publish from regenerating/committing coverage artifacts it lacks the inputs to
build correctly.

(The accompanying vroom "parsing issues" warning in the log is the
sparse-column type-inference noise documented inside
`warn_if_scrape_count_dropped` — a red herring here.)

### 3.6 Publish: "Accreditation export join bug ... Re-run the DAPIP audit before exporting."

Guard at `build_web_exports.R:1314-1319` — fires when audit rows tagged
`scraper_backed_keep`/`hybrid_keep` fail to join back to
`accreditation_tracker_actions_joined.csv` on the content-derived
`scraper_source_key` (§1c).

Why it fired in normal operation: publish originally **trusted the committed
audit CSV** from the last refresh. Between that refresh and the publish run,
recomputed keys diverged — either a code change to label/type
normalization/classification (e.g. `4647e7e64` expanded label patterns) or a
blended commit (§3.4, bullet 3). Fix already applied in `244781c51`: publish
now rebuilds the audit from committed inputs before exporting, so audit and
export use the same code + data. That is the right *direction*; the residual
risk is that both this guard and the audit depend on a content-derived key
that drifts by design (§1c).

Answer to the brief's question — publish does **not** need to re-run scrapers;
it needs (and now has) a rebuilt audit from the committed action CSVs. Full
scraper re-runs in publish would re-introduce fresh-scrape volatility into a
human-review publish step, which is the wrong trade.

### 3.7 Publish: `cannot rebase: You have unstaged changes` — see §2b.

### 3.8 Publish: `allow_editor_added_rows not found` — see §2a.

### 3.9 (Rev 3) Confirmed editorial-work loss: the UC Davis case

First demonstrated instance of the §3.1 orphan loop destroying a human
editorial decision, not just failing runs. Every step verified from run logs
and commit contents:

1. **Jul 6 ~03:14 UTC** — failed refresh `28763144456` appended 8 new cuts
   rows to the Sheet (UC Davis equestrian cut, cut_id
   `990d9d98-5fc7-4bb8-bb94-a822f38007cd`, among them), then failed reading
   them back — this is the *same batch* as the §3.2
   `row_origin='2026-07-06'` column-shift error. The run never committed, so
   all 8 rows were sheet-only orphans.
2. The editor edited that orphan row (shortened the cut text, set approved).
   The decision existed only in the Sheet.
3. **Jul 7 ~02:30 UTC** — a manual tab rewrite
   (`rewrite_college_cuts_review_sheet_from_overrides.R`) replaced the whole
   tab with the 250 rows from the local CSV. Davis was in no committed CSV
   (every pull that evening had failed), so the rewrite **silently erased the
   edited, approved row**. The script has no guard against discarding rows
   that carry review decisions.
4. **03:43 UTC** — the successful publish committed a CSV with zero Davis
   rows (`cc3f2e854`, verified). The edited text never reached the site; the
   cuts review gate withheld the absent row.
5. **03:57 UTC** — the first fully successful refresh in a week
   (`28840421669`) staged the Davis cut as a brand-new **unreviewed** row
   with regenerated text (`first_seen: 2026-07-07`, committed in
   `a24cfdbe4`). To the editor this looked like "refresh un-approved my row";
   in fact their row had been deleted and replaced.

Two silent-failure gaps confirmed while tracing this:

- **The publish stale-row dropper is silent in CI.** The "Ignoring stale
  sheet-only scraper row(s)" message in `pull_college_cuts_overrides.R`
  prints only under `--verbose`, which the publish workflow does not pass.
  Rows can be discarded with no log line at all.
- **The apply-only accreditation gate silently ignored 47 recomputed
  action(s)** in the same successful publish (`28839925327`) — the
  `drop_unlisted` path logs a single message and continues. The size of that
  number (47) after a mid-refactor evening suggests the committed snapshot
  and the recomputed actions had drifted substantially; nothing surfaced it
  as a problem.

Hardening implications (folded into §7 item 1): Sheet-append must not precede
the CSV commit; the tab-rewrite script and the stale-row dropper must
**refuse to discard any row with a non-empty review decision** (fail loudly
instead); the dropper's message must print unconditionally; and large
`drop_unlisted` counts should trip the drift reporter rather than pass as one
info line.

### 3.10 (Rev 4) The "47 ignored actions" — settled

`Apply-only accreditation review gate: ignoring 47 recomputed action(s)…`
appeared in both the successful publish (`28839925327`) and the successful
refresh (`28840421669`) on Jul 7. Investigation verdict: **46 transient +
1 persistent**.

**The 46 — by-design review gating, no action needed.** The gated export
compares recomputed actions against the *run-start* snapshot
(`review-snapshots/…`, one week old by construction). The week ending Jul 7
covered the Jun-29→Jul-6 failed-refresh gap plus the mid-refactor ID churn,
so the count was unusually large. Rerunning the gate diagnostic against the
*current* committed candidates yields exactly **1** unexpected action — all
other 46 are now in the committed candidates (33 genuinely new: 9 notice,
9 warning, 9 probation, 4 show_cause, 1 adverse, 1 other) or absorbed by
canonicalization. This weekly "ignoring N" of fresh unreviewed actions is
the review gate working as intended (same as the cuts-side "ignoring 9
recomputed cut row(s)" in the same run). It is *reported misleadingly* —
the message reads like an error and gives no IDs — but it is not a defect.

**The 1 — a real, persistent defect with a novel root cause.** The Virginia
Wesleyan / Sentara College merger action (SACSCOC, 2026-06-11, action_id
`3b4d2f5b81a8`, DAPIP `dapip_backed_keep`) is present in the export's
assembled actions but **absent from the committed candidates** — so it can
never enter the review sheet, never be approved, and is withheld from the
public export on every run. Forensic isolation:

- Regenerating candidates locally from the *same committed inputs* produces
  the run's 656 rows **plus exactly this one row** (657) — nothing else
  differs.
- The row's label (`label_source: dapip_file_text`) derives from a DAPIP
  PDF. The filtered CSV stores **absolute runner paths**
  (`/home/runner/work/…/cache/dapip/…`) to a **gitignored** text cache.
- `.select_action_summary_source()` (export_helpers.R:2829) prefers cached
  PDF text for SACSCOC rows **when the file exists on disk**. On the runner
  the text exists (restored/downloaded in-run) and its OCR content
  ("Heightened Monitoring or Focused Review" family) drives a classification
  that excludes the row from candidates; locally the text is missing, the
  code falls back to the stored label, and the row is included.

**General class discovered: candidate generation is
environment-dependent.** The same commit produces different
`accreditation_review_candidates.csv` on different machines depending on
which PDF text files happen to exist in a gitignored cache. Because the
runner (the authoritative environment) persistently *has* the cache, the
merger row is persistently excluded there — this will NOT self-heal next
week. And because the export assembly and the candidates builder consume the
text differently, a row can be permanently stranded between them: in the
public-table assembly (so the gate must drop it) but never in candidates (so
it can never be reviewed into publication).

**Full-population scan — FINAL (Rev 7): stranded count = 0 in the
authoritative cache-present environment.** The one-off runner diagnostic
(run 28889590630 on PR #5; cache verified warm — 18,065 DAPIP cache files —
with a hard-fail guard so a cold cache can never masquerade as a real
count) returned an **empty diagnostic CSV** and logged no "ignoring N"
line; it canonicalized 28 snapshot IDs (vs 27 cache-absent — the extra
remap is the VWU row resolving instead of stranding).

Post-mortem of the earlier numbers: the cache-absent local scan said "1"
(VWU/Sentara) — that was a measurement artifact of the same
environment-dependence being measured, in the opposite direction from the
assumed hazard. Cache-present, the pipeline treats the VWU merger row
*consistently* across candidates and export (classified into the
monitoring/procedural family per the PDF text, canonicalized at the gate)
— excluded everywhere, deadlocked nowhere. It is not on the site and not
in the review queue, but by consistent classification, not by stranding.
Whether a merger/absorption approval *should* surface publicly is an
editorial-policy question, not a pipeline defect; flagged for the editor.
The naive 1,051-keep-row cross-check (280 "misses") remains documented as
the wrong instrument (candidates are a recent-review queue, not a table
mirror).

**What survives as a real finding:** candidate generation and export
assembly are environment-dependent — the same commit produced a 656-row
candidates file on the runner and 657 locally, proven in both directions.
No action is currently harmed by it, but any local/publish-runner
regeneration of candidates (publish has no accreditation cache!) writes
subtly different data than the weekly refresh. §7 item 6 (persist
text-derived classification into committed CSVs) is retained as a
determinism/reproducibility fix at reduced priority, and §7 item 7's
weekly diagnostic artifact keeps the stranded count permanently
observable.

Fix direction (Phase 1): make candidate inclusion deterministic from
committed data — either classify from the committed CSV fields only, or
persist the text-derived classification into the filtered CSV at DAPIP-build
time so every downstream consumer sees the same value; and reconcile the
candidates builder with the export assembly so no `*_keep` action can be
absent from candidates. Also: the gate's "ignoring N" message should split
by-design withholding (new-this-week) from anomalies (kept-by-audit but not
reviewable) and list sample IDs for the latter.

### 3.11 (Rev 5) Rewrite-script exposure and the full casualty list

- **Not cron-wired.** `grep -r rewrite_ .github/` returns nothing. Both
  rewrite scripts are manual tools: the accreditation one first committed in
  `792406c55` (Jul 6, 15:37 EDT), the cuts one in `3fb127ca9` (Jul 6,
  22:42 EDT). Total known executions: one cuts-tab rewrite and one
  accreditation-tab rewrite, both during the Jul 6-7 debugging session.
  No unattended runs have ever occurred.
- **The full wiped cuts batch is identified.** The 8 sheet-only orphan rows
  (appended by failed run `28763144456`, wiped by the cuts rewrite,
  re-staged `first_seen=2026-07-07` by run `28840421669`): Saint Louis
  University (staff_layoff), Pennsylvania Western University
  (program_suspension), Eastern Washington University (program_suspension),
  University of Central Florida (department_closure), Life University
  (staff_layoff), Iowa State University (program_suspension), **University
  of California-Davis** (program_suspension — the one confirmed lost
  editorial decision), Kenyon College (staff_layoff). All 8 are now
  `unreviewed` and need (re-)review.
- **Whether the other 7 cuts rows or any of the 38 wiped accreditation
  sheet rows carried review decisions is NOT yet confirmed.** The
  authoritative source is Google Sheets revision history; the Drive API is
  disabled in the service account's GCP project (project 627248100171,
  `SERVICE_DISABLED`), so the API path is blocked until it is enabled.
  This is an explicit unknown, not a confirmed absence.
- **Risk re-rating for §7 item 1:** implementation risk stays low (guards +
  sequencing), but the guard scope must cover the manual rewrite scripts,
  not just CI paths.

### 3.12 (Rev 6; closed Rev 8) The accreditation-tab rewrite — second destructive event; re-derived rows re-reviewed

Rev 5 buried this inside §3.11; it deserves its own item. The same session
that ran the cuts-tab rewrite (§3.9) also ran
`rewrite_accreditation_review_sheet_from_overrides.R` (~Jul 7 02:00-02:40
UTC), replacing the whole `accreditation_review` tab with 622 rows built
from the committed overrides CSV.

**What is verified (evidence):**

- Immediately before the rewrite window, the tab contained **38 sheet-only
  non-manual action rows** — proven by the two publish failures at 22:26
  and 22:38 UTC Jul 6 ("Google Sheet contains 38 action_id value(s) that
  are not present in editorial_overrides.csv").
- Immediately after, those rows were gone — proven by the 03:43 UTC publish
  succeeding through the same strict pull guard with no orphan error and no
  `allow_editor_added_rows` import messages.
- The wiped population is fully accounted for arithmetically:
  38 = 11 pre-existing orphans (flagged by refresh `28823719699` at 22:13)
  + 11 appended to the Sheet by that same run's staging + 16 appended by
  earlier failed runs (in that run's local CSV, hence invisible to its own
  pull, but never committed). **27 of the 38 re-derived** under the same
  IDs in the Jul 7 successful refresh and are back in the sheet as
  `unreviewed` — including 4 show_cause, 5 probation, and 9 warning rows
  (substantive sanctions awaiting review: Talladega ×2, U. of Holy Cross,
  Johnson C. Smith, Mary Baldwin, U. of Valley Forge, Rider, Hilbert,
  Calvary ×2, Sinte Gleska ×2, and others). The remaining **11 never
  re-derived** (the mid-refactor ID-churn cohort; their underlying actions
  may exist under new IDs among the week's 33 new candidates, or were
  dropped by the newer teach-out/procedural filters).

**What is NOT verified — and the resolution of the "two executions / one
documented destructive event" contradiction:** the *row removal* is
documented for both rewrites; *destruction of editorial work* is documented
only for the cuts rewrite (UC Davis: editor testimony + commit-level
absence). For the accreditation rewrite it is **purely theoretical
exposure**: whether any of the 38 rows carried a non-empty
review/approval status at wipe time is unknown. The rows were appended as
`unreviewed` by the failed runs; damage occurred only if the editor
reviewed any of them in the Sheet between Jul 6 03:14 UTC and the rewrite.
The authoritative source is Google Sheets revision history, blocked on the
Drive API being disabled in the service-account project (§3.11). Until
that is pulled (or the editor confirms they made no accreditation reviews
in that window), this stays an **open data-recovery question with the same
priority as the formerly-open cuts rows.**

**Cuts-side closure note (Rev 6):** the editor has since re-reviewed and
re-approved all 8 re-staged cuts rows with fresh edited text — the cuts
half of the recovery question is functionally closed regardless of what
revision history shows.

**Accreditation-side closure (Rev 8):** the editor confirmed the destruction
finding and has manually re-reviewed and re-approved all 27 re-derived
accreditation rows, the same way the 8 cuts rows were handled. Both
re-derived cohorts are now functionally recovered. What remains open:
(a) the 11 never-re-derived accreditation IDs — their underlying actions
may exist under new IDs among the week's 33 new candidates or may have
been dropped by the newer teach-out/procedural filters; (b) the Google
Sheets revision-history pull (Drive API still disabled), now of forensic
interest only. The destructive-rewrite *hazard* itself remains open until
the Phase 1 §7 item 1 guards land.


### 3.13 (Rev 9) HLC status-badge candidates: staging-only suppression with no gate-side mirror — found live, hotfixed same day

Found by the first post-recovery Publish run (2026-07-07), which died at
the export review gate: "3 committed accreditation review candidate(s)
are missing editorial overrides" (`ebe6772ec0fe` Ohio Dominican notice,
`fddf18cf82c8` Saint Mary-of-the-Woods notice, `43d33bfc7586` Wittenberg
probation — all real HLC sanctions dated 2026-07-02, all in tracker
scope).

Root cause — a **third suppression path the audit's layer catalog had
missed**: `stage_accreditation_editorial_overrides` drops new HLC rows
scraped from `hlcommission.org/institution/` pages whose raw label is a
bare status badge ("On Notice", "On Probation", ...). Deliberate and
correct — the badges duplicate the real board actions — but the
exclusion existed only at staging. The candidates builder includes badge
rows, and gate-time canonicalization mirrors only the
cross-source-duplicate exemption, so committed candidates containing a
new badge row deterministically fail every gated export that checks the
*current* candidates. Refresh gates against the week-old run-start
snapshot, which is why the Jul 7 refresh introduced the rows without
failing — and why the Jul 13 scheduled refresh would have failed once
its snapshot caught up.

Two additional badge rows (Calvary, Sinte Gleska — 5 in the current
snapshot total) did not surface in the failure because the ±30-day
duplicate matcher canonicalized them onto those schools' real sanction
rows; the three that failed had no compatible row inside the window.

Fix (`d57bff8d2`, merged to `main` 2026-07-07): the badge mask is
extracted into `is_hlc_institution_status_page_row()` and shared by
staging and `canonicalize_accreditation_review_gate_action_ids`, which
now exempts badge candidates from the allowed set (their recomputed
actions fall to the drop_unlisted path — the treatment staging always
intended). Replayed against the committed CSVs: missing-overrides count
3 → 0; all 5 badge candidates have no override rows, so nothing
published changes. The regression test reproduces the production hash
of the Ohio Dominican row exactly.

**Class lesson:** any staging-only exclusion needs a gate-side mirror,
or it manufactures permanent candidates/overrides mismatches. The two
layers now share one helper so they cannot drift for this class again.

---

## 4. Drift detection & alerting audit

### What exists (and is genuinely good)

- Per-site `warn_on_empty_parse()` — 0 rows parsed from ≥2KB of HTML.
- Per-accreditor 0-row check with `ZERO_IS_EXPECTED` allowlist (NWCCU),
  schema validation per scraper.
- Aggregate drift vs prior committed CSV: `warn_if_scrape_count_dropped`
  (>40% per-accreditor drop), `warn_if_action_type_dropped`.
- In CI **without** `--allow-partial-accreditation`, empty-parse and 0-row
  conditions `stop()` (fail-fast).
- Workflow-level log-grep step scans `refresh-logs/combined.log` (all pipeline
  steps tee into it) and emits `::warning::`.

### The two real gaps

1. **Weekly refresh runs WITH `--allow-partial-accreditation`** (per the
   workflow comment), so fail-fast is disabled on the schedule that matters.
   Everything funnels to the log-grep step, which emits a GitHub annotation
   and **exits 0**. Green run → no notification → a broken scraper waits for
   someone to open a green checkmark's logs. This is acceptance-criterion #4,
   unmet by design.
2. **Guard-pattern coverage is accreditor-scrapers-only.** The reporter greps
   five patterns, all emitted by the accreditor scraping layer:
   - **DAPIP** download/parse produces `empty_*()` fallbacks silently — a
     0-row DAPIP week emits none of the grepped strings.
   - **College cuts API**: non-200 is fatal (good); but HTTP-200-with-0-rows
     ends pagination quietly, and the malformed-`totalPages` warning text
     matches no grep pattern.
   - **Grant Witness** (research cuts): not audited this pass; flagged for
     Phase 1 verification.

### Escalation options (constrained by what exists)

Checked: repo workflows use **no Slack/email secrets** — only Google
credentials. Realistic options, no new integrations:

- **Auto-file/update a tracked GitHub Issue on drift** using the built-in
  `GITHUB_TOKEN` (issues generate email notifications to watchers — the
  cheapest real notification channel available today).
- **Fail after N consecutive drift weeks** on the same source, tracked in a
  small committed state file — converts chronic ignored drift into a red run.
- Making the weekly run fail-fast again (dropping `--allow-partial-...`) is
  NOT recommended: it would violate acceptance criterion #1 (refresh must
  complete despite backlog) whenever one accreditor site hiccups.

---

## 5. Scraper architecture: duplication vs precision

| File | Lines | Role |
|---|---|---|
| `shared/accreditation_scrapers.R` | 3105 | 6 accreditor-specific `parse_*` pipelines |
| `build_accreditation_actions.R` | 706 | orchestration, tracker matching, summaries |
| `build_dapip_crosswalk.R` | 166 | DAPIP↔unitid crosswalk |
| `build_dapip_accreditation_actions.R` | 396 | DAPIP download → classified action rows |
| `build_dapip_vs_scraper_audit.R` | 903 | cross-source reconciliation + keep-strategy |
| `shared/accreditation_helpers.R` | 1031 | classification, normalization, source keys |
| `shared/dapip_helpers.R` | 1101 | DAPIP code classification |

**Leave alone (precision-bearing, intrinsically site-specific):** each
accreditor's parse pipeline. MSCHE walks ~565 per-institution pages (parallel,
throttled); SACSCOC has three formats (HTML detail pages, combined-report PDF,
disclosure PDFs) plus archived variants; NECHE parses statement PDFs; HLC
walks content nodes. These fallback tiers encode real site behavior. Unifying
them would blur distinctions the site currently gets right. They already share
the right primitives (`parse_items_to_rows`, `parse_public_action_sections`,
`fetch_binary_file`, the cache layer, the `warn_*` guards, schema
enforcement).

**Genuine duplication worth consolidating (Phase 1 candidates):**

1. **The two review-pipeline stacks** in `editorial_review_helpers_impl.R` —
   accreditation and cuts each have near-parallel stage/coerce/merge/gate
   functions plus two structurally different pull scripts. This divergence
   directly caused bug §2a. Highest-value dedup in the repo.
2. **Workflow setup + push logic** — ~15 near-identical setup steps and two
   divergent push implementations. A composite action for setup and one shared
   push script. The brief's hypothesis (setup duplication is *why* push logic
   drifted) is consistent with the history.
3. Repeated empty-schema constructors / CSV col-type declarations across the
   DAPIP scripts — mild, low-priority.

---

## 6. Summary table

| Failure | Root cause | Layer | Already fixed? |
|---|---|---|---|
| orphaned action_ids (11/38) | teach-out cleanup partial failure + action_id content-hash drift + scope filtering | ID contract / cleanup | repair valve only (24b161edb) |
| row_origin = date | positional `sheet_append` vs mismatched tab layout after schema churn | Sheet I/O | read-side repair only |
| orphaned cut_id | scope filtering / upstream record removal | ID contract | repair valve only |
| review gate 25 missing | candidates/overrides committed from different states (cleanup ordering, blended `checkout --theirs` commits, ID drift) | atomicity | ordering fix (0da1e9a45); blending risk remains |
| `action_description` count error | publish-only fallback to filtered CSV lacking the column | env-dependent path | crash-guarded (a124fe589); now commits degraded coverage CSV |
| DAPIP audit export guard | content-derived `scraper_source_key` recomputed under changed code vs stale committed audit | ID contract | publish now rebuilds audit (244781c51) |
| cannot rebase / unstaged | publish add-list omissions + no stash (then); no retry/conflict handling + separate concurrency groups (still) | git plumbing | partially (stash only) |
| allow_editor_added_rows | identical patch applied to structurally different scripts | script drift | fixed (4c7833955) |
| UC Davis approved row lost (§3.9) | orphan-batch edit erased by tab rewrite; silent stale dropper; re-staged unreviewed | §3.1 loop + destructive tab ops | **guards live (Tranche A, PR #6, merged `4d775a181`): rewrite abort-by-default + quarantine CSV + unconditional dropper message; abort path verified live Rev 10** |
| 47 ignored actions (§3.10) | 46 = by-design gating vs stale run-start snapshot; 1 apparent strand was a cache-absent measurement artifact — authoritative cache-present count: **0** | review gate / determinism | **closed (run 28889590630); determinism fix retained at reduced priority** |
| Accreditation tab rewrite (§3.12) | 38 sheet-only rows wiped (27 re-derived unreviewed, 11 never re-derived); destruction confirmed by editor | destructive tab ops | **functionally closed Rev 8 — all 27 re-derived rows re-reviewed/re-approved; residual: 11 never-re-derived IDs; rewrite-guard hazard closed by Tranche A (PR #6, merged)** |
| HLC status-badge gate failure (§3.13) | staging-only HLC institution-page suppression had no gate-side mirror; 3 new badge candidates failed the missing-override check in publish | staging/gate consistency | **fixed (`d57bff8d2`, on main); regression test pins the production IDs** |

## 7. Proposed Phase 1 ordering (Rev 2 — reordered after forensics)

**Implementation status (Rev 9; merged Rev 10, `4d775a181`):** items 1 and 2
are implemented and live on `main`: staging Sheet-appends moved after
commit-and-push via `scripts/sync_review_sheet_appends.R` (a dead run
never touches the Sheet; failed appends stay CSV-only and self-heal);
rewrite scripts abort by default on decision loss
(`--force-discard-decisions` is the valve); the stale dropper
quarantines decision rows to a committed `review_quarantine.csv` and
logs unconditionally with sample ids; apply-only gates print sample ids
and warn above 20 ignored rows; header-order verification guards every
append. The Issue-based escalation channel stays with item 7.

1. **(a/b) Fix the append-then-fail loop (§3.1, proven root cause of the
   orphan class; §3.9 proves it destroys editorial work):** move the staging
   Sheet-append after the CSV commit lands, or gate it on the run reaching
   its final step. In the same change: tab-rewrite scripts (manual tools —
   §3.11) and the stale-row dropper must never silently discard rows with a
   non-empty review decision; dropper message prints unconditionally; large
   apply-only `drop_unlisted` counts trip the drift reporter. Keep
   `--allow-editor-added-rows` as the recovery valve.
   **Fail-loudly semantics (pinned, Rev 5): the run's exit code stays 0 and
   weekly refresh always completes.** A guard trip quarantines/skips only
   the offending row(s) — written to a committed quarantine CSV, left
   untouched in the Sheet — and raises the alert through the escalation
   channel from item 7 (auto-filed/updated GitHub Issue), never as an abort
   and never as only a console line.
2. **(b) durability:** header-order verification before every `sheet_append`;
   refuse to append into a mismatched tab (§3.2, proven).
3. **(a) bug fix / durability:** shared push script + one concurrency
   umbrella; publish gains retry + conflict handling (§2b — the unstaged-file
   class has fired twice; the race is latent but unguarded).
4. **(b) Harden + document the absorption layers (§1a/§1d):** make
   suppression fail closed (label-similarity check before folding;
   one-directionalize `notice↔monitoring`; never fold text into an approved
   row without flipping it to unreviewed); log every suppression into the
   drift report; document both layers in OPERATIONS_MANUAL as load-bearing.
5. **(b):** tombstone semantics instead of row deletion in
   `editorial_overrides.csv` (scope filter and teach-out cleanup mark rows
   inactive rather than removing them).
6. **(b, downgraded Rev 7) determinism — §3.10:** make candidate generation
   deterministic from committed data (persist text-derived classification
   into the filtered CSV at DAPIP-build time; stop reading the gitignored
   text cache at candidates/export time, or read it identically in both).
   No action is currently stranded (cache-present count = 0), but the same
   commit provably produces different candidates/exports per machine, and
   the publish runner has no accreditation cache — so publish-side rebuilds
   run in the divergent environment. Reproducibility fix, reduced
   priority.
7. **(b) alerting:** drift → auto-filed GitHub Issue + fail-after-N-weeks
   state; add DAPIP / cuts-API / Grant Witness 0-row guards to the grep
   contract (or better: replace log-grep with a structured drift-report
   file); split the gate's "ignoring N" message into by-design withholding
   vs anomalies, with sample IDs.
8. **(c) simplification:** unify the two pull scripts' shape; then the
   accreditation/cuts helper stacks where behavior is provably identical.
9. **(c):** publish stops regenerating artifacts it cannot build correctly
   (code-coverage CSVs; §3.5 — degradation is future + cosmetic, verified:
   zero degraded commits so far, main's copy healthy, no site/test consumer).
10. **(deferred, HIGH RISK):** durable `action_id` migration — only if the
    hardened matcher's residual gaps (§1d) prove noisy in practice. Ships
    with a migration table for Sheet + CSV or not at all.
