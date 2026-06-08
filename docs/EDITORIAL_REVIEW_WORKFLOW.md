# Editorial Review Workflow â€” Plan

Status: implemented with a manual publish step. The old sheet-driven
auto-dispatch path is retired and any references to it below are historical
unless explicitly marked as archival.

---

## 1. The problem

Today the weekly Action (`.github/workflows/refresh-ipeds-site-data.yml`,
Mondays at noon) runs scrape â†’ join â†’ `build_web_exports.R` â†’ commits
`data/*.json` â†’ the site updates. The review gate now keeps new accreditation
and cuts rows out of the public exports until a human approves them.

We want: new rows land in a Google Sheet for human review first; the site only
shows rows a human has approved; approved rows can be published the same day by
running a manual workflow; editors can copy-edit anytime; we can add our own
rows; and a wrong field already on the site can be corrected within minutes.

## 2. Design principles

These come from the earlier Codex discussion and hold up against how this repo
already works:

- **The Google Sheet is the editorial queue / workflow UI. It is NOT the system
  of record.** Production must not depend on someone free-typing in Sheets.
- **A git-tracked override CSV is the system of record.** This repo already does
  exactly this â€” `data_pipelines/grant_witness/manual_include.csv` and
  `manual_match_overrides.csv` are committed CSVs that `build_grant_witness_join.R`
  merges in as a controlled layer. We extend that pattern, we don't invent a new one.
- **A stable `action_id` is the linchpin.** Without it, row matching breaks every
  time scraped text shifts slightly.
- **The scrapers and `derive_action_label_short()` stay exactly as they are.**
  The review layer sits *on top* of the generated statement, it does not replace it.

## 3. Timing â€” the review week

The cadence is built around how you want to work: rows in hand Sunday evening,
reviewed and approved through Monday, each approval live the same day.

```
Sunday evening   weekly Action runs: scrape -> full refresh -> stage new rows
                 to the Sheet + CSV -> build WITH gate
                 (new unreviewed rows withheld) -> commit + push
Sunday night /   editors review in the Sheet. Each row flipped to `approved`
Monday           can be published with a manual workflow run -> live the same day
ongoing          editors copy-edit anytime; edits re-publish the same way
```

The only schedule change is moving the cron earlier â€” from `0 12 * * 1`
(Monday noon UTC) to roughly `0 0 * * 1` (Monday 00:00 UTC â‰ˆ Sunday 7â€“8pm US
Eastern). That exact value is the one knob to set for your timezone; note that
GitHub scheduled runs can be delayed under load, so treat it as "Sunday
evening-ish," not a hard clock.

## 4. Architecture: stage â†’ review â†’ gate â†’ publish

One new git-tracked file per dataset is the spine. For the accreditation pilot:

`data_pipelines/accreditation/editorial_overrides.csv`

This file is **both** the system of record the build trusts **and** the memory
of "every action we have ever seen." The Google Sheet is a view onto it.

Two new scripts and two modified/new workflow pieces:

| Piece | Type | Role |
|---|---|---|
| `scripts/stage_accreditation_review.R` | new | Compute `action_id` for every scraped action; find ids not yet in `editorial_overrides.csv`; append them (status `unreviewed`) to both the CSV and the Sheet. |
| `scripts/pull_accreditation_overrides.R` | new | Read the Sheet's editor columns back down; update `editorial_overrides.csv` (Sheet wins for editor-owned columns). |
| `build_web_exports.R` | modified | Join actions to `editorial_overrides.csv` on `action_id`; apply the publish gate; use explicit `editor_*` field overrides when present. |
| `refresh-ipeds-site-data.yml` | modified | Move cron to Sunday evening; insert the stage + pull steps; build runs gated. |
| `publish-editorial-overrides.yml` | new | The same-day publish path: pull Sheet â†’ rebuild â†’ publish, no scraping, ~minutes. Triggered manually with `workflow_dispatch`. |

### Weekly flow (across time)

```
Sunday evening Action run:
  scrape  ->  full refresh  ->  stage new ids to Sheet + CSV
          ->  pull any already-approved decisions
          ->  build_web_exports.R WITH gate (new unreviewed rows withheld)
          ->  commit data/ + editorial_overrides.csv  ->  push

Sunday night / Monday (editors review in the Sheet):
  each row set to `approved` -> maintainer runs Publish Editorial Overrides
    -> row live the same day
```

## 5. `action_id` â€” the stable key

Derive it from normalized identity fields, hashed:

```
action_id = sha1( normalize(unitid) | normalize(accreditor)
                   | normalize(action_date) | normalize(action_label_raw) )[:12]
```

`normalize()` = lowercase, strip punctuation, collapse whitespace. Aggressive
normalization is what keeps the id stable when the scraper returns cosmetically
different text for the same action.

Honest tradeoff: if the *substance* of `action_label_raw` changes, the id
changes and the row returns to the queue as "new." That is arguably correct â€”
changed source text deserves re-review â€” but it does mean some churn. A v2
upgrade is "sticky" matching on `(unitid, accreditor, action_date)` + fuzzy
label, minting a new id only when no plausible prior match exists. Start with
the plain hash; measure churn in Phase 1 before adding complexity.

Editor-added rows (Section 8) get ids like `editor-<short-uuid>` so they never
collide with scraper ids and are never expected to appear in scraper output.

## 6. Google Sheet schema

This worksheet now has one active review tab:

- `accreditation_review`

Only primary-tracker accreditation rows stay in the Sheet review queue. "Other
institutions" accreditation rows and college cuts no longer route through the
Google Sheet. Research grants also remain trusted and do **not** go through an
editorial review queue right now. Administrative teach-out process rows
(accepted/approved teach-out plans, agreements, provisional-plan paperwork,
receiving-institution approvals) are excluded from this queue; substantive
sanction or monitoring rows that mention teach-out requirements still stay in
scope.

The spreadsheet document title can change without affecting the workflow. The
actual contract is the Google Sheet ID/URL plus the tab name `accreditation_review`.

### Accreditation tab

**System columns** â€” written by the pipeline, protected so editors cannot edit
them (the service account can still write):

`action_id`, `first_seen`, `unitid`, `institution_name`, `accreditor`,
`action_date`, `action_type`, `action_label_raw`, `generated_statement`
(the current `derive_action_label_short()` output), `source_url`,
`source_title`, `row_origin` (`scraper` | `manual`), `grandfathered`.

**Editor columns** â€” editors own these:

`review_status` (dropdown: `unreviewed` / `in_review` / `approved` /
`needs_revision` / `reject`), `editor_action_label_short`,
`editor_action_date`, `editor_action_type`, `editor_source_url`,
`editor_source_title`, `editor_notes`, `reviewer`, `reviewed_at`.

Sheet conventions: freeze the header and system columns; protect the system
column range; conditional formatting on `review_status`; saved filter views for
*unreviewed*, *needs_revision*, *approved*; `source_url` rendered as a hyperlink.

The pipeline **only ever** appends new rows and writes system columns. It never
touches an existing row's editor columns. That one-directional rule is what
makes the two-way sync safe.

### College cuts (`college_cuts_review` tab)

College cuts use a separate `college_cuts_review` tab in the same Google Sheet.
New cuts rows for primary-tracker institutions are staged to the sheet by
`stage_college_cuts_review.R` and pulled back via `pull_college_cuts_overrides.R`.

**Sheet columns:**

`cut_id`, `unitid`, `institution_name`, `state`, `announcement_date`,
`announcement_year`, `cut_type`, `cut_description` (the source action
description), `cut_label` (public short label, max 160 chars), `cut_summary`
(public institution-page summary, max 4 sentences), `source_url`,
`source_publication`, `row_origin`, `first_seen`, `review_status`, `reviewer`,
`reviewer_notes` (internal-only workflow notes, never public), `reviewed_at`,
`grandfathered`.

**Lane semantics:**
- `cut_description` = the source action description lane; editors correct
  mislabeled types or descriptions here.
- `cut_label` = the public short label shown in the cuts table and index card.
  Pre-populated from `cut_label_public` derivation (notes fallback for generic
  labels, `program_name` for actionable descriptions). Editors can refine.
- `cut_summary` = the public institution-page paragraph. Pre-populated from
  cleaned `notes` capped to 4 sentences. Editors can refine.
- `reviewer_notes` = internal workflow column; **never routed to public fields**.

**Migration:** Run `scripts/migrate_college_cuts_review_sheet_schema.R` once
to add the new `cut_label` / `cut_summary` columns to an existing sheet.

For college cuts local overrides, `row_origin` is provenance rather than a
publication field. Use `scraper` for pipeline rows, `manual` for generic
human-added rows, and `hechinger` for Hechinger-found rows. Outlet names belong
in `source_publication`, not `row_origin`.

## 7. The publish gate (in `build_web_exports.R`)

A new helper runs just before the accreditation JSON is written:

1. Left-join the joined actions to `editorial_overrides.csv` on `action_id`.
2. Published fields are overlaid explicitly, e.g.
   `action_label_short = coalesce(editor_action_label_short, generated_statement)`,
   `action_date = coalesce(editor_action_date, action_date)`,
   `action_type = coalesce(editor_action_type, action_type)`,
   `source_url = coalesce(editor_source_url, source_url)`,
   `source_title = coalesce(editor_source_title, source_title)`.
3. `is_published = review_status == "approved"`.
4. The public `data/accreditation.json` includes only `is_published` rows,
   plus approved editor-added rows.
5. `reject` rows never publish. `unreviewed` / `in_review` / `needs_revision`
   rows are withheld until approved.

Gate it behind a flag (`--enforce-review-gate`) so it can be turned on
deliberately in Phase 3 and rolled back fast if something is wrong.

### Grandfathering â€” do not skip this

The joined actions file currently has ~7,100 rows. If the gate switches on cold,
every one of them is `unreviewed` and the accreditation table empties out. So
the rollout step that introduces `editorial_overrides.csv` seeds **every action
currently live on the site** with `review_status = approved` and a
`grandfathered = TRUE` flag. Only genuinely new actions after that point pass
through the queue. Editors can still revisit grandfathered rows anytime.

## 8. Same-day publish + hotfix path

New workflow `publish-editorial-overrides.yml`:

```
checkout -> pull_accreditation_overrides.R (Sheet -> editorial_overrides.csv)
         -> build_web_exports.R  (no scraping; uses committed joins + cached IPEDS)
         -> commit + push
```

Runs in a few minutes. It is triggered manually from the Actions tab with
`workflow_dispatch`.

Approved rows do not auto-publish from Google Sheets anymore. After editors
finish a review batch or make an urgent correction to an approved row, a
maintainer should run `Publish Editorial Overrides`.

Editor-added rows: an editor adds a row at the bottom of the Sheet, leaves
`action_id` blank, fills in `institution_name` / `accreditor` / `action_date` /
`action_type` / `source_url` / `editor_action_label_short`, and sets `review_status = approved`.
`pull_accreditation_overrides.R` mints an `editor-<uuid>` id for any
blank-id row and writes it into `editorial_overrides.csv` with
`row_origin = manual`. `build_web_exports.R` publishes approved editor rows even
though no scraper row backs them â€” this covers "news broke on the institution's
site before our scraper saw it."

If a later scraper or DAPIP refresh stages what appears to be the **same**
accreditation action as a previously editor-added row, that new scraper-backed
row should still enter the review queue as a separate `action_id`. The expected
editor workflow is to compare the two rows, confirm whether they are truly the
same action, and then resolve the duplicate deliberately â€” typically by keeping
the scraper-backed row as the long-term record and marking the older editor row
`reject` once its replacement is approved. The system should not try to guess
that match automatically in Phase 1.

Emergency fallback: edit `editorial_overrides.csv` directly in the repo and push.
Functional, but hand-editing CSV is error-prone, so the Sheet path is preferred.

## 9. Auth and secrets

- `GOOGLE_SERVICE_ACCOUNT_JSON_B64` â€” base64-encoded service account JSON stored
  as a GitHub repo secret. The workflow decodes it to a temp file and exports
  `GOOGLE_APPLICATION_CREDENTIALS` for `googlesheets4`. Share the Sheet with the
  service account email as **Editor**.
- `ACCREDITATION_REVIEW_SHEET_ID` â€” the Sheet id (repo secret or workflow env).
- GitHub workflow file that must exist on the repo default branch:
  `publish-editorial-overrides.yml`

## 10. Manual publish

The supported same-day publish path is manual:

1. Editors mark rows `approved` in the Google Sheet.
2. A maintainer runs `Publish Editorial Overrides`.
3. The workflow pulls editor-owned columns from the Sheet, rebuilds exports
   against the committed review snapshots, and publishes only approved rows.

## 11. Rollout phases

| Phase | Deliverable | Site impact |
|---|---|---|
| 0 | Create the Sheet + service account; add the secrets; pick the Sunday cron value; agree the grandfather baseline. | none |
| 1 | `stage_accreditation_review.R` computes `action_id` and writes/maintains `editorial_overrides.csv` only. Verify ids are stable across two consecutive runs; measure real weekly new-row volume. | none |
| 2 | Stage script pushes new rows into the Sheet; move the cron to Sunday evening. Editors start reviewing. | none |
| 3 | `pull_accreditation_overrides.R` + the gate in `build_web_exports.R` behind `--enforce-review-gate`. Grandfather all current live rows as approved. Flip the gate on. | gate live |
| 4 | `publish-editorial-overrides.yml` as a manual same-day publish workflow. | same-day publish live |
| 5 | Editor-added rows (blank-id minting). | editors can add rows |
| 6 | Retired: cuts no longer use a Google Sheet review tab; keep **research** trusted for now. | cuts publish directly; research unchanged |

## 12. Risks and honest caveats

- **`action_id` churn.** Drift in scraped label text re-queues rows. Normalization
  limits it; the sticky-matching upgrade is the fix if churn is high. Measure first.
- **The Sheet becomes a build dependency.** If the pull step can't reach the Sheet
  or the header is broken, it must **fail loudly and fall back to the last
  committed `editorial_overrides.csv`** â€” never "publish everything ungated."
  This is the single most important failure-mode rule.
- **The manual publish step is operational, not automatic.** Approved rows stay
  out of the site until someone runs `Publish Editorial Overrides`.
- **Archived Apps Script references are historical only.** The old dispatch
  automation is not part of the supported production path anymore.
- **Grandfathering.** Get the baseline right in Phase 3 or the accreditation table
  empties out on cutover.
- **Two-way sync.** The pipeline must never overwrite editor columns. Append-only
  + system-columns-only writes is the discipline that keeps it safe.
- **Editor-added row duplication.** A manual row may later reappear from DAPIP or
  a scraper as a brand-new `action_id`. That is expected under the current
  identity model. The safe rule is human review: compare the staged scraper row
  against the existing editor row, approve the canonical replacement, then
  retire the older manual row explicitly instead of trying to auto-merge them.
- **Weekly CSV churn.** `editorial_overrides.csv` grows and changes every week;
  expect sizeable diffs. It is data, not code â€” fine, but worth noting against the
  repo's file-integrity tooling.
- **Volume unknown.** ~7,100 historical rows is a lot, but the *new* actions per
  week is probably small (tens). Phase 1 measures this before anyone commits to a
  review cadence.
