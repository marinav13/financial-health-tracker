# College Cuts Discovery — Coding Plan (Codex handoff)

**Date:** 2026-07-13
**Status:** Approved design; implementation not started
**Why:** The CollegeCuts public API (`college-cuts.com/api/cuts`) went down
2026-07-12 and the pipeline now runs on its cache (commit `3522d55d4`).
Hechinger is building its own cuts *discovery* front end. Everything
downstream of discovery — review sheet, editorial overrides, unitid
matching, exports — already exists and MUST NOT change its contracts.

**Companion:** the sourcing rationale (tiers, editorial standards) was
approved in chat 2026-07-13; this file is the implementation contract.
Code chunks below are **suggested shapes, not final code** — adapt to what
you find in the repo, but keep the contracts and invariants exact.

## Decisions already made (do not re-litigate)

1. **Rules pre-filter + Claude model final read.** Keyword rules cut the
   noise cheaply; a Claude API call (cheapest current model, Haiku) makes
   the final "is this an announced cut, and fill in the fields" call.
   Requires a new `ANTHROPIC_API_KEY` secret in the private repo.
2. **Wide net first.** Tune for recall; Marina prefers rejecting junk rows
   over missing cuts. Precision telemetry (per `row_origin`) is the dial.
3. **Hiring freezes / furloughs become a first-class category.** NOTE:
   `js/cuts.js` already recognizes a `hiring_freeze` type token (lines
   ~191, ~314) and buckets it under "Staff layoffs / furloughs" — so
   Tranche A starts with an audit of the real end-to-end vocabulary, not a
   blind addition. See Tranche A.
4. **Watchlist of 100 institutions**: live accreditation action + already
   in the cuts tracker + fill ranked by worst 5-year enrollment change
   (Option B, decided 2026-07-13; exact recipe in D1).
5. Nothing publishes without Marina's approval. The model only pre-sorts;
   discovered rows are staged `unreviewed` exactly like API rows were.
6. Discovery failures must never fail the weekly refresh (mirror the
   `--allow-partial-accreditation` philosophy): warn, skip, self-heal.
7. **Higher Ed Dive's layoffs-tracker page is NOT a source** (decided
   2026-07-13). Their articles arrive through their normal RSS feed like
   any other trade outlet; do not scrape the tracker page.

## Fixed integration contracts (read these files first)

- **Candidate CSV** `data_pipelines/college_cuts/college_cuts_review_candidates.csv`
  — header (do not change):

  ```
  cut_id,unitid,institution_name,state,announcement_date,announcement_year,cut_type,program_name,generated_cut_label,generated_cut_summary,source_url,source_title,source_publication,row_origin
  ```

- **`cut_id` for discovered rows.** Existing cached rows use
  `make_cached_cut_id()` (`scripts/build_college_cuts_join.R` ~line 443):
  `cached-<row_index>-<slug>`. The row-index component is order-dependent,
  which is fine for a frozen cache but wrong for weekly re-discovery. Use
  a content-only recipe with a distinct prefix so collision with existing
  ids is impossible:

  ```python
  import hashlib

  def discovered_cut_id(unitid: str, cut_type: str,
                        announcement_date: str, source_url: str) -> str:
      base = "|".join([
          str(unitid or ""), cut_type or "",
          announcement_date or "", normalize_url(source_url or ""),
      ])
      return "discovered-" + hashlib.sha1(base.encode("utf-8")).hexdigest()[:16]
  ```

  Same inputs → same id on every run. Never include mutable text
  (headline, summary) in the hash.

- **Staging** — `scripts/stage_college_cuts_review.R` +
  `scripts/sync_review_sheet_appends.R --pipeline cuts` consume the
  candidate CSV and append new rows to the `college_cuts_review` sheet
  tab. Do not modify their sheet-side behavior; new `row_origin` values
  flow through as data.
- **Name → unitid** —
  `data_pipelines/college_cuts/supabase_institution_unitid_mapping.csv`
  plus `manual_aliases.csv`, normalization in
  `scripts/shared/name_normalization.R`. The Python side reuses the
  mapping CSVs; port the normalization minimally and add a parity fixture
  test (~20 known names, R output vs Python output).
- **Sheet columns** (append-only; order enforced by
  `assert_review_sheet_header_order`): `cut_id | unitid | institution_name
  | state | announcement_date | announcement_year | cut_type |
  display_categories | edited_cut_text | raw_cut_text | source_url |
  source_publication | row_origin | first_seen | review_status | reviewer
  | reviewer_notes | reviewed_at | grandfathered`.
- **Python conventions**: stdlib-first (`requirements.txt` is nearly
  empty); tests are plain scripts run directly (`python
  tests/test_x.py`) and listed one per line in the `python-tests` job of
  `.github/workflows/tests.yml`. Parse RSS with `xml.etree`, fetch with
  `urllib.request`, call Claude with raw REST. If a real dependency is
  clearly justified, add it to `requirements.txt` and say so in the PR.

## Repo ground rules (same as Phase 1)

- Work on wip branches; per-fix commits; PRs to main.
- Protected-path write protocol from `CLAUDE.md` applies (no Edit-tool
  writes to R files or files > 5 KB; write via temp file + verify
  `wc -c` / null bytes / trailing newline).
- Never change `cut_id`/`action_id` derivation for existing rows.
- Every tranche lands with tests; run `npm run test:smoke` and
  `Rscript ./tests/run_shared_helper_smoke_tests.R` before each PR.
- New committed CSVs must be added to the refresh workflow's staged-file
  list (see the stage-list test in `tests/test_refresh_workflows.js`;
  grep for `college_cuts_review_candidates.csv` to find both spots).

---

## New subsystem layout

```
scripts/cuts_discovery/
  __init__.py            (empty; package marker)
  common.py              lead schema, id hashing, CSV io, url normalization,
                         polite fetcher (UA, per-host rate limit, disk cache)
  harvest_feeds.py       Tier 1: trade-press RSS feeds
  harvest_google_news.py Tier 2: standing-query + watchlist-query Google News RSS
  harvest_warn.py        Tier 3: state WARN notice aggregation
  build_watchlist.py     the 100-institution watchlist
  filter_rules.py        include/exclude keyword pre-filter + name→unitid match
  classify_leads.py      Claude API final read (structured JSON out)
  assemble_candidates.py cluster, suppress known, emit discovered candidates CSV
  run_discovery.py       orchestrator: harvest → filter → classify → assemble
config/
  cuts_feeds.yml         feed URLs + parse hints (Tier 1)
  cuts_queries.yml       Google News standing queries + keyword/kill lists
data_pipelines/college_cuts/discovery/
  leads.csv              append-only lead log (committed)
  classifications.csv    cached model verdicts keyed by lead_id (committed)
  watchlist.csv          regenerated weekly (committed)
  cache/                 fetched HTML/article text (gitignored; actions/cache)
data_pipelines/college_cuts/discovered_cut_candidates.csv   (committed)
```

Weekly data flow, before the existing cuts staging step:

```
harvest_* → leads.csv → filter_rules → classify_leads → assemble_candidates
  → discovered_cut_candidates.csv
  → build_college_cuts_join.R unions it into college_cuts_review_candidates.csv
  → existing staging/sheet/overrides flow (unchanged)
```

### Lead schema (`leads.csv`, append-only)

```
lead_id,first_seen,tier,query_or_feed,url,publisher,headline,published_date,snippet,status,status_reason
```

- `lead_id` = `sha1(normalize_url(url))[:16]`
- `tier` ∈ `trade_feed | google_news | warn | watchlist_feed`
- `status` ∈ `new | filtered_out | classified | candidate | suppressed | error`

Harvesters skip known `lead_id`s. This file is the audit trail for "why
did/didn't X show up."

```python
# common.py (suggested shape)
import csv, hashlib, io, json, time, urllib.parse, urllib.request
from pathlib import Path

DISCOVERY_DIR = Path("data_pipelines/college_cuts/discovery")
LEADS_CSV = DISCOVERY_DIR / "leads.csv"
LEAD_FIELDS = [
    "lead_id", "first_seen", "tier", "query_or_feed", "url", "publisher",
    "headline", "published_date", "snippet", "status", "status_reason",
]
USER_AGENT = "HechingerFinancialHealthTracker/1.0 (+https://financialtracker.hechingerreport.org/)"

TRACKING_PARAMS = {"utm_source", "utm_medium", "utm_campaign", "utm_term",
                   "utm_content", "fbclid", "gclid"}

def normalize_url(url: str) -> str:
    parts = urllib.parse.urlsplit(url.strip())
    query = urllib.parse.parse_qsl(parts.query, keep_blank_values=True)
    query = [(k, v) for k, v in query if k.lower() not in TRACKING_PARAMS]
    return urllib.parse.urlunsplit((
        parts.scheme.lower(), parts.netloc.lower(), parts.path.rstrip("/"),
        urllib.parse.urlencode(query), "",
    ))

def lead_id_for(url: str) -> str:
    return hashlib.sha1(normalize_url(url).encode("utf-8")).hexdigest()[:16]

def read_known_lead_ids() -> set:
    if not LEADS_CSV.exists():
        return set()
    with LEADS_CSV.open(encoding="utf-8", newline="") as fh:
        return {row["lead_id"] for row in csv.DictReader(fh)}

def append_leads(rows: list) -> int:
    """Append new lead dicts; returns count written. LF-only, always ends
    with a newline (pre-commit hook requirement)."""
    new = [r for r in rows if r["lead_id"] not in read_known_lead_ids()]
    if not new:
        return 0
    write_header = not LEADS_CSV.exists()
    buf = io.StringIO()
    writer = csv.DictWriter(buf, fieldnames=LEAD_FIELDS, lineterminator="\n")
    if write_header:
        writer.writeheader()
    for row in new:
        writer.writerow(row)
    with LEADS_CSV.open("a", encoding="utf-8", newline="") as fh:
        fh.write(buf.getvalue())
    return len(new)

class PoliteFetcher:
    """One fetcher for every harvester: UA header, per-host spacing,
    on-disk cache so reruns and tests don't refetch."""
    def __init__(self, cache_dir: Path, min_interval_s: float = 2.0):
        self.cache_dir = cache_dir
        self.min_interval_s = min_interval_s
        self._last_hit = {}

    def get(self, url: str, max_age_days: float = 6.0) -> bytes:
        key = hashlib.sha1(normalize_url(url).encode()).hexdigest()
        cached = self.cache_dir / f"{key}.body"
        if cached.exists():
            age_days = (time.time() - cached.stat().st_mtime) / 86400
            if age_days <= max_age_days:
                return cached.read_bytes()
        host = urllib.parse.urlsplit(url).netloc
        wait = self.min_interval_s - (time.time() - self._last_hit.get(host, 0))
        if wait > 0:
            time.sleep(wait)
        req = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
        with urllib.request.urlopen(req, timeout=30) as resp:
            body = resp.read()
        self._last_hit[host] = time.time()
        self.cache_dir.mkdir(parents=True, exist_ok=True)
        cached.write_bytes(body)
        return body
```

### Classification cache (`classifications.csv`)

```
lead_id,classified_at,model,is_cut,confidence,institution_name_raw,unitid,state,cut_type,announcement_date,scale_text,summary,notes
```

A lead is never re-sent to the API once a verdict is cached.

---

## Tranche A — vocabulary audit + plumbing (no harvesters yet)

### A1. Cut-type vocabulary audit, then the freeze/furlough category

`js/cuts.js` already treats `hiring_freeze` as a known token. First map
the real vocabulary end to end:

1. Enumerate every distinct `cut_type` in
   `data_pipelines/college_cuts/editorial_overrides.csv` and the cached
   API snapshot.
2. Grep every validation/display site: `build_college_cuts_join.R`
   (candidate validation), `build_web_exports.R` / `export_helpers.R`
   (display_categories derivation), `js/cuts.js` and `js/school.js`
   (rendering, filters), tests.
3. Decision rule: if `hiring_freeze` is already an accepted value through
   the whole chain, **reuse it** and extend it to cover furloughs at the
   classification layer (the model maps "furlough" stories →
   `hiring_freeze`); only if it is *not* accepted end-to-end, add one new
   value `hiring_freeze_or_furlough` everywhere. Either way the visible
   bucket stays "Staff layoffs / furloughs". Document the outcome in the
   PR description.

Example of the JS-side check to keep in sync (from `js/cuts.js` ~314):

```js
if (["staff_layoff", "faculty_layoff", "hiring_freeze"].includes(normalizedType)) return ["Staff layoffs / furloughs"];
```

### A2. Package scaffolding

Create `scripts/cuts_discovery/` with `common.py` (above), empty configs,
`data_pipelines/college_cuts/discovery/` with a `.gitignore` containing
`cache/`.

### A3. Candidate-merge plumbing in R

Extend `build_college_cuts_join.R` after the API/cache candidate assembly.
Suggested shape:

```r
merge_discovered_candidates <- function(candidates, discovered_path) {
  if (!file.exists(discovered_path)) {
    message("No discovered cuts file at ", discovered_path, " — skipping.")
    return(candidates)
  }
  discovered <- readr::read_csv(discovered_path, col_types = readr::cols(.default = "c"))
  expected <- c(
    "cut_id", "unitid", "institution_name", "state", "announcement_date",
    "announcement_year", "cut_type", "program_name", "generated_cut_label",
    "generated_cut_summary", "source_url", "source_title",
    "source_publication", "row_origin"
  )
  if (!identical(names(discovered), expected)) {
    warning("discovered_cut_candidates.csv header mismatch — ignoring file this run.")
    return(candidates)
  }
  # API/cache rows win on any cut_id collision (should not happen given the
  # "discovered-" prefix, but belt and braces).
  discovered <- dplyr::anti_join(discovered, candidates, by = "cut_id")
  # Rows the assembler could not resolve to a unitid go to the existing
  # unmatched-for-review output, never silently dropped.
  unmatched <- dplyr::filter(discovered, is.na(unitid) | !nzchar(unitid))
  if (nrow(unmatched) > 0L) {
    append_unmatched_for_review(unmatched)  # reuse the existing writer
  }
  dplyr::bind_rows(candidates, dplyr::filter(discovered, nzchar(unitid)))
}
```

Wire it in with a `--discovered-cuts` CLI arg defaulting to
`data_pipelines/college_cuts/discovered_cut_candidates.csv`, following the
existing `get_arg_value` pattern in that script.

### A4. Tests

R fixture test (follow `tests/test_college_cuts_pipeline_fixture.R`
style): synthetic discovered CSV with (1) one clean new row, (2) one
`cut_id` collision, (3) one row without unitid → expect exactly one new
candidate, collision dropped, unmatched surfaced. Plus: header-mismatch
file → warning + unchanged candidates.

**Acceptance:** refresh green with the file absent (no behavior change);
fixture produces expected candidates; freeze/furlough category renders on
`cuts.html` from a hand-planted fixture row.

## Tranche B — harvesters + rules filter (rules-only end to end)

### B1. Config files

```yaml
# config/cuts_feeds.yml
feeds:
  - name: higher_ed_dive
    url: https://www.highereddive.com/feeds/news/
    tier: trade_feed
  - name: inside_higher_ed
    url: https://www.insidehighered.com/rss.xml
    tier: trade_feed
  - name: chronicle
    url: https://www.chronicle.com/section/news/rss
    tier: trade_feed
```

(Verify each URL by hand before committing — feed paths move.)

```yaml
# config/cuts_queries.yml
standing_queries:
  - '"university" ("layoffs" OR "laid off" OR "eliminating positions")'
  - '"college" ("program cuts" OR "programs eliminated" OR "majors cut")'
  - 'university ("hiring freeze" OR furloughs)'
  - 'college ("campus closing" OR "campus closure" OR "teach-out")'
  - 'university "budget deficit" ("layoffs" OR "cuts")'
include_keywords:
  - layoff
  - laid off
  - eliminat        # eliminate/eliminating/elimination
  - suspend
  - discontinu
  - furlough
  - hiring freeze
  - closure
  - closing
  - teach-out
kill_patterns:
  - '\b(football|basketball|roster|athletic)\b'   # sports "cuts"
  - '\bschool district\b'                          # K-12
  - '\b(op-ed|opinion|letters? to the editor)\b'
  - '\bproposed budget\b(?![^.]*approv)'           # proposals w/o action
```

### B2. Google News harvester

```python
# harvest_google_news.py (suggested shape)
import urllib.parse
import xml.etree.ElementTree as ET
from datetime import datetime, timezone

from common import PoliteFetcher, lead_id_for, append_leads

def google_news_rss_url(query: str) -> str:
    return ("https://news.google.com/rss/search?q=" +
            urllib.parse.quote(query + " when:7d") +
            "&hl=en-US&gl=US&ceid=US:en")

def harvest_query(fetcher: PoliteFetcher, query: str, tier: str) -> list:
    body = fetcher.get(google_news_rss_url(query), max_age_days=0.9)
    root = ET.fromstring(body)
    leads = []
    for item in root.iter("item"):
        url = (item.findtext("link") or "").strip()
        if not url:
            continue
        leads.append({
            "lead_id": lead_id_for(url),
            "first_seen": datetime.now(timezone.utc).date().isoformat(),
            "tier": tier,
            "query_or_feed": query,
            "url": url,
            "publisher": (item.findtext("source") or "").strip(),
            "headline": (item.findtext("title") or "").strip(),
            "published_date": (item.findtext("pubDate") or "").strip(),
            "snippet": (item.findtext("description") or "").strip()[:500],
            "status": "new",
            "status_reason": "",
        })
    return leads
```

Notes: Google News `link`s are redirect URLs — resolve to the final
article URL before hashing when feasible (follow one redirect with the
fetcher; on failure, keep the Google URL and note it). Standing queries
run for everyone; per-institution queries run for the 100 watchlist rows
(Tranche D wires the real watchlist; until then read a hand-seeded
`watchlist.csv` if present).

### B3. Trade feeds

`harvest_feeds.py` parses the configured RSS feeds into the same lead
schema as the Google News harvester (`tier=trade_feed`).

### B4. Rules filter + name matching

```python
# filter_rules.py (suggested shape)
import csv, re
from pathlib import Path

def load_mapping():
    """institution name (normalized) -> (unitid, state).
    Reuses the pipeline's own mapping + alias CSVs — do NOT re-derive."""
    mapping = {}
    base = Path("data_pipelines/college_cuts")
    for fname, name_col, unitid_col in [
        ("supabase_institution_unitid_mapping.csv", "institution_name_api", "unitid"),
        ("manual_aliases.csv", "alias", "unitid"),
    ]:
        with (base / fname).open(encoding="utf-8", newline="") as fh:
            for row in csv.DictReader(fh):
                mapping[normalize_name(row[name_col])] = row[unitid_col]
    return mapping

def normalize_name(name: str) -> str:
    # Port the rules from scripts/shared/name_normalization.R minimally;
    # add tests/test_cuts_discovery_name_parity.py proving parity on ~20
    # names against a fixture generated from the R implementation.
    ...

def filter_lead(lead: dict, cfg: dict, mapping: dict) -> dict:
    text = f"{lead['headline']} {lead['snippet']}".lower()
    if not any(k in text for k in cfg["include_keywords"]):
        return {**lead, "status": "filtered_out", "status_reason": "no_include_keyword"}
    for pattern in cfg["kill_patterns"]:
        if re.search(pattern, text, flags=re.IGNORECASE):
            return {**lead, "status": "filtered_out", "status_reason": f"kill:{pattern[:30]}"}
    return lead  # survives; institution resolution happens at classify time
```

(YAML configs: parse with a ~30-line stdlib reader for the simple
key/list subset used here, or add `pyyaml` to requirements.txt — CI
already installs it for other jobs; check first. State the choice in the
PR.)

### B5. Rules-only candidate assembly

`assemble_candidates.py` clusters survivors by
`(unitid_guess, cut_type_guess, published_date ± 14 days)`, suppresses
clusters matching existing tracker rows (same unitid + type + date window
against `editorial_overrides.csv` and the current candidates CSV), and
emits `discovered_cut_candidates.csv` with:

- `cut_id` from `discovered_cut_id(...)`
- `row_origin` per tier: `news_scan` (feeds + Google News), `warn_notice`
- `generated_cut_summary` = best headline (until Tranche C)
- best source = trade press > local > student; losing URLs preserved in
  `generated_cut_label` suffix `" [also: url1; url2]"`

In rules-only mode, unresolved institution names mean the row goes out
with empty `unitid` — the R merge routes it to unmatched-for-review, which
is the correct wide-net behavior (a human looks at it).

### B6. Orchestrator + workflow step

`run_discovery.py` runs harvest → filter → (classify if key present) →
assemble, prints one summary line per tier
(`cuts_discovery: google_news leads=137 new=27 survivors=9`), and exits 0
even when a tier fails (print `::warning::` and continue; exit non-zero
only if *every* tier fails).

Workflow (`.github/workflows/refresh-ipeds-site-data.yml`, before the
cuts join step; mirror the publish step's secret-gating pattern):

```yaml
      # Discovers cut candidates from news feeds/queries. Failures warn
      # and skip — discovery must never take down the refresh. The Claude
      # classification step inside activates only when ANTHROPIC_API_KEY
      # is configured (Tranche C); otherwise rules-only.
      - name: Discover college cuts
        shell: bash
        timeout-minutes: 20
        env:
          ANTHROPIC_API_KEY: ${{ secrets.ANTHROPIC_API_KEY }}
        run: |
          python3 scripts/cuts_discovery/run_discovery.py \
            2>&1 | tee -a refresh-logs/combined.log \
            || echo "::warning::cuts discovery failed; staging continues without new discovered candidates."
```

Plus: add `data_pipelines/college_cuts/discovery/cache` to the existing
actions/cache step for scraped content (or its own cache step keyed like
the accreditation one); add `discovery/leads.csv`,
`discovery/classifications.csv`, `discovery/watchlist.csv`, and
`discovered_cut_candidates.csv` to the run's `git add -f` stage list; add
a drift marker when every tier returns zero leads
(`PIPELINE DRIFT WARNING: cuts_discovery`) — zero leads across all tiers
means harvesting is broken, not that news was quiet.

### B7. Tests (fixture-based; no network in CI)

- `tests/test_cuts_discovery_harvest.py` — recorded RSS XML fixtures →
  expected leads; dedupe on second run (same fixture → 0 new).
- `tests/test_cuts_discovery_filter.py` — include/kill behavior on a
  table of headlines (sports story dies, layoff story survives).
- `tests/test_cuts_discovery_ids.py` — `lead_id`/`discovered_cut_id`
  stability (same input → same id; tracking params stripped).
- `tests/test_cuts_discovery_name_parity.py` — normalization parity vs an
  R-generated fixture.
- Register each in `.github/workflows/tests.yml` `python-tests` (one line
  per file, matching the existing pattern) and in `package.json`'s
  `test:smoke` if the existing python tests are chained there (check).
- Extend `tests/test_refresh_workflows.js`: discovery step exists, has a
  bounded timeout, is warn-gated; new CSVs staged.

**Acceptance:** local run from fixtures produces a plausible discovered
CSV; a supervised live run stages real candidates to the sheet as
`unreviewed`; weekly refresh green with the new step; leads.csv audit
trail explains every filtered lead.

## Tranche C — Claude classification + clustering quality

### C1. The API call (raw REST, stdlib)

```python
# classify_leads.py (suggested shape)
import json, os, urllib.request

MODEL = os.environ.get("CUTS_CLASSIFIER_MODEL", "claude-haiku-4-5-20251001")

PROMPT_TEMPLATE = """You are screening news items for a tracker of budget cuts at U.S. four-year colleges and universities.

Item headline: {headline}
Publisher: {publisher}
Published: {published_date}
Article text (may be truncated):
{article_text}

Answer in strict JSON only, no prose, matching exactly this schema:
{{
  "is_cut": true/false,          // an ANNOUNCED or ENACTED cut at a specific U.S. degree-granting college/university (not K-12, not a proposal still under debate, not sports rosters)
  "confidence": "high"|"medium"|"low",
  "institution_name": "official institution name or null",
  "state": "two-letter state code or null",
  "cut_type": "staff_layoff"|"program_suspension"|"department_closure"|"campus_closure"|"institution_closure"|"hiring_freeze"|"other",
  "announcement_date": "YYYY-MM-DD or null (date the cut was announced, not the article date, if stated)",
  "scale_text": "brief scale, e.g. '42 positions' or '6 degree programs', or null",
  "summary": "one to two factual sentences describing the cut, suitable for an editor to review"
}}

Furloughs and hiring freezes both map to "hiring_freeze". If the story
covers multiple institutions, answer for the most prominent one and note
the others in summary. If not a cut, set is_cut=false and leave the other
fields null."""

def classify(lead: dict, article_text: str) -> dict:
    body = json.dumps({
        "model": MODEL,
        "max_tokens": 500,
        "messages": [{"role": "user", "content": PROMPT_TEMPLATE.format(
            headline=lead["headline"], publisher=lead["publisher"],
            published_date=lead["published_date"],
            article_text=article_text[:4000],
        )}],
    }).encode("utf-8")
    req = urllib.request.Request(
        "https://api.anthropic.com/v1/messages",
        data=body,
        headers={
            "x-api-key": os.environ["ANTHROPIC_API_KEY"],
            "anthropic-version": "2023-06-01",
            "content-type": "application/json",
        },
    )
    with urllib.request.urlopen(req, timeout=60) as resp:
        payload = json.loads(resp.read())
    return parse_verdict(payload["content"][0]["text"])  # strict; see C2
```

### C2. Strictness and failure handling

- `parse_verdict` validates every field against the schema (enum
  membership, date shape); malformed → one retry with "Your previous
  answer was not valid JSON matching the schema; answer again" → still
  bad → `status=error`, lead held for next week. **Never drop a lead on
  API failure.**
- No `ANTHROPIC_API_KEY` in env → classification step logs
  `ANTHROPIC_API_KEY is not set; running rules-only.` and the pipeline
  degrades to Tranche-B behavior (copy the `PUBLIC_SITE_DEPLOY_TOKEN`
  skip-with-notice pattern).
- Verdicts cached in `classifications.csv` by `lead_id`; cache hit → no
  API call. Budget guard: hard cap (e.g. 400 classifications/run) with a
  `::warning::` if hit.
- Article text extraction: fetch survivor URLs via `PoliteFetcher`, strip
  tags crudely (stdlib `html.parser`, drop script/style/nav), take first
  ~4,000 chars. Perfect extraction is not required — the model tolerates
  messy text; the headline+snippet fallback (fetch failed) is acceptable
  and recorded in `notes`.

### C3. Model-mode assembly

- Only `is_cut=true` becomes a candidate — at ANY confidence (wide-net
  decision); `confidence` and `scale_text` are embedded in
  `generated_cut_label` so Marina sees them on the sheet
  (e.g. `staff_layoff — 42 positions [model: high confidence]`).
- `institution_name` resolves through the same mapping/normalization as
  B4; unresolved → empty unitid → unmatched-for-review.
- `cut_type=other` + `is_cut=true` → still staged (Marina decides), with
  `generated_cut_label` prefixed `UNCLASSIFIED TYPE:`.
- Wayback archiving: per emitted candidate, best-effort POST to the
  Save-Page-Now endpoint for `source_url`; cache archived URLs in the
  discovery cache; failures logged, never fatal.

### C4. Tests

- `tests/test_cuts_discovery_classify.py` — mock `urllib.request.urlopen`
  (recorded response payloads): happy path, malformed-JSON retry path,
  missing-key skip path, cache-hit path (asserts zero HTTP calls).
- Golden test: 10 fixture articles (mix of true cuts, sports noise,
  proposal-only stories) with recorded verdicts → assembled candidates
  match a checked-in expected CSV byte for byte.

**Acceptance:** with the key set, a supervised live run classifies a
week's survivors for well under a dollar and sheet rows carry
model-written summaries; without it, Tranche-B behavior with a notice.

## Tranche D — WARN, watchlist, telemetry

### D1. Watchlist builder

```python
# build_watchlist.py (suggested shape)
# 100 = (a) live tracked accreditation action  ∪  (b) already in cuts tracker,
# then fill remaining slots from the bottom of the composite ranking.
import json, csv

def build(n=100):
    accred = json.load(open("data/accreditation_index.json", encoding="utf-8"))
    with_actions = {str(r["unitid"]) for r in iter_index_rows(accred) if r.get("has_active_action")}
    in_tracker = {row["unitid"] for row in csv.DictReader(
        open("data_pipelines/college_cuts/editorial_overrides.csv", encoding="utf-8"))
        if row.get("review_status") == "approved"}
    # Fill ranking (Option B, decided 2026-07-13): worst 5-year enrollment
    # change first, tie-broken by the federal composite score where present
    # (private institutions only; blank for publics), then unitid for
    # determinism. Fields live in data/downloads/full_dataset.csv:
    #   enrollment_pct_change_5yr         ascending (most negative = worst)
    #   federal_composite_score_2022_2023 ascending (lower = worse)
    ranked = sorted(read_full_dataset_rows(), key=lambda s: (
        as_float(s.get("enrollment_pct_change_5yr"), default=0.0),
        as_float(s.get("federal_composite_score_2022_2023"), default=99.0),
        s["unitid"],
    ))
    picked, seen = [], set()
    for pool in ({**{}}, ):  # pseudo: with_actions ∪ in_tracker first, then ranked fill
        ...
    return picked[:n]
```

(The exact JSON field names must be read from the real files — the chunk
above is the priority logic, not the parsing.) Emit
`discovery/watchlist.csv` (`unitid,institution_name,state,reason`),
regenerated weekly *after* the accreditation build so (a) is current.
Per-institution Google News queries and future newsroom feeds read it.

### D2. WARN notices

`harvest_warn.py`: start with 3–5 states that publish machine-readable
WARN lists (CA, NY, WA are historically the most usable) plus one
aggregator; each source gets a small parser with a shape-change guard.
Filter employers to higher-ed via the mapping CSVs (normalized-name
containment: "Trustees of X University" must match "X University").
Employee counts → `scale_text`; `row_origin=warn_notice`. Document each
state's URL and fragility in `config/cuts_feeds.yml` comments.

### D3. Precision telemetry

After the sheet-pull step, compute per-`row_origin` outcomes from the
pulled overrides snapshot and print greppable lines:

```
CUTS DISCOVERY PRECISION: news_scan approved=6 rejected=11 pending=4
CUTS DISCOVERY PRECISION: warn_notice approved=2 rejected=0 pending=1
```

That's the wide-vs-tight dial. No thresholds/alerts yet — just the lines.

### D4. Deferred (do not build)

GDELT doc + TV APIs, watchlist newsroom RSS, board-minutes monitoring,
curated student-paper feeds. Listed for the roadmap only.

**Acceptance:** watchlist regenerates deterministically (stable ordering,
ties broken by unitid); WARN leads carry counts; precision lines appear
after one review cycle.

---

## Explicitly out of scope

- Changing how existing CollegeCuts-derived rows are stored, displayed, or
  attributed (CollegeCuts attribution stays for its rows).
- Any change to review-sheet columns, tab names, or pull/append semantics.
- Removing the CollegeCuts API path — keep it; if the API returns, it's
  one more tier. Its cache fallback (commit `3522d55d4`) stays.
- Front-end redesign of cuts.html beyond the category label work in A1.

## Open items for the user (flag in PR if hit)

- `ANTHROPIC_API_KEY` is set (2026-07-13) with a personal key so Tranche C
  is unblocked. Later, rotate to a fresh Hechinger-billed key — a
  one-secret swap; the current key should be rotated regardless of the
  billing move.
