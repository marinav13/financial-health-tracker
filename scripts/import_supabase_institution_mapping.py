#!/usr/bin/env python3
"""
import_supabase_institution_mapping.py

Pulls all institutions from the College Cuts Supabase project and builds a
complete institution-name -> IPEDS unitid mapping, writing it to:
  data_pipelines/college_cuts/supabase_institution_unitid_mapping.csv

Two-pass matching strategy:
  1. Supabase unitids  -- institutions already have unitids set in Supabase.
                         These are used as-is (highest confidence), subject to
                         any overrides in SUPABASE_DATA_CORRECTIONS below.
  2. IPEDS name match  -- For the remaining institutions without Supabase
                         unitids, normalize both the Supabase name and all IPEDS
                         institution names (lowercase, strip punctuation, expand
                         abbreviations) and match on (norm_name, state).  Only
                         unambiguous 1-to-1 matches are accepted.

When Supabase carries a known-wrong unitid or state, add an entry to
SUPABASE_DATA_CORRECTIONS rather than patching the live database.  IPEDS is
treated as authoritative for those corrections.

The output CSV is the authoritative matching source consumed by
build_college_cuts_join.R.  Run this script whenever new institutions are
added to Supabase to keep the mapping up to date.

Usage (no arguments needed -- credentials are hardcoded):
    python scripts/import_supabase_institution_mapping.py

Override via CLI or env vars if needed:
    python scripts/import_supabase_institution_mapping.py \\
        --url "https://<project>.supabase.co" \\
        --key "<anon_key>" \\
        --ipeds-canonical "ipeds/derived/ipeds_financial_health_canonical_2015_2025.csv"
"""

import argparse
import csv
import glob
import io
import json
import os
import re
import urllib.request


OUTPUT_PATH = os.path.join(
    os.path.dirname(__file__),
    "..", "data_pipelines", "college_cuts", "supabase_institution_unitid_mapping.csv"
)
IPEDS_DERIVED_DIR = os.path.join(
    os.path.dirname(__file__),
    "..", "ipeds", "derived"
)
IPEDS_CANONICAL_PATTERN = "ipeds_financial_health_canonical_*.csv"
OUTPUT_COLUMNS = [
    "institution_name_api",
    "unitid",
    "state_full",
    "tracker_institution_name",
    "match_source",   # "supabase" | "ipeds_name_match" | "manual_alias"
]

# ---------------------------------------------------------------------------
# Supabase data-quality corrections
#
# Applied after fetching from Supabase, before the pass-1/pass-2/pass-3 logic.
# Use this when Supabase carries a wrong state or wrong unitid and the live
# database cannot be patched.  IPEDS is the authoritative source for these
# corrections.
#
# Key:   exact api_name string as it appears in Supabase
# Value: dict with any of: state_full (str), unitid (int or None)
#
# Corrections are logged at run time so they are visible in CI output.
# ---------------------------------------------------------------------------
SUPABASE_DATA_CORRECTIONS: dict[str, dict] = {
    # Supabase has state="DC" for WSU -- correct state is Washington.
    "Washington State University": {
        "state_full": "Washington",
    },
    # Supabase unitid 483036 is Texas A&M University-Central Texas.
    # Correct unitid for the flagship (College Station) is 228723.
    "Texas A&M University": {
        "unitid": 228723,
    },
}

# ---------------------------------------------------------------------------
# Manual aliases  (Pass 3 -- applied after Supabase unitids and IPEDS name match)
#
# Source of truth: data_pipelines/college_cuts/supabase_manual_aliases.csv
#
# Keyed by (api_name_exact, state_full) -> IPEDS unitid string.
# Use this for institutions whose Supabase API name cannot be normalised to
# match the IPEDS name automatically.
#
# The CSV has columns:
#   api_name     -- exact Supabase institution_name string
#   state_full   -- expanded state name (matches expand_state output)
#   unitid       -- IPEDS unitid as a 6-digit numeric string
#   in_canonical -- "true" if the unitid appears in ipeds_financial_health_canonical_*.csv
#                   (full financial profile available); "false" if the unitid
#                   exists only in raw IPEDS data (closed, 2-year, health-science,
#                   or specialty institution -- appears in cuts tracker but no
#                   financial profile page)
#   notes        -- free-form human-readable explanation
#
# Edit the CSV, not this file, to add or remove aliases. The structural
# invariants in tests/test_import_supabase.py still guard the loaded dict.
# ---------------------------------------------------------------------------
MANUAL_ALIASES_CSV_PATH = os.path.join(
    os.path.dirname(__file__),
    "..", "data_pipelines", "college_cuts", "supabase_manual_aliases.csv"
)


def _load_manual_aliases(csv_path: str) -> dict[tuple[str, str], str]:
    """Loads the (api_name, state_full) -> unitid table from a CSV.

    Raises with a pointed message if the file is missing, malformed, or
    contains duplicate keys, so a broken CSV never silently degrades the
    Pass-3 mapping to an empty dict.
    """
    if not os.path.exists(csv_path):
        raise FileNotFoundError(
            "Manual-alias CSV not found: {}. "
            "This file is the authoritative source for Supabase -> IPEDS "
            "manual mappings and must exist for the import to run.".format(csv_path)
        )
    required_columns = {"api_name", "state_full", "unitid"}
    aliases: dict[tuple[str, str], str] = {}
    with open(csv_path, newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        missing = required_columns - set(reader.fieldnames or [])
        if missing:
            raise ValueError(
                "Manual-alias CSV {} is missing required columns: {}".format(
                    csv_path, sorted(missing)
                )
            )
        for row_num, row in enumerate(reader, start=2):  # header is row 1
            api_name = (row.get("api_name") or "").strip()
            state_full = (row.get("state_full") or "").strip()
            unitid = (row.get("unitid") or "").strip()
            if not (api_name and state_full and unitid):
                raise ValueError(
                    "{} row {}: api_name, state_full, and "
                    "unitid are all required (got {!r})".format(csv_path, row_num, row)
                )
            key = (api_name, state_full)
            if key in aliases:
                raise ValueError(
                    "{} row {}: duplicate key {!r}".format(csv_path, row_num, key)
                )
            aliases[key] = unitid
    return aliases


MANUAL_ALIASES: dict[tuple[str, str], str] = _load_manual_aliases(MANUAL_ALIASES_CSV_PATH)

# ---------------------------------------------------------------------------
# Explicitly excluded institutions
#
# Keyed by (api_name_exact, state_full) -> plain-English reason.
# These are Supabase institutions that fall outside the 4-year financial
# tracker scope and will never have a matching IPEDS unitid in our dataset.
# They are logged at run time but omitted from the output CSV and from the
# "Still unmatched" count, keeping the review list focused on real problems.
#
# Note: keys use the state_full value AFTER any SUPABASE_DATA_CORRECTIONS
# have been applied, so corrections and exclusions compose correctly.
# ---------------------------------------------------------------------------
EXCLUDED_INSTITUTIONS: dict[tuple[str, str], str] = {
    # 2-year / community / technical colleges
    ("Santa Monica College",              "California")     : "2-year community college",
    ("Napa Valley College",               "California")     : "2-year community college",
    ("Cuyahoga Community College",        "Ohio")           : "2-year community college",
    ("Harrisburg Area Community College", "Pennsylvania")   : "2-year community college",
    ("Tarrant County College",            "Texas")          : "2-year community college",
    ("Salt Lake Community College",       "Utah")           : "2-year community college",
    ("Milwaukee Area Technical College",  "Wisconsin")      : "2-year technical college",
    ("Western Wyoming Community College", "Wyoming")        : "2-year community college",

    # Non-degree-granting / sub-units / system offices
    ("University of Minnesota Extension", "Minnesota")      : "extension service, not a degree-granting institution",
    ("University of Wisconsin Law School","Wisconsin")      : "law school sub-unit of UW-Madison, not a standalone institution",
    ("State University of New York",      "New York")       : "SUNY system office; individual campuses are tracked separately",

    # International / outside IPEDS scope
    ("Texas A&M University at Qatar",     "Non-US")         : "international branch campus, not in IPEDS",
}

# State abbreviation -> full name
STATE_ABBREV = {
    "AL": "Alabama", "AK": "Alaska", "AZ": "Arizona", "AR": "Arkansas",
    "CA": "California", "CO": "Colorado", "CT": "Connecticut", "DE": "Delaware",
    "FL": "Florida", "GA": "Georgia", "HI": "Hawaii", "ID": "Idaho",
    "IL": "Illinois", "IN": "Indiana", "IA": "Iowa", "KS": "Kansas",
    "KY": "Kentucky", "LA": "Louisiana", "ME": "Maine", "MD": "Maryland",
    "MA": "Massachusetts", "MI": "Michigan", "MN": "Minnesota", "MS": "Mississippi",
    "MO": "Missouri", "MT": "Montana", "NE": "Nebraska", "NV": "Nevada",
    "NH": "New Hampshire", "NJ": "New Jersey", "NM": "New Mexico", "NY": "New York",
    "NC": "North Carolina", "ND": "North Dakota", "OH": "Ohio", "OK": "Oklahoma",
    "OR": "Oregon", "PA": "Pennsylvania", "RI": "Rhode Island", "SC": "South Carolina",
    "SD": "South Dakota", "TN": "Tennessee", "TX": "Texas", "UT": "Utah",
    "VT": "Vermont", "VA": "Virginia", "WA": "Washington", "WV": "West Virginia",
    "WI": "Wisconsin", "WY": "Wyoming", "DC": "District of Columbia",
    "PR": "Puerto Rico", "GU": "Guam", "VI": "Virgin Islands",
}

# ---------------------------------------------------------------------------
# College Cuts Supabase project (public anon key -- intentionally hardcoded)
# ---------------------------------------------------------------------------
_DEFAULT_URL = "https://nvjhqurarkdcgzwwpbhc.supabase.co"
_DEFAULT_KEY = (
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9."
    "eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Im52amhxdXJhcmtkY2d6d3dwYmhjIiwicm9sZSI6"
    "ImFub24iLCJpYXQiOjE3NTA2Mzk4NjcsImV4cCI6MjA2NjIxNTg2N30."
    "kaVPHXV33oiDfM0bUEcKYZkqpihUEeVIiokRpL3VC5s"
)


# ---------------------------------------------------------------------------
# Name normalisation
# ---------------------------------------------------------------------------
#
# CONTRACT: This function mirrors `normalize_name_cuts()` in
# scripts/shared/name_normalization.R byte-for-byte. When you edit one,
# edit the other in the same commit. The Python drift guard lives in
# `tests/test_import_supabase.py::test_normalize_name_cuts_shared_fixtures`;
# the R side is `tests/test_name_normalization.R`. Both feed the shared
# fixtures in `tests/fixtures/name_normalization_cuts.json` and fail CI
# if either implementation disagrees with the pinned expectations.

def normalize_name(s: str) -> str:
    s = (s or "").lower()
    s = re.sub(r"^the +", "", s)
    s = re.sub(r"^suny +", "", s)
    s = s.replace("&", " and ")
    s = re.sub(r"[^a-z0-9 ]", " ", s)
    s = re.sub(r"\s+main campus$", "", s)
    s = re.sub(r"\bst\b", "saint", s)
    s = re.sub(r"\s+", " ", s).strip()
    return s


# ---------------------------------------------------------------------------
# Supabase fetch
# ---------------------------------------------------------------------------

def expand_state(state_raw: str) -> str:
    import sys
    s = (state_raw or "").strip()
    result = STATE_ABBREV.get(s.upper(), s)
    # Warn about unknown state codes
    if s.upper() and s.upper() not in STATE_ABBREV and result == s:
        print("  WARNING: Unknown state code '{}' passed through unchanged".format(s), file=sys.stderr)
    return result


def fetch_with_retry(url: str, headers: dict, max_attempts: int = 3) -> dict:
    """Fetch URL with exponential backoff retry."""
    import time
    import sys

    last_error = None
    for attempt in range(1, max_attempts + 1):
        try:
            req = urllib.request.Request(url, headers=headers)
            with urllib.request.urlopen(req, timeout=30) as resp:
                return json.loads(resp.read().decode())
        except Exception as e:
            last_error = e
            if attempt < max_attempts:
                wait = 2 ** attempt  # exponential backoff: 2, 4, 8 seconds
                print(
                    "  Attempt {}/{} failed: {}. Retrying in {}s...".format(
                        attempt, max_attempts, e, wait
                    ),
                    file=sys.stderr,
                )
                time.sleep(wait)
            else:
                break

    raise RuntimeError(
        "Failed to fetch {} after {} attempts: {}".format(url, max_attempts, last_error)
    ) from last_error


def fetch_all_supabase_institutions(
    base_url: str,
    api_key: str,
    page_size: int = 1000,
    hard_cap: int = 100_000,
) -> list[dict]:
    """Return all rows from the Supabase institutions table.

    Supabase (PostgREST) caps single-response results at a server-configured
    maximum (1,000 by default). The previous implementation used `?limit=10000`,
    which silently truncates once the table exceeds whatever cap the server
    enforces. We now page explicitly with `limit`/`offset` and stop when we
    receive a short page. The `hard_cap` is a paranoia guard against a runaway
    loop, not a real upper bound; raise it if the table ever grows past it.
    """
    import sys

    base = base_url.rstrip("/")
    headers = {
        "apikey":        api_key,
        "Authorization": "Bearer {}".format(api_key),
        "Accept":        "application/json",
    }

    out: list[dict] = []
    offset = 0
    while offset < hard_cap:
        url = (
            "{}/rest/v1/institutions"
            "?select=name,unitid,state"
            "&order=unitid.asc.nullslast"
            "&limit={}&offset={}".format(base, page_size, offset)
        )
        rows = fetch_with_retry(url, headers)
        if not isinstance(rows, list):
            raise RuntimeError(
                "Supabase returned a non-list payload at offset={}: {!r}".format(offset, rows)
            )
        for row in rows:
            name = (row.get("name") or "").strip()
            if not name:
                continue
            out.append({
                "api_name":   name,
                "unitid":     row.get("unitid"),       # may be None
                "state_full": expand_state(row.get("state") or ""),
            })
        if len(rows) < page_size:
            return out
        offset += page_size
        print(
            "  Supabase: fetched {} rows, continuing...".format(offset),
            file=sys.stderr,
        )

    raise RuntimeError(
        "Supabase pagination exceeded hard_cap={}; either the table "
        "has grown unexpectedly large or the loop is not advancing.".format(hard_cap)
    )


# ---------------------------------------------------------------------------
# IPEDS canonical lookup -- build norm_name -> (unitid, ipeds_name) index
# ---------------------------------------------------------------------------

def _canonical_sort_key(path: str) -> tuple:
    """Sort canonical IPEDS files by parsed end year, then file mtime."""
    name = os.path.basename(path)
    match = re.search(r"canonical_(\d{4})_(\d{4})\.csv$", name)
    if match:
        start_year, end_year = (int(match.group(1)), int(match.group(2)))
    else:
        start_year, end_year = (0, 0)
    try:
        mtime = os.path.getmtime(path)
    except OSError:
        mtime = 0
    return (end_year, start_year, mtime, name)


def find_latest_ipeds_canonical_path(search_dir: str = IPEDS_DERIVED_DIR) -> str:
    """Return the newest committed canonical IPEDS CSV in the derived directory."""
    pattern = os.path.join(search_dir, IPEDS_CANONICAL_PATTERN)
    candidates = [path for path in glob.glob(pattern) if os.path.isfile(path)]
    if not candidates:
        raise FileNotFoundError(
            "No canonical IPEDS CSV found matching {}".format(os.path.normpath(pattern))
        )
    return os.path.normpath(sorted(candidates, key=_canonical_sort_key)[-1])


def load_ipeds_lookup(ipeds_path: str) -> dict:
    """
    Returns {(norm_name, state_full): {"unitid": ..., "ipeds_name": ...}}
    Only includes unambiguous entries (exactly one unitid per norm_name+state).
    """
    if not os.path.exists(ipeds_path):
        raise FileNotFoundError("IPEDS canonical dataset not found at {}".format(ipeds_path))

    with open(ipeds_path, "rb") as f:
        raw = f.read().replace(b"\x00", b"")

    reader = csv.DictReader(io.StringIO(raw.decode("utf-8", errors="replace")))
    # Collect unique (unitid, ipeds_name) per (norm_name, state) key
    index: dict[tuple, set] = {}
    names: dict[tuple, str] = {}
    for row in reader:
        uid   = (row.get("unitid") or "").strip()
        name  = (row.get("institution_name") or "").strip()
        state = (row.get("state") or "").strip()
        if not uid or not name or not state:
            continue
        key = (normalize_name(name), state)
        index.setdefault(key, set()).add(uid)
        names[key] = name   # keep any one IPEDS name for display

    # Drop ambiguous keys (multiple unitids for same norm_name+state)
    return {
        key: {"unitid": next(iter(uids)), "ipeds_name": names[key]}
        for key, uids in index.items()
        if len(uids) == 1
    }


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--url", default=os.environ.get("SUPABASE_URL", _DEFAULT_URL))
    parser.add_argument("--key", default=os.environ.get("SUPABASE_KEY", _DEFAULT_KEY))
    parser.add_argument("--ipeds-canonical",
                        default=os.environ.get("IPEDS_CANONICAL_PATH"),
                        help="Optional explicit canonical IPEDS CSV path")
    parser.add_argument("--skip-stale-check", action="store_true",
                        help="Skip freshness check and force re-run even if file is recent")
    args = parser.parse_args()

    # Check if output file is recent (skip stale check if flag set)
    if not args.skip_stale_check and os.path.exists(OUTPUT_PATH):
        import time
        file_age_days = (time.time() - os.path.getmtime(OUTPUT_PATH)) / 86400
        if file_age_days < 1:
            print("Output file is fresh ({:.1f} days old). Use --skip-stale-check to force re-run.".format(file_age_days))
            return

    # 1. Fetch all Supabase institutions
    print("Fetching all institutions from {} ...".format(args.url))
    supabase_rows = fetch_all_supabase_institutions(args.url, args.key)

    # 1a. Apply data-quality corrections for known bad Supabase records.
    # IPEDS is authoritative for these overrides; see SUPABASE_DATA_CORRECTIONS above.
    corrections_applied = []
    for r in supabase_rows:
        corrections = SUPABASE_DATA_CORRECTIONS.get(r["api_name"], {})
        for field, value in corrections.items():
            old_value = r[field]
            if old_value != value:
                r[field] = value
                corrections_applied.append(
                    "  CORRECTION: {!r} field '{}': {!r} -> {!r} (IPEDS override)".format(
                        r["api_name"], field, old_value, value
                    )
                )
    if corrections_applied:
        print("Applied {} Supabase data-quality correction(s):".format(len(corrections_applied)))
        for msg in corrections_applied:
            print(msg)

    with_uid    = [r for r in supabase_rows if r["unitid"]]
    without_uid = [r for r in supabase_rows if not r["unitid"]]
    print("  Total: {}  |  with unitid: {}  |  missing: {}".format(
        len(supabase_rows), len(with_uid), len(without_uid)
    ))

    # 2. Load IPEDS lookup for fallback matching
    ipeds_path = os.path.normpath(args.ipeds_canonical) if args.ipeds_canonical else find_latest_ipeds_canonical_path()
    print("Loading IPEDS canonical dataset: {}".format(ipeds_path))
    ipeds_lookup = load_ipeds_lookup(ipeds_path)
    print("  {} unambiguous IPEDS (norm_name, state) entries".format(len(ipeds_lookup)))

    # 3. Build output rows
    results = []

    # Pass 1 -- Supabase unitids (highest confidence, after corrections)
    for r in with_uid:
        results.append({
            "institution_name_api":     r["api_name"],
            "unitid":                   str(int(r["unitid"])),
            "state_full":               r["state_full"],
            "tracker_institution_name": r["api_name"],
            "match_source":             "supabase",
        })

    # Pass 2 -- IPEDS name match for those missing unitids
    # Build a set of (api_name, state) already resolved by Pass 1 so we don't double-count.
    resolved_names: set[tuple[str, str]] = {
        (r["institution_name_api"], r["state_full"]) for r in results
    }

    ipeds_matched: list[dict] = []
    pass2_unresolved: list[dict] = []
    for r in without_uid:
        identity = (r["api_name"], r["state_full"])
        if identity in resolved_names:
            continue
        key = (normalize_name(r["api_name"]), r["state_full"])
        hit = ipeds_lookup.get(key)
        if hit:
            row = {
                "institution_name_api":     r["api_name"],
                "unitid":                   hit["unitid"],
                "state_full":               r["state_full"],
                "tracker_institution_name": hit["ipeds_name"],
                "match_source":             "ipeds_name_match",
            }
            results.append(row)
            resolved_names.add(identity)
            ipeds_matched.append(row)
        else:
            pass2_unresolved.append(r)

    print("  IPEDS fallback: {} matched, {} still unresolved".format(
        len(ipeds_matched), len(pass2_unresolved)
    ))

    # Pass 3 -- Manual aliases for institutions whose names can't be auto-normalised
    alias_matched: list[dict] = []
    still_unresolved: list[dict] = []
    for r in pass2_unresolved:
        identity = (r["api_name"], r["state_full"])
        unitid = MANUAL_ALIASES.get(identity)
        if unitid:
            row = {
                "institution_name_api":     r["api_name"],
                "unitid":                   unitid,
                "state_full":               r["state_full"],
                "tracker_institution_name": r["api_name"],
                "match_source":             "manual_alias",
            }
            results.append(row)
            resolved_names.add(identity)
            alias_matched.append(row)
        else:
            still_unresolved.append(r)

    print("  Manual aliases:  {} matched".format(len(alias_matched)))

    # Separate the truly unresolved from the explicitly excluded
    excluded   = [r for r in still_unresolved
                  if (r["api_name"], r["state_full"]) in EXCLUDED_INSTITUTIONS]
    unresolved = [r for r in still_unresolved
                  if (r["api_name"], r["state_full"]) not in EXCLUDED_INSTITUTIONS]

    print("  Explicitly excluded (out of scope): {}".format(len(excluded)))
    print("  Genuinely unresolved: {}".format(len(unresolved)))
    print("  Total output rows: {}".format(len(results)))

    # Unmatched-rate threshold check.
    # Excluded institutions are intentionally out of scope; only truly unresolved
    # ones count against the threshold.
    import sys
    total = len(supabase_rows)
    unmatched_count = len(unresolved)
    unmatched_rate = unmatched_count / total if total > 0 else 0
    print("  Unmatched rate (excl. excluded): {:.1%}  ({}/{})".format(
        unmatched_rate, unmatched_count, total
    ))
    if unmatched_rate > 0.20:
        print(
            "  WARNING: HIGH UNMATCHED RATE -- {:.1%} of Supabase institutions "
            "({} of {}) could not be matched to IPEDS.\n"
            "  Check name normalization or whether the IPEDS canonical CSV is current.".format(
                unmatched_rate, unmatched_count, total
            ),
            file=sys.stderr,
        )

    # 4. Deduplicate on normalized key before writing.
    # The same school can appear under two slightly different API names that
    # collapse to the same (normalize(api_name), state_full) key -- e.g. a
    # hyphen vs em-dash variant.  When unitids agree, keep the highest-confidence
    # source.  When unitids conflict, stop so a human resolves it rather than
    # silently emitting a bad mapping.
    SOURCE_PRIORITY = {"supabase": 0, "ipeds_name_match": 1, "manual_alias": 2}
    seen_keys: dict[tuple[str, str], dict] = {}
    for row in results:
        key = (normalize_name(row["institution_name_api"]), row["state_full"])
        if key not in seen_keys:
            seen_keys[key] = row
        else:
            existing = seen_keys[key]
            if existing["unitid"] != row["unitid"]:
                raise ValueError(
                    "Conflicting unitids for normalized key {!r}: "
                    "{!r} -> {} ({}) vs {!r} -> {} ({}). "
                    "Fix the source data in Supabase or supabase_manual_aliases.csv.".format(
                        key,
                        existing["institution_name_api"], existing["unitid"], existing["match_source"],
                        row["institution_name_api"], row["unitid"], row["match_source"],
                    )
                )
            # Same unitid -- keep the higher-confidence source entry.
            existing_prio = SOURCE_PRIORITY.get(existing["match_source"], 99)
            row_prio = SOURCE_PRIORITY.get(row["match_source"], 99)
            if row_prio < existing_prio:
                seen_keys[key] = row
    results = list(seen_keys.values())

    # 5. Write CSV
    out = os.path.normpath(OUTPUT_PATH)
    os.makedirs(os.path.dirname(out), exist_ok=True)
    with open(out, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=OUTPUT_COLUMNS)
        writer.writeheader()
        writer.writerows(results)

    print("  Written to {}".format(out))

    # 6. Report excluded institutions (informational -- expected, not bugs)
    if excluded:
        print("\nExplicitly excluded from mapping ({}) -- out of tracker scope:".format(len(excluded)))
        for r in sorted(excluded, key=lambda x: x["state_full"]):
            reason = EXCLUDED_INSTITUTIONS.get((r["api_name"], r["state_full"]), "unknown")
            print("  {:<20s}  {}  [{}]".format(r["state_full"], r["api_name"], reason))

    # 7. Report genuinely unresolved institutions -- these need attention
    if unresolved:
        print("\nStill unresolved ({}) -- consider adding to MANUAL_ALIASES:".format(len(unresolved)))
        for r in sorted(unresolved, key=lambda x: x["state_full"]):
            print("  {:<20s}  {}".format(r["state_full"], r["api_name"]))


if __name__ == "__main__":
    main()
