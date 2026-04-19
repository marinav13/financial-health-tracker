#!/usr/bin/env python3
"""
import_supabase_institution_mapping.py

Pulls all institutions from the College Cuts Supabase project and builds a
complete institution-name → IPEDS unitid mapping, writing it to:
  data_pipelines/college_cuts/supabase_institution_unitid_mapping.csv

Two-pass matching strategy:
  1. Supabase unitids  — 82 institutions already have unitids set in Supabase.
                         These are used as-is (highest confidence).
  2. IPEDS name match  — For the remaining ~130 institutions without Supabase
                         unitids, normalize both the Supabase name and all IPEDS
                         institution names (lowercase, strip punctuation, expand
                         abbreviations) and match on (norm_name, state).  Only
                         unambiguous 1-to-1 matches are accepted.

The output CSV is the authoritative matching source consumed by
build_college_cuts_join.R.  Run this script whenever new institutions are
added to Supabase to keep the mapping up to date.

Usage (no arguments needed — credentials are hardcoded):
    python scripts/import_supabase_institution_mapping.py

Override via CLI or env vars if needed:
    python scripts/import_supabase_institution_mapping.py \\
        --url "https://<project>.supabase.co" \\
        --key "<anon_key>"
"""

import argparse
import csv
import io
import json
import os
import re
import urllib.request


OUTPUT_PATH = os.path.join(
    os.path.dirname(__file__),
    "..", "data_pipelines", "college_cuts", "supabase_institution_unitid_mapping.csv"
)
IPEDS_CANONICAL_PATH = os.path.join(
    os.path.dirname(__file__),
    "..", "ipeds", "derived", "ipeds_financial_health_canonical_2014_2024.csv"
)
OUTPUT_COLUMNS = [
    "institution_name_api",
    "unitid",
    "state_full",
    "tracker_institution_name",
    "match_source",   # "supabase" | "ipeds_name_match" | "manual_alias"
]

# ---------------------------------------------------------------------------
# Manual aliases  (Pass 3 — applied after Supabase unitids and IPEDS name match)
#
# Keyed by (api_name_exact, state_full) → IPEDS unitid string.
# Use this for institutions whose Supabase API name cannot be normalised to
# match the IPEDS name automatically.
#
# Annotations:
#   [canonical]  unitid is in ipeds_financial_health_canonical_2014_2024.csv
#                → full financial profile available
#   [ipeds-only] unitid exists in raw IPEDS data but NOT in canonical CSV
#                (closed, 2-year, health-science, or specialty institution)
#                → appears in cuts tracker but no financial profile page
# ---------------------------------------------------------------------------
MANUAL_ALIASES: dict[tuple[str, str], str] = {
    # ── Name-variant mismatches (institution IS in canonical) ────────────────
    ("University of Illinois UC",                          "Illinois")     : "145637",  # [canonical] Univ. of Illinois Urbana-Champaign
    ("University of Oklahoma",                             "Oklahoma")     : "207500",  # [canonical] Univ. of Oklahoma-Norman Campus
    ("Rutgers University",                                 "New Jersey")   : "186380",  # [canonical] Rutgers Univ.-New Brunswick (main campus)
    ("Indiana University",                                 "Indiana")      : "151351",  # [canonical] Indiana Univ.-Bloomington (main campus)
    ("University of Wisconsin-Platteville Baraboo Sauk County", "Wisconsin"): "240462", # [canonical] Univ. of Wisconsin-Platteville (main campus)
    ("University of Texas HSCH",                           "Texas")        : "229300",  # [canonical] UT Health Science Center at Houston

    # ── Branch-campus entries whose parent unitid is in canonical ─────────────
    ("Johnson University Florida",                         "Florida")      : "132879",  # [ipeds-only] Johnson Univ. Florida campus

    # ── Closed 4-year institutions — in raw IPEDS, not in canonical ───────────
    ("Birmingham-Southern College",                        "Alabama")      : "100937",  # [ipeds-only] closed May 2023
    ("Hodges University",                                  "Florida")      : "367884",  # [ipeds-only] closed 2023
    ("Cabrini University",                                 "Pennsylvania") : "211352",  # [ipeds-only] closed May 2024
    ("Goddard College",                                    "Vermont")      : "230889",  # [ipeds-only] suspended/closed 2023–24
    ("Wells College",                                      "New York")     : "197230",  # [ipeds-only] closed December 2024
    ("Notre Dame College",                                 "Ohio")         : "204468",  # [ipeds-only] closed May 2024
    ("Delaware College of Art and Design",                 "Delaware")     : "432524",  # [ipeds-only] closed 2020
    ("The King's College",                                 "New York")     : "454184",  # [ipeds-only] closed Aug 2023

    # ── Specialty / small / not-primarily-baccalaureate — in raw IPEDS ────────
    ("Sullivan University",                                "Kentucky")     : "157793",  # [ipeds-only] primarily associate/certificate
    ("Magdalen College of the Liberal Arts",               "New Hampshire"): "182917",  # [ipeds-only] very small liberal-arts college
    ("Bacone College",                                     "Oklahoma")     : "206817",  # [ipeds-only] tribal college (2-year & 4-year)
    ("Pittsburgh Technical College",                       "Pennsylvania") : "215415",  # [ipeds-only] technical/for-profit college
    ("University of Saint Katherine",                      "California")   : "488785",  # [ipeds-only] small Catholic institution
    ("Oak Point University",                               "Illinois")     : "149763",  # [ipeds-only] health-sciences institution (BSN/MSN)
}

# ---------------------------------------------------------------------------
# Explicitly excluded institutions
#
# Keyed by (api_name_exact, state_full) → plain-English reason.
# These are Supabase institutions that fall outside the 4-year financial
# tracker scope and will never have a matching IPEDS unitid in our dataset.
# They are logged at run time but omitted from the output CSV and from the
# "Still unmatched" count, keeping the review list focused on real problems.
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

# State abbreviation → full name
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
# College Cuts Supabase project (public anon key — intentionally hardcoded)
# ---------------------------------------------------------------------------
_DEFAULT_URL = "https://nvjhqurarkdcgzwwpbhc.supabase.co"
_DEFAULT_KEY = (
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9."
    "eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Im52amhxdXJhcmtkY2d6d3dwYmhjIiwicm9sZSI6"
    "ImFub24iLCJpYXQiOjE3NTA2Mzk4NjcsImV4cCI6MjA2NjIxNTg2N30."
    "kaVPHXV33oiDfM0bUEcKYZkqpihUEeVIiokRpL3VC5s"
)


# ---------------------------------------------------------------------------
# Name normalisation  (mirrors normalize_name() in build_college_cuts_join.R)
# ---------------------------------------------------------------------------

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
# Supabase fetch — all 213 institutions
# ---------------------------------------------------------------------------

def expand_state(state_raw: str) -> str:
    import sys
    s = (state_raw or "").strip()
    result = STATE_ABBREV.get(s.upper(), s)
    # Warn about unknown state codes
    if s.upper() and s.upper() not in STATE_ABBREV and result == s:
        print(f"  WARNING: Unknown state code '{s}' passed through unchanged", file=sys.stderr)
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
                print(f"  Attempt {attempt}/{max_attempts} failed: {e}. Retrying in {wait}s...", file=sys.stderr)
                time.sleep(wait)
            else:
                break
    
    raise RuntimeError(
        f"Failed to fetch {url} after {max_attempts} attempts: {last_error}"
    ) from last_error


def fetch_all_supabase_institutions(base_url: str, api_key: str) -> list[dict]:
    """Return all rows from the Supabase institutions table."""
    url = (
        f"{base_url.rstrip('/')}/rest/v1/institutions"
        f"?select=name,unitid,state&limit=10000"
    )
    headers = {
        "apikey":        api_key,
        "Authorization": f"Bearer {api_key}",
        "Accept":        "application/json",
    }
    rows = fetch_with_retry(url, headers)
    
    out = []
    for row in rows:
        name = (row.get("name") or "").strip()
        if not name:
            continue
        out.append({
            "api_name":   name,
            "unitid":     row.get("unitid"),       # may be None
            "state_full": expand_state(row.get("state") or ""),
        })
    return out


# ---------------------------------------------------------------------------
# IPEDS canonical lookup — build norm_name → (unitid, ipeds_name) index
# ---------------------------------------------------------------------------

def load_ipeds_lookup(ipeds_path: str) -> dict:
    """
    Returns {(norm_name, state_full): {"unitid": ..., "ipeds_name": ...}}
    Only includes unambiguous entries (exactly one unitid per norm_name+state).
    """
    if not os.path.exists(ipeds_path):
        print(f"  Warning: IPEDS canonical dataset not found at {ipeds_path}")
        return {}

    with open(ipeds_path, "rb") as f:
        raw = f.read().replace(b"\x00", b"")

    reader = csv.DictReader(io.StringIO(raw.decode("utf-8", errors="replace")))
    # Collect unique (unitid, ipeds_name) per (norm_name, state) key
    index: dict[tuple, set] = {}
    names: dict[tuple, str] = {}
    for row in reader:
        uid  = (row.get("unitid") or "").strip()
        name = (row.get("institution_name") or "").strip()
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
    parser.add_argument("--skip-stale-check", action="store_true",
                        help="Skip freshness check and force re-run even if file is recent")
    args = parser.parse_args()
    
    # Check if output file is recent (skip stale check if flag set)
    if not args.skip_stale_check and os.path.exists(OUTPUT_PATH):
        import time
        file_age_days = (time.time() - os.path.getmtime(OUTPUT_PATH)) / 86400
        if file_age_days < 1:
            print(f"Output file is fresh ({file_age_days:.1f} days old). Use --skip-stale-check to force re-run.")
            return

    # 1. Fetch all Supabase institutions
    print(f"Fetching all institutions from {args.url} …")
    supabase_rows = fetch_all_supabase_institutions(args.url, args.key)
    with_uid    = [r for r in supabase_rows if r["unitid"]]
    without_uid = [r for r in supabase_rows if not r["unitid"]]
    print(f"  Total: {len(supabase_rows)}  |  with unitid: {len(with_uid)}  |  missing: {len(without_uid)}")

    # 2. Load IPEDS lookup for fallback matching
    ipeds_path = os.path.normpath(IPEDS_CANONICAL_PATH)
    print(f"Loading IPEDS canonical dataset …")
    ipeds_lookup = load_ipeds_lookup(ipeds_path)
    print(f"  {len(ipeds_lookup)} unambiguous IPEDS (norm_name, state) entries")

    # 3. Build output rows
    results = []

    # Pass 1 — Supabase unitids (highest confidence)
    for r in with_uid:
        results.append({
            "institution_name_api":     r["api_name"],
            "unitid":                   str(int(r["unitid"])),
            "state_full":               r["state_full"],
            "tracker_institution_name": r["api_name"],
            "match_source":             "supabase",
        })

    # Pass 2 — IPEDS name match for those missing unitids
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

    print(f"  IPEDS fallback: {len(ipeds_matched)} matched, {len(pass2_unresolved)} still unresolved")

    # Pass 3 — Manual aliases for institutions whose names can't be auto-normalised
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

    print(f"  Manual aliases:  {len(alias_matched)} matched")

    # Separate the truly unresolved from the explicitly excluded
    excluded   = [r for r in still_unresolved
                  if (r["api_name"], r["state_full"]) in EXCLUDED_INSTITUTIONS]
    unresolved = [r for r in still_unresolved
                  if (r["api_name"], r["state_full"]) not in EXCLUDED_INSTITUTIONS]

    print(f"  Explicitly excluded (out of scope): {len(excluded)}")
    print(f"  Genuinely unresolved: {len(unresolved)}")
    print(f"  Total output rows: {len(results)}")

    # Priority 3: Unmatched-rate threshold check
    # Excluded institutions are intentionally out of scope; only truly unresolved
    # ones count against the threshold.
    import sys
    total = len(supabase_rows)
    unmatched_count = len(unresolved)
    unmatched_rate = unmatched_count / total if total > 0 else 0
    print(f"  Unmatched rate (excl. excluded): {unmatched_rate:.1%}  ({unmatched_count}/{total})")
    if unmatched_rate > 0.20:
        print(
            f"  WARNING: HIGH UNMATCHED RATE — {unmatched_rate:.1%} of Supabase institutions "
            f"({unmatched_count} of {total}) could not be matched to IPEDS.\n"
            f"  Check name normalization or whether the IPEDS canonical CSV is current.",
            file=sys.stderr,
        )

    # 4. Write CSV
    out = os.path.normpath(OUTPUT_PATH)
    os.makedirs(os.path.dirname(out), exist_ok=True)
    with open(out, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=OUTPUT_COLUMNS)
        writer.writeheader()
        writer.writerows(results)

    print(f"  Written to {out}")

    # 5. Report excluded institutions (informational — expected, not bugs)
    if excluded:
        print(f"\nExplicitly excluded from mapping ({len(excluded)}) — out of tracker scope:")
        for r in sorted(excluded, key=lambda x: x["state_full"]):
            reason = EXCLUDED_INSTITUTIONS.get((r["api_name"], r["state_full"]), "unknown")
            print(f"  {r['state_full']:20s}  {r['api_name']}  [{reason}]")

    # 6. Report genuinely unresolved institutions — these need attention
    if unresolved:
        print(f"\nStill unresolved ({len(unresolved)}) — consider adding to MANUAL_ALIASES:")
        for r in sorted(unresolved, key=lambda x: x["state_full"]):
            print(f"  {r['state_full']:20s}  {r['api_name']}")


if __name__ == "__main__":
    main()
