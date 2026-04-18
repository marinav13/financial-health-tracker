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
    "match_source",   # "supabase" | "ipeds_name_match"
]

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
    s = (state_raw or "").strip()
    return STATE_ABBREV.get(s.upper(), s)


def fetch_all_supabase_institutions(base_url: str, api_key: str) -> list[dict]:
    """Return all rows from the Supabase institutions table."""
    url = (
        f"{base_url.rstrip('/')}/rest/v1/institutions"
        f"?select=name,unitid,state&limit=10000"
    )
    req = urllib.request.Request(
        url,
        headers={
            "apikey":        api_key,
            "Authorization": f"Bearer {api_key}",
            "Accept":        "application/json",
        },
    )
    with urllib.request.urlopen(req, timeout=30) as resp:
        rows = json.loads(resp.read().decode())

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
    args = parser.parse_args()

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
    matched = unmatched = 0
    for r in without_uid:
        key = (normalize_name(r["api_name"]), r["state_full"])
        hit = ipeds_lookup.get(key)
        if hit:
            results.append({
                "institution_name_api":     r["api_name"],
                "unitid":                   hit["unitid"],
                "state_full":               r["state_full"],
                "tracker_institution_name": hit["ipeds_name"],
                "match_source":             "ipeds_name_match",
            })
            matched += 1
        else:
            unmatched += 1

    print(f"  IPEDS fallback: {matched} matched, {unmatched} still unmatched")
    print(f"  Total output rows: {len(results)}")

    # 4. Write CSV
    out = os.path.normpath(OUTPUT_PATH)
    os.makedirs(os.path.dirname(out), exist_ok=True)
    with open(out, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=OUTPUT_COLUMNS)
        writer.writeheader()
        writer.writerows(results)

    print(f"  Written to {out}")

    # 5. Report unmatched so they can be reviewed / added as manual aliases
    still_missing = [r for r in without_uid
                     if (normalize_name(r["api_name"]), r["state_full"]) not in ipeds_lookup]
    if still_missing:
        print(f"\nStill unmatched ({len(still_missing)}) — consider manual aliases in build_college_cuts_join.R:")
        for r in sorted(still_missing, key=lambda x: x["state_full"]):
            print(f"  {r['state_full']:20s}  {r['api_name']}")


if __name__ == "__main__":
    main()
