#!/usr/bin/env python3
"""
import_supabase_institution_mapping.py

Pulls the institution-name → IPEDS unitid mapping from the College Cuts
Supabase project and writes it to:
  data_pipelines/college_cuts/supabase_institution_unitid_mapping.csv

This CSV is the authoritative matching source used by build_college_cuts_join.R.
Run this script whenever new institutions are added to Supabase to keep the
mapping up to date.

Usage (no arguments needed — credentials are hardcoded):
    python scripts/import_supabase_institution_mapping.py

Override via CLI or env vars if needed:
    python scripts/import_supabase_institution_mapping.py \\
        --url "https://<project>.supabase.co" \\
        --key "<anon_key>"

    SUPABASE_URL=... SUPABASE_KEY=... python scripts/...

Output columns:
    institution_name_api     – name as it appears in the college-cuts API
    unitid                   – IPEDS unitid (integer)
    state_full               – full state name (e.g. "California")
    tracker_institution_name – canonical name from the Supabase institutions table
"""

import argparse
import csv
import json
import os
import sys
import urllib.request


OUTPUT_PATH = os.path.join(
    os.path.dirname(__file__),
    "..", "data_pipelines", "college_cuts", "supabase_institution_unitid_mapping.csv"
)
OUTPUT_COLUMNS = [
    "institution_name_api",
    "unitid",
    "state_full",
    "tracker_institution_name",
]

# State abbreviation → full name lookup
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
# Supabase REST fetch
# ---------------------------------------------------------------------------

def expand_state(state_raw: str) -> str:
    """Convert a 2-letter abbreviation to a full state name; pass through if already full."""
    s = (state_raw or "").strip()
    return STATE_ABBREV.get(s.upper(), s)


def fetch_institution_mapping(base_url: str, api_key: str) -> list[dict]:
    """
    Fetch all rows from the institutions table and return those with a unitid.

    Actual schema discovered from the College Cuts Supabase project:
        name    – institution name (used as both API name and tracker name)
        unitid  – IPEDS unitid (integer, nullable)
        state   – 2-letter state abbreviation
    """
    TABLE        = "institutions"
    NAME_COL     = "name"
    UNITID_COL   = "unitid"
    STATE_COL    = "state"

    # Fetch all rows (no server-side null filter — client-side filter below)
    url = (
        f"{base_url.rstrip('/')}/rest/v1/{TABLE}"
        f"?select={NAME_COL},{UNITID_COL},{STATE_COL}"
        f"&limit=10000"
    )
    req = urllib.request.Request(
        url,
        headers={
            "apikey":         api_key,
            "Authorization":  f"Bearer {api_key}",
            "Accept":         "application/json",
        },
    )
    with urllib.request.urlopen(req, timeout=30) as resp:
        rows = json.loads(resp.read().decode())

    results = []
    for row in rows:
        unitid   = row.get(UNITID_COL)
        api_name = (row.get(NAME_COL) or "").strip()
        if not unitid or not api_name:
            continue
        state_full = expand_state(row.get(STATE_COL) or "")
        results.append({
            "institution_name_api":      api_name,
            "unitid":                    str(int(unitid)),
            "state_full":                state_full,
            "tracker_institution_name":  api_name,
        })

    return results


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--url", default=os.environ.get("SUPABASE_URL", _DEFAULT_URL),
                        help="Supabase project URL")
    parser.add_argument("--key", default=os.environ.get("SUPABASE_KEY", _DEFAULT_KEY),
                        help="Supabase API key (anon or service role)")
    args = parser.parse_args()

    print(f"Fetching institution mapping from {args.url} …")
    rows = fetch_institution_mapping(args.url, args.key)
    print(f"  Retrieved {len(rows)} institutions with unitids")

    out = os.path.normpath(OUTPUT_PATH)
    os.makedirs(os.path.dirname(out), exist_ok=True)
    with open(out, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=OUTPUT_COLUMNS)
        writer.writeheader()
        writer.writerows(rows)

    print(f"  Written to {out}")
    if rows:
        print("  Sample rows:")
        for r in rows[:3]:
            print(f"    {r['institution_name_api']} ({r['state_full']}) → unitid {r['unitid']}")


if __name__ == "__main__":
    main()
