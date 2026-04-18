#!/usr/bin/env python3
"""
import_supabase_institution_mapping.py

Pulls the institution-name → IPEDS unitid mapping from Supabase and writes it
to data_pipelines/college_cuts/supabase_institution_unitid_mapping.csv.

This CSV is the authoritative matching source used by build_college_cuts_join.R.
Run this script whenever new institutions are added to Supabase to keep the
mapping up to date.

Usage:
    python scripts/import_supabase_institution_mapping.py \
        --url  "https://<project>.supabase.co" \
        --key  "<service_role_or_anon_key>"

Environment variables (alternative to CLI flags):
    SUPABASE_URL
    SUPABASE_KEY

Output columns:
    institution_name_api   – name exactly as it appears in the college-cuts API
    unitid                 – IPEDS unitid (integer)
    state_full             – full state name (e.g. "California")
    tracker_institution_name – canonical IPEDS institution name from your tracker
"""

import argparse
import csv
import json
import os
import sys
import urllib.request
import urllib.parse


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

# ---------------------------------------------------------------------------
# Supabase REST fetch
# ---------------------------------------------------------------------------

def fetch_institution_mapping(base_url: str, api_key: str) -> list[dict]:
    """
    Fetch all rows from the institutions table that have a unitid set.

    Adjust the table name and column names below to match your Supabase schema.
    Expected columns in the table:
        institution  – the name as used in the cuts API / source data
        unitid       – IPEDS unitid (integer or text)
        state        – full state name  (or abbreviation — handled below)
        name         – canonical IPEDS institution name (optional)
    """
    TABLE = "institutions"          # ← change if your table has a different name
    API_COL_NAME  = "institution"   # ← column holding the API/source institution name
    UNITID_COL    = "unitid"
    STATE_COL     = "state"
    TRACKER_NAME_COL = "name"       # ← canonical tracker name (can be same as API col)

    url = (
        f"{base_url.rstrip('/')}/rest/v1/{TABLE}"
        f"?select={API_COL_NAME},{UNITID_COL},{STATE_COL},{TRACKER_NAME_COL}"
        f"&{UNITID_COL}=not.is.null"
        f"&limit=10000"
    )
    req = urllib.request.Request(
        url,
        headers={
            "apikey": api_key,
            "Authorization": f"Bearer {api_key}",
            "Accept": "application/json",
        },
    )
    with urllib.request.urlopen(req, timeout=30) as resp:
        rows = json.loads(resp.read().decode())

    # Normalise to our output schema
    results = []
    for row in rows:
        unitid = row.get(UNITID_COL)
        api_name = (row.get(API_COL_NAME) or "").strip()
        if not unitid or not api_name:
            continue
        results.append({
            "institution_name_api":   api_name,
            "unitid":                 str(int(unitid)),
            "state_full":             (row.get(STATE_COL) or "").strip(),
            "tracker_institution_name": (row.get(TRACKER_NAME_COL) or api_name).strip(),
        })

    return results


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--url", default=os.environ.get("SUPABASE_URL", ""),
                        help="Supabase project URL")
    parser.add_argument("--key", default=os.environ.get("SUPABASE_KEY", ""),
                        help="Supabase API key (anon or service role)")
    args = parser.parse_args()

    if not args.url or not args.key:
        print(
            "ERROR: Supabase URL and key are required.\n"
            "  Pass --url / --key or set SUPABASE_URL / SUPABASE_KEY env vars.",
            file=sys.stderr,
        )
        sys.exit(1)

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


if __name__ == "__main__":
    main()
