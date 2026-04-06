import csv
import json
import re
import zipfile
from pathlib import Path


# Build a tracker-only lookup of schools flagged as closed in the federal
# school file. We now prefer the current schfile access-format extract over the
# older April 2025 workbook because the schfile gives us an exact closure date
# and extends into 2026.
#
# Matching logic:
# - match tracker schools to federal rows by OPEID6
# - require closure year in 2024-2026, because the tracker universe was built
#   from schools active in 2024 and the user only wants recent closures flagged
# - exclude obvious fake-main-campus branch rows by requiring the 00 row's
#   location name to match the school name
# - when an OPEID6 maps to multiple tracker schools, require a strong name
#   match so we do not smear a closure across unrelated schools sharing a
#   federal agreement

REPO_ROOT = Path(__file__).resolve().parents[1]
SCHFILE_ZIP = Path(r"C:\Users\mv3031\Downloads\schfile_access_format_20260402.dat.zip")
RAW_IPEDS = REPO_ROOT / "ipeds" / "ipeds_financial_health_raw_2014_2024.csv"
SCHOOLS_INDEX = REPO_ROOT / "data" / "schools_index.json"
OUTPUT_JSON = REPO_ROOT / "data" / "closure_status_by_unitid.json"
OUTPUT_CSV = REPO_ROOT / "federal_closure" / "closure_status_tracker_matches.csv"
MIN_CLOSE_YEAR = 2024
MAX_CLOSE_YEAR = 2026


def load_tracker_unitids():
    with SCHOOLS_INDEX.open("r", encoding="utf-8") as handle:
        schools = json.load(handle)
    return {str(row["unitid"]) for row in schools}


def normalize_name(value):
    value = (value or "").lower().strip()
    value = re.sub(r"^the\s+", "", value)
    value = value.replace("&", " and ")
    value = re.sub(r"[^a-z0-9]+", " ", value)
    return " ".join(value.split())


def name_match(a, b):
    a_norm = normalize_name(a)
    b_norm = normalize_name(b)
    if not a_norm or not b_norm:
        return False
    if a_norm == b_norm or a_norm in b_norm or b_norm in a_norm:
        return True

    a_tokens = {token for token in a_norm.split() if token not in {"college", "university", "school", "of", "the", "and"}}
    b_tokens = {token for token in b_norm.split() if token not in {"college", "university", "school", "of", "the", "and"}}
    if not a_tokens or not b_tokens:
        return False
    overlap = len(a_tokens & b_tokens) / min(len(a_tokens), len(b_tokens))
    return overlap >= 0.8


def build_opeid6_to_tracker_school(tracker_unitids):
    mapping = {}
    with RAW_IPEDS.open("r", encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        for row in reader:
            unitid = str(row.get("unitid") or "").strip()
            opeid = str(row.get("opeid") or "").strip()
            if not unitid or unitid not in tracker_unitids or not opeid:
                continue
            digits = "".join(ch for ch in opeid if ch.isdigit()).zfill(8)
            opeid6 = digits[:6]
            if not opeid6:
                continue
            mapping.setdefault(opeid6, []).append({
                "unitid": unitid,
                "institution_name": str(row.get("institution_name") or "").strip(),
                "state": str(row.get("state") or "").strip(),
                "opeid": digits,
            })
    return mapping


def parse_schfile_rows():
    if not SCHFILE_ZIP.exists():
        return

    def fwf(line, start, end):
        return line[start - 1:end].strip()

    with zipfile.ZipFile(SCHFILE_ZIP) as zf:
        member = next(name for name in zf.namelist() if "rectype_01" in name.lower())
        with zf.open(member, "r") as handle:
            for raw_line in handle:
                line = raw_line.decode("latin-1", errors="ignore").rstrip("\r\n")
                digits = "".join(ch for ch in fwf(line, 3, 10) if ch.isdigit()).zfill(8)
                if not digits.strip("0") or digits[6:] != "00":
                    continue

                close_dt = fwf(line, 382, 389)
                if not (len(close_dt) == 8 and close_dt[:4].isdigit()):
                    continue
                close_year = int(close_dt[:4])
                if close_year < MIN_CLOSE_YEAR or close_year > MAX_CLOSE_YEAR:
                    continue

                reinstate_dt = fwf(line, 412, 419)
                if len(reinstate_dt) == 8 and reinstate_dt[:4].isdigit() and int(reinstate_dt[:4]) <= MAX_CLOSE_YEAR:
                    continue

                school_name = fwf(line, 12, 81)
                location_name = fwf(line, 82, 151)
                country = fwf(line, 252, 276)

                # Exclude foreign locations and obvious branch-like 00 rows.
                if country:
                    continue
                if location_name and not name_match(school_name, location_name):
                    continue

                yield {
                    "school_name": school_name,
                    "state": fwf(line, 247, 248),
                    "opeid6": digits[:6],
                    "close_year": close_year,
                    "close_date": f"{close_dt[:4]}-{close_dt[4:6]}-{close_dt[6:8]}",
                    "location_name": location_name,
                }


def main():
    tracker_unitids = load_tracker_unitids()
    tracker_by_opeid6 = build_opeid6_to_tracker_school(tracker_unitids)

    matches = []
    lookup = {}

    for closure_row in parse_schfile_rows():
        tracker_matches = tracker_by_opeid6.get(closure_row["opeid6"], [])
        if not tracker_matches:
            continue

        matched_tracker = [row for row in tracker_matches if row["state"] == closure_row["state"] and name_match(row["institution_name"], closure_row["school_name"])]
        if not matched_tracker and len(tracker_matches) == 1 and tracker_matches[0]["state"] == closure_row["state"] and name_match(tracker_matches[0]["institution_name"], closure_row["school_name"]):
            matched_tracker = tracker_matches

        for tracker_match in matched_tracker:
            unitid = tracker_match["unitid"]
            record = {
                "unitid": unitid,
                "institution_name": tracker_match["institution_name"],
                "state": tracker_match["state"],
                "close_year": closure_row["close_year"],
                "close_date": closure_row["close_date"],
                "closure_source_date": "2026-04-02",
                "federal_school_name": closure_row["school_name"],
                "opeid6": closure_row["opeid6"],
            }
            lookup[unitid] = record
            matches.append(record)

    OUTPUT_JSON.parent.mkdir(parents=True, exist_ok=True)
    OUTPUT_CSV.parent.mkdir(parents=True, exist_ok=True)

    with OUTPUT_JSON.open("w", encoding="utf-8") as handle:
        json.dump(
            {
                "source_file": SCHFILE_ZIP.name,
                "as_of_date": "2026-04-02",
                "min_close_year": MIN_CLOSE_YEAR,
                "max_close_year": MAX_CLOSE_YEAR,
                "schools": lookup,
            },
            handle,
            indent=2,
            ensure_ascii=False,
        )

    with OUTPUT_CSV.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=[
                "unitid",
                "institution_name",
                "state",
                "close_year",
                "close_date",
                "closure_source_date",
                "federal_school_name",
                "opeid6",
            ],
        )
        writer.writeheader()
        writer.writerows(matches)

    print(f"Wrote {len(matches)} tracker closure matches to {OUTPUT_JSON}")


if __name__ == "__main__":
    main()
