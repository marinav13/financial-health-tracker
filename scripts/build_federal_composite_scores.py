import csv
import json
import os
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
DATA_PIPELINES = ROOT / "data_pipelines"

# The federal score file is a one-sheet extract from Federal Student Aid's
# published composite score workbook for institutions with fiscal years ending
# between July 1, 2022 and June 30, 2023.
SOURCE_VERSION_LABEL = "Fiscal years ending July 1, 2022 through June 30, 2023"
SOURCE_YEAR_LABEL = "2022-23"
SOURCE_CSV = DATA_PIPELINES / "federal_composite" / "ay_2022_2023_composite_scores.csv"
SCORE_COLUMN = " Composite Score for Institution's  Fiscal Year Ending Between 07/01/2022 - 6/30/2023"

# The raw IPEDS export already carries OPEID alongside UNITID, so we can map
# the federal workbook onto our school pages without rebuilding the full IPEDS
# pipeline.
def resolve_ipeds_raw_path(start_year=None, end_year=None, explicit_path=None):
    explicit = explicit_path or os.getenv("IPEDS_RAW_PATH")
    if explicit:
        return Path(explicit).expanduser().resolve()
    start = str(start_year or os.getenv("IPEDS_START_YEAR", "2014"))
    end = str(end_year or os.getenv("IPEDS_END_YEAR", "2024"))
    return ROOT / "ipeds" / "raw" / f"ipeds_financial_health_raw_{start}_{end}.csv"


IPEDS_RAW = resolve_ipeds_raw_path()

# The school page only needs a lightweight lookup keyed by UNITID.
OUTPUT_JSON = ROOT / "data" / "federal_composite_scores_by_unitid.json"


def ensure_required_inputs(script_name, requirements):
    missing = []
    for requirement in requirements:
        if requirement["path"].exists():
            continue
        missing.append(requirement)

    if not missing:
        return

    message_lines = [
        f"{script_name} cannot start because required local input files are missing.",
        "",
        "Add the missing files below and then run the script again.",
        "",
    ]

    for requirement in missing:
        message_lines.append(f"- {requirement['label']}")
        message_lines.append(f"  Expected path: {requirement['path']}")
        if requirement.get("note"):
            message_lines.append(f"  Why it is needed: {requirement['note']}")
        message_lines.append("")

    raise FileNotFoundError("\n".join(message_lines).rstrip())


def normalize_opeid(value: str) -> str:
    text = str(value or "").strip()
    if not text:
        return ""
    # Federal Student Aid publishes OPEIDs as 8-digit strings.
    digits = "".join(ch for ch in text if ch.isdigit())
    return digits.zfill(8) if digits else ""


def parse_float(value):
    try:
        return float(value)
    except Exception:
        return None


def build_ipeds_lookup():
    latest_rows = {}
    with IPEDS_RAW.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        for row in reader:
            unitid = str(row.get("unitid", "")).strip()
            year = str(row.get("year", "")).strip()
            opeid = normalize_opeid(row.get("opeid", ""))
            if not unitid or not opeid:
                continue
            current = latest_rows.get(unitid)
            if current is None or year > current["year"]:
                latest_rows[unitid] = {
                    "year": year,
                    "opeid": opeid,
                    "institution_name": row.get("institution_name", ""),
                    "city": row.get("city", ""),
                    "state": row.get("state", "")
                }

    return {row["opeid"]: {"unitid": unitid, **row} for unitid, row in latest_rows.items()}


def composite_status(score):
    if score is None:
        return None
    if score >= 1.5:
        return "financially_responsible"
    if score >= 1.0:
        return "additional_oversight"
    return "not_financially_responsible"


def composite_status_label(status):
    labels = {
        "financially_responsible": "Financially responsible",
        "additional_oversight": "Financially responsible, but subject to additional oversight",
        "not_financially_responsible": "Not financially responsible"
    }
    return labels.get(status)


def main():
    ensure_required_inputs(
        "build_federal_composite_scores.py",
        [
            {
                "label": "Federal composite score CSV",
                "path": SOURCE_CSV,
                "note": "Used to build the federal composite score lookup shown on school profiles.",
            },
            {
                "label": "IPEDS raw dataset",
                "path": IPEDS_RAW,
                "note": "Used to map federal OPEIDs back to UNITIDs and tracker school names.",
            },
        ],
    )
    ipeds_by_opeid = build_ipeds_lookup()
    rows_by_unitid = {}

    with SOURCE_CSV.open(newline="", encoding="utf-8") as handle:
        reader = csv.reader(handle)
        next(reader, None)
        headers = next(reader, None)
        if not headers:
            raise RuntimeError(f"No header row found in {SOURCE_CSV}")
        reader = csv.DictReader(handle, fieldnames=headers)
        for row in reader:
            opeid = normalize_opeid(row.get("OPEID", ""))
            score = parse_float(row.get(SCORE_COLUMN, ""))
            if not opeid or score is None:
                continue
            match = ipeds_by_opeid.get(opeid)
            if not match:
                continue

            status = composite_status(score)
            rows_by_unitid[match["unitid"]] = {
                "unitid": match["unitid"],
                "opeid": opeid,
                "institution_name": row.get("Institution Name", ""),
                "city": row.get("City", ""),
                "state": row.get("State", ""),
                "institution_type": row.get("Institution Type", ""),
                "fiscal_year_end_excel_serial": row.get("Institution Fiscal Year End", ""),
                "federal_composite_score_2022_2023": score,
                "federal_composite_score_year_label": SOURCE_YEAR_LABEL,
                "federal_composite_score_status": status,
                "federal_composite_score_status_label": composite_status_label(status),
                "source": "Federal Student Aid composite scores"
            }

    OUTPUT_JSON.parent.mkdir(parents=True, exist_ok=True)
    with OUTPUT_JSON.open("w", encoding="utf-8") as handle:
        json.dump(
            {
                "generated_at": __import__("datetime").date.today().isoformat(),
                "source_version_label": SOURCE_VERSION_LABEL,
                "source_file": str(SOURCE_CSV.relative_to(ROOT)).replace("\\", "/"),
                "schools": rows_by_unitid
            },
            handle,
            indent=2
        )

    print(f"Wrote {len(rows_by_unitid)} matched federal composite scores to {OUTPUT_JSON}")


if __name__ == "__main__":
    try:
        main()
    except (FileNotFoundError, RuntimeError) as exc:
        raise SystemExit(str(exc))
