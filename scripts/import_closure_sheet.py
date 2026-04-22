import argparse
import csv
import json
import sys
import urllib.error
import urllib.parse
import urllib.request
from datetime import date
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_SHEET_URL = "https://docs.google.com/spreadsheets/d/1TyVZlzfoD1sr0jID6Rt421-bN5wS_9JjWnCBofmbhi8/edit?gid=0#gid=0"
DERIVED_DIR = REPO_ROOT / "data_pipelines" / "federal_closure" / "derived"
STATUS_JSON = REPO_ROOT / "data" / "closure_status_by_unitid.json"
SUMMARY_JSON = DERIVED_DIR / "closure_pipeline_summary.json"

# The closure scraper now lives outside this repo. This project only needs the
# cleaned outputs, so we import a small set of published CSV tabs and keep the
# filenames stable for the workbook and site code.
TAB_EXPORTS = (
    {
        "tab": "running_closures",
        "output": DERIVED_DIR / "running_closures.csv",
        "required_columns": (
            "event_year",
            "classification",
            "record_kind",
            "school_name_source",
        ),
    },
    {
        "tab": "main_campus_closures",
        "output": DERIVED_DIR / "main_campus_closures.csv",
        "required_columns": (
            "event_year",
            "classification",
            "record_kind",
            "school_name_source",
        ),
    },
    {
        "tab": "branch_campus_closures",
        "output": DERIVED_DIR / "branch_campus_closures.csv",
        "required_columns": (
            "event_year",
            "classification",
            "record_kind",
            "school_name_source",
        ),
    },
    {
        "tab": "mergers_consolidations",
        "output": DERIVED_DIR / "mergers_consolidations.csv",
        "required_columns": (
            "event_year",
            "classification",
            "record_kind",
            "school_name_source",
        ),
    },
    {
        "tab": "private_sector_federal_main_closures",
        "output": DERIVED_DIR / "private_sector_federal_main_closures.csv",
        "required_columns": (
            "event_year",
            "classification",
            "record_kind",
            "school_name_source",
        ),
    },
    {
        "tab": "closure_status_tracker_matches",
        "output": DERIVED_DIR / "closure_status_tracker_matches.csv",
        "required_columns": (
            "unitid",
            "institution_name",
            "state",
            "close_year",
            "federal_school_name",
            "opeid6",
        ),
    },
)


def parse_args():
    parser = argparse.ArgumentParser(
        description=(
            "Import closure outputs for the interactive from a published Google "
            "Sheet or from a local directory of CSV exports."
        )
    )
    parser.add_argument(
        "--sheet",
        default=DEFAULT_SHEET_URL,
        help=(
            "Google Sheet URL or sheet ID. The sheet must expose tabs named like "
            "the closure output files, for example running_closures."
        ),
    )
    parser.add_argument(
        "--from-dir",
        default="",
        help=(
            "Optional local directory of CSV exports for offline testing. When "
            "set, the script reads <tab>.csv files from this folder instead of "
            "downloading from Google Sheets."
        ),
    )
    parser.add_argument(
        "--output-root",
        default=str(REPO_ROOT),
        help=(
            "Root folder for generated data/ and data_pipelines/ outputs. "
            "Defaults to the repository root. Tests using --from-dir should "
            "write to a temporary output root so fixture data cannot overwrite "
            "committed site data."
        ),
    )
    return parser.parse_args()


def extract_sheet_id(value):
    value = (value or "").strip()
    if not value:
        raise ValueError("No Google Sheet URL or ID was provided.")

    if "/spreadsheets/d/" in value:
        marker = "/spreadsheets/d/"
        start = value.index(marker) + len(marker)
        tail = value[start:]
        return tail.split("/", 1)[0]

    if "/" in value or " " in value:
        raise ValueError(
            "The --sheet value must be a Google Sheet URL or a plain sheet ID."
        )

    return value


def sheet_csv_url(sheet_id, tab_name):
    encoded_tab = urllib.parse.quote(tab_name, safe="")
    return (
        f"https://docs.google.com/spreadsheets/d/{sheet_id}/gviz/tq"
        f"?tqx=out:csv&sheet={encoded_tab}"
    )


def read_text_from_google_sheet(sheet_id, tab_name):
    url = sheet_csv_url(sheet_id, tab_name)
    try:
        with urllib.request.urlopen(url, timeout=30) as response:
            payload = response.read().decode("utf-8-sig")
    except urllib.error.HTTPError as exc:
        raise RuntimeError(
            f"Could not download the '{tab_name}' tab from the closure sheet.\n"
            f"URL: {url}\n"
            f"HTTP status: {exc.code}\n\n"
            "Make sure the tab exists and the sheet is published or viewable "
            "without a Google login."
        ) from exc
    except urllib.error.URLError as exc:
        raise RuntimeError(
            f"Could not reach the closure sheet for tab '{tab_name}'.\n"
            f"URL: {url}\n"
            f"Network error: {exc.reason}"
        ) from exc

    if not payload.strip():
        raise RuntimeError(
            f"The closure sheet tab '{tab_name}' downloaded as an empty file.\n"
            "Make sure the tab has data and is published correctly."
        )

    if payload.lstrip().startswith("<"):
        raise RuntimeError(
            f"The closure sheet tab '{tab_name}' did not return CSV data.\n"
            "Google returned HTML instead, which usually means the sheet is "
            "private or the tab name is wrong.\n\n"
            f"Requested URL: {url}"
        )

    return payload


def read_text_from_local_dir(source_dir, tab_name):
    path = source_dir / f"{tab_name}.csv"
    if not path.exists():
        raise FileNotFoundError(
            f"Offline closure import could not find the file:\n{path}\n\n"
            f"Expected a CSV named {tab_name}.csv in the folder you passed to --from-dir."
        )
    return path.read_text(encoding="utf-8-sig")


def validate_csv_text(csv_text, tab_name, required_columns):
    reader = csv.DictReader(csv_text.splitlines())
    fieldnames = tuple(reader.fieldnames or ())
    missing_columns = [column for column in required_columns if column not in fieldnames]
    if missing_columns:
        raise RuntimeError(
            f"The closure sheet tab '{tab_name}' is missing required columns.\n"
            f"Missing: {', '.join(missing_columns)}\n"
            f"Found: {', '.join(fieldnames) if fieldnames else '(no header row found)'}"
        )
    rows = list(reader)
    return fieldnames, rows


def normalize_int(value):
    text = str(value or "").strip()
    if not text:
        return None
    try:
        return int(float(text))
    except ValueError:
        return None


def build_closure_status_json(rows, source_label, status_json):
    schools = {}
    skipped = 0
    for row in rows:
        unitid = str(row.get("unitid") or "").strip()
        if not unitid:
            print(f"  WARNING: Skipping row with no unitid: {row.get('institution_name', '(unknown)')}", file=sys.stderr)
            skipped += 1
            continue

        record = {
            "unitid": unitid,
            "institution_name": str(row.get("institution_name") or "").strip(),
            "state": str(row.get("state") or "").strip(),
            "close_year": normalize_int(row.get("close_year")),
            "closure_source_date": str(row.get("closure_source_date") or "").strip(),
            "federal_school_name": str(row.get("federal_school_name") or "").strip(),
            "opeid6": str(row.get("opeid6") or "").strip(),
        }

        close_date = str(row.get("close_date") or "").strip()
        if close_date:
            record["close_date"] = close_date

        schools[unitid] = record

    if skipped > 0:
        print(f"  {skipped} closure rows skipped due to missing unitid", file=sys.stderr)

    close_years = [
        record["close_year"]
        for record in schools.values()
        if isinstance(record.get("close_year"), int)
    ]
    payload = {
        "source_file": source_label,
        "as_of_date": date.today().isoformat(),
        "schools": schools,
    }
    if close_years:
        payload["min_close_year"] = min(close_years)
        payload["max_close_year"] = max(close_years)

    status_json.parent.mkdir(parents=True, exist_ok=True)
    with status_json.open("w", encoding="utf-8") as handle:
        json.dump(payload, handle, indent=2, ensure_ascii=False)


def write_summary(source_label, row_counts, summary_json):
    summary_json.parent.mkdir(parents=True, exist_ok=True)
    with summary_json.open("w", encoding="utf-8") as handle:
        json.dump(
            {
                "source": source_label,
                "refreshed_on": date.today().isoformat(),
                "row_counts": row_counts,
            },
            handle,
            indent=2,
        )


def main():
    args = parse_args()

    output_root = Path(args.output_root).expanduser().resolve()
    derived_dir = output_root / "data_pipelines" / "federal_closure" / "derived"
    status_json = output_root / "data" / "closure_status_by_unitid.json"
    summary_json = derived_dir / "closure_pipeline_summary.json"

    from_dir = Path(args.from_dir).expanduser().resolve() if args.from_dir else None
    if from_dir and output_root == REPO_ROOT:
        raise RuntimeError(
            "Offline --from-dir imports must pass --output-root pointing to a "
            "temporary or staging folder. This prevents fixture CSVs from "
            "overwriting committed production closure data."
        )

    # Check if committed closure JSON is stale (>30 days old)
    if status_json.exists():
        import time
        file_age_days = (time.time() - status_json.stat().st_mtime) / 86400
        if file_age_days > 30:
            print(f"WARNING: closure_status_by_unitid.json is {file_age_days:.0f} days old — source data may be stale", file=sys.stderr)

    if from_dir and not from_dir.exists():
        raise FileNotFoundError(
            f"The --from-dir folder does not exist:\n{from_dir}"
        )

    sheet_id = None if from_dir else extract_sheet_id(args.sheet)
    source_label = str(from_dir) if from_dir else f"google_sheet:{sheet_id}"

    derived_dir.mkdir(parents=True, exist_ok=True)
    row_counts = {}
    status_rows = []

    for export in TAB_EXPORTS:
        tab_name = export["tab"]
        if from_dir:
            csv_text = read_text_from_local_dir(from_dir, tab_name)
        else:
            csv_text = read_text_from_google_sheet(sheet_id, tab_name)

        _, rows = validate_csv_text(
            csv_text,
            tab_name,
            export["required_columns"],
        )

        output_path = derived_dir / f"{tab_name}.csv"
        output_path.write_text(csv_text, encoding="utf-8", newline="")
        row_counts[tab_name] = len(rows)

        if tab_name == "closure_status_tracker_matches":
            status_rows = rows

    build_closure_status_json(status_rows, source_label, status_json)
    write_summary(source_label, row_counts, summary_json)

    print(
        json.dumps(
            {
                "source": source_label,
                "row_counts": row_counts,
                "status_json": str(status_json),
            },
            indent=2,
        )
    )


if __name__ == "__main__":
    try:
        main()
    except (FileNotFoundError, RuntimeError, ValueError) as exc:
        raise SystemExit(str(exc))
