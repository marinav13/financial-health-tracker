#!/usr/bin/env python3
import argparse
import csv
import json
from pathlib import Path

if __package__ in (None, ""):
    import sys

    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
    from cuts_discovery.common import ROOT, WATCHLIST_CSV, WATCHLIST_FIELDS, write_csv_rows
else:
    from .common import ROOT, WATCHLIST_CSV, WATCHLIST_FIELDS, write_csv_rows


FULL_DATASET_CSV = ROOT / "data" / "downloads" / "full_dataset.csv"
ACCREDITATION_SUMMARY_CSV = ROOT / "data_pipelines" / "accreditation" / "accreditation_tracker_institution_summary.csv"
ACCREDITATION_INDEX_JSON = ROOT / "data" / "accreditation_index.json"
EDITORIAL_OVERRIDES_CSV = ROOT / "data_pipelines" / "college_cuts" / "editorial_overrides.csv"


def _trim(value) -> str:
    return str(value or "").strip()


def _first_nonempty(row: dict, keys: tuple[str, ...]) -> str:
    for key in keys:
        value = _trim(row.get(key))
        if value:
            return value
    return ""


def _parse_float(value, default: float) -> float:
    text = _trim(value)
    if not text:
        return default
    try:
        return float(text)
    except ValueError:
        return default


def _parse_int(value, default: int = 0) -> int:
    text = _trim(value)
    if not text:
        return default
    try:
        return int(float(text))
    except ValueError:
        return default


def load_tracker_rows(path: Path = FULL_DATASET_CSV) -> list[dict]:
    rows = []
    with Path(path).open("r", encoding="utf-8", newline="") as handle:
        for row in csv.DictReader(handle):
            unitid = _trim(row.get("unitid"))
            institution_name = _trim(row.get("institution_name"))
            if not unitid or not institution_name:
                continue
            rows.append(
                {
                    "unitid": unitid,
                    "institution_name": institution_name,
                    "state": _trim(row.get("state")),
                    "enrollment_pct_change_5yr": _parse_float(row.get("enrollment_pct_change_5yr"), float("inf")),
                    "federal_composite_score_2022_2023": _parse_float(
                        row.get("federal_composite_score_2022_2023"),
                        float("inf"),
                    ),
                }
            )
    return rows


def load_live_action_unitids(
    summary_path: Path = ACCREDITATION_SUMMARY_CSV,
    index_path: Path = ACCREDITATION_INDEX_JSON,
) -> set[str]:
    summary_csv = Path(summary_path)
    if summary_csv.exists():
        unitids = set()
        with summary_csv.open("r", encoding="utf-8", newline="") as handle:
            for row in csv.DictReader(handle):
                unitid = _trim(row.get("unitid"))
                if not unitid:
                    continue
                if _parse_int(row.get("action_count")) > 0:
                    unitids.add(unitid)
        if unitids:
            return unitids

    index_json = Path(index_path)
    if not index_json.exists():
        return set()

    payload = json.loads(index_json.read_text(encoding="utf-8"))
    if isinstance(payload, dict):
        rows = payload.values()
    elif isinstance(payload, list):
        rows = payload
    else:
        rows = []

    unitids = set()
    for row in rows:
        if not isinstance(row, dict):
            continue
        unitid = _trim(row.get("unitid"))
        if not unitid:
            continue
        action_count = _parse_int(row.get("action_count"))
        landing_actions = row.get("landing_actions") or []
        if action_count > 0 or (isinstance(landing_actions, list) and len(landing_actions) > 0):
            unitids.add(unitid)
    return unitids


def load_approved_cut_unitids(path: Path = EDITORIAL_OVERRIDES_CSV) -> set[str]:
    overrides_csv = Path(path)
    if not overrides_csv.exists():
        return set()

    unitids = set()
    with overrides_csv.open("r", encoding="utf-8", newline="") as handle:
        for row in csv.DictReader(handle):
            if _trim(row.get("review_status")).lower() != "approved":
                continue
            unitid = _first_nonempty(row, ("override_unitid", "source_unitid"))
            if unitid:
                unitids.add(unitid)
    return unitids


def ranking_key(row: dict) -> tuple[float, float, str]:
    return (
        row["enrollment_pct_change_5yr"],
        row["federal_composite_score_2022_2023"],
        row["unitid"],
    )


def reason_for_unitid(unitid: str, live_action_unitids: set[str], approved_cut_unitids: set[str]) -> str:
    reasons = []
    if unitid in live_action_unitids:
        reasons.append("accreditation_action")
    if unitid in approved_cut_unitids:
        reasons.append("existing_cut")
    if not reasons:
        reasons.append("risk_rank")
    return ";".join(reasons)


def build_watchlist_rows(
    n: int = 100,
    full_dataset_path: Path = FULL_DATASET_CSV,
    summary_path: Path = ACCREDITATION_SUMMARY_CSV,
    index_path: Path = ACCREDITATION_INDEX_JSON,
    overrides_path: Path = EDITORIAL_OVERRIDES_CSV,
) -> list[dict]:
    tracker_rows = load_tracker_rows(full_dataset_path)
    live_action_unitids = load_live_action_unitids(summary_path=summary_path, index_path=index_path)
    approved_cut_unitids = load_approved_cut_unitids(overrides_path)
    priority_unitids = live_action_unitids | approved_cut_unitids

    ranked_rows = sorted(tracker_rows, key=ranking_key)
    selected = []
    seen = set()

    for row in ranked_rows:
        unitid = row["unitid"]
        if unitid not in priority_unitids or unitid in seen:
            continue
        selected.append(
            {
                "unitid": unitid,
                "institution_name": row["institution_name"],
                "state": row["state"],
                "reason": reason_for_unitid(unitid, live_action_unitids, approved_cut_unitids),
            }
        )
        seen.add(unitid)
        if len(selected) >= n:
            return selected

    for row in ranked_rows:
        unitid = row["unitid"]
        if unitid in seen:
            continue
        selected.append(
            {
                "unitid": unitid,
                "institution_name": row["institution_name"],
                "state": row["state"],
                "reason": "risk_rank",
            }
        )
        seen.add(unitid)
        if len(selected) >= n:
            break

    return selected


def write_watchlist(rows: list[dict], path: Path = WATCHLIST_CSV) -> None:
    write_csv_rows(path, WATCHLIST_FIELDS, rows)


def main() -> int:
    parser = argparse.ArgumentParser(description="Build the weekly college cuts discovery watchlist.")
    parser.add_argument("--limit", type=int, default=100, help="Number of institutions to emit.")
    parser.add_argument("--output", default=str(WATCHLIST_CSV), help="Output CSV path.")
    args = parser.parse_args()

    rows = build_watchlist_rows(n=args.limit)
    write_watchlist(rows, Path(args.output))
    print(f"cuts discovery watchlist: wrote {len(rows)} institution(s) to {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
