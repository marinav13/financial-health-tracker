#!/usr/bin/env python3
import csv
from pathlib import Path

if __package__ in (None, ""):
    import sys

    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
    from cuts_discovery.common import ROOT
else:
    from .common import ROOT


EDITORIAL_OVERRIDES_CSV = ROOT / "data_pipelines" / "college_cuts" / "editorial_overrides.csv"
DISCOVERY_ROW_ORIGINS = ("news_scan", "warn_notice")


def _trim(value) -> str:
    return str(value or "").strip()


def summarize_precision(path: Path = EDITORIAL_OVERRIDES_CSV) -> dict[str, dict[str, int]]:
    summary = {
        row_origin: {"approved": 0, "rejected": 0, "pending": 0}
        for row_origin in DISCOVERY_ROW_ORIGINS
    }
    overrides_csv = Path(path)
    if not overrides_csv.exists():
        return summary

    with overrides_csv.open("r", encoding="utf-8", newline="") as handle:
        for row in csv.DictReader(handle):
            row_origin = _trim(row.get("source_row_origin")).lower()
            if row_origin not in summary:
                continue
            review_status = _trim(row.get("review_status")).lower()
            if review_status == "approved":
                summary[row_origin]["approved"] += 1
            elif review_status == "reject":
                summary[row_origin]["rejected"] += 1
            else:
                summary[row_origin]["pending"] += 1
    return summary


def main() -> int:
    summary = summarize_precision()
    for row_origin in DISCOVERY_ROW_ORIGINS:
        counts = summary[row_origin]
        print(
            "CUTS DISCOVERY PRECISION: "
            f"{row_origin} approved={counts['approved']} rejected={counts['rejected']} pending={counts['pending']}"
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
