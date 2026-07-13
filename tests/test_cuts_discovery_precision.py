"""Regression tests for cuts discovery precision telemetry."""

import csv
import tempfile
from pathlib import Path
import sys

REPO_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO_ROOT / "scripts"))

from cuts_discovery.report_precision import summarize_precision  # noqa: E402


def main():
    with tempfile.TemporaryDirectory() as tmp:
        overrides_csv = Path(tmp) / "editorial_overrides.csv"
        with overrides_csv.open("w", encoding="utf-8", newline="") as handle:
            writer = csv.DictWriter(
                handle,
                fieldnames=["source_row_origin", "review_status"],
                lineterminator="\n",
            )
            writer.writeheader()
            writer.writerows(
                [
                    {"source_row_origin": "news_scan", "review_status": "approved"},
                    {"source_row_origin": "news_scan", "review_status": "reject"},
                    {"source_row_origin": "news_scan", "review_status": ""},
                    {"source_row_origin": "warn_notice", "review_status": "approved"},
                    {"source_row_origin": "warn_notice", "review_status": "unreviewed"},
                    {"source_row_origin": "scraper", "review_status": "approved"},
                ]
            )

        summary = summarize_precision(overrides_csv)
        assert summary["news_scan"] == {"approved": 1, "rejected": 1, "pending": 1}, summary
        assert summary["warn_notice"] == {"approved": 1, "rejected": 0, "pending": 1}, summary

    print("cuts discovery precision tests: all passed.")


if __name__ == "__main__":
    main()
