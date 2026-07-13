"""Regression tests for the tranche-D cuts discovery watchlist builder."""

import csv
import json
import tempfile
from pathlib import Path
import sys

REPO_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO_ROOT / "scripts"))

from cuts_discovery.build_watchlist import build_watchlist_rows  # noqa: E402


def write_csv(path: Path, fieldnames: list[str], rows: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, lineterminator="\n")
        writer.writeheader()
        for row in rows:
            writer.writerow(row)


def main():
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        full_dataset = root / "data" / "downloads" / "full_dataset.csv"
        summary_csv = root / "data_pipelines" / "accreditation" / "accreditation_tracker_institution_summary.csv"
        overrides_csv = root / "data_pipelines" / "college_cuts" / "editorial_overrides.csv"
        index_json = root / "data" / "accreditation_index.json"

        write_csv(
            full_dataset,
            [
                "unitid",
                "institution_name",
                "state",
                "enrollment_pct_change_5yr",
                "federal_composite_score_2022_2023",
            ],
            [
                {"unitid": "100", "institution_name": "Alpha University", "state": "Alabama", "enrollment_pct_change_5yr": "-1", "federal_composite_score_2022_2023": "0.5"},
                {"unitid": "200", "institution_name": "Beta College", "state": "Texas", "enrollment_pct_change_5yr": "-10", "federal_composite_score_2022_2023": "1.0"},
                {"unitid": "300", "institution_name": "Gamma University", "state": "Ohio", "enrollment_pct_change_5yr": "-10", "federal_composite_score_2022_2023": "-0.2"},
                {"unitid": "400", "institution_name": "Delta Institute", "state": "New York", "enrollment_pct_change_5yr": "5", "federal_composite_score_2022_2023": "0.1"},
                {"unitid": "500", "institution_name": "Epsilon State", "state": "Florida", "enrollment_pct_change_5yr": "-10", "federal_composite_score_2022_2023": "-0.2"},
            ],
        )
        write_csv(
            summary_csv,
            ["unitid", "action_count"],
            [
                {"unitid": "400", "action_count": "1"},
                {"unitid": "200", "action_count": "0"},
            ],
        )
        write_csv(
            overrides_csv,
            ["source_unitid", "override_unitid", "review_status"],
            [
                {"source_unitid": "100", "override_unitid": "", "review_status": "approved"},
                {"source_unitid": "400", "override_unitid": "", "review_status": "approved"},
                {"source_unitid": "300", "override_unitid": "", "review_status": "reject"},
            ],
        )
        index_json.parent.mkdir(parents=True, exist_ok=True)
        index_json.write_text(json.dumps({"999": {"unitid": "999", "action_count": 9}}), encoding="utf-8")

        rows = build_watchlist_rows(
            n=4,
            full_dataset_path=full_dataset,
            summary_path=summary_csv,
            index_path=index_json,
            overrides_path=overrides_csv,
        )
        assert [row["unitid"] for row in rows] == ["100", "400", "300", "500"], rows
        assert rows[0]["reason"] == "existing_cut", rows[0]
        assert rows[1]["reason"] == "accreditation_action;existing_cut", rows[1]
        assert rows[2]["reason"] == "risk_rank", rows[2]

        summary_csv.unlink()
        rows_from_index = build_watchlist_rows(
            n=2,
            full_dataset_path=full_dataset,
            summary_path=summary_csv,
            index_path=index_json,
            overrides_path=overrides_csv,
        )
        assert rows_from_index[0]["unitid"] == "100", rows_from_index
        assert rows_from_index[1]["unitid"] == "400", rows_from_index

    print("cuts discovery watchlist tests: all passed.")


if __name__ == "__main__":
    main()
