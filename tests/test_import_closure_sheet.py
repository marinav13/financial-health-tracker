"""
Smoke tests for scripts/import_closure_sheet.py.

Uses the --from-dir flag to run the script against local fixture CSV files
without hitting the live Google Sheet.

Run with: python tests/test_import_closure_sheet.py
"""

import json
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
SCRIPT = REPO_ROOT / "scripts" / "import_closure_sheet.py"


def make_fixture_dir():
    """Create a temp directory with fixture CSV files matching the expected tab structure."""
    fixture = Path(tempfile.mkdtemp(prefix="closure-fixture-"))

    # Each CSV must have the required columns for its tab.
    # running_closures
    (fixture / "running_closures.csv").write_text(
        "event_year,classification,record_kind,school_name_source\n"
        "2024,closed,main,Boston University\n",
        encoding="utf-8",
    )

    # main_campus_closures
    (fixture / "main_campus_closures.csv").write_text(
        "event_year,classification,record_kind,school_name_source\n"
        "2023,closed,main,Sample State College\n",
        encoding="utf-8",
    )

    # branch_campus_closures
    (fixture / "branch_campus_closures.csv").write_text(
        "event_year,classification,record_kind,school_name_source\n",
        encoding="utf-8",
    )

    # mergers_consolidations
    (fixture / "mergers_consolidations.csv").write_text(
        "event_year,classification,record_kind,school_name_source\n",
        encoding="utf-8",
    )

    # private_sector_federal_main_closures
    (fixture / "private_sector_federal_main_closures.csv").write_text(
        "event_year,classification,record_kind,school_name_source\n",
        encoding="utf-8",
    )

    # closure_status_tracker_matches — the tab that drives the JSON output
    (fixture / "closure_status_tracker_matches.csv").write_text(
        "unitid,institution_name,state,close_year,federal_school_name,opeid6,"
        "closure_source_date,close_date\n"
        "100654,Example State University,Massachusetts,2024,Example State U,010203,"
        "2024-06-01,2024-08-15\n"
        "100812,Sample College,California,2025,Sample College,040506,,2025-01-20\n",
        encoding="utf-8",
    )

    return fixture


def make_output_root():
    """Create an isolated output root with data/ and data_pipelines/ below it."""
    return Path(tempfile.mkdtemp(prefix="closure-output-"))


def run_import(fixture, output_root):
    return subprocess.run(
        [
            sys.executable,
            str(SCRIPT),
            "--from-dir",
            str(fixture),
            "--output-root",
            str(output_root),
        ],
        capture_output=True,
        text=True,
        cwd=str(REPO_ROOT),
    )


def test_script_exits_zero_with_valid_fixture():
    """Script should exit 0 when given valid fixture CSVs."""
    fixture = make_fixture_dir()
    output_root = make_output_root()
    try:
        result = run_import(fixture, output_root)
        print(f"\nstdout:\n{result.stdout}")
        if result.returncode != 0:
            print(f"stderr:\n{result.stderr}")
        assert result.returncode == 0, (
            f"Script exited {result.returncode} — stderr: {result.stderr}"
        )
    finally:
        shutil.rmtree(fixture, ignore_errors=True)
        shutil.rmtree(output_root, ignore_errors=True)


def test_from_dir_requires_isolated_output_root():
    """Fixture imports should not be allowed to write to committed data paths."""
    fixture = make_fixture_dir()
    try:
        result = subprocess.run(
            [sys.executable, str(SCRIPT), "--from-dir", str(fixture)],
            capture_output=True,
            text=True,
            cwd=str(REPO_ROOT),
        )
        assert result.returncode != 0, "Expected --from-dir without --output-root to fail"
        assert "temporary or staging folder" in result.stderr, (
            f"Expected fixture-safety error. Got: {result.stderr}"
        )
    finally:
        shutil.rmtree(fixture, ignore_errors=True)


def test_closure_status_json_structure():
    """The closure_status_by_unitid.json should have correct structure."""
    fixture = make_fixture_dir()
    output_root = make_output_root()
    try:
        result = run_import(fixture, output_root)
        assert result.returncode == 0, f"Script failed: {result.stderr}"

        status_json = output_root / "data" / "closure_status_by_unitid.json"
        assert status_json.exists(), (
            f"closure_status_by_unitid.json was not created at {status_json}"
        )

        with open(status_json, encoding="utf-8") as f:
            data = json.load(f)

        # Top-level keys must include source_file, as_of_date, schools
        assert "source_file" in data, "JSON should have 'source_file' key"
        assert "as_of_date" in data, "JSON should have 'as_of_date' key"
        assert data["min_close_year"] == 2024
        assert data["max_close_year"] == 2025
        assert "schools" in data, "JSON should have 'schools' key"
        assert isinstance(data["schools"], dict), "'schools' should be a dict (unitid-keyed)"

        # Fixture rows should appear under their unitids
        assert "100654" in data["schools"], "Unitid 100654 should appear in schools"
        assert "100812" in data["schools"], "Unitid 100812 should appear in schools"

        # Spot-check the first school's fields
        school = data["schools"]["100654"]
        assert school["unitid"] == "100654"
        assert school["institution_name"] == "Example State University"
        assert school["state"] == "Massachusetts"
        assert school["close_year"] == 2024
        assert school["close_date"] == "2024-08-15"

        # Rows with no unitid should be silently skipped (not crash)
        # The script handles empty/missing unitid gracefully.

    finally:
        shutil.rmtree(fixture, ignore_errors=True)
        shutil.rmtree(output_root, ignore_errors=True)


def test_pipeline_summary_json_structure():
    """The closure_pipeline_summary.json should have correct structure."""
    fixture = make_fixture_dir()
    output_root = make_output_root()
    derived_dir = output_root / "data_pipelines" / "federal_closure" / "derived"
    summary_json = derived_dir / "closure_pipeline_summary.json"
    try:
        result = run_import(fixture, output_root)
        assert result.returncode == 0, f"Script failed: {result.stderr}"
        assert summary_json.exists(), (
            f"closure_pipeline_summary.json was not created at {summary_json}"
        )

        with open(summary_json, encoding="utf-8") as f:
            data = json.load(f)

        assert "source" in data, "Summary should have 'source' key"
        assert "refreshed_on" in data, "Summary should have 'refreshed_on' key"
        assert "row_counts" in data, "Summary should have 'row_counts' key"
        assert isinstance(data["row_counts"], dict), "'row_counts' should be a dict"

        # The fixture sets row_counts for each tab
        assert "closure_status_tracker_matches" in data["row_counts"]
        assert data["row_counts"]["closure_status_tracker_matches"] == 2

    finally:
        shutil.rmtree(fixture, ignore_errors=True)
        shutil.rmtree(output_root, ignore_errors=True)


def test_exits_nonzero_on_missing_required_column():
    """Script should exit non-zero if a required column is absent."""
    fixture = Path(tempfile.mkdtemp(prefix="closure-bad-fixture-"))
    output_root = make_output_root()
    try:
        # Write a CSV missing required columns
        (fixture / "running_closures.csv").write_text(
            "event_year,classification\n"
            "2024,closed\n",
            encoding="utf-8",
        )
        (fixture / "main_campus_closures.csv").write_text(
            "event_year,classification\n"
            "2023,closed\n",
            encoding="utf-8",
        )
        (fixture / "branch_campus_closures.csv").write_text(
            "event_year,classification,record_kind,school_name_source\n",
            encoding="utf-8",
        )
        (fixture / "mergers_consolidations.csv").write_text(
            "event_year,classification,record_kind,school_name_source\n",
            encoding="utf-8",
        )
        (fixture / "private_sector_federal_main_closures.csv").write_text(
            "event_year,classification,record_kind,school_name_source\n",
            encoding="utf-8",
        )
        (fixture / "closure_status_tracker_matches.csv").write_text(
            "unitid,institution_name\n100654,Example U\n",
            encoding="utf-8",
        )

        result = subprocess.run(
            [
                sys.executable,
                str(SCRIPT),
                "--from-dir",
                str(fixture),
                "--output-root",
                str(output_root),
            ],
            capture_output=True,
            text=True,
            cwd=str(REPO_ROOT),
        )
        # Script should fail due to missing required columns
        assert result.returncode != 0, (
            "Script should exit non-zero when required columns are missing"
        )
        # Error message should mention the missing column
        assert "record_kind" in result.stderr or "school_name_source" in result.stderr, (
            f"Error should mention missing required column. Got: {result.stderr}"
        )
    finally:
        shutil.rmtree(fixture, ignore_errors=True)
        shutil.rmtree(output_root, ignore_errors=True)


def test_exits_nonzero_on_missing_tab_file():
    """Script should exit non-zero when a required tab CSV is absent."""
    fixture = Path(tempfile.mkdtemp(prefix="closure-missing-tab-"))
    output_root = make_output_root()
    try:
        # Only write some tabs, not all
        (fixture / "running_closures.csv").write_text(
            "event_year,classification,record_kind,school_name_source\n"
            "2024,closed,main,Boston U\n",
            encoding="utf-8",
        )
        result = subprocess.run(
            [
                sys.executable,
                str(SCRIPT),
                "--from-dir",
                str(fixture),
                "--output-root",
                str(output_root),
            ],
            capture_output=True,
            text=True,
            cwd=str(REPO_ROOT),
        )
        assert result.returncode != 0, (
            "Script should exit non-zero when a required tab file is missing"
        )
        assert "main_campus_closures" in result.stderr or "not found" in result.stderr.lower(), (
            f"Error should mention missing tab file. Got: {result.stderr}"
        )
    finally:
        shutil.rmtree(fixture, ignore_errors=True)
        shutil.rmtree(output_root, ignore_errors=True)


if __name__ == "__main__":
    tests = [
        test_script_exits_zero_with_valid_fixture,
        test_from_dir_requires_isolated_output_root,
        test_closure_status_json_structure,
        test_pipeline_summary_json_structure,
        test_exits_nonzero_on_missing_required_column,
        test_exits_nonzero_on_missing_tab_file,
    ]

    passed = 0
    failed = 0
    for test_fn in tests:
        try:
            print(f"\n--- {test_fn.__name__} ---")
            test_fn()
            print(f"PASS: {test_fn.__name__}")
            passed += 1
        except Exception as exc:  # noqa: BLE001
            print(f"FAIL: {test_fn.__name__}: {exc}")
            failed += 1

    print(f"\n=== Python tests: {passed} passed, {failed} failed ===")
    if failed:
        sys.exit(1)
