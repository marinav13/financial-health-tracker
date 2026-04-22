"""
Unit tests for scripts/build_hcm_level2.py.

Tests cover the pure helper functions that process quarterly HCM snapshots
into school-level histories. No file I/O or Excel parsing required.

Run with: python tests/test_build_hcm_level2.py
"""

import sys
import unittest
from pathlib import Path

# ---------------------------------------------------------------------------
# Import the module without triggering __main__ execution.
# ---------------------------------------------------------------------------
REPO_ROOT = Path(__file__).resolve().parents[1]
SCRIPT = REPO_ROOT / "scripts" / "build_hcm_level2.py"

_spec = __import__("importlib.util", fromlist=["spec_from_file_location", "module_from_spec"])
spec = _spec.spec_from_file_location("build_hcm_level2", SCRIPT)
mod = _spec.module_from_spec(spec)
spec.loader.exec_module(mod)

normalize_opeid = mod.normalize_opeid
map_sector_label = mod.map_sector_label
resolve_ipeds_raw_path = mod.resolve_ipeds_raw_path
first_missing_snapshot = mod.first_missing_snapshot
build_histories = mod.build_histories
make_transition_rows = mod.make_transition_rows
build_downgrade_rows = mod.build_downgrade_rows
SOURCE_FILES = mod.SOURCE_FILES


# ---------------------------------------------------------------------------
# Helpers to build synthetic snapshot rows
# ---------------------------------------------------------------------------
SNAPSHOT_DATES = [item["snapshot_date"] for item in SOURCE_FILES]
SNAPSHOT_LABELS = [item["snapshot_label"] for item in SOURCE_FILES]


def make_row(opeid, snapshot_date, unitid="100001", institution_name="Test University",
             state="MA", sector="Private not-for-profit", reason=""):
    label = next(item["snapshot_label"] for item in SOURCE_FILES
                 if item["snapshot_date"] == snapshot_date)
    return {
        "opeid": opeid,
        "unitid": unitid,
        "institution_name": institution_name,
        "state": state,
        "sector": sector,
        "snapshot_date": snapshot_date,
        "snapshot_label": label,
        "reason_on_description": reason,
        "in_finance_tracker": "Yes",
        "hcm_level": "HCM2",
    }


class TestNormalizeOpeid(unittest.TestCase):
    def test_pads_short_number(self):
        self.assertEqual(normalize_opeid("12345"), "00012345")

    def test_strips_dashes(self):
        self.assertEqual(normalize_opeid("001234-56"), "00123456")

    def test_none_returns_empty(self):
        self.assertEqual(normalize_opeid(None), "")

    def test_empty_string_returns_empty(self):
        self.assertEqual(normalize_opeid(""), "")

    def test_already_8_digits_unchanged(self):
        self.assertEqual(normalize_opeid("00376503"), "00376503")


class TestMapSectorLabel(unittest.TestCase):
    def test_public(self):
        self.assertEqual(map_sector_label("Public"), "Public")

    def test_private_nonprofit(self):
        self.assertEqual(map_sector_label("Private Non-Profit"), "Private not-for-profit")

    def test_foreign_nonprofit(self):
        self.assertEqual(map_sector_label("Foreign Private Non-Profit"), "Private not-for-profit")

    def test_proprietary_maps_to_for_profit(self):
        self.assertEqual(map_sector_label("Proprietary"), "Private for-profit")

    def test_foreign_for_profit(self):
        self.assertEqual(map_sector_label("Foreign For-Profit"), "Private for-profit")

    def test_unknown_passes_through(self):
        self.assertEqual(map_sector_label("Unknown Type"), "Unknown Type")

    def test_none_returns_empty_string(self):
        self.assertEqual(map_sector_label(None), "")

    def test_empty_string_returns_empty_string(self):
        self.assertEqual(map_sector_label(""), "")


class TestIpedsRawPath(unittest.TestCase):
    def test_future_end_year_changes_default_raw_filename(self):
        path = resolve_ipeds_raw_path(start_year="2014", end_year="2026")
        self.assertEqual(path.name, "ipeds_financial_health_raw_2014_2026.csv")

    def test_explicit_path_overrides_years(self):
        path = resolve_ipeds_raw_path(start_year="2014", end_year="2026", explicit_path="tmp/custom.csv")
        self.assertEqual(path.name, "custom.csv")


class TestFirstMissingSnapshot(unittest.TestCase):
    """first_missing_snapshot(after_index, present_indices) returns the label
    of the first snapshot after `after_index` that is NOT in present_indices."""

    def test_next_snapshot_is_missing(self):
        # Present at index 0 only; index 1 is missing
        result = first_missing_snapshot(0, {0})
        self.assertEqual(result, SNAPSHOT_LABELS[1])

    def test_all_subsequent_present_returns_none(self):
        # All snapshots present — nothing missing after last index
        all_indices = set(range(len(SOURCE_FILES)))
        result = first_missing_snapshot(len(SOURCE_FILES) - 1, all_indices)
        self.assertIsNone(result)

    def test_skipped_middle_snapshot(self):
        # Present at 0 and 2 (skipping 1)
        result = first_missing_snapshot(0, {0, 2})
        self.assertEqual(result, SNAPSHOT_LABELS[1])


class TestBuildHistories(unittest.TestCase):
    def test_school_on_all_snapshots_is_on_latest(self):
        rows = [make_row("00012345", d) for d in SNAPSHOT_DATES]
        histories = build_histories(rows)
        hist = histories["00012345"]
        self.assertTrue(hist["on_latest_snapshot"])
        self.assertEqual(hist["first_snapshot_label"], SNAPSHOT_LABELS[0])
        self.assertEqual(hist["first_snapshot_absent_after_last_presence"], "")

    def test_school_only_on_first_snapshot_is_not_on_latest(self):
        rows = [make_row("00099999", SNAPSHOT_DATES[0])]
        histories = build_histories(rows)
        hist = histories["00099999"]
        self.assertFalse(hist["on_latest_snapshot"])
        # First missing snapshot after index 0 is index 1
        self.assertEqual(hist["first_snapshot_absent_after_last_presence"], SNAPSHOT_LABELS[1])

    def test_history_tracks_correct_snapshot_count(self):
        rows = [make_row("00011111", SNAPSHOT_DATES[0]),
                make_row("00011111", SNAPSHOT_DATES[1])]
        histories = build_histories(rows)
        self.assertEqual(len(histories["00011111"]["snapshots_present"]), 2)

    def test_multiple_schools_tracked_independently(self):
        rows = (
            [make_row("00000001", d) for d in SNAPSHOT_DATES] +
            [make_row("00000002", SNAPSHOT_DATES[0])]
        )
        histories = build_histories(rows)
        self.assertTrue(histories["00000001"]["on_latest_snapshot"])
        self.assertFalse(histories["00000002"]["on_latest_snapshot"])


class TestMakeTransitionRows(unittest.TestCase):
    def setUp(self):
        # Build a histories dict with one school present on all snapshots
        # and one that dropped off after the first
        rows_persistent = [make_row("00000001", d) for d in SNAPSHOT_DATES]
        rows_dropped = [make_row("00000002", SNAPSHOT_DATES[0])]
        self.histories = build_histories(rows_persistent + rows_dropped)

    def test_dropped_school_appears_with_require_all_future_false(self):
        result = make_transition_rows(self.histories, SNAPSHOT_DATES[0], require_all_future=False)
        opeids = [row["opeid"] for row in result]
        self.assertIn("00000002", opeids)

    def test_persistent_school_excluded_with_require_all_future_false(self):
        # require_all_future=False means "dropped off latest" — persistent school should be excluded
        result = make_transition_rows(self.histories, SNAPSHOT_DATES[0], require_all_future=False)
        opeids = [row["opeid"] for row in result]
        self.assertNotIn("00000001", opeids)

    def test_persistent_school_appears_with_require_all_future_true(self):
        result = make_transition_rows(self.histories, SNAPSHOT_DATES[0], require_all_future=True)
        opeids = [row["opeid"] for row in result]
        self.assertIn("00000001", opeids)

    def test_school_not_in_start_snapshot_excluded(self):
        # School 00000002 is only in the first snapshot; if we start from the second, it's excluded
        result = make_transition_rows(self.histories, SNAPSHOT_DATES[1], require_all_future=False)
        opeids = [row["opeid"] for row in result]
        self.assertNotIn("00000002", opeids)


class TestBuildDowngradeRows(unittest.TestCase):
    def test_school_with_hcm1_after_hcm2_is_a_downgrade(self):
        # School was on HCM2 in snapshot 0, then on HCM1 in snapshot 1
        hcm2_rows = [make_row("00000001", SNAPSHOT_DATES[0], unitid="100001")]
        hcm1_rows = [make_row("00000001", SNAPSHOT_DATES[1], unitid="100001")]
        hcm2_histories = build_histories(hcm2_rows)
        hcm1_histories = build_histories(hcm1_rows)
        result = build_downgrade_rows(hcm1_histories, hcm2_histories)
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]["opeid"], "00000001")

    def test_school_only_on_hcm2_is_not_a_downgrade(self):
        hcm2_rows = [make_row("00000001", SNAPSHOT_DATES[0], unitid="100001")]
        hcm2_histories = build_histories(hcm2_rows)
        result = build_downgrade_rows({}, hcm2_histories)
        self.assertEqual(len(result), 0)

    def test_school_on_hcm1_only_is_not_a_downgrade(self):
        hcm1_rows = [make_row("00000002", SNAPSHOT_DATES[0], unitid="100002")]
        hcm1_histories = build_histories(hcm1_rows)
        result = build_downgrade_rows(hcm1_histories, {})
        self.assertEqual(len(result), 0)


if __name__ == "__main__":
    result = unittest.main(exit=False, verbosity=2)
    passed = result.result.testsRun - len(result.result.failures) - len(result.result.errors)
    print(f"\n=== Results: {passed} passed, {len(result.result.failures) + len(result.result.errors)} failed ===")
    sys.exit(0 if result.result.wasSuccessful() else 1)
