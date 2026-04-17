"""
Unit tests for scripts/build_federal_composite_scores.py.

Tests cover the pure helper functions that transform raw CSV data into
the JSON lookup consumed by school profile pages. No file I/O required.

Run with: python tests/test_build_federal_composite_scores.py
"""

import sys
import types
import unittest
from pathlib import Path

# ---------------------------------------------------------------------------
# Import the module without triggering __main__ execution.
# We load only the helper functions — not main() — so no input files needed.
# ---------------------------------------------------------------------------
REPO_ROOT = Path(__file__).resolve().parents[1]
SCRIPT = REPO_ROOT / "scripts" / "build_federal_composite_scores.py"

_spec = __import__("importlib.util", fromlist=["spec_from_file_location", "module_from_spec"])
spec = _spec.spec_from_file_location("build_federal_composite_scores", SCRIPT)
mod = _spec.module_from_spec(spec)
spec.loader.exec_module(mod)

normalize_opeid = mod.normalize_opeid
parse_float = mod.parse_float
composite_status = mod.composite_status
composite_status_label = mod.composite_status_label


class TestNormalizeOpeid(unittest.TestCase):
    def test_pads_short_number_to_8_digits(self):
        self.assertEqual(normalize_opeid("12345"), "00012345")

    def test_strips_leading_zeros_then_repads(self):
        # Already 8 digits — no change
        self.assertEqual(normalize_opeid("00123456"), "00123456")

    def test_removes_non_digit_characters(self):
        self.assertEqual(normalize_opeid("001234-56"), "00123456")

    def test_empty_string_returns_empty(self):
        self.assertEqual(normalize_opeid(""), "")

    def test_none_returns_empty(self):
        self.assertEqual(normalize_opeid(None), "")

    def test_whitespace_only_returns_empty(self):
        self.assertEqual(normalize_opeid("   "), "")

    def test_full_8_digit_opeid_unchanged(self):
        self.assertEqual(normalize_opeid("00376503"), "00376503")

    def test_numeric_int_input(self):
        # Some CSV readers may return integers
        self.assertEqual(normalize_opeid(376503), "00376503")


class TestParseFloat(unittest.TestCase):
    def test_valid_positive_float(self):
        self.assertAlmostEqual(parse_float("1.5"), 1.5)

    def test_valid_negative_float(self):
        self.assertAlmostEqual(parse_float("-0.5"), -0.5)

    def test_integer_string(self):
        self.assertAlmostEqual(parse_float("2"), 2.0)

    def test_empty_string_returns_none(self):
        self.assertIsNone(parse_float(""))

    def test_non_numeric_string_returns_none(self):
        self.assertIsNone(parse_float("N/A"))

    def test_none_returns_none(self):
        self.assertIsNone(parse_float(None))


class TestCompositeStatus(unittest.TestCase):
    def test_score_above_threshold_is_responsible(self):
        self.assertEqual(composite_status(1.5), "financially_responsible")

    def test_score_well_above_threshold_is_responsible(self):
        self.assertEqual(composite_status(3.0), "financially_responsible")

    def test_score_exactly_at_lower_threshold_is_additional_oversight(self):
        self.assertEqual(composite_status(1.0), "additional_oversight")

    def test_score_between_thresholds_is_additional_oversight(self):
        self.assertEqual(composite_status(1.2), "additional_oversight")

    def test_score_below_lower_threshold_is_not_responsible(self):
        self.assertEqual(composite_status(0.9), "not_financially_responsible")

    def test_negative_score_is_not_responsible(self):
        self.assertEqual(composite_status(-1.0), "not_financially_responsible")

    def test_none_score_returns_none(self):
        self.assertIsNone(composite_status(None))


class TestCompositeStatusLabel(unittest.TestCase):
    def test_financially_responsible_label(self):
        self.assertEqual(
            composite_status_label("financially_responsible"),
            "Financially responsible"
        )

    def test_additional_oversight_label(self):
        self.assertIn("additional oversight", composite_status_label("additional_oversight"))

    def test_not_financially_responsible_label(self):
        self.assertIn("Not", composite_status_label("not_financially_responsible"))

    def test_unknown_status_returns_none(self):
        self.assertIsNone(composite_status_label("unknown_value"))

    def test_roundtrip_through_composite_status(self):
        # Any valid score should produce a non-None label
        for score in [3.0, 1.5, 1.2, 1.0, 0.5, -1.0]:
            status = composite_status(score)
            label = composite_status_label(status)
            self.assertIsNotNone(label, f"Expected non-None label for score {score}")


if __name__ == "__main__":
    result = unittest.main(exit=False, verbosity=2)
    passed = result.result.testsRun - len(result.result.failures) - len(result.result.errors)
    print(f"\n=== Results: {passed} passed, {len(result.result.failures) + len(result.result.errors)} failed ===")
    sys.exit(0 if result.result.wasSuccessful() else 1)
