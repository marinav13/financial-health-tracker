"""Parity test for the discovery-side institution-name normalizer."""

import json
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO_ROOT / "scripts"))

from cuts_discovery.filter_rules import normalize_name  # noqa: E402


def main():
    fixture_path = REPO_ROOT / "tests" / "fixtures" / "name_normalization_cuts.json"
    fixture = json.loads(fixture_path.read_text(encoding="utf-8"))
    mismatches = []
    for case in fixture["cases"]:
        actual = normalize_name(case["input"])
        if actual != case["expected"]:
            mismatches.append((case["input"], case["expected"], actual))
    assert not mismatches, mismatches
    print("cuts discovery name parity tests: all passed.")


if __name__ == "__main__":
    main()
