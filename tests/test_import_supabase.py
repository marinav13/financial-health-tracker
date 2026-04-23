"""
Smoke tests for scripts/import_supabase_institution_mapping.py.

Tests the normalize_name() function, MANUAL_ALIASES, and EXCLUDED_INSTITUTIONS
dictionaries without hitting the live Supabase API or writing any files.

Run with: python tests/test_import_supabase.py
"""

import json
import sys
import tempfile
from pathlib import Path

# Add repo root to path so we can import the script's module-level objects
REPO_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO_ROOT / "scripts"))

# Import only the pure functions and constants (no side-effects at module level)
from import_supabase_institution_mapping import (  # noqa: E402
    normalize_name,
    MANUAL_ALIASES,
    EXCLUDED_INSTITUTIONS,
    find_latest_ipeds_canonical_path,
    load_ipeds_lookup,
)


# ---------------------------------------------------------------------------
# normalize_name
# ---------------------------------------------------------------------------

def test_normalize_name_lowercases():
    assert normalize_name("Harvard University") == "harvard university"


def test_normalize_name_strips_leading_the():
    assert normalize_name("The Ohio State University") == "ohio state university"


def test_normalize_name_strips_suny_prefix():
    assert normalize_name("SUNY Buffalo") == "buffalo"


def test_normalize_name_expands_st_abbreviation():
    # "st" word boundary → "saint"
    assert normalize_name("St. John's University") == "saint john s university"
    assert normalize_name("St Xavier University") == "saint xavier university"


def test_normalize_name_replaces_ampersand():
    # & expands to " and " then whitespace is collapsed
    assert normalize_name("Arts & Sciences College") == "arts and sciences college"


def test_normalize_name_strips_main_campus_suffix():
    assert normalize_name("MIT - Main Campus") == "mit"


def test_normalize_name_collapses_whitespace():
    # Leading/trailing and internal runs of whitespace are collapsed
    assert normalize_name("  Example  College  ") == "example college"


def test_normalize_name_empty_string():
    assert normalize_name("") == ""


def test_normalize_name_none():
    assert normalize_name(None) == ""


def test_normalize_name_cuts_shared_fixtures():
    """Drift guard: Python normalize_name() must match every case in
    tests/fixtures/name_normalization_cuts.json, which is also consumed by
    the R test `test_name_normalization.R`. If either side changes behavior
    without updating the fixture, whichever side still matches keeps
    passing and the other side fails. Fix the mismatch or update the
    fixture (in which case both sides are forced to stay in sync)."""
    fixture_path = REPO_ROOT / "tests" / "fixtures" / "name_normalization_cuts.json"
    with fixture_path.open("r", encoding="utf-8") as handle:
        fixture = json.load(handle)
    mismatches = []
    for case in fixture["cases"]:
        input_value = case["input"]
        expected = case["expected"]
        actual = normalize_name(input_value)
        if actual != expected:
            mismatches.append((input_value, expected, actual))
    assert not mismatches, (
        "Python normalize_name() drifted from shared fixtures. "
        "R mirror is in scripts/shared/name_normalization.R::normalize_name_cuts. "
        f"Mismatches: {mismatches}"
    )


# ---------------------------------------------------------------------------
# MANUAL_ALIASES structural invariants
# ---------------------------------------------------------------------------

def test_manual_aliases_keys_are_tuples_of_strings():
    for key in MANUAL_ALIASES:
        assert isinstance(key, tuple) and len(key) == 2, (
            f"MANUAL_ALIASES key must be a 2-tuple: {key!r}"
        )
        api_name, state = key
        assert isinstance(api_name, str) and api_name, (
            f"api_name in key must be a non-empty string: {key!r}"
        )
        assert isinstance(state, str) and state, (
            f"state in key must be a non-empty string: {key!r}"
        )


def test_manual_alias_unitids_are_numeric_strings():
    for key, uid in MANUAL_ALIASES.items():
        assert isinstance(uid, str) and uid.isdigit(), (
            f"Unitid for {key!r} must be a numeric string, got {uid!r}"
        )


def test_manual_alias_unitids_are_plausible_ipeds_range():
    # IPEDS unitids are 6-digit integers (roughly 100000–999999)
    for key, uid in MANUAL_ALIASES.items():
        assert 100_000 <= int(uid) <= 999_999, (
            f"Unitid {uid!r} for {key!r} is outside the plausible IPEDS range"
        )


# ---------------------------------------------------------------------------
# EXCLUDED_INSTITUTIONS structural invariants
# ---------------------------------------------------------------------------

def test_excluded_institutions_keys_are_tuples_of_strings():
    for key in EXCLUDED_INSTITUTIONS:
        assert isinstance(key, tuple) and len(key) == 2, (
            f"EXCLUDED_INSTITUTIONS key must be a 2-tuple: {key!r}"
        )


def test_excluded_institutions_have_non_empty_reasons():
    for key, reason in EXCLUDED_INSTITUTIONS.items():
        assert isinstance(reason, str) and reason.strip(), (
            f"Reason for excluded institution {key!r} must be a non-empty string"
        )


# ---------------------------------------------------------------------------
# No overlap between the two dicts
# ---------------------------------------------------------------------------

def test_no_overlap_between_aliases_and_excluded():
    overlap = set(MANUAL_ALIASES.keys()) & set(EXCLUDED_INSTITUTIONS.keys())
    assert not overlap, (
        f"These keys appear in both MANUAL_ALIASES and EXCLUDED_INSTITUTIONS: {overlap}"
    )


# ---------------------------------------------------------------------------
# Coverage: all 33 previously-unmatched institutions are handled
# ---------------------------------------------------------------------------

# This list was captured from a run of the script before H5 was implemented.
# Every entry must now be in MANUAL_ALIASES or EXCLUDED_INSTITUTIONS.
PREVIOUSLY_UNMATCHED = [
    ("Birmingham-Southern College",                        "Alabama"),
    ("University of Saint Katherine",                      "California"),
    ("Santa Monica College",                               "California"),
    ("Napa Valley College",                                "California"),
    ("Delaware College of Art and Design",                 "Delaware"),
    ("Johnson University Florida",                         "Florida"),
    ("Hodges University",                                  "Florida"),
    ("Oak Point University",                               "Illinois"),
    ("University of Illinois UC",                          "Illinois"),
    ("Indiana University",                                 "Indiana"),
    ("Sullivan University",                                "Kentucky"),
    ("University of Minnesota Extension",                  "Minnesota"),
    ("Magdalen College of the Liberal Arts",               "New Hampshire"),
    ("Rutgers University",                                 "New Jersey"),
    ("Wells College",                                      "New York"),
    ("The King's College",                                 "New York"),
    ("State University of New York",                       "New York"),
    ("Texas A&M University at Qatar",                      "Non-US"),
    ("Cuyahoga Community College",                         "Ohio"),
    ("Notre Dame College",                                 "Ohio"),
    ("Bacone College",                                     "Oklahoma"),
    ("University of Oklahoma",                             "Oklahoma"),
    ("Pittsburgh Technical College",                       "Pennsylvania"),
    ("Cabrini University",                                 "Pennsylvania"),
    ("Harrisburg Area Community College",                  "Pennsylvania"),
    ("University of Texas HSCH",                           "Texas"),
    ("Tarrant County College",                             "Texas"),
    ("Salt Lake Community College",                        "Utah"),
    ("Goddard College",                                    "Vermont"),
    ("Milwaukee Area Technical College",                   "Wisconsin"),
    ("University of Wisconsin Law School",                 "Wisconsin"),
    ("University of Wisconsin-Platteville Baraboo Sauk County", "Wisconsin"),
    ("Western Wyoming Community College",                  "Wyoming"),
]


def test_all_previously_unmatched_are_now_handled():
    unhandled = []
    for api_name, state in PREVIOUSLY_UNMATCHED:
        key = (api_name, state)
        if key not in MANUAL_ALIASES and key not in EXCLUDED_INSTITUTIONS:
            unhandled.append(key)
    assert not unhandled, (
        f"These institutions are still unhandled (add to MANUAL_ALIASES or "
        f"EXCLUDED_INSTITUTIONS):\n" + "\n".join(f"  {k!r}" for k in unhandled)
    )


def test_previously_unmatched_count_matches_expected():
    # PREVIOUSLY_UNMATCHED is a frozen historical snapshot — it should never change.
    assert len(PREVIOUSLY_UNMATCHED) == 33, (
        "Update this test if the list of previously-unmatched institutions changes."
    )


# ---------------------------------------------------------------------------
# Baseline coverage sets (derived from H5 resolution of PREVIOUSLY_UNMATCHED)
#
# These are SUBSET checks, not equality checks.  Adding new entries to
# MANUAL_ALIASES or EXCLUDED_INSTITUTIONS will NOT break these tests.
# Only accidentally *removing* a baseline entry triggers a failure.
# ---------------------------------------------------------------------------

# The 21 previously-unmatched institutions that were resolved via MANUAL_ALIASES.
_BASELINE_ALIASES: frozenset = frozenset([
    ("Birmingham-Southern College",                             "Alabama"),
    ("University of Saint Katherine",                          "California"),
    ("Delaware College of Art and Design",                     "Delaware"),
    ("Johnson University Florida",                             "Florida"),
    ("Hodges University",                                      "Florida"),
    ("Oak Point University",                                   "Illinois"),
    ("University of Illinois UC",                              "Illinois"),
    ("Indiana University",                                     "Indiana"),
    ("Sullivan University",                                    "Kentucky"),
    ("Magdalen College of the Liberal Arts",                   "New Hampshire"),
    ("Rutgers University",                                     "New Jersey"),
    ("Wells College",                                          "New York"),
    ("The King's College",                                     "New York"),
    ("Notre Dame College",                                     "Ohio"),
    ("Bacone College",                                         "Oklahoma"),
    ("University of Oklahoma",                                 "Oklahoma"),
    ("Pittsburgh Technical College",                           "Pennsylvania"),
    ("Cabrini University",                                     "Pennsylvania"),
    ("University of Texas HSCH",                              "Texas"),
    ("Goddard College",                                        "Vermont"),
    ("University of Wisconsin-Platteville Baraboo Sauk County", "Wisconsin"),
])

# The 12 previously-unmatched institutions that were explicitly excluded.
_BASELINE_EXCLUSIONS: frozenset = frozenset([
    ("Santa Monica College",              "California"),
    ("Napa Valley College",               "California"),
    ("University of Minnesota Extension", "Minnesota"),
    ("State University of New York",      "New York"),
    ("Texas A&M University at Qatar",     "Non-US"),
    ("Cuyahoga Community College",        "Ohio"),
    ("Harrisburg Area Community College", "Pennsylvania"),
    ("Tarrant County College",            "Texas"),
    ("Salt Lake Community College",       "Utah"),
    ("Milwaukee Area Technical College",  "Wisconsin"),
    ("University of Wisconsin Law School","Wisconsin"),
    ("Western Wyoming Community College", "Wyoming"),
])


def test_manual_aliases_retain_baseline_coverage():
    """No baseline alias may be silently removed.

    Adding new entries to MANUAL_ALIASES is fine and will not break this test.
    Only removing an entry that was part of the H5 resolution set triggers failure.
    """
    missing = _BASELINE_ALIASES - set(MANUAL_ALIASES.keys())
    assert not missing, (
        "These institutions were removed from MANUAL_ALIASES but are still "
        "required by the baseline coverage set:\n"
        + "\n".join(f"  {k!r}" for k in sorted(missing))
    )


def test_excluded_institutions_retain_baseline_coverage():
    """No baseline exclusion may be silently removed.

    Adding new entries to EXCLUDED_INSTITUTIONS is fine and will not break this test.
    Only removing an entry that was part of the H5 resolution set triggers failure.
    """
    missing = _BASELINE_EXCLUSIONS - set(EXCLUDED_INSTITUTIONS.keys())
    assert not missing, (
        "These institutions were removed from EXCLUDED_INSTITUTIONS but are still "
        "required by the baseline coverage set:\n"
        + "\n".join(f"  {k!r}" for k in sorted(missing))
    )


def test_baseline_sets_are_disjoint():
    """Sanity-check the baseline sets themselves don't overlap."""
    overlap = _BASELINE_ALIASES & _BASELINE_EXCLUSIONS
    assert not overlap, (
        f"_BASELINE_ALIASES and _BASELINE_EXCLUSIONS share keys: {overlap}"
    )


def test_baseline_sets_cover_previously_unmatched():
    """Every entry in PREVIOUSLY_UNMATCHED is accounted for in one baseline set."""
    combined = _BASELINE_ALIASES | _BASELINE_EXCLUSIONS
    uncovered = set(PREVIOUSLY_UNMATCHED) - combined
    assert not uncovered, (
        "These PREVIOUSLY_UNMATCHED entries are not in either baseline set "
        "(add them to _BASELINE_ALIASES or _BASELINE_EXCLUSIONS):\n"
        + "\n".join(f"  {k!r}" for k in sorted(uncovered))
    )


# ---------------------------------------------------------------------------
# Canonical IPEDS lookup path selection
# ---------------------------------------------------------------------------

def test_find_latest_ipeds_canonical_path_uses_newest_end_year():
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        older = root / "ipeds_financial_health_canonical_2014_2024.csv"
        newer = root / "ipeds_financial_health_canonical_2015_2025.csv"
        older.write_text("unitid,institution_name,state\n", encoding="utf-8")
        newer.write_text("unitid,institution_name,state\n", encoding="utf-8")

        assert Path(find_latest_ipeds_canonical_path(str(root))).name == newer.name


def test_find_latest_ipeds_canonical_path_fails_when_missing():
    with tempfile.TemporaryDirectory() as tmp:
        try:
            find_latest_ipeds_canonical_path(tmp)
        except FileNotFoundError as exc:
            assert "No canonical IPEDS CSV found" in str(exc)
        else:
            raise AssertionError("Expected FileNotFoundError for missing canonical CSV")


def test_load_ipeds_lookup_fails_when_file_missing():
    try:
        load_ipeds_lookup("missing-canonical-file.csv")
    except FileNotFoundError as exc:
        assert "IPEDS canonical dataset not found" in str(exc)
    else:
        raise AssertionError("Expected FileNotFoundError for missing canonical CSV")


# ---------------------------------------------------------------------------
# Runner
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    tests = [
        test_normalize_name_lowercases,
        test_normalize_name_strips_leading_the,
        test_normalize_name_strips_suny_prefix,
        test_normalize_name_expands_st_abbreviation,
        test_normalize_name_replaces_ampersand,
        test_normalize_name_strips_main_campus_suffix,
        test_normalize_name_collapses_whitespace,
        test_normalize_name_empty_string,
        test_normalize_name_none,
        test_normalize_name_cuts_shared_fixtures,
        test_manual_aliases_keys_are_tuples_of_strings,
        test_manual_alias_unitids_are_numeric_strings,
        test_manual_alias_unitids_are_plausible_ipeds_range,
        test_excluded_institutions_keys_are_tuples_of_strings,
        test_excluded_institutions_have_non_empty_reasons,
        test_no_overlap_between_aliases_and_excluded,
        test_all_previously_unmatched_are_now_handled,
        test_previously_unmatched_count_matches_expected,
        test_manual_aliases_retain_baseline_coverage,
        test_excluded_institutions_retain_baseline_coverage,
        test_baseline_sets_are_disjoint,
        test_baseline_sets_cover_previously_unmatched,
        test_find_latest_ipeds_canonical_path_uses_newest_end_year,
        test_find_latest_ipeds_canonical_path_fails_when_missing,
        test_load_ipeds_lookup_fails_when_file_missing,
    ]

    passed = failed = 0
    for fn in tests:
        try:
            print(f"--- {fn.__name__}")
            fn()
            print(f"PASS")
            passed += 1
        except Exception as exc:  # noqa: BLE001
            print(f"FAIL: {exc}")
            failed += 1

    print(f"\n=== {passed} passed, {failed} failed ===")
    if failed:
        sys.exit(1)
