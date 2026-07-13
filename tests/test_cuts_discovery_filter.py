"""Rules-filter and assembly tests for the tranche-B discovery pipeline."""

import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO_ROOT / "scripts"))

from cuts_discovery.assemble_candidates import assemble_candidates  # noqa: E402
from cuts_discovery.filter_rules import filter_lead, load_mapping_rows  # noqa: E402


CONFIG = {
    "include_keywords": [
        "layoff",
        "laid off",
        "eliminat",
        "suspend",
        "discontinu",
        "furlough",
        "hiring freeze",
        "closure",
        "closing",
        "teach-out",
    ],
    "kill_patterns": [
        r"\b(football|basketball|roster|athletic)\b",
        r"\bschool district\b",
    ],
}


def main():
    mapping_rows = load_mapping_rows()

    sports_story = {
        "lead_id": "sports",
        "first_seen": "2026-07-09",
        "tier": "google_news",
        "query_or_feed": "fixture",
        "url": "https://example.org/sports",
        "publisher": "Campus Paper",
        "headline": "University of Alabama athletics announces layoffs after football losses",
        "published_date": "2026-07-09",
        "snippet": "Athletic department staff trimmed the roster before camp.",
        "status": "new",
        "status_reason": "",
    }
    filtered_sports = filter_lead(sports_story, CONFIG, mapping_rows)
    assert filtered_sports["status"] == "filtered_out", filtered_sports
    assert filtered_sports["status_reason"].startswith("kill:"), filtered_sports

    layoff_story = {
        "lead_id": "layoff",
        "first_seen": "2026-07-09",
        "tier": "google_news",
        "query_or_feed": "fixture",
        "url": "https://example.org/layoff",
        "publisher": "State Journal",
        "headline": "Temple University lays off 40 employees amid deficit",
        "published_date": "2026-07-09",
        "snippet": "Administrators said the university is eliminating dozens of positions.",
        "status": "new",
        "status_reason": "",
    }
    filtered_layoff = filter_lead(layoff_story, CONFIG, mapping_rows)
    assert filtered_layoff["status"] == "new", filtered_layoff
    assert filtered_layoff["cut_type_guess"] == "staff_layoff", filtered_layoff
    assert filtered_layoff["unitid_guess"] == "216339", filtered_layoff
    assert filtered_layoff["institution_name_guess"] == "Temple University", filtered_layoff

    freeze_story = {
        "lead_id": "freeze",
        "first_seen": "2026-07-10",
        "tier": "google_news",
        "query_or_feed": "fixture",
        "url": "https://example.org/freeze",
        "publisher": "Daily Bruin",
        "headline": "Regional University announces furloughs and hiring freeze",
        "published_date": "2026-07-10",
        "snippet": "Officials said the freeze starts immediately.",
        "status": "new",
        "status_reason": "",
    }
    filtered_freeze = filter_lead(freeze_story, CONFIG, mapping_rows)
    assert filtered_freeze["cut_type_guess"] == "hiring_freeze", filtered_freeze

    candidates, candidate_ids, suppressed_ids = assemble_candidates(
        [filtered_layoff, filtered_freeze],
        suppression_rows=[
            {
                "unitid": "216339",
                "cut_type": "staff_layoff",
                "announcement_date": "2026-07-08",
            }
        ],
    )
    assert candidate_ids == {"freeze"}, candidate_ids
    assert suppressed_ids == {"layoff"}, suppressed_ids
    assert len(candidates) == 1, candidates
    assert candidates[0]["cut_type"] == "hiring_freeze", candidates[0]
    assert candidates[0]["row_origin"] == "news_scan", candidates[0]

    print("cuts discovery filter tests: all passed.")


if __name__ == "__main__":
    main()
