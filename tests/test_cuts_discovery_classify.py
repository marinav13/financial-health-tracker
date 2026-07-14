"""Classification and model-mode assembly tests for cuts discovery."""

import os
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO_ROOT / "scripts"))

from cuts_discovery.assemble_candidates import assemble_candidates, write_discovered_candidates  # noqa: E402
from cuts_discovery.classify_leads import parse_verdict, classify_survivors, write_classification_cache  # noqa: E402


class FakeFetcher:
    def __init__(self, bodies=None):
        self.bodies = bodies or {}

    def get(self, url: str, max_age_days: float = 6.0) -> bytes:  # noqa: ARG002
        if url not in self.bodies:
            raise RuntimeError(f"unexpected fetch: {url}")
        return self.bodies[url]


class FakeResponse:
    def __init__(self, payload: str, headers=None):
        self.payload = payload.encode("utf-8")
        self.headers = headers or {}

    def read(self):
        return self.payload

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb):  # noqa: ARG002
        return False


class FakeRequester:
    def __init__(self, payloads):
        self.payloads = list(payloads)
        self.calls = []

    def __call__(self, req, timeout=60):  # noqa: ARG002
        self.calls.append(req)
        payload = self.payloads.pop(0)
        return FakeResponse(payload)


class RaisingRequester:
    def __init__(self, exc: Exception):
        self.exc = exc
        self.calls = []

    def __call__(self, req, timeout=60):  # noqa: ARG002
        self.calls.append(req)
        raise self.exc


def test_classify_survivors_happy_path():
    lead = {
        "lead_id": "lead-1",
        "headline": "Temple University lays off 40 employees amid deficit",
        "publisher": "State Journal",
        "published_date": "2026-07-09",
        "snippet": "Administrators said the university is eliminating dozens of positions.",
        "url": "https://example.org/lead-1",
    }
    fetcher = FakeFetcher(
        {
            "https://example.org/lead-1": (
                b"<html><body><article><p>Temple University announced 40 layoffs to address a budget deficit.</p></article></body></html>"
            )
        }
    )
    payload = """{"content":[{"text":"{\\"is_cut\\":true,\\"confidence\\":\\"high\\",\\"institution_name\\":\\"Temple University\\",\\"state\\":\\"PA\\",\\"cut_type\\":\\"staff_layoff\\",\\"announcement_date\\":\\"2026-07-08\\",\\"scale_text\\":\\"40 employees\\",\\"summary\\":\\"Temple University laid off 40 employees to reduce a budget deficit.\\"}"}]}"""
    requester = FakeRequester([payload])

    with tempfile.TemporaryDirectory() as tmp:
        cache_path = Path(tmp) / "classifications.csv"
        old_key = os.environ.get("ANTHROPIC_API_KEY")
        os.environ["ANTHROPIC_API_KEY"] = "test-key"
        try:
            cache, errors = classify_survivors(
                [lead],
                fetcher=fetcher,
                mapping_rows=[],
                path=cache_path,
                requester=requester,
            )
        finally:
            if old_key is None:
                os.environ.pop("ANTHROPIC_API_KEY", None)
            else:
                os.environ["ANTHROPIC_API_KEY"] = old_key

        assert not errors, errors
        assert "lead-1" in cache, cache
        row = cache["lead-1"]
        assert row["is_cut"] == "true", row
        assert row["cut_type"] == "staff_layoff", row
        assert row["announcement_date"] == "2026-07-08", row
        assert len(requester.calls) == 1, len(requester.calls)


def test_classify_survivors_retries_on_malformed_json():
    lead = {
        "lead_id": "lead-2",
        "headline": "Regional University institutes hiring freeze",
        "publisher": "Local Gazette",
        "published_date": "2026-07-09",
        "snippet": "Officials said a hiring freeze begins immediately.",
        "url": "https://example.org/lead-2",
    }
    fetcher = FakeFetcher(
        {
            "https://example.org/lead-2": (
                b"<html><body><article><p>Regional University announced a hiring freeze.</p></article></body></html>"
            )
        }
    )
    requester = FakeRequester(
        [
            """{"content":[{"text":"not json"}]}""",
            """{"content":[{"text":"{\\"is_cut\\":true,\\"confidence\\":\\"medium\\",\\"institution_name\\":\\"Regional University\\",\\"state\\":\\"OH\\",\\"cut_type\\":\\"hiring_freeze\\",\\"announcement_date\\":\\"2026-07-09\\",\\"scale_text\\":null,\\"summary\\":\\"Regional University announced a hiring freeze.\\"}"}]}""",
        ]
    )

    with tempfile.TemporaryDirectory() as tmp:
        cache_path = Path(tmp) / "classifications.csv"
        old_key = os.environ.get("ANTHROPIC_API_KEY")
        os.environ["ANTHROPIC_API_KEY"] = "test-key"
        try:
            cache, errors = classify_survivors(
                [lead],
                fetcher=fetcher,
                mapping_rows=[],
                path=cache_path,
                requester=requester,
            )
        finally:
            if old_key is None:
                os.environ.pop("ANTHROPIC_API_KEY", None)
            else:
                os.environ["ANTHROPIC_API_KEY"] = old_key

        assert not errors, errors
        assert cache["lead-2"]["cut_type"] == "hiring_freeze", cache
        assert len(requester.calls) == 2, len(requester.calls)


def test_parse_verdict_unwraps_fenced_json():
    raw_text = """Model output:

```json
{
  "is_cut": true,
  "confidence": "high",
  "institution_name": "Temple University",
  "state": "PA",
  "cut_type": "staff_layoff",
  "announcement_date": "2026-07-08",
  "scale_text": "40 employees",
  "summary": "Temple University laid off 40 employees to reduce a budget deficit."
}
```
"""
    verdict = parse_verdict(raw_text)
    assert verdict["is_cut"] is True, verdict
    assert verdict["cut_type"] == "staff_layoff", verdict
    assert verdict["institution_name"] == "Temple University", verdict


def test_classify_survivors_skips_when_key_missing():
    lead = {
        "lead_id": "lead-3",
        "headline": "Example University story",
        "publisher": "Example Paper",
        "published_date": "2026-07-09",
        "snippet": "Example summary.",
        "url": "https://example.org/lead-3",
    }
    requester = FakeRequester([])
    with tempfile.TemporaryDirectory() as tmp:
        cache_path = Path(tmp) / "classifications.csv"
        old_key = os.environ.pop("ANTHROPIC_API_KEY", None)
        try:
            cache, errors = classify_survivors(
                [lead],
                fetcher=FakeFetcher(),
                mapping_rows=[],
                path=cache_path,
                requester=requester,
            )
        finally:
            if old_key is not None:
                os.environ["ANTHROPIC_API_KEY"] = old_key

        assert cache == {}, cache
        assert errors == set(), errors
        assert len(requester.calls) == 0, requester.calls


def test_classify_survivors_cache_hit_avoids_http():
    cached_row = {
        "lead_id": "lead-4",
        "classified_at": "2026-07-13",
        "model": "fixture-model",
        "is_cut": "true",
        "confidence": "high",
        "institution_name_raw": "Temple University",
        "unitid": "216339",
        "state": "Pennsylvania",
        "cut_type": "staff_layoff",
        "announcement_date": "2026-07-08",
        "scale_text": "40 employees",
        "summary": "Temple University laid off 40 employees to reduce a budget deficit.",
        "notes": "",
    }
    requester = FakeRequester([])
    with tempfile.TemporaryDirectory() as tmp:
        cache_path = Path(tmp) / "classifications.csv"
        write_classification_cache({"lead-4": cached_row}, cache_path)
        old_key = os.environ.get("ANTHROPIC_API_KEY")
        os.environ["ANTHROPIC_API_KEY"] = "test-key"
        try:
            cache, errors = classify_survivors(
                [{"lead_id": "lead-4", "url": "https://example.org/lead-4"}],
                fetcher=FakeFetcher(),
                mapping_rows=[],
                path=cache_path,
                requester=requester,
            )
        finally:
            if old_key is None:
                os.environ.pop("ANTHROPIC_API_KEY", None)
            else:
                os.environ["ANTHROPIC_API_KEY"] = old_key

        assert not errors, errors
        assert cache["lead-4"]["summary"] == cached_row["summary"], cache
        assert len(requester.calls) == 0, requester.calls


def test_classify_survivors_transport_error_marks_only_that_lead():
    cached_row = {
        "lead_id": "lead-cached",
        "classified_at": "2026-07-13",
        "model": "fixture-model",
        "is_cut": "true",
        "confidence": "high",
        "institution_name_raw": "Temple University",
        "unitid": "216339",
        "state": "Pennsylvania",
        "cut_type": "staff_layoff",
        "announcement_date": "2026-07-08",
        "scale_text": "40 employees",
        "summary": "Temple University laid off 40 employees to reduce a budget deficit.",
        "notes": "",
    }
    lead = {
        "lead_id": "lead-transport",
        "headline": "Regional University institutes hiring freeze",
        "publisher": "Local Gazette",
        "published_date": "2026-07-09",
        "snippet": "Officials said a hiring freeze begins immediately.",
        "url": "https://example.org/lead-transport",
    }
    fetcher = FakeFetcher(
        {
            "https://example.org/lead-transport": (
                b"<html><body><article><p>Regional University announced a hiring freeze.</p></article></body></html>"
            )
        }
    )
    requester = RaisingRequester(RuntimeError("HTTP 429"))

    with tempfile.TemporaryDirectory() as tmp:
        cache_path = Path(tmp) / "classifications.csv"
        write_classification_cache({"lead-cached": cached_row}, cache_path)
        old_key = os.environ.get("ANTHROPIC_API_KEY")
        os.environ["ANTHROPIC_API_KEY"] = "test-key"
        try:
            cache, errors = classify_survivors(
                [{"lead_id": "lead-cached", "url": "https://example.org/lead-cached"}, lead],
                fetcher=fetcher,
                mapping_rows=[],
                path=cache_path,
                requester=requester,
            )
        finally:
            if old_key is None:
                os.environ.pop("ANTHROPIC_API_KEY", None)
            else:
                os.environ["ANTHROPIC_API_KEY"] = old_key

        assert "lead-transport" in errors, errors
        assert "lead-cached" in cache, cache
        assert "lead-transport" not in cache, cache
        assert cache["lead-cached"]["summary"] == cached_row["summary"], cache
        assert len(requester.calls) == 1, requester.calls


def test_model_mode_candidate_golden_csv():
    survivors = [
        {
            "lead_id": "lead-a",
            "tier": "trade_feed",
            "url": "https://example.org/temple",
            "publisher": "Higher Ed Dive",
            "headline": "Temple University lays off 40 employees amid deficit",
            "published_date": "2026-07-09",
            "classification": {
                "is_cut": "true",
                "confidence": "high",
                "institution_name_raw": "Temple University",
                "unitid": "216339",
                "state": "Pennsylvania",
                "cut_type": "staff_layoff",
                "announcement_date": "2026-07-08",
                "scale_text": "40 employees",
                "summary": "Temple University laid off 40 employees to reduce a budget deficit.",
            },
        },
        {
            "lead_id": "lead-b",
            "tier": "google_news",
            "url": "https://example.org/proposal",
            "publisher": "Local Gazette",
            "headline": "Budget proposal sparks debate",
            "published_date": "2026-07-09",
            "classification": {
                "is_cut": "false",
                "confidence": "medium",
                "institution_name_raw": "Budget Proposal College",
                "unitid": "",
                "state": "",
                "cut_type": "",
                "announcement_date": "",
                "scale_text": "",
                "summary": "",
            },
        },
        {
            "lead_id": "lead-c",
            "tier": "google_news",
            "url": "https://example.org/multi",
            "publisher": "Student Paper",
            "headline": "University system cuts several offerings",
            "published_date": "2026-07-10",
            "classification": {
                "is_cut": "true",
                "confidence": "low",
                "institution_name_raw": "Regional University",
                "unitid": "",
                "state": "Ohio",
                "cut_type": "other",
                "announcement_date": "2026-07-10",
                "scale_text": "",
                "summary": "Regional University announced several cuts but the exact category is unclear.",
            },
        },
    ]
    candidates, _, _ = assemble_candidates(survivors, suppression_rows=[])
    with tempfile.TemporaryDirectory() as tmp:
        actual_path = Path(tmp) / "actual.csv"
        write_discovered_candidates(candidates, actual_path)
        expected_path = REPO_ROOT / "tests" / "fixtures" / "cuts_discovery" / "classified_candidates_expected.csv"
        actual = actual_path.read_text(encoding="utf-8")
        expected = expected_path.read_text(encoding="utf-8")
        assert actual == expected, f"Expected:\n{expected}\nActual:\n{actual}"


def main():
    tests = [
        test_classify_survivors_happy_path,
        test_classify_survivors_retries_on_malformed_json,
        test_parse_verdict_unwraps_fenced_json,
        test_classify_survivors_skips_when_key_missing,
        test_classify_survivors_cache_hit_avoids_http,
        test_classify_survivors_transport_error_marks_only_that_lead,
        test_model_mode_candidate_golden_csv,
    ]
    for test in tests:
        test()
    print("cuts discovery classify tests: all passed.")


if __name__ == "__main__":
    main()
