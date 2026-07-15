import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO_ROOT / "scripts"))

from cuts_discovery.classify_leads import archive_source_url  # noqa: E402
from cuts_discovery.run_discovery import archive_candidates  # noqa: E402


class _FakeArchiveResponse:
    def __init__(self, content_location: str):
        self.headers = {"Content-Location": content_location}

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb):  # noqa: ARG002
        return False


def test_archive_source_url_uses_configured_timeout_and_caches(tmp_path):
    calls = []

    def requester(req, timeout=60):
        calls.append({"url": req.full_url, "timeout": timeout})
        return _FakeArchiveResponse("/web/20260715/example")

    archived = archive_source_url(
        "https://example.org/story",
        requester=requester,
        cache_dir=tmp_path,
        timeout=7,
    )

    assert archived == "/web/20260715/example"
    assert calls == [
        {
            "url": "https://web.archive.org/save/https://example.org/story",
            "timeout": 7,
        }
    ]

    cached = archive_source_url(
        "https://example.org/story",
        requester=requester,
        cache_dir=tmp_path,
        timeout=99,
    )
    assert cached == "/web/20260715/example"
    assert len(calls) == 1


def test_archive_candidates_stops_when_time_budget_is_exhausted():
    attempted = []
    now_values = iter([0.0, 0.0, 0.6, 1.2, 1.2])

    def fake_now():
        return next(now_values)

    def fake_archive(url, timeout=10):
        attempted.append((url, timeout))

    candidates = [
        {"source_url": "https://example.org/1"},
        {"source_url": "https://example.org/2"},
        {"source_url": "https://example.org/3"},
    ]

    count = archive_candidates(
        candidates,
        archive_url=fake_archive,
        max_seconds=1.0,
        request_timeout=5,
        now_fn=fake_now,
    )

    assert count == 2
    assert attempted == [
        ("https://example.org/1", 5),
        ("https://example.org/2", 5),
    ]
