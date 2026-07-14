"""Fixture-backed tests for RSS and Google News discovery harvesting."""

import os
import tempfile
from datetime import date
from pathlib import Path
import sys

REPO_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO_ROOT / "scripts"))

from cuts_discovery.common import LEAD_FIELDS, append_leads, read_csv_rows  # noqa: E402
from cuts_discovery.harvest_feeds import harvest_feed  # noqa: E402
from cuts_discovery.harvest_google_news import google_news_rss_url, google_news_rss_urls, harvest_all, harvest_query  # noqa: E402


class FakeFetcher:
    def __init__(self, bodies: dict[str, bytes], resolved_urls: dict[str, str] | None = None):
        self.bodies = bodies
        self.resolved_urls = resolved_urls or {}

    def get(self, url: str, max_age_days: float = 6.0) -> bytes:  # noqa: ARG002
        return self.bodies[url]

    def resolve_url(self, url: str) -> str:
        return self.resolved_urls.get(url, url)


def main():
    fixture_dir = REPO_ROOT / "tests" / "fixtures" / "cuts_discovery"
    feed_xml = (fixture_dir / "trade_feed.xml").read_bytes()
    google_xml = (fixture_dir / "google_news.xml").read_bytes()
    previous_news_window = os.environ.get("CUTS_NEWS_WINDOW")

    try:
        os.environ.pop("CUTS_NEWS_WINDOW", None)
        fetcher = FakeFetcher(
            bodies={
                "https://feed.example.org/rss": feed_xml,
                google_news_rss_url("example"): google_xml,
            },
            resolved_urls={
                "https://news.google.com/rss/articles/CBMiYWh0dHBzOi8vZXhhbXBsZS5vcmcvcmVnaW9uYWwtdW5pdmVyc2l0eS1mcmVlemXSAQA":
                "https://example.org/regional-university-freeze?utm_source=gn"
            },
        )

        feed_rows = harvest_feed(
            fetcher,
            {
                "name": "trade_feed_fixture",
                "url": "https://feed.example.org/rss",
                "tier": "trade_feed",
            },
        )
        assert len(feed_rows) == 2, feed_rows
        assert feed_rows[0]["publisher"] == "Higher Ed Trade Feed", feed_rows[0]
        assert feed_rows[0]["published_date"] == "2026-07-07", feed_rows[0]

        google_rows = harvest_query(fetcher, "example", "google_news")
        assert len(google_rows) == 1, google_rows
        assert google_rows[0]["publisher"] == "Local Gazette", google_rows[0]
        assert google_rows[0]["url"] == "https://example.org/regional-university-freeze", google_rows[0]

        with tempfile.TemporaryDirectory() as tmp:
            leads_path = Path(tmp) / "leads.csv"
            assert append_leads(feed_rows + google_rows, leads_path=leads_path) == 3
            assert append_leads(feed_rows + google_rows, leads_path=leads_path) == 0
            stored = read_csv_rows(leads_path, LEAD_FIELDS)
            assert len(stored) == 3, stored

        os.environ["CUTS_NEWS_WINDOW"] = "30d"
        assert "when%3A30d" in google_news_rss_url("example"), google_news_rss_url("example")

        wide_urls = google_news_rss_urls("example", news_window="90d", today=date(2026, 7, 13))
        assert len(wide_urls) > 1, wide_urls
        assert all("after%3A" in url and "before%3A" in url for url in wide_urls), wide_urls
        assert "when%3A90d" not in " ".join(wide_urls), wide_urls

        historical_urls = google_news_rss_urls("example", news_window="from:2024-01-01", today=date(2024, 1, 31))
        assert len(historical_urls) == 3, historical_urls
        assert historical_urls[0].endswith("after%3A2024-01-01%20before%3A2024-01-15&hl=en-US&gl=US&ceid=US:en"), historical_urls[0]
        assert historical_urls[-1].endswith("after%3A2024-01-29%20before%3A2024-02-01&hl=en-US&gl=US&ceid=US:en"), historical_urls[-1]

        limited_rows = harvest_all(
            fetcher,
            standing_queries=[],
            watchlist_rows=[
                {"institution_name": "Alpha College"},
                {"institution_name": "Beta University"},
            ],
            max_watchlist_queries=0,
        )
        assert len(limited_rows) == 0, limited_rows
    finally:
        if previous_news_window is None:
            os.environ.pop("CUTS_NEWS_WINDOW", None)
        else:
            os.environ["CUTS_NEWS_WINDOW"] = previous_news_window

    print("cuts discovery harvest tests: all passed.")


if __name__ == "__main__":
    main()
