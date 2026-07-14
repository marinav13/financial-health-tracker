import os
import re
import urllib.parse
import xml.etree.ElementTree as ET
from datetime import date, timedelta
from pathlib import Path

if __package__ in (None, ""):
    import sys

    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
    from cuts_discovery.common import lead_id_for, load_watchlist_rows, normalize_url, parse_date_to_iso, today_iso
else:
    from .common import lead_id_for, load_watchlist_rows, normalize_url, parse_date_to_iso, today_iso


WATCHLIST_QUERY_TEMPLATE = '"{institution_name}" ("layoffs" OR "laid off" OR "hiring freeze" OR furlough OR "program cuts" OR closure)'
DEFAULT_NEWS_WINDOW = "14d"
DATE_SLICE_DAYS = 14


def _build_google_news_rss_url(query: str, query_window: str) -> str:
    return (
        "https://news.google.com/rss/search?q="
        + urllib.parse.quote(f"{query} {query_window}".strip())
        + "&hl=en-US&gl=US&ceid=US:en"
    )


def configured_news_window() -> str:
    return (os.getenv("CUTS_NEWS_WINDOW") or DEFAULT_NEWS_WINDOW).strip().lower()


def configured_news_end_date() -> date | None:
    raw_value = (os.getenv("CUTS_NEWS_END_DATE") or "").strip()
    if not raw_value:
        return None
    try:
        return date.fromisoformat(raw_value)
    except ValueError:
        raise ValueError(f"Invalid CUTS_NEWS_END_DATE {raw_value!r}; expected YYYY-MM-DD.")


def _window_token_to_days(news_window: str) -> int | None:
    token = (news_window or "").strip().lower()
    match = re.fullmatch(r"(\d+)([dm])", token)
    if not match:
        return None
    count = int(match.group(1))
    unit = match.group(2)
    if unit == "d":
        return count
    return count * 30


def _window_token_to_start_date(news_window: str) -> date | None:
    token = (news_window or "").strip().lower()
    match = re.fullmatch(r"(?:from|since):(\d{4}-\d{2}-\d{2})", token)
    if not match:
        return None
    try:
        return date.fromisoformat(match.group(1))
    except ValueError:
        return None


def _date_slice_fragments_between(start_date: date, end_date: date) -> list[str]:
    if start_date >= end_date:
        return []
    fragments = []
    cursor = start_date
    while cursor < end_date:
        slice_end = min(cursor + timedelta(days=DATE_SLICE_DAYS), end_date)
        fragments.append(f"after:{cursor.isoformat()} before:{slice_end.isoformat()}")
        cursor = slice_end
    return fragments


def _date_slice_fragments(total_days: int, today: date) -> list[str]:
    if total_days <= 0:
        return []
    end_date = today + timedelta(days=1)
    start_date = end_date - timedelta(days=total_days)
    return _date_slice_fragments_between(start_date, end_date)


def google_news_rss_urls(query: str,
                         news_window: str | None = None,
                         today: date | None = None,
                         end_date: date | None = None) -> list[str]:
    token = (news_window or configured_news_window() or DEFAULT_NEWS_WINDOW).strip().lower()
    today = today or date.today()
    end_date = end_date or configured_news_end_date() or today
    if end_date > today:
        end_date = today

    if "after:" in token or "before:" in token:
        return [_build_google_news_rss_url(query, token)]

    start_date = _window_token_to_start_date(token)
    if start_date is not None:
        return [
            _build_google_news_rss_url(query, fragment)
            for fragment in _date_slice_fragments_between(start_date, end_date + timedelta(days=1))
        ]

    total_days = _window_token_to_days(token)
    if total_days is None:
        return [_build_google_news_rss_url(query, DEFAULT_NEWS_WINDOW)]

    # Empirical probe on 2026-07-13: Google News RSS honored day tokens such as
    # when:14d and when:30d, but month-style tokens like when:1m / when:3m
    # returned zero results. Wide backfills therefore use date-sliced
    # after:/before: queries instead of relying on long-window when: tokens.
    if token.endswith("d") and total_days <= 30:
        return [_build_google_news_rss_url(query, f"when:{token}")]

    return [_build_google_news_rss_url(query, fragment) for fragment in _date_slice_fragments(total_days, today)]


def google_news_rss_url(query: str,
                        news_window: str | None = None,
                        today: date | None = None,
                        end_date: date | None = None) -> str:
    return google_news_rss_urls(query, news_window=news_window, today=today, end_date=end_date)[0]


def resolve_google_news_url(fetcher, url: str) -> str:
    try:
        resolved = fetcher.resolve_url(url)
        return normalize_url(resolved or url)
    except Exception:
        return normalize_url(url)


def harvest_query(
    fetcher,
    query: str,
    tier: str,
    seen_lead_ids: set[str] | None = None,
    rss_urls: list[str] | None = None,
    on_rss_batch=None,
) -> list[dict]:
    leads = []
    seen_lead_ids = seen_lead_ids if seen_lead_ids is not None else set()
    for rss_url in (rss_urls or google_news_rss_urls(query)):
        body = fetcher.get(rss_url, max_age_days=0.9)
        root = ET.fromstring(body)
        batch_rows = []
        for item in root.iter("item"):
            url = (item.findtext("link") or "").strip()
            if not url:
                continue
            final_url = resolve_google_news_url(fetcher, url)
            lead_id = lead_id_for(final_url)
            if lead_id in seen_lead_ids:
                continue
            seen_lead_ids.add(lead_id)
            row = {
                "lead_id": lead_id,
                "first_seen": today_iso(),
                "tier": tier,
                "query_or_feed": query,
                "url": final_url,
                "publisher": (item.findtext("source") or "").strip(),
                "headline": (item.findtext("title") or "").strip(),
                "published_date": parse_date_to_iso(item.findtext("pubDate") or ""),
                "snippet": (item.findtext("description") or "").strip()[:500],
                "status": "new",
                "status_reason": "",
            }
            leads.append(row)
            batch_rows.append(row)
        if on_rss_batch is not None:
            on_rss_batch(query=query, tier=tier, rss_url=rss_url, rows=batch_rows)
    return leads


def build_watchlist_queries(rows: list[dict]) -> list[str]:
    queries = []
    seen = set()
    for row in rows:
        institution_name = (row.get("institution_name") or "").strip()
        if not institution_name:
            continue
        query = WATCHLIST_QUERY_TEMPLATE.format(institution_name=institution_name)
        if query in seen:
            continue
        seen.add(query)
        queries.append(query)
    return queries


def harvest_all(
    fetcher,
    standing_queries: list[str],
    watchlist_rows: list[dict] | None = None,
    max_watchlist_queries: int | None = None,
    on_rss_batch=None,
) -> list[dict]:
    leads = []
    seen_lead_ids: set[str] = set()
    for query in standing_queries:
        leads.extend(
            harvest_query(
                fetcher,
                query,
                "google_news",
                seen_lead_ids=seen_lead_ids,
                on_rss_batch=on_rss_batch,
            )
        )
    watchlist_queries = build_watchlist_queries(watchlist_rows or [])
    if max_watchlist_queries is not None:
        watchlist_queries = watchlist_queries[:max(0, max_watchlist_queries)]
    for query in watchlist_queries:
        leads.extend(
            harvest_query(
                fetcher,
                query,
                "watchlist_feed",
                seen_lead_ids=seen_lead_ids,
                on_rss_batch=on_rss_batch,
            )
        )
    return leads


def harvest_from_config(
    fetcher,
    queries_config: dict,
    max_watchlist_queries: int | None = None,
    on_rss_batch=None,
) -> list[dict]:
    return harvest_all(
        fetcher,
        standing_queries=queries_config.get("standing_queries", []),
        watchlist_rows=load_watchlist_rows(),
        max_watchlist_queries=max_watchlist_queries,
        on_rss_batch=on_rss_batch,
    )
