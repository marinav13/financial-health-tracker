import urllib.parse
import xml.etree.ElementTree as ET
from pathlib import Path

if __package__ in (None, ""):
    import sys

    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
    from cuts_discovery.common import lead_id_for, load_watchlist_rows, normalize_url, parse_date_to_iso, today_iso
else:
    from .common import lead_id_for, load_watchlist_rows, normalize_url, parse_date_to_iso, today_iso


WATCHLIST_QUERY_TEMPLATE = '"{institution_name}" ("layoffs" OR "laid off" OR "hiring freeze" OR furlough OR "program cuts" OR closure)'


def google_news_rss_url(query: str) -> str:
    return (
        "https://news.google.com/rss/search?q="
        + urllib.parse.quote(query + " when:7d")
        + "&hl=en-US&gl=US&ceid=US:en"
    )


def resolve_google_news_url(fetcher, url: str) -> str:
    try:
        resolved = fetcher.resolve_url(url)
        return normalize_url(resolved or url)
    except Exception:
        return normalize_url(url)


def harvest_query(fetcher, query: str, tier: str) -> list[dict]:
    body = fetcher.get(google_news_rss_url(query), max_age_days=0.9)
    root = ET.fromstring(body)
    leads = []
    for item in root.iter("item"):
        url = (item.findtext("link") or "").strip()
        if not url:
            continue
        final_url = resolve_google_news_url(fetcher, url)
        leads.append(
            {
                "lead_id": lead_id_for(final_url),
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
        )
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


def harvest_all(fetcher, standing_queries: list[str], watchlist_rows: list[dict] | None = None) -> list[dict]:
    leads = []
    for query in standing_queries:
        leads.extend(harvest_query(fetcher, query, "google_news"))
    for query in build_watchlist_queries(watchlist_rows or []):
        leads.extend(harvest_query(fetcher, query, "watchlist_feed"))
    return leads


def harvest_from_config(fetcher, queries_config: dict) -> list[dict]:
    return harvest_all(
        fetcher,
        standing_queries=queries_config.get("standing_queries", []),
        watchlist_rows=load_watchlist_rows(),
    )
