import xml.etree.ElementTree as ET
from pathlib import Path

if __package__ in (None, ""):
    import sys

    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
    from cuts_discovery.common import lead_id_for, load_feeds_config, parse_date_to_iso, today_iso
else:
    from .common import lead_id_for, load_feeds_config, parse_date_to_iso, today_iso


def harvest_feed(fetcher, feed_config: dict) -> list[dict]:
    body = fetcher.get(feed_config["url"], max_age_days=0.9)
    root = ET.fromstring(body)
    channel = root.find("channel")
    channel_title = ""
    if channel is not None:
        channel_title = (channel.findtext("title") or "").strip()
    leads = []
    for item in root.iter("item"):
        url = (item.findtext("link") or "").strip()
        if not url:
            continue
        leads.append(
            {
                "lead_id": lead_id_for(url),
                "first_seen": today_iso(),
                "tier": feed_config["tier"],
                "query_or_feed": feed_config["name"],
                "url": url,
                "publisher": (item.findtext("source") or channel_title or feed_config["name"]).strip(),
                "headline": (item.findtext("title") or "").strip(),
                "published_date": parse_date_to_iso(item.findtext("pubDate") or ""),
                "snippet": (item.findtext("description") or "").strip()[:500],
                "status": "new",
                "status_reason": "",
            }
        )
    return leads


def harvest_all(fetcher, config_path=None) -> list[dict]:
    config = load_feeds_config(config_path) if config_path else load_feeds_config()
    leads = []
    for feed in config["feeds"]:
        leads.extend(harvest_feed(fetcher, feed))
    return leads
