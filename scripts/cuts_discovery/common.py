import csv
import hashlib
import io
import time
import urllib.parse
import urllib.request
from pathlib import Path


DISCOVERY_DIR = Path("data_pipelines/college_cuts/discovery")
LEADS_CSV = DISCOVERY_DIR / "leads.csv"
LEAD_FIELDS = [
    "lead_id",
    "first_seen",
    "tier",
    "query_or_feed",
    "url",
    "publisher",
    "headline",
    "published_date",
    "snippet",
    "status",
    "status_reason",
]
USER_AGENT = "HechingerFinancialHealthTracker/1.0 (+https://financialtracker.hechingerreport.org/)"
TRACKING_PARAMS = {
    "utm_source",
    "utm_medium",
    "utm_campaign",
    "utm_term",
    "utm_content",
    "fbclid",
    "gclid",
}


def normalize_url(url: str) -> str:
    parts = urllib.parse.urlsplit((url or "").strip())
    query = urllib.parse.parse_qsl(parts.query, keep_blank_values=True)
    filtered_query = [(key, value) for key, value in query if key.lower() not in TRACKING_PARAMS]
    return urllib.parse.urlunsplit(
        (
            parts.scheme.lower(),
            parts.netloc.lower(),
            parts.path.rstrip("/"),
            urllib.parse.urlencode(filtered_query),
            "",
        )
    )


def lead_id_for(url: str) -> str:
    return hashlib.sha1(normalize_url(url).encode("utf-8")).hexdigest()[:16]


def read_known_lead_ids() -> set[str]:
    if not LEADS_CSV.exists():
        return set()
    with LEADS_CSV.open(encoding="utf-8", newline="") as handle:
        return {row["lead_id"] for row in csv.DictReader(handle)}


def append_leads(rows: list[dict]) -> int:
    known_ids = read_known_lead_ids()
    new_rows = [row for row in rows if row["lead_id"] not in known_ids]
    if not new_rows:
        return 0
    DISCOVERY_DIR.mkdir(parents=True, exist_ok=True)
    write_header = not LEADS_CSV.exists()
    buffer = io.StringIO()
    writer = csv.DictWriter(buffer, fieldnames=LEAD_FIELDS, lineterminator="\n")
    if write_header:
        writer.writeheader()
    for row in new_rows:
        writer.writerow(row)
    with LEADS_CSV.open("a", encoding="utf-8", newline="") as handle:
        handle.write(buffer.getvalue())
    return len(new_rows)


class PoliteFetcher:
    """Shared fetcher for discovery harvesters with per-host spacing and disk cache."""

    def __init__(self, cache_dir: Path, min_interval_s: float = 2.0):
        self.cache_dir = Path(cache_dir)
        self.min_interval_s = min_interval_s
        self._last_hit: dict[str, float] = {}

    def get(self, url: str, max_age_days: float = 6.0) -> bytes:
        key = hashlib.sha1(normalize_url(url).encode("utf-8")).hexdigest()
        cached_path = self.cache_dir / f"{key}.body"
        if cached_path.exists():
            age_days = (time.time() - cached_path.stat().st_mtime) / 86400
            if age_days <= max_age_days:
                return cached_path.read_bytes()

        host = urllib.parse.urlsplit(url).netloc.lower()
        wait_seconds = self.min_interval_s - (time.time() - self._last_hit.get(host, 0.0))
        if wait_seconds > 0:
            time.sleep(wait_seconds)

        request = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
        with urllib.request.urlopen(request, timeout=30) as response:
            body = response.read()

        self._last_hit[host] = time.time()
        self.cache_dir.mkdir(parents=True, exist_ok=True)
        cached_path.write_bytes(body)
        return body
