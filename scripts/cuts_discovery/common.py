import csv
import hashlib
import io
import time
import urllib.error
import urllib.parse
import urllib.request
from datetime import datetime, timezone
from email.utils import parsedate_to_datetime
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
DISCOVERY_DIR = ROOT / "data_pipelines" / "college_cuts" / "discovery"
DISCOVERY_CACHE_DIR = DISCOVERY_DIR / "cache"
LEADS_CSV = DISCOVERY_DIR / "leads.csv"
CLASSIFICATIONS_CSV = DISCOVERY_DIR / "classifications.csv"
WATCHLIST_CSV = DISCOVERY_DIR / "watchlist.csv"
DISCOVERED_CANDIDATES_CSV = ROOT / "data_pipelines" / "college_cuts" / "discovered_cut_candidates.csv"
FEEDS_CONFIG = ROOT / "config" / "cuts_feeds.yml"
QUERIES_CONFIG = ROOT / "config" / "cuts_queries.yml"

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
CLASSIFICATION_FIELDS = [
    "lead_id",
    "classified_at",
    "model",
    "is_cut",
    "confidence",
    "institution_name_raw",
    "unitid",
    "state",
    "cut_type",
    "announcement_date",
    "scale_text",
    "summary",
    "notes",
]
WATCHLIST_FIELDS = ["unitid", "institution_name", "state", "reason"]
DISCOVERED_CANDIDATE_FIELDS = [
    "cut_id",
    "unitid",
    "institution_name",
    "state",
    "announcement_date",
    "announcement_year",
    "cut_type",
    "program_name",
    "generated_cut_label",
    "generated_cut_summary",
    "source_url",
    "source_title",
    "source_publication",
    "row_origin",
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


def today_iso() -> str:
    return datetime.now(timezone.utc).date().isoformat()


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


def discovered_cut_id(unitid: str, cut_type: str, announcement_date: str, source_url: str) -> str:
    base = "|".join(
        [
            str(unitid or ""),
            cut_type or "",
            announcement_date or "",
            normalize_url(source_url or ""),
        ]
    )
    return "discovered-" + hashlib.sha1(base.encode("utf-8")).hexdigest()[:16]


def _coerce_row(row: dict, fieldnames: list[str]) -> dict:
    return {field: (row.get(field, "") if row.get(field, "") is not None else "") for field in fieldnames}


def ensure_csv(path: Path, fieldnames: list[str]) -> None:
    csv_path = Path(path)
    csv_path.parent.mkdir(parents=True, exist_ok=True)
    if csv_path.exists():
        return
    write_csv_rows(csv_path, fieldnames, [])


def read_csv_rows(path: Path, fieldnames: list[str] | None = None) -> list[dict]:
    csv_path = Path(path)
    if not csv_path.exists():
        return []
    with csv_path.open("r", encoding="utf-8", newline="") as handle:
        rows = list(csv.DictReader(handle))
    if fieldnames is None:
        return rows
    return [_coerce_row(row, fieldnames) for row in rows]


def write_csv_rows(path: Path, fieldnames: list[str], rows: list[dict]) -> None:
    csv_path = Path(path)
    csv_path.parent.mkdir(parents=True, exist_ok=True)
    buffer = io.StringIO()
    writer = csv.DictWriter(buffer, fieldnames=fieldnames, lineterminator="\n")
    writer.writeheader()
    for row in rows:
        writer.writerow(_coerce_row(row, fieldnames))
    csv_path.write_text(buffer.getvalue(), encoding="utf-8", newline="\n")


def read_known_lead_ids(leads_path: Path = LEADS_CSV) -> set[str]:
    return {row["lead_id"] for row in read_csv_rows(leads_path, LEAD_FIELDS) if row.get("lead_id")}


def append_leads(rows: list[dict], leads_path: Path = LEADS_CSV, known_ids: set[str] | None = None) -> int:
    csv_path = Path(leads_path)
    ensure_csv(csv_path, LEAD_FIELDS)
    known_ids = known_ids if known_ids is not None else read_known_lead_ids(csv_path)
    new_rows = []
    for row in rows:
        if row["lead_id"] in known_ids:
            continue
        new_rows.append(_coerce_row(row, LEAD_FIELDS))
        known_ids.add(row["lead_id"])
    if not new_rows:
        return 0
    with csv_path.open("a", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=LEAD_FIELDS, lineterminator="\n")
        for row in new_rows:
            writer.writerow(row)
    return len(new_rows)


def strip_inline_comment(line: str) -> str:
    in_single = False
    in_double = False
    for index, char in enumerate(line):
        if char == "'" and not in_double:
            in_single = not in_single
        elif char == '"' and not in_single:
            in_double = not in_double
        elif char == "#" and not in_single and not in_double:
            return line[:index].rstrip()
    return line.rstrip()


def unquote_scalar(value: str) -> str:
    value = value.strip()
    if len(value) >= 2 and value[0] == value[-1] and value[0] in {"'", '"'}:
        return value[1:-1]
    return value


def parse_key_value(line: str) -> tuple[str, str]:
    if ":" not in line:
        raise ValueError(f"Expected key: value entry, got: {line!r}")
    key, value = line.split(":", 1)
    return key.strip(), unquote_scalar(value.strip())


def load_feeds_config(path: Path = FEEDS_CONFIG) -> dict:
    feeds = []
    current = None
    for raw_line in path.read_text(encoding="utf-8").splitlines():
        line = strip_inline_comment(raw_line)
        stripped = line.strip()
        if not stripped:
            continue
        if stripped == "feeds:":
            continue
        if stripped.startswith("- "):
            if current:
                feeds.append(current)
            current = {}
            tail = stripped[2:].strip()
            if tail:
                key, value = parse_key_value(tail)
                current[key] = value
            continue
        if current is None:
            raise ValueError(f"Unexpected feeds config line before first list item: {raw_line!r}")
        key, value = parse_key_value(stripped)
        current[key] = value
    if current:
        feeds.append(current)
    for feed in feeds:
        missing = [key for key in ("name", "url", "tier") if not feed.get(key)]
        if missing:
            raise ValueError(f"Feed entry is missing required keys {missing}: {feed!r}")
    return {"feeds": feeds}


def load_queries_config(path: Path = QUERIES_CONFIG) -> dict:
    config = {
        "standing_queries": [],
        "include_keywords": [],
        "kill_patterns": [],
    }
    current_key = None
    for raw_line in path.read_text(encoding="utf-8").splitlines():
        line = strip_inline_comment(raw_line)
        stripped = line.strip()
        if not stripped:
            continue
        if stripped.endswith(":"):
            current_key = stripped[:-1]
            if current_key not in config:
                raise ValueError(f"Unexpected discovery query section: {current_key}")
            continue
        if not stripped.startswith("- ") or current_key is None:
            raise ValueError(f"Malformed discovery query config line: {raw_line!r}")
        config[current_key].append(unquote_scalar(stripped[2:].strip()))
    return config


def load_watchlist_rows(path: Path = WATCHLIST_CSV) -> list[dict]:
    if not Path(path).exists():
        return []
    return read_csv_rows(path, WATCHLIST_FIELDS)


def parse_date_to_iso(value: str) -> str:
    text = (value or "").strip()
    if not text:
        return ""
    try:
        return datetime.fromisoformat(text.replace("Z", "+00:00")).date().isoformat()
    except ValueError:
        pass
    for fmt in ("%m/%d/%Y", "%m/%d/%y", "%B %d, %Y", "%b %d, %Y"):
        try:
            return datetime.strptime(text, fmt).date().isoformat()
        except ValueError:
            continue
    try:
        return parsedate_to_datetime(text).date().isoformat()
    except (TypeError, ValueError, IndexError, OverflowError):
        return ""


class PoliteFetcher:
    """Shared fetcher for discovery harvesters with per-host spacing and disk cache."""

    def __init__(self, cache_dir: Path, min_interval_s: float = 2.0):
        self.cache_dir = Path(cache_dir)
        self.min_interval_s = min_interval_s
        self._last_hit: dict[str, float] = {}
        self._host_penalty_s: dict[str, float] = {}
        self._resolved_url_memory: dict[str, str] = {}

    def _host_for(self, url: str) -> str:
        return urllib.parse.urlsplit(url).netloc.lower()

    def _respect_host_spacing(self, url: str) -> None:
        host = self._host_for(url)
        min_interval_s = self.min_interval_s + self._host_penalty_s.get(host, 0.0)
        wait_seconds = min_interval_s - (time.time() - self._last_hit.get(host, 0.0))
        if wait_seconds > 0:
            time.sleep(wait_seconds)
        self._last_hit[host] = time.time()

    def _bump_host_penalty(self, host: str, amount_s: float, ceiling_s: float = 45.0) -> None:
        self._host_penalty_s[host] = min(ceiling_s, max(0.0, self._host_penalty_s.get(host, 0.0) + amount_s))

    def _relax_host_penalty(self, host: str, amount_s: float = 0.5) -> None:
        current = self._host_penalty_s.get(host, 0.0)
        if current <= 0:
            return
        next_value = max(0.0, current - amount_s)
        if next_value == 0.0:
            self._host_penalty_s.pop(host, None)
        else:
            self._host_penalty_s[host] = next_value

    def _request_with_backoff(self, url: str, timeout: int, reader):
        host = self._host_for(url)
        max_attempts = 3 if host == "news.google.com" else 1
        last_error = None

        for attempt in range(1, max_attempts + 1):
            self._respect_host_spacing(url)
            request = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
            started = time.time()
            try:
                with urllib.request.urlopen(request, timeout=timeout) as response:
                    result = reader(response)
                elapsed_s = time.time() - started
                if host == "news.google.com":
                    if elapsed_s >= 8.0:
                        self._bump_host_penalty(host, 2.0)
                        print(
                            f"cuts_discovery: google_news slow_response elapsed_s={elapsed_s:.1f} "
                            f"penalty_s={self._host_penalty_s.get(host, 0.0):.1f}"
                        )
                    else:
                        self._relax_host_penalty(host)
                return result
            except urllib.error.HTTPError as exc:
                last_error = exc
                if host != "news.google.com" or exc.code not in {429, 500, 502, 503, 504} or attempt >= max_attempts:
                    raise
                retry_after = 0.0
                if exc.headers:
                    retry_after_raw = (exc.headers.get("Retry-After") or "").strip()
                    if retry_after_raw.isdigit():
                        retry_after = float(retry_after_raw)
                self._bump_host_penalty(host, max(5.0, retry_after or 10.0))
                delay_s = max(5.0, retry_after or self._host_penalty_s.get(host, 5.0))
                print(
                    f"cuts_discovery: google_news retryable_http code={exc.code} attempt={attempt}/{max_attempts} "
                    f"sleep_s={delay_s:.1f}"
                )
                time.sleep(delay_s)
            except (TimeoutError, urllib.error.URLError) as exc:
                last_error = exc
                if host != "news.google.com" or attempt >= max_attempts:
                    raise
                self._bump_host_penalty(host, 5.0)
                delay_s = max(5.0, self._host_penalty_s.get(host, 5.0))
                print(
                    f"cuts_discovery: google_news retryable_network attempt={attempt}/{max_attempts} "
                    f"sleep_s={delay_s:.1f} error={exc}"
                )
                time.sleep(delay_s)

        if last_error is not None:
            raise last_error
        raise RuntimeError(f"Unexpected request failure for {url}")

    def get(self, url: str, max_age_days: float = 6.0) -> bytes:
        key = hashlib.sha1(normalize_url(url).encode("utf-8")).hexdigest()
        cached_path = self.cache_dir / f"{key}.body"
        if cached_path.exists():
            age_days = (time.time() - cached_path.stat().st_mtime) / 86400
            if age_days <= max_age_days:
                return cached_path.read_bytes()

        body = self._request_with_backoff(url, timeout=30, reader=lambda response: response.read())

        self.cache_dir.mkdir(parents=True, exist_ok=True)
        cached_path.write_bytes(body)
        return body

    def resolve_url(self, url: str, max_age_days: float = 30.0) -> str:
        normalized_url = normalize_url(url)
        cached_resolved = self._resolved_url_memory.get(normalized_url)
        if cached_resolved:
            return cached_resolved

        key = hashlib.sha1(normalized_url.encode("utf-8")).hexdigest()
        cached_path = self.cache_dir / f"{key}.redirect"
        if cached_path.exists():
            age_days = (time.time() - cached_path.stat().st_mtime) / 86400
            if age_days <= max_age_days:
                cached_resolved = cached_path.read_text(encoding="utf-8").strip() or normalized_url
                self._resolved_url_memory[normalized_url] = cached_resolved
                return cached_resolved

        resolved = normalize_url(
            self._request_with_backoff(url, timeout=30, reader=lambda response: response.geturl() or normalized_url)
        )

        self.cache_dir.mkdir(parents=True, exist_ok=True)
        cached_path.write_text(resolved, encoding="utf-8", newline="\n")
        self._resolved_url_memory[normalized_url] = resolved
        return resolved
