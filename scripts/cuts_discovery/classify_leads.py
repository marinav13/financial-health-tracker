import json
import os
import re
import hashlib
import urllib.parse
import urllib.request
from html.parser import HTMLParser
from pathlib import Path

if __package__ in (None, ""):
    import sys

    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
    from cuts_discovery.common import (
        CLASSIFICATION_FIELDS,
        CLASSIFICATIONS_CSV,
        DISCOVERY_CACHE_DIR,
        normalize_url,
        today_iso,
        read_csv_rows,
        write_csv_rows,
    )
    from cuts_discovery.filter_rules import resolve_institution
else:
    from .common import (
        CLASSIFICATION_FIELDS,
        CLASSIFICATIONS_CSV,
        DISCOVERY_CACHE_DIR,
        normalize_url,
        today_iso,
        read_csv_rows,
        write_csv_rows,
    )
    from .filter_rules import resolve_institution


MODEL = os.environ.get("CUTS_CLASSIFIER_MODEL", "claude-haiku-4-5-20251001")
MAX_CLASSIFICATIONS_PER_RUN = 400
ALLOWED_CONFIDENCE = {"high", "medium", "low"}
ALLOWED_CUT_TYPES = {
    "staff_layoff",
    "program_suspension",
    "department_closure",
    "campus_closure",
    "institution_closure",
    "hiring_freeze",
    "other",
}

PROMPT_TEMPLATE = """You are screening news items for a tracker of budget cuts at U.S. four-year colleges and universities.

Item headline: {headline}
Publisher: {publisher}
Published: {published_date}
Article text (may be truncated):
{article_text}

Answer in strict JSON only, no prose, matching exactly this schema:
{{
  "is_cut": true/false,
  "confidence": "high"|"medium"|"low",
  "institution_name": "official institution name or null",
  "state": "two-letter state code or null",
  "cut_type": "staff_layoff"|"program_suspension"|"department_closure"|"campus_closure"|"institution_closure"|"hiring_freeze"|"other",
  "announcement_date": "YYYY-MM-DD or null",
  "scale_text": "brief scale, e.g. '42 positions' or '6 degree programs', or null",
  "summary": "one to two factual sentences describing the cut, suitable for an editor to review"
}}

Furloughs and hiring freezes both map to "hiring_freeze". If the story
covers multiple institutions, answer for the most prominent one and note
the others in summary. If not a cut, set is_cut=false and leave the other
fields null."""

RETRY_PROMPT = (
    "Your previous answer was not valid JSON matching the schema. "
    "Answer again in strict JSON only."
)


class ArticleTextExtractor(HTMLParser):
    def __init__(self):
        super().__init__()
        self._skip_stack = []
        self._chunks = []

    def handle_starttag(self, tag, attrs):  # noqa: ARG002
        if tag in {"script", "style", "nav", "header", "footer", "noscript"}:
            self._skip_stack.append(tag)

    def handle_endtag(self, tag):
        if self._skip_stack and self._skip_stack[-1] == tag:
            self._skip_stack.pop()

    def handle_data(self, data):
        if self._skip_stack:
            return
        cleaned = re.sub(r"\s+", " ", data or "").strip()
        if cleaned:
            self._chunks.append(cleaned)

    def text(self) -> str:
        return " ".join(self._chunks)


def _clean_optional_text(value):
    if value is None:
        return None
    text = str(value).strip()
    return text or None


def parse_verdict(raw_text: str) -> dict:
    try:
        payload = json.loads(raw_text)
    except json.JSONDecodeError as exc:
        raise ValueError(f"Verdict was not valid JSON: {exc}") from exc
    if not isinstance(payload, dict):
        raise ValueError("Verdict payload must be a JSON object.")

    unknown = set(payload) - {
        "is_cut",
        "confidence",
        "institution_name",
        "state",
        "cut_type",
        "announcement_date",
        "scale_text",
        "summary",
    }
    if unknown:
        raise ValueError(f"Verdict has unexpected field(s): {sorted(unknown)}")

    if not isinstance(payload.get("is_cut"), bool):
        raise ValueError("Verdict field is_cut must be boolean.")
    confidence = payload.get("confidence")
    if confidence not in ALLOWED_CONFIDENCE:
        raise ValueError(f"Verdict confidence must be one of {sorted(ALLOWED_CONFIDENCE)}.")

    institution_name = _clean_optional_text(payload.get("institution_name"))
    state = _clean_optional_text(payload.get("state"))
    cut_type = _clean_optional_text(payload.get("cut_type"))
    announcement_date = _clean_optional_text(payload.get("announcement_date"))
    scale_text = _clean_optional_text(payload.get("scale_text"))
    summary = _clean_optional_text(payload.get("summary"))

    if state is not None and not re.fullmatch(r"[A-Z]{2}", state):
        raise ValueError("Verdict state must be two uppercase letters or null.")
    if announcement_date is not None and not re.fullmatch(r"\d{4}-\d{2}-\d{2}", announcement_date):
        raise ValueError("Verdict announcement_date must be YYYY-MM-DD or null.")
    if payload["is_cut"]:
        if cut_type not in ALLOWED_CUT_TYPES:
            raise ValueError(f"Verdict cut_type must be one of {sorted(ALLOWED_CUT_TYPES)}.")
        if not summary:
            raise ValueError("Verdict summary is required when is_cut=true.")

    return {
        "is_cut": payload["is_cut"],
        "confidence": confidence,
        "institution_name": institution_name,
        "state": state,
        "cut_type": cut_type,
        "announcement_date": announcement_date,
        "scale_text": scale_text,
        "summary": summary,
    }


def read_classification_cache(path: Path = CLASSIFICATIONS_CSV) -> dict[str, dict]:
    return {row["lead_id"]: row for row in read_csv_rows(path, CLASSIFICATION_FIELDS) if row.get("lead_id")}


def write_classification_cache(cache: dict[str, dict], path: Path = CLASSIFICATIONS_CSV) -> None:
    ordered = [cache[key] for key in sorted(cache)]
    write_csv_rows(path, CLASSIFICATION_FIELDS, ordered)


def extract_article_text(fetcher, lead: dict) -> tuple[str, str]:
    fallback = " ".join(filter(None, [(lead.get("headline") or "").strip(), (lead.get("snippet") or "").strip()])).strip()
    try:
        body = fetcher.get((lead.get("url") or "").strip(), max_age_days=6.0)
        html = body.decode("utf-8", "replace")
        parser = ArticleTextExtractor()
        parser.feed(html)
        text = parser.text().strip()
        if text:
            return text[:4000], ""
        return fallback[:4000], "article_fetch_empty_fallback"
    except Exception as exc:
        return fallback[:4000], f"article_fetch_failed:{exc}"


def extract_response_text(payload: dict) -> str:
    content = payload.get("content")
    if not isinstance(content, list) or not content:
        raise ValueError("Anthropic response missing content list.")
    first = content[0]
    if not isinstance(first, dict):
        raise ValueError("Anthropic response content item must be an object.")
    text = first.get("text")
    if not isinstance(text, str):
        raise ValueError("Anthropic response content[0].text missing.")
    return text


def call_anthropic_api(messages: list[dict], api_key: str, requester=urllib.request.urlopen, model: str = MODEL) -> dict:
    body = json.dumps(
        {
            "model": model,
            "max_tokens": 500,
            "messages": messages,
        }
    ).encode("utf-8")
    req = urllib.request.Request(
        "https://api.anthropic.com/v1/messages",
        data=body,
        headers={
            "x-api-key": api_key,
            "anthropic-version": "2023-06-01",
            "content-type": "application/json",
        },
    )
    with requester(req, timeout=60) as resp:
        return json.loads(resp.read().decode("utf-8"))


def classify_single_lead(
    lead: dict,
    article_text: str,
    article_notes: str,
    api_key: str,
    requester=urllib.request.urlopen,
    model: str = MODEL,
) -> dict:
    prompt = PROMPT_TEMPLATE.format(
        headline=(lead.get("headline") or "").strip(),
        publisher=(lead.get("publisher") or "").strip(),
        published_date=(lead.get("published_date") or "").strip(),
        article_text=(article_text or "")[:4000],
    )
    messages = [{"role": "user", "content": prompt}]
    errors = []
    for attempt in range(2):
        payload = call_anthropic_api(messages, api_key=api_key, requester=requester, model=model)
        try:
            verdict = parse_verdict(extract_response_text(payload))
            notes = article_notes
            return {
                "ok": True,
                "verdict": verdict,
                "notes": notes,
            }
        except ValueError as exc:
            errors.append(str(exc))
            if attempt == 0:
                messages.append({"role": "assistant", "content": extract_response_text(payload)})
                messages.append({"role": "user", "content": RETRY_PROMPT})
                continue
    return {
        "ok": False,
        "error": "; ".join(errors),
        "notes": article_notes,
    }


def verdict_to_cache_row(lead: dict, verdict: dict, mapping_rows: list[dict], model: str, notes: str) -> dict:
    resolved = resolve_institution(
        verdict.get("institution_name") or "",
        verdict.get("state") or "",
        mapping_rows,
    )
    note_parts = [part for part in [notes, resolved.get("match_method", "")] if part]
    return {
        "lead_id": lead["lead_id"],
        "classified_at": today_iso(),
        "model": model,
        "is_cut": "true" if verdict["is_cut"] else "false",
        "confidence": verdict["confidence"],
        "institution_name_raw": verdict.get("institution_name") or "",
        "unitid": resolved.get("unitid") or "",
        "state": resolved.get("state") or (verdict.get("state") or ""),
        "cut_type": verdict.get("cut_type") or "",
        "announcement_date": verdict.get("announcement_date") or "",
        "scale_text": verdict.get("scale_text") or "",
        "summary": verdict.get("summary") or "",
        "notes": "; ".join(note_parts),
    }


def classify_survivors(
    leads: list[dict],
    fetcher,
    mapping_rows: list[dict],
    path: Path = CLASSIFICATIONS_CSV,
    requester=urllib.request.urlopen,
    model: str = MODEL,
    max_classifications: int = MAX_CLASSIFICATIONS_PER_RUN,
) -> tuple[dict[str, dict], set[str]]:
    api_key = os.environ.get("ANTHROPIC_API_KEY", "").strip()
    cache = read_classification_cache(path)
    error_lead_ids = set()
    if not api_key:
        return cache, error_lead_ids

    remaining = max_classifications
    mutated = False
    uncached = [lead for lead in leads if lead["lead_id"] not in cache]
    if len(uncached) > max_classifications:
        print(
            f"::warning::cuts discovery classification cap hit ({max_classifications}); "
            "remaining survivor leads stay rules-only this run."
        )
    for lead in uncached:
        if remaining <= 0:
            break
        try:
            article_text, article_notes = extract_article_text(fetcher, lead)
            result = classify_single_lead(
                lead,
                article_text=article_text,
                article_notes=article_notes,
                api_key=api_key,
                requester=requester,
                model=model,
            )
            if result["ok"]:
                cache_row = verdict_to_cache_row(
                    lead,
                    verdict=result["verdict"],
                    mapping_rows=mapping_rows,
                    model=model,
                    notes=result["notes"],
                )
                cache[lead["lead_id"]] = cache_row
                mutated = True
            else:
                error_lead_ids.add(lead["lead_id"])
                print(f"::warning::cuts discovery classification failed for {lead['lead_id']}: {result['error']}")
        except Exception as exc:
            error_lead_ids.add(lead["lead_id"])
            print(f"::warning::cuts discovery classification failed for {lead['lead_id']}: {exc}")
        remaining -= 1

    if mutated:
        write_classification_cache(cache, path)
    return cache, error_lead_ids


def archive_source_url(url: str, requester=urllib.request.urlopen, cache_dir: Path = DISCOVERY_CACHE_DIR) -> str:
    target = (url or "").strip()
    if not target:
        return ""
    key = hashlib.sha1(normalize_url(target).encode("utf-8")).hexdigest()
    cached_path = Path(cache_dir) / f"{key}.archive"
    if cached_path.exists():
        return cached_path.read_text(encoding="utf-8").strip()
    req = urllib.request.Request(
        "https://web.archive.org/save/" + urllib.parse.quote(target, safe=":/?&=%"),
        method="POST",
        headers={"User-Agent": "HechingerFinancialHealthTracker/1.0"},
    )
    with requester(req, timeout=60) as resp:
        archived = resp.headers.get("Content-Location", "")
    Path(cache_dir).mkdir(parents=True, exist_ok=True)
    cached_path.write_text(archived, encoding="utf-8", newline="\n")
    return archived
