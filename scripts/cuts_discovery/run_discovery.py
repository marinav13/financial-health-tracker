#!/usr/bin/env python3
import os
import sys
import time
from pathlib import Path

if __package__ in (None, ""):
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
    from cuts_discovery.assemble_candidates import assemble_candidates, load_suppression_rows, write_discovered_candidates
    from cuts_discovery.classify_leads import archive_source_url, classify_survivors
    from cuts_discovery.common import (
        CLASSIFICATION_FIELDS,
        CLASSIFICATIONS_CSV,
        DISCOVERED_CANDIDATE_FIELDS,
        DISCOVERED_CANDIDATES_CSV,
        DISCOVERY_CACHE_DIR,
        LEAD_FIELDS,
        LEADS_CSV,
        PoliteFetcher,
        WATCHLIST_CSV,
        WATCHLIST_FIELDS,
        append_leads,
        ensure_csv,
        load_queries_config,
        read_csv_rows,
        write_csv_rows,
    )
    from cuts_discovery.filter_rules import filter_lead, load_mapping_rows
    from cuts_discovery.harvest_feeds import harvest_all as harvest_trade_feeds
    from cuts_discovery.harvest_google_news import harvest_from_config as harvest_google_news
    from cuts_discovery.harvest_warn import harvest_all as harvest_warn
else:
    from .assemble_candidates import assemble_candidates, load_suppression_rows, write_discovered_candidates
    from .classify_leads import archive_source_url, classify_survivors
    from .common import (
        CLASSIFICATION_FIELDS,
        CLASSIFICATIONS_CSV,
        DISCOVERED_CANDIDATE_FIELDS,
        DISCOVERED_CANDIDATES_CSV,
        DISCOVERY_CACHE_DIR,
        LEAD_FIELDS,
        LEADS_CSV,
        PoliteFetcher,
        WATCHLIST_CSV,
        WATCHLIST_FIELDS,
        append_leads,
        ensure_csv,
        load_queries_config,
        read_csv_rows,
        write_csv_rows,
    )
    from .filter_rules import filter_lead, load_mapping_rows
    from .harvest_feeds import harvest_all as harvest_trade_feeds
    from .harvest_google_news import harvest_from_config as harvest_google_news
    from .harvest_warn import harvest_all as harvest_warn


def ensure_discovery_files() -> None:
    ensure_csv(LEADS_CSV, LEAD_FIELDS)
    ensure_csv(CLASSIFICATIONS_CSV, CLASSIFICATION_FIELDS)
    ensure_csv(WATCHLIST_CSV, WATCHLIST_FIELDS)
    ensure_csv(DISCOVERED_CANDIDATES_CSV, DISCOVERED_CANDIDATE_FIELDS)


def summarize_tier(tier_name: str, lead_ids: set[str], harvested_count: int, new_count: int, survivor_ids: set[str]) -> None:
    survivors = len(lead_ids & survivor_ids)
    print(f"cuts_discovery: {tier_name} leads={harvested_count} new={new_count} survivors={survivors}")


def env_bool(name: str, default: bool) -> bool:
    value = os.environ.get(name)
    if value is None or not value.strip():
        return default
    return value.strip().lower() not in {"0", "false", "no", "off"}


def env_int(name: str) -> int | None:
    value = os.environ.get(name)
    if value is None or not value.strip():
        return None
    try:
        parsed = int(value.strip())
    except ValueError as exc:
        raise ValueError(f"Environment variable {name} must be an integer, got {value!r}") from exc
    return max(parsed, 0)


def env_float(name: str) -> float | None:
    value = os.environ.get(name)
    if value is None or not value.strip():
        return None
    try:
        parsed = float(value.strip())
    except ValueError as exc:
        raise ValueError(f"Environment variable {name} must be a number, got {value!r}") from exc
    return max(parsed, 0.0)


def run_google_news_tier(fetcher, queries_config: dict, watchlist_query_limit: int | None) -> tuple[list[dict], int]:
    known_lead_ids = read_csv_rows(LEADS_CSV, LEAD_FIELDS)
    known_ids = {row["lead_id"] for row in known_lead_ids if row.get("lead_id")}
    new_count = 0

    def persist_rss_batch(*, query: str, tier: str, rss_url: str, rows: list[dict]) -> None:
        nonlocal new_count
        batch_new = append_leads(rows, LEADS_CSV, known_ids=known_ids)
        new_count += batch_new
        print(
            "cuts_discovery: "
            f"google_news_batch tier={tier} "
            f"rows={len(rows)} new={batch_new} "
            f"rss_url={rss_url}"
        )

    rows = harvest_google_news(
        fetcher,
        queries_config,
        max_watchlist_queries=watchlist_query_limit,
        on_rss_batch=persist_rss_batch,
    )
    return rows, new_count


def archive_candidates(
    candidates: list[dict],
    *,
    archive_url=archive_source_url,
    max_seconds: float = 120.0,
    request_timeout: int = 10,
    now_fn=time.perf_counter,
) -> int:
    archive_attempts = 0
    archive_started = now_fn()
    budget_hit = False
    for candidate in candidates:
        source_url = (candidate.get("source_url") or "").strip()
        if not source_url:
            continue
        if max_seconds > 0 and (now_fn() - archive_started) >= max_seconds:
            budget_hit = True
            break
        archive_attempts += 1
        try:
            archive_url(source_url, timeout=request_timeout)
        except Exception as exc:
            print(f"::warning::cuts discovery archive failed for {source_url}: {exc}")
    archive_elapsed_s = now_fn() - archive_started
    if budget_hit:
        print(
            "::warning::cuts discovery archive budget exhausted; "
            f"stopped after {archive_attempts} attempt(s) and {archive_elapsed_s:.1f}s."
        )
    print(
        "cuts_discovery: "
        f"archive elapsed_s={archive_elapsed_s:.1f} "
        f"attempted={archive_attempts}"
    )
    return archive_attempts


def main() -> int:
    ensure_discovery_files()

    profile_name = (os.environ.get("CUTS_DISCOVERY_PROFILE") or "default").strip() or "default"
    watchlist_query_limit = env_int("CUTS_WATCHLIST_QUERY_LIMIT")
    classification_limit = env_int("CUTS_CLASSIFICATION_MAX")
    archive_enabled = env_bool("CUTS_ENABLE_ARCHIVE", True)
    archive_max_seconds = env_float("CUTS_ARCHIVE_MAX_SECONDS")
    archive_request_timeout = env_int("CUTS_ARCHIVE_TIMEOUT_SECONDS")
    use_classification = env_bool(
        "CUTS_DISCOVERY_USE_CLASSIFICATION",
        bool(os.environ.get("ANTHROPIC_API_KEY", "").strip()),
    )
    print(
        "cuts_discovery: "
        f"profile={profile_name} "
        f"news_window={(os.environ.get('CUTS_NEWS_WINDOW') or '').strip() or 'default'} "
        f"watchlist_query_limit={watchlist_query_limit if watchlist_query_limit is not None else 'all'} "
        f"classification={'on' if use_classification else 'off'} "
        f"classification_limit={classification_limit if classification_limit is not None else 'default'} "
        f"archive={'on' if archive_enabled else 'off'} "
        f"archive_max_seconds={archive_max_seconds if archive_max_seconds is not None else 120.0} "
        f"archive_timeout_s={archive_request_timeout if archive_request_timeout is not None else 10}"
    )
    if not use_classification:
        print("cuts_discovery: running rules-only classification path.")

    queries_config = load_queries_config()
    mapping_rows = load_mapping_rows()
    suppression_rows = load_suppression_rows()
    fetcher = PoliteFetcher(DISCOVERY_CACHE_DIR)

    tier_runs = [
        ("trade_feed", lambda: (harvest_trade_feeds(fetcher), None)),
        (
            "google_news",
            lambda: run_google_news_tier(
                fetcher,
                queries_config,
                watchlist_query_limit,
            ),
        ),
        ("warn", lambda: (harvest_warn(fetcher, mapping_rows), None)),
    ]
    harvested_by_tier = {}
    new_count_by_tier = {}
    tier_failures = 0

    for tier_name, runner in tier_runs:
        tier_started = time.perf_counter()
        try:
            rows, new_count = runner()
            harvested_by_tier[tier_name] = rows
            if new_count is None:
                new_count = append_leads(rows)
            new_count_by_tier[tier_name] = new_count
        except Exception as exc:
            tier_failures += 1
            harvested_by_tier[tier_name] = []
            new_count_by_tier[tier_name] = 0
            print(f"::warning::cuts discovery tier {tier_name} failed: {exc}")
        finally:
            elapsed_s = time.perf_counter() - tier_started
            print(f"cuts_discovery: tier={tier_name} elapsed_s={elapsed_s:.1f}")

    if tier_failures == len(tier_runs):
        return 1

    print(
        "cuts_discovery: "
        f"standing_queries={len(queries_config.get('standing_queries', []))} "
        f"watchlist_rows={len(read_csv_rows(WATCHLIST_CSV, WATCHLIST_FIELDS))}"
    )

    all_leads = read_csv_rows(LEADS_CSV, LEAD_FIELDS)
    filtered_rows = []
    survivors = []
    filter_started = time.perf_counter()
    for row in all_leads:
        filtered = filter_lead(row, queries_config, mapping_rows)
        filtered_rows.append(filtered)
        if filtered.get("status") != "filtered_out":
            survivors.append(filtered)
    print(
        "cuts_discovery: "
        f"filter elapsed_s={time.perf_counter() - filter_started:.1f} "
        f"all_leads={len(all_leads)} survivors={len(survivors)}"
    )

    classification_cache = {}
    classification_error_ids = set()
    if use_classification:
        existing_classified_ids = {
            row["lead_id"]
            for row in read_csv_rows(CLASSIFICATIONS_CSV, CLASSIFICATION_FIELDS)
            if row.get("lead_id")
        }
        classification_started = time.perf_counter()
        classification_cache, classification_error_ids = classify_survivors(
            survivors,
            fetcher=fetcher,
            mapping_rows=mapping_rows,
            path=CLASSIFICATIONS_CSV,
            max_classifications=classification_limit if classification_limit is not None else 400,
        )
        uncached_survivors = sum(1 for row in survivors if row["lead_id"] not in existing_classified_ids)
        print(
            "cuts_discovery: "
            f"classification elapsed_s={time.perf_counter() - classification_started:.1f} "
            f"uncached_survivors={uncached_survivors} "
            f"classification_errors={len(classification_error_ids)}"
        )
        for row in survivors:
            cached = classification_cache.get(row["lead_id"])
            if cached:
                row["classification"] = cached

    assemble_started = time.perf_counter()
    candidates, candidate_ids, suppressed_ids = assemble_candidates(survivors, suppression_rows=suppression_rows)
    survivor_ids = {row["lead_id"] for row in survivors}
    print(
        "cuts_discovery: "
        f"assemble elapsed_s={time.perf_counter() - assemble_started:.1f} "
        f"candidates={len(candidates)} suppressed={len(suppressed_ids)}"
    )

    updated_leads = []
    for row in filtered_rows:
        lead_id = row["lead_id"]
        updated = {field: row.get(field, "") for field in LEAD_FIELDS}
        if row.get("status") == "filtered_out":
            updated["status"] = "filtered_out"
            updated["status_reason"] = row.get("status_reason", "")
        elif lead_id in classification_error_ids:
            updated["status"] = "error"
            updated["status_reason"] = "classification_error"
        elif lead_id in suppressed_ids:
            updated["status"] = "suppressed"
            updated["status_reason"] = "existing_tracker_match"
        elif lead_id in candidate_ids:
            updated["status"] = "candidate"
            updated["status_reason"] = ""
        elif lead_id in classification_cache:
            updated["status"] = "classified"
            updated["status_reason"] = "model_not_cut"
        else:
            updated["status"] = "new"
            updated["status_reason"] = ""
        updated_leads.append(updated)

    write_csv_rows(LEADS_CSV, LEAD_FIELDS, updated_leads)
    write_discovered_candidates(candidates)
    if archive_enabled:
        archive_candidates(
            candidates,
            max_seconds=archive_max_seconds if archive_max_seconds is not None else 120.0,
            request_timeout=archive_request_timeout if archive_request_timeout is not None else 10,
        )
    else:
        print(
            "cuts_discovery: "
            "archive elapsed_s=0.0 attempted=0"
        )

    for tier_name, rows in harvested_by_tier.items():
        summarize_tier(
            tier_name=tier_name,
            lead_ids={row["lead_id"] for row in rows},
            harvested_count=len(rows),
            new_count=new_count_by_tier[tier_name],
            survivor_ids=survivor_ids,
        )

    if sum(len(rows) for rows in harvested_by_tier.values()) == 0:
        print(
            "PIPELINE DRIFT WARNING: cuts_discovery: every discovery tier returned 0 leads - harvesting may be broken."
        )

    return 0


if __name__ == "__main__":
    sys.exit(main())
