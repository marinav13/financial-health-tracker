#!/usr/bin/env python3
import os
import sys
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


def ensure_discovery_files() -> None:
    ensure_csv(LEADS_CSV, LEAD_FIELDS)
    ensure_csv(CLASSIFICATIONS_CSV, CLASSIFICATION_FIELDS)
    ensure_csv(WATCHLIST_CSV, WATCHLIST_FIELDS)
    ensure_csv(DISCOVERED_CANDIDATES_CSV, DISCOVERED_CANDIDATE_FIELDS)


def summarize_tier(tier_name: str, lead_ids: set[str], harvested_count: int, new_count: int, survivor_ids: set[str]) -> None:
    survivors = len(lead_ids & survivor_ids)
    print(f"cuts_discovery: {tier_name} leads={harvested_count} new={new_count} survivors={survivors}")


def main() -> int:
    ensure_discovery_files()

    use_classification = bool(os.environ.get("ANTHROPIC_API_KEY", "").strip())
    if not use_classification:
        print("ANTHROPIC_API_KEY is not set; running rules-only.")

    queries_config = load_queries_config()
    mapping_rows = load_mapping_rows()
    suppression_rows = load_suppression_rows()
    fetcher = PoliteFetcher(DISCOVERY_CACHE_DIR)

    tier_runs = [
        ("trade_feed", lambda: harvest_trade_feeds(fetcher)),
        ("google_news", lambda: harvest_google_news(fetcher, queries_config)),
    ]
    harvested_by_tier = {}
    new_count_by_tier = {}
    tier_failures = 0

    for tier_name, runner in tier_runs:
        try:
            rows = runner()
            harvested_by_tier[tier_name] = rows
            new_count_by_tier[tier_name] = append_leads(rows)
        except Exception as exc:
            tier_failures += 1
            harvested_by_tier[tier_name] = []
            new_count_by_tier[tier_name] = 0
            print(f"::warning::cuts discovery tier {tier_name} failed: {exc}")

    if tier_failures == len(tier_runs):
        return 1

    all_leads = read_csv_rows(LEADS_CSV, LEAD_FIELDS)
    filtered_rows = []
    survivors = []
    for row in all_leads:
        filtered = filter_lead(row, queries_config, mapping_rows)
        filtered_rows.append(filtered)
        if filtered.get("status") != "filtered_out":
            survivors.append(filtered)

    classification_cache = {}
    classification_error_ids = set()
    if use_classification:
        classification_cache, classification_error_ids = classify_survivors(
            survivors,
            fetcher=fetcher,
            mapping_rows=mapping_rows,
            path=CLASSIFICATIONS_CSV,
        )
        for row in survivors:
            cached = classification_cache.get(row["lead_id"])
            if cached:
                row["classification"] = cached

    candidates, candidate_ids, suppressed_ids = assemble_candidates(survivors, suppression_rows=suppression_rows)
    survivor_ids = {row["lead_id"] for row in survivors}

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
    for candidate in candidates:
        try:
            archive_source_url(candidate.get("source_url") or "")
        except Exception as exc:
            print(f"::warning::cuts discovery archive failed for {candidate.get('source_url')}: {exc}")

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
