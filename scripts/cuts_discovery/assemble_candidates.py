import csv
from datetime import date
from pathlib import Path

if __package__ in (None, ""):
    import sys

    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
    from cuts_discovery.common import (
        DISCOVERED_CANDIDATE_FIELDS,
        DISCOVERED_CANDIDATES_CSV,
        discovered_cut_id,
        parse_date_to_iso,
        write_csv_rows,
    )
else:
    from .common import (
        DISCOVERED_CANDIDATE_FIELDS,
        DISCOVERED_CANDIDATES_CSV,
        discovered_cut_id,
        parse_date_to_iso,
        write_csv_rows,
    )


COLLEGE_CUTS_DIR = Path(__file__).resolve().parents[2] / "data_pipelines" / "college_cuts"
STUDENT_SOURCE_KEYWORDS = (
    "student",
    "bruin",
    "collegian",
    "crimson",
    "lantern",
    "oracle",
    "daily pennsylvanian",
    "student life",
)

CUT_TYPE_LABELS = {
    "staff_layoff": "Staff layoff",
    "program_suspension": "Program suspension",
    "department_closure": "Department closure",
    "campus_closure": "Campus closure",
    "institution_closure": "Institution closure",
    "hiring_freeze": "Hiring freeze",
    "other": "Other cut",
}


def _days_between(left: str, right: str) -> int | None:
    left_iso = parse_date_to_iso(left)
    right_iso = parse_date_to_iso(right)
    if not left_iso or not right_iso:
        return None
    left_date = date.fromisoformat(left_iso)
    right_date = date.fromisoformat(right_iso)
    return abs((left_date - right_date).days)


def _first_nonempty(row: dict, keys: tuple[str, ...]) -> str:
    for key in keys:
        value = (row.get(key) or "").strip()
        if value:
            return value
    return ""


def load_suppression_rows(base_dir: Path = COLLEGE_CUTS_DIR) -> list[dict]:
    rows = []
    candidates_path = Path(base_dir) / "college_cuts_review_candidates.csv"
    overrides_path = Path(base_dir) / "editorial_overrides.csv"

    if candidates_path.exists():
        with candidates_path.open("r", encoding="utf-8", newline="") as handle:
            for row in csv.DictReader(handle):
                rows.append(
                    {
                        "unitid": (row.get("unitid") or "").strip(),
                        "cut_type": (row.get("cut_type") or "").strip(),
                        "announcement_date": (row.get("announcement_date") or "").strip(),
                    }
                )

    if overrides_path.exists():
        with overrides_path.open("r", encoding="utf-8", newline="") as handle:
            for row in csv.DictReader(handle):
                rows.append(
                    {
                        "unitid": _first_nonempty(row, ("override_unitid", "source_unitid")),
                        "cut_type": _first_nonempty(row, ("override_cut_type", "source_cut_type")),
                        "announcement_date": _first_nonempty(
                            row,
                            ("override_announcement_date", "source_announcement_date"),
                        ),
                    }
                )
    return rows


def row_origin_for_tier(tier: str) -> str:
    return "warn_notice" if tier == "warn" else "news_scan"


def source_rank(lead: dict) -> int:
    tier = (lead.get("tier") or "").strip()
    if tier == "trade_feed":
        return 0
    publisher = " ".join(
        [
            (lead.get("publisher") or "").lower(),
            (lead.get("headline") or "").lower(),
            (lead.get("url") or "").lower(),
        ]
    )
    if any(keyword in publisher for keyword in STUDENT_SOURCE_KEYWORDS):
        return 2
    return 1


def cluster_survivors(rows: list[dict]) -> list[list[dict]]:
    sorted_rows = sorted(
        rows,
        key=lambda row: (
            row.get("unitid_effective") or row.get("institution_name_effective") or row["lead_id"],
            row.get("cut_type_effective") or "",
            row.get("announcement_date_effective") or "9999-99-99",
            row["lead_id"],
        ),
    )
    clusters = []
    for row in sorted_rows:
        key = row.get("unitid_effective") or row.get("institution_name_effective") or row["lead_id"]
        cut_type = row.get("cut_type_effective") or ""
        announcement_date = row.get("announcement_date_effective") or ""
        attached = False
        for cluster in clusters:
            exemplar = cluster[0]
            if key != (exemplar.get("unitid_effective") or exemplar.get("institution_name_effective") or exemplar["lead_id"]):
                continue
            if cut_type != (exemplar.get("cut_type_effective") or ""):
                continue
            delta_days = _days_between(announcement_date, exemplar.get("announcement_date_effective") or "")
            if delta_days is not None and delta_days <= 14:
                cluster.append(row)
                attached = True
                break
        if not attached:
            clusters.append([row])
    return clusters


def cluster_matches_existing(cluster: list[dict], suppression_rows: list[dict]) -> bool:
    exemplar = cluster[0]
    unitid = (exemplar.get("unitid_effective") or "").strip()
    cut_type = (exemplar.get("cut_type_effective") or "").strip()
    announcement_date = (exemplar.get("announcement_date_effective") or "").strip()
    if not unitid or not cut_type or not announcement_date:
        return False
    for row in suppression_rows:
        if unitid != (row.get("unitid") or "").strip():
            continue
        if cut_type != (row.get("cut_type") or "").strip():
            continue
        delta_days = _days_between(announcement_date, row.get("announcement_date") or "")
        if delta_days is not None and delta_days <= 14:
            return True
    return False


def effective_lead_row(row: dict) -> dict:
    classification = row.get("classification") or {}
    is_classified = bool(classification)
    is_cut = (classification.get("is_cut") or "").strip().lower() == "true"
    effective = dict(row)
    effective["unitid_effective"] = (classification.get("unitid") or row.get("unitid_guess") or "").strip()
    effective["institution_name_effective"] = (
        classification.get("institution_name_raw")
        or row.get("institution_name_guess")
        or ""
    ).strip()
    effective["state_effective"] = (classification.get("state") or row.get("state_guess") or "").strip()
    effective["cut_type_effective"] = (classification.get("cut_type") or row.get("cut_type_guess") or "").strip()
    effective["announcement_date_effective"] = (
        classification.get("announcement_date") or row.get("announcement_date_guess") or ""
    ).strip()
    effective["classification_is_cut"] = (not is_classified) or is_cut
    return effective


def label_for_classified_cluster(best: dict, extra_urls: list[str]) -> str:
    classification = best.get("classification") or {}
    summary = (classification.get("summary") or "").strip()
    cut_type = (classification.get("cut_type") or "").strip()
    confidence = (classification.get("confidence") or "").strip()
    scale_text = (classification.get("scale_text") or "").strip()
    base = summary or (best.get("headline") or "").strip() or CUT_TYPE_LABELS.get(cut_type, "Cut")
    if cut_type == "other":
        base = "UNCLASSIFIED TYPE: " + base
    elif scale_text and scale_text.lower() not in base.lower():
        base = f"{CUT_TYPE_LABELS.get(cut_type, cut_type)} - {scale_text}"
    if confidence:
        base = f"{base} [model: {confidence} confidence]"
    if extra_urls:
        base += " [also: " + "; ".join(extra_urls) + "]"
    return base


def cluster_to_candidate(cluster: list[dict]) -> dict:
    ranked = sorted(
        cluster,
        key=lambda row: (
            source_rank(row),
            row.get("announcement_date_guess") or "9999-99-99",
            row["lead_id"],
        ),
    )
    best = ranked[0]
    best_url = (best.get("url") or "").strip()
    all_urls = []
    seen = set()
    for row in ranked:
        url = (row.get("url") or "").strip()
        if not url or url in seen:
            continue
        all_urls.append(url)
        seen.add(url)
    extra_urls = [url for url in all_urls if url != best_url]
    suffix = ""
    if extra_urls:
        suffix = " [also: " + "; ".join(extra_urls) + "]"
    announcement_date = (best.get("announcement_date_effective") or "").strip()
    cut_type = (best.get("cut_type_effective") or "").strip()
    unitid = (best.get("unitid_effective") or "").strip()
    headline = (best.get("headline") or "").strip()
    if best.get("classification"):
        classification = best["classification"]
        generated_label = label_for_classified_cluster(best, extra_urls)
        generated_summary = (classification.get("summary") or headline).strip()
        program_name = generated_summary
        institution_name = (best.get("institution_name_effective") or "").strip()
        state = (best.get("state_effective") or "").strip()
    else:
        generated_label = headline + suffix
        generated_summary = headline
        program_name = headline
        institution_name = (best.get("institution_name_effective") or "").strip()
        state = (best.get("state_effective") or "").strip()
    return {
        "cut_id": discovered_cut_id(unitid, cut_type, announcement_date, best_url),
        "unitid": unitid,
        "institution_name": institution_name,
        "state": state,
        "announcement_date": announcement_date,
        "announcement_year": announcement_date[:4] if announcement_date else "",
        "cut_type": cut_type,
        "program_name": program_name,
        "generated_cut_label": generated_label,
        "generated_cut_summary": generated_summary,
        "source_url": best_url,
        "source_title": headline,
        "source_publication": (best.get("publisher") or "").strip(),
        "row_origin": row_origin_for_tier((best.get("tier") or "").strip()),
    }


def assemble_candidates(
    survivor_rows: list[dict],
    suppression_rows: list[dict] | None = None,
) -> tuple[list[dict], set[str], set[str]]:
    suppression_rows = suppression_rows or []
    effective_rows = [effective_lead_row(row) for row in survivor_rows if row]
    effective_rows = [row for row in effective_rows if row.get("classification_is_cut", True)]
    clusters = cluster_survivors(effective_rows)
    candidates = []
    candidate_lead_ids = set()
    suppressed_lead_ids = set()
    for cluster in clusters:
        lead_ids = {row["lead_id"] for row in cluster}
        if cluster_matches_existing(cluster, suppression_rows):
            suppressed_lead_ids.update(lead_ids)
            continue
        candidates.append(cluster_to_candidate(cluster))
        candidate_lead_ids.update(lead_ids)
    return candidates, candidate_lead_ids, suppressed_lead_ids


def write_discovered_candidates(rows: list[dict], path: Path = DISCOVERED_CANDIDATES_CSV) -> None:
    ordered = sorted(rows, key=lambda row: (row.get("announcement_date") or "", row.get("cut_id") or ""))
    write_csv_rows(path, DISCOVERED_CANDIDATE_FIELDS, ordered)
