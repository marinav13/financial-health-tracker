import csv
import re
import sys
from pathlib import Path

if __package__ in (None, ""):
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
    from cuts_discovery.common import ROOT
else:
    from .common import ROOT

SCRIPTS_DIR = Path(__file__).resolve().parents[1]
if str(SCRIPTS_DIR) not in sys.path:
    sys.path.insert(0, str(SCRIPTS_DIR))

from import_supabase_institution_mapping import STATE_ABBREV, normalize_name  # noqa: E402


COLLEGE_CUTS_DIR = ROOT / "data_pipelines" / "college_cuts"
STATE_NAME_TO_ABBREV = {name.lower(): abbr for abbr, name in STATE_ABBREV.items()}
STATE_ABBREV_TO_NAME = {abbr.upper(): name for abbr, name in STATE_ABBREV.items()}
INSTITUTION_PATTERNS = [
    re.compile(
        r"\b((?:The )?(?:University|College|Institute|School|Seminary|Academy)"
        r"(?: of [A-Z][A-Za-z&.'-]+(?: [A-Z][A-Za-z&.'-]+){0,6})+)\b"
    ),
    re.compile(
        r"\b(([A-Z][A-Za-z&.'-]+(?: [A-Z][A-Za-z&.'-]+){0,6}) "
        r"(?:University|College|Institute|School|Seminary|Academy))\b"
    ),
]


def load_mapping_rows(base_dir: Path = COLLEGE_CUTS_DIR) -> list[dict]:
    rows = []
    supabase_path = Path(base_dir) / "supabase_institution_unitid_mapping.csv"
    alias_path = Path(base_dir) / "manual_aliases.csv"

    with supabase_path.open("r", encoding="utf-8", newline="") as handle:
        for row in csv.DictReader(handle):
            norm_name = normalize_name(row.get("institution_name_api") or "")
            unitid = (row.get("unitid") or "").strip()
            if not norm_name or not unitid:
                continue
            rows.append(
                {
                    "norm_name": norm_name,
                    "unitid": unitid,
                    "state": (row.get("state_full") or "").strip(),
                    "institution_name": (row.get("tracker_institution_name") or row.get("institution_name_api") or "").strip(),
                    "match_method": "supabase_mapping",
                }
            )

    with alias_path.open("r", encoding="utf-8", newline="") as handle:
        for row in csv.DictReader(handle):
            norm_name = (row.get("norm_name") or "").strip()
            unitid = (row.get("unitid_candidate") or "").strip()
            if not norm_name or not unitid:
                continue
            rows.append(
                {
                    "norm_name": norm_name,
                    "unitid": unitid,
                    "state": (row.get("state_full") or "").strip(),
                    "institution_name": (row.get("fallback_tracker_institution_name") or "").strip(),
                    "match_method": "manual_alias",
                }
            )

    deduped = {}
    for row in rows:
        key = (row["norm_name"], row["state"], row["unitid"])
        deduped[key] = row
    return sorted(deduped.values(), key=lambda row: (-len(row["norm_name"]), row["norm_name"], row["unitid"]))


def guess_state_from_text(text: str) -> str:
    lower_text = (text or "").lower()
    matches = []
    for state_name, state_abbr in STATE_NAME_TO_ABBREV.items():
        if re.search(rf"\b{re.escape(state_name)}\b", lower_text):
            matches.append(state_abbr)
    if len(matches) == 1:
        return matches[0]
    return ""


def extract_institution_name_from_text(text: str) -> str:
    for pattern in INSTITUTION_PATTERNS:
        match = pattern.search(text or "")
        if match:
            return match.group(1).strip(" ,.;:")
    return ""


def _pick_state_match(raw_text: str, matches: list[dict]) -> dict | None:
    lower_text = (raw_text or "").lower()
    state_matches = [row for row in matches if row.get("state") and row["state"].lower() in lower_text]
    if len({row["unitid"] for row in state_matches}) == 1 and state_matches:
        return state_matches[0]
    return None


def guess_institution(text: str, mapping_rows: list[dict]) -> dict:
    raw_text = text or ""
    normalized_text = f" {normalize_name(raw_text)} "
    matches = []
    for row in mapping_rows:
        norm_name = row["norm_name"]
        if not norm_name:
            continue
        if f" {norm_name} " in normalized_text or normalized_text.strip() == norm_name:
            matches.append(row)

    if matches:
        unitids = {row["unitid"] for row in matches}
        if len(unitids) == 1:
            winner = matches[0]
            return {
                "unitid": winner["unitid"],
                "institution_name": winner["institution_name"],
                "state": winner["state"],
                "match_method": winner["match_method"],
            }
        state_winner = _pick_state_match(raw_text, matches)
        if state_winner:
            return {
                "unitid": state_winner["unitid"],
                "institution_name": state_winner["institution_name"],
                "state": state_winner["state"],
                "match_method": f"{state_winner['match_method']}_state",
            }

    extracted_name = extract_institution_name_from_text(raw_text)
    return {
        "unitid": "",
        "institution_name": extracted_name,
        "state": guess_state_from_text(raw_text),
        "match_method": "headline_extract" if extracted_name else "",
    }


def resolve_institution(name: str, state: str, mapping_rows: list[dict]) -> dict:
    norm_name = normalize_name(name or "")
    state_value = (state or "").strip()
    state_full = STATE_ABBREV_TO_NAME.get(state_value.upper(), state_value).strip()
    if not norm_name:
        return {
            "unitid": "",
            "institution_name": (name or "").strip(),
            "state": state_full,
            "match_method": "",
        }

    exact_matches = [row for row in mapping_rows if row["norm_name"] == norm_name]
    if not exact_matches:
        return {
            "unitid": "",
            "institution_name": (name or "").strip(),
            "state": state_full,
            "match_method": "classified_name_only",
        }

    if state_full:
        state_matches = [row for row in exact_matches if (row.get("state") or "").strip().lower() == state_full.lower()]
        if len({row["unitid"] for row in state_matches}) == 1 and state_matches:
            winner = state_matches[0]
            return {
                "unitid": winner["unitid"],
                "institution_name": winner["institution_name"],
                "state": winner["state"],
                "match_method": f"{winner['match_method']}_classified_state",
            }

    if len({row["unitid"] for row in exact_matches}) == 1:
        winner = exact_matches[0]
        return {
            "unitid": winner["unitid"],
            "institution_name": winner["institution_name"],
            "state": winner["state"],
            "match_method": f"{winner['match_method']}_classified_name",
        }

    return {
        "unitid": "",
        "institution_name": (name or "").strip(),
        "state": state_full,
        "match_method": "classified_ambiguous",
    }


def guess_cut_type(text: str) -> str:
    lower_text = (text or "").lower()
    if "hiring freeze" in lower_text or "furlough" in lower_text:
        return "hiring_freeze"
    if "department" in lower_text and any(token in lower_text for token in ("closure", "close", "cut", "eliminat")):
        return "department_closure"
    if any(token in lower_text for token in ("teach-out", "teach out", "campus closure", "campus closing")):
        return "campus_closure"
    if (
        "institution closure" in lower_text
        or "college closure" in lower_text
        or "university closure" in lower_text
        or ("close permanently" in lower_text and any(token in lower_text for token in ("college", "university")))
    ):
        return "institution_closure"
    if (
        any(token in lower_text for token in ("program cut", "program cuts", "program suspension", "programs suspended", "programs eliminated", "majors cut", "major cuts"))
        or (any(token in lower_text for token in ("program", "major", "degree")) and any(token in lower_text for token in ("suspend", "eliminat", "discontinu", "teach-out", "teach out")))
    ):
        return "program_suspension"
    if any(
        token in lower_text
        for token in (
            "layoff",
            "laid off",
            "eliminating positions",
            "eliminate positions",
            "position cuts",
            "job cuts",
            "cuts staff",
            "staff cuts",
            "vacant positions eliminated",
        )
    ):
        return "staff_layoff"
    if "closure" in lower_text or "closing" in lower_text:
        return "campus_closure"
    if "discontinu" in lower_text or "suspend" in lower_text:
        return "program_suspension"
    if "eliminat" in lower_text:
        if any(token in lower_text for token in ("staff", "employee", "position", "faculty")):
            return "staff_layoff"
        return "program_suspension"
    return ""


def filter_lead(lead: dict, cfg: dict, mapping_rows: list[dict]) -> dict:
    headline = (lead.get("headline") or "").strip()
    snippet = (lead.get("snippet") or "").strip()
    text = f"{headline} {snippet}".strip()
    lower_text = text.lower()

    if not any(keyword.lower() in lower_text for keyword in cfg.get("include_keywords", [])):
        return {**lead, "status": "filtered_out", "status_reason": "no_include_keyword"}

    for pattern in cfg.get("kill_patterns", []):
        if re.search(pattern, text, flags=re.IGNORECASE):
            return {**lead, "status": "filtered_out", "status_reason": f"kill:{pattern[:30]}"}

    cut_type = guess_cut_type(text)
    if not cut_type:
        return {**lead, "status": "filtered_out", "status_reason": "no_cut_type_guess"}

    institution = guess_institution(text, mapping_rows)
    return {
        **lead,
        "status": "new",
        "status_reason": "",
        "cut_type_guess": cut_type,
        "unitid_guess": institution["unitid"],
        "institution_name_guess": institution["institution_name"],
        "state_guess": institution["state"],
        "match_method": institution["match_method"],
        "announcement_date_guess": (lead.get("published_date") or "").strip(),
    }
