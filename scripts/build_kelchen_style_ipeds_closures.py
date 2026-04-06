import csv
import json
import re
import zipfile
from collections import Counter, defaultdict
from pathlib import Path


# Build a more selective, IPEDS-only closure series that approximates the
# conservative methodology discussed by Robert Kelchen.
#
# This script intentionally does not try to replace the broader hybrid closure
# pipeline in build_closure_outputs.py. Instead, it answers a narrower question:
# when a UNITID leaves the annual IPEDS directory universe, does that exit look
# more like an apparent closure, a likely combination/consolidation, or some
# other type of disappearance?
#
# Why an IPEDS-only script?
# - We do not currently have the weekly federal closed-school search file.
# - Kelchen's blog and the Federal Reserve paper make clear that naive counts of
#   institutions leaving IPEDS overstate true closures.
# - The annual HD directory files still let us build a conservative "best
#   effort" series by separating degree-granting leavers from likely
#   combinations and other exits.
#
# Practical rule set used here:
# 1. Track each UNITID across annual HD files.
# 2. Treat a UNITID as a leaver once it has a last-seen year before the newest
#    available HD file.
# 3. Require that the UNITID does not later reappear.
# 4. Classify the exit conservatively:
#    - likely_combination_or_successor:
#        ACT == C, or NEWID points to a successor UNITID
#    - apparent_closure:
#        degree-granting institution and ACT in {D, M} with no NEWID successor
#    - other_exit:
#        everything else
#
# This is still an approximation. It gets us much closer to Kelchen's
# "selective" IPEDS framing than a raw list of disappeared UNITIDs, but it is
# not a substitute for the federal weekly closed-school file.

REPO_ROOT = Path(__file__).resolve().parents[1]
IPEDS_DOWNLOADS_DIR = Path(r"C:\Users\mv3031\Downloads")
OUTPUT_DIR = REPO_ROOT / "federal_closure"
START_YEAR = 2003
END_YEAR = 2023

LEAVERS_OUTPUT = OUTPUT_DIR / "kelchen_style_ipeds_leavers.csv"
APPARENT_CLOSURES_OUTPUT = OUTPUT_DIR / "kelchen_style_ipeds_apparent_closures.csv"
COMBINATIONS_OUTPUT = OUTPUT_DIR / "kelchen_style_ipeds_combinations.csv"
YEAR_SUMMARY_OUTPUT = OUTPUT_DIR / "kelchen_style_ipeds_year_summary.csv"
SUMMARY_JSON_OUTPUT = OUTPUT_DIR / "kelchen_style_ipeds_summary.json"


def normalize_text(value):
    return re.sub(r"\s+", " ", str(value or "")).strip()


def safe_int(value):
    value = normalize_text(value)
    if value in {"", "-1", "-2", "-3"}:
        return None
    try:
        return int(float(value))
    except ValueError:
        return None


def sector_label(sector_code):
    return {
        "1": "Public, 4-year or above",
        "2": "Private not-for-profit, 4-year or above",
        "3": "Private for-profit, 4-year or above",
        "4": "Public, at least 2 but less than 4 years",
        "5": "Private not-for-profit, at least 2 but less than 4 years",
        "6": "Private for-profit, at least 2 but less than 4 years",
        "7": "Public, less than 2 years",
        "8": "Private not-for-profit, less than 2 years",
        "9": "Private for-profit, less than 2 years",
        "99": "Sector unknown",
    }.get(normalize_text(sector_code), "")


def broad_sector_label(control_code):
    return {
        "1": "Public",
        "2": "Private not-for-profit",
        "3": "Private for-profit",
    }.get(normalize_text(control_code), "")


def institutional_level_label(iclevel_code):
    return {
        "1": "4-year or above",
        "2": "At least 2 but less than 4 years",
        "3": "Less than 2 years",
    }.get(normalize_text(iclevel_code), "")


def tracker_category_label(row):
    # Mirror the tracker vocabulary where possible, but stay entirely inside the
    # annual HD directory fields so this script remains self-contained.
    if normalize_text(row.get("DEGGRANT")) == "1" and normalize_text(row.get("ICLEVEL")) == "1":
        return "Degree-granting, primarily baccalaureate or above"
    if normalize_text(row.get("DEGGRANT")) == "1" and normalize_text(row.get("ICLEVEL")) == "2":
        return "Degree-granting, primarily subbaccalaureate"
    return institutional_level_label(row.get("ICLEVEL"))


def highest_degree_label(code):
    return {
        "0": "Non-degree granting",
        "1": "First-professional only",
        "2": "Less than 1 year certificate",
        "3": "At least 1 but less than 2 years certificate",
        "4": "Associate's",
        "5": "Bachelor's",
        "6": "Postbaccalaureate certificate",
        "7": "Master's",
        "8": "Post-master's certificate",
        "9": "Doctor's - research/scholarship",
        "10": "Doctor's - professional practice",
        "11": "Doctor's - other",
        "12": "Other",
    }.get(normalize_text(code), "")


def read_zipped_csv_rows(zip_path):
    with zipfile.ZipFile(zip_path) as zf:
        member = next(name for name in zf.namelist() if name.lower().endswith(".csv"))
        with zf.open(member, "r") as handle:
            text = handle.read().decode("utf-8-sig", errors="ignore").splitlines()
    return list(csv.DictReader(text))


def load_hd_rows():
    annual_rows = {}
    available_years = []
    for year in range(START_YEAR, END_YEAR + 1):
        zip_path = IPEDS_DOWNLOADS_DIR / f"HD{year}.zip"
        if not zip_path.exists():
            continue
        annual_rows[year] = read_zipped_csv_rows(zip_path)
        available_years.append(year)
    return annual_rows, sorted(available_years)


def build_unitid_histories(annual_rows):
    histories = defaultdict(list)
    for year, rows in annual_rows.items():
        for row in rows:
            unitid = normalize_text(row.get("UNITID"))
            if not unitid:
                continue
            clean_row = {key: normalize_text(value) for key, value in row.items()}
            histories[unitid].append((year, clean_row))
    for rows in histories.values():
        rows.sort(key=lambda item: item[0])
    return histories


def build_headcount_lookup():
    # Pull the last known 12-month unduplicated headcount from EFFY so leaver
    # rows can still report size even after the institution disappears.
    #
    # EFFY coverage on hand starts later than the HD directory history. For
    # early years, headcount may be blank; that is still preferable to blocking
    # the closure run on missing non-HD files.
    annual_effy = {}
    for year in range(START_YEAR, END_YEAR + 1):
        zip_path = IPEDS_DOWNLOADS_DIR / f"EFFY{year}.zip"
        if not zip_path.exists():
            continue
        rows = read_zipped_csv_rows(zip_path)
        year_lookup = {}
        for row in rows:
            unitid = normalize_text(row.get("UNITID"))
            if not unitid:
                continue
            effy_alev = normalize_text(row.get("EFFYALEV"))
            lstudy = normalize_text(row.get("LSTUDY"))
            is_total_row = effy_alev == "1" or (not effy_alev and lstudy in {"999", "1"})
            if not is_total_row:
                continue
            headcount = safe_int(row.get("EFYTOTLT"))
            if headcount is not None and unitid not in year_lookup:
                year_lookup[unitid] = headcount
        annual_effy[year] = year_lookup
    return annual_effy


def find_last_headcount(unitid, last_seen_year, annual_effy):
    for year in range(last_seen_year, START_YEAR - 1, -1):
        headcount = annual_effy.get(year, {}).get(unitid)
        if headcount is not None:
            return year, headcount
    return None, None


def academic_year_label(first_missing_year):
    return f"{first_missing_year - 1}-{str(first_missing_year)[-2:]}"


def has_real_successor(newid):
    newid = normalize_text(newid)
    return newid not in {"", "-1", "-2", "-3"}


def classify_exit(last_row):
    act = normalize_text(last_row.get("ACT"))
    degree_granting = normalize_text(last_row.get("DEGGRANT")) == "1"
    successor = has_real_successor(last_row.get("NEWID"))

    # ACT=C is the cleanest combination signal in IPEDS. A real NEWID also
    # strongly suggests a successor institution rather than a clean closure.
    if act == "C" or successor:
        return "likely_combination_or_successor"

    # For a conservative closure proxy, focus on degree-granting institutions
    # that either closed during the current year (M) or were marked out of
    # business/delete (D) without a successor.
    if degree_granting and act in {"D", "M"}:
        return "apparent_closure"

    return "other_exit"


def closure_proxy_note(last_row):
    act = normalize_text(last_row.get("ACT"))
    newid = normalize_text(last_row.get("NEWID"))
    if act == "C":
        return "IPEDS ACT=C suggests this UNITID combined with another institution."
    if has_real_successor(newid):
        return "IPEDS NEWID points to a successor institution, so this looks more like a recode or combination than a clean closure."
    if act == "D":
        return "IPEDS ACT=D marks the institution out of business/delete; treated here as an apparent closure unless there is a successor."
    if act == "M":
        return "IPEDS ACT=M marks the institution as closed during the current year; treated here as an apparent closure."
    return "IPEDS does not give a clean closure or combination signal for this exit."


def build_rows(available_years, histories, annual_effy):
    max_year = max(available_years)
    rows = []
    for unitid, history in histories.items():
        years = [year for year, _ in history]
        last_seen_year, last_row = history[-1]

        # If the UNITID is still present in the newest directory file, it has
        # not yet left the IPEDS universe.
        if last_seen_year >= max_year:
            continue

        # We only treat a UNITID as having left once its final appearance is
        # before the newest available year.
        first_missing_year = last_seen_year + 1
        headcount_year, student_headcount = find_last_headcount(unitid, last_seen_year, annual_effy)
        exit_group = classify_exit(last_row)

        row = {
            "unitid": unitid,
            "institution_name": normalize_text(last_row.get("INSTNM")),
            "state": normalize_text(last_row.get("STABBR")),
            "city": normalize_text(last_row.get("CITY")),
            "last_seen_year": last_seen_year,
            "first_missing_year": first_missing_year,
            "academic_year": academic_year_label(first_missing_year),
            "first_seen_year": years[0],
            "reappeared_after_gap": "yes" if years != list(range(years[0], years[-1] + 1)) else "no",
            "act_code": normalize_text(last_row.get("ACT")),
            "closedat": normalize_text(last_row.get("CLOSEDAT")),
            "newid": normalize_text(last_row.get("NEWID")),
            "degree_granting": "yes" if normalize_text(last_row.get("DEGGRANT")) == "1" else "no",
            "sector": sector_label(last_row.get("SECTOR")),
            "broad_sector": broad_sector_label(last_row.get("CONTROL")),
            "institutional_level": institutional_level_label(last_row.get("ICLEVEL")),
            "tracker_category": tracker_category_label(last_row),
            "highest_degree_offered": highest_degree_label(last_row.get("HDEGOFR1")),
            "instcat": normalize_text(last_row.get("INSTCAT")),
            "opeid8": normalize_text(last_row.get("OPEID")),
            "headcount_year": headcount_year or "",
            "student_headcount": student_headcount or "",
            "exit_group": exit_group,
            "closure_proxy_note": closure_proxy_note(last_row),
            "source_group": "ipeds_annual_directory",
            "source_file": f"HD{last_seen_year}.zip",
            "source_note": "Annual IPEDS HD directory leaver analysis; approximate Kelchen-style IPEDS closure screen.",
        }
        rows.append(row)
    rows.sort(key=lambda item: (item["first_missing_year"], item["institution_name"], item["unitid"]))
    return rows


def write_csv(path, rows, fieldnames):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def build_year_summary(rows):
    grouped = defaultdict(list)
    for row in rows:
        grouped[row["first_missing_year"]].append(row)

    summary_rows = []
    for year in sorted(grouped):
        year_rows = grouped[year]
        degree_rows = [row for row in year_rows if row["degree_granting"] == "yes"]
        apparent = [row for row in degree_rows if row["exit_group"] == "apparent_closure"]
        combinations = [row for row in degree_rows if row["exit_group"] == "likely_combination_or_successor"]
        other = [row for row in degree_rows if row["exit_group"] == "other_exit"]

        summary_rows.append({
            "first_missing_year": year,
            "academic_year": academic_year_label(year),
            "all_leavers": len(year_rows),
            "degree_granting_leavers": len(degree_rows),
            "apparent_closures_degree_granting": len(apparent),
            "likely_combinations_degree_granting": len(combinations),
            "other_degree_granting_exits": len(other),
            "public_apparent_closures": sum(1 for row in apparent if row["broad_sector"] == "Public"),
            "private_nfp_apparent_closures": sum(1 for row in apparent if row["broad_sector"] == "Private not-for-profit"),
            "private_fp_apparent_closures": sum(1 for row in apparent if row["broad_sector"] == "Private for-profit"),
        })
    return summary_rows


def build_summary_json(rows, apparent_rows, combination_rows, year_summary, available_years):
    return {
        "available_hd_years": available_years,
        "min_hd_year": min(available_years),
        "max_hd_year": max(available_years),
        "all_ipeds_leavers": len(rows),
        "apparent_closures": len(apparent_rows),
        "likely_combinations_or_successors": len(combination_rows),
        "other_exits": len(rows) - len(apparent_rows) - len(combination_rows),
        "years_with_leavers": [row["academic_year"] for row in year_summary],
        "method_note": "This is an IPEDS-only approximation of a selective closure methodology. It is designed to be closer to Kelchen's conservative framing than a raw list of leavers, but it does not replicate federal weekly closed-school-file counts.",
    }


def main():
    annual_rows, available_years = load_hd_rows()
    histories = build_unitid_histories(annual_rows)
    annual_effy = build_headcount_lookup()

    leaver_rows = build_rows(available_years, histories, annual_effy)
    apparent_rows = [row for row in leaver_rows if row["exit_group"] == "apparent_closure"]
    combination_rows = [row for row in leaver_rows if row["exit_group"] == "likely_combination_or_successor"]
    year_summary = build_year_summary(leaver_rows)

    fieldnames = [
        "unitid",
        "institution_name",
        "state",
        "city",
        "first_seen_year",
        "last_seen_year",
        "first_missing_year",
        "academic_year",
        "reappeared_after_gap",
        "act_code",
        "closedat",
        "newid",
        "degree_granting",
        "sector",
        "broad_sector",
        "institutional_level",
        "tracker_category",
        "highest_degree_offered",
        "instcat",
        "opeid8",
        "headcount_year",
        "student_headcount",
        "exit_group",
        "closure_proxy_note",
        "source_group",
        "source_file",
        "source_note",
    ]
    write_csv(LEAVERS_OUTPUT, leaver_rows, fieldnames)
    write_csv(APPARENT_CLOSURES_OUTPUT, apparent_rows, fieldnames)
    write_csv(COMBINATIONS_OUTPUT, combination_rows, fieldnames)
    write_csv(
        YEAR_SUMMARY_OUTPUT,
        year_summary,
        [
            "first_missing_year",
            "academic_year",
            "all_leavers",
            "degree_granting_leavers",
            "apparent_closures_degree_granting",
            "likely_combinations_degree_granting",
            "other_degree_granting_exits",
            "public_apparent_closures",
            "private_nfp_apparent_closures",
            "private_fp_apparent_closures",
        ],
    )

    summary = build_summary_json(leaver_rows, apparent_rows, combination_rows, year_summary, available_years)
    with SUMMARY_JSON_OUTPUT.open("w", encoding="utf-8") as handle:
        json.dump(summary, handle, indent=2)

    print(json.dumps(summary, indent=2))


if __name__ == "__main__":
    main()
