import csv
import html
import json
import urllib.request
import zipfile
from collections import Counter, defaultdict
from pathlib import Path


# Build a closer approximation to the Kelchen/Federal Reserve closure panel by
# aggregating annual IPEDS directory-style files to OPEID6 and pairing that
# panel with federal school-file closure dates from the schfile fixed-width
# extract.
#
# Why a separate script?
# - build_kelchen_style_ipeds_closures.py works at the UNITID level.
# - Kelchen explicitly aggregates to the OPEID level because parent/child
#   reporting agreements make UNITID-only closure counts too noisy.
# - We still do not have the weekly federal closed-school search file, so this
#   remains an IPEDS-only approximation rather than a full replication.
#
# What this script does:
# 1. Pull annual directory-style records from the best file on hand for each
#    year from 1996 through the latest local IPEDS year.
# 2. Collapse all UNITIDs under the same OPEID6 into a single institution-year.
# 3. Build an OPEID-level panel and flag OPEIDs that leave the panel.
# 4. Parse the schfile rectype-01 fixed-width extract and pull close dates for
#    main-campus 00 records, which is much closer to the PEPS-style federal
#    closure logic used in the paper than an IPEDS ACT-code proxy alone.
# 5. Compare multiple sample/closure-rule combinations against Table 3 in the
#    Kelchen/FEDS paper for 1996-2023 and keep the closest federal-schfile
#    variant as the primary output.
# 6. Extend that best-fitting setup through the present schfile year so we can
#    report 1996-present closures separately from the benchmark comparison.
#
# Important limitations:
# - The 1996 and 1997 IPEDS files do not expose the same closure-status fields
#   as the later directory files.
# - We now have the current schfile fixed-width school extract, but not a full
#   historical stack of weekly closed-school files. That means the sample still
#   has to be approximated from IPEDS while the closure signal comes from the
#   federal school file's close dates.

REPO_ROOT = Path(__file__).resolve().parents[1]
IPEDS_DOWNLOADS_DIR = Path(r"C:\Users\mv3031\Downloads")
OUTPUT_DIR = REPO_ROOT / "federal_closure"
SCHFILE_ZIP = IPEDS_DOWNLOADS_DIR / "schfile_access_format_20260402.dat.zip"
CANONICAL_IPEDS_CSV = REPO_ROOT / "ipeds" / "ipeds_financial_health_dataset_2014_2024.csv"
RAW_IPEDS_CSV = REPO_ROOT / "ipeds" / "ipeds_financial_health_raw_2014_2024.csv"
SELECTED_FILE_CATALOG_CSV = REPO_ROOT / "ipeds" / "ipeds_financial_health_selected_file_catalog.csv"

PANEL_OUTPUT = OUTPUT_DIR / "kelchen_like_opeid_panel.csv"
CLOSURES_OUTPUT = OUTPUT_DIR / "kelchen_like_opeid_main_campus_closures.csv"
YEAR_COUNTS_OUTPUT = OUTPUT_DIR / "kelchen_like_opeid_year_counts.csv"
YEAR_TYPE_COUNTS_OUTPUT = OUTPUT_DIR / "kelchen_like_opeid_year_type_counts.csv"
YEAR_UNIVERSE_OUTPUT = OUTPUT_DIR / "kelchen_like_year_universe_closures.csv"
STUDENTS_BY_SECTOR_OUTPUT = OUTPUT_DIR / "kelchen_like_closure_students_by_sector.csv"
CLOSURES_WITH_HEADCOUNT_OUTPUT = OUTPUT_DIR / "kelchen_like_closures_with_headcount.csv"
METRICS_CLOSURE_LIST_OUTPUT = OUTPUT_DIR / "kelchen_like_closures_with_student_staff_metrics_2001_2026.csv"
TABLE3_COMPARE_OUTPUT = OUTPUT_DIR / "kelchen_like_table3_comparison.csv"
VARIANT_COMPARE_OUTPUT = OUTPUT_DIR / "kelchen_like_variant_comparison.csv"
SUMMARY_OUTPUT = OUTPUT_DIR / "kelchen_like_opeid_summary.json"
FIGURE2_OUTPUT = OUTPUT_DIR / "kelchen_like_closures_figure2.png"

START_YEAR = 1996
KELCHEN_COMPARE_END_YEAR = 2023
PANEL_END_YEAR = 2024
PRESENT_END_YEAR = 2026
STUDENT_IMPACT_START_YEAR = 2001

# The older IPEDS directory/history files do not all follow the same naming
# convention. These are the best official NCES files currently recoverable from
# the IPEDS Data Center catalog HTML already saved in the repo.
FILE_MAP = {
    1996: "ic9697_A.zip",
    1997: "ic9798_HDR.zip",
    1998: "IC98hdac.zip",
    1999: "IC99_HD.zip",
    2000: "FA2000HD.zip",
    2001: "FA2001HD.zip",
}
for year in range(2002, PANEL_END_YEAR + 1):
    FILE_MAP[year] = f"HD{year}.zip"


# Table 3 from the Kelchen/Federal Reserve paper gives the cleanest exact
# benchmark we can compare against without reverse-engineering Figure 2 by eye.
KELCHEN_TABLE3 = {
    "Public 4-year": {"institutions": 850, "closures": 2},
    "Public 2-year": {"institutions": 1682, "closures": 45},
    "For-profit 4-year": {"institutions": 473, "closures": 100},
    "For-profit 2-year": {"institutions": 3732, "closures": 1222},
    "Nonprofit 4-year": {"institutions": 2002, "closures": 142},
    "Nonprofit 2-year": {"institutions": 732, "closures": 152},
    "Total": {"institutions": 8633, "closures": 1661},
}


def normalize_text(value):
    if value is None:
        return ""
    return " ".join(str(value).strip().split())


def real_value(value):
    return normalize_text(value) not in {"", "-1", "-2", "-3"}


def opeid6(value):
    value = normalize_text(value)
    if not real_value(value):
        return ""
    digits = "".join(ch for ch in value if ch.isdigit())
    if not digits:
        return ""
    return digits.zfill(8)[:6]


def read_rows_from_zip(zip_path):
    with zipfile.ZipFile(zip_path) as zf:
        member = next(name for name in zf.namelist() if name.lower().endswith(".csv"))
        with zf.open(member, "r") as handle:
            text = handle.read().decode("utf-8-sig", errors="ignore").splitlines()
    return [
        {str(key).lower(): normalize_text(value) for key, value in row.items()}
        for row in csv.DictReader(text)
    ]


def available_effy_years():
    return [
        year
        for year in range(2008, PANEL_END_YEAR + 1)
        if (IPEDS_DOWNLOADS_DIR / f"EFFY{year}.zip").exists()
    ]


def to_number(value):
    text = normalize_text(value)
    if not real_value(text):
        return None
    try:
        return float(text.replace(",", ""))
    except ValueError:
        return None


def selected_file_catalog():
    rows = []
    with SELECTED_FILE_CATALOG_CSV.open("r", encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            rows.append(row)
    return rows


def eap_zip_path(year):
    return IPEDS_DOWNLOADS_DIR / f"EAP{year}.zip"


def ensure_eap_zip(year, catalog_rows):
    zip_path = eap_zip_path(year)
    if zip_path.exists():
        return zip_path

    row = next((item for item in catalog_rows if item.get("table_name") == f"EAP{year}"), None)
    if row is None:
        return None
    url = html.unescape(row.get("data_url", ""))
    if not url:
        return None

    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req, timeout=120) as response:
        zip_path.write_bytes(response.read())
    return zip_path


def preferred_name(rows):
    names = [row.get("instnm", "") for row in rows if real_value(row.get("instnm"))]
    return Counter(names).most_common(1)[0][0] if names else ""


def preferred_state(rows):
    states = [row.get("stabbr", "") for row in rows if real_value(row.get("stabbr"))]
    return Counter(states).most_common(1)[0][0] if states else ""


def broad_sector_from_controls(rows):
    controls = {row.get("control", "") for row in rows if real_value(row.get("control"))}
    if "1" in controls:
        return "Public"
    if "2" in controls:
        return "Nonprofit"
    if "3" in controls:
        return "For-profit"
    return "Unknown"


def level_from_iclevel(rows):
    # Kelchen collapses the panel into 4-year versus 2-year. To mirror that, we
    # treat any institution with at least one 4-year component as 4-year and
    # everything else as 2-year.
    levels = {row.get("iclevel", "") for row in rows if real_value(row.get("iclevel"))}
    if "1" in levels:
        return "4-year"
    if levels:
        return "2-year"
    return "Unknown"


def sample_type_label(broad_sector, level):
    if broad_sector == "Unknown":
        return "Unknown"
    if level == "Unknown":
        return broad_sector
    return f"{broad_sector} {level}"


def build_yearly_groups(end_year):
    groups = defaultdict(list)
    for year in range(START_YEAR, end_year + 1):
        zip_name = FILE_MAP.get(year)
        if not zip_name:
            continue
        zip_path = IPEDS_DOWNLOADS_DIR / zip_name
        if not zip_path.exists():
            continue
        for row in read_rows_from_zip(zip_path):
            op6 = opeid6(row.get("opeid"))
            if not op6:
                continue
            row["source_year"] = str(year)
            row["source_zip"] = zip_name
            groups[(op6, year)].append(row)
    return groups


def build_unitid_to_opeid_by_year(groups):
    mapping = defaultdict(dict)
    for (op6, year), rows in groups.items():
        for row in rows:
            unitid = normalize_text(row.get("unitid"))
            if real_value(unitid):
                mapping[year][unitid] = op6
    return mapping


def build_panel(groups):
    by_opeid = defaultdict(list)
    for (op6, year), rows in groups.items():
        by_opeid[op6].append({
            "opeid6": op6,
            "year": year,
            "institution_name": preferred_name(rows),
            "state": preferred_state(rows),
            "broad_sector": broad_sector_from_controls(rows),
            "level": level_from_iclevel(rows),
            "sample_type": sample_type_label(broad_sector_from_controls(rows), level_from_iclevel(rows)),
            "acts": sorted({row.get("act", "") for row in rows if real_value(row.get("act"))}),
            "newids": sorted({row.get("newid", "") for row in rows if real_value(row.get("newid"))}),
            "flags": sorted({row.get("opeflag", "") for row in rows if real_value(row.get("opeflag"))}),
            "rows": rows,
        })
    for entries in by_opeid.values():
        entries.sort(key=lambda item: item["year"])
    return by_opeid


def parse_schfile_rectype01():
    # The user found the current weekly school file access-format zip in
    # Downloads. The closure_file_processing.R script already documented the
    # fixed-width positions, so we reuse those exact offsets here.
    if not SCHFILE_ZIP.exists():
        return {}

    def fwf(line, start, end):
        return normalize_text(line[start - 1:end])

    records = {}
    with zipfile.ZipFile(SCHFILE_ZIP) as zf:
        member = next(name for name in zf.namelist() if "rectype_01" in name.lower())
        with zf.open(member, "r") as handle:
            for raw_line in handle:
                line = raw_line.decode("latin-1", errors="ignore").rstrip("\r\n")
                opeid = fwf(line, 3, 10)
                digits = "".join(ch for ch in opeid if ch.isdigit()).zfill(8)
                if not digits.strip("0"):
                    continue

                op6 = digits[:6]
                suffix = digits[6:]
                close_dt = fwf(line, 382, 389)
                reinstate_dt = fwf(line, 412, 419)

                records.setdefault(op6, []).append({
                    "opeid8": digits,
                    "opeid6": op6,
                    "suffix": suffix,
                    "school_name": fwf(line, 12, 81),
                    "location_name": fwf(line, 82, 151),
                    "city": fwf(line, 222, 246),
                    "state": fwf(line, 247, 248),
                    "country": fwf(line, 252, 276),
                    "branch_ind": fwf(line, 409, 409),
                    "elig_status_ind": fwf(line, 316, 316),
                    "pgm_length": fwf(line, 355, 356),
                    "sch_type": fwf(line, 357, 357),
                    "close_dt": close_dt,
                    "close_year": int(close_dt[:4]) if len(close_dt) >= 4 and close_dt[:4].isdigit() else "",
                    "reinstate_dt": reinstate_dt,
                    "reinstate_year": int(reinstate_dt[:4]) if len(reinstate_dt) >= 4 and reinstate_dt[:4].isdigit() else "",
                })
    return records


def names_match(a, b):
    def compact(value):
        value = normalize_text(value).lower()
        keep = []
        for char in value:
            keep.append(char if char.isalnum() else " ")
        return " ".join("".join(keep).split())

    return compact(a) == compact(b)


def federal_main_campus_closures(schfile_records, closure_end_year):
    # Kelchen focuses on main-campus/full-system closures after aggregating to
    # OPEID. The closest simple proxy available from the current federal school
    # file is whether the 00 main-campus record has a closure date on or before
    # file is whether the 00 main-campus record has a closure date on or before
    # 2023.
    #
    # Refinements to get closer to the paper:
    # - use the *first* closure, not the latest, if a school closed/reopened
    #   and later closed again.
    # - drop obvious fake 00 rows where the location name clearly identifies a
    #   branch/additional site rather than the main campus.
    # - drop foreign locations.
    closures = {}
    for op6, rows in schfile_records.items():
        main_rows = [row for row in rows if row["suffix"] == "00"]
        if not main_rows:
            continue

        chosen = None
        for row in main_rows:
            close_year = row["close_year"]
            if not close_year or close_year > closure_end_year:
                continue

            # Exclude foreign locations from the main-campus closure counts.
            if real_value(row["country"]):
                continue

            # Kelchen notes that a small number of 00 OPEIDs are really branch
            # campuses. The clearest machine-detectable version of that problem
            # is when the 00 record's location name is present and plainly not
            # the same as the school name.
            if real_value(row["location_name"]) and not names_match(row["school_name"], row["location_name"]):
                continue

            # Count the first closure only.
            if chosen is None or close_year < chosen["close_year"]:
                chosen = row

        if chosen is not None:
            closures[op6] = chosen
    return closures


def classify_exit(last_entry):
    acts = set(last_entry["acts"])
    has_successor = any(real_value(value) for value in last_entry["newids"])
    has_closure_code = bool(acts & {"D", "M"})
    has_combo_code = "C" in acts

    if has_combo_code or has_successor:
        return "likely_combination_or_successor"
    if has_closure_code:
        return "apparent_main_campus_closure"
    return "other_exit"


def build_opeid_panel_rows(by_opeid):
    rows = []
    for op6, entries in by_opeid.items():
        first_entry = entries[0]
        last_entry = entries[-1]
        all_flags = sorted({flag for entry in entries for flag in entry["flags"]})
        all_broad_sectors = sorted({entry["broad_sector"] for entry in entries if entry["broad_sector"] != "Unknown"})
        all_levels = sorted({entry["level"] for entry in entries if entry["level"] != "Unknown"})
        exit_group = ""
        first_missing_year = ""
        if last_entry["year"] < PANEL_END_YEAR:
            first_missing_year = last_entry["year"] + 1
            exit_group = classify_exit(last_entry)

        rows.append({
            "opeid6": op6,
            "institution_name_first_seen": first_entry["institution_name"],
            "institution_name_last_seen": last_entry["institution_name"],
            "state_last_seen": last_entry["state"],
            "first_seen_year": first_entry["year"],
            "last_seen_year": last_entry["year"],
            "first_missing_year": first_missing_year,
            "ever_opeflag_values": "; ".join(all_flags),
            "ever_opeflag_1to5": "yes" if any(flag in {"1", "2", "3", "4", "5"} for flag in all_flags) else "no",
            "ever_opeflag_6": "yes" if "6" in all_flags else "no",
            "ever_broad_sectors": "; ".join(all_broad_sectors),
            "ever_levels": "; ".join(all_levels),
            "sample_type_last_seen": last_entry["sample_type"],
            "exit_group": exit_group,
            "last_seen_acts": "; ".join(last_entry["acts"]),
            "last_seen_newids": "; ".join(last_entry["newids"]),
            "coverage_note": (
                "1996-1997 files do not expose the same closure-status fields as later directory files."
                if last_entry["year"] <= 1997 else ""
            ),
        })
    rows.sort(key=lambda item: (item["first_seen_year"], item["opeid6"]))
    return rows


def preferred_sample_opeids(panel_rows):
    # Best IPEDS-only approximation to the paper's federal-aid-eligible OPEID
    # panel. Requiring at least one observed OPEFLAG 1-5 is transparent and
    # gets very close to the published sample size.
    return {
        row["opeid6"]
        for row in panel_rows
        if row["ever_opeflag_1to5"] == "yes"
    }


def all_valid_sample_opeids(panel_rows):
    return {row["opeid6"] for row in panel_rows}


def known_type_sample_opeids(panel_rows):
    # Exclude OPEIDs that never resolve to a usable public/nonprofit/for-profit
    # and 2-year/4-year combination across the panel.
    sample = set()
    for row in panel_rows:
        has_known_sector = real_value(row["ever_broad_sectors"])
        has_known_level = real_value(row["ever_levels"])
        if has_known_sector and has_known_level:
            sample.add(row["opeid6"])
    return sample


def no_1996_only_blank_sample_opeids(panel_rows):
    sample = set()
    for row in panel_rows:
        drop = (
            row["ever_opeflag_1to5"] == "no"
            and row["ever_opeflag_6"] == "no"
            and row["first_seen_year"] == 1996
            and row["last_seen_year"] == 1996
        )
        if not drop:
            sample.add(row["opeid6"])
    return sample


def build_ipeds_proxy_closure_rows(panel_rows, by_opeid, sample_opeids):
    rows = []
    for panel_row in panel_rows:
        if panel_row["opeid6"] not in sample_opeids:
            continue
        if panel_row["exit_group"] != "apparent_main_campus_closure":
            continue
        last_entry = by_opeid[panel_row["opeid6"]][-1]
        rows.append({
            "opeid6": panel_row["opeid6"],
            "institution_name": panel_row["institution_name_last_seen"],
            "state": panel_row["state_last_seen"],
            "closure_year": panel_row["first_missing_year"],
            "sample_type": last_entry["sample_type"],
            "broad_sector": last_entry["broad_sector"],
            "level": last_entry["level"],
            "last_seen_year": panel_row["last_seen_year"],
            "last_seen_acts": panel_row["last_seen_acts"],
            "last_seen_newids": panel_row["last_seen_newids"],
            "closure_rule": "IPEDS_ACT_proxy",
            "source_note": "IPEDS-only OPEID6 apparent closure proxy; not a full PEPS replication.",
        })
    rows.sort(key=lambda item: (item["closure_year"], item["institution_name"], item["opeid6"]))
    return rows


def build_schfile_closure_rows(by_opeid, sample_opeids, schfile_main_closures):
    rows = []
    for op6 in sorted(sample_opeids & set(schfile_main_closures)):
        federal_row = schfile_main_closures[op6]
        entries = by_opeid.get(op6, [])
        # Use the last IPEDS-seen sample type when possible so the resulting
        # breakdown can be compared to Kelchen's institution-type counts.
        last_entry = entries[-1] if entries else {
            "sample_type": "Unknown",
            "broad_sector": "Unknown",
            "level": "Unknown",
        }
        rows.append({
            "opeid6": op6,
            "institution_name": federal_row["school_name"] or (entries[-1]["institution_name"] if entries else ""),
            "state": federal_row["state"] or (entries[-1]["state"] if entries else ""),
            "closure_year": federal_row["close_year"],
            "sample_type": last_entry["sample_type"],
            "broad_sector": last_entry["broad_sector"],
            "level": last_entry["level"],
            "ever_broad_sectors": "; ".join(sorted({entry["broad_sector"] for entry in entries if entry["broad_sector"] != "Unknown"})),
            "overall_level": (
                "4-year"
                if any(entry["level"] == "4-year" for entry in entries)
                else ("2-year" if any(entry["level"] == "2-year" for entry in entries) else "Unknown")
            ),
            "last_seen_year": entries[-1]["year"] if entries else "",
            "last_seen_acts": "; ".join(entries[-1]["acts"]) if entries else "",
            "last_seen_newids": "; ".join(entries[-1]["newids"]) if entries else "",
            "closure_rule": "schfile_main00_close_date",
            "source_note": "Federal schfile rectype-01 main-campus 00 close date proxy.",
        })
    rows.sort(key=lambda item: (item["closure_year"], item["institution_name"], item["opeid6"]))
    return rows


def filter_closure_rows_to_year(closure_rows, closure_end_year):
    return [row for row in closure_rows if int(row["closure_year"]) <= closure_end_year]


def build_year_counts(panel_rows, closure_rows, sample_opeids):
    # Report all exits, apparent closures, and likely combinations by year so we
    # can see the gap between the conservative closure proxy and all OPEID
    # disappearances.
    panel_lookup = {row["opeid6"]: row for row in panel_rows}
    all_exit_counter = Counter()
    closure_counter = Counter()
    combo_counter = Counter()

    for row in panel_rows:
        if row["opeid6"] not in sample_opeids or not row["first_missing_year"]:
            continue
        all_exit_counter[int(row["first_missing_year"])] += 1
        if row["exit_group"] == "likely_combination_or_successor":
            combo_counter[int(row["first_missing_year"])] += 1

    for row in closure_rows:
        closure_counter[int(row["closure_year"])] += 1

    summary_rows = []
    for year in range(START_YEAR, PRESENT_END_YEAR + 1):
        summary_rows.append({
            "year": year,
            "all_opeid_exits": all_exit_counter[year],
            "main_campus_closures": closure_counter[year],
            "likely_combinations_or_successors": combo_counter[year],
            "coverage_note": (
                "Closure-status fields are incomplete in recovered 1996-1997 IPEDS files."
                if year <= 1998 else (
                    "IPEDS panel exits stop after 2024 because newer IPEDS directory files are not yet available locally."
                    if year > PANEL_END_YEAR else ""
                )
            ),
        })
    return summary_rows


def membership_types_for_panel_row(panel_row):
    # Mirror the paper's treatment of sector changes:
    # - if a school changes sector, count it in every observed sector
    # - if any component is 4-year, treat the OPEID as 4-year overall
    sectors = [sector for sector in panel_row["ever_broad_sectors"].split("; ") if real_value(sector)]
    overall_level = "4-year" if "4-year" in panel_row["ever_levels"].split("; ") else ("2-year" if "2-year" in panel_row["ever_levels"].split("; ") else "Unknown")
    if overall_level == "Unknown":
        return []
    return [f"{sector} {overall_level}" for sector in sectors]


def build_table3_comparison(by_opeid, closure_rows, sample_opeids):
    panel_lookup = {row["opeid6"]: row for row in build_opeid_panel_rows(by_opeid)}
    institutions_by_type = defaultdict(set)
    for op6 in sample_opeids:
        panel_row = panel_lookup[op6]
        for sample_type in membership_types_for_panel_row(panel_row):
            institutions_by_type[sample_type].add(op6)

    closures_by_type = Counter()
    for row in closure_rows:
        for sector in [sector for sector in row["ever_broad_sectors"].split("; ") if real_value(sector)]:
            level = row["overall_level"]
            if level != "Unknown":
                closures_by_type[f"{sector} {level}"] += 1

    comparison_rows = []
    ordered_types = [
        "Public 4-year",
        "Public 2-year",
        "For-profit 4-year",
        "For-profit 2-year",
        "Nonprofit 4-year",
        "Nonprofit 2-year",
    ]
    for sample_type in ordered_types:
        kelchen = KELCHEN_TABLE3[sample_type]
        our_institutions = len(institutions_by_type[sample_type])
        our_closures = closures_by_type[sample_type]
        comparison_rows.append({
            "sample_type": sample_type,
            "kelchen_institutions": kelchen["institutions"],
            "our_institutions": our_institutions,
            "institution_difference": our_institutions - kelchen["institutions"],
            "kelchen_closures": kelchen["closures"],
            "our_main_campus_closures": our_closures,
            "closure_difference": our_closures - kelchen["closures"],
        })

    comparison_rows.append({
        "sample_type": "Total",
        "kelchen_institutions": KELCHEN_TABLE3["Total"]["institutions"],
        "our_institutions": len(sample_opeids),
        "institution_difference": len(sample_opeids) - KELCHEN_TABLE3["Total"]["institutions"],
        "kelchen_closures": KELCHEN_TABLE3["Total"]["closures"],
        "our_main_campus_closures": len(closure_rows),
        "closure_difference": len(closure_rows) - KELCHEN_TABLE3["Total"]["closures"],
    })
    return comparison_rows


def build_year_type_counts(by_opeid, closure_rows):
    counts = Counter()
    for row in closure_rows:
        for sector in [sector for sector in row["ever_broad_sectors"].split("; ") if real_value(sector)]:
            if row["overall_level"] == "Unknown":
                continue
            counts[(int(row["closure_year"]), f"{sector} {row['overall_level']}")] += 1

    rows = []
    ordered_types = [
        "Public 4-year",
        "Public 2-year",
        "For-profit 4-year",
        "For-profit 2-year",
        "Nonprofit 4-year",
        "Nonprofit 2-year",
    ]
    for year in range(START_YEAR, PRESENT_END_YEAR + 1):
        for sample_type in ordered_types:
            rows.append({
                "year": year,
                "sample_type": sample_type,
                "main_campus_closures": counts[(year, sample_type)],
                "coverage_note": (
                    "1996-1998 are less reliable, and 2023 may exceed the paper because the current schfile can include later backfilled 2023 closures."
                    if year <= 1998 or year == KELCHEN_COMPARE_END_YEAR else ""
                ),
            })
    return rows


def build_year_universe_closures(by_opeid, sample_opeids, closure_rows):
    closure_counter = Counter(int(row["closure_year"]) for row in closure_rows)
    rows = []
    for year in range(START_YEAR, PRESENT_END_YEAR + 1):
        if year <= PANEL_END_YEAR:
            universe = sum(
                1
                for op6 in sample_opeids
                if any(entry["year"] == year for entry in by_opeid.get(op6, []))
            )
        else:
            universe = ""

        rows.append({
            "year": year,
            "universe": universe,
            "closures": closure_counter[year],
            "coverage_note": (
                "Universe comes from the selected OPEID6 IPEDS panel. Closures come from the current federal schfile main-campus close-date signal."
                if year <= PANEL_END_YEAR else
                "Closures are extended from the current federal schfile, but comparable IPEDS universe counts are not yet available locally."
            ),
        })
    return rows


def build_opeid_headcount_by_year(unitid_to_opeid_by_year):
    # Use annual 12-month unduplicated headcount from EFFY and aggregate
    # UNITIDs to OPEID6 using same-year directory mappings.
    headcounts = defaultdict(dict)
    for year in available_effy_years():
        year_unitid_map = unitid_to_opeid_by_year.get(year, {})
        if not year_unitid_map:
            continue

        zip_path = IPEDS_DOWNLOADS_DIR / f"EFFY{year}.zip"
        for row in read_rows_from_zip(zip_path):
            if normalize_text(row.get("effylev")) != "1":
                continue
            unitid = normalize_text(row.get("unitid"))
            op6 = year_unitid_map.get(unitid)
            if not op6:
                continue
            value = normalize_text(row.get("efytotlt"))
            if not real_value(value):
                continue
            try:
                headcounts[op6][year] = headcounts[op6].get(year, 0) + int(float(value))
            except ValueError:
                continue
    return headcounts


def build_opeid_metrics_by_year(unitid_to_opeid_by_year):
    # The raw all-institutions IPEDS file gives us student FTE and staff FTE
    # across a much broader universe than the tracker-only canonical dataset.
    metrics = defaultdict(lambda: defaultdict(lambda: {
        "student_fte": 0.0,
        "student_fte_count": 0,
        "staff_fte": 0.0,
        "staff_fte_count": 0,
    }))

    if not RAW_IPEDS_CSV.exists():
        return {}

    with RAW_IPEDS_CSV.open("r", encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            year_num = to_number(row.get("year"))
            if year_num is None:
                continue
            year = int(year_num)
            unitid = normalize_text(row.get("unitid"))
            op6 = unitid_to_opeid_by_year.get(year, {}).get(unitid)
            if not op6:
                continue

            store = metrics[op6][year]
            for out_key, field_name in [
                ("student_fte", "fte_12_months"),
                ("staff_fte", "fte_total_staff"),
            ]:
                value = to_number(row.get(field_name))
                if value is None:
                    continue
                store[out_key] += value
                store[f"{out_key}_count"] += 1

    finalized = defaultdict(dict)
    for op6, year_map in metrics.items():
        for year, values in year_map.items():
            finalized[op6][year] = {
                "student_fte": round(values["student_fte"], 1) if values["student_fte_count"] else "",
                "staff_fte": round(values["staff_fte"], 1) if values["staff_fte_count"] else "",
            }
    return finalized


def build_opeid_staff_headcount_by_year(unitid_to_opeid_by_year):
    staff_counts = defaultdict(dict)
    catalog_rows = selected_file_catalog()
    for year in range(2014, PANEL_END_YEAR + 1):
        year_unitid_map = unitid_to_opeid_by_year.get(year, {})
        if not year_unitid_map:
            continue
        zip_path = ensure_eap_zip(year, catalog_rows)
        if not zip_path or not zip_path.exists():
            continue

        for row in read_rows_from_zip(zip_path):
            if normalize_text(row.get("eapcat")) != "10000":
                continue
            unitid = normalize_text(row.get("unitid"))
            op6 = year_unitid_map.get(unitid)
            if not op6:
                continue
            value = to_number(row.get("eaptot"))
            if value is None:
                continue
            staff_counts[op6][year] = staff_counts[op6].get(year, 0) + int(value)
    return staff_counts


def closure_row_membership_types(row):
    sectors = [sector for sector in row["ever_broad_sectors"].split("; ") if real_value(sector)]
    if row["overall_level"] == "Unknown":
        return []
    return [f"{sector} {row['overall_level']}" for sector in sectors]


def build_closures_with_headcount(closure_rows, opeid_headcount_by_year, opeid_metrics_by_year, opeid_staff_headcount_by_year):
    rows = []
    for row in closure_rows:
        closure_year = int(row["closure_year"])
        if closure_year < STUDENT_IMPACT_START_YEAR or closure_year > PRESENT_END_YEAR:
            continue

        available_years = sorted(
            year
            for year in opeid_headcount_by_year.get(row["opeid6"], {})
            if year <= min(closure_year, PANEL_END_YEAR)
        )
        matched_year = available_years[-1] if available_years else ""
        matched_headcount = opeid_headcount_by_year[row["opeid6"]][matched_year] if matched_year else ""
        metrics_year = closure_year - 2
        metrics = opeid_metrics_by_year.get(row["opeid6"], {}).get(metrics_year, {})
        student_headcount_two_years_prior = opeid_headcount_by_year.get(row["opeid6"], {}).get(metrics_year, "")
        staff_headcount_two_years_prior = opeid_staff_headcount_by_year.get(row["opeid6"], {}).get(metrics_year, "")

        rows.append({
            **row,
            "matched_headcount_year": matched_year,
            "matched_student_headcount": matched_headcount,
            "metrics_year_two_years_prior": metrics_year,
            "student_headcount_two_years_prior": student_headcount_two_years_prior,
            "student_fte_two_years_prior": metrics.get("student_fte", ""),
            "staff_headcount_two_years_prior": staff_headcount_two_years_prior,
            "staff_fte_two_years_prior": metrics.get("staff_fte", ""),
            "headcount_note": (
                "Last available EFFY total headcount at or before the closure year."
                if matched_year else
                "No matched EFFY headcount available."
            ),
            "metrics_note": (
                "Student headcount comes from EFFY, student FTE/staff FTE come from the raw all-institutions tracker IPEDS file, and staff headcount comes from EAP; all are aligned to two years before closure when available."
                if student_headcount_two_years_prior != "" or staff_headcount_two_years_prior != "" or metrics else
                "No matched student/staff metrics available two years before closure."
            ),
        })
    return rows


def build_closure_students_by_sector(closures_with_headcount):
    summary_rows = []
    matched_rows = [row for row in closures_with_headcount if row["matched_student_headcount"] != ""]
    fte_rows = [row for row in closures_with_headcount if row["student_fte_two_years_prior"] != ""]
    summary_rows.append({
        "group": "Overall",
        "closures": len(closures_with_headcount),
        "closures_with_student_fte": len(fte_rows),
        "student_headcount": sum(int(row["matched_student_headcount"]) for row in matched_rows),
        "median_student_fte_two_years_prior": median_value([row["student_fte_two_years_prior"] for row in fte_rows]),
        "median_student_headcount_two_years_prior": median_value([row["student_headcount_two_years_prior"] for row in fte_rows]),
        "median_staff_fte_two_years_prior": median_value([row["staff_fte_two_years_prior"] for row in fte_rows]),
        "median_staff_headcount_two_years_prior": median_value([row["staff_headcount_two_years_prior"] for row in fte_rows]),
        "coverage_note": "Overall total does not double-count institutions.",
    })

    sector_groups = [
        ("Private for-profit", "For-profit"),
        ("Private nonprofit", "Nonprofit"),
        ("Public", "Public"),
    ]
    for label, sector in sector_groups:
        sector_rows = [
            row for row in matched_rows
            if any(membership.startswith(sector) for membership in closure_row_membership_types(row))
        ]
        sector_fte_rows = [
            row for row in fte_rows
            if any(membership.startswith(sector) for membership in closure_row_membership_types(row))
        ]
        summary_rows.append({
            "group": label,
            "closures": len([
                row for row in closures_with_headcount
                if any(membership.startswith(sector) for membership in closure_row_membership_types(row))
            ]),
            "closures_with_student_fte": len(sector_fte_rows),
            "student_headcount": sum(int(row["matched_student_headcount"]) for row in sector_rows),
            "median_student_fte_two_years_prior": median_value([row["student_fte_two_years_prior"] for row in sector_fte_rows]),
            "median_student_headcount_two_years_prior": median_value([row["student_headcount_two_years_prior"] for row in sector_fte_rows]),
            "median_staff_fte_two_years_prior": median_value([row["staff_fte_two_years_prior"] for row in sector_fte_rows]),
            "median_staff_headcount_two_years_prior": median_value([row["staff_headcount_two_years_prior"] for row in sector_fte_rows]),
            "coverage_note": "Sector rows can double-count a small number of institutions that changed sectors over time.",
        })
    return summary_rows


def build_kelchen_figure2(year_type_rows):
    try:
        from PIL import Image, ImageDraw, ImageFont
    except ImportError:
        return False

    ordered_types = [
        "For-profit 2-year",
        "Nonprofit 2-year",
        "Public 2-year",
        "For-profit 4-year",
        "Nonprofit 4-year",
        "Public 4-year",
    ]
    colors = {
        "For-profit 2-year": "#3d73c9",
        "Nonprofit 2-year": "#c6ddff",
        "Public 2-year": "#2f5f56",
        "For-profit 4-year": "#d4ad33",
        "Nonprofit 4-year": "#8dc6b0",
        "Public 4-year": "#6f6243",
    }
    years = list(range(START_YEAR, PRESENT_END_YEAR + 1))
    lookup = {
        (int(row["year"]), row["sample_type"]): int(row["main_campus_closures"])
        for row in year_type_rows
    }
    max_y = max(5, max(lookup.values()))
    max_y = ((max_y + 9) // 10) * 10

    width, height = 1400, 900
    margin_left, margin_right = 110, 70
    margin_top, margin_bottom = 95, 160
    plot_left = margin_left
    plot_top = margin_top
    plot_right = width - margin_right
    plot_bottom = height - margin_bottom
    plot_width = plot_right - plot_left
    plot_height = plot_bottom - plot_top

    image = Image.new("RGB", (width, height), "white")
    draw = ImageDraw.Draw(image)
    title_font = ImageFont.load_default()
    body_font = ImageFont.load_default()

    draw.text((plot_left, 28), "Number of Main-Campus Closures by Institution Type and Year, 1996-2026", fill="black", font=title_font)

    # grid and y-axis labels
    y_ticks = list(range(0, max_y + 1, 25 if max_y > 100 else 10))
    if y_ticks[-1] != max_y:
        y_ticks.append(max_y)
    for tick in y_ticks:
        y = plot_bottom - (tick / max_y) * plot_height
        draw.line((plot_left, y, plot_right, y), fill="#d9d9d9", width=1)
        draw.text((40, y - 8), str(tick), fill="black", font=body_font)

    # axes
    draw.line((plot_left, plot_top, plot_left, plot_bottom), fill="black", width=2)
    draw.line((plot_left, plot_bottom, plot_right, plot_bottom), fill="black", width=2)
    draw.text((36, plot_top - 26), "# Closures", fill="black", font=body_font)
    draw.text((plot_left + plot_width // 2 - 12, plot_bottom + 40), "Year", fill="black", font=body_font)

    x_positions = {
        year: plot_left + ((year - START_YEAR) / (PRESENT_END_YEAR - START_YEAR)) * plot_width
        for year in years
    }
    for year in years:
        if year % 2 == 0:
            x = x_positions[year]
            draw.text((x - 12, plot_bottom + 12), str(year), fill="black", font=body_font)

    for sample_type in ordered_types:
        points = []
        for year in years:
            value = lookup.get((year, sample_type), 0)
            x = x_positions[year]
            y = plot_bottom - (value / max_y) * plot_height
            points.append((x, y))
        draw.line(points, fill=colors[sample_type], width=4)

    # legend
    legend_x = plot_left + 40
    legend_y = height - 110
    for idx, sample_type in enumerate(ordered_types):
        row = idx // 3
        col = idx % 3
        x = legend_x + col * 390
        y = legend_y + row * 28
        draw.line((x, y + 7, x + 30, y + 7), fill=colors[sample_type], width=4)
        draw.text((x + 40, y), sample_type, fill="black", font=body_font)

    FIGURE2_OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    image.save(FIGURE2_OUTPUT)
    return True


def median_value(values):
    cleaned = sorted(float(value) for value in values if value not in {"", None})
    if not cleaned:
        return ""
    mid = len(cleaned) // 2
    if len(cleaned) % 2 == 1:
        return round(cleaned[mid], 1)
    return round((cleaned[mid - 1] + cleaned[mid]) / 2, 1)


def build_variant_comparison(by_opeid, panel_rows, schfile_main_closures):
    variants = []
    sample_variant_map = sample_variants(panel_rows)

    for sample_name, sample_opeids in sample_variant_map.items():
        closure_variants = {
            "IPEDS_ACT_proxy": filter_closure_rows_to_year(
                build_ipeds_proxy_closure_rows(panel_rows, by_opeid, sample_opeids),
                KELCHEN_COMPARE_END_YEAR,
            ),
            "schfile_main00_close_date": filter_closure_rows_to_year(
                build_schfile_closure_rows(by_opeid, sample_opeids, schfile_main_closures),
                KELCHEN_COMPARE_END_YEAR,
            ),
        }
        for closure_name, closure_rows in closure_variants.items():
            variants.append({
                "sample_variant": sample_name,
                "closure_variant": closure_name,
                "sample_size": len(sample_opeids),
                "sample_gap_vs_kelchen": len(sample_opeids) - KELCHEN_TABLE3["Total"]["institutions"],
                "main_campus_closures": len(closure_rows),
                "closure_gap_vs_kelchen": len(closure_rows) - KELCHEN_TABLE3["Total"]["closures"],
                "absolute_total_gap": abs(len(sample_opeids) - KELCHEN_TABLE3["Total"]["institutions"]) + abs(len(closure_rows) - KELCHEN_TABLE3["Total"]["closures"]),
            })
    variants.sort(key=lambda row: (row["absolute_total_gap"], abs(row["sample_gap_vs_kelchen"]), abs(row["closure_gap_vs_kelchen"])))
    return variants


def sample_variants(panel_rows):
    return {
        "preferred_opeflag_1to5": preferred_sample_opeids(panel_rows),
        "all_valid_opeid6": all_valid_sample_opeids(panel_rows),
        "all_valid_known_types": known_type_sample_opeids(panel_rows),
        "all_valid_no_1996_only_blank": no_1996_only_blank_sample_opeids(panel_rows),
    }


def write_csv(path, rows, fieldnames):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def main():
    yearly_groups_benchmark = build_yearly_groups(KELCHEN_COMPARE_END_YEAR)
    by_opeid_benchmark = build_panel(yearly_groups_benchmark)
    panel_rows_benchmark = build_opeid_panel_rows(by_opeid_benchmark)

    yearly_groups = build_yearly_groups(PANEL_END_YEAR)
    unitid_to_opeid_by_year = build_unitid_to_opeid_by_year(yearly_groups)
    by_opeid = build_panel(yearly_groups)
    panel_rows = build_opeid_panel_rows(by_opeid)
    schfile_records = parse_schfile_rectype01()
    opeid_headcount_by_year = build_opeid_headcount_by_year(unitid_to_opeid_by_year)
    opeid_metrics_by_year = build_opeid_metrics_by_year(unitid_to_opeid_by_year)
    opeid_staff_headcount_by_year = build_opeid_staff_headcount_by_year(unitid_to_opeid_by_year)
    schfile_main_closures_2023 = federal_main_campus_closures(schfile_records, KELCHEN_COMPARE_END_YEAR)
    schfile_main_closures_present = federal_main_campus_closures(schfile_records, PRESENT_END_YEAR)
    variant_rows = build_variant_comparison(by_opeid_benchmark, panel_rows_benchmark, schfile_main_closures_2023)
    samples = sample_variants(panel_rows_benchmark)

    # Keep the comparison anchored to the 2023-panel variant that best matched
    # Kelchen before extending the closure series to the present.
    selected_sample_variant = "all_valid_no_1996_only_blank"
    sample_opeids = samples[selected_sample_variant]
    closure_rows = build_schfile_closure_rows(by_opeid, sample_opeids, schfile_main_closures_present)
    closure_rows_2023 = filter_closure_rows_to_year(closure_rows, KELCHEN_COMPARE_END_YEAR)
    year_count_rows = build_year_counts(panel_rows, closure_rows, sample_opeids)
    year_type_count_rows = build_year_type_counts(by_opeid, closure_rows)
    year_universe_rows = build_year_universe_closures(by_opeid, sample_opeids, closure_rows)
    closures_with_headcount = build_closures_with_headcount(closure_rows, opeid_headcount_by_year, opeid_metrics_by_year, opeid_staff_headcount_by_year)
    students_by_sector_rows = build_closure_students_by_sector(closures_with_headcount)
    table3_rows = build_table3_comparison(by_opeid_benchmark, closure_rows_2023, sample_opeids)
    figure_built = build_kelchen_figure2(year_type_count_rows)

    write_csv(
        PANEL_OUTPUT,
        panel_rows,
        [
            "opeid6",
            "institution_name_first_seen",
            "institution_name_last_seen",
            "state_last_seen",
            "first_seen_year",
            "last_seen_year",
            "first_missing_year",
            "ever_opeflag_values",
            "ever_opeflag_1to5",
            "ever_opeflag_6",
            "ever_broad_sectors",
            "ever_levels",
            "sample_type_last_seen",
            "exit_group",
            "last_seen_acts",
            "last_seen_newids",
            "coverage_note",
        ],
    )
    write_csv(
        CLOSURES_OUTPUT,
        closure_rows,
        [
            "opeid6",
            "institution_name",
            "state",
            "closure_year",
            "sample_type",
            "broad_sector",
            "level",
            "ever_broad_sectors",
            "overall_level",
            "last_seen_year",
            "last_seen_acts",
            "last_seen_newids",
            "source_note",
            "closure_rule",
        ],
    )
    write_csv(
        YEAR_COUNTS_OUTPUT,
        year_count_rows,
        [
            "year",
            "all_opeid_exits",
            "main_campus_closures",
            "likely_combinations_or_successors",
            "coverage_note",
        ],
    )
    write_csv(
        YEAR_TYPE_COUNTS_OUTPUT,
        year_type_count_rows,
        [
            "year",
            "sample_type",
            "main_campus_closures",
            "coverage_note",
        ],
    )
    write_csv(
        YEAR_UNIVERSE_OUTPUT,
        year_universe_rows,
        [
            "year",
            "universe",
            "closures",
            "coverage_note",
        ],
    )
    write_csv(
        CLOSURES_WITH_HEADCOUNT_OUTPUT,
        closures_with_headcount,
        [
            "opeid6",
            "institution_name",
            "state",
            "closure_year",
            "sample_type",
            "broad_sector",
            "level",
            "ever_broad_sectors",
            "overall_level",
            "last_seen_year",
            "last_seen_acts",
            "last_seen_newids",
            "source_note",
            "closure_rule",
            "matched_headcount_year",
            "matched_student_headcount",
            "metrics_year_two_years_prior",
            "student_headcount_two_years_prior",
            "student_fte_two_years_prior",
            "staff_headcount_two_years_prior",
            "staff_fte_two_years_prior",
            "headcount_note",
            "metrics_note",
        ],
    )
    write_csv(
        METRICS_CLOSURE_LIST_OUTPUT,
        closures_with_headcount,
        [
            "opeid6",
            "institution_name",
            "state",
            "closure_year",
            "sample_type",
            "broad_sector",
            "level",
            "ever_broad_sectors",
            "overall_level",
            "metrics_year_two_years_prior",
            "student_headcount_two_years_prior",
            "student_fte_two_years_prior",
            "staff_headcount_two_years_prior",
            "staff_fte_two_years_prior",
            "matched_headcount_year",
            "matched_student_headcount",
            "last_seen_year",
            "last_seen_acts",
            "last_seen_newids",
            "source_note",
            "closure_rule",
            "headcount_note",
            "metrics_note",
        ],
    )
    write_csv(
        STUDENTS_BY_SECTOR_OUTPUT,
        students_by_sector_rows,
        [
            "group",
            "closures",
            "closures_with_student_fte",
            "student_headcount",
            "median_student_fte_two_years_prior",
            "median_student_headcount_two_years_prior",
            "median_staff_fte_two_years_prior",
            "median_staff_headcount_two_years_prior",
            "coverage_note",
        ],
    )
    write_csv(
        TABLE3_COMPARE_OUTPUT,
        table3_rows,
        [
            "sample_type",
            "kelchen_institutions",
            "our_institutions",
            "institution_difference",
            "kelchen_closures",
            "our_main_campus_closures",
            "closure_difference",
        ],
    )
    write_csv(
        VARIANT_COMPARE_OUTPUT,
        variant_rows,
        [
            "sample_variant",
            "closure_variant",
            "sample_size",
            "sample_gap_vs_kelchen",
            "main_campus_closures",
            "closure_gap_vs_kelchen",
            "absolute_total_gap",
        ],
    )

    summary = {
        "panel_year_start": START_YEAR,
        "panel_year_end": PANEL_END_YEAR,
        "kelchen_compare_end_year": KELCHEN_COMPARE_END_YEAR,
        "present_closure_end_year": PRESENT_END_YEAR,
        "unique_opeid6_with_valid_opeid": len({row["opeid6"] for row in panel_rows}),
        "preferred_sample_opeid6": len(preferred_sample_opeids(panel_rows_benchmark)),
        "all_valid_sample_opeid6": len(all_valid_sample_opeids(panel_rows_benchmark)),
        "known_type_sample_opeid6": len(known_type_sample_opeids(panel_rows_benchmark)),
        "all_valid_no_1996_only_blank_sample_opeid6": len(no_1996_only_blank_sample_opeids(panel_rows_benchmark)),
        "selected_sample_variant": selected_sample_variant,
        "kelchen_table3_total_institutions": KELCHEN_TABLE3["Total"]["institutions"],
        "sample_gap_vs_kelchen": len(sample_opeids) - KELCHEN_TABLE3["Total"]["institutions"],
        "main_campus_closures_1996_2023": len(closure_rows_2023),
        "main_campus_closures_1996_present": len(closure_rows),
        "closures_with_matched_headcount": len([row for row in closures_with_headcount if row["matched_student_headcount"] != ""]),
        "closures_with_student_fte_two_years_prior": len([row for row in closures_with_headcount if row["student_fte_two_years_prior"] != ""]),
        "matched_student_headcount_total": sum(int(row["matched_student_headcount"]) for row in closures_with_headcount if row["matched_student_headcount"] != ""),
        "kelchen_table3_total_closures": KELCHEN_TABLE3["Total"]["closures"],
        "closure_gap_vs_kelchen": len(closure_rows_2023) - KELCHEN_TABLE3["Total"]["closures"],
        "best_variant": variant_rows[0],
        "figure2_output_built": figure_built,
        "method_note": (
            "This script approximates Kelchen's OPEID-level panel using official IPEDS directory-style files and "
            "the federal schfile rectype-01 main-campus close-date signal. It is materially closer to the paper's "
            "PEPS-style closure logic than an IPEDS ACT-code proxy. It now uses first closure only, excludes obvious fake 00 branch rows, "
            "drops foreign locations, classifies OPEIDs as 4-year if any component is 4-year, and double-counts sector changers in the type tables."
        ),
        "coverage_note": (
            "Recovered 1996-1997 IPEDS files help define the panel, but they do not expose the same closure-status fields "
            "as later directory files. Annual closure counts should therefore be treated as strongest from 1998 onward. "
            "The 1996-2023 comparison can still differ from the paper because the current schfile can include later backfilled closures beyond the November 2023 PEPS snapshot used in the paper. "
            "The present-day extension uses the current schfile through 2026, while IPEDS panel exits stop after 2024 because newer directory files are not yet available locally."
        ),
    }
    with SUMMARY_OUTPUT.open("w", encoding="utf-8") as handle:
        json.dump(summary, handle, indent=2)

    print(json.dumps(summary, indent=2))


if __name__ == "__main__":
    main()
