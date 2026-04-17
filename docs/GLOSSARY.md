# Glossary

Plain-language explanations of the terms used throughout this project.

---

## unitid (Unit ID)

A unique number the U.S. Department of Education assigns to every college and university. This project uses it as the primary identifier — every table and JSON file links data to a specific school through `unitid`.

Example: `172264` is Georgetown University.

---

## IPEDS

**Integrated Postsecondary Education Data System.** A federal database run by the National Center for Education Statistics (NCES). It contains enrollment, graduation rates, finances, and staffing data for ~7,000 colleges going back decades.

This project downloads IPEDS data year by year, selects the variables it needs, and builds a cleaned "canonical" dataset from those selections.

---

## canonical dataset

The cleaned, decoded master dataset this project builds from IPEDS. It contains one row per college per year, with column names that make sense (e.g., `total_revenue` instead of `F2T01`) and values that are actual dollars or counts instead of raw federal codes.

File: `ipeds/derived/ipeds_financial_health_canonical_2014_2024.csv`

---

## join

Combining two datasets by a shared column. For example, the canonical IPEDS dataset (one row per college) gets "joined" to the college cuts data (one row per cut event) using `unitid` as the shared column. The result is a single table where each college row also shows its cut history.

If a college in the cuts data doesn't exist in IPEDS, it gets dropped from the joined result — that's called an "unmatched" record.

---

## manifest

A catalog listing every IPEDS file this project downloads and every variable it selects from each file. The manifest proves exactly which source files and columns were used, making the build reproducible.

Files:
- `ipeds/manifests/ipeds_financial_health_selected_file_catalog.csv` — which IPEDS files were used
- `ipeds/manifests/ipeds_financial_health_field_resolution_audit_2014_2024.csv` — how variable names were resolved across years

---

## raw vs. derived (IPEDS data)

**Raw IPEDS data** is the exact federal data as downloaded — with federal variable codes, encoded values, and missing-value flags.

**Derived/canonical data** is this project's cleaned version — human-readable column names, decoded values (e.g., "public" instead of a code), calculated ratios.

The raw data stays in `ipeds/raw/`. The derived data goes in `ipeds/derived/` and is what all downstream joins consume.

---

## smoke test

A quick check that the code runs without crashing, rather than testing every possible input. This project has a lightweight smoke test runner:

```bash
Rscript --vanilla ./tests/run_shared_helper_smoke_tests.R
```

It runs ~91 small tests in a few seconds and is the gate before any commit.

---

## fixture (in tests)

A small, simplified copy of a real data input used to test the pipeline. Fixtures let the tests run in isolation without needing the actual IPEDS downloads or external APIs.

For example, a fixture might have 3 fictional colleges instead of 7,000 — enough to verify that the join logic works correctly.

---

## accreditor / accreditation action

An **accreditor** is an independent body that evaluates whether colleges meet quality standards. Six major accreditors cover most U.S. colleges.

An **accreditation action** is an official finding — for example, a school being put on "warning" or "probation." This project tracks those actions because they are early signals of financial distress.

---

## College Scorecard

A U.S. Department of Education dataset of college outcomes — graduation rates, student debt, post-graduation earnings. This project joins Scorecard data into the canonical dataset to show outcomes alongside financial health.

---

## Grant Witness

An NCES database tracking federal grant awards to colleges by agency, award year, and recipient institution. This project uses it to identify colleges facing major cuts to federal research funding.

---

## USAspending.gov

A federal website publishing government contract and grant spending data. The `build_grant_witness_usaspending_sensitivity.R` script queries it to check whether a grant cut signal from Grant Witness is reliable (the grant was actually reduced) or might be a false positive (an institution was renamed, a grant was transferred, etc.).

---

## HCM / HCM2

**Heightened Cash Monitoring** — a status the Department of Education places schools on when it has concerns about their financial management. HCM2 is the more serious tier.

Schools on HCM2 appear on the public tracker and are flagged in the financial health data.

---

## closure scraper

This repo no longer runs a closure scraper. Instead, a separate workflow maintains closure data and publishes it to a Google Sheet. This repo imports that Google Sheet with `import_closure_sheet.py`.

See `docs/REFRESH_CYCLE.md` for the full data flow.
