IPEDS Financial Health Project
Full Walkthrough

This project lets someone:

- rebuild the full raw IPEDS dataset
- create the Looker-ready CSV
- create the fuller reporting CSV
- create the workbook XML
- publish the Looker-ready CSV into Google Sheets
- connect that Google Sheet to Looker Studio

Project folders

- `scripts` contains the R scripts
- `raw_build` contains the raw CSV and the cached IPEDS downloads
- `looker_studio` contains the main Looker-ready CSV
- `reporting` contains the fuller reporting CSV
- `workbooks` contains the multi-tab workbook XML
- `docs` contains handoff notes

Main scripts

- `scripts/build_ipeds_tracker_dataset.R`
- `scripts/build_looker_ready_metrics.R`
- `scripts/build_article_workbook.R`
- `scripts/publish_to_google_sheets.R`
- `scripts/annual_refresh_and_publish.R`

What the current main outputs are

- `raw_build/ipeds_financial_health_raw_2014_2024.csv`
- `raw_build/ipeds_financial_health_selected_file_catalog.csv`
- `looker_studio/ipeds_financial_health_looker_ready_2014_2024.csv`
- `reporting/ipeds_financial_health_reporting_2014_2024.csv`
- `workbooks/ipeds_financial_health_article_workbook_r.xml`

What you need

- R
- RStudio
- internet access when you rebuild IPEDS files
- a Google account if you want Google Sheets publishing

One-time R setup

Open RStudio and set the working directory to the project folder.

Run:

```r
user_lib <- "C:/Users/YourName/Documents/R/win-library/4.4"
dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(user_lib, .libPaths()))

install.packages(c(
  "dplyr",
  "googlesheets4",
  "openxlsx",
  "purrr",
  "readr",
  "readxl",
  "stringr",
  "tidyr",
  "xml2"
))
```

Replace `YourName` with the Windows username.

How to rebuild everything from scratch

Step 1. Build the raw IPEDS dataset

```r
source("scripts/build_ipeds_tracker_dataset.R")
main(c("--start-year", "2014", "--end-year", "2024"))
```

This downloads the IPEDS source files into `raw_build/downloads` and creates:

- `raw_build/ipeds_financial_health_raw_2014_2024.csv`
- `raw_build/ipeds_financial_health_selected_file_catalog.csv`

Step 2. Build the Looker-ready and reporting datasets

```r
source("scripts/build_looker_ready_metrics.R")
main()
```

This creates:

- `looker_studio/ipeds_financial_health_looker_ready_2014_2024.csv`
- `reporting/ipeds_financial_health_reporting_2014_2024.csv`

Step 3. Build the workbook

```r
source("scripts/build_article_workbook.R")
main(c(
  "--input", "./reporting/ipeds_financial_health_reporting_2014_2024.csv",
  "--output", "./workbooks/ipeds_financial_health_article_workbook_r.xml"
))
```

This creates:

- `workbooks/ipeds_financial_health_article_workbook_r.xml`

Quick sanity checks after rebuild

Open the reporting CSV and confirm:

- `state` is a full state name
- `urbanization` is a label, not a numeric code
- `category` is a label, not a numeric code

Spot-check one school:

```r
library(readr)
df <- read_csv("reporting/ipeds_financial_health_reporting_2014_2024.csv", show_col_types = FALSE)
subset(df, unitid == 100663 & year %in% c(2014, 2019, 2020, 2024),
       select = c(unitid, institution_name, year, enrollment_headcount_total, staff_headcount_total, fte_12_months))
```

How the Google Sheets publishing works

The recommended setup is:

- build data in R
- publish only the Looker-ready CSV to Google Sheets
- connect Looker Studio to that sheet

Do not use the raw CSV as the main Google Sheet source unless you really need it.

Current size notes

- the Looker-ready CSV is about 3.0 million cells, which fits under the Google Sheets 10 million cell limit
- the raw CSV is about 7.0 million cells, which also fits today, but is much heavier and slower

Recommendation:

- use `looker_studio/ipeds_financial_health_looker_ready_2014_2024.csv` for Google Sheets and Looker Studio
- keep the raw and reporting CSVs on disk, not as the live Looker source

How to create and fill a Google Sheet

If you already have a Google Sheet:

```r
source("scripts/publish_to_google_sheets.R")
main(c(
  "--input", "./looker_studio/ipeds_financial_health_looker_ready_2014_2024.csv",
  "--sheet", "YOUR_GOOGLE_SHEET_URL_OR_ID",
  "--tab", "looker_ready",
  "--email", "your.email@example.com"
))
```

If you want the script to create a new Google Sheet:

```r
source("scripts/publish_to_google_sheets.R")
main(c(
  "--input", "./looker_studio/ipeds_financial_health_looker_ready_2014_2024.csv",
  "--create", "IPEDS Financial Health Tracker",
  "--tab", "looker_ready",
  "--email", "your.email@example.com"
))
```

What happens next:

- the tab named `looker_ready` is created or replaced
- that sheet becomes the source for Looker Studio

How to connect the Google Sheet to Looker Studio

1. Open Looker Studio.
2. Create a new data source.
3. Choose the Google Sheets connector.
4. Select the spreadsheet and the `looker_ready` tab.
5. Use the first row as headers.
6. Finish the connection.

Important field-type cleanup in Looker Studio

You will usually need to change some fields from `Text` to `Number`.

Important numeric fields to fix first:

- `year`
- `enrollment_headcount_total`
- `enrollment_headcount_undergrad`
- `enrollment_headcount_graduate`
- `staff_headcount_total`
- `staff_headcount_instructional`
- `revenue_total`
- `expenses_total`
- `loss_amount`
- `net_tuition_per_fte`
- `net_tuition_per_fte_change_5yr`
- `pct_international_all`
- `pct_international_undergraduate`
- `pct_international_graduate`
- `tuition_dependence_pct`
- `federal_grants_contracts_pell_adjusted_pct_core_revenue`
- `state_funding_pct_core_revenue`
- `endowment_value`
- `discount_pct_change_5yr`
- `transfer_out_rate_bachelor`
- `transfer_out_rate_bachelor_change_5yr`

Recommended aggregation defaults

- use `Sum` for counts and dollars
- use `Average` for percent/rate fields in single-school views
- use `None` for labels and yes/no flags

Examples:

- `enrollment_headcount_total` -> Number, Sum
- `revenue_total` -> Number, Sum
- `pct_international_all` -> Number, Average
- `year` -> Number, None
- `unitid` -> Number, None

Year handling in Looker Studio

`year` is stored as a number.

For line charts:

- use a regular line chart if possible
- use `year` as the dimension

If you need a true date field for a time series chart, create:

```text
PARSE_DATE("%Y%m%d", CONCAT(CAST(year AS TEXT), "0101"))
```

and name it `year_date`.

Filter your sentence cards and scorecards to:

- `year = 2024`

That makes them show the current year row for the selected institution.

Suggested Looker Studio layout

Top text:

`As colleges and universities face federal funding cuts and immigration policy changes, which ones are most vulnerable to financial pressures and dips in international enrollment? Look up your institution for a snapshot of key financial trends, from enrollment to revenue and more.`

Institution selector:

- dimension: `institution_unique_name`

Institution snapshot:

- `institution_name`
- `state`
- `city`
- `urbanization`
- `control_label`
- `category`

Section 1. Enrollment

- line chart dimension: `year`
- line chart metric: `enrollment_headcount_total`
- scorecard: `enrollment_decline_last_3_of_5`
- scorecard: `enrollment_pct_change_5yr`

Section 2. International and student exposure

- sentence: international share
- sentence: international 5-year change
- sentence: borrowing

Section 3. Staffing

- line chart dimension: `year`
- line chart metrics: `staff_headcount_total`, `staff_headcount_instructional`
- scorecard: `staff_total_headcount_pct_change_5yr`

Section 4. Revenue

- line chart dimension: `year`
- line chart metric: `revenue_total`
- scorecard: `revenue_10pct_drop_last_3_of_5`
- scorecard: `revenue_pct_change_5yr`

Section 5. Losses

- line chart dimension: `year`
- line chart metrics: `revenue_total`, `expenses_total`
- scorecard: `ended_2024_at_loss`
- scorecard: `losses_last_3_of_5`
- scorecard: `loss_years_last_10`

Section 6. Net tuition

- line chart dimension: `year`
- line chart metric: `net_tuition_per_fte`
- scorecard: `net_tuition_per_fte_change_5yr`
- sentence: tuition dependence

Section 7. Federal and state exposure

- federal scorecard: `federal_grants_contracts_pell_adjusted_pct_core_revenue`
- state scorecard: `state_funding_pct_core_revenue`
- federal line chart: `federal_grants_contracts_pell_adjusted`
- state line chart: `state_funding`

Section 8. Endowment

- line chart dimension: `year`
- line chart metric: `endowment_value`
- sentence: endowment 5-year change

Recommended custom number formats in Looker Studio

For scorecards where you want explicit positive and negative signs:

- no decimal places: `+0'%';-0'%'`
- one decimal place: `+0.0'%';-0.0'%'`

Dynamic sentence formulas for Looker Studio

International share:

```text
CONCAT(
  CAST(ROUND(100 * MAX(CAST(pct_international_all AS NUMBER)), 1) AS TEXT),
  "% of students are international. That includes ",
  CAST(ROUND(100 * MAX(CAST(pct_international_undergraduate AS NUMBER)), 1) AS TEXT),
  "% of undergraduates and ",
  CAST(ROUND(100 * MAX(CAST(pct_international_graduate AS NUMBER)), 1) AS TEXT),
  "% of graduate students."
)
```

International change over 5 years:

```text
CASE
  WHEN MAX(CAST(international_enrollment_pct_change_5yr AS NUMBER)) IS NULL THEN NULL
  WHEN MAX(CAST(international_enrollment_pct_change_5yr AS NUMBER)) > 0 THEN
    CONCAT(
      "The number of international students has increased ",
      CAST(ROUND(MAX(CAST(international_enrollment_pct_change_5yr AS NUMBER)), 1) AS TEXT),
      "% over the past five years."
    )
  WHEN MAX(CAST(international_enrollment_pct_change_5yr AS NUMBER)) < 0 THEN
    CONCAT(
      "The number of international students has decreased ",
      CAST(ROUND(ABS(MAX(CAST(international_enrollment_pct_change_5yr AS NUMBER))), 1) AS TEXT),
      "% over the past five years."
    )
  ELSE
    "The number of international students has not changed over the past five years."
END
```

Enrollment change over 5 years:

```text
CASE
  WHEN MAX(CAST(enrollment_pct_change_5yr AS NUMBER)) IS NULL THEN NULL
  WHEN MAX(CAST(enrollment_pct_change_5yr AS NUMBER)) > 0 THEN
    CONCAT(
      "Enrollment has increased ",
      CAST(ROUND(MAX(CAST(enrollment_pct_change_5yr AS NUMBER)), 1) AS TEXT),
      "% over the past five years."
    )
  WHEN MAX(CAST(enrollment_pct_change_5yr AS NUMBER)) < 0 THEN
    CONCAT(
      "Enrollment has decreased ",
      CAST(ROUND(ABS(MAX(CAST(enrollment_pct_change_5yr AS NUMBER))), 1) AS TEXT),
      "% over the past five years."
    )
  ELSE
    "Enrollment has not changed over the past five years."
END
```

Staffing change over 5 years:

```text
CASE
  WHEN MAX(CAST(staff_total_headcount_pct_change_5yr AS NUMBER)) IS NULL THEN NULL
  WHEN MAX(CAST(staff_total_headcount_pct_change_5yr AS NUMBER)) > 0 THEN
    CONCAT(
      "Total staff headcount has increased ",
      CAST(ROUND(MAX(CAST(staff_total_headcount_pct_change_5yr AS NUMBER)), 1) AS TEXT),
      "% over the past five years."
    )
  WHEN MAX(CAST(staff_total_headcount_pct_change_5yr AS NUMBER)) < 0 THEN
    CONCAT(
      "Total staff headcount has decreased ",
      CAST(ROUND(ABS(MAX(CAST(staff_total_headcount_pct_change_5yr AS NUMBER))), 1) AS TEXT),
      "% over the past five years."
    )
  ELSE
    "Total staff headcount has not changed over the past five years."
END
```

Revenue change over 5 years:

```text
CASE
  WHEN MAX(CAST(revenue_pct_change_5yr AS NUMBER)) IS NULL THEN NULL
  WHEN MAX(CAST(revenue_pct_change_5yr AS NUMBER)) > 0 THEN
    CONCAT(
      "Revenue has increased ",
      CAST(ROUND(MAX(CAST(revenue_pct_change_5yr AS NUMBER)), 1) AS TEXT),
      "% over the past five years."
    )
  WHEN MAX(CAST(revenue_pct_change_5yr AS NUMBER)) < 0 THEN
    CONCAT(
      "Revenue has decreased ",
      CAST(ROUND(ABS(MAX(CAST(revenue_pct_change_5yr AS NUMBER))), 1) AS TEXT),
      "% over the past five years."
    )
  ELSE
    "Revenue has not changed over the past five years."
END
```

Net tuition per student over 5 years:

```text
CASE
  WHEN MAX(CAST(net_tuition_per_fte_change_5yr AS NUMBER)) IS NULL THEN NULL
  WHEN MAX(CAST(net_tuition_per_fte_change_5yr AS NUMBER)) > 0 THEN
    CONCAT(
      "Net tuition revenue per student has increased ",
      CAST(ROUND(MAX(CAST(net_tuition_per_fte_change_5yr AS NUMBER)), 1) AS TEXT),
      "% over the past five years."
    )
  WHEN MAX(CAST(net_tuition_per_fte_change_5yr AS NUMBER)) < 0 THEN
    CONCAT(
      "Net tuition revenue per student has decreased ",
      CAST(ROUND(ABS(MAX(CAST(net_tuition_per_fte_change_5yr AS NUMBER))), 1) AS TEXT),
      "% over the past five years."
    )
  ELSE
    "Net tuition revenue per student has not changed over the past five years."
END
```

Student borrowing:

```text
CONCAT(
  CAST(ROUND(MAX(CAST(federal_loan_pct_most_recent AS NUMBER)), 1) AS TEXT),
  "% of undergraduates at this institution took out federal loans in the most recent year available. That was ",
  CAST(ROUND(MAX(CAST(federal_loan_count_most_recent AS NUMBER)), 0) AS TEXT),
  " students, and the average loan amount was $",
  CAST(ROUND(MAX(CAST(federal_loan_avg_most_recent AS NUMBER)), 0) AS TEXT),
  "."
)
```

Tuition dependence:

```text
CONCAT(
  CAST(ROUND(MAX(CAST(tuition_dependence_pct AS NUMBER)), 1) AS TEXT),
  "% of this institution's revenue comes from net tuition."
)
```

Endowment change:

```text
CASE
  WHEN MAX(CAST(endowment_pct_change_5yr AS NUMBER)) IS NULL THEN NULL
  WHEN MAX(CAST(endowment_pct_change_5yr AS NUMBER)) > 0 THEN
    CONCAT(
      "The institution's endowment has increased ",
      CAST(ROUND(MAX(CAST(endowment_pct_change_5yr AS NUMBER)), 1) AS TEXT),
      "% over the past five years."
    )
  WHEN MAX(CAST(endowment_pct_change_5yr AS NUMBER)) < 0 THEN
    CONCAT(
      "The institution's endowment has decreased ",
      CAST(ROUND(ABS(MAX(CAST(endowment_pct_change_5yr AS NUMBER))), 1) AS TEXT),
      "% over the past five years."
    )
  ELSE
    "The institution's endowment has not changed over the past five years."
END
```

Discount rate change:

```text
CASE
  WHEN MAX(CAST(discount_pct_change_5yr AS NUMBER)) IS NULL THEN NULL
  WHEN MAX(CAST(discount_pct_change_5yr AS NUMBER)) > 0 THEN
    CONCAT(
      "The discount rate has increased ",
      CAST(ROUND(MAX(CAST(discount_pct_change_5yr AS NUMBER)), 1) AS TEXT),
      "% over the past five years."
    )
  WHEN MAX(CAST(discount_pct_change_5yr AS NUMBER)) < 0 THEN
    CONCAT(
      "The discount rate has decreased ",
      CAST(ROUND(ABS(MAX(CAST(discount_pct_change_5yr AS NUMBER))), 1) AS TEXT),
      "% over the past five years."
    )
  ELSE
    "The discount rate has not changed over the past five years."
END
```

Federal grants and contracts dependence:

```text
CONCAT(
  CAST(ROUND(100 * MAX(CAST(federal_grants_contracts_pell_adjusted_pct_core_revenue AS NUMBER)), 1) AS TEXT),
  "% of core revenue came from federal grants and contracts, excluding Pell grants."
)
```

State funding dependence:

```text
CONCAT(
  CAST(ROUND(100 * MAX(CAST(state_funding_pct_core_revenue AS NUMBER)), 1) AS TEXT),
  "% of core revenue came from state appropriations."
)
```

Annual IPEDS release-cycle automation

This project is better thought of as a three-times-a-year refresh, not just once a year.

Current 2024-25 release schedule:

- Fall surveys release: September 23, 2025
- Winter surveys release: December 9, 2025
- Spring surveys release: January 6, 2026

What each release affects most:

- Fall surveys: IC, Completions, 12-month Enrollment
- Winter surveys: Admissions, Graduation Rates, Student Financial Aid, Outcome Measures, Cost
- Spring surveys: Fall Enrollment, Finance, Human Resources, Academic Libraries

Recommended refresh schedule:

- run on September 24 after the fall release
- run on December 10 after the winter release
- run on January 7 after the spring release

How to automate the refresh

The easiest approach is Windows Task Scheduler.

Create a scheduled task that runs:

```powershell
"C:\Program Files\IBM\SPSS Statistics\R\bin\x64\Rscript.exe" --vanilla "C:\Users\mv3031\Desktop\Financial_Health_Project\scripts\annual_refresh_and_publish.R" --start-year 2014 --end-year 2024 --sheet YOUR_GOOGLE_SHEET_URL_OR_ID --tab looker_ready --email your.email@example.com
```

Use the same Google Sheet every time.

What happens when the scheduled run succeeds:

- IPEDS files are refreshed in the project cache
- the Looker-ready CSV is rebuilt
- the Google Sheet tab is replaced
- Looker Studio continues reading the same sheet

If the Google Sheet or Looker report is lost later

Someone else can still:

- rerun the three build scripts
- create a new Google Sheet with `publish_to_google_sheets.R`
- connect that new sheet to a new Looker Studio report

Known expected blanks

Some blanks are normal and not a bug.

- `transfer_out_rate_bachelor` is cohort-based, so many institutions will be blank because they do not have a usable bachelor transfer-out cohort in that year
- `state_funding_*` is mostly meaningful for publics
- `federal_grants_contracts_pell_adjusted_*` is intentionally blank for for-profits
- `endowment_*`, `liquidity`, and `leverage` are not universal across sectors

Core fields that should generally be populated across the decade now:

- `enrollment_headcount_total`
- `staff_headcount_total`
- `fte_12_months`
- `revenue_total`
- `net_tuition_per_fte`

If those look blank across whole years, something is wrong and the pipeline should be rerun.

Looker Studio fields to change to Number

In the Google Sheets connector or Looker Studio data source, change these fields to `Number`.

Alphabetical list:

- `discount_pct_change_5yr`
- `discount_rate`
- `endowment_pct_change_5yr`
- `endowment_value`
- `enrollment_headcount_graduate`
- `enrollment_headcount_total`
- `enrollment_headcount_undergrad`
- `enrollment_nonresident_graduate`
- `enrollment_nonresident_total`
- `enrollment_nonresident_undergrad`
- `enrollment_pct_change_5yr`
- `expenses_total`
- `federal_grants_contracts_pell_adjusted`
- `federal_grants_contracts_pell_adjusted_pct_change_5yr`
- `federal_grants_contracts_pell_adjusted_pct_core_revenue`
- `federal_loan_avg_most_recent`
- `federal_loan_count_most_recent`
- `federal_loan_pct_most_recent`
- `fte_12_months`
- `fte_graduate`
- `fte_undergrad`
- `international_enrollment_pct_change_10yr`
- `international_enrollment_pct_change_5yr`
- `international_student_count_change_5yr`
- `leverage`
- `leverage_percentile_private_nfp`
- `liquidity`
- `liquidity_percentile_private_nfp`
- `loan_avg_undergrad_federal_latest`
- `loan_count_undergrad_federal_latest`
- `loan_pct_undergrad_federal_latest`
- `loss_amount`
- `loss_amount_2024`
- `loss_years_last_10`
- `loss_years_last_5`
- `net_tuition_per_fte`
- `net_tuition_per_fte_change_5yr`
- `net_tuition_total`
- `pct_international_all`
- `pct_international_graduate`
- `pct_international_undergraduate`
- `revenue_pct_change_5yr`
- `revenue_total`
- `sector_enrollment_pct_change_5yr_national`
- `sector_enrollment_total_national`
- `share_grad_students`
- `staff_fte_instructional`
- `staff_fte_total`
- `staff_headcount_instructional`
- `staff_headcount_total`
- `staff_instructional_fte_pct_change_5yr`
- `staff_instructional_headcount_pct_change_5yr`
- `staff_total_headcount_pct_change_5yr`
- `staff_total_pct_change_5yr`
- `state_funding`
- `state_funding_pct_change_5yr`
- `state_funding_pct_core_revenue`
- `transfer_out_rate_bachelor`
- `transfer_out_rate_bachelor_change_10yr`
- `transfer_out_rate_bachelor_change_5yr`
- `tuition_dependence_pct`
- `tuition_dependence_ratio`
- `year`

Fields that should stay as text

- `institution_name`
- `institution_unique_name`
- `state`
- `city`
- `sector`
- `level`
- `urbanization`
- `category`
- `control_label`
- all yes/no flags such as `enrollment_decline_last_3_of_5` and `ended_2024_at_loss`

Sentence fields in Looker Studio

- create sentence fields as `dimensions`, not metrics
- use row-level formulas, not `MAX()`
- filter sentence charts to `year = 2024`
