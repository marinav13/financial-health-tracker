# scripts/shared/workbook_registry_helpers.R
#
# Workbook registry/index helpers extracted from workbook_helpers.R.
# Source this after workbook_table_helpers.R and before scripts that assemble worksheets.

build_worksheet_index_rows <- function(sheet_specs) {
  rows <- lapply(sheet_specs, function(spec) {
    make_row(
      "Worksheet index",
      spec$name,
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      spec$description
    )
  })
  do.call(append_rows, rows)
}

# Builds a benchmark summary tab for a named list of grouped data frames.

build_article_workbook_registry <- function(
  summary_rows,
  report_answers,
  bacc_benchmarks,
  all_sheet_bacc,
  base_sheets,
  state_breakdown,
  finance_sheets,
  theme_sheets,
  staff_cut_yoy,
  graduate_sheets,
  flagship_cuts,
  distress_compare,
  intl_offset_10yr,
  intl_offset_10yr_ranked,
  accredit_finance_xtab,
  accreditation_summary_bacc,
  cuts_finance_xtab,
  college_cuts_summary_bacc,
  hcm_summary,
  hcm_all,
  hcm_dec24_drop,
  hcm_mar25_drop,
  hcm_jun25_drop,
  hcm_dec24_stay,
  hcm_mar25_stay,
  running_closures,
  main_campus_closures,
  branch_campus_closures,
  mergers_consol,
  private_federal_main_closures,
  intl_vulnerable,
  intl_vulnerable_large
) {
  list(
    Summary = summary_rows,
    ReportAnswers = report_answers,
    BaccBenchmarks = bacc_benchmarks,
    All_2024 = all_sheet_bacc,
    EnrollDecl3of5 = base_sheets$EnrollDecl3of5,
    RevDecl3of5 = base_sheets$RevDecl3of5,
    Red3of5 = base_sheets$Red3of5,
    EnrollRev3of5 = base_sheets$EnrollRev3of5,
    EnrollRed3of5 = base_sheets$EnrollRed3of5,
    All3Signals = base_sheets$All3Signals,
    IntlUp5yr = base_sheets$IntlUp5yr,
    IntlUp10yr = base_sheets$IntlUp10yr,
    Flagships = base_sheets$Flagships,
    FlagshipFed = base_sheets$FlagshipFed,
    ResearchLeaders = base_sheets$ResearchLeaders,
    Loss2024 = base_sheets$Loss2024,
    StateDown5yr = base_sheets$StateDown5yr,
    EndowDown5yr = base_sheets$EndowDown5yr,
    DiscRateUp5yr = base_sheets$DiscRateUp5yr,
    EnrollDown5yr = base_sheets$EnrollDown5yr,
    RevDown5yr = base_sheets$RevDown5yr,
    StaffDown5yr = base_sheets$StaffDown5yr,
    InstrStaffDown5yr = base_sheets$InstrStaffDown5yr,
    StaffNetTuitionDown = base_sheets$StaffNetTuitionDown,
    StaffCutRisk = base_sheets$StaffCutRisk,
    TransferOutUp5yr = base_sheets$TransferOutUp5yr,
    TransferOutUp10yr = base_sheets$TransferOutUp10yr,
    FedDepend = base_sheets$FedDepend,
    StateDepend = base_sheets$StateDepend,
    StateBySt = state_breakdown,
    YearsAtLoss = base_sheets$YearsAtLoss,
    TuitionDepend = base_sheets$TuitionDepend,
    NetTuitionDown = base_sheets$NetTuitionDown,
    IntlShare = base_sheets$IntlShare,
    LowCushion = base_sheets$LowCushion,
    HighDebt = base_sheets$HighDebt,
    FedAndIntl = base_sheets$FedAndIntl,
    MultiSignal = base_sheets$MultiSignal,
    PrivateCloseRisk = base_sheets$PrivateCloseRisk,
    PublicCampusRisk = base_sheets$PublicCampusRisk,
    PublicFinBad50 = finance_sheets$PublicFinBad50,
    PrivateFinBad50 = finance_sheets$PrivateFinBad50,
    LossTuition = theme_sheets$LossTuition,
    PrivNFPStress = theme_sheets$PrivNFPStress,
    MultiFront = theme_sheets$MultiFront,
    DistressCore = theme_sheets$DistressCore,
    DistressIntl10 = theme_sheets$DistressIntl10,
    StaffCutsYoY = staff_cut_yoy,
    PublicFedTop = graduate_sheets$PublicFedTop,
    GradDependTop = graduate_sheets$GradDependTop,
    PublicGradTop = graduate_sheets$PublicGradTop,
    StudPerInstr50 = base_sheets$StudPerInstr50,
    FlagshipCuts = flagship_cuts,
    DistressCompare = distress_compare,
    IntlOffset10yr = intl_offset_10yr,
    BiggestDropsNoIntl = intl_offset_10yr_ranked,
    AccredFinanceXtab = accredit_finance_xtab,
    AccredMatches = accreditation_summary_bacc,
    CutsFinanceXtab = cuts_finance_xtab,
    CutsMatches = college_cuts_summary_bacc,
    HCM2Summary = hcm_summary,
    HCM2All = hcm_all,
    HCM2Dec24Drop = hcm_dec24_drop,
    HCM2Mar25Drop = hcm_mar25_drop,
    HCM2Jun25Drop = hcm_jun25_drop,
    HCM2Dec24Stay = hcm_dec24_stay,
    HCM2Mar25Stay = hcm_mar25_stay,
    RunningClosures = running_closures,
    MainCampusClosures = main_campus_closures,
    BranchCampusClosures = branch_campus_closures,
    MergersConsol = mergers_consol,
    PrivFedMainClose = private_federal_main_closures,
    IntlVulnerable = intl_vulnerable,
    IntlVulnLarge = intl_vulnerable_large
  )
}

# Removes duplicate non-summary tabs based on worksheet content and keeps the
# Summary worksheet index in sync with the surviving tab names.
prune_duplicate_worksheets <- function(worksheets) {
  worksheet_names <- names(worksheets)
  non_summary_names <- setdiff(worksheet_names, "Summary")
  seen_signatures <- character(0)
  duplicate_worksheet_names <- character(0)

  for (name in rev(non_summary_names)) {
    sig <- worksheet_signature(worksheets[[name]])
    if (sig %in% seen_signatures) {
      duplicate_worksheet_names <- c(duplicate_worksheet_names, name)
    } else {
      seen_signatures <- c(seen_signatures, sig)
    }
  }

  if (length(duplicate_worksheet_names) == 0) {
    return(worksheets)
  }

  worksheets <- worksheets[setdiff(names(worksheets), duplicate_worksheet_names)]
  worksheets$Summary <- worksheets$Summary[
    !(worksheets$Summary$metric == "Worksheet index" &
        worksheets$Summary$statistic %in% duplicate_worksheet_names),
    ,
    drop = FALSE
  ]
  worksheets
}

# Builds year-over-year staffing cut summary rows, including a visible baseline
# year and then comparisons against each prior year through `end_year`.
