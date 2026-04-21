run_test("Workbook helper grouping", function() {
  group_list <- list(
    all = data.frame(flag = c("Yes", "No"), value = c("10", "20"), metric = c(1, 2), stringsAsFactors = FALSE),
    public = data.frame(flag = "Yes", value = "5", metric = 4, stringsAsFactors = FALSE),
    private_nfp = data.frame(flag = character(), value = character(), metric = numeric(), stringsAsFactors = FALSE),
    private_fp = data.frame(flag = "No", value = "7", metric = 3, stringsAsFactors = FALSE),
    bacc_public = data.frame(flag = "Yes", value = "8", metric = 6, stringsAsFactors = FALSE),
    bacc_private_nfp = data.frame(flag = "No", value = "9", metric = 1, stringsAsFactors = FALSE),
    bacc_private_fp = data.frame(flag = "Yes", value = "11", metric = 5, stringsAsFactors = FALSE)
  )

  counts <- count_by_group_from(group_list, function(df) yes_flag(df$flag))
  pcts <- pct_by_group_from(group_list, function(df) yes_flag(df$flag))
  medians <- numeric_stat_by_group(group_list, "value")
  tops <- top_metric_by_group_from(group_list, "metric")
  summary_rows <- make_count_pct_rows("Example metric", counts, pcts)

  assert_equal(unname(counts[["all"]]), 1)
  assert_equal(unname(pcts[["public"]]), 100)
  assert_equal(unname(medians[["all"]]), 15)
  assert_equal(tops$public$metric[[1]], 4)
  assert_identical(nrow(summary_rows), 2L)
})

run_test("Workbook helper row appends", function() {
  row_a <- make_group_row("Metric A", "count", c(all = 1, public = 2))
  row_b <- make_group_row("Metric B", "percent", c(all = 50, public = 40))
  combined <- append_rows(row_a, row_b)

  assert_identical(nrow(combined), 2L)
  assert_identical(combined$metric[[1]], "Metric A")
  assert_identical(combined$statistic[[2]], "percent")
})

run_test("Workbook helper index and benchmark builders", function() {
  index_rows <- build_worksheet_index_rows(list(
    list(name = "Summary", description = "Summary tab"),
    list(name = "All_2024", description = "All rows")
  ))

  benchmarks <- build_benchmark_tab(list(
    all = data.frame(
      losses_last_3_of_5 = c("Yes", "No"),
      revenue_pct_change_5yr = c(-5, 2),
      net_tuition_per_fte_change_5yr = c(-1, 3),
      transfer_out_rate_bachelor = c(10, 20),
      transfer_out_rate_bachelor_change_5yr = c(1, 2),
      staff_total_headcount_pct_change_5yr = c(-4, 1),
      state_funding_pct_change_5yr = c(-3, 2),
      stringsAsFactors = FALSE
    )
  ))

  assert_identical(nrow(index_rows), 2L)
  assert_identical(index_rows$statistic[[1]], "Summary")
  assert_identical(nrow(benchmarks), 1L)
  assert_equal(benchmarks$institutions[[1]], 2)
  assert_equal(benchmarks$staffing_cut_share[[1]], 50)
})

run_test("Workbook helper distress and staffing summaries", function() {
  read_df <- data.frame(
    unitid = c("100", "100", "200", "200"),
    year = c(2014L, 2015L, 2014L, 2015L),
    category = rep("Degree-granting, primarily baccalaureate or above", 4),
    enrollment_decline_last_3_of_5 = c("Yes", "Yes", "No", "No"),
    revenue_10pct_drop_last_3_of_5 = c("Yes", "Yes", "No", "No"),
    losses_last_3_of_5 = c("Yes", "Yes", "No", "No"),
    ended_year_at_loss = c("Yes", "Yes", "No", "No"),
    staff_total_headcount_pct_change_5yr = c(-2, -3, 1, 2),
    net_tuition_per_fte_change_5yr = c(-1, -2, 3, 4),
    enrollment_pct_change_5yr = c(-12, -15, 2, 4),
    revenue_pct_change_5yr = c(-11, -13, 3, 5),
    enrollment_headcount_total = c(100, 95, 80, 82),
    staff_headcount_total = c(40, 38, 30, 31),
    staff_headcount_instructional = c(10, 9, 8, 8),
    stringsAsFactors = FALSE
  )

  distress_compare <- build_distress_compare(
    read_df,
    "Degree-granting, primarily baccalaureate or above",
    years = c(2015L, 2014L)
  )
  staff_cut_yoy <- build_staff_cut_yoy(read_df, start_year = 2014L, end_year = 2015L)

  assert_identical(nrow(distress_compare), 2L)
  assert_equal(distress_compare$distress_count[distress_compare$year == 2015], 1)
  assert_identical(nrow(staff_cut_yoy), 2L)
  assert_equal(staff_cut_yoy$institutions_cutting_staff[staff_cut_yoy$year == 2015], 1)
})

run_test("Workbook helper report answers builder", function() {
  distress_compare <- data.frame(
    year = c(2024L, 2019L, 2014L),
    distress_count = c(12, 9, 0),
    distress_pct = c(15, 11, NA),
    enrollment_drop_10pct_count = c(7, 5, 0),
    revenue_drop_10pct_count = c(6, 4, 0),
    distress_students = c(5000, 4200, 0),
    longrun_count = c(3, 2, 0),
    longrun_students = c(1200, 900, 0),
    comparison_note = c("2024 comparable", "2019 comparable", "2014 not comparable"),
    stringsAsFactors = FALSE
  )
  distress_intl10 <- data.frame(unitid = c(1, 2), stringsAsFactors = FALSE)
  flagship_cuts <- data.frame(
    total_disrupted_award_remaining = c(1500000, 500000),
    stringsAsFactors = FALSE
  )
  staff_cut_yoy <- data.frame(
    year = c(2023L, 2024L),
    institutions_cutting_staff = c(8, 11),
    stringsAsFactors = FALSE
  )

  report_answers <- build_report_answers(
    distress_compare = distress_compare,
    distress_intl10 = distress_intl10,
    flagship_cuts = flagship_cuts,
    staff_cut_yoy = staff_cut_yoy
  )

  assert_identical(nrow(report_answers), 16L)
  assert_identical(report_answers$value[[1]], "12")
  assert_identical(report_answers$value[[13]], "2")
  assert_identical(report_answers$value[[15]], "1")
  assert_identical(report_answers$value[[16]], "11")
})

run_test("Workbook report answers labels follow supplied years", function() {
  distress_compare <- data.frame(
    year = c(2025L, 2020L, 2015L),
    distress_count = c(13, 10, 0),
    distress_pct = c(16, 12, NA),
    enrollment_drop_10pct_count = c(8, 6, 0),
    revenue_drop_10pct_count = c(7, 5, 0),
    distress_students = c(5100, 4300, 0),
    longrun_count = c(4, 3, 0),
    longrun_students = c(1300, 950, 0),
    comparison_note = c("2025 comparable", "2020 comparable", "2015 not comparable"),
    stringsAsFactors = FALSE
  )
  staff_cut_yoy <- data.frame(
    year = c(2024L, 2025L),
    institutions_cutting_staff = c(8, 12),
    stringsAsFactors = FALSE
  )

  report_answers <- build_report_answers(
    distress_compare = distress_compare,
    distress_intl10 = data.frame(unitid = integer(), stringsAsFactors = FALSE),
    flagship_cuts = data.frame(total_disrupted_award_remaining = numeric(), stringsAsFactors = FALSE),
    staff_cut_yoy = staff_cut_yoy,
    latest_year = 2025L,
    comparison_year = 2020L,
    baseline_year = 2015L,
    prior_year = 2024L
  )

  assert_true(grepl("2025 distressed institutions", report_answers$question[[1]], fixed = TRUE))
  assert_true(grepl("2020 distressed institutions", report_answers$question[[8]], fixed = TRUE))
  assert_true(grepl("2015 comparison note", report_answers$question[[12]], fixed = TRUE))
  assert_true(grepl("2024 to 2025", report_answers$question[[16]], fixed = TRUE))
  assert_identical(report_answers$value[[16]], "12")
})

run_test("Workbook helper state breakdown builder", function() {
  all_sheet_bacc <- data.frame(
    control_label = c("Public", "Public", "Public", "Private not-for-profit"),
    state = c("CA", "CA", "TX", "CA"),
    state_funding_pct_change_5yr = c(-4, 2, -1, -3),
    state_funding_pct_core_revenue = c(0.20, 0.10, 0.30, 0.50),
    stringsAsFactors = FALSE
  )

  state_breakdown <- build_state_breakdown(all_sheet_bacc)

  assert_identical(nrow(state_breakdown), 2L)
  assert_identical(state_breakdown$state[[1]], "TX")
  assert_equal(state_breakdown$state_funding_down_5yr_percent[state_breakdown$state == "CA"], 50)
  assert_equal(state_breakdown$mean_state_funding_pct_core_revenue[state_breakdown$state == "TX"], 30)
})

run_test("Workbook helper worksheet registry builder", function() {
  base_sheets <- list(
    EnrollDecl3of5 = data.frame(a = 1),
    RevDecl3of5 = data.frame(a = 1),
    Red3of5 = data.frame(a = 1),
    EnrollRev3of5 = data.frame(a = 1),
    EnrollRed3of5 = data.frame(a = 1),
    All3Signals = data.frame(a = 1),
    IntlUp5yr = data.frame(a = 1),
    IntlUp10yr = data.frame(a = 1),
    Flagships = data.frame(a = 1),
    FlagshipFed = data.frame(a = 1),
    ResearchLeaders = data.frame(a = 1),
    Loss2024 = data.frame(a = 1),
    StateDown5yr = data.frame(a = 1),
    EndowDown5yr = data.frame(a = 1),
    DiscRateUp5yr = data.frame(a = 1),
    EnrollDown5yr = data.frame(a = 1),
    RevDown5yr = data.frame(a = 1),
    StaffDown5yr = data.frame(a = 1),
    InstrStaffDown5yr = data.frame(a = 1),
    StaffNetTuitionDown = data.frame(a = 1),
    StaffCutRisk = data.frame(a = 1),
    TransferOutUp5yr = data.frame(a = 1),
    TransferOutUp10yr = data.frame(a = 1),
    FedDepend = data.frame(a = 1),
    StateDepend = data.frame(a = 1),
    YearsAtLoss = data.frame(a = 1),
    TuitionDepend = data.frame(a = 1),
    NetTuitionDown = data.frame(a = 1),
    IntlShare = data.frame(a = 1),
    LowCushion = data.frame(a = 1),
    HighDebt = data.frame(a = 1),
    FedAndIntl = data.frame(a = 1),
    MultiSignal = data.frame(a = 1),
    PrivateCloseRisk = data.frame(a = 1),
    PublicCampusRisk = data.frame(a = 1),
    StudPerInstr50 = data.frame(a = 1)
  )
  finance_sheets <- list(PublicFinBad50 = data.frame(a = 1), PrivateFinBad50 = data.frame(a = 1))
  theme_sheets <- list(
    LossTuition = data.frame(a = 1),
    PrivNFPStress = data.frame(a = 1),
    MultiFront = data.frame(a = 1),
    DistressCore = data.frame(a = 1),
    DistressIntl10 = data.frame(a = 1)
  )
  graduate_sheets <- list(PublicFedTop = data.frame(a = 1), GradDependTop = data.frame(a = 1), PublicGradTop = data.frame(a = 1))

  worksheets <- build_article_workbook_registry(
    summary_rows = data.frame(metric = "Worksheet index", statistic = "Summary", stringsAsFactors = FALSE),
    report_answers = data.frame(question = "q", value = "v", note = "n", stringsAsFactors = FALSE),
    bacc_benchmarks = data.frame(group = "all", stringsAsFactors = FALSE),
    all_sheet_bacc = data.frame(unitid = 1, stringsAsFactors = FALSE),
    base_sheets = base_sheets,
    state_breakdown = data.frame(state = "CA", stringsAsFactors = FALSE),
    finance_sheets = finance_sheets,
    theme_sheets = theme_sheets,
    staff_cut_yoy = data.frame(year = 2024L, stringsAsFactors = FALSE),
    graduate_sheets = graduate_sheets,
    flagship_cuts = data.frame(unitid = 1, stringsAsFactors = FALSE),
    distress_compare = data.frame(year = 2024L, stringsAsFactors = FALSE),
    intl_offset_10yr = data.frame(unitid = 1, stringsAsFactors = FALSE),
    intl_offset_10yr_ranked = data.frame(unitid = 1, stringsAsFactors = FALSE),
    accredit_finance_xtab = data.frame(event_type = "x", stringsAsFactors = FALSE),
    accreditation_summary_bacc = data.frame(unitid = 1, stringsAsFactors = FALSE),
    cuts_finance_xtab = data.frame(event_type = "y", stringsAsFactors = FALSE),
    college_cuts_summary_bacc = data.frame(unitid = 1, stringsAsFactors = FALSE),
    hcm_summary = data.frame(x = 1),
    hcm_all = data.frame(x = 1),
    hcm_dec24_drop = data.frame(x = 1),
    hcm_mar25_drop = data.frame(x = 1),
    hcm_jun25_drop = data.frame(x = 1),
    hcm_dec24_stay = data.frame(x = 1),
    hcm_mar25_stay = data.frame(x = 1),
    running_closures = data.frame(x = 1),
    main_campus_closures = data.frame(x = 1),
    branch_campus_closures = data.frame(x = 1),
    mergers_consol = data.frame(x = 1),
    private_federal_main_closures = data.frame(x = 1),
    intl_vulnerable = data.frame(x = 1),
    intl_vulnerable_large = data.frame(x = 1)
  )

  assert_identical(names(worksheets)[1], "Summary")
  assert_identical(names(worksheets)[2], "ReportAnswers")
  assert_true("StateBySt" %in% names(worksheets))
  assert_true("IntlVulnLarge" %in% names(worksheets))
})

run_test("Workbook helper duplicate worksheet pruning", function() {
  worksheets <- list(
    Summary = data.frame(
      metric = c("Worksheet index", "Worksheet index", "Other"),
      statistic = c("TabA", "TabB", "Keep"),
      stringsAsFactors = FALSE
    ),
    TabA = data.frame(x = 1, stringsAsFactors = FALSE),
    TabB = data.frame(x = 1, stringsAsFactors = FALSE),
    TabC = data.frame(x = 2, stringsAsFactors = FALSE)
  )

  pruned <- prune_duplicate_worksheets(worksheets)

  assert_true("TabB" %in% names(pruned))
  assert_true(!("TabA" %in% names(pruned)))
  assert_identical(pruned$Summary$statistic[[1]], "TabB")
  assert_true(!any(pruned$Summary$statistic == "TabA"))
})
