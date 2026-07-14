if (!exists("run_test", mode = "function")) {
  source(file.path(getwd(), "tests", "test_support.R"))
}

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
    distress_count = c(2, 1, 0),
    distress_pct = c(66.7, 33.3, NA),
    institutions_total = c(3, 3, 0),
    distress_students = c(1700, 1500, 0),
    longrun_count = c(1, 1, 0),
    longrun_students = c(880, 1000, 0),
    comparison_note = c("2024 comparable", "2019 comparable", "2014 not comparable"),
    stringsAsFactors = FALSE
  )
  years <- 2014:2024
  category_label <- "Degree-granting, primarily baccalaureate or above"
  make_inst_rows <- function(unitid, control_label, enroll_values, revenue_values, latest_students,
                             summary_2019, summary_2024) {
    rows <- lapply(seq_along(years), function(idx) {
      year_value <- years[[idx]]
      summary_values <- if (year_value == 2019L) summary_2019 else if (year_value == 2024L) summary_2024 else NULL
      data.frame(
        unitid = unitid,
        year = year_value,
        category = category_label,
        control_label = control_label,
        enrollment_headcount_total = if (year_value == 2024L) latest_students else enroll_values[[idx]],
        revenue_total_adjusted = revenue_values[[idx]],
        enrollment_decline_last_3_of_5 = if (is.null(summary_values)) "No" else summary_values$enrollment_decline_last_3_of_5,
        revenue_10pct_drop_last_3_of_5 = if (is.null(summary_values)) "No" else summary_values$revenue_10pct_drop_last_3_of_5,
        losses_last_3_of_5 = if (is.null(summary_values)) "No" else summary_values$losses_last_3_of_5,
        ended_year_at_loss = if (is.null(summary_values)) "No" else summary_values$ended_year_at_loss,
        enrollment_pct_change_5yr = if (is.null(summary_values)) NA_real_ else summary_values$enrollment_pct_change_5yr,
        revenue_pct_change_5yr = if (is.null(summary_values)) NA_real_ else summary_values$revenue_pct_change_5yr,
        staff_total_headcount_pct_change_5yr = if (is.null(summary_values)) NA_real_ else summary_values$staff_total_headcount_pct_change_5yr,
        net_tuition_per_fte_change_5yr = if (is.null(summary_values)) NA_real_ else summary_values$net_tuition_per_fte_change_5yr,
        discount_pct_change_5yr = if (is.null(summary_values)) NA_real_ else summary_values$discount_pct_change_5yr,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  }
  public_rows <- make_inst_rows(
    unitid = "100",
    control_label = "Public",
    enroll_values = round(1000 * (0.88 ^ (0:10))),
    revenue_values = round(200 * (0.88 ^ (0:10))),
    latest_students = 278,
    summary_2019 = list(enrollment_decline_last_3_of_5 = "Yes", revenue_10pct_drop_last_3_of_5 = "Yes", losses_last_3_of_5 = "Yes", ended_year_at_loss = "Yes", enrollment_pct_change_5yr = -47, revenue_pct_change_5yr = -47, staff_total_headcount_pct_change_5yr = -12, net_tuition_per_fte_change_5yr = -15, discount_pct_change_5yr = 3),
    summary_2024 = list(enrollment_decline_last_3_of_5 = "Yes", revenue_10pct_drop_last_3_of_5 = "Yes", losses_last_3_of_5 = "Yes", ended_year_at_loss = "Yes", enrollment_pct_change_5yr = -47, revenue_pct_change_5yr = -47, staff_total_headcount_pct_change_5yr = -12, net_tuition_per_fte_change_5yr = -15, discount_pct_change_5yr = 3)
  )
  private_nfp_rows <- make_inst_rows(
    unitid = "200",
    control_label = "Private not-for-profit",
    enroll_values = round(800 * (1.01 ^ (0:10))),
    revenue_values = round(150 * (0.975 ^ (0:10))),
    latest_students = 884,
    summary_2019 = list(enrollment_decline_last_3_of_5 = "No", revenue_10pct_drop_last_3_of_5 = "No", losses_last_3_of_5 = "Yes", ended_year_at_loss = "Yes", enrollment_pct_change_5yr = 5, revenue_pct_change_5yr = -12, staff_total_headcount_pct_change_5yr = -4, net_tuition_per_fte_change_5yr = -12, discount_pct_change_5yr = 12),
    summary_2024 = list(enrollment_decline_last_3_of_5 = "No", revenue_10pct_drop_last_3_of_5 = "No", losses_last_3_of_5 = "Yes", ended_year_at_loss = "Yes", enrollment_pct_change_5yr = 5, revenue_pct_change_5yr = -12, staff_total_headcount_pct_change_5yr = -4, net_tuition_per_fte_change_5yr = -12, discount_pct_change_5yr = 12)
  )
  private_fp_rows <- make_inst_rows(
    unitid = "300",
    control_label = "Private for-profit",
    enroll_values = round(600 * (1.03 ^ (0:10))),
    revenue_values = round(100 * (1.02 ^ (0:10))),
    latest_students = 806,
    summary_2019 = list(enrollment_decline_last_3_of_5 = "No", revenue_10pct_drop_last_3_of_5 = "No", losses_last_3_of_5 = "No", ended_year_at_loss = "No", enrollment_pct_change_5yr = 16, revenue_pct_change_5yr = 10, staff_total_headcount_pct_change_5yr = 6, net_tuition_per_fte_change_5yr = 5, discount_pct_change_5yr = -2),
    summary_2024 = list(enrollment_decline_last_3_of_5 = "No", revenue_10pct_drop_last_3_of_5 = "No", losses_last_3_of_5 = "No", ended_year_at_loss = "No", enrollment_pct_change_5yr = 16, revenue_pct_change_5yr = 10, staff_total_headcount_pct_change_5yr = 6, net_tuition_per_fte_change_5yr = 5, discount_pct_change_5yr = -2)
  )
  read_df <- rbind(public_rows, private_nfp_rows, private_fp_rows)

  report_answers <- build_report_answers(
    read_df = read_df,
    distress_compare = distress_compare,
    bacc_category_label = category_label,
    latest_year = 2024L,
    comparison_year = 2019L,
    baseline_year = 2014L
  )

  assert_identical(nrow(report_answers), 226L)
  assert_true(all(c("group", "scope", "count_yes", "students_at_yes_institutions", "pct_of_yes_institutions") %in% names(report_answers)))
  assert_identical(report_answers$question[[1]], "2014 comparison note")
  assert_identical(report_answers$question[[2]], "How the workbook defines rising discount rates and falling net tuition per FTE")
  assert_identical(report_answers$note[[1]], "2014 not comparable")
  assert_identical(report_answers$calculation[[2]], "discount_pct_change_5yr > 0 AND net_tuition_per_fte_change_5yr < 0")

  public_rev_decline <- report_answers[
    report_answers$question == "2024 Public institutions with revenue declines in at least 3 of the last 5 year-to-year comparisons",
    ,
    drop = FALSE
  ]
  assert_identical(nrow(public_rev_decline), 1L)
  assert_equal(public_rev_decline$count_yes[[1]], 1)
  assert_equal(public_rev_decline$pct_of_scope_institutions[[1]], 100)

  mixed_repeated <- report_answers[
    report_answers$question == "2024 institutions with enrollment declines in at least 3 of the last 5 year-to-year comparisons, a revenue decline of at least 10% in at least 3 of the last 5 year-to-year comparisons, and losses in 3 of the last 5 years",
    ,
    drop = FALSE
  ]
  assert_identical(mixed_repeated$group[[1]], "2024 Repeated-decline flags")
  assert_equal(mixed_repeated$count_yes[[1]], 1)

  discount_stress <- report_answers[
    report_answers$question == "2024 Private not-for-profit institutions with rising discount rates and falling net tuition per FTE",
    ,
    drop = FALSE
  ]
  assert_equal(discount_stress$count_yes[[1]], 1)
  assert_equal(discount_stress$students_at_yes_institutions[[1]], 884)
})

run_test("Workbook helper distress and research answer builders", function() {
  distress_compare <- data.frame(
    year = c(2024L, 2019L, 2014L),
    distress_count = c(2, 1, 0),
    distress_pct = c(66.7, 33.3, NA),
    institutions_total = c(3, 3, 0),
    distress_students = c(1700, 1500, 0),
    longrun_count = c(1, 1, 0),
    comparison_note = c("2024 comparable", "2019 comparable", "2014 not comparable"),
    stringsAsFactors = FALSE
  )
  read_df <- data.frame(
    unitid = c("100", "200", "300", "100", "200", "300"),
    year = c(2019L, 2019L, 2019L, 2024L, 2024L, 2024L),
    category = rep("Degree-granting, primarily baccalaureate or above", 6),
    control_label = c("Public", "Private not-for-profit", "Private for-profit", "Public", "Private not-for-profit", "Private for-profit"),
    enrollment_decline_last_3_of_5 = c("Yes", "No", "No", "Yes", "No", "No"),
    revenue_10pct_drop_last_3_of_5 = c("Yes", "No", "No", "Yes", "No", "No"),
    losses_last_3_of_5 = c("Yes", "Yes", "No", "Yes", "Yes", "No"),
    ended_year_at_loss = c("Yes", "Yes", "No", "Yes", "Yes", "No"),
    staff_total_headcount_pct_change_5yr = c(-12, -4, 6, -12, -4, 6),
    net_tuition_per_fte_change_5yr = c(-15, -12, 5, -15, -12, 5),
    enrollment_headcount_total = c(527, 840, 694, 278, 884, 806),
    stringsAsFactors = FALSE
  )
  distress_answers <- build_distress_answers(
    read_df = read_df,
    distress_compare = distress_compare,
    distress_intl10 = data.frame(unitid = c(1, 2), stringsAsFactors = FALSE),
    accredit_finance_xtab = data.frame(
      event_type = c("Active warning/notice", "Active warning/notice"),
      control_scope = c("All", "All"),
      cohort = c("With event", "Without event"),
      distress_share_pct = c(40, 20),
      staff_total_decline_5yr_pct = c(60, 30),
      stringsAsFactors = FALSE
    )
  )
  research_cuts_answers <- build_research_cuts_answers(
    flagship_cuts = data.frame(total_disrupted_award_remaining = c(1500000, 500000), stringsAsFactors = FALSE)
  )

  assert_true("How the workbook defines distressed institutions" %in% distress_answers$question)
  assert_true("How the workbook defines long-running challenge institutions" %in% distress_answers$question)
  assert_true("Public flagships with still-disrupted federal research cuts" %in% research_cuts_answers$question)
  assert_identical(
    distress_answers$value[distress_answers$question == "Active accreditation warning/notice overlap with workbook distress"][[1]],
    "40.0% with warning/notice vs 20.0% without; 60.0% with warning/notice also cut staff"
  )
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
  assert_equal(state_breakdown$state_support_down_5yr_percent[state_breakdown$state == "CA"], 50)
  assert_equal(state_breakdown$mean_state_support_pct_core_revenue[state_breakdown$state == "TX"], 30)
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
    report_answers = data.frame(question = "q", calculation = "c", note = "n", stringsAsFactors = FALSE),
    distress_answers = data.frame(question = "dq", value = "dv", calculation = "dc", note = "dn", stringsAsFactors = FALSE),
    research_cuts_answers = data.frame(question = "rq", value = "rv", calculation = "rc", note = "rn", stringsAsFactors = FALSE),
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
  assert_identical(names(worksheets)[3], "DistressAnswers")
  assert_identical(names(worksheets)[4], "ResearchCutsAnswers")
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
