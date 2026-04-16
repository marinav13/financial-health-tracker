run_test("IPEDS collector EAP total FTE returns NA for missing unitid", function() {
  empty_index <- list()
  result <- get_eap_total_fte("99999", empty_index)
  assert_identical(result, NA_real_)
})

run_test("IPEDS collector EAP total FTE computes ft+pt/3", function() {
  fake_row <- data.frame(UNITID = "100", EAPFT = "42", EAPPT = "6", stringsAsFactors = FALSE)
  fake_index <- list("100" = fake_row)
  result <- get_eap_total_fte("100", fake_index)
  # 42 + 6/3 = 44
  assert_equal(result, 44)
})

run_test("IPEDS collector EAP total FTE returns NA when both fields NA", function() {
  fake_row <- data.frame(UNITID = "100", EAPFT = NA, EAPPT = NA, stringsAsFactors = FALSE)
  fake_index <- list("100" = fake_row)
  result <- get_eap_total_fte("100", fake_index)
  assert_identical(result, NA_real_)
})

run_test("IPEDS collector EAP instructional FTE returns NA for missing unitid", function() {
  empty_index <- list()
  result <- get_eap_instructional_fte("99999", empty_index)
  assert_identical(result, NA_real_)
})

run_test("IPEDS collector EAP instructional FTE uses instructional index", function() {
  # Build two indexes with different values to verify the right one is used
  fake_100 <- data.frame(UNITID = "100", EAPFT = "100", EAPPT = "30", stringsAsFactors = FALSE)
  fake_210 <- data.frame(UNITID = "100", EAPFT = "20", EAPPT = "3", stringsAsFactors = FALSE)
  idx_100 <- list("100" = fake_100)
  idx_210 <- list("100" = fake_210)
  # Total should be 100 + 30/3 = 110
  assert_equal(get_eap_total_fte("100", idx_100), 110)
  # Instructional should be 20 + 3/3 = 21
  assert_equal(get_eap_instructional_fte("100", idx_210), 21)
})

run_test("IPEDS collector total headcount returns NA for missing unitid", function() {
  empty_index <- list()
  result <- get_eap_total_headcount("99999", empty_index)
  assert_identical(result, NA_real_)
})

run_test("IPEDS collector total headcount returns EAPTOT value", function() {
  fake_row <- data.frame(UNITID = "100", EAPTOT = "987", stringsAsFactors = FALSE)
  fake_index <- list("100" = fake_row)
  result <- get_eap_total_headcount("100", fake_index)
  assert_equal(result, 987)
})

run_test("IPEDS collector instructional headcount returns NA for missing unitid", function() {
  empty_index <- list()
  result <- get_eap_instructional_headcount("99999", empty_index)
  assert_identical(result, NA_real_)
})

run_test("IPEDS collector lookup_row returns NULL for missing alias", function() {
  idx <- list()
  result <- lookup_row(idx, "HD", "100")
  assert_identical(result, NULL)
})

run_test("IPEDS collector lookup_row returns NULL for missing unitid", function() {
  fake_row <- data.frame(UNITID = "100", col = "val", stringsAsFactors = FALSE)
  idx <- list("HD" = list("100" = fake_row))
  result <- lookup_row(idx, "HD", "99999")
  assert_identical(result, NULL)
})

run_test("IPEDS collector lookup_row returns first row for valid lookup", function() {
  fake_row <- data.frame(UNITID = "100", col = "val", stringsAsFactors = FALSE)
  idx <- list("HD" = list("100" = fake_row))
  result <- lookup_row(idx, "HD", "100")
  assert_identical(result, fake_row)
})

run_test("IPEDS collector lookup_string returns NA for NULL row", function() {
  result <- lookup_string(NULL, "col")
  assert_identical(result, NA_character_)
})

run_test("IPEDS collector lookup_string returns NA for missing column", function() {
  row <- data.frame(col_a = "val", stringsAsFactors = FALSE)
  result <- lookup_string(row, "col_b")
  assert_identical(result, NA_character_)
})

run_test("IPEDS collector lookup_string returns value for present column", function() {
  row <- data.frame(col = "expected_value", stringsAsFactors = FALSE)
  result <- lookup_string(row, "col")
  assert_identical(result, "expected_value")
})

run_test("IPEDS collector lookup_number returns NA_real_ for NULL row", function() {
  result <- lookup_number(NULL, "col")
  assert_identical(result, NA_real_)
})

run_test("IPEDS collector lookup_number parses numeric string", function() {
  row <- data.frame(col = "42.5", stringsAsFactors = FALSE)
  result <- lookup_number(row, "col")
  assert_equal(result, 42.5)
})

run_test("IPEDS collector lookup_number strips commas", function() {
  row <- data.frame(col = "1,234", stringsAsFactors = FALSE)
  result <- lookup_number(row, "col")
  assert_equal(result, 1234)
})

run_test("IPEDS collector rebuild_core_revenue_gasb returns NA when F1D01 absent", function() {
  fake_row <- data.frame(F1B05 = "100", F1B06 = "50", F1B07 = "25", stringsAsFactors = FALSE)
  result <- rebuild_core_revenue_gasb(fake_row)
  assert_identical(result, NA_real_)
})

run_test("IPEDS collector rebuild_core_revenue_gasb subtracts auxiliaries", function() {
  # F1D01 = 1000, F1B05 = 100, F1B06 = 50, F1B07 = 25 → 1000 - 175 = 825
  fake_row <- data.frame(F1D01 = "1000", F1B05 = "100", F1B06 = "50", F1B07 = "25", stringsAsFactors = FALSE)
  result <- rebuild_core_revenue_gasb(fake_row)
  assert_equal(result, 825)
})

run_test("IPEDS collector rebuild_core_revenue_gasb handles NULL row", function() {
  result <- rebuild_core_revenue_gasb(NULL)
  assert_identical(result, NA_real_)
})

run_test("IPEDS collector rebuild_core_revenue_fasb returns NA when total field absent", function() {
  fake_row <- data.frame(F2D12 = "10", F2D13 = "5", F2D14 = "3", stringsAsFactors = FALSE)
  result <- rebuild_core_revenue_fasb(fake_row, "F2B01")
  assert_identical(result, NA_real_)
})

run_test("IPEDS collector rebuild_core_revenue_fasb subtracts auxiliaries", function() {
  # F2B01 = 500, F2D12 = 50, F2D13 = 25, F2D14 = 10 → 500 - 85 = 415
  fake_row <- data.frame(F2B01 = "500", F2D12 = "50", F2D13 = "25", F2D14 = "10", stringsAsFactors = FALSE)
  result <- rebuild_core_revenue_fasb(fake_row, "F2B01")
  assert_equal(result, 415)
})

run_test("IPEDS collector rebuild_core_revenue_fasb handles NULL row", function() {
  result <- rebuild_core_revenue_fasb(NULL, "F2B01")
  assert_identical(result, NA_real_)
})
