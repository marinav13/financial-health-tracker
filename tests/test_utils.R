run_test("CLI arg helpers", function() {
  args <- c("--input", "file.csv", "--refresh", "TRUE", "--dry-run")
  assert_identical(get_arg(args, "--input"), "file.csv")
  assert_identical(get_arg(args, "--missing", "fallback"), "fallback")
  assert_true(arg_has(args, "--dry-run"))
  assert_true(!arg_has(args, "--force"))
})

run_test("Numeric helpers", function() {
  assert_equal(to_num(c("1,234", "", "NULL")), c(1234, NA_real_, NA_real_))
  assert_identical(safe_divide(10, 2), 5)
  assert_true(is.na(safe_divide(10, 0)))
  assert_identical(safe_pct_change(90, 100), -10)
})

run_test("File helpers", function() {
  temp_dir <- tempfile("helper-smoke-")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  sample_txt <- file.path(temp_dir, "sample.txt")
  writeLines("ok", sample_txt)
  require_existing_local_file(sample_txt, "sample text", "unused")
  assert_identical(NULL %||% "fallback", "fallback")
  assert_identical(NA %||% "fallback", "fallback")

  csv_path <- file.path(temp_dir, "sample.csv")
  write_csv_atomic(data.frame(col = 1, stringsAsFactors = FALSE), csv_path)
  assert_true(file.exists(csv_path), "Expected atomic CSV output to exist.")

  missing_message <- tryCatch(
    {
      require_existing_local_file(file.path(temp_dir, "missing.txt"), "missing text", "create it")
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  assert_true(grepl("Missing required local source file", missing_message, fixed = TRUE))
})

run_test("Collapse unique values", function() {
  assert_identical(collapse_unique_values(c("b", NA, "a", "b")), "a; b")
  assert_true(is.na(collapse_unique_values(c(NA, NA))))
})
