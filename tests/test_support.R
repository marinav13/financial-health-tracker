root <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)

source(file.path(root, "scripts", "shared", "utils.R"))

required_pkgs <- c("dplyr", "jsonlite", "purrr", "readr", "readxl", "stringr", "xml2", "httr2")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing_pkgs) > 0L) {
  stop(
    paste(
      "Smoke tests require these installed packages:",
      paste(missing_pkgs, collapse = ", ")
    ),
    call. = FALSE
  )
}
invisible(lapply(required_pkgs, library, character.only = TRUE))

source(file.path(root, "scripts", "shared", "export_helpers.R"))
source(file.path(root, "scripts", "shared", "accreditation_helpers.R"))
source(file.path(root, "scripts", "shared", "grant_witness_helpers.R"))
source(file.path(root, "scripts", "shared", "workbook_helpers.R"))
source(file.path(root, "scripts", "shared", "ipeds_helpers.R"))
source(file.path(root, "scripts", "shared", "ipeds_collector_helpers.R"))
source(file.path(root, "scripts", "shared", "contracts.R"))

failures <- character()
passes <- 0L

fail_test <- function(name, message) {
  failures <<- c(failures, name)
  cat(sprintf("FAIL %s\n%s\n", name, message))
}

pass_test <- function(name) {
  passes <<- passes + 1L
  cat(sprintf("PASS %s\n", name))
}

assert_true <- function(condition, message = "Assertion failed.") {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

assert_identical <- function(actual, expected, message = "Values differed.") {
  if (!identical(actual, expected)) {
    stop(
      sprintf(
        "%s\nExpected: %s\nActual: %s",
        message,
        paste(capture.output(str(expected)), collapse = " "),
        paste(capture.output(str(actual)), collapse = " ")
      ),
      call. = FALSE
    )
  }
}

assert_equal <- function(actual, expected, message = "Values were not equal.") {
  comparison <- all.equal(actual, expected, check.attributes = FALSE)
  if (!isTRUE(comparison)) {
    stop(
      sprintf("%s\n%s", message, paste(comparison, collapse = "\n")),
      call. = FALSE
    )
  }
}

run_test <- function(name, fn) {
  tryCatch(
    {
      fn()
      pass_test(name)
    },
    error = function(e) fail_test(name, conditionMessage(e))
  )
}

run_test_files <- function(test_paths) {
  for (path in test_paths) {
    sys.source(path, envir = globalenv())
  }
  cat(sprintf("\nShared helper smoke tests: %d passed, %d failed.\n", passes, length(failures)))
  if (length(failures) > 0L) {
    stop("Smoke tests failed.", call. = FALSE)
  }
  invisible(list(passes = passes, failures = failures))
}
