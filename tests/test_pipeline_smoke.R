run_test("Pipeline scripts parse cleanly", function() {
  script_paths <- sort(list.files(
    file.path(root, "scripts"),
    pattern = "\\.R$",
    recursive = TRUE,
    full.names = TRUE
  ))
  assert_true(length(script_paths) > 0L, "Expected at least one R script under scripts/.")

  parse_failures <- character()
  for (script_path in script_paths) {
    err <- tryCatch(
      {
        parse(file = script_path)
        NULL
      },
      error = function(e) conditionMessage(e)
    )
    if (!is.null(err)) {
      rel_path <- sub(paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", root), "/"), "", normalizePath(script_path, winslash = "/", mustWork = TRUE))
      parse_failures <- c(parse_failures, sprintf("%s: %s", rel_path, err))
    }
  }

  if (length(parse_failures) > 0L) {
    stop(
      paste(
        "One or more scripts/ R files failed to parse:",
        paste(parse_failures, collapse = "\n"),
        sep = "\n"
      ),
      call. = FALSE
    )
  }
})

run_test("Pipeline entry scripts source cleanly", function() {
  script_paths <- c(
    file.path(root, "scripts", "build_web_exports.R"),
    file.path(root, "scripts", "build_grant_witness_join.R"),
    file.path(root, "scripts", "build_grant_witness_usaspending_sensitivity.R"),
    file.path(root, "scripts", "build_ipeds_canonical_dataset.R"),
    file.path(root, "scripts", "collect_ipeds_data.R")
  )

  for (script_path in script_paths) {
    env <- new.env(parent = globalenv())
    sys.source(script_path, envir = env)
    assert_true(exists("main", envir = env, inherits = FALSE), paste("Expected main() in", basename(script_path)))
  }
})
