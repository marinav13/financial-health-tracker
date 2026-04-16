run_test("Pipeline scripts source cleanly", function() {
  script_paths <- c(
    file.path(root, "scripts", "build_web_exports.R"),
    file.path(root, "scripts", "build_article_workbook.R"),
    file.path(root, "scripts", "build_grant_witness_join.R"),
    file.path(root, "scripts", "build_ipeds_canonical_dataset.R"),
    file.path(root, "scripts", "collect_ipeds_data.R")
  )

  for (script_path in script_paths) {
    env <- new.env(parent = globalenv())
    sys.source(script_path, envir = env)
    assert_true(exists("main", envir = env, inherits = FALSE), paste("Expected main() in", basename(script_path)))
  }
})
