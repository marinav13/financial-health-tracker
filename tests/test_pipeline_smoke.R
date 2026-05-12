run_test("Pipeline scripts parse cleanly", function() {
  # Prefer `git ls-files --cached --others --exclude-standard` so untracked
  # scratch files (analyst one-offs gitignored by pattern, e.g. *_PMI.R) don't
  # break the parse smoke test. That listing still includes new in-progress
  # R files that aren't yet committed, so a typo in a brand-new pipeline
  # script still gets caught. Falls back to list.files if git is unavailable.
  # Pass -C root so this works no matter what working directory an earlier
  # fixture left behind.
  git_listing <- tryCatch(
    suppressWarnings(system2(
      "git",
      c("-C", root, "ls-files", "--cached", "--others", "--exclude-standard", "scripts/"),
      stdout = TRUE,
      stderr = FALSE
    )),
    error = function(e) character(0),
    warning = function(w) character(0)
  )
  if (!is.character(git_listing)) git_listing <- character(0)

  script_paths <- if (length(git_listing) > 0L) {
    sort(file.path(root, git_listing[grepl("\\.R$", git_listing)]))
  } else {
    # git unavailable or empty listing — fall back to list.files but exclude
    # known scratch patterns so analyst one-off scripts don't break parse.
    all_files <- sort(list.files(
      file.path(root, "scripts"),
      pattern = "\\.R$",
      recursive = TRUE,
      full.names = TRUE
    ))
    all_files[!grepl("(?:^|/)(?:[0-9]+_script_)?PMI\\.R$|_PMI\\.R$|msi_analysis\\.R$", all_files)]
  }
  script_paths <- script_paths[file.exists(script_paths)]
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

run_test("Deprecated college cuts JSON exporter hard-stops before writing production data", function() {
  script_path <- file.path(root, "scripts", "build_college_cuts_json.R")
  env <- new.env(parent = globalenv())
  sys.source(script_path, envir = env)
  assert_true(exists("main", envir = env, inherits = FALSE), "Expected main() in build_college_cuts_json.R")

  err <- tryCatch(
    {
      env$main()
      NULL
    },
    error = function(e) conditionMessage(e)
  )

  assert_true(
    !is.null(err) && grepl("deprecated and intentionally blocked", err, fixed = TRUE),
    "Deprecated college cuts JSON exporter should stop with an explicit deprecation message."
  )
  assert_true(
    grepl("build_web_exports.R", err, fixed = TRUE),
    "Deprecated exporter stop message should redirect maintainers to build_web_exports.R."
  )
})
