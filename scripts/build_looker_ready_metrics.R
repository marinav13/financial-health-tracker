main <- function(cli_args = NULL) {
  # Backward-compatible wrapper. The canonical dataset build lives in
  # build_ipeds_canonical_dataset.R now, but older entry points may still
  # source this file.
  env <- new.env(parent = globalenv())
  sys.source(file.path(getwd(), "scripts", "build_ipeds_canonical_dataset.R"), envir = env)
  get("main", envir = env, inherits = FALSE)(cli_args)
}

if (sys.nframe() == 0) {
  main()
}
