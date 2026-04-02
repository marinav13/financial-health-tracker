main <- function(cli_args = NULL) {
  # Compatibility wrapper: the project now uses collect_ipeds_data.R as the
  # canonical raw IPEDS collector, but this entrypoint stays in place so older
  # scripts or commands still work.
  args <- if (is.null(cli_args)) commandArgs(trailingOnly = TRUE) else cli_args
  collector_path <- file.path(getwd(), "scripts", "collect_ipeds_data.R")
  env <- new.env(parent = globalenv())
  sys.source(collector_path, envir = env)
  if (!exists("main", envir = env, inherits = FALSE)) {
    stop("No main() found in script: ", collector_path)
  }
  get("main", envir = env, inherits = FALSE)(args)
}

if (sys.nframe() == 0) {
  main()
}
