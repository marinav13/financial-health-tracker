main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args <- parse_cli_args(cli_args)
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)
  has_flag <- function(flag) arg_has(args, flag)

  ensure_packages(c("dplyr", "readr", "digest"))
  source(file.path(getwd(), "scripts", "shared", "editorial_review_helpers.R"))

  input_path <- get_arg_value(
    "--input",
    file.path(getwd(), "data_pipelines", "accreditation", "editorial_overrides.csv")
  )
  output_path <- get_arg_value("--output", input_path)
  reviewed_at <- get_arg_value("--reviewed-at", as.character(Sys.Date()))
  reviewer <- get_arg_value("--reviewer", "grandfathered")
  verbose <- has_flag("--verbose")

  require_existing_local_file(
    input_path,
    "accreditation editorial overrides",
    "Run `Rscript ./scripts/stage_accreditation_review.R --verbose` first so editorial_overrides.csv exists."
  )

  existing <- read_accreditation_editorial_overrides(input_path)
  grandfathered <- grandfather_accreditation_editorial_overrides(
    overrides = existing,
    reviewed_at = reviewed_at,
    reviewer = reviewer
  )

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write_csv_atomic(grandfathered, output_path)

  status_changed <- dplyr::coalesce(existing$review_status, "") != dplyr::coalesce(grandfathered$review_status, "")
  grandfather_flag_changed <- dplyr::coalesce(existing$grandfathered, FALSE) != dplyr::coalesce(grandfathered$grandfathered, FALSE)
  changed_count <- sum(status_changed | grandfather_flag_changed, na.rm = TRUE)
  if (verbose) {
    message("Editorial overrides before grandfathering: ", nrow(existing))
  }
  message("Grandfathered rows now approved: ", sum(grandfathered$grandfathered, na.rm = TRUE))
  message("Rows changed in this run: ", changed_count)
  invisible(list(
    rows = nrow(grandfathered),
    grandfathered = sum(grandfathered$grandfathered, na.rm = TRUE),
    changed = changed_count,
    output = output_path
  ))
}

if (sys.nframe() == 0) {
  main()
}
