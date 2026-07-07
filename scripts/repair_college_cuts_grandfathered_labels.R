main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args <- parse_cli_args(cli_args)
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)
  has_flag <- function(flag) arg_has(args, flag)

  ensure_packages(c("dplyr", "readr"))
  source(file.path(getwd(), "scripts", "shared", "editorial_review_helpers.R"))

  input_path <- get_arg_value(
    "--input",
    file.path(getwd(), "data_pipelines", "college_cuts", "editorial_overrides.csv")
  )
  output_path <- get_arg_value("--output", input_path)
  verbose <- has_flag("--verbose")

  require_existing_local_file(
    input_path,
    "college cuts editorial overrides",
    "Run `Rscript ./scripts/stage_college_cuts_review.R --verbose` first so editorial_overrides.csv exists."
  )

  existing <- read_college_cuts_editorial_overrides(input_path)
  repaired <- backfill_college_cuts_grandfathered_generic_labels(existing)

  override_description_before <- trim_optional_text(existing$override_cut_description)
  override_label_before <- trim_optional_text(existing$override_cut_label)
  override_description_after <- trim_optional_text(repaired$override_cut_description)
  override_label_after <- trim_optional_text(repaired$override_cut_label)
  description_changed <- (!is.na(override_description_before) | !is.na(override_description_after)) &
    (is.na(override_description_before) != is.na(override_description_after) |
       dplyr::coalesce(override_description_before, "") != dplyr::coalesce(override_description_after, ""))
  label_changed <- (!is.na(override_label_before) | !is.na(override_label_after)) &
    (is.na(override_label_before) != is.na(override_label_after) |
       dplyr::coalesce(override_label_before, "") != dplyr::coalesce(override_label_after, ""))
  changed_mask <- description_changed | label_changed

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write_csv_atomic(repaired, output_path)

  if (verbose && any(changed_mask, na.rm = TRUE)) {
    print(utils::head(
      repaired[changed_mask, c("cut_id", "source_institution_name", "source_cut_description", "source_generated_cut_label", "override_cut_description", "override_cut_label"), drop = FALSE],
      10L
    ))
  }
  message("College cuts rows repaired: ", sum(changed_mask, na.rm = TRUE))
  message("Output written: ", output_path)

  invisible(list(
    rows = nrow(repaired),
    changed = sum(changed_mask, na.rm = TRUE),
    output = output_path
  ))
}

if (sys.nframe() == 0) {
  main()
}
