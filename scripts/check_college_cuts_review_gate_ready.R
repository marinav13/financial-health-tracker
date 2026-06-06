main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args <- parse_cli_args(cli_args)
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)
  has_flag <- function(flag) arg_has(args, flag)

  ensure_packages(c("readr"))
  source(file.path(getwd(), "scripts", "shared", "editorial_review_helpers.R"))

  candidates_path <- get_arg_value(
    "--candidates",
    file.path(getwd(), "data_pipelines", "college_cuts", "college_cuts_review_candidates.csv")
  )
  overrides_path <- get_arg_value(
    "--overrides",
    file.path(getwd(), "data_pipelines", "college_cuts", "editorial_overrides.csv")
  )
  sheet_id_or_url <- get_arg_value("--sheet", NA_character_)
  sheet_tab <- get_arg_value("--tab", Sys.getenv("COLLEGE_CUTS_REVIEW_SHEET_TAB", unset = "college_cuts_review"))
  auth_json <- get_arg_value("--auth-json", Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = NA_character_))
  email <- get_arg_value("--email", NA_character_)
  cache_dir <- get_arg_value("--cache", file.path(getwd(), ".secrets", "googlesheets4"))
  verbose <- has_flag("--verbose")

  require_existing_local_file(
    candidates_path,
    "college cuts review candidates",
    "Run `Rscript ./scripts/build_web_exports.R --only cuts` first so the review candidates CSV exists."
  )
  require_existing_local_file(
    overrides_path,
    "college cuts editorial overrides",
    "Run `Rscript ./scripts/stage_college_cuts_review.R --verbose` first so editorial_overrides.csv exists."
  )

  sheet_target <- trimws(as.character(sheet_id_or_url %||% ""))
  if (nzchar(sheet_target)) {
    if (verbose) {
      message("Syncing current college cuts review statuses from Google Sheet before readiness check.")
    }
    source(file.path(getwd(), "scripts", "pull_college_cuts_overrides.R"), local = TRUE)
    pull_college_cuts_overrides(
      input_path = overrides_path,
      output_path = overrides_path,
      sheet_id_or_url = sheet_target,
      sheet_tab = sheet_tab,
      auth_json = auth_json,
      email = email,
      cache_dir = cache_dir,
      verbose = verbose
    )
  }

  candidates <- read_college_cuts_review_candidates(candidates_path)
  overrides <- read_college_cuts_editorial_overrides(overrides_path)

  candidate_ids <- trim_text(candidates$cut_id)
  override_ids <- trim_text(overrides$cut_id)
  missing_override_ids <- candidate_ids[!(candidate_ids %in% override_ids)]
  if (length(missing_override_ids) > 0L) {
    stop(
      sprintf(
        paste(
          "College cuts review gate is not ready.",
          "%d committed review candidate(s) are missing override rows.",
          "Sample cut_id values: %s"
        ),
        length(missing_override_ids),
        paste(utils::head(missing_override_ids, 5L), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  matched_overrides <- overrides[match(candidate_ids, override_ids), , drop = FALSE]
  terminal_mask <- is_terminal_review_decision(matched_overrides$review_status)
  missing_decision_ids <- candidate_ids[!terminal_mask]
  if (length(missing_decision_ids) > 0L) {
    stop(
      sprintf(
        paste(
          "College cuts review gate is not ready.",
          "%d committed review candidate(s) are missing a terminal decision (approved/reject).",
          "Sample cut_id values: %s"
        ),
        length(missing_decision_ids),
        paste(utils::head(missing_decision_ids, 5L), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  approved_count <- sum(trim_text(matched_overrides$review_status) == "approved", na.rm = TRUE)
  reject_count <- sum(trim_text(matched_overrides$review_status) == "reject", na.rm = TRUE)

  if (verbose) {
    message("Committed candidate rows: ", length(candidate_ids))
    message("Override rows available: ", nrow(overrides))
  }
  message("College cuts review gate is ready.")
  message("Committed candidate rows covered: ", length(candidate_ids))
  message("Approved rows: ", approved_count)
  message("Rejected rows: ", reject_count)
  invisible(list(
    candidates = length(candidate_ids),
    overrides = nrow(overrides),
    approved = approved_count,
    rejected = reject_count
  ))
}

if (sys.nframe() == 0) {
  main()
}
