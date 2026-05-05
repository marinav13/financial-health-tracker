main <- function(cli_args = NULL) {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  args <- parse_cli_args(cli_args)
  ipeds <- load_ipeds_paths()
  ipeds_layout <- ipeds$ipeds_layout
  get_arg_value <- function(flag, default = NULL) get_arg(args, flag, default)

  ensure_packages(c("dplyr", "httr2", "jsonlite", "readr", "stringr", "xml2"))
  source(file.path(getwd(), "scripts", "shared", "accreditation_helpers.R"))
  source(file.path(getwd(), "scripts", "shared", "dapip_helpers.R"))

  input_csv <- get_arg_value("--input", ipeds_layout(root = ".")$dataset_csv)
  raw_ipeds_csv <- ipeds_layout(root = ".")$raw_csv
  output_prefix <- get_arg_value(
    "--output-prefix",
    file.path(getwd(), "data_pipelines", "accreditation", "dapip")
  )
  cache_dir <- get_arg_value(
    "--cache-dir",
    file.path(getwd(), "data_pipelines", "accreditation", "cache", "dapip")
  )
  manual_aliases_path <- get_arg_value(
    "--manual-aliases",
    file.path(getwd(), "data_pipelines", "accreditation", "dapip_manual_aliases.csv")
  )
  refresh <- tolower(get_arg_value("--refresh", "true")) %in% c("true", "1", "yes", "y")
  limit <- suppressWarnings(as.integer(get_arg_value("--limit", NA_character_)))
  verbose <- arg_has(args, "--verbose")

  require_existing_local_file(
    input_csv,
    "DAPIP crosswalk input dataset",
    "Build the canonical IPEDS dataset first."
  )

  dir.create(dirname(output_prefix), recursive = TRUE, showWarnings = FALSE)
  ensure_dapip_cache_dirs(cache_dir)

  message("Reading canonical IPEDS dataset ...")
  ipeds_all <- readr::read_csv(input_csv, show_col_types = FALSE, progress = FALSE)
  if (!"opeid" %in% names(ipeds_all)) ipeds_all$opeid <- NA_character_
  if (!"city" %in% names(ipeds_all)) ipeds_all$city <- NA_character_
  if (!"state" %in% names(ipeds_all)) {
    stop("DAPIP crosswalk input is missing required `state` column.", call. = FALSE)
  }
  if (!"institution_name" %in% names(ipeds_all)) {
    stop("DAPIP crosswalk input is missing required `institution_name` column.", call. = FALSE)
  }
  latest_year <- suppressWarnings(max(ipeds_all$year, na.rm = TRUE))
  current_opeid_norm <- normalize_opeid(ipeds_all$opeid)

  if ((!("opeid" %in% names(ipeds_all)) || all(current_opeid_norm == "")) &&
      file.exists(raw_ipeds_csv)) {
    message("Canonical IPEDS input lacks OPEID values; enriching from raw IPEDS dataset ...")
    raw_ipeds <- readr::read_csv(raw_ipeds_csv, show_col_types = FALSE, progress = FALSE)
    if ("year" %in% names(raw_ipeds) && "unitid" %in% names(raw_ipeds) && "opeid" %in% names(raw_ipeds)) {
      raw_latest <- raw_ipeds |>
        dplyr::filter(year == latest_year) |>
        dplyr::transmute(
          unitid = trimws(as.character(unitid)),
          opeid = normalize_opeid(opeid)
        ) |>
        dplyr::distinct(unitid, .keep_all = TRUE)

      ipeds_all <- ipeds_all |>
        dplyr::mutate(unitid = trimws(as.character(unitid))) |>
        dplyr::left_join(raw_latest, by = "unitid", suffix = c("", "_raw")) |>
        dplyr::mutate(
          opeid = dplyr::coalesce(
            dplyr::na_if(normalize_opeid(opeid), ""),
            dplyr::na_if(opeid_raw, "")
          )
        ) |>
        dplyr::select(-tidyselect::any_of("opeid_raw"))
    }
  }

  latest <- ipeds_all |>
    dplyr::filter(year == latest_year) |>
    dplyr::mutate(
      unitid = trimws(as.character(unitid)),
      opeid = normalize_opeid(opeid),
      tracker_institution_name = as.character(institution_name),
      tracker_state = state_name(state),
      tracker_city = as.character(city %||% NA_character_)
    ) |>
    dplyr::select(unitid, opeid, tracker_institution_name, tracker_state, tracker_city) |>
    dplyr::distinct(unitid, .keep_all = TRUE)

  if (!is.na(limit) && limit > 0L) {
    latest <- latest |> dplyr::slice_head(n = limit)
  }

  aliases <- dapip_load_manual_aliases(manual_aliases_path)

  message("Resolving DAPIP crosswalk ...")
  resolved <- purrr::map_dfr(seq_len(nrow(latest)), function(i) {
    row <- latest[i, , drop = FALSE]
    match <- dapip_resolve_crosswalk_row(
      unitid = row$unitid[[1]],
      opeid = row$opeid[[1]],
      institution_name = row$tracker_institution_name[[1]],
      state = row$tracker_state[[1]],
      aliases = aliases,
      cache_dir = cache_dir,
      refresh = refresh,
      verbose = verbose
    )

    tibble::tibble(
      unitid = row$unitid[[1]],
      opeid = row$opeid[[1]],
      opeid_normalized = normalize_opeid(row$opeid[[1]]),
      tracker_institution_name = row$tracker_institution_name[[1]],
      tracker_state = row$tracker_state[[1]],
      tracker_city = row$tracker_city[[1]],
      dapip_id = match$dapip_id %||% NA_integer_,
      dapip_institution_name = match$dapip_institution_name %||% NA_character_,
      dapip_state = match$dapip_state %||% NA_character_,
      match_method = match$match_method %||% "unmatched",
      match_confidence = match$match_confidence %||% "none",
      manual_override_used = isTRUE(match$manual_override_used),
      match_notes = match$match_notes %||% NA_character_,
      search_error = match$search_error %||% NA_character_,
      last_seen_at = as.character(Sys.time())
    )
  })

  matched <- resolved |>
    dplyr::filter(!is.na(dapip_id))

  unmatched <- resolved |>
    dplyr::filter(is.na(dapip_id)) |>
    dplyr::transmute(
      unitid,
      opeid,
      tracker_institution_name,
      tracker_state,
      search_attempt = dplyr::case_when(
        nzchar(opeid_normalized) ~ "opeid_then_unitid",
        TRUE ~ "unitid_only"
      ),
      failure_reason = dplyr::coalesce(search_error, "No DAPIP candidate matched the available identifiers."),
      suggested_manual_action = "Add a row to data_pipelines/accreditation/dapip_manual_aliases.csv with the confirmed DAPIP ID."
    )

  outputs <- list(
    matched = paste0(output_prefix, "_institution_map.csv"),
    unmatched = paste0(output_prefix, "_institution_map_unmatched.csv")
  )

  write_csv_atomic(matched, outputs$matched)
  write_csv_atomic(unmatched, outputs$unmatched)

  message(sprintf("Saved DAPIP crosswalk to %s", outputs$matched))
  message(sprintf("Saved DAPIP unmatched review file to %s", outputs$unmatched))
  message(sprintf(
    "DAPIP crosswalk summary: %d matched, %d unmatched",
    nrow(matched),
    nrow(unmatched)
  ))
}

if (sys.nframe() == 0) {
  main()
}
