main <- function(cli_args = NULL) {
  args <- if (is.null(cli_args)) commandArgs(trailingOnly = TRUE) else cli_args

  get_arg_value <- function(flag, default = NULL) {
    idx <- match(flag, args)
    if (!is.na(idx) && idx < length(args)) args[[idx + 1]] else default
  }

  has_flag <- function(flag) {
    flag %in% args
  }

  ensure_packages <- function(pkgs) {
    missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing) > 0) {
      install.packages(missing, repos = "https://cloud.r-project.org")
    }
    invisible(lapply(pkgs, library, character.only = TRUE))
  }

  ensure_packages(c("dplyr", "readr", "stringr", "tidyr"))

  financial_input <- get_arg_value(
    "--financial-input",
    file.path(getwd(), "reporting", "ipeds_financial_health_reporting_2014_2024.csv")
  )
  output_prefix <- get_arg_value(
    "--output-prefix",
    file.path(getwd(), "grant_witness", "grant_witness")
  )
  cache_dir <- get_arg_value(
    "--cache-dir",
    file.path(getwd(), "grant_witness", "cache")
  )
  manual_include_path <- get_arg_value(
    "--manual-include",
    file.path(getwd(), "grant_witness", "manual_include.csv")
  )
  manual_match_overrides_path <- get_arg_value(
    "--manual-match-overrides",
    file.path(getwd(), "grant_witness", "manual_match_overrides.csv")
  )
  skip_download <- has_flag("--skip-download")

  if (!file.exists(financial_input)) {
    stop("Financial input file not found: ", financial_input)
  }

  dir.create(dirname(output_prefix), recursive = TRUE, showWarnings = FALSE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  downloads <- c(
    nih = "https://files.grant-witness.us/nih_terminations.csv",
    nsf = "https://files.grant-witness.us/nsf_terminations.csv",
    epa = "https://files.grant-witness.us/epa_terminations.csv",
    samhsa = "https://files.grant-witness.us/samhsa_terminations.csv",
    cdc = "https://files.grant-witness.us/cdc_terminations.csv"
  )

  cached_paths <- file.path(cache_dir, paste0(names(downloads), "_terminations.csv"))
  names(cached_paths) <- names(downloads)

  normalize_name <- function(x) {
    x |>
      as.character() |>
      stringr::str_to_lower() |>
      stringr::str_replace_all("&", " and ") |>
      stringr::str_replace_all("[^a-z0-9 ]", " ") |>
      stringr::str_squish()
  }

  prettify_text <- function(x) {
    x_chr <- as.character(x)
    letters_only <- stringr::str_replace_all(x_chr, "[^A-Za-z]", "")
    upper_only <- stringr::str_replace_all(x_chr, "[^A-Z]", "")
    upper_share <- dplyr::if_else(
      nchar(letters_only) > 0,
      nchar(upper_only) / nchar(letters_only),
      0
    )
    is_shouty <- !is.na(x_chr) &
      nchar(stringr::str_squish(x_chr)) > 0 &
      stringr::str_detect(x_chr, "[A-Z]") &
      (
        !stringr::str_detect(x_chr, "[a-z]") |
        upper_share >= 0.7
      )
    out <- x_chr
    out[is_shouty] <- tools::toTitleCase(stringr::str_to_lower(out[is_shouty]))
    out <- stringr::str_replace_all(out, regex("\\bA\\s*&\\s*M\\b", ignore_case = TRUE), "A&M")
    out <- stringr::str_replace_all(out, regex("\\bMed Br\\b"), "Medical Branch")
    out <- stringr::str_replace_all(out, regex("\\bMed Sci(?:ences)?\\b"), "Medical Sciences")
    out <- stringr::str_replace_all(out, regex("\\bMed Ctr\\b"), "Medical Center")
    out <- stringr::str_replace_all(out, regex("\\bHlth Sci(?:ence)? C(?:n)?tr\\b"), "Health Sciences Center")
    out <- stringr::str_replace_all(out, regex("\\bHlth Science Center\\b"), "Health Sciences Center")
    out <- stringr::str_replace_all(out, regex("\\bHlth Science\\b", ignore_case = TRUE), "Health Science")
    out <- stringr::str_replace_all(out, regex("\\bHlth\\b"), "Health")
    out <- stringr::str_replace_all(out, regex("\\bCtr\\b"), "Center")
    out <- stringr::str_replace_all(out, regex("\\bCntr\\b"), "Center")
    out <- stringr::str_replace_all(out, regex("\\bSci\\b"), "Science")
    out <- stringr::str_replace_all(out, regex("\\bScis\\b"), "Sciences")
    out <- stringr::str_replace_all(out, regex("\\bInst\\b"), "Institute")
    out <- stringr::str_replace_all(out, regex("\\bSt Univ\\b"), "State University")
    out <- stringr::str_replace_all(out, regex("\\bUniv\\b"), "University")
    out <- stringr::str_replace_all(out, regex("\\bSch Of\\b"), "School of")
    out <- stringr::str_replace_all(out, regex("\\bOf The\\b"), "of the")
    out <- stringr::str_replace_all(out, regex(", The$"), "")
    out <- stringr::str_replace_all(out, regex("\\bThe University Of Central Florida Board Of Trustees\\b"), "The University of Central Florida Board of Trustees")
    out <- stringr::str_replace_all(out, regex("\\bRector & Visitors Of The University Of Virginia\\b"), "Rector & Visitors of the University of Virginia")
    out <- stringr::str_replace_all(out, regex("\\bRegents Of The University Of Minnesota\\b"), "Regents of the University of Minnesota")
    out <- stringr::str_replace_all(out, regex("\\bRegents Of The University Of California, San Francisco\\b"), "Regents of the University of California, San Francisco")
    out <- stringr::str_replace_all(out, regex("\\bPennsylvania State University, The\\b"), "The Pennsylvania State University")
    out <- stringr::str_replace_all(out, regex("\\bOhio State University\\b"), "The Ohio State University")
    out <- stringr::str_replace_all(out, regex("\\bUniversity Of Texas Medicalical Branch\\b"), "University of Texas Medical Branch")
    out <- stringr::str_replace_all(out, regex("\\bUniversity Of Texas Medical Branch Galveston\\b"), "University of Texas Medical Branch Galveston")
    out <- stringr::str_replace_all(out, regex("\\bUniversity Of Texas Health Science Center\\b"), "University of Texas Health Sciences Center")
    out <- stringr::str_replace_all(out, regex("\\bUt Southwestern Medical Center\\b"), "UT Southwestern Medical Center")
    out <- stringr::str_replace_all(out, regex("\\bTexas a&m University\\b", ignore_case = TRUE), "Texas A&M University")
    out <- stringr::str_replace_all(out, regex("\\bTexas a & m University\\b", ignore_case = TRUE), "Texas A&M University")
    out <- stringr::str_replace_all(out, regex("\\bInter American University Of Puerto Rico, Inc\\.\\b"), "Inter American University of Puerto Rico, Inc.")
    out <- stringr::str_replace_all(out, regex("\\bCuny\\b"), "CUNY")
    out <- stringr::str_replace_all(out, regex("\\bSuny\\b"), "SUNY")
    out <- stringr::str_replace_all(out, "\\bNyu\\b", "NYU")
    out <- stringr::str_replace_all(out, "\\bUcsf\\b", "UCSF")
    out <- stringr::str_replace_all(out, "\\bUcla\\b", "UCLA")
    out <- stringr::str_replace_all(out, "\\bUt\\b", "UT")
    out <- stringr::str_replace_all(out, "\\bA&m\\b", "A&M")
    out <- stringr::str_replace_all(out, "\\bUtah\\b State Higher Education System--University Of Utah\\b", "Utah State Higher Education System--University of Utah")
    out <- stringr::str_squish(out)
    out
  }

  prettify_institution_name <- function(x) {
    out <- prettify_text(x)
    out <- stringr::str_replace_all(out, regex("^University of Texas Medical Branch Galveston$", ignore_case = TRUE), "University of Texas Medical Branch at Galveston")
    out <- stringr::str_replace_all(out, regex("^Rector & Visitors of the University of Virginia$", ignore_case = TRUE), "University of Virginia")
    out <- stringr::str_replace_all(out, regex("^Regents of the University of Minnesota$", ignore_case = TRUE), "University of Minnesota")
    out <- stringr::str_replace_all(out, regex("^Regents of the University of California, San Francisco,? the?$", ignore_case = TRUE), "University of California, San Francisco")
    out <- stringr::str_replace_all(out, regex("^The University of Central Florida Board of Trustees$", ignore_case = TRUE), "University of Central Florida")
    out <- stringr::str_replace_all(out, regex("^Trustees of Indiana University$", ignore_case = TRUE), "Indiana University")
    out <- stringr::str_replace_all(out, regex("^Board of Trustees of Southern Illinois University$", ignore_case = TRUE), "Southern Illinois University")
    out <- stringr::str_replace_all(out, regex("^Pennsylvania State University,? the$", ignore_case = TRUE), "Pennsylvania State University")
    out <- stringr::str_replace_all(out, regex("^The Ohio State University(?:-Main Campus)?$", ignore_case = TRUE), "The Ohio State University")
    out <- stringr::str_replace_all(out, regex("^Texas a&m University$", ignore_case = TRUE), "Texas A&M University")
    out <- stringr::str_replace_all(out, regex("^University of Texas Health Sciences Center$", ignore_case = TRUE), "University of Texas Health Sciences Center")
    out <- stringr::str_replace_all(out, regex("^Northwestern University at Chicago$", ignore_case = TRUE), "Northwestern University")
    out <- stringr::str_replace_all(out, regex("^University of Washington$", ignore_case = TRUE), "University of Washington")
    out <- stringr::str_replace_all(out, regex("^University of Colorado at Boulder$", ignore_case = TRUE), "University of Colorado Boulder")
    out <- stringr::str_replace_all(out, regex("^University of Illinois at Urbana-Champaign$", ignore_case = TRUE), "University of Illinois Urbana-Champaign")
    out <- stringr::str_replace_all(out, regex("^University of South Carolina at Columbia$", ignore_case = TRUE), "University of South Carolina")
    out <- stringr::str_replace_all(out, regex("^University of Texas San Antonio$", ignore_case = TRUE), "University of Texas at San Antonio")
    out <- stringr::str_replace_all(out, regex("^University of Texas El Paso$", ignore_case = TRUE), "University of Texas at El Paso")
    out <- stringr::str_replace_all(out, regex("^University of Texas Arlington$", ignore_case = TRUE), "University of Texas at Arlington")
    out <- stringr::str_replace_all(out, regex("^University of Alabama Tuscaloosa$", ignore_case = TRUE), "University of Alabama")
    out <- stringr::str_replace_all(out, regex("^University of Puerto Rico Medical Sciences$", ignore_case = TRUE), "University of Puerto Rico Medical Sciences")
    out <- stringr::str_replace_all(out, regex("^Tufts University Boston$", ignore_case = TRUE), "Tufts University")
    out <- stringr::str_replace_all(out, regex("^University of Puerto Rico Med Sciences$", ignore_case = TRUE), "University of Puerto Rico Medical Sciences")
    out <- stringr::str_replace_all(out, regex("^Lsu Health Sciences Center$", ignore_case = TRUE), "LSU Health Science Center")
    out <- stringr::str_squish(out)
    out
  }

  is_excluded_higher_ed_name <- function(x) {
    norm <- normalize_name(x)
    norm %in% c(
      "university enterprises incorporated",
      "american college of obstetricians and gynecologists"
    )
  }

  is_noncampus_medical_or_foundation_name <- function(x) {
    norm <- normalize_name(x)
    has_campus_anchor <- stringr::str_detect(
      norm,
      regex("\\b(university|college|school of medicine|medical college|community college|polytechnic|institute of technology)\\b", ignore_case = TRUE)
    )
    stringr::str_detect(
      norm,
      regex("\\b(medical center|cancer center|research foundation|foundation)\\b", ignore_case = TRUE)
    ) & !has_campus_anchor
  }

  strip_legal_prefixes <- function(x) {
    x |>
      as.character() |>
      stringr::str_replace(
        regex("^(the )?(regents of( the)?|trustees of|president and fellows of|board of trustees of|the trustees of)\\s+", ignore_case = TRUE),
        ""
      ) |>
      stringr::str_replace(regex("^the\\s+", ignore_case = TRUE), "") |>
      stringr::str_squish()
  }

  simplify_institution_name <- function(x) {
    out <- x |>
      as.character() |>
      strip_legal_prefixes() |>
      stringr::str_replace(regex("\\bhlth sci cntr\\b", ignore_case = TRUE), "health sciences center") |>
      stringr::str_replace(regex("\\bhlth sci ctr\\b", ignore_case = TRUE), "health sciences center") |>
      stringr::str_replace(regex("\\bhlth science ctr\\b", ignore_case = TRUE), "health sciences center") |>
      stringr::str_replace(regex("\\bhlth science center\\b", ignore_case = TRUE), "health sciences center") |>
      stringr::str_replace(regex("\\bhealth science center\\b", ignore_case = TRUE), "health sciences center") |>
      stringr::str_replace(regex("\\bhealth scis ctr\\b", ignore_case = TRUE), "health sciences center") |>
      stringr::str_replace(regex("\\bhealth scis center\\b", ignore_case = TRUE), "health sciences center") |>
      stringr::str_replace(regex("\\bmed ctr\\b", ignore_case = TRUE), "medical center") |>
      stringr::str_replace(regex("\\bcan ctr\\b", ignore_case = TRUE), "cancer center") |>
      stringr::str_replace(regex("\\binst\\b", ignore_case = TRUE), "institute") |>
      stringr::str_replace(regex("\\bst univ\\b", ignore_case = TRUE), "state university") |>
      stringr::str_replace(regex("\\btech university\\b", ignore_case = TRUE), "technology university") |>
      stringr::str_replace(regex("\\b(main campus)\\b", ignore_case = TRUE), "") |>
      stringr::str_replace(regex("\\bschool of medicine\\b", ignore_case = TRUE), "") |>
      stringr::str_replace(regex("\\bresearch foundation(,? inc\\.?| incorporated)?\\b", ignore_case = TRUE), "") |>
      stringr::str_replace(regex("\\bfoundation(,? inc\\.?| incorporated)?\\b", ignore_case = TRUE), "") |>
      stringr::str_replace(regex("\\bin st\\.? louis\\b", ignore_case = TRUE), "") |>
      stringr::str_replace(regex("-[[:space:]]*[A-Za-z .']+ campus$", ignore_case = TRUE), "") |>
      stringr::str_replace(regex("-[[:space:]]*ann arbor$", ignore_case = TRUE), "") |>
      stringr::str_replace(regex("\\bat pittsburgh\\b", ignore_case = TRUE), "") |>
      stringr::str_replace(regex("\\bat ann arbor\\b", ignore_case = TRUE), "") |>
      stringr::str_replace(regex("\\bthe ohio state university\\b", ignore_case = TRUE), "ohio state university") |>
      stringr::str_replace(regex("\\bharvard college\\b", ignore_case = TRUE), "harvard university") |>
      stringr::str_replace(regex("\\bvirginia polytechnic inst and st univ\\b", ignore_case = TRUE), "virginia polytechnic institute and state university") |>
      stringr::str_replace(regex("\\bvirginia polytechnic institute and state universityersity\\b", ignore_case = TRUE), "virginia polytechnic institute and state university") |>
      stringr::str_replace(regex("\\buniv of maryland\\b", ignore_case = TRUE), "university of maryland") |>
      stringr::str_replace(regex("\\buniv of\\b", ignore_case = TRUE), "university of") |>
      stringr::str_replace(regex("\\buniversity of tx\\b", ignore_case = TRUE), "university of texas") |>
      stringr::str_replace(
        regex("^university of ([a-z]+) at ([a-z].+)$", ignore_case = TRUE),
        "university of \\1 \\2"
      ) |>
      stringr::str_replace(regex("\\buniv\\b", ignore_case = TRUE), "university") |>
      stringr::str_replace(regex("\\bhlth\\b", ignore_case = TRUE), "health") |>
      stringr::str_replace(regex("\\bctr\\b", ignore_case = TRUE), "center") |>
      stringr::str_replace(regex("\\bcntr\\b", ignore_case = TRUE), "center") |>
      stringr::str_replace(regex("\\bmed\\b", ignore_case = TRUE), "medical") |>
      stringr::str_replace(regex("\\bscis\\b", ignore_case = TRUE), "sciences") |>
      stringr::str_replace(regex("\\btech\\b", ignore_case = TRUE), "technology") |>
      stringr::str_replace_all(regex("\\s+-\\s+"), " - ") |>
      stringr::str_squish()

    normalize_name(out)
  }

  is_likely_higher_ed_name <- function(x) {
    norm <- normalize_name(x)
    excluded <- is_excluded_higher_ed_name(x)
    has_signal <- stringr::str_detect(
      norm,
      regex("\\b(university|college|institute of technology|polytechnic|school of medicine|medical college|community college|research foundation|medical center|cancer center|health science center|health sciences)\\b", ignore_case = TRUE)
    )
    standalone_medical_or_foundation <- is_noncampus_medical_or_foundation_name(x)
    has_exclusion <- stringr::str_detect(
      norm,
      regex("\\b(department|state of|board of health|commission|authority|office|county|city of|fund|network|corporation|llc|department of health|public health)\\b", ignore_case = TRUE)
    )
    has_signal & !has_exclusion & !excluded & !standalone_medical_or_foundation
  }

  state_lookup <- c(
    setNames(state.name, state.abb),
    DC = "District of Columbia",
    PR = "Puerto Rico",
    VI = "Virgin Islands",
    GU = "Guam",
    MP = "Northern Mariana Islands",
    AS = "American Samoa",
    PW = "Palau",
    FM = "Federated States of Micronesia",
    MH = "Marshall Islands"
  )

  abbr_to_state <- function(x) {
    x_chr <- as.character(x)
    out <- unname(state_lookup[toupper(x_chr)])
    out[is.na(out)] <- x_chr[is.na(out)]
    out
  }

  to_num <- function(x) {
    if (is.null(x)) return(rep(NA_real_, length.out = length(x)))
    x_chr <- trimws(as.character(x))
    x_chr[x_chr %in% c("", "NA", "NULL", "N/A")] <- NA_character_
    suppressWarnings(as.numeric(gsub("[,$]", "", x_chr)))
  }

  null_if_empty <- function(x) {
    x_chr <- trimws(as.character(x))
    x_chr[x_chr == ""] <- NA_character_
    x_chr
  }

  prettify_location_text <- function(x) {
    x_chr <- null_if_empty(x)
    keep <- !is.na(x_chr)
    x_chr[keep] <- x_chr[keep] |>
      iconv(from = "", to = "ASCII//TRANSLIT") |>
      stringr::str_replace_all("\\s+", " ") |>
      stringr::str_squish() |>
      stringr::str_to_title()
    x_chr
  }

  normalize_city <- function(x) {
    x |>
      as.character() |>
      iconv(from = "", to = "ASCII//TRANSLIT") |>
      stringr::str_replace_all("[^A-Za-z0-9 ]", " ") |>
      stringr::str_squish() |>
      stringr::str_to_lower()
  }

  normalize_status <- function(x) {
    x |>
      as.character() |>
      iconv(from = "", to = "ASCII//TRANSLIT") |>
      stringr::str_replace_all("[^A-Za-z ]", " ") |>
      stringr::str_squish() |>
      stringr::str_to_lower()
  }

  is_currently_disrupted <- function(agency, status) {
    status_norm <- normalize_status(status)
    dplyr::case_when(
      agency == "nih" ~ status_norm %in% c("terminated", "frozen funding"),
      agency == "nsf" ~ status_norm %in% c("terminated"),
      agency == "epa" ~ status_norm %in% c("terminated"),
      agency == "samhsa" ~ status_norm %in% c("terminated"),
      agency == "cdc" ~ status_norm %in% c("terminated", "at risk"),
      TRUE ~ FALSE
    )
  }

  classify_status_bucket <- function(agency, status) {
    status_norm <- normalize_status(status)
    dplyr::case_when(
      is_currently_disrupted(agency, status) ~ "currently_disrupted",
      agency == "nih" & status_norm %in% c("possibly reinstated", "possibly unfrozen funding", "unfrozen funding") ~ "not_currently_disrupted",
      status_norm %in% c("possibly reinstated", "reinstated") ~ "not_currently_disrupted",
      TRUE ~ "other"
    )
  }

  maybe_download <- function(url, path) {
    if (skip_download && file.exists(path)) return(invisible(path))
    message("Downloading ", basename(path), " ...")
    utils::download.file(url, destfile = path, mode = "wb", quiet = FALSE)
    invisible(path)
  }

  safe_max <- function(x) {
    if (length(x) == 0 || all(is.na(x))) return(NA_real_)
    max(x, na.rm = TRUE)
  }

  for (agency in names(downloads)) {
    maybe_download(downloads[[agency]], cached_paths[[agency]])
  }

  message("Reading financial tracker data ...")
  financial_all <- readr::read_csv(financial_input, show_col_types = FALSE, progress = FALSE)
  latest_year <- suppressWarnings(max(financial_all$year, na.rm = TRUE))
  financial_latest <- financial_all |>
    dplyr::filter(year == latest_year) |>
    dplyr::transmute(
      unitid = as.character(unitid),
      tracker_institution_name = institution_name,
      tracker_city = prettify_location_text(city),
      tracker_state = prettify_location_text(as.character(state)),
      tracker_control_label = control_label,
      tracker_category = category,
      norm_name = normalize_name(institution_name),
      city_norm = normalize_city(city),
      state_full = prettify_location_text(as.character(state))
    )

  city_lookup <- financial_latest |>
    dplyr::add_count(norm_name, state_full, city_norm, name = "candidate_count") |>
    dplyr::filter(!is.na(city_norm), city_norm != "", candidate_count == 1) |>
    dplyr::select(-candidate_count)

  fallback_lookup <- financial_latest |>
    dplyr::add_count(norm_name, state_full, name = "candidate_count") |>
    dplyr::filter(candidate_count == 1) |>
    dplyr::select(-candidate_count)

  alias_seed <- financial_latest |>
    dplyr::transmute(
      unitid,
      tracker_institution_name,
      tracker_city,
      tracker_state,
      tracker_control_label,
      tracker_category,
      alias = tracker_institution_name
    )

  alias_variants <- financial_latest |>
    dplyr::transmute(
      unitid,
      tracker_institution_name,
      tracker_city,
      tracker_state,
      tracker_control_label,
      tracker_category,
      alias_1 = stringr::str_replace(tracker_institution_name, regex("-[[:space:]]*Main Campus$", ignore_case = TRUE), ""),
      alias_2 = dplyr::case_when(
        stringr::str_detect(tracker_institution_name, regex("^University of Pittsburgh-Pittsburgh Campus$", ignore_case = TRUE)) ~ "University of Pittsburgh",
        TRUE ~ NA_character_
      ),
      alias_3 = dplyr::case_when(
        stringr::str_detect(tracker_institution_name, regex("^University of Pittsburgh-Pittsburgh Campus$", ignore_case = TRUE)) ~ "University of Pittsburgh at Pittsburgh",
        TRUE ~ NA_character_
      ),
      alias_4 = dplyr::case_when(
        stringr::str_detect(tracker_institution_name, regex("^University of Michigan-Ann Arbor$", ignore_case = TRUE)) ~ "University of Michigan",
        TRUE ~ NA_character_
      ),
      alias_5 = dplyr::case_when(
        stringr::str_detect(tracker_institution_name, regex("^Washington University in St Louis$", ignore_case = TRUE)) ~ "Washington University",
        TRUE ~ NA_character_
      ),
      alias_6 = dplyr::case_when(
        stringr::str_detect(tracker_institution_name, regex("^Ohio State University-Main Campus$", ignore_case = TRUE)) ~ "The Ohio State University",
        TRUE ~ NA_character_
      ),
      alias_7 = dplyr::case_when(
        stringr::str_detect(tracker_institution_name, regex("^Ohio State University-Main Campus$", ignore_case = TRUE)) ~ "Ohio State University, The",
        TRUE ~ NA_character_
      ),
      alias_8 = dplyr::case_when(
        stringr::str_detect(tracker_institution_name, regex("^University of Michigan-Ann Arbor$", ignore_case = TRUE)) ~ "Regents of the University of Michigan - Ann Arbor",
        TRUE ~ NA_character_
      ),
      alias_9 = dplyr::case_when(
        stringr::str_detect(tracker_institution_name, regex("^University of Michigan-Ann Arbor$", ignore_case = TRUE)) ~ "Regents of the University of Michigan",
        TRUE ~ NA_character_
      ),
      alias_10 = dplyr::case_when(
        stringr::str_detect(tracker_institution_name, regex("^Boston University$", ignore_case = TRUE)) ~ "Trustees of Boston University",
        TRUE ~ NA_character_
      ),
      alias_11 = dplyr::case_when(
        stringr::str_detect(tracker_institution_name, regex("^Harvard University$", ignore_case = TRUE)) ~ "President and Fellows of Harvard College",
        TRUE ~ NA_character_
      ),
      alias_12 = dplyr::case_when(
        stringr::str_detect(tracker_institution_name, regex("^Washington University in St Louis$", ignore_case = TRUE)) ~ "Washington University, The",
        TRUE ~ NA_character_
      ),
      alias_13 = dplyr::case_when(
        stringr::str_detect(tracker_institution_name, regex("^Virginia Polytechnic Institute and State University$", ignore_case = TRUE)) ~ "Virginia Polytechnic Inst and St Univ",
        TRUE ~ NA_character_
      )
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("alias_"),
      names_to = "alias_type",
      values_to = "alias"
    ) |>
    dplyr::select(-alias_type)

  alias_lookup <- dplyr::bind_rows(alias_seed, alias_variants) |>
    dplyr::filter(!is.na(alias), trimws(alias) != "") |>
    dplyr::mutate(
      alias_norm = simplify_institution_name(alias),
      tracker_city = prettify_location_text(tracker_city),
      state_full = tracker_state
    ) |>
    dplyr::distinct(alias_norm, state_full, unitid, .keep_all = TRUE) |>
    dplyr::add_count(alias_norm, state_full, name = "candidate_count") |>
    dplyr::filter(candidate_count == 1) |>
    dplyr::select(-candidate_count) |>
    dplyr::distinct(alias_norm, state_full, .keep_all = TRUE)

  manual_include <- if (file.exists(manual_include_path)) {
    readr::read_csv(manual_include_path, show_col_types = FALSE, progress = FALSE) |>
      dplyr::transmute(
        organization_name,
        organization_state = null_if_empty(organization_state),
        include_in_dataset = toupper(as.character(include_in_dataset %||% "TRUE")) == "TRUE"
      )
  } else {
    data.frame(
      organization_name = character(),
      organization_state = character(),
      include_in_dataset = logical(),
      stringsAsFactors = FALSE
    )
  }

  manual_match_overrides <- if (file.exists(manual_match_overrides_path)) {
    readr::read_csv(manual_match_overrides_path, show_col_types = FALSE, progress = FALSE) |>
      dplyr::transmute(
        organization_name,
        organization_name_display = prettify_institution_name(organization_name),
        organization_state = null_if_empty(organization_state),
        override_unitid = as.character(unitid),
        override_tracker_institution_name = institution_name_override,
        override_tracker_state = state_override,
        override_tracker_control_label = control_label_override,
        override_tracker_category = category_override,
        override_likely_higher_ed = TRUE
      ) |>
      dplyr::left_join(
        financial_latest |>
          dplyr::select(
            unitid,
            financial_tracker_institution_name = tracker_institution_name,
            financial_tracker_city = tracker_city,
            financial_tracker_state = tracker_state,
            financial_tracker_control_label = tracker_control_label,
            financial_tracker_category = tracker_category
          ),
        by = c("override_unitid" = "unitid")
      ) |>
      dplyr::mutate(
        override_tracker_institution_name = dplyr::coalesce(override_tracker_institution_name, financial_tracker_institution_name),
        override_tracker_city = financial_tracker_city,
        override_tracker_state = dplyr::coalesce(override_tracker_state, financial_tracker_state),
        override_tracker_control_label = dplyr::coalesce(override_tracker_control_label, financial_tracker_control_label),
        override_tracker_category = dplyr::coalesce(override_tracker_category, financial_tracker_category)
      ) |>
      dplyr::select(-dplyr::starts_with("financial_tracker_"))
  } else {
    data.frame(
      organization_name = character(),
      organization_name_display = character(),
      organization_state = character(),
      override_unitid = character(),
      override_tracker_institution_name = character(),
      override_tracker_city = character(),
      override_tracker_state = character(),
      override_tracker_control_label = character(),
      override_tracker_category = character(),
      override_likely_higher_ed = logical(),
      stringsAsFactors = FALSE
    )
  }

  read_csv_utf8 <- function(path) {
    readr::read_csv(
      path,
      show_col_types = FALSE,
      progress = FALSE,
      locale = readr::locale(encoding = "UTF-8")
    )
  }

  standardize_grants <- function(agency, df) {
    agency_name <- agency
    source_file_name <- basename(cached_paths[[agency_name]])

    if (agency == "nih") {
      out <- df |>
        dplyr::transmute(
          agency = agency_name,
          source_file = source_file_name,
          grant_id = as.character(full_award_number),
          grant_id_core = as.character(core_award_number),
          status = status,
          organization_name = org_name,
          organization_state = abbr_to_state(org_state),
          organization_city = org_city,
          organization_type = dplyr::coalesce(org_type, org_traits),
          project_title = project_title,
          project_abstract = abstract_text,
          start_date = targeted_start_date,
          original_end_date = targeted_end_date,
          termination_date = termination_date,
          reinstatement_date = reinstated_est_date,
          award_value = to_num(total_award),
          award_outlaid = to_num(total_estimated_outlays),
          award_remaining = to_num(total_estimated_remaining),
          remaining_field = "total_estimated_remaining",
          source_url = usaspending_url,
          detail_url = reporter_url
        )
    } else if (agency == "nsf") {
      out <- df |>
        dplyr::transmute(
          agency = agency_name,
          source_file = source_file_name,
          grant_id = as.character(grant_id),
          grant_id_core = as.character(grant_id),
          status = status,
          organization_name = org_name,
          organization_state = abbr_to_state(org_state),
          organization_city = org_city,
          organization_type = award_type,
          project_title = project_title,
          project_abstract = abstract,
          start_date = dplyr::coalesce(nsf_start_date, usasp_start_date),
          original_end_date = dplyr::coalesce(nsf_end_date, usasp_end_date),
          termination_date = termination_date,
          reinstatement_date = reinstatement_date,
          award_value = to_num(estimated_budget),
          award_outlaid = to_num(estimated_outlays),
          award_remaining = to_num(estimated_remaining),
          remaining_field = "estimated_remaining",
          source_url = usaspending_url,
          detail_url = nsf_url
        )
    } else if (agency == "epa") {
      out <- df |>
        dplyr::transmute(
          agency = agency_name,
          source_file = source_file_name,
          grant_id = as.character(grant_id),
          grant_id_core = as.character(grant_id),
          status = status,
          organization_name = organization,
          organization_state = abbr_to_state(org_state),
          organization_city = org_city,
          organization_type = org_type,
          project_title = project_title,
          project_abstract = project_description,
          start_date = start_date,
          original_end_date = original_end_date,
          termination_date = termination_date,
          reinstatement_date = reinstatement_date,
          award_value = to_num(award_value),
          award_outlaid = to_num(award_outlaid),
          award_remaining = to_num(award_remaining),
          remaining_field = "award_remaining",
          source_url = usaspending_url,
          detail_url = nggs_url
        )
    } else if (agency == "samhsa") {
      out <- df |>
        dplyr::transmute(
          agency = agency_name,
          source_file = source_file_name,
          grant_id = as.character(grant_id),
          grant_id_core = as.character(grant_id),
          status = status,
          organization_name = org_name,
          organization_state = abbr_to_state(org_state),
          organization_city = org_city,
          organization_type = org_type,
          project_title = title,
          project_abstract = abstract,
          start_date = dplyr::coalesce(project_start_date, first_award_date),
          original_end_date = project_original_end_date,
          termination_date = termination_date,
          reinstatement_date = reinstatement_date,
          award_value = to_num(award_value),
          award_outlaid = to_num(award_outlaid),
          award_remaining = to_num(award_remaining),
          remaining_field = "award_remaining",
          source_url = usaspending_url,
          detail_url = taggs_url
        )
    } else if (agency == "cdc") {
      out <- df |>
        dplyr::transmute(
          agency = agency_name,
          source_file = source_file_name,
          grant_id = as.character(grant_id),
          grant_id_core = as.character(grant_id),
          status = status,
          organization_name = org_name,
          organization_state = abbr_to_state(org_state),
          organization_city = org_city,
          organization_type = org_type,
          project_title = title,
          project_abstract = NA_character_,
          start_date = project_start_date,
          original_end_date = project_original_end_date,
          termination_date = termination_date,
          reinstatement_date = reinstatement_date,
          award_value = to_num(award_value),
          award_outlaid = to_num(award_outlaid),
          award_remaining = to_num(award_remaining),
          remaining_field = "award_remaining",
          source_url = usaspending_url,
          detail_url = taggs_url
        )
    } else {
      stop("Unsupported agency: ", agency)
    }

    out |>
      dplyr::mutate(
        organization_name = null_if_empty(organization_name),
        organization_state = prettify_location_text(null_if_empty(organization_state)),
        organization_city = prettify_location_text(null_if_empty(organization_city)),
        organization_type = null_if_empty(organization_type),
        project_title = null_if_empty(prettify_text(project_title)),
        project_abstract = null_if_empty(project_abstract),
        start_date = null_if_empty(start_date),
        original_end_date = null_if_empty(original_end_date),
        termination_date = null_if_empty(termination_date),
        reinstatement_date = null_if_empty(reinstatement_date),
        source_url = null_if_empty(source_url),
        detail_url = null_if_empty(detail_url),
        status_bucket = classify_status_bucket(agency, status),
        currently_disrupted = is_currently_disrupted(agency, status),
        organization_name_display = prettify_institution_name(organization_name),
        norm_name = normalize_name(organization_name),
        simplified_norm_name = simplify_institution_name(organization_name),
        organization_city_norm = normalize_city(organization_city),
        likely_higher_ed = is_likely_higher_ed_name(organization_name)
      )
  }

  message("Reading and standardizing Grant Witness files ...")
  grants_joined <- dplyr::bind_rows(lapply(names(cached_paths), function(agency) {
    standardize_grants(agency, read_csv_utf8(cached_paths[[agency]]))
  })) |>
    dplyr::left_join(
      city_lookup |>
        dplyr::rename(
          city_unitid = unitid,
          city_tracker_institution_name = tracker_institution_name,
          city_tracker_city = tracker_city,
          city_tracker_state = tracker_state,
          city_tracker_control_label = tracker_control_label,
          city_tracker_category = tracker_category
        ),
      by = c(
        "norm_name",
        "organization_state" = "state_full",
        "organization_city_norm" = "city_norm"
      )
    ) |>
    dplyr::left_join(
      fallback_lookup,
      by = c("norm_name", "organization_state" = "state_full")
    ) |>
    dplyr::left_join(
      alias_lookup |>
        dplyr::rename(
          alias_unitid = unitid,
          alias_tracker_institution_name = tracker_institution_name,
          alias_tracker_city = tracker_city,
          alias_tracker_state = tracker_state,
          alias_tracker_control_label = tracker_control_label,
          alias_tracker_category = tracker_category
        ),
      by = c("simplified_norm_name" = "alias_norm", "organization_state" = "state_full")
    ) |>
    dplyr::left_join(
      manual_include,
      by = c("organization_name_display" = "organization_name", "organization_state")
    ) |>
    dplyr::left_join(
      manual_match_overrides |>
        dplyr::select(-organization_name_display),
      by = c("organization_name", "organization_state")
    ) |>
    dplyr::left_join(
      manual_match_overrides |>
        dplyr::rename(
          display_override_unitid = override_unitid,
          display_override_tracker_institution_name = override_tracker_institution_name,
          display_override_tracker_city = override_tracker_city,
          display_override_tracker_state = override_tracker_state,
          display_override_tracker_control_label = override_tracker_control_label,
          display_override_tracker_category = override_tracker_category,
          display_override_likely_higher_ed = override_likely_higher_ed
        ) |>
        dplyr::select(
          organization_name_display,
          organization_state,
          display_override_unitid,
          display_override_tracker_institution_name,
          display_override_tracker_city,
          display_override_tracker_state,
          display_override_tracker_control_label,
          display_override_tracker_category,
          display_override_likely_higher_ed
        ),
      by = c("organization_name_display", "organization_state")
    ) |>
    dplyr::mutate(
      matched_unitid = dplyr::coalesce(city_unitid, unitid, alias_unitid, override_unitid, display_override_unitid),
      tracker_institution_name = dplyr::coalesce(city_tracker_institution_name, tracker_institution_name, alias_tracker_institution_name, override_tracker_institution_name, display_override_tracker_institution_name),
      tracker_city = dplyr::coalesce(city_tracker_city, tracker_city, alias_tracker_city, override_tracker_city, display_override_tracker_city),
      tracker_state = dplyr::coalesce(city_tracker_state, tracker_state, alias_tracker_state, override_tracker_state, display_override_tracker_state),
      tracker_control_label = dplyr::coalesce(city_tracker_control_label, tracker_control_label, alias_tracker_control_label, override_tracker_control_label, display_override_tracker_control_label),
      tracker_category = dplyr::coalesce(city_tracker_category, tracker_category, alias_tracker_category, override_tracker_category, display_override_tracker_category),
      likely_higher_ed = dplyr::if_else(
        is_noncampus_medical_or_foundation_name(organization_name),
        FALSE,
        dplyr::coalesce(override_likely_higher_ed, display_override_likely_higher_ed, include_in_dataset, likely_higher_ed)
      ),
      match_method = dplyr::case_when(
        !is.na(city_unitid) ~ "normalized_name_city_state",
        is.na(city_unitid) & !is.na(unitid) ~ "normalized_name_state_fallback",
        is.na(city_unitid) & is.na(unitid) & !is.na(alias_unitid) ~ "alias_name_state_fallback",
        is.na(city_unitid) & is.na(unitid) & is.na(alias_unitid) & !is.na(override_unitid) ~ "manual_name_override",
        is.na(city_unitid) & is.na(unitid) & is.na(alias_unitid) & is.na(override_unitid) & !is.na(display_override_unitid) ~ "manual_display_name_override",
        include_in_dataset %in% TRUE ~ "manual_include_unmatched",
        likely_higher_ed ~ "likely_higher_ed_unmatched",
        TRUE ~ "unmatched"
      ),
      in_financial_tracker = !is.na(matched_unitid)
    ) |>
    dplyr::select(
      agency,
      source_file,
      grant_id,
      grant_id_core,
      status,
      status_bucket,
      currently_disrupted,
      organization_name,
      organization_name_display,
      organization_state,
      organization_city,
      organization_type,
      project_title,
      project_abstract,
      start_date,
      original_end_date,
      termination_date,
      reinstatement_date,
      award_value,
      award_outlaid,
      award_remaining,
      remaining_field,
      source_url,
      detail_url,
      likely_higher_ed,
      matched_unitid,
      tracker_institution_name,
      tracker_city,
      tracker_state,
      tracker_control_label,
      tracker_category,
      match_method,
      in_financial_tracker
    )

  grants_joined <- grants_joined |>
    dplyr::filter(!(currently_disrupted & !is.na(award_remaining) & award_remaining <= 0))

  institution_summary_long <- grants_joined |>
    dplyr::filter(currently_disrupted, !is.na(organization_name), !is.na(organization_state)) |>
    dplyr::mutate(
      institution_key = dplyr::coalesce(
        paste0("unitid:", matched_unitid),
        paste0("name_state:", normalize_name(organization_name), "|", organization_state)
      ),
      display_name = dplyr::coalesce(tracker_institution_name, organization_name),
      display_name = prettify_institution_name(display_name),
      display_city = dplyr::coalesce(tracker_city, organization_city),
      display_city = prettify_location_text(display_city),
      display_state = dplyr::coalesce(tracker_state, organization_state)
    ) |>
    dplyr::group_by(
      institution_key,
      matched_unitid,
      display_name,
      display_city,
      display_state,
      tracker_control_label,
      tracker_category,
      agency
    ) |>
    dplyr::summarise(
      likely_higher_ed = any(likely_higher_ed, na.rm = TRUE),
      disrupted_grants = dplyr::n(),
      disrupted_award_remaining = sum(award_remaining, na.rm = TRUE),
      largest_single_grant_remaining = safe_max(award_remaining),
      .groups = "drop"
    ) |>
    dplyr::mutate(largest_single_grant_remaining = as.numeric(largest_single_grant_remaining))

  institution_summary_wide <- institution_summary_long |>
    dplyr::select(
      institution_key,
      matched_unitid,
      display_name,
      display_city,
      display_state,
      tracker_control_label,
      tracker_category,
      likely_higher_ed,
      agency,
      disrupted_grants,
      disrupted_award_remaining
    ) |>
    tidyr::pivot_wider(
      names_from = agency,
      values_from = c(disrupted_grants, disrupted_award_remaining),
      names_glue = "{agency}_{.value}",
      values_fill = 0
    )

  grant_cols <- grep("_disrupted_grants$", names(institution_summary_wide), value = TRUE)
  amount_cols <- grep("_disrupted_award_remaining$", names(institution_summary_wide), value = TRUE)
  if (length(grant_cols) > 0) {
    institution_summary_wide$total_disrupted_grants <- rowSums(institution_summary_wide[, grant_cols, drop = FALSE], na.rm = TRUE)
  } else {
    institution_summary_wide$total_disrupted_grants <- integer(nrow(institution_summary_wide))
  }
  if (length(amount_cols) > 0) {
    institution_summary_wide$total_disrupted_award_remaining <- rowSums(institution_summary_wide[, amount_cols, drop = FALSE], na.rm = TRUE)
  } else {
    institution_summary_wide$total_disrupted_award_remaining <- numeric(nrow(institution_summary_wide))
  }

  institution_summary_wide <- institution_summary_wide |>
    dplyr::arrange(dplyr::desc(total_disrupted_award_remaining), display_name)

  unmatched_for_review <- grants_joined |>
    dplyr::filter(currently_disrupted, !in_financial_tracker) |>
    dplyr::count(agency, organization_name = organization_name_display, organization_state, organization_city, organization_type, likely_higher_ed, match_method, wt = award_remaining, name = "disrupted_award_remaining") |>
    dplyr::arrange(dplyr::desc(disrupted_award_remaining), organization_name)

  grant_path <- paste0(output_prefix, "_grant_level_joined.csv")
  summary_long_path <- paste0(output_prefix, "_institution_summary_long.csv")
  summary_path <- paste0(output_prefix, "_institution_summary.csv")
  higher_ed_summary_path <- paste0(output_prefix, "_higher_ed_institution_summary.csv")
  unmatched_path <- paste0(output_prefix, "_unmatched_for_review.csv")
  likely_higher_ed_unmatched_path <- paste0(output_prefix, "_likely_higher_ed_unmatched_for_review.csv")

  higher_ed_summary <- institution_summary_wide |>
    dplyr::filter(!is.na(matched_unitid) | likely_higher_ed)

  likely_higher_ed_unmatched <- unmatched_for_review |>
    dplyr::filter(likely_higher_ed)

  likely_higher_ed_review_ready <- likely_higher_ed_unmatched |>
    dplyr::group_by(organization_name, organization_state) |>
    dplyr::summarise(
      agencies = paste(sort(unique(stats::na.omit(as.character(agency)))), collapse = "; "),
      disrupted_award_remaining = sum(disrupted_award_remaining, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(disrupted_award_remaining), organization_name)

  readr::write_csv(grants_joined, grant_path, na = "")
  readr::write_csv(institution_summary_long, summary_long_path, na = "")
  readr::write_csv(institution_summary_wide, summary_path, na = "")
  readr::write_csv(higher_ed_summary, higher_ed_summary_path, na = "")
  readr::write_csv(unmatched_for_review, unmatched_path, na = "")
  readr::write_csv(likely_higher_ed_unmatched, likely_higher_ed_unmatched_path, na = "")
  readr::write_csv(likely_higher_ed_review_ready, paste0(output_prefix, "_likely_higher_ed_review_ready.csv"), na = "")

  cat(sprintf("Saved grant-level data to %s\n", grant_path))
  cat(sprintf("Saved institution summary (long) to %s\n", summary_long_path))
  cat(sprintf("Saved institution summary to %s\n", summary_path))
  cat(sprintf("Saved higher-ed institution summary to %s\n", higher_ed_summary_path))
  cat(sprintf("Saved unmatched review file to %s\n", unmatched_path))
  cat(sprintf("Saved likely higher-ed unmatched review file to %s\n", likely_higher_ed_unmatched_path))
  cat(sprintf("Saved likely higher-ed review-ready file to %s\n", paste0(output_prefix, "_likely_higher_ed_review_ready.csv")))
}

if (sys.nframe() == 0) {
  main()
}
