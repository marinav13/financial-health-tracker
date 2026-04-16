# scripts/shared/grant_witness_helpers.R
#
# Text normalisation, institution name matching, and download helpers
# for the Grant Witness data pipeline.
# Source this after utils.R inside main() in build_grant_witness_join.R.
#
# Requires: dplyr, stringr, tools (loaded by the caller)
# Note: to_num here strips "$" and "," which differs slightly from utils.R;
# it intentionally shadows the version from utils.R in this script's scope.

normalize_name <- function(x) {
  x |>
    as.character() |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("&", " and ") |>
    stringr::str_replace_all("[^a-z0-9 ]", " ") |>
    stringr::str_squish()
}

# ---------------------------------------------------------------------------
# Lookup tables for name normalisation
# ---------------------------------------------------------------------------

# Applied by prettify_text() after toTitleCase().
# Order matters: more-specific patterns (multi-word, compound) come first.
# Patterns without (?i) are intentionally case-sensitive — the text is
# already title-cased at the point these run.
PRETTIFY_ABBREV <- c(
  "(?i)\\bA\\s*&\\s*M\\b"              = "A&M",
  "\\bMed Br\\b"                        = "Medical Branch",
  "\\bMed Sci(?:ences)?\\b"             = "Medical Sciences",
  "\\bMed Ctr\\b"                       = "Medical Center",
  "\\bHlth Sci(?:ence)? C(?:n)?tr\\b"  = "Health Sciences Center",
  "\\bHlth Science Center\\b"           = "Health Sciences Center",
  "(?i)\\bHlth Science\\b"             = "Health Science",
  "\\bHlth\\b"                          = "Health",
  "\\bCtr\\b"                           = "Center",
  "\\bCntr\\b"                          = "Center",
  "\\bSci\\b"                           = "Science",
  "\\bScis\\b"                          = "Sciences",
  "\\bInst\\b"                          = "Institute",
  "\\bSt Univ\\b"                       = "State University",
  "\\bUniv\\b"                          = "University",
  "\\bSch Of\\b"                        = "School of",
  "\\bOf The\\b"                        = "of the",
  ", The$"                              = ""
)

PRETTIFY_INSTITUTION_FIXES <- c(
  "\\bThe University Of Central Florida Board Of Trustees\\b"    = "The University of Central Florida Board of Trustees",
  "\\bRector & Visitors Of The University Of Virginia\\b"        = "Rector & Visitors of the University of Virginia",
  "\\bRegents Of The University Of Minnesota\\b"                 = "Regents of the University of Minnesota",
  "\\bRegents Of The University Of California, San Francisco\\b" = "Regents of the University of California, San Francisco",
  "\\bPennsylvania State University, The\\b"                     = "The Pennsylvania State University",
  "\\bOhio State University\\b"                                  = "The Ohio State University",
  "\\bUniversity Of Texas Medicalical Branch\\b"                 = "University of Texas Medical Branch",
  "\\bUniversity Of Texas Medical Branch Galveston\\b"           = "University of Texas Medical Branch Galveston",
  "\\bUniversity Of Texas Health Science Center\\b"              = "University of Texas Health Sciences Center",
  "\\bUt Southwestern Medical Center\\b"                        = "UT Southwestern Medical Center",
  "(?i)\\bTexas a&m University\\b"                              = "Texas A&M University",
  "(?i)\\bTexas a & m University\\b"                            = "Texas A&M University",
  "\\bInter American University Of Puerto Rico, Inc\\.\\b"       = "Inter American University of Puerto Rico, Inc."
)

PRETTIFY_ACRONYMS <- c(
  "\\bCuny\\b" = "CUNY",
  "\\bSuny\\b" = "SUNY",
  "\\bNyu\\b"  = "NYU",
  "\\bUcsf\\b" = "UCSF",
  "\\bUcla\\b" = "UCLA",
  "\\bUt\\b"   = "UT",
  "\\bA&m\\b"  = "A&M",
  "\\bUtah\\b State Higher Education System--University Of Utah\\b" = "Utah State Higher Education System--University of Utah"
)

# Full-string canonical name corrections applied by prettify_institution_name().
# All patterns are anchored (^...$) and case-insensitive ((?i)).
PRETTIFY_CANONICAL_NAMES <- c(
  "(?i)^University of Texas Medical Branch Galveston$"                    = "University of Texas Medical Branch at Galveston",
  "(?i)^Rector & Visitors of the University of Virginia$"                 = "University of Virginia",
  "(?i)^Regents of the University of Minnesota$"                          = "University of Minnesota",
  "(?i)^Regents of the University of California, San Francisco,? the?$"   = "University of California, San Francisco",
  "(?i)^The University of Central Florida Board of Trustees$"             = "University of Central Florida",
  "(?i)^Trustees of Indiana University$"                                  = "Indiana University",
  "(?i)^Board of Trustees of Southern Illinois University$"               = "Southern Illinois University",
  "(?i)^Pennsylvania State University,? the$"                             = "Pennsylvania State University",
  "(?i)^The Ohio State University(?:-Main Campus)?$"                      = "The Ohio State University",
  "(?i)^Texas a&m University$"                                            = "Texas A&M University",
  "(?i)^University of Texas Health Sciences Center$"                      = "University of Texas Health Sciences Center",
  "(?i)^Northwestern University at Chicago$"                              = "Northwestern University",
  "(?i)^University of Washington$"                                        = "University of Washington",
  "(?i)^University of Colorado at Boulder$"                               = "University of Colorado Boulder",
  "(?i)^University of Illinois at Urbana-Champaign$"                      = "University of Illinois Urbana-Champaign",
  "(?i)^University of South Carolina at Columbia$"                        = "University of South Carolina",
  "(?i)^University of Texas San Antonio$"                                 = "University of Texas at San Antonio",
  "(?i)^University of Texas El Paso$"                                     = "University of Texas at El Paso",
  "(?i)^University of Texas Arlington$"                                   = "University of Texas at Arlington",
  "(?i)^University of Alabama Tuscaloosa$"                                = "University of Alabama",
  "(?i)^University of Puerto Rico Medical Sciences$"                      = "University of Puerto Rico Medical Sciences",
  "(?i)^Tufts University Boston$"                                         = "Tufts University",
  "(?i)^University of Puerto Rico Med Sciences$"                          = "University of Puerto Rico Medical Sciences",
  "(?i)^Lsu Health Sciences Center$"                                      = "LSU Health Science Center"
)

# Abbreviation normalisation applied by simplify_institution_name().
# All patterns are case-insensitive ((?i)).
# Order matters: compound / more-specific forms come before single-word abbreviations.
SIMPLIFY_ABBREV <- c(
  "(?i)\\bhlth sci cntr\\b"         = "health sciences center",
  "(?i)\\bhlth sci ctr\\b"          = "health sciences center",
  "(?i)\\bhlth science ctr\\b"      = "health sciences center",
  "(?i)\\bhlth science center\\b"   = "health sciences center",
  "(?i)\\bhealth science center\\b" = "health sciences center",
  "(?i)\\bhealth scis ctr\\b"       = "health sciences center",
  "(?i)\\bhealth scis center\\b"    = "health sciences center",
  "(?i)\\bmed ctr\\b"               = "medical center",
  "(?i)\\bcan ctr\\b"               = "cancer center",
  "(?i)\\binst\\b"                  = "institute",
  "(?i)\\bst univ\\b"               = "state university",
  "(?i)\\btech university\\b"       = "technology university",
  "(?i)\\b(main campus)\\b"         = "",
  "(?i)\\bschool of medicine\\b"    = "",
  "(?i)\\bresearch foundation(,? inc\\.?| incorporated)?\\b" = "",
  "(?i)\\bfoundation(,? inc\\.?| incorporated)?\\b"         = "",
  "(?i)\\bin st\\.? louis\\b"       = "",
  "(?i)-[[:space:]]*[A-Za-z .']+ campus$" = "",
  "(?i)-[[:space:]]*ann arbor$"     = "",
  "(?i)\\bat pittsburgh\\b"         = "",
  "(?i)\\bat ann arbor\\b"          = "",
  "(?i)\\bthe ohio state university\\b"                              = "ohio state university",
  "(?i)\\bharvard college\\b"                                        = "harvard university",
  "(?i)\\bvirginia polytechnic inst and st univ\\b"                  = "virginia polytechnic institute and state university",
  "(?i)\\bvirginia polytechnic institute and state universityersity\\b" = "virginia polytechnic institute and state university",
  "(?i)\\buniv of maryland\\b"      = "university of maryland",
  "(?i)\\buniv of\\b"               = "university of",
  "(?i)\\buniversity of tx\\b"      = "university of texas",
  "(?i)^university of ([a-z]+) at ([a-z].+)$" = "university of \\1 \\2",
  "(?i)\\buniv\\b"                  = "university",
  "(?i)\\bhlth\\b"                  = "health",
  "(?i)\\bctr\\b"                   = "center",
  "(?i)\\bcntr\\b"                  = "center",
  "(?i)\\bmed\\b"                   = "medical",
  "(?i)\\bscis\\b"                  = "sciences",
  "(?i)\\btech\\b"                  = "technology",
  "\\s+-\\s+"                       = " - "
)

# ---------------------------------------------------------------------------
# Name normalisation functions
# ---------------------------------------------------------------------------

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
  out <- stringr::str_replace_all(out, PRETTIFY_ABBREV)
  out <- stringr::str_replace_all(out, PRETTIFY_INSTITUTION_FIXES)
  out <- stringr::str_replace_all(out, PRETTIFY_ACRONYMS)
  stringr::str_squish(out)
}

prettify_institution_name <- function(x) {
  out <- prettify_text(x)
  out <- stringr::str_replace_all(out, PRETTIFY_CANONICAL_NAMES)
  stringr::str_squish(out)
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

# These phrases are the explicit text cues we use to identify awards that
# look like grantmaking/pass-through programs rather than direct university
# research grants.
pass_through_exclusion_phrases <- c(
  "grantmaker initiative",
  "regional grantmaker",
  "subgrantee",
  "subgrantees",
  "pass-through entity",
  "pass through entity",
  "pass-through",
  "manage and distribute funds",
  "administer subawards",
  "will administer subawards",
  "issue subawards",
  "will issue subawards",
  "competitive and noncompetitive subgrants",
  "subaward distribution",
  "subaward administration"
)

# Return the matched phrase list so the exclusion remains auditable rather
# than acting like an opaque boolean flag.
detect_pass_through_phrase <- function(project_title, project_abstract) {
  text <- paste(project_title, project_abstract, sep = " ")
  text <- text |>
    as.character() |>
    stringr::str_to_lower() |>
    stringr::str_squish()

  matches <- pass_through_exclusion_phrases[
    vapply(
      pass_through_exclusion_phrases,
      function(phrase) stringr::str_detect(text, stringr::fixed(phrase, ignore_case = TRUE)),
      logical(1)
    )
  ]

  if (length(matches) == 0) {
    NA_character_
  } else {
    paste(matches, collapse = "; ")
  }
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
    stringr::str_replace_all(SIMPLIFY_ABBREV) |>
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

# Agency-specific status rules are data, not control flow. Keep them in lookup
# tables so changing a label means editing one row, not rewriting case_when().
GRANT_WITNESS_STATUS_RULES <- list(
  currently_disrupted = list(
    nih = c("terminated", "frozen funding"),
    nsf = c("terminated"),
    epa = c("terminated"),
    samhsa = c("terminated"),
    cdc = c("terminated", "at risk")
  ),
  not_currently_disrupted = list(
    nih = c("possibly reinstated", "possibly unfrozen funding", "unfrozen funding"),
    .default = c("possibly reinstated", "reinstated")
  )
)

status_rule_values <- function(rule_name, agency) {
  rules <- GRANT_WITNESS_STATUS_RULES[[rule_name]] %||% list()
  values <- rules[[agency]]
  if (is.null(values)) {
    values <- rules[[".default"]]
  }
  values %||% character()
}

status_matches_rule <- function(agency, status, rule_name) {
  mapply(
    function(one_agency, one_status) {
      normalize_status(one_status) %in% status_rule_values(rule_name, one_agency)
    },
    agency,
    status,
    USE.NAMES = FALSE
  )
}

is_currently_disrupted <- function(agency, status) {
  status_matches_rule(agency, status, "currently_disrupted")
}

classify_status_bucket <- function(agency, status) {
  dplyr::case_when(
    is_currently_disrupted(agency, status) ~ "currently_disrupted",
    status_matches_rule(agency, status, "not_currently_disrupted") ~ "not_currently_disrupted",
    TRUE ~ "other"
  )
}

# Column mappings for the raw Grant Witness agency files. Each agency points
# the standardizer at source columns and lightweight transforms.
GRANT_WITNESS_STANDARDIZATION_SPECS <- list(
  nih = list(
    grant_id = list(columns = "full_award_number"),
    grant_id_core = list(columns = "core_award_number"),
    status = list(columns = "status"),
    organization_name = list(columns = "org_name"),
    organization_state = list(columns = "org_state", transform = "state"),
    organization_city = list(columns = "org_city"),
    organization_type = list(columns = c("org_type", "org_traits")),
    project_title = list(columns = "project_title"),
    project_abstract = list(columns = "abstract_text"),
    start_date = list(columns = "targeted_start_date"),
    original_end_date = list(columns = "targeted_end_date"),
    termination_date = list(columns = "termination_date"),
    reinstatement_date = list(columns = "reinstated_est_date"),
    award_value = list(columns = "total_award", transform = "numeric"),
    award_outlaid = list(columns = "total_estimated_outlays", transform = "numeric"),
    award_remaining = list(columns = "total_estimated_remaining", transform = "numeric"),
    remaining_field = list(value = "total_estimated_remaining"),
    source_url = list(columns = "usaspending_url"),
    detail_url = list(columns = "reporter_url")
  ),
  nsf = list(
    grant_id = list(columns = "grant_id"),
    grant_id_core = list(columns = "grant_id"),
    status = list(columns = "status"),
    organization_name = list(columns = "org_name"),
    organization_state = list(columns = "org_state", transform = "state"),
    organization_city = list(columns = "org_city"),
    organization_type = list(columns = "award_type"),
    project_title = list(columns = "project_title"),
    project_abstract = list(columns = "abstract"),
    start_date = list(columns = c("nsf_start_date", "usasp_start_date")),
    original_end_date = list(columns = c("nsf_end_date", "usasp_end_date")),
    termination_date = list(columns = "termination_date"),
    reinstatement_date = list(columns = "reinstatement_date"),
    award_value = list(columns = "estimated_budget", transform = "numeric"),
    award_outlaid = list(columns = "estimated_outlays", transform = "numeric"),
    award_remaining = list(columns = "estimated_remaining", transform = "numeric"),
    remaining_field = list(value = "estimated_remaining"),
    source_url = list(columns = "usaspending_url"),
    detail_url = list(columns = "nsf_url")
  ),
  epa = list(
    grant_id = list(columns = "grant_id"),
    grant_id_core = list(columns = "grant_id"),
    status = list(columns = "status"),
    organization_name = list(columns = "organization"),
    organization_state = list(columns = "org_state", transform = "state"),
    organization_city = list(columns = "org_city"),
    organization_type = list(columns = "org_type"),
    project_title = list(columns = "project_title"),
    project_abstract = list(columns = "project_description"),
    start_date = list(columns = "start_date"),
    original_end_date = list(columns = "original_end_date"),
    termination_date = list(columns = "termination_date"),
    reinstatement_date = list(columns = "reinstatement_date"),
    award_value = list(columns = "award_value", transform = "numeric"),
    award_outlaid = list(columns = "award_outlaid", transform = "numeric"),
    award_remaining = list(columns = "award_remaining", transform = "numeric"),
    remaining_field = list(value = "award_remaining"),
    source_url = list(columns = "usaspending_url"),
    detail_url = list(columns = "nggs_url")
  ),
  samhsa = list(
    grant_id = list(columns = "grant_id"),
    grant_id_core = list(columns = "grant_id"),
    status = list(columns = "status"),
    organization_name = list(columns = "org_name"),
    organization_state = list(columns = "org_state", transform = "state"),
    organization_city = list(columns = "org_city"),
    organization_type = list(columns = "org_type"),
    project_title = list(columns = "title"),
    project_abstract = list(columns = "abstract"),
    start_date = list(columns = c("project_start_date", "first_award_date")),
    original_end_date = list(columns = "project_original_end_date"),
    termination_date = list(columns = "termination_date"),
    reinstatement_date = list(columns = "reinstatement_date"),
    award_value = list(columns = "award_value", transform = "numeric"),
    award_outlaid = list(columns = "award_outlaid", transform = "numeric"),
    award_remaining = list(columns = "award_remaining", transform = "numeric"),
    remaining_field = list(value = "award_remaining"),
    source_url = list(columns = "usaspending_url"),
    detail_url = list(columns = "taggs_url")
  ),
  cdc = list(
    grant_id = list(columns = "grant_id"),
    grant_id_core = list(columns = "grant_id"),
    status = list(columns = "status"),
    organization_name = list(columns = "org_name"),
    organization_state = list(columns = "org_state", transform = "state"),
    organization_city = list(columns = "org_city"),
    organization_type = list(columns = "org_type"),
    project_title = list(columns = "title"),
    project_abstract = list(value = NA_character_),
    start_date = list(columns = "project_start_date"),
    original_end_date = list(columns = "project_original_end_date"),
    termination_date = list(columns = "termination_date"),
    reinstatement_date = list(columns = "reinstatement_date"),
    award_value = list(columns = "award_value", transform = "numeric"),
    award_outlaid = list(columns = "award_outlaid", transform = "numeric"),
    award_remaining = list(columns = "award_remaining", transform = "numeric"),
    remaining_field = list(value = "award_remaining"),
    source_url = list(columns = "usaspending_url"),
    detail_url = list(columns = "taggs_url")
  )
)

pick_grant_witness_columns <- function(df, columns) {
  present <- columns[columns %in% names(df)]
  if (!length(present)) {
    return(rep(NA_character_, nrow(df)))
  }
  values <- lapply(present, function(col) as.character(df[[col]]))
  Reduce(function(x, y) dplyr::coalesce(x, y), values)
}

transform_grant_witness_field <- function(values, transform = "identity") {
  switch(
    transform,
    identity = as.character(values),
    numeric = to_num(values),
    state = abbr_to_state(values),
    stop("Unsupported Grant Witness field transform: ", transform, call. = FALSE)
  )
}

extract_grant_witness_field <- function(df, field_spec) {
  if (!is.null(field_spec$value)) {
    return(rep(field_spec$value, nrow(df)))
  }
  raw_values <- pick_grant_witness_columns(df, field_spec$columns %||% character())
  transform_grant_witness_field(raw_values, field_spec$transform %||% "identity")
}

standardize_grant_witness_rows <- function(agency, df, source_file_name = paste0(agency, "_terminations.csv")) {
  spec <- GRANT_WITNESS_STANDARDIZATION_SPECS[[agency]]
  if (is.null(spec)) {
    stop("Unsupported agency: ", agency, call. = FALSE)
  }

  tibble::tibble(
    agency = agency,
    source_file = source_file_name,
    grant_id = as.character(extract_grant_witness_field(df, spec$grant_id)),
    grant_id_core = as.character(extract_grant_witness_field(df, spec$grant_id_core)),
    status = extract_grant_witness_field(df, spec$status),
    organization_name = extract_grant_witness_field(df, spec$organization_name),
    organization_state = extract_grant_witness_field(df, spec$organization_state),
    organization_city = extract_grant_witness_field(df, spec$organization_city),
    organization_type = extract_grant_witness_field(df, spec$organization_type),
    project_title = extract_grant_witness_field(df, spec$project_title),
    project_abstract = extract_grant_witness_field(df, spec$project_abstract),
    start_date = extract_grant_witness_field(df, spec$start_date),
    original_end_date = extract_grant_witness_field(df, spec$original_end_date),
    termination_date = extract_grant_witness_field(df, spec$termination_date),
    reinstatement_date = extract_grant_witness_field(df, spec$reinstatement_date),
    award_value = extract_grant_witness_field(df, spec$award_value),
    award_outlaid = extract_grant_witness_field(df, spec$award_outlaid),
    award_remaining = extract_grant_witness_field(df, spec$award_remaining),
    remaining_field = extract_grant_witness_field(df, spec$remaining_field),
    source_url = extract_grant_witness_field(df, spec$source_url),
    detail_url = extract_grant_witness_field(df, spec$detail_url)
  )
}

maybe_download <- function(url, path, skip_download = FALSE) {
  if (skip_download && file.exists(path)) return(invisible(path))
  live_result <- tryCatch({
    message("Downloading ", basename(path), " ...")
    utils::download.file(url, destfile = path, mode = "wb", quiet = FALSE)
    TRUE
  }, error = function(e) e)

  if (!inherits(live_result, "error") && file.exists(path) && file.info(path)$size > 0) {
    return(invisible(path))
  }

  if (file.exists(path) && file.info(path)$size > 0) {
    message("Falling back to cached Grant Witness file for ", basename(path))
    return(invisible(path))
  }

  if (inherits(live_result, "error")) {
    stop("Failed to download ", basename(path), ": ", live_result$message)
  }

  stop("Grant Witness file ", basename(path), " is unavailable and no cache was found.")
}

safe_max <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}
