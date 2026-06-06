################################################################################
# scripts/add_manual_overrides.R
# One-time: adds two rows to editorial_overrides.csv
#   1. Appalachian State SACSCOC June 2019 monitoring (unreviewed manual)
#   2. Alabama State SACSCOC May 2026 "gratitude" boilerplate (reject)
# Run once, then delete this file.
################################################################################

if (!exists("root", inherits = TRUE)) root <- getwd()
overrides_path <- file.path(root, "data_pipelines", "accreditation", "editorial_overrides.csv")

overrides <- readr::read_csv(
  overrides_path,
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character(), grandfathered = readr::col_logical())
)

# ── 1. Appalachian State ─────────────────────────────────────────────────────
# “ = left double curly quote, ” = right double curly quote
asc_label <- paste0(
  "Belle Wheclan's email of July 3, 2018, institutions were prompted to select a baseline ",
  "completion indicator (IPEDS, Clearinghouse, or SACSOC Profile data) from which a baseline ",
  "would be established and then tracked by institutions over time, and that these ",
  "“data will be used during the reaffirmation process for each institution to determine if ",
  "progress is being made in improving student completion” ",
  "as part of the institution's response to Core Requirement 8.1 (Student achievement)."
)

asc_row <- data.frame(
  action_id                    = "98c76811cdee",
  source_unitid                = "197869",
  source_institution_name      = "Appalachian State University",
  source_accreditor            = "SACSCOC",
  source_action_date           = "2019-06-13",
  source_action_type           = "monitoring",
  source_action_label_raw      = asc_label,
  source_generated_statement   = "Required to track and report on student completion progress (Core Requirement 8.1)",
  source_source_url            = "https://ope.ed.gov/dapip/#/institution-profile/133049",
  source_source_title          = "DAPIP Institutional Accreditation Action",
  source_row_origin            = "manual",
  override_unitid              = NA_character_,
  override_institution_name    = NA_character_,
  override_accreditor          = NA_character_,
  override_action_date         = NA_character_,
  override_action_type         = NA_character_,
  override_action_label_raw    = NA_character_,
  override_generated_statement = NA_character_,
  override_source_url          = NA_character_,
  override_source_title        = NA_character_,
  first_seen                   = "2026-06-06",
  review_status                = "unreviewed",
  reviewer                     = NA_character_,
  reviewer_notes               = NA_character_,
  reviewed_at                  = NA_character_,
  grandfathered                = FALSE,
  stringsAsFactors             = FALSE
)

# ── 2. Alabama State (reject DAPIP boilerplate extraction) ───────────────────
asu_label <- paste0(
  "We extend our sincere gratitude for your continued dedication and support ",
  "of the accreditation process."
)

asu_row <- data.frame(
  action_id                    = "b0d8ae1241fb",
  source_unitid                = "100724",
  source_institution_name      = "Alabama State University",
  source_accreditor            = "SACSCOC",
  source_action_date           = "2026-05-12",
  source_action_type           = "notice",
  source_action_label_raw      = asu_label,
  source_generated_statement   = asu_label,
  source_source_url            = "https://ope.ed.gov/dapip/#/institution-profile/100724",
  source_source_title          = "DAPIP Institutional Accreditation Action",
  source_row_origin            = "scraper",
  override_unitid              = NA_character_,
  override_institution_name    = NA_character_,
  override_accreditor          = NA_character_,
  override_action_date         = NA_character_,
  override_action_type         = NA_character_,
  override_action_label_raw    = NA_character_,
  override_generated_statement = NA_character_,
  override_source_url          = NA_character_,
  override_source_title        = NA_character_,
  first_seen                   = "2026-06-06",
  review_status                = "reject",
  reviewer                     = "MV",
  reviewer_notes               = "DAPIP PDF extraction error: boilerplate form letter acknowledgment, not an accreditation action",
  reviewed_at                  = "2026-06-06",
  grandfathered                = FALSE,
  stringsAsFactors             = FALSE
)

# ── Append only rows not already present ─────────────────────────────────────
existing_ids <- overrides$action_id
new_rows <- dplyr::bind_rows(
  if (!"98c76811cdee" %in% existing_ids) asc_row else NULL,
  if (!"b0d8ae1241fb" %in% existing_ids) asu_row else NULL
)

for (id in c("98c76811cdee", "b0d8ae1241fb")) {
  if (id %in% existing_ids) message(sprintf("SKIP (already present): %s", id))
  else                       message(sprintf("Adding: %s", id))
}

if (!nrow(new_rows)) {
  message("Nothing to add.")
  quit(status = 0)
}

updated <- dplyr::bind_rows(overrides, new_rows)
readr::write_csv(updated, overrides_path, na = "")
message(sprintf("Done. %d rows in overrides (%d added).", nrow(updated), nrow(new_rows)))
