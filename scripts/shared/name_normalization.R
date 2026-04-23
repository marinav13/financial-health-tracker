# scripts/shared/name_normalization.R
#
# Single source of truth for institution-name normalization across the
# R pipeline. Three external data sources have genuinely different name
# conventions, so each has its own distinct normalizer rather than a single
# "one size fits none" helper. All three live here so any edit forces the
# author to see the other forms side by side.
#
# Source this file via:
#
#   source(file.path(getwd(), "scripts", "shared", "name_normalization.R"))
#
# The college-cuts variant (`normalize_name_cuts`) is kept byte-for-byte in
# sync with `normalize_name()` in `scripts/import_supabase_institution_mapping.py`.
# When one changes, the other MUST change. Drift is caught on both sides:
# `tests/test_name_normalization.R` pins the R side, and
# `tests/test_import_supabase.py::test_normalize_name_cuts_shared_fixtures`
# pins the Python side. Both read the same fixtures from
# `tests/fixtures/name_normalization_cuts.json`, so any unilateral change
# fails CI.

# ---------------------------------------------------------------------------
# Accreditation data
# ---------------------------------------------------------------------------
#
# Matches accreditor website names (HLC, MSCHE, SACSCOC, WSCUC, etc.) against
# IPEDS institution names. Accreditor sources emit:
#   - Hawaiian okina/curly apostrophes that IPEDS omits
#   - Leading/trailing "The" (e.g. "Catholic University of America, The")
#   - Campus descriptors like " main campus" or " campus immersion"
#   - "Saint" / "Saint." variants where IPEDS uses "St"
#   - "University of Hawai'i, Hilo" vs IPEDS's "University of Hawaii at Hilo"
#
# Canonical output direction: `saint` -> `st` (matches IPEDS abbreviation).

normalize_name_accreditation <- function(x) {
  x |>
    as.character() |>
    stringr::str_to_lower() |>
    # Hawaiian okina/curly apostrophe variants appear in accreditor data but
    # not in IPEDS names; remove these before punctuation stripping so
    # "Hawai\u2019i" normalizes to "hawaii" rather than "hawai i". The plain
    # ASCII apostrophe is intentionally excluded: it marks English possessives
    # ("Mary's") and is handled by the [^a-z0-9 ] step below, producing
    # "mary s" to align with the shared fixture contract.
    stringr::str_replace_all("(?<=[a-z])[\u2018\u2019\u02bb\u02bc](?=[a-z])", "") |>
    # Expand ampersand to full word for matching (e.g., "A&M" -> "a and m")
    stringr::str_replace_all("&", " and ") |>
    # Abbreviate "Saint" (word boundary to match) to "St"
    stringr::str_replace_all("\\bsaint\\b", "st") |>
    # Remove all special characters except alphanumeric and space
    stringr::str_replace_all("[^a-z0-9 ]", " ") |>
    # Clean up any resulting whitespace issues
    stringr::str_squish() |>
    # Normalize leading/trailing "The" variants from accreditor names such
    # as "Catholic University of America, The" while preserving interior words.
    stringr::str_replace("^the\\s+", "") |>
    stringr::str_replace("\\s+the$", "") |>
    # IPEDS often appends campus descriptors that accreditor pages omit for
    # the same primary institution name.
    stringr::str_replace("\\s+main campus$", "") |>
    stringr::str_replace("\\s+campus immersion$", "") |>
    stringr::str_squish() |>
    # WSCUC has emitted "University of Hawai'i, Hilo"; IPEDS uses
    # "University of Hawaii at Hilo".
    stringr::str_replace_all("^university of hawaii hilo$", "university of hawaii at hilo")
}

# ---------------------------------------------------------------------------
# College-cuts data (R + Python contract)
# ---------------------------------------------------------------------------
#
# Matches names emitted by the public college-cuts API and Supabase tracker
# against IPEDS. This data source uses:
#   - Leading "The" that IPEDS often omits
#   - Leading "SUNY" (e.g. "SUNY Oswego") where IPEDS uses the verbose form
#   - "St" / "St." abbreviations where cuts sources mix both forms
#   - Trailing " main campus" suffixes
#
# Canonical output direction: `st` -> `saint`. This is the OPPOSITE direction
# from `normalize_name_accreditation`; the two name spaces never cross in a
# single join, so both directions are valid within their own pipeline.
#
# CONTRACT: This function's behavior is mirrored byte-for-byte by the Python
# `normalize_name()` in `scripts/import_supabase_institution_mapping.py`.
# When you edit one, edit the other in the same commit, and update the
# fixtures in `tests/test_name_normalization.R` if the expected output
# changes.

normalize_name_cuts <- function(x) {
  x |>
    as.character() |>
    stringr::str_to_lower() |>
    stringr::str_remove("^the +") |>
    stringr::str_remove("^suny +") |>
    stringr::str_replace_all("&", " and ") |>
    stringr::str_replace_all("[^a-z0-9 ]", " ") |>
    stringr::str_remove("\\s+main campus$") |>
    # Expand "st" abbreviation so "College of St. Scholastica" matches
    # "The College of Saint Scholastica" in IPEDS.
    stringr::str_replace_all("\\bst\\b", "saint") |>
    stringr::str_squish()
}

# ---------------------------------------------------------------------------
# Grant-witness data
# ---------------------------------------------------------------------------
#
# Matches federal grant-recipient names (USAspending, Grant Witness) against
# IPEDS. These sources come from structured federal reporting and are
# already fairly clean; the only reliable transformations are casing,
# ampersand expansion, and punctuation stripping. Applying the heavier
# accreditation- or cuts-specific rules here has historically caused false
# positives (e.g., stripping "SUNY" breaks SUNY Research Foundation).

normalize_name_grant_witness <- function(x) {
  x |>
    as.character() |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("&", " and ") |>
    stringr::str_replace_all("[^a-z0-9 ]", " ") |>
    stringr::str_squish()
}
