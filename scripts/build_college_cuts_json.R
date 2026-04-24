# scripts/build_college_cuts_json.R
#
# PURPOSE: Rebuild college_cuts.json from the joined CSV data.
#          Simplified version that doesn't depend on build_web_exports.R.

main <- function() {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  
  cuts_path <- file.path(
    getwd(), "data_pipelines", "college_cuts",
    "college_cuts_financial_tracker_cut_level_joined.csv"
  )
  
  if (!file.exists(cuts_path)) {
    stop("Run build_college_cuts_join.R first to create: ", cuts_path)
  }
  
  message("Reading: ", cuts_path)
  d <- read.csv(cuts_path, stringsAsFactors = FALSE, na.strings = c("NA", ""))
  
  # Parse dates
  d$ann_date <- as.Date(d$announcement_date)
  
  # Sort by date descending
  d <- d[order(d$ann_date, decreasing = TRUE), ]
  
  # Take top 25
  recent <- head(d, 25)
  
  make_export_id <- function(uid, name, state) {
    if (is.na(uid) || uid == "") {
      paste0("cut-", tolower(gsub("[^a-z0-9]+", "-", paste(name, state, sep = " "))))
    } else {
      as.character(uid)
    }
  }
  
  output <- list(
    generated_at = as.character(Sys.Date()),
    recent = vector("list", nrow(recent))
  )
  
  for (i in seq_len(nrow(recent))) {
    row <- recent[i, ]
    positions <- if (!is.na(row$faculty_affected)) {
      row$faculty_affected
    } else if (!is.na(row$students_affected)) {
      row$students_affected
    } else {
      NA
    }
    
    output$recent[[i]] <- list(
      unitid = make_export_id(row$matched_unitid, row$institution_name_collegecuts, row$institution_state_abbr),
      financial_unitid = ifelse(!is.na(row$matched_unitid) && row$matched_unitid != "", row$matched_unitid, NA),
      institution_name = row$institution_name_collegecuts,
      city = row$institution_city,
      state = row$institution_state_full,
      control_label = ifelse(is.na(row$institution_control) || row$institution_control == "", "Unknown", row$institution_control),
      category = NA,
      is_primary_tracker = FALSE,
      has_financial_profile = !is.na(row$matched_unitid) && row$matched_unitid != "",
      announcement_date = as.character(row$ann_date),
      announcement_year = substr(as.character(row$ann_date), 1, 4),
      program_name = row$program_name,
      cut_type = row$cut_type,
      status = row$status,
      effective_term = row$effective_term,
      positions_affected = positions,
      notes = row$notes,
      source_url = row$source_url,
      source_title = row$source_title,
      source_publication = row$source_publication
    )
  }
  
  json_path <- file.path(getwd(), "data", "college_cuts.json")
  jsonlite::write_json(output, json_path, auto_unbox = TRUE, pretty = TRUE)
  message("Wrote: ", json_path)
  message("Recent cuts: ", length(output$recent))
  
  cat("\nFirst 10:\n")
  for (i in seq_len(min(10, length(output$recent)))) {
    cat("  ", i, ": ", output$recent[[i]]$institution_name, "\n", sep = "")
  }
  
  invisible(output)
}

if (sys.nframe() == 0) {
  main()
}
