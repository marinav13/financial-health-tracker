# scripts/add_matching_to_csv.R
#
# PURPOSE: Add financial tracker matching to CSV cuts data
#          Run after build_college_cuts_join.R in CSV fallback mode

main <- function() {
  source(file.path(getwd(), "scripts", "shared", "utils.R"))
  
  csv_path <- file.path(
    getwd(), "data_pipelines", "college_cuts",
    "college_cuts_financial_tracker_cut_level_joined.csv"
  )
  
  if (!file.exists(csv_path)) {
    stop("Run build_college_cuts_join.R first to create: ", csv_path)
  }
  
  message("Reading: ", csv_path)
  d <- read.csv(csv_path, stringsAsFactors = FALSE, na.strings = c("NA", ""))
  
  # Load financial tracker
  ipeds <- load_ipeds_paths()
  ipeds_layout <- ipeds$ipeds_layout
  financial_path <- ipeds_layout(root = ".")$dataset_csv
  
  message("Reading financial tracker: ", financial_path)
  fin <- read.csv(financial_path, stringsAsFactors = FALSE)
  latest_year <- max(fin$year, na.rm = TRUE)
  fin_latest <- fin[fin$year == latest_year, ]
  
  # Normalize names
  norm <- function(x) {
    x <- tolower(gsub("[^a-z0-9]", " ", x))
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }
  
  d$norm_name <- norm(d$institution_name_collegecuts)
  d$norm_state <- tolower(d$institution_state_full)
  
  fin_latest$norm_name <- norm(fin_latest$institution_name)
  fin_latest$norm_state <- tolower(fin_latest$state)
  
  # Match by name + state
  cat("Matching...\n")
  matched <- FALSE
  for (i in seq_len(nrow(d))) {
    if (is.na(d$matched_unitid[i]) || d$matched_unitid[i] == "") {
      nm <- d$norm_name[i]
      st <- d$norm_state[i]
      if (!is.na(nm) && !is.na(st)) {
        idx <- which(fin_latest$norm_name == nm & fin_latest$norm_state == st)
        if (length(idx) > 0) {
          d$matched_unitid[i] <- fin_latest$unitid[idx[1]]
          d$matched_method[i] <- "normalized_name_state"
          d$in_financial_tracker[i] <- TRUE
          matched <- TRUE
        }
      }
    }
  }
  
  cat("Direct matched:", sum(!is.na(d$matched_unitid)), "out of", nrow(d), "\n")
  
  # Write output
  write.csv(d, csv_path, row.names = FALSE)
  message("Updated: ", csv_path)
  
  invisible(d)
}

if (sys.nframe() == 0) {
  main()
}