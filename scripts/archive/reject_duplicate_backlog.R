################################################################################
# scripts/archive/reject_duplicate_backlog.R
#
# Archived on 2026-06-17: one-time duplicate-review cleanup utility.
#
# One-time bulk rejection of confirmed duplicate rows in editorial_overrides.csv.
# Reads the current CSV, sets review_status = "reject" for each listed action_id
# that is currently "unreviewed", and writes the file back.
#
# Usage: Rscript --vanilla scripts/archive/reject_duplicate_backlog.R
################################################################################

if (!exists("root", inherits = TRUE)) {
  root <- getwd()
}

overrides_path <- file.path(root, "data_pipelines", "accreditation", "editorial_overrides.csv")

rejections <- c(
  fd767daed895 = "Duplicate of 6e6015a02c89 (Wheeling University removed – same event different date)",
  db137b76da4a = "Duplicate of 758ee6a03954 (Christian Brothers University removed Dec 2025 – same event different source)",
  b626e611aeda = "Duplicate of 74e06ea70450 (Guilford College removed Dec 2025 – same event different source)",
  `5269634bb3a2` = "Duplicate of caf264231cce (Kentucky State University probation Dec 2025 – same event different source)",
  afc6dc9ca759 = "Duplicate of 2ea6206d057c (Virginia Union University removed Dec 2025 – same event different source)",
  `721e6701af33` = "Duplicate of 8caaafbc40d2 (Jacksonville University removed Dec 2025 – same event different source)",
  `3127c8298583` = "Duplicate of a804846da635 (Kentucky State University warning Dec 2023 – same date same event)",
  `1e4e182ff73a` = "Duplicate of 6dc1167ad291 (Montreat College removed Dec 2021 – same event different source)",
  `37f9dbe03aaf` = "Duplicate of d663171e07dd (Bethel University removed Dec 2022 – same event different source)",
  `127ddc22c423` = "Duplicate of fd3cef281ee0 (University of Mount Olive removed Dec 2022 – same event different source)",
  `81a95ed83a54` = "Duplicate of b68f3e1a3099 (Georgetown College warning Dec 2023 – same event different source)",
  `4883a81f02bb` = "Duplicate of 570969f24783 (Saint Leo University probation Dec 2023 – same event different source)",
  `7e1cb222a494` = "Duplicate of 1b76b63fa896 (South University-Savannah removed Dec 2024 – same event different source)",
  `654a3a6862cd` = "Duplicate of b7dbaec120ed (Christian Brothers University probation Dec 2024 – same event different source)",
  `9364ef338aa5` = "Duplicate of 484e4f3def7a (Georgetown College probation Dec 2024 – same event different source)",
  c4abae94859e = "Duplicate of a8efbc9e0fe1 (Guilford College probation Dec 2024 – same event different source)",
  `8adf8b006315` = "Duplicate of 354e0258bf5d (Kentucky State University warning Dec 2024 – same event different source)",
  `348de3df0b5b` = "Duplicate of 3b3bebfd2328 (Saint Augustine's University adverse Dec 2024 – same event different source)",
  `21666b6a6bd7` = "Duplicate of 32839e229621 (Virginia Union University probation Dec 2024 – same event different source)",
  eb8ebc79ff7c = "Duplicate of 8b2d0c9de397 (Jacksonville University warning Dec 2024 – same event same date)",
  `706f768ff970` = "Duplicate of 2de44f53a784 (University of Lynchburg warning Dec 2024 – same event same date)",
  `8b79f1d0743a` = "Duplicate of 7b8c172240e8 (Blue Mountain Christian University warning Dec 2025 – same event different source)",
  c52be840c6ed = "Duplicate of 7a541c15d76e (Greensboro College warning Dec 2025 – same event different source)",
  `379dd24fc0dd` = "Duplicate of c1cdc56bec0b (Union Commonwealth University warning Dec 2025 – same event different source)",
  `2f9849421887` = "Duplicate of 504211cbf59c (Anna Maria College adverse Apr 2026 – same event different source)",
  `6187c1d1e96a` = "Duplicate of d3ccb64fbc98 (Hampshire College adverse Apr 2026 – same event same date)",
  `74fbc2e51cd4` = "Duplicate of 3714d6a69f71 (Hampshire College show cause Mar 2026 – same event different date)",
  `0759d6dbd6ff` = "Duplicate of 37ff6ce5f765 (Hellenic College probation removed 2021 – same event different date)",
  `053383962a0c` = "Duplicate of b2e43c95a6d1 (University of Hawaii at Hilo removed Jun 2025 – same event different source)",
  ce54343dc025 = "Duplicate of 56b15b6f908f (Woodbury University removed Jun 2025 – same event different source)",
  `2c7a2c362d01` = "Duplicate of 96e697639a3a (CSU East Bay notice Jun 2025 – same event different source)",
  `1df5bd7692a2` = "Duplicate of cf1cbb96ae25 (Golden Gate University notice Jun 2025 – same event different source)",
  `480dc09da3ba` = "Duplicate of 7150f3eee439 (California College of ASU warning Jun 2025 – same event different source)",
  f7490b8666ee = "Duplicate of 21d12f8f4485 (Woodbury University notice Jun 2024 – same event different source)",
  dbb4268ea126 = "Duplicate of f1e78c4125da (San Diego Christian College warning Jun 2024 – same event different source)",
  ddf2f0d159ab = "Duplicate of f1f2a0e20937 (University of Hawaii at Hilo notice Jun 2024 – same event different source)",
  `71b750e7e83e` = "Duplicate of 07befde64f93 (Providence Christian College probation Feb 2025 – same event different source)",
  `22bb11f5597d` = "Duplicate of 972ac69eaf2c (Bethune-Cookman University removed Sep 2020 – same event different source)",
  a740e2772c4c = "Duplicate of 8e14c3aa2d59 (St. Thomas University removed Jun 2021 – same event different date)",
  `27b0419f6c9d` = "Duplicate of 284762020a1f (Saint Augustine's University warning Jun 2022 – same event different date)",
  `11f3fab3167d` = "Duplicate of 4fff062d7bdb (Saint Augustine's University probation Dec 2022 – same event different source)",
  d905f0e88821 = "HLC institution-page status-scrape of already-captured event (de6a198c3733 Wittenberg probation Nov 2025)",
  `18d5ed0a7f7b` = "HLC institution-page status-scrape of already-captured event (cfb6a0358bc4 Saint Mary-of-the-Woods warning Jun 2025)"
)

overrides <- readr::read_csv(
  overrides_path,
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character(), grandfathered = readr::col_logical())
)

rejection_ids <- names(rejections)
n_applied <- 0L
n_already_reviewed <- 0L
n_not_found <- 0L

for (action_id in rejection_ids) {
  idx <- which(overrides$action_id == action_id)
  if (!length(idx)) {
    message(sprintf("SKIP (not found): %s", action_id))
    n_not_found <- n_not_found + 1L
    next
  }
  current_status <- overrides$review_status[[idx[[1]]]]
  if (!is.na(current_status) && current_status != "unreviewed") {
    message(sprintf("SKIP (already %s): %s", current_status, action_id))
    n_already_reviewed <- n_already_reviewed + 1L
    next
  }
  overrides$review_status[[idx[[1]]]] <- "reject"
  overrides$reviewer[[idx[[1]]]]      <- "MV"
  overrides$reviewer_notes[[idx[[1]]]] <- rejections[[action_id]]
  overrides$reviewed_at[[idx[[1]]]]   <- "2026-06-05"
  message(sprintf("REJECTED: %s", action_id))
  n_applied <- n_applied + 1L
}

message(sprintf(
  "\nSummary: %d rejected, %d skipped (already reviewed), %d not found",
  n_applied, n_already_reviewed, n_not_found
))

readr::write_csv(overrides, overrides_path)
message(sprintf("Wrote %d rows to %s", nrow(overrides), overrides_path))
