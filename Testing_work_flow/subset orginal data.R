# ══════════════════════════════════════════════════════════════════════════════
# Script:   setup_test_environment.R
# Purpose:  Create a small test dataset by copying 8 PDFs from each year folder
#           into a new testing_workflow folder that mirrors the original structure.
#
# Output structure:
#   testing_workflow/
#     1980/  (8 PDFs)
#     1982/  (8 PDFs)
#     1985/  (8 PDFs)
#     ... etc
#
# Packages: tidyverse, fs
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(fs)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir <- "N:/work/RiskWise/Brendan_Ag_Conf_papers"
test_dir <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/testing_workflow"
n_papers <- 8
# ──────────────────────────────────────────────────────────────────────────────

# Find all year folders (4-digit folder names only)
year_folders <- dir_ls(root_dir, type = "directory") |>
  keep(~ str_detect(path_file(.x), "^\\d{4}$"))

cat("Found", length(year_folders), "year folders\n")

# For each year folder, copy n_papers PDFs into the test structure
walk(year_folders, function(year_folder) {
  year <- path_file(year_folder)
  
  # Get all PDFs in this year folder
  pdfs <- dir_ls(year_folder, glob = "*.pdf")
  
  if (length(pdfs) == 0) {
    cat("  Skipping", year, "— no PDFs found\n")
    return()
  }
  
  # Take the first n_papers (or all if fewer than n_papers exist)
  selected <- head(pdfs, n_papers)
  
  # Create the matching year subfolder in test_dir
  test_year_dir <- path(test_dir, year)
  dir_create(test_year_dir)
  
  # Copy selected PDFs
  walk(selected, ~ file_copy(.x, path(test_year_dir, path_file(.x))))
  
  cat("  Copied", length(selected), "PDFs to testing_workflow/", year, "\n")
})

cat("\n✓ Test environment created at:", test_dir, "\n")
cat("  Update root_dir in your main scripts to point here before testing.\n")