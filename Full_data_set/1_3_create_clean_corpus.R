# ══════════════════════════════════════════════════════════════════════════════
# Script:   create_clean_corpus.R
# Purpose:  Create a clean flat directory containing only one copy of each
#           readable paper — duplicates and unreadable files excluded.
#
# What it does:
#   1. Reads index_deduplicated.csv (output of detect_duplicates.R), which
#      already contains only copy_1 records.
#   2. Removes any remaining rows where title is NA (PDFs that could not be
#      read, as logged in failed_title_extractions.csv).
#   3. Copies the qualifying PDFs from papers_flat to a new clean folder,
#      keeping the standardised YYYY_NNNN.pdf filenames.
#   4. Saves a CSV record of what was copied (clean_corpus_index.csv) and a
#      CSV of what was excluded (excluded_from_corpus.csv) for audit purposes.
#
# Inputs:   index_deduplicated.csv   (root_dir)   — from detect_duplicates.R
#           papers_flat/             (output_dir)  — from build_paper_index.R
#
# Outputs:  papers_clean/            (clean_dir)   — deduplicated, readable PDFs
#           clean_corpus_index.csv   (root_dir)    — index of included papers
#           excluded_from_corpus.csv (root_dir)    — index of excluded papers
#
# Packages: tidyverse, fs
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(fs)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir   <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/Full_data_set/"
output_dir <- file.path(root_dir, "papers_flat")
clean_dir  <- file.path(root_dir, "papers_clean")
index_in   <- file.path(root_dir, "index_deduplicated.csv")
clean_out  <- file.path(root_dir, "clean_corpus_index.csv")
excl_out   <- file.path(root_dir, "excluded_from_corpus.csv")
do_copy    <- TRUE
# ──────────────────────────────────────────────────────────────────────────────
# ── 1. READ DEDUPLICATED INDEX ────────────────────────────────────────────────
index <- read_csv(index_in, show_col_types = FALSE)

cat("Read", nrow(index), "papers from deduplicated index\n")

# ── 2. SPLIT INTO INCLUDE / EXCLUDE ──────────────────────────────────────────
# Excluded: title is NA, meaning pdf_text() could not extract readable content.
# These were logged in failed_title_extractions.csv by build_paper_index.R.

included <- index |>
  filter(!is.na(title)) |>
  mutate(path_flat  = file.path(output_dir, paste0(id, ".pdf")),
         path_clean = file.path(clean_dir,  paste0(id, ".pdf")))

excluded <- index |>
  filter(is.na(title)) |>
  mutate(exclusion_reason = "title extraction failed — no readable text layer")

cat("Papers included (readable, deduplicated):", nrow(included), "\n")
cat("Papers excluded (unreadable):", nrow(excluded), "\n")

# ── 3. CHECK SOURCE FILES EXIST ───────────────────────────────────────────────
missing_src <- included |>
  filter(!file_exists(path_flat))

if (nrow(missing_src) > 0) {
  cat("\nWARNING:", nrow(missing_src),
      "source file(s) not found in papers_flat — these will be skipped:\n")
  print(select(missing_src, id, filename_original), n = Inf)
  included <- included |> filter(file_exists(path_flat))
}

# ── 4. COPY FILES TO CLEAN DIRECTORY ─────────────────────────────────────────
if (do_copy) {
  dir_create(clean_dir)
  walk2(included$path_flat, included$path_clean, \(src, dst) {
    file_copy(src, dst, overwrite = TRUE)
  })
  cat("\n✓ Copied", nrow(included), "files to:", clean_dir, "\n")
} else {
  cat("\ndo_copy = FALSE: no files copied (dry run).\n")
}

# ── 5. SAVE INDEX AND EXCLUSIONS LOG ─────────────────────────────────────────
included |>
  select(id, year, filename_original, title,
         page_count, word_count, first_author, path_original) |>
  write_csv(clean_out)

cat("✓ Clean corpus index saved to:", clean_out, "\n")

excluded |>
  select(id, year, filename_original, exclusion_reason,
         page_count, word_count, first_author, path_original) |>
  write_csv(excl_out)

cat("✓ Excluded papers log saved to:", excl_out, "\n")

cat("\nDone.\n")
cat("  Clean corpus:", nrow(included), "papers in", clean_dir, "\n")
cat("  Excluded:    ", nrow(excluded), "papers logged in", excl_out, "\n")
