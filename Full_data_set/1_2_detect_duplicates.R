# ══════════════════════════════════════════════════════════════════════════════
# Script:   detect_duplicates.R
# Purpose:  Identify duplicate papers in the paper index and produce a
#           deduplicated index keeping only one copy of each paper.
#
# What it does:
#   1. Reads paper_index.csv
#   2. Groups papers where title, word_count, year and page_count all match
#   3. Within each duplicate group ranks copies 1, 2, 3 etc. by id order
#   4. Outputs:
#        - duplicates.csv        all duplicate groups with copy ranking
#        - index_deduplicated.csv  cleaned index with only copy_1 records
#
# Inputs:   paper_index.csv in root_dir
# Outputs:  duplicates.csv          (root_dir)
#           index_deduplicated.csv  (root_dir)
#
# Packages: tidyverse
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir  <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/Full_data_set/"
index_in  <- file.path(root_dir, "paper_index.csv")
dupe_out  <- file.path(root_dir, "duplicates.csv")
clean_out <- file.path(root_dir, "index_deduplicated.csv")
# ──────────────────────────────────────────────────────────────────────────────

# ── 1. READ INDEX ─────────────────────────────────────────────────────────────
index <- read_csv(index_in, show_col_types = FALSE)

cat("Read", nrow(index), "papers from index\n")

# ── 2. IDENTIFY DUPLICATE GROUPS ─────────────────────────────────────────────
# Papers are duplicates if title, word_count, year and page_count all match.
# NAs in title are excluded from duplicate detection — these are failed
# extractions and should be handled manually via the Excel fixes sheet.

index_with_copy <- index |>
  filter(!is.na(title)) |>
  arrange(id) |>
  group_by(title, word_count, year, page_count) |>
  mutate(
    n_copies  = n(),
    copy_rank = row_number(),
    copy_label = paste0("copy_", row_number())
  ) |>
  ungroup()

# Add back the NA-title rows as copy_1 (nothing to deduplicate against)
na_titles <- index |>
  filter(is.na(title)) |>
  mutate(n_copies = 1L, copy_rank = 1L, copy_label = "copy_1")

index_with_copy <- bind_rows(index_with_copy, na_titles) |>
  arrange(id)

# ── 3. REPORT DUPLICATE GROUPS ────────────────────────────────────────────────
dupes <- index_with_copy |>
  filter(n_copies > 1) |>
  arrange(title, copy_rank) |>
  select(copy_label, n_copies, id, year, title,
         page_count, word_count, first_author, filename_original)

n_groups  <- dupes |> distinct(title, word_count, year, page_count) |> nrow()
n_removes <- dupes |> filter(copy_label != "copy_1") |> nrow()

cat("\nDuplicate groups found:", n_groups, "\n")
cat("Papers flagged for removal:", n_removes, "\n")
cat("Papers retained (copy_1):", nrow(index_with_copy) - n_removes, "\n")

# ── 4. SAVE DUPLICATES LIST ───────────────────────────────────────────────────
dupes |>
  write_csv(dupe_out)

cat("\n✓ Duplicate groups saved to:", dupe_out, "\n")

# ── 5. SAVE DEDUPLICATED INDEX ────────────────────────────────────────────────
index_with_copy |>
  filter(copy_label == "copy_1") |>
  select(id, year, filename_original, title,
         page_count, word_count, first_author, path_original) |>
  write_csv(clean_out)

cat("✓ Deduplicated index saved to:", clean_out, "\n")
cat("\nDone. Review duplicates.csv before using index_deduplicated.csv.\n")