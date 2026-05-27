# ══════════════════════════════════════════════════════════════════════════════
# Script:   find_duplicates.R
# Purpose:  Identify suspected duplicate papers in the index built by
#           build_paper_index.R.
#
# Duplicate logic (ALL three conditions must be met):
#   1. Same first author  — fuzzy match >= author_threshold
#   2. Similar title      — fuzzy match >= title_threshold
#   3. Same word count    — exact match (different word counts = different papers)
#
# Exclusion logic (ANY one condition excludes the pair):
#   - Different word counts
#   - Sufficiently different author names
#   - Sufficiently different titles
#
# Note: with a small test subset (8 papers per year) few or no duplicates
#       are expected. Run on the full dataset for meaningful results.
#
# Inputs:   paper_index.csv (from build_paper_index.R)
# Outputs:  duplicates.csv
#
# Packages: tidyverse, stringdist, scales
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(stringdist)
library(scales)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir   <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/testing_workflow"
index_path <- file.path(root_dir, "paper_index.csv")
out_path   <- file.path(root_dir, "duplicates.csv")

author_threshold <- 0.85    # fuzzy match threshold for author similarity
title_threshold  <- 0.85    # fuzzy match threshold for title similarity
# ──────────────────────────────────────────────────────────────────────────────

# ── 1. LOAD INDEX ─────────────────────────────────────────────────────────────
index <- read_csv(index_path, show_col_types = FALSE)
cat("Loaded", nrow(index), "papers\n")

# ── 2. NORMALISE COMPARISON FIELDS ────────────────────────────────────────────
# Lowercase and strip punctuation from author and title fields so that
# minor formatting differences don't block matches.
index_clean <- index |>
  mutate(
    author_clean = first_author |>
      str_to_lower() |>
      str_remove_all("[^a-z\\s]") |>
      str_squish(),
    title_clean = title |>
      str_to_lower() |>
      str_remove_all("[^a-z\\s]") |>
      str_squish()
  )

# ── 3. BUILD AND FILTER PAIRS ─────────────────────────────────────────────────
# Compare every unique pair of papers.
# Exclusion rules are applied sequentially — cheapest check first —
# so fuzzy string matching only runs on pairs that pass the word count filter.
n <- nrow(index_clean)
cat("Comparing", comma(n * (n - 1) / 2), "pairs...\n")

pairs <- combn(seq_len(n), 2) |>
  t() |>
  as_tibble(.name_repair = ~ c("i", "j")) |>
  mutate(
    id_a         = index_clean$id[i],
    id_b         = index_clean$id[j],
    word_count_a = index_clean$word_count[i],
    word_count_b = index_clean$word_count[j],
    author_a     = index_clean$author_clean[i],
    author_b     = index_clean$author_clean[j],
    title_a      = index_clean$title_clean[i],
    title_b      = index_clean$title_clean[j]
  ) |>
  
  # Exclusion rule 1: different word counts = different papers
  filter(word_count_a == word_count_b) |>
  
  # Compute similarity scores only on pairs that survive the word count filter
  mutate(
    author_sim = round(1 - stringdist(author_a, author_b, method = "jw"), 3),
    title_sim  = round(1 - stringdist(title_a,  title_b,  method = "jw"), 3)
  ) |>
  
  # Exclusion rule 2: authors too different
  filter(author_sim >= author_threshold) |>
  
  # Exclusion rule 3: titles too different
  filter(title_sim >= title_threshold) |>
  
  select(id_a, id_b, author_sim, title_sim, word_count_a)

cat("Suspected duplicate pairs found:", nrow(pairs), "\n")

# ── 4. EXPAND TO ONE ROW PER PAPER ───────────────────────────────────────────
# Output mirrors paper_index.csv format with extra columns for review.
# Each duplicate pair appears as two rows so both papers are visible
# when the CSV is sorted or filtered in Excel.
duplicates <- bind_rows(
  pairs |>
    left_join(index, by = c("id_a" = "id")) |>
    mutate(matched_id = id_b) |>
    select(id = id_a, matched_id, author_sim, title_sim,
           year, filename_original, title,
           page_count, word_count, first_author, path_original),
  pairs |>
    left_join(index, by = c("id_b" = "id")) |>
    mutate(matched_id = id_a) |>
    select(id = id_b, matched_id, author_sim, title_sim,
           year, filename_original, title,
           page_count, word_count, first_author, path_original)
) |>
  arrange(desc(author_sim), desc(title_sim), id)

# ── 5. SAVE ───────────────────────────────────────────────────────────────────
write_csv(duplicates, out_path)

cat("\n── Summary ───────────────────────────────────────────\n")
cat("Total files flagged as suspected duplicates:", nrow(duplicates), "\n")
cat("Unique pairs:                               ", nrow(pairs), "\n")
cat("✓ Saved to:", out_path, "\n")
