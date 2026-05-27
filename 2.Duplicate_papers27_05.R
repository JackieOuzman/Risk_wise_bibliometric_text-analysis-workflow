# ══════════════════════════════════════════════════════════════════════════════
# Script:   find_duplicates.R
# Purpose:  Identify suspected duplicate papers in the index built by
#           build_paper_index.R, using a combination of author and title
#           similarity. Results are saved as a CSV in the same format as
#           paper_index.csv for manual review and later use in refining
#           the paper database.
#
# What it does:
#   1. Loads paper_index.csv produced by build_paper_index.R.
#   2. Compares every pair of papers on two criteria:
#        - Author similarity:  fuzzy match on the authors field
#        - Title similarity:   fuzzy match using string distance (Jaro-Winkler),
#                              reported as a 0-1 similarity score.
#   3. Flags pairs that meet BOTH of two thresholds:
#        - High threshold (~90%):  near-certain duplicates (reprints, identical
#                                  submissions)
#        - Moderate threshold (~70%): possible duplicates (reworded titles,
#                                  slightly different author spellings)
#   4. Saves one CSV per threshold so you can compare and decide which
#      level of matching suits your data.
#
# Inputs:   paper_index.csv (from build_paper_index.R)
# Outputs:  duplicates_strict.csv   — high confidence (>=90% similarity)
#           duplicates_moderate.csv — broader net     (>=70% similarity)
#
# Packages: tidyverse, stringdist
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(stringdist)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir       <- "N:/work/RiskWise/Brendan_Ag_Conf_papers"
index_path     <- file.path(root_dir, "paper_index.csv")
out_strict     <- file.path(root_dir, "duplicates_strict.csv")      # >= 90% match
out_moderate   <- file.path(root_dir, "duplicates_moderate.csv")    # >= 70% match

threshold_strict   <- 0.90
threshold_moderate <- 0.70
# ──────────────────────────────────────────────────────────────────────────────

# ── 1. LOAD INDEX ─────────────────────────────────────────────────────────────
index <- read_csv(index_path, show_col_types = FALSE)
cat("Loaded", nrow(index), "papers\n")

# ── 2. PREPARE CLEAN COMPARISON FIELDS ───────────────────────────────────────
# Normalise both fields to lowercase with punctuation removed so that
# minor formatting differences don't block matches.
index_clean <- index |>
  mutate(
    author_clean = authors |>
      str_to_lower() |>
      str_remove_all("[^a-z\\s]") |>
      str_squish(),
    title_clean = title |>
      str_to_lower() |>
      str_remove_all("[^a-z\\s]") |>
      str_squish()
  )

# ── 3. COMPARE ALL PAIRS ──────────────────────────────────────────────────────
# Build every unique pair of papers (i vs j, i < j) and compute similarity.
# Jaro-Winkler is used: it handles minor spelling differences and transpositions
# well, which suits author name variants and slightly reworded titles.
# Note: this is an O(n²) comparison — fine for a few thousand papers but will
# slow noticeably above ~5,000.

n <- nrow(index_clean)
cat("Comparing", scales::comma(n * (n - 1) / 2), "pairs...\n")

pairs <- combn(seq_len(n), 2) |>
  t() |>
  as_tibble(.name_repair = ~ c("i", "j")) |>
  mutate(
    author_sim = 1 - stringdist(
      index_clean$author_clean[i],
      index_clean$author_clean[j],
      method = "jw"
    ),
    title_sim = 1 - stringdist(
      index_clean$title_clean[i],
      index_clean$title_clean[j],
      method = "jw"
    ),
    id_a = index_clean$id[i],
    id_b = index_clean$id[j]
  ) |>
  select(id_a, id_b, author_sim, title_sim) |>
  mutate(across(c(author_sim, title_sim), ~ round(.x, 3)))


# ── 4. APPLY THRESHOLDS AND BUILD OUTPUT ──────────────────────────────────────
# A suspected duplicate must exceed the threshold on BOTH author AND title.
# Output rows are in the same format as paper_index.csv, with one row per paper
# in the pair plus columns showing the matched partner and similarity scores.

format_duplicates <- function(pairs_filtered, index) {
  bind_rows(
    pairs_filtered |>
      left_join(index, by = c("id_a" = "id")) |>
      mutate(matched_id = id_b) |>
      select(id = id_a, matched_id, author_sim, title_sim,
             year, filename_original, title, affiliation_raw,
             page_count, word_count, authors, path_original),
    pairs_filtered |>
      left_join(index, by = c("id_b" = "id")) |>
      mutate(matched_id = id_a) |>
      select(id = id_b, matched_id, author_sim, title_sim,
             year, filename_original, title, affiliation_raw,
             page_count, word_count, authors, path_original)
  ) |>
    arrange(author_sim |> desc(), title_sim |> desc(), id)
}

# Strict (>= 90%)
strict_pairs <- pairs |>
  filter(author_sim >= threshold_strict & title_sim >= threshold_strict)

cat("Strict duplicates found (>=90%):", nrow(strict_pairs), "pairs\n")

format_duplicates(strict_pairs, index) |>
  write_csv(out_strict)

cat("✓ Saved:", out_strict, "\n")

# Moderate (>= 70%)
moderate_pairs <- pairs |>
  filter(author_sim >= threshold_moderate & title_sim >= threshold_moderate)

cat("Moderate duplicates found (>=70%):", nrow(moderate_pairs), "pairs\n")

format_duplicates(moderate_pairs, index) |>
  write_csv(out_moderate)

cat("✓ Saved:", out_moderate, "\n")
