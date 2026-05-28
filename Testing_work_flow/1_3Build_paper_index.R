# ══════════════════════════════════════════════════════════════════════════════
# Script:   resolve_duplicates.R
# Purpose:  Remove duplicate papers from the index, keeping one copy of each.
#           Sits between find_duplicates.R and any downstream analysis.
#
# find_duplicates.R flagged pairs of papers that appear to be the same
# manuscript. This script reads that list and decides which copy to keep,
# producing a clean index with one row per unique paper.
#
# For each duplicate pair the paper with the lower ID is kept by default
# (e.g. 1996_0001 is kept, 1996_0002 is dropped). If you want to override
# that for a specific pair, add a row to duplicates_keep.csv specifying
# which ID to keep. Only exceptions need to go in that file.
#
# Inputs:
#   paper_index.csv       — full index from build_paper_index.R
#   duplicates.csv        — pairs flagged by find_duplicates.R
#   duplicates_keep.csv   — (optional) manual overrides; columns: id_a, id_b, keep_id
#
# Output:
#   paper_index_deduped.csv — same columns as paper_index.csv, one row per
#                             unique paper. Use this in all downstream scripts.
#
# Packages: tidyverse
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir        <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/testing_workflow"
index_path      <- file.path(root_dir, "paper_index.csv")
dupes_path      <- file.path(root_dir, "duplicates.csv")
overrides_path  <- file.path(root_dir, "duplicates_keep.csv")   # optional
out_path        <- file.path(root_dir, "paper_index_deduped.csv")


# ── 1. LOAD FILES ─────────────────────────────────────────────────────────────
index <- read_csv(index_path, show_col_types = FALSE)
cat("Loaded index:", nrow(index), "papers\n")

dupes_raw <- read_csv(dupes_path, show_col_types = FALSE)
cat("Loaded duplicates file:", nrow(dupes_raw), "rows\n")

names(dupes_raw)
# ── 2. EXTRACT UNIQUE PAIRS ────────────────────────────────────────────────────
pairs <- dupes_raw |>
  distinct(id, matched_id) |>
  mutate(
    lo = if_else(id < matched_id, id, matched_id),
    hi = if_else(id < matched_id, matched_id, id)
  ) |>
  distinct(lo, hi) |>
  rename(id_a = lo, id_b = hi)

cat("Unique duplicate pairs:", nrow(pairs), "\n")

# ── 3. APPLY AUTOMATIC KEEP RULE ──────────────────────────────────────────────
pairs <- pairs |>
  mutate(keep_id = id_a,
         drop_id = id_b)

# ── 4. APPLY MANUAL OVERRIDES (if file exists) ────────────────────────────────
# Only needed if you want to keep the second copy of a pair instead of the first.
# Format of duplicates_keep.csv:
#   id_a,id_b,keep_id
#   1996_0001,1996_0002,1996_0002
# Leave this file empty or absent to use the automatic rule for all pairs.

if (file.exists(overrides_path)) {
  overrides <- read_csv(overrides_path, show_col_types = FALSE) |>
    mutate(
      lo = if_else(id_a < id_b, id_a, id_b),
      hi = if_else(id_a < id_b, id_b, id_a)
    ) |>
    select(id_a = lo, id_b = hi, keep_id) |>
    mutate(drop_id = if_else(keep_id == id_a, id_b, id_a))
  
  pairs <- pairs |>
    rows_update(overrides, by = c("id_a", "id_b"), unmatched = "ignore")
  
  cat("Applied", nrow(overrides), "manual override(s)\n")
  
} else {
  cat("No overrides file found — using automatic rule for all pairs\n")
}

# ── 5. IDENTIFY IDs TO DROP ───────────────────────────────────────────────────
ids_to_drop <- pairs |>
  pull(drop_id) |>
  unique()

cat("IDs to remove:", length(ids_to_drop), "\n")

# ── 6. FILTER INDEX ───────────────────────────────────────────────────────────
index_deduped <- index |>
  filter(!id %in% ids_to_drop)

cat("Papers remaining after deduplication:", nrow(index_deduped), "\n")

# ── 7. SAVE ───────────────────────────────────────────────────────────────────
write_csv(index_deduped, out_path)
cat("✓ Deduplicated index saved to:", out_path, "\n")


# ── 8. SUMMARY ────────────────────────────────────────────────────────────────
cat("\n── Summary ────────────────────────────────────────────\n")
cat("Papers in original index:           ", nrow(index), "\n")
cat("Duplicate pairs found:              ", nrow(pairs), "\n")
cat("Papers removed (one per pair):      ", length(ids_to_drop), "\n")
cat("Papers in deduplicated index:       ", nrow(index_deduped), "\n")

if (length(ids_to_drop) > 0) {
  cat("\nDropped IDs:\n")
  cat(paste(" ", ids_to_drop), sep = "\n")
}
