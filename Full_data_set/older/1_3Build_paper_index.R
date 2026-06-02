# ══════════════════════════════════════════════════════════════════════════════
# Script:   resolve_duplicates.R
# Purpose:  Remove duplicate papers from the index, keeping one copy of each.
#           Sits between find_duplicates.R and any downstream analysis.
#
# find_duplicates.R flagged pairs of papers that appear to be the same
# manuscript. This script reads that list and decides which copy to keep,
# producing a clean index with one row per unique paper.
#
# Uses connected-components clustering (igraph) so that groups of 3 or 4
# duplicates are handled correctly even if not all pairs were flagged by
# find_duplicates.R. Within each cluster the paper with the lowest ID is kept.
#
# For each duplicate cluster the paper with the lower ID is kept by default
# (e.g. 1996_0001 is kept, 1996_0002 is dropped). If you want to override
# that for a specific cluster, add a row to duplicates_keep.csv specifying
# which ID to keep. Only exceptions need to go in that file.
#
# Inputs:
#   paper_index.csv       — full index from build_paper_index.R
#   duplicates.csv        — pairs flagged by find_duplicates.R
#   duplicates_keep.csv   — (optional) manual overrides; columns: keep_id
#
# Output:
#   paper_index_deduped.csv — same columns as paper_index.csv, one row per
#                             unique paper. Use this in all downstream scripts.
#
# Packages: tidyverse, igraph
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(igraph)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir       <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/Full_data_set/"
index_path     <- file.path(root_dir, "paper_index.csv")
dupes_path     <- file.path(root_dir, "duplicates.csv")
overrides_path <- file.path(root_dir, "duplicates_keep.csv")   # optional
out_path       <- file.path(root_dir, "paper_index_deduped.csv")

# ── 1. LOAD FILES ─────────────────────────────────────────────────────────────
index <- read_csv(index_path, show_col_types = FALSE)
cat("Loaded index:", nrow(index), "papers\n")

dupes_raw <- read_csv(dupes_path, show_col_types = FALSE)
cat("Loaded duplicates file:", nrow(dupes_raw), "rows\n")

# ── 2. BUILD CLUSTERS VIA CONNECTED COMPONENTS ────────────────────────────────
# Each flagged pair is an edge in a graph. Connected components finds all
# groups of papers that are transitively linked — so a triple (A-B, B-C)
# forms one cluster {A, B, C} even if A-C was never directly compared.
# The lowest ID in each cluster is kept; all others are dropped.

edges <- dupes_raw |>
  dplyr::distinct(id, matched_id)

g <- graph_from_data_frame(edges, directed = FALSE,
                           vertices = data.frame(name = unique(index$id)))

membership <- components(g)$membership

dup_clusters <- tibble(
  id         = names(membership),
  cluster_id = membership
) |>
  dplyr::filter(id %in% edges$id | id %in% edges$matched_id)

# Default keep rule: lowest ID in each cluster
cluster_keepers <- dup_clusters |>
  dplyr::group_by(cluster_id) |>
  dplyr::summarise(keep_id = min(id), .groups = "drop")

cat("Unique duplicate clusters:", nrow(cluster_keepers), "\n")

# ── 3. APPLY MANUAL OVERRIDES (if file exists) ────────────────────────────────
# Only needed if you want to keep a different copy than the lowest ID.
# Format of duplicates_keep.csv — one column, one row per override:
#   keep_id
#   1996_0003
# Leave this file empty or absent to use the automatic rule for all clusters.

if (file.exists(overrides_path)) {
  overrides_raw <- read_csv(overrides_path, show_col_types = FALSE)
  
  overrides <- overrides_raw |>
    dplyr::select(keep_id) |>
    dplyr::left_join(dup_clusters, by = c("keep_id" = "id")) |>
    dplyr::filter(!is.na(cluster_id))
  
  cluster_keepers <- cluster_keepers |>
    dplyr::rows_update(dplyr::select(overrides, cluster_id, keep_id),
                       by = "cluster_id", unmatched = "ignore")
  
  cat("Applied", nrow(overrides), "manual override(s)\n")
  
} else {
  cat("No overrides file found — using automatic rule for all clusters\n")
}

# ── 4. SANITY CHECK ───────────────────────────────────────────────────────────
ids_to_drop <- dup_clusters |>
  dplyr::left_join(cluster_keepers, by = "cluster_id") |>
  dplyr::filter(id != keep_id) |>
  dplyr::pull(id) |>
  unique()

# Confirm no keeper also appears in drop list
conflicts <- intersect(cluster_keepers$keep_id, ids_to_drop)
if (length(conflicts) > 0) {
  warning("These IDs are marked as both keep and drop — check overrides:\n",
          paste(conflicts, collapse = ", "))
}

cat("IDs to remove:", length(ids_to_drop), "\n")

# ── 5. FILTER INDEX ───────────────────────────────────────────────────────────
index_deduped <- index |>
  dplyr::filter(!id %in% ids_to_drop)

cat("Papers remaining after deduplication:", nrow(index_deduped), "\n")

# ── 6. SAVE ───────────────────────────────────────────────────────────────────
write_csv(index_deduped, out_path)
cat("✓ Deduplicated index saved to:", out_path, "\n")

# ── 7. SUMMARY ────────────────────────────────────────────────────────────────
cat("\n── Summary ────────────────────────────────────────────\n")
cat("Papers in original index:           ", nrow(index), "\n")
cat("Duplicate clusters found:           ", nrow(cluster_keepers), "\n")
cat("Papers removed:                     ", length(ids_to_drop), "\n")
cat("Papers in deduplicated index:       ", nrow(index_deduped), "\n")

cat("\n── Clusters ───────────────────────────────────────────\n")
dup_clusters |>
  dplyr::left_join(cluster_keepers, by = "cluster_id") |>
  dplyr::mutate(status = dplyr::if_else(id == keep_id, "KEEP", "DROP")) |>
  dplyr::arrange(cluster_id, id) |>
  print(n = Inf)

if (length(ids_to_drop) > 0) {
  cat("\nDropped IDs:\n")
  cat(paste(" ", ids_to_drop), sep = "\n")
}


# index |> 
#   dplyr::filter(id %in% c("1996_0001","1996_0002","1996_0003","1996_0004")) |>
#   dplyr::select(id, word_count)
