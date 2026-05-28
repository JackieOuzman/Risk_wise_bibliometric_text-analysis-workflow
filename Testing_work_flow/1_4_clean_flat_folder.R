# ══════════════════════════════════════════════════════════════════════════════
# Script:   clean_flat_folder.R
# Purpose:  Remove duplicate PDFs from papers_flat so the folder matches
#           the deduplicated index exactly.
#
# The original files are safe in their year-named folders so it is fine
# to delete from papers_flat. This script only deletes files whose ID
# does not appear in paper_index_deduped.csv.
#
# Inputs:
#   paper_index_deduped.csv  — the deduplicated index from resolve_duplicates.R
#   papers_flat/             — the flat folder to clean
#
# Output:
#   papers_flat/ with duplicate PDFs removed
#
# Packages: tidyverse, fs
# ══════════════════════════════════════════════════════════════════════════════


library(tidyverse)
library(fs)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir    <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/testing_workflow"
flat_dir    <- file.path(root_dir, "papers_flat")
index_path  <- file.path(root_dir, "paper_index_deduped.csv")
# ──────────────────────────────────────────────────────────────────────────────


# ── 1. LOAD DEDUPED INDEX ─────────────────────────────────────────────────────
index <- read_csv(index_path, show_col_types = FALSE)
cat("Papers in deduped index:", nrow(index), "\n")

# ── 2. SCAN FLAT FOLDER ───────────────────────────────────────────────────────
flat_files <- dir_ls(flat_dir, glob = "*.pdf") |>
  as.character() |>
  tibble(path = _) |>
  mutate(
    filename = path_file(path),
    id       = str_remove(filename, "\\.pdf$")
  )

cat("PDFs currently in flat folder:", nrow(flat_files), "\n")


# ── 3. IDENTIFY FILES TO DELETE ───────────────────────────────────────────────
to_delete <- flat_files |>
  filter(!id %in% index$id)

cat("Files to delete:", nrow(to_delete), "\n")
cat("\nFiles that will be deleted:\n")
print(to_delete$filename)

# ── 4. DELETE FILES ───────────────────────────────────────────────────────────
file_delete(to_delete$path)
cat("✓ Deleted", nrow(to_delete), "duplicate PDFs from flat folder\n")

# ── 5. VERIFY ─────────────────────────────────────────────────────────────────
remaining <- dir_ls(flat_dir, glob = "*.pdf") |> length()
cat("PDFs remaining in flat folder:", remaining, "\n")
cat("Papers in deduped index:      ", nrow(index), "\n")

if (remaining == nrow(index)) {
  cat("✓ Flat folder and deduped index are in sync\n")
} else {
  cat("⚠ Warning: counts do not match — check manually\n")
}
