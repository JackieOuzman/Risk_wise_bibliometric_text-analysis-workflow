

library(tidyverse)
library(fs)  # install.packages("fs") if needed






# ── CONFIGURATION ─────────────────────────────────────────────────────────────
# Set this to your top-level folder containing the year subfolders
root_dir <- "N:/work/RiskWise/Brendan_Ag_Conf_papers"
#root_dir <- "D:/work/RiskWise/Brendan_Ag_Conf_papers"

# Where to put the flat output folder (can be inside root_dir or alongside it)
#output_dir <- file.path(root_dir, "papers_flat")
output_dir <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/papers_flat"

# Set to FALSE first to do a dry run and inspect the index before any files move
do_copy <- TRUE
# ──────────────────────────────────────────────────────────────────────────────

# 1. Find all PDFs across year subfolders
pdf_files <- dir_ls(root_dir, recurse = TRUE, glob = "*.pdf")

# 2. Build index table
index <- tibble(path_original = as.character(pdf_files)) |>
  mutate(
    year = path_original |> path_dir() |> path_file() |> as.integer(),
    filename_original = path_file(path_original),
    # Parse title from filename: strip leading "YYYY_AuthorName_" prefix
    title = filename_original |>
      str_remove("\\.pdf$") |>
      str_remove("^\\d{4}_[^_]+_") |>   # remove YYYY_Author_
      str_replace_all("_", " ") |>
      str_squish()
  ) |>
  arrange(year, filename_original) |>
  group_by(year) |>
  mutate(
    # 4-digit sequential ID within each year
    n = row_number(),
    id = sprintf("%d_%04d", year, n),
    filename_new = paste0(id, ".pdf"),
    path_new = file.path(output_dir, filename_new)
  ) |>
  ungroup() |>
  select(id, year, filename_new, filename_original, title, path_original, path_new)

# 3. Inspect before doing anything
print(index, n = 20)
cat("\nTotal papers:", nrow(index), "\n")
cat("Years covered:", paste(sort(unique(index$year)), collapse = ", "), "\n")

# 4. When happy, set do_copy <- TRUE and re-run
if (do_copy) {
  dir_create(output_dir)
  
  walk2(index$path_original, index$path_new, \(src, dst) {
    file_copy(src, dst, overwrite = FALSE)
  })
  
  # Save index CSV alongside the flat folder
  index_path <- file.path(root_dir, "paper_index.csv")
  write_csv(index |> select(-path_original, -path_new), index_path)
  
  cat("\n✓ Copied", nrow(index), "files to:", output_dir, "\n")
  cat("✓ Index saved to:", index_path, "\n")
}



# Save the index
# ── CONFIGURATION ─────────────────────────────────────────────────────────────
flat_dir  <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/papers_flat"
index_out <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/paper_index.csv"
# ──────────────────────────────────────────────────────────────────────────────

# Build index from the renamed files (YYYY_NNNN.pdf format)
index <- dir_ls(flat_dir, glob = "*.pdf") |>
  as_tibble_col(column_name = "path") |>
  mutate(
    filename_new = path_file(path),
    id           = str_remove(filename_new, "\\.pdf$"),
    year         = str_extract(id, "^\\d{4}") |> as.integer(),
    paper_num    = str_extract(id, "\\d{4}$") |> as.integer()
  ) |>
  arrange(year, paper_num) |>
  select(id, year, paper_num, filename_new)

# Save
write_csv(index, index_out)



#