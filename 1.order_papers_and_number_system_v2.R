library(tidyverse)
library(fs)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir   <- "N:/work/RiskWise/Brendan_Ag_Conf_papers"
output_dir <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/papers_flat"
index_out  <- file.path(root_dir, "paper_index.csv")
do_copy    <- TRUE
# ──────────────────────────────────────────────────────────────────────────────

# 1. Find all PDFs across year subfolders
pdf_files <- dir_ls(root_dir, recurse = TRUE, glob = "*.pdf")

# 2. Build index table
index <- tibble(path_original = as.character(pdf_files)) |>
  mutate(
    year              = path_original |> path_dir() |> path_file() |> as.integer(),
    filename_original = path_file(path_original),
    # Parse title: strip leading "YYYY_AuthorName_" prefix
    title = filename_original |>
      str_remove("\\.pdf$") |>
      str_remove("^\\d{4}_[^_]+_") |>
      str_replace_all("_", " ") |>
      str_squish(),
    # Parse author: second segment of filename (YYYY_Author_Title.pdf)
    authors = filename_original |>
      str_remove("\\.pdf$") |>
      str_extract("^\\d{4}_([^_]+)") |>
      str_remove("^\\d{4}_") |>
      str_replace_all("([a-z])([A-Z])", "\\1 \\2")  # split CamelCase if needed
  ) |>
  arrange(year, filename_original) |>
  group_by(year) |>
  mutate(
    id           = sprintf("%d_%04d", year, row_number()),
    filename_new = paste0(id, ".pdf"),
    path_new     = file.path(output_dir, filename_new)
  ) |>
  ungroup() |>
  select(id, year, filename_original, title, authors, path_original, path_new)

# 3. Inspect
print(index, n = 20)
cat("\nTotal papers:", nrow(index), "\n")
cat("Years covered:", paste(sort(unique(index$year)), collapse = ", "), "\n")

# 4. Copy files and save index
if (do_copy) {
  dir_create(output_dir)
  
  walk2(index$path_original, index$path_new, \(src, dst) {
    file_copy(src, dst, overwrite = TRUE)
  })
  
  # Save full index (keep path_original for reference, drop internal path_new)
  index |>
    select(id, year, filename_original, title, authors, path_original) |>
    write_csv(index_out)
  
  cat("\n✓ Copied", nrow(index), "files to:", output_dir, "\n")
  cat("✓ Index saved to:", index_out, "\n")
}
