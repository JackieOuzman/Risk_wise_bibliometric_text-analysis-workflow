library(tidyverse)
library(fs)
library(pdftools)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir   <- "N:/work/RiskWise/Brendan_Ag_Conf_papers"
output_dir <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/papers_flat"
index_out  <- file.path(root_dir, "paper_index.csv")
do_copy    <- TRUE
# ──────────────────────────────────────────────────────────────────────────────

# Title extractor: first non-empty line of page 1
extract_title <- function(path) {
  tryCatch({
    txt <- pdf_text(path)[1]
    lines <- str_split(txt, "\n")[[1]] |> str_squish() |> discard(~ .x == "")
    lines[1]
  }, error = function(e) {
    warning("Could not extract title from: ", path)
    NA_character_
  })
}

# 1. Find all PDFs in year subfolders only (exclude papers_flat)
pdf_files <- dir_ls(root_dir, recurse = TRUE, glob = "*.pdf") |>
  as.character() |>
  keep(~ str_detect(.x, "/\\d{4}/"))  # only files inside a YYYY folder

cat("Found", length(pdf_files), "PDFs\n")

# 2. Extract titles with progress bar
cat("Extracting titles from PDFs (this will take a few minutes)...\n")
titles_extracted <- map_chr(
  cli::cli_progress_along(pdf_files, name = "Reading PDFs"),
  ~ extract_title(pdf_files[.x])
)

# 3. Build index
index <- tibble(path_original = pdf_files) |>
  mutate(
    year              = path_original |> path_dir() |> path_file() |> as.integer(),
    filename_original = path_file(path_original),
    title             = titles_extracted,
    authors           = filename_original |>
      str_remove("\\.pdf$") |>
      str_extract("^\\d{4}_([^_]+)") |>
      str_remove("^\\d{4}_") |>
      str_replace_all("([a-z])([A-Z])", "\\1 \\2")
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

# 4. Inspect
print(index, n = 20)
cat("\nTotal papers:", nrow(index), "\n")
cat("Years covered:", paste(sort(unique(index$year)), collapse = ", "), "\n")
cat("NAs in title:", sum(is.na(index$title)), "\n")

# Check which files failed to extract a title
index |>
  filter(is.na(title)) |>
  select(id, year, filename_original) |>
  print(n = Inf)

# Fill NA titles with filename-parsed version as fallback
index <- index |>
  mutate(
    title = if_else(
      is.na(title),
      filename_original |>
        str_remove("\\.pdf$") |>
        str_remove("^\\d{4}_[^_]+_") |>
        str_replace_all("_", " ") |>
        str_squish(),
      title
    )
  )

# Confirm no more NAs
cat("NAs remaining:", sum(is.na(index$title)), "\n")

# Check the 11 fallback titles look reasonable
index |>
  filter(id %in% c("2004_0067", "2004_0070", "2004_0074", "2004_0088",
                   "2004_0095", "2004_0099", "2004_0113", "2004_0148",
                   "2004_0154", "2010_0070", "2017_0103")) |>
  select(id, filename_original, title)


#As expected — the fallback titles are just the session labels ("Water", "Prediction", "Climate change")
#which aren't great but at least they're not blank. These 11 can be fixed manually in the CSV later.

# 5. Copy files and save index
if (do_copy) {
  dir_create(output_dir)
  
  walk2(index$path_original, index$path_new, \(src, dst) {
    file_copy(src, dst, overwrite = TRUE)
  })
  
  index |>
    select(id, year, filename_original, title, authors, path_original) |>
    write_csv(index_out)
  
  cat("\n✓ Copied", nrow(index), "files to:", output_dir, "\n")
  cat("✓ Index saved to:", index_out, "\n")
}

index |>
  select(id, year, filename_original, title, authors, path_original) |>
  write_csv(index_out)

cat("✓ Index saved to:", index_out, "\n")

###################################################################################

# Manually specify correct titles for the 11 problem files I cant open these??
manual_fixes <- tribble(
  ~id,          ~title,
  "2004_0067",  "paste correct title here",
  "2004_0070",  "paste correct title here",
  "2004_0074",  "paste correct title here",
  "2004_0088",  "paste correct title here",
  "2004_0095",  "paste correct title here",
  "2004_0099",  "paste correct title here",
  "2004_0113",  "paste correct title here",
  "2004_0148",  "paste correct title here",
  "2004_0154",  "paste correct title here",
  "2010_0070",  "paste correct title here",
  "2017_0103",  "paste correct title here"
)

index <- index |>
  rows_update(manual_fixes, by = "id")

# Re-save
index |>
  select(id, year, filename_original, title, authors, path_original) |>
  write_csv(index_out)

