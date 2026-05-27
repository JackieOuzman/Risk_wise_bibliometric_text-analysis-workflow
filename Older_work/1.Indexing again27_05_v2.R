# ══════════════════════════════════════════════════════════════════════════════
# Script:   build_paper_index.R
# Purpose:  Organise agricultural conference papers (PDFs) stored in year-named
#           subfolders into a flat folder with standardised filenames, and
#           produce a CSV index of key metadata.
#
# What it does:
#   1. Scans year-named subfolders (e.g. /1985/, /2004/) for all PDFs,
#      excluding the output folder.
#   2. Opens each PDF and extracts in a single read pass:
#        - Title:       first meaningful line of page 1, skipping blank lines
#                       and bare numbers (handles superscripts like "15" in 15N).
#        - Page count:  total number of pages.
#        - Word count:  total word count across all pages.
#   3. Parses first author surname only from the filename (YYYY_Surname_...).
#      Only the surname is retained — session labels and categories that follow
#      in the filename are discarded.
#   4. Assigns each paper a standardised ID (e.g. 1985_0001) and copies it to
#      a flat output folder (papers_flat) with the ID as the new filename.
#   5. Where title extraction fails (scanned/image-only or corrupted PDFs),
#      falls back to a rough title derived from the filename. Known problem
#      files are flagged for manual title entry in Section 5.
#   6. Saves the completed index as paper_index.csv in the root folder.
#
# Known limitations:
#   - Title extraction depends on the PDF having a text layer. Scanned
#     image-only PDFs will fail and fall back to filename-derived titles.
#   - Only the first author surname is captured; full author lists would
#     require PDF text extraction which is unreliable across decades of
#     varying formatting styles.
#
# Inputs:   PDFs in year-named subfolders under root_dir
# Outputs:  Flat folder of renamed PDFs    (output_dir)
#           CSV index of metadata          (index_out)
#
# Packages: tidyverse, fs, pdftools, cli
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(fs)
library(pdftools)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir   <- "N:/work/RiskWise/Brendan_Ag_Conf_papers"
output_dir <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/papers_flat"
index_out  <- file.path(root_dir, "paper_index.csv")
do_copy    <- TRUE
# ──────────────────────────────────────────────────────────────────────────────

# Extract metadata from page 1 of a PDF.
#
# Title:      first meaningful line — skips blanks and bare numbers
#             (e.g. superscript "15" appearing before "15N" in a title).
#             Output passed through iconv() to fix UTF-8 encoding issues
#             (e.g. em-dashes rendering as â€").
# Page count: total pages in the document.
# Word count: total words across all pages.
extract_pdf_metadata <- function(path) {
  tryCatch({
    pages      <- pdf_text(path)
    page1      <- pages[1] |> iconv(from = "UTF-8", to = "UTF-8", sub = "-")
    all_text   <- paste(pages, collapse = " ")
    page_count <- length(pages)
    word_count <- str_count(all_text, "\\S+")
    
    lines <- str_split(page1, "\n")[[1]] |>
      str_squish() |>
      discard(~ .x == "" | str_detect(.x, "^\\d{1,4}$"))
    
    title <- lines[1]
    
    list(
      title      = title,
      page_count = page_count,
      word_count = word_count
    )
    
  }, error = function(e) {
    warning("Could not read: ", path)
    list(
      title      = NA_character_,
      page_count = NA_integer_,
      word_count = NA_integer_
    )
  })
}

# Parse first author surname from filename.
# Filename convention: YYYY_Surname_<anything>.pdf
# Anything after the surname (session labels, categories, co-author names)
# is discarded. Handles CamelCase surnames by inserting spaces.
parse_first_author <- function(filename) {
  filename |>
    str_remove("\\.pdf$") |>
    str_remove("^\\d{4}_") |>          # remove leading year
    str_extract("^[^_]+") |>           # take only the first _ segment
    str_replace_all("([a-z])([A-Z])", "\\1 \\2") |>   # CamelCase → spaces
    str_squish()
}

# ── 1. FIND PDFS ──────────────────────────────────────────────────────────────
# Collect all PDFs inside year-named subfolders; excludes papers_flat.
pdf_files <- dir_ls(root_dir, recurse = TRUE, glob = "*.pdf") |>
  as.character() |>
  keep(~ str_detect(.x, "/\\d{4}/"))

cat("Found", length(pdf_files), "PDFs\n")

# ── 2. EXTRACT METADATA ───────────────────────────────────────────────────────
cat("Extracting metadata (this may take a few minutes)...\n")
metadata_list <- map(
  cli::cli_progress_along(pdf_files, name = "Reading PDFs"),
  ~ extract_pdf_metadata(pdf_files[.x])
)

titles_extracted <- map_chr(metadata_list, "title")
page_counts      <- map_int(metadata_list, "page_count")
word_counts      <- map_int(metadata_list, "word_count")

# ── 3. BUILD INDEX ────────────────────────────────────────────────────────────
index <- tibble(path_original = pdf_files) |>
  mutate(
    year              = path_original |> path_dir() |> path_file() |> as.integer(),
    filename_original = path_file(path_original),
    title             = titles_extracted,
    page_count        = page_counts,
    word_count        = word_counts,
    first_author      = parse_first_author(filename_original)
  ) |>
  arrange(year, filename_original) |>
  group_by(year) |>
  mutate(
    id           = sprintf("%d_%04d", year, row_number()),
    filename_new = paste0(id, ".pdf"),
    path_new     = file.path(output_dir, filename_new)
  ) |>
  ungroup() |>
  select(id, year, filename_original, title,
         page_count, word_count, first_author, path_original, path_new)

# ── 4. FALLBACK TITLES ────────────────────────────────────────────────────────
# Report files where title extraction failed BEFORE applying the fallback,
# so you can see exactly which ones need manual attention.
failed_extraction <- index |>
  filter(is.na(title)) |>
  select(id, filename_original)

if (nrow(failed_extraction) > 0) {
  cat("\nFiles where title extraction failed (will use filename fallback):\n")
  print(failed_extraction, n = Inf)
} else {
  cat("\nAll titles extracted successfully.\n")
}

# For failed files, derive a rough placeholder from the filename.
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

# ── 5. MANUAL TITLE FIXES ─────────────────────────────────────────────────────
# Open each PDF manually and paste the correct title string below.
# Add or remove rows as needed based on the fallback report above.
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

# ── 6. COPY FILES AND SAVE INDEX ──────────────────────────────────────────────
if (do_copy) {
  dir_create(output_dir)
  walk2(index$path_original, index$path_new, \(src, dst) {
    file_copy(src, dst, overwrite = TRUE)
  })
  cat("✓ Copied", nrow(index), "files to:", output_dir, "\n")
}

index |>
  select(id, year, filename_original, title,
         page_count, word_count, first_author, path_original) |>
  write_csv(index_out)

cat("✓ Index saved to:", index_out, "\n")
