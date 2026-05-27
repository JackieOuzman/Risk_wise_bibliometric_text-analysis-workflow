# ══════════════════════════════════════════════════════════════════════════════
# Script:   build_paper_index.R
# Purpose:  Organise a collection of agricultural conference papers (PDFs) stored
#           in year-named subfolders into a flat folder with standardised filenames,
#           and produce a CSV index of key metadata.
#
# What it does:
#   1. Scans year-named subfolders (e.g. /1985/, /2004/) for all PDF files,
#      excluding the output folder.
#   2. Opens each PDF and extracts four metadata fields in a single read pass:
#        - Title:           the first meaningful line of page 1, skipping blank
#                           lines and bare numbers (handles cases like "15" 
#                           appearing as a superscript before "15N" in a title).
#        - Affiliation:     a raw block of 1-4 lines immediately following the
#                           title line, stopping when a line looks like body text
#                           (more than 12 words). Good enough for institution-level
#                           filtering; not cleanly split by author. May need
#                           manual tidying in the CSV for some papers.
#        - Page count:      total number of pages.
#        - Word count:      total word count across all pages.
#   3. Parses author names from the original filename (assumes naming convention
#      YYYY_Surname_...).
#   4. Assigns each paper a standardised ID (e.g. 1985_0001) and copies it to
#      a flat output folder (papers_flat) with the ID as the new filename.
#   5. Where title extraction fails (e.g. scanned/image-only or corrupted PDFs),
#      falls back to a rough title derived from the filename. Eleven known problem
#      files are flagged for manual title entry in Section 5.
#   6. Saves the completed index as a CSV (paper_index.csv) in the root folder.
#
# Known limitations:
#   - Title and affiliation extraction depends on the PDF having a text layer.
#     Scanned image-only PDFs will fail and fall back to filename-derived titles.
#   - Affiliation extraction is a heuristic and will be imperfect across the wide
#     range of formatting styles present in papers spanning several decades.
#   - Author-to-affiliation matching (e.g. via superscript numbers) is not attempted
#     as superscripts are lost in plain-text PDF extraction.
#
# Inputs:   PDFs in year-named subfolders under root_dir
# Outputs:  Flat folder of renamed PDFs (output_dir)
#           CSV index of metadata     (index_out)
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
# Title:            first meaningful line (skips blanks and bare numbers e.g. superscript "15" in 15N)
# Affiliation_raw:  the 1-4 lines immediately after the author line, before body text begins.
#                   Captured as a raw block — reliable enough for institution-level filtering
#                   but not cleanly split by author. Best cleaned manually in the CSV if needed.
# Page count:       total pages
# Word count:       total words across all pages
extract_pdf_metadata <- function(path) {
  tryCatch({
    pages      <- pdf_text(path)
    page1      <- pages[1]
    all_text   <- paste(pages, collapse = " ")
    page_count <- length(pages)
    word_count <- str_count(all_text, "\\S+")
    
    # Split page 1 into clean lines
    lines <- str_split(page1, "\n")[[1]] |> str_squish()
    
    # Title: first non-empty line that isn't a bare number (handles superscripts like "15" in 15N)
    meaningful <- lines |> discard(~ .x == "" | str_detect(.x, "^\\d{1,4}$"))
    title <- meaningful[1]
    
    # Affiliation: capture 1-4 non-empty lines after the title line, stopping when
    # a line looks like body text (i.e. more than 12 words — heuristic for a sentence)
    title_pos <- which(lines == title)[1]
    if (!is.na(title_pos) && title_pos < length(lines)) {
      post_title <- lines[(title_pos + 1):length(lines)] |> discard(~ .x == "")
      affil_lines <- character(0)
      for (ln in post_title[1:min(6, length(post_title))]) {
        word_n <- str_count(ln, "\\S+")
        if (word_n > 12) break          # looks like body text, stop
        affil_lines <- c(affil_lines, ln)
        if (length(affil_lines) == 4) break   # cap at 4 lines
      }
      affiliation_raw <- paste(affil_lines, collapse = "; ")
    } else {
      affiliation_raw <- NA_character_
    }
    
    list(
      title           = title,
      affiliation_raw = affiliation_raw,
      page_count      = page_count,
      word_count      = word_count
    )
    
  }, error = function(e) {
    warning("Could not read: ", path)
    list(
      title           = NA_character_,
      affiliation_raw = NA_character_,
      page_count      = NA_integer_,
      word_count      = NA_integer_
    )
  })
}

# ── 1. FIND PDFS ──────────────────────────────────────────────────────────────
# Collect all PDFs inside year-named subfolders (e.g. /1985/); excludes papers_flat.
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

titles_extracted     <- map_chr(metadata_list, "title")
affiliations_raw     <- map_chr(metadata_list, "affiliation_raw")
page_counts          <- map_int(metadata_list, "page_count")
word_counts          <- map_int(metadata_list, "word_count")

# ── 3. BUILD INDEX ────────────────────────────────────────────────────────────
index <- tibble(path_original = pdf_files) |>
  mutate(
    year              = path_original |> path_dir() |> path_file() |> as.integer(),
    filename_original = path_file(path_original),
    title             = titles_extracted,
    affiliation_raw   = affiliations_raw,
    page_count        = page_counts,
    word_count        = word_counts,
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
  select(id, year, filename_original, title, affiliation_raw,
         page_count, word_count, authors, path_original, path_new)

# ── 4. FALLBACK TITLES ────────────────────────────────────────────────────────
# Where title extraction failed, derive a rough placeholder from the filename.
# These 11 files could not be opened by pdftools — likely scanned image-only PDFs,
# password-protected, or corrupted. Titles must be filled in manually below.
#
#   2004_0067  2004_0070  2004_0074  2004_0088  2004_0095  2004_0099
#   2004_0113  2004_0148  2004_0154  2010_0070  2017_0103
#
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
# Open each PDF manually and paste the correct title string below. #NB I cant get these at all
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
  select(id, year, filename_original, title, affiliation_raw,
         page_count, word_count, authors, path_original) |>
  write_csv(index_out)

cat("✓ Index saved to:", index_out, "\n")
