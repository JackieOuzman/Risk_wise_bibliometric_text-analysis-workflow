# ══════════════════════════════════════════════════════════════════════════════
# Script:   build_paper_index.R
# Purpose:  Organise agricultural conference papers (PDFs) stored in year-named
#           subfolders into a flat folder with standardised filenames, and
#           produce a CSV index of key metadata.
#
# What it does:
#   1. Scans year-named subfolders (e.g. /1985/, /2004/) for all PDFs,
#      excluding the papers_flat output folder.
#   2. Opens each PDF and extracts:
#        - Title:       first meaningful line(s) of page 1. Where the first
#                       line ends with a colon, the next line is appended as
#                       the title continues across two lines.
#        - Page count:  total number of pages.
#        - Word count:  total word count across all pages.
#   3. Parses first author from the filename by taking the text between the
#      first and second underscore. This is the most reliable approach given
#      the consistent filename convention: YYYY_Author_RestOfFilename.pdf
#   4. Assigns each paper a standardised ID (e.g. 1996_0001) and copies it
#      to papers_flat with the ID as the new filename.
#   5. Reports any PDFs where title extraction failed for manual fixing.
#   6. Saves the completed index as paper_index.csv in the root folder.
#
# Known limitations:
#   - Title extraction depends on the PDF having a text layer.
#   - Only the first author is captured, parsed from the filename.
#   - A small number of filenames do not follow the YYYY_Author_... convention
#     (e.g. where the filename starts with a title fragment like A CASE STUDY).
#     These will need manual correction in the CSV after running.
#
# Inputs:   PDFs in year-named subfolders under root_dir
# Outputs:  Flat folder of renamed PDFs    (output_dir)
#           CSV index of metadata          (index_out)
#
# Packages: tidyverse, fs, pdftools
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(fs)
library(pdftools)
library(readxl)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir   <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/"
output_dir <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/papers_flat"
index_out  <- file.path(root_dir, "paper_index.csv")
do_copy    <- TRUE
# ──────────────────────────────────────────────────────────────────────────────

# ── 1. FIND PDFS ──────────────────────────────────────────────────────────────
pdf_files <- dir_ls(root_dir, recurse = TRUE, glob = "*.pdf") |>
  as.character() |>
  keep(~ str_detect(.x, "/\\d{4}/"))

cat("Found", length(pdf_files), "PDFs\n")

# ── 2. EXTRACT METADATA FROM EACH PDF ─────────────────────────────────────────
# Title extraction:
#   - Split page 1 into lines, remove blanks and bare numbers
#   - Take the first meaningful line as the title
#   - If that line ends with a colon, append the next line because the
#     title continues e.g.:
#     "FIELD APPLICATION OF VULPIA PHYTOTOXICITY MANAGEMENT:"
#     "A CASE STUDY"
#     becomes: "FIELD APPLICATION OF VULPIA PHYTOTOXICITY MANAGEMENT: A CASE STUDY"
#
# Note: discard() was found to behave unexpectedly in testing so direct
# logical indexing is used instead.

cat("Extracting metadata (this may take a few minutes)...\n")

titles      <- character(length(pdf_files))
page_counts <- integer(length(pdf_files))
word_counts <- integer(length(pdf_files))

for (i in seq_along(pdf_files)) {
  
  result <- tryCatch({
    
    pages      <- pdf_text(pdf_files[i])
    page1      <- pages[1] |> iconv(from = "UTF-8", to = "UTF-8", sub = "-")
    all_text   <- paste(pages, collapse = " ")
    page_count <- length(pages)
    word_count <- str_count(all_text, "\\S+")
    
    # Split page 1 into lines, remove blanks and bare numbers
    lines     <- str_split(page1, "\n")[[1]] |> str_squish()
    is_empty  <- lines == ""
    is_number <- str_detect(lines, "^\\d{1,4}$")
    lines     <- lines[!(is_empty | is_number)]
    
    # First remaining line is the title
    title <- if (length(lines) > 0) lines[1] else NA_character_
    
    # If the title line ends with a colon, the title continues on the next line
    if (!is.na(title) && str_ends(title, ":") && length(lines) > 1) {
      title <- paste(title, lines[2])
    }
    
    list(title = title, page_count = page_count, word_count = word_count)
    
  }, error = function(e) {
    warning("Could not read: ", pdf_files[i])
    list(title = NA_character_, page_count = NA_integer_, word_count = NA_integer_)
  })
  
  titles[i]      <- result$title
  page_counts[i] <- result$page_count
  word_counts[i] <- result$word_count
}

cat("Metadata extraction complete\n")

# ── 3. BUILD INDEX ────────────────────────────────────────────────────────────
# Author parsing from filename:
#   Filenames follow the convention: YYYY_Author Name_Rest of title.pdf
#   The author is always the segment between the FIRST and SECOND underscore.
#   This is extracted by:
#     1. Removing the .pdf extension
#     2. Removing the leading YYYY_ prefix
#     3. Taking everything up to the next underscore
#
#   Examples:
#     1980_A. Axelsen_Improved Plant Management.pdf      → A. Axelsen
#     1980_A. D. Doyle and N. W. Forrester_Surface...   → A. D. Doyle and N. W. Forrester
#     1980_Ann Petch_Crop Sequences...                   → Ann Petch
#
#   Known exception: some 1996 filenames start with a title fragment
#   rather than an author (e.g. 1996_A CASE STUDY_Poster Papers.pdf).
#   These will show the title fragment as the author and need manual
#   correction in the CSV — they are a small minority.

index <- tibble(path_original = pdf_files) |>
  mutate(
    year              = path_original |> path_dir() |> path_file() |> as.integer(),
    filename_original = path_file(path_original),
    title             = titles,
    page_count        = page_counts,
    word_count        = word_counts,
    
    # Extract author: text between first and second underscore
    first_author = filename_original |>
      str_remove("\\.pdf$") |>          # drop .pdf
      str_remove("^\\d{4}_") |>         # drop YYYY_
      str_extract("^[^_]+") |>          # take everything up to the next _
      str_squish() |>
      str_remove(" and .*$")            # keep only first author, drop " and ..."
    
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

# ── 4. REPORT FAILED TITLE EXTRACTIONS ───────────────────────────────────────
# Show any files where pdftools could not extract a title.
# These will need to be fixed manually in section 5.
failed <- index |> filter(is.na(title))

if (nrow(failed) > 0) {
  cat("\nFiles where title extraction failed — fix manually in section 5:\n")
  print(select(failed, id, filename_original), n = Inf)
} else {
  cat("\nAll titles extracted successfully.\n")
}


# ── 5. MANUAL TITLE FIXES ─────────────────────────────────────────────────────
# For any file listed in section 4, open the PDF and paste the correct
# title below. Remove comment markers and fill in the title string.
# Leave this block empty if section 4 reported no failures.
# JACKIE - this could be replaced with a file that looks up the replacements

# ── 5. MANUAL FIXES FROM EXCEL ────────────────────────────────────────────────
# Reads metadata_manual fix.xlsx from the root folder.
# Columns expected: ID, first_author_new, title_actual
# "no change" (any case) in a cell means that field is left as-is.
# Either fix column can be blank/NA — only the populated fields are applied.

fixes_path <- file.path(root_dir, "metadata_manual fix.xlsx")

if (file.exists(fixes_path)) {
  
  manual_fixes_raw <- readxl::read_excel(fixes_path) |>
    rename(
      id            = ID,
      first_author  = first_author_new,
      title         = title_actual
    ) |>
    # Treat "no change" (case-insensitive) and blank/NA as "do not update"
    mutate(
      first_author = if_else(
        is.na(first_author) | str_to_lower(str_squish(first_author)) == "no change",
        NA_character_, str_squish(first_author)
      ),
      title = if_else(
        is.na(title) | str_to_lower(str_squish(title)) == "no change",
        NA_character_, str_squish(title)
      )
    ) |>
    # Drop rows where both fixes are NA (nothing to do)
    filter(!is.na(first_author) | !is.na(title))
  
  # Apply author fixes
  author_fixes <- manual_fixes_raw |>
    filter(!is.na(first_author)) |>
    select(id, first_author)
  
  if (nrow(author_fixes) > 0) {
    index <- index |> rows_update(author_fixes, by = "id")
    cat("✓ Applied", nrow(author_fixes), "author fix(es) from Excel\n")
  }
  
  # Apply title fixes
  title_fixes <- manual_fixes_raw |>
    filter(!is.na(title)) |>
    select(id, title)
  
  if (nrow(title_fixes) > 0) {
    index <- index |> rows_update(title_fixes, by = "id")
    cat("✓ Applied", nrow(title_fixes), "title fix(es) from Excel\n")
  }
  
} else {
  cat("No manual fixes file found at:", fixes_path, "\n")
}
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
