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
#        - Title:       first meaningful line of page 1, skipping blank lines
#                       and bare numbers (handles superscripts like "15" in 15N).
#        - Page count:  total number of pages.
#        - Word count:  total word count across all pages.
#   3. Builds an index table with a standardised ID, parsed first author from
#      the filename, and all extracted metadata.
#   4. Reports any PDFs where title extraction failed.
#   5. Allows manual title fixes for failed extractions.
#   6. Copies all PDFs to a flat output folder with standardised filenames,
#      and saves the index as paper_index.csv.
#
# Known limitations:
#   - Title extraction depends on the PDF having a text layer. Scanned
#     image-only PDFs will fail and return NA.
#   - Only the first author is captured, parsed from the filename.
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

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir   <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/testing_workflow"
output_dir <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/testing_workflow/papers_flat"
index_out  <- file.path(root_dir, "paper_index.csv")
do_copy    <- TRUE
# ──────────────────────────────────────────────────────────────────────────────

# ── 1. FIND PDFS ──────────────────────────────────────────────────────────────
# Collect all PDFs inside year-named subfolders; excludes papers_flat.
pdf_files <- dir_ls(root_dir, recurse = TRUE, glob = "*.pdf") |>
  as.character() |>
  keep(~ str_detect(.x, "/\\d{4}/"))

cat("Found", length(pdf_files), "PDFs\n")

# ── 2. EXTRACT METADATA FROM EACH PDF ─────────────────────────────────────────
# Read each PDF once and extract title, page count, and word count.
#
# Title extraction approach:
#   - Read page 1 as plain text using pdf_text()
#   - Fix encoding issues e.g. em-dash showing as â€" using iconv()
#   - Split into individual lines on newline characters
#   - Remove blank lines and lines that are only short numbers
#     (handles superscripts like "15" appearing before "15N" in a title)
#   - Take the first remaining line as the title
#
# Note: discard() was found to behave unexpectedly in testing so direct
# logical indexing is used instead — it is clearer to read anyway.

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
    
    # Split into lines, remove blanks and bare numbers
    lines     <- str_split(page1, "\n")[[1]] |> str_squish()
    is_empty  <- lines == ""
    is_number <- str_detect(lines, "^\\d{1,4}$")
    lines     <- lines[!(is_empty | is_number)]
    
    # First remaining line is the title
    title <- if (length(lines) > 0) lines[1] else NA_character_
    
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
# First author is parsed from the filename only — not from the PDF text.
# This avoids problems with colons or special characters in titles
# affecting author extraction.
#
# Filename convention: YYYY_Author Name_Session Label.pdf
# The session label is always the last underscore-separated segment.
# Strip the year prefix and the last segment to get the author name.

index <- tibble(path_original = pdf_files) |>
  mutate(
    year              = path_original |> path_dir() |> path_file() |> as.integer(),
    filename_original = path_file(path_original),
    title             = titles,
    page_count        = page_counts,
    word_count        = word_counts,
    first_author      = filename_original |>
      str_remove("\\.pdf$") |>
      str_remove("^\\d{4}_") |>                        # drop leading YYYY_
      str_replace("_[^_]+$", "") |>                    # drop last _SessionLabel
      str_replace_all("([a-z])([A-Z])", "\\1 \\2") |>  # CamelCase → spaces
      str_squish()
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

# Give failed files a placeholder so the CSV is not left with bare NAs
index <- index |>
  mutate(
    title = if_else(
      is.na(title),
      paste("TITLE NEEDED —", filename_original |>
              str_remove("\\.pdf$") |>
              str_remove("^\\d{4}_")),
      title
    )
  )

# ── 5. MANUAL TITLE FIXES ─────────────────────────────────────────────────────
# For any file listed in section 4, open the PDF and paste the correct
# title below. Remove the comment markers (#) and fill in the title string.
# Leave this block empty if section 4 reported no failures.
manual_fixes <- tribble(
  ~id,          ~title
  # "2004_0067",  "correct title here",
  # "2004_0070",  "correct title here"
)

if (nrow(manual_fixes) > 0) {
  index <- index |> rows_update(manual_fixes, by = "id")
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
