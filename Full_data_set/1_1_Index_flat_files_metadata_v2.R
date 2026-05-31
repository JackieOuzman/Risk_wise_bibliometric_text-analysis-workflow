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
#        - Title:       first meaningful line(s) of page 1. Multi-line titles
#                       are handled by continuation punctuation detection and
#                       by checking whether the next line looks like an author.
#        - Page count:  total number of pages.
#        - Word count:  total word count across all pages.
#   3. Parses first author from the filename by taking the text between the
#      first and second underscore.
#   4. Assigns each paper a standardised ID (e.g. 1996_0001) and copies it
#      to papers_flat with the ID as the new filename.
#   5. Reports any PDFs where title extraction failed for manual fixing.
#   6. Saves the completed index as paper_index.csv in the root folder.
#
# Known limitations:
#   - Title extraction depends on the PDF having a text layer. Scanned PDFs
#     with no text layer will return NA and flag for manual fixing.
#   - Only the first author is captured, parsed from the filename.
#   - A small number of filenames do not follow the YYYY_Author_... convention.
#     These will need manual correction in the CSV after running.
#
# Inputs:   PDFs in year-named subfolders under root_dir
# Outputs:  Flat folder of renamed PDFs    (output_dir)
#           CSV index of metadata          (index_out)
#
# Packages: tidyverse, fs, pdftools, readxl
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(fs)
library(pdftools)
library(readxl)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir   <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/Full_data_set/"
output_dir <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/Full_data_set/papers_flat"
index_out  <- file.path(root_dir, "paper_index.csv")
do_copy    <- TRUE
# ──────────────────────────────────────────────────────────────────────────────

# ── 1. FIND PDFS ──────────────────────────────────────────────────────────────
pdf_files <- dir_ls(root_dir, recurse = TRUE, glob = "*.pdf") |>
  as.character() |>
  keep(~ str_detect(.x, "/\\d{4}/"))

cat("Found", length(pdf_files), "PDFs\n")

# ── 2. EXTRACT METADATA FROM EACH PDF ─────────────────────────────────────────
# Title extraction strategy:
#   - Transliterate common non-ASCII punctuation (curly quotes, em-dash,
#     trademark etc.) to ASCII equivalents before processing
#   - Split page 1 into lines, remove blanks, bare punctuation, and known
#     boilerplate header words (e.g. "OFFICIAL")
#   - Drop lines that are mostly encoding-replacement characters
#   - Take the first remaining line as the title
#   - Extend onto the next line if:
#       (a) line ends with continuation punctuation (:, -, ;), OR
#       (b) line doesn't end a sentence AND the next line doesn't look like
#           an author or institution line (catches plain-wrapped titles)

# Boilerplate header words/patterns to skip before selecting the title line
boilerplate_patterns <- c(
  "^official$", "^confidential$", "^draft$", "^restricted$", "^protected$",
  "^summary$", "^abstract$", "^introduction$",
  "^\\d+$",        # bare page numbers
  "^[ivxlcdm]+$",  # roman numerals
  "^https?://",    # URLs
  "^www\\.",
  "^\\s*$",        # whitespace only
  "^\\.$",         # bare full stop
  "^,$",           # bare comma
  "^;$",           # bare semicolon
  "^-$",           # bare hyphen
  "^\\?$"          # bare question mark (also catches image-only pages)
)

is_boilerplate <- function(x) {
  str_to_lower(x) |>
    map_lgl(~ any(str_detect(.x, boilerplate_patterns)))
}

# Pattern to detect author or institution lines — used to avoid appending
# these onto the title when a title wraps without continuation punctuation
author_institution_pattern <- paste(
  "university|department|institute|csiro|division|college|centre|center",
  "research|school|faculty|laboratory|ltd|pty|inc|gov\\.au|email|@",
  sep = "|"
)

is_author_or_institution <- function(x) {
  str_detect(str_to_lower(x), author_institution_pattern) |
    # "John Smith1," or "Colin McMaster1,"
    str_detect(x, "^[A-Z][a-z]+\\s[A-Z][a-z]+\\d*[,\\s]") |
    # "A.K. Abadi" or "J.F. Angus"
    str_detect(x, "^[A-Z]\\.[A-Z A-Z\\.]*[,\\d\\s]")
}

cat("Extracting metadata (this may take a few minutes)...\n")

titles      <- character(length(pdf_files))
page_counts <- integer(length(pdf_files))
word_counts <- integer(length(pdf_files))

for (i in seq_along(pdf_files)) {
  
  result <- tryCatch({
    
    pages      <- pdf_text(pdf_files[i])
    page_count <- length(pages)
    word_count <- str_count(paste(pages, collapse = " "), "\\S+")
    
    page1 <- pages[1] |>
      iconv(from = "UTF-8", to = "UTF-8", sub = "\uFFFD") |>
      # Transliterate common non-ASCII punctuation to ASCII equivalents
      # so these characters don't corrupt the extracted title
      str_replace_all("\u2019|\u2018", "'") |>   # curly apostrophes → '
      str_replace_all("\u201C|\u201D", '"') |>   # curly quotes → "
      str_replace_all("\u2013", "-") |>          # en-dash → -
      str_replace_all("\u2014", "-") |>          # em-dash → -
      str_replace_all("\u2122", "TM") |>         # ™ → TM
      str_replace_all("\u00AE", "(R)") |>        # ® → (R)
      str_replace_all("\u00B0", " degrees") |>   # ° → degrees
      str_replace_all("\uFFFD", "")              # drop anything else unreadable
    
    # Split into lines, squish whitespace, filter boilerplate and junk
    lines <- str_split(page1, "\n")[[1]] |>
      str_squish() |>
      keep(~ .x != "") |>
      keep(~ !is_boilerplate(.x)) |>
      # Drop lines where >30% of characters are replacement chars
      keep(~ str_count(.x, "\uFFFD") / max(str_length(.x), 1) < 0.3)
    
    title <- if (length(lines) > 0) lines[1] else NA_character_
    
    if (!is.na(title) && length(lines) > 1) {
      
      # (a) Continuation punctuation: title clearly continues on next line
      if (str_detect(title, "[:\\-;]$")) {
        title <- paste(title, lines[2])
        if (length(lines) > 2 && str_detect(lines[2], "[:\\-;]$")) {
          title <- paste(title, lines[3])
        }
        
        # (b) No sentence-ending punctuation and next line isn't author/institution:
        #     title has wrapped onto the next line without a continuation character
      } else if (!str_detect(title, "[.!?]$") &&
                 !is_author_or_institution(lines[2])) {
        title <- paste(title, lines[2])
      }
    }
    
    # Tidy the final title
    title <- str_squish(title)
    if (title == "") title <- NA_character_
    
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
index <- tibble(path_original = pdf_files) |>
  mutate(
    year              = path_original |> path_dir() |> path_file() |> as.integer(),
    filename_original = path_file(path_original),
    title             = titles,
    page_count        = page_counts,
    word_count        = word_counts,
    
    first_author = filename_original |>
      str_remove("\\.pdf$") |>
      str_remove("^\\d{4}_") |>
      str_extract("^[^_]+") |>
      str_squish() |>
      str_remove(" and .*$")
    
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
failed <- index |> filter(is.na(title))

if (nrow(failed) > 0) {
  cat("\nFiles where title extraction failed — fix manually in section 5:\n")
  print(select(failed, id, filename_original), n = Inf)
} else {
  cat("\nAll titles extracted successfully.\n")
}

# ── 5. MANUAL FIXES FROM EXCEL ────────────────────────────────────────────────
fixes_path       <- file.path(root_dir, "metadata_manual fix.xlsx")
manual_fixes_raw <- readxl::read_excel(fixes_path)
manual_fixes_raw <- manual_fixes_raw |>
  rename(
    id           = ID,
    first_author = first_author_new,
    title        = title_actual
  ) |>
  select(id, first_author, title)

index <- index |>
  rows_update(manual_fixes_raw, by = "id", unmatched = "ignore")

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