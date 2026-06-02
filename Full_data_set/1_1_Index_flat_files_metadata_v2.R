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
#        - Title:       first meaningful line(s) of page 1. Line 2 is appended
#                       if line 1 ends with , : - ; or the words and/AND/&
#                       OR if line 1 ends with a Roman numeral and line 2 is
#                       a short single word (series subtitle pattern).
#        - First author: the first line after the title ends, with everything
#                       after the first comma or " and " removed, and trailing
#                       superscript digits stripped.
#        - Page count:  total number of pages.
#        - Word count:  total word count across all pages.
#   3. Falls back to filename-parsed author if PDF author extraction fails.
#   4. Assigns each paper a standardised ID (e.g. 1996_0001) and copies it
#      to papers_flat with the ID as the new filename.
#   5. Reports any PDFs where title extraction failed for manual fixing.
#   6. Saves the completed index as paper_index.csv in the root folder.
#
# Known limitations:
#   - Title and author extraction depend on the PDF having a text layer.
#   - Titles that wrap across more than 2 lines will be truncated.
#   - A small number of filenames do not follow the YYYY_Author_... convention.
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
# Title extraction:
#   - Transliterate common non-ASCII punctuation to ASCII equivalents
#   - Skip boilerplate header lines (OFFICIAL, page numbers etc.)
#   - Take first remaining line as title
#   - Append line 2 if line 1 ends with , : - ; or and/AND/& AND line 2
#     doesn't look like an address or email
#   - Also append line 2 if line 1 ends with a Roman numeral (series subtitle
#     pattern e.g. "- I" / "phosphorus") and line 2 is a short single word
#
# Author extraction:
#   - Take the first line after the title ends
#   - Strip Unicode superscript digits and inline superscripts
#   - Strip everything after the first comma or " and " to get first author only
#   - Fall back to filename-parsed author if result looks garbled

boilerplate_patterns <- c(
  "^official$", "^confidential$", "^draft$", "^restricted$", "^protected$",
  "^summary$", "^abstract$", "^introduction$",
  "^\\d+$",        # bare page numbers
  "^[ivxlcdm]+$",  # roman numerals on their own line
  "^https?://",    # URLs
  "^www\\.",
  "^\\s*$",        # whitespace only
  "^\\.$",         # bare full stop
  "^,$",           # bare comma
  "^;$",           # bare semicolon
  "^-$",           # bare hyphen
  "^\\?$"          # bare question mark
)

is_boilerplate <- function(x) {
  str_to_lower(x) |>
    map_lgl(~ any(str_detect(.x, boilerplate_patterns)))
}

cat("Extracting metadata (this may take a few minutes)...\n")

titles      <- character(length(pdf_files))
authors_pdf <- character(length(pdf_files))
page_counts <- integer(length(pdf_files))
word_counts <- integer(length(pdf_files))

for (i in seq_along(pdf_files)) {
  
  result <- tryCatch({
    
    pages      <- pdf_text(pdf_files[i])
    page_count <- length(pages)
    word_count <- str_count(paste(pages, collapse = " "), "\\S+")
    
    page1 <- pages[1] |>
      iconv(from = "UTF-8", to = "UTF-8", sub = "\uFFFD") |>
      str_replace_all("\u2019|\u2018", "'") |>   # curly apostrophes → '
      str_replace_all("\u201C|\u201D", '"') |>   # curly quotes → "
      str_replace_all("\u2013", "-") |>          # en-dash → -
      str_replace_all("\u2014", "-") |>          # em-dash → -
      str_replace_all("\u2122", "TM") |>         # ™ → TM
      str_replace_all("\u00AE", "(R)") |>        # ® → (R)
      str_replace_all("\u00B0", " degrees") |>   # ° → degrees
      str_replace_all("\uFFFD", "")              # drop anything else unreadable
    
    lines <- str_split(page1, "\n")[[1]] |>
      str_squish() |>
      keep(~ .x != "") |>
      keep(~ !is_boilerplate(.x)) |>
      keep(~ str_count(.x, "\uFFFD") / max(str_length(.x), 1) < 0.3)
    
    title          <- if (length(lines) > 0) lines[1] else NA_character_
    title_end_line <- 1L
    
    # Primary continuation: line 1 ends with , : - ; or and/AND/&
    # and line 2 doesn't look like an address or email
    if (!is.na(title) && length(lines) > 1) {
      
      line2 <- lines[2]
      
      is_continuation_punct <- str_detect(title, "[,:\\-;]$")
      is_continuation_and   <- str_detect(title, "\\band$|\\bAND$|&$")
      is_not_address        <- !str_detect(line2, "@|P\\.O\\.|PMB|GPO|Box\\s\\d|\\d{4}$")
      
      if ((is_continuation_punct || is_continuation_and) && is_not_address) {
        title          <- paste(title, line2)
        title_end_line <- 2L
      }
    }
    
    # Secondary continuation: series subtitle pattern
    # Line 1 ends with a Roman numeral (e.g. "- I", "- II") and line 2 is
    # a short single word (the subtitle e.g. "phosphorus", "molybdenum")
    if (!is.na(title) && length(lines) > 2 && title_end_line == 1L) {
      ends_roman  <- str_detect(title, "-\\s+[IVX]+$")
      single_word <- !str_detect(lines[2], "\\s")
      short_line  <- str_length(lines[2]) < 30
      if (ends_roman && single_word && short_line) {
        title          <- paste(title, lines[2])
        title_end_line <- 2L
      }
    }
    
    title <- str_squish(title)
    if (title == "") title <- NA_character_
    
    # Author: first line after the title ends
    author_line_idx <- title_end_line + 1L
    author <- if (length(lines) >= author_line_idx) {
      lines[author_line_idx] |>
        # Strip Unicode superscript digits (¹²³ etc.) anywhere in string
        str_remove_all("[\u00B9\u00B2\u00B3\u2070-\u2079\u207F]") |>
        # Strip regular digits immediately following a letter (inline superscripts)
        str_remove_all("(?<=[a-zA-Z])\\d+") |>
        # Keep only first author: drop from first comma or " and "
        str_remove(",.*$") |>
        str_remove("\\s+and\\s+.*$") |>
        str_remove("\\s+AND\\s+.*$") |>
        # Strip any remaining trailing digits or symbols
        str_remove("[\\d\\*†‡§]+$") |>
        str_squish()
    } else {
      NA_character_
    }
    
    # Discard author if garbled, too short, or looks like an institution
    institution_signals <- paste(
      "university|department|institute|csiro|division",
      "college|centre|center|research|school|faculty",
      "laboratory|ltd|pty|inc|gov|email|@",
      sep = "|"
    )
    
    if (!is.na(author) && (
      str_length(author) < 3 |
      str_detect(author, "\uFFFD|€|Â") |
      str_detect(str_to_lower(author), institution_signals)
    )) {
      author <- NA_character_
    }
    
    list(title = title, author = author,
         page_count = page_count, word_count = word_count)
    
  }, error = function(e) {
    warning("Could not read: ", pdf_files[i])
    list(title = NA_character_, author = NA_character_,
         page_count = NA_integer_, word_count = NA_integer_)
  })
  
  titles[i]      <- result$title
  authors_pdf[i] <- result$author %||% NA_character_
  page_counts[i] <- result$page_count
  word_counts[i] <- result$word_count
}

cat("Metadata extraction complete\n")
# ── 3. BUILD INDEX ────────────────────────────────────────────────────────────
# Author parsing from filename used as fallback only:
#   Filenames follow the convention: YYYY_Author Name_Rest of title.pdf
#   The author is the segment between the FIRST and SECOND underscore.
#
#   Examples:
#     1980_A. Axelsen_Improved Plant Management.pdf      → A. Axelsen
#     1980_A. D. Doyle and N. W. Forrester_Surface...   → A. D. Doyle
#     1980_Ann Petch_Crop Sequences...                   → Ann Petch

index <- tibble(path_original = pdf_files) |>
  mutate(
    year              = path_original |> path_dir() |> path_file() |> as.integer(),
    filename_original = path_file(path_original),
    title             = titles,
    page_count        = page_counts,
    word_count        = word_counts,
    author_pdf        = authors_pdf,
    
    # Filename-based author fallback
    author_filename = filename_original |>
      str_remove("\\.pdf$") |>
      str_remove("^\\d{4}_") |>
      str_extract("^[^_]+") |>
      str_squish() |>
      str_remove(" and .*$"),
    
    # Use PDF-extracted author where available, fall back to filename
    first_author = if_else(!is.na(author_pdf) & author_pdf != "",
                           author_pdf, author_filename)
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


pdf_text("N:/work/RiskWise/Brendan_Ag_Conf_papers/Full_data_set/1980/1980_D.F. Cameron_Genetic Exploitation of the Environment.pdf")[1] |>
  iconv(from = "UTF-8", to = "UTF-8", sub = "\uFFFD") |>
  strsplit("\n") |> unlist() |> trimws() |> (\(x) head(x[x != ""], 6))()
