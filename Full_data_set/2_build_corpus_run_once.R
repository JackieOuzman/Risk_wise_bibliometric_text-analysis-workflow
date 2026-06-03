# ══════════════════════════════════════════════════════════════════════════════
# Script:   build_corpus.R
# Purpose:  Extract text from all PDFs in papers_clean and save as corpus.rds.
#           This is the slow step — run once, then load from RDS in all
#           downstream scripts.
#
# Inputs:   clean_corpus_index.csv   (root_dir)  — from create_clean_corpus.R
#           papers_clean/            (clean_dir)  — deduplicated, readable PDFs
#
# Output:   corpus.rds               (root_dir)  — one row per paper:
#                                                   id, year, title,
#                                                   first_author, text
#
# Packages: tidyverse, pdftools
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(pdftools)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir   <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/Full_data_set/"
clean_dir  <- file.path(root_dir, "papers_clean")
index_path <- file.path(root_dir, "clean_corpus_index.csv")
corpus_out <- file.path(root_dir, "corpus.rds")
# ──────────────────────────────────────────────────────────────────────────────

# ── 1. LOAD INDEX ─────────────────────────────────────────────────────────────
index <- read_csv(index_path, show_col_types = FALSE) |>
  mutate(filepath = file.path(clean_dir, paste0(id, ".pdf")))

cat("Papers to process:", nrow(index), "\n")

# ── 2. SANITY CHECK — confirm all expected files are present ──────────────────
missing <- index |> filter(!file.exists(filepath))

if (nrow(missing) > 0) {
  cat("WARNING:", nrow(missing),
      "file(s) listed in index not found in papers_clean:\n")
  print(select(missing, id, filename_original), n = Inf)
  cat("These will be skipped.\n\n")
  index <- index |> filter(file.exists(filepath))
}

# ── 3. EXTRACT TEXT ───────────────────────────────────────────────────────────
# Each PDF is read and all pages collapsed into a single text string.
# Non-ASCII characters are transliterated to ASCII equivalents where possible
# (curly quotes, en/em dashes, degree symbols etc.) so downstream text
# processing does not trip on encoding artefacts.

extract_text <- function(path) {
  tryCatch({
    pdf_text(path) |>
      paste(collapse = " ") |>
      str_replace_all("\u2019|\u2018", "'") |>
      str_replace_all("\u201C|\u201D", '"') |>
      str_replace_all("\u2013|\u2014", "-") |>
      str_replace_all("\u00B0", " degrees") |>
      str_replace_all("\uFFFD", "")
  },
  error = function(e) {
    warning("Could not read: ", path)
    NA_character_
  })
}

cat("Extracting text from", nrow(index), "PDFs — this may take several minutes...\n")

corpus <- index |>
  select(id, year, title, first_author, filepath) |>
  mutate(text = map_chr(filepath, extract_text))

cat("✓ Text extracted:", nrow(corpus), "papers\n")
cat("  Unexpected failures:", sum(is.na(corpus$text)),
    "(should be 0 — all unreadables removed in earlier steps)\n")

# ── 4. DROP FILEPATH AND SAVE ─────────────────────────────────────────────────
# filepath is an artefact of this script — not needed downstream.

corpus <- corpus |> select(-filepath)

write_rds(corpus, corpus_out)
cat("✓ Corpus saved to:", corpus_out, "\n")
cat("  Reload in downstream scripts with: corpus <- read_rds('",
    corpus_out, "')\n", sep = "")
