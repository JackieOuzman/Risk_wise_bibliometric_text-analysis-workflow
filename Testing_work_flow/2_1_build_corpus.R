# ══════════════════════════════════════════════════════════════════════════════
# Script:   build_corpus.R
# Purpose:  Extract text from all PDFs in papers_flat and save as corpus.rds.
#           This is the slow step —**** run once ****, then load from RDS in all
#           downstream scripts.
#
# Inputs:
#   paper_index_deduped.csv  — list of papers to process
#   papers_flat/             — folder containing the PDFs
#
# Output:
#   corpus.rds  — one row per paper with id, year, title, first_author, text
#
# Packages: tidyverse, fs, pdftools
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(fs)
library(pdftools)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir    <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/testing_workflow"
flat_dir    <- file.path(root_dir, "papers_flat")
index_path  <- file.path(root_dir, "paper_index_deduped.csv")
corpus_out  <- file.path(root_dir, "corpus.rds")
# ──────────────────────────────────────────────────────────────────────────────

# ── 1. LOAD INDEX ─────────────────────────────────────────────────────────────
index <- read_csv(index_path, show_col_types = FALSE) |>
  mutate(filepath = file.path(flat_dir, paste0(id, ".pdf")))

cat("Papers to process:", nrow(index), "\n")

# ── 2. EXTRACT TEXT ───────────────────────────────────────────────────────────
# This is the slow step — may take a few minutes for large collections.
# Each PDF is read and all pages collapsed into a single text string.

extract_text <- function(path) {
  tryCatch(
    pdf_text(path) |> paste(collapse = " "),
    error = function(e) {
      warning("Could not read: ", path)
      NA_character_
    }
  )
}

cat("Extracting text from PDFs...\n")

corpus <- index |>
  select(id, year, title, first_author, filepath) |>
  mutate(text = map_chr(filepath, extract_text))

cat("✓ Text extracted:", nrow(corpus), "papers\n")
cat("  Failed extractions:", sum(is.na(corpus$text)), "\n")

# ── 3. SAVE CORPUS ────────────────────────────────────────────────────────────
write_rds(corpus, corpus_out)
cat("✓ Corpus saved to:", corpus_out, "\n")
