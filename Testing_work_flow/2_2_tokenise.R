# ══════════════════════════════════════════════════════════════════════════════
# Script:   tokenise.R
# Purpose:  Load the corpus, apply stopwords, tokenise, and stem.
#           Save the result as tokens.rds for use in plotting scripts.
#
#           This is the script you will re-run most often — add noise words
#           to the custom stopword list below and re-run. You never need to
#           go back to build_corpus.R unless your set of papers changes.
#
# Inputs:
#   corpus.rds  — extracted text from build_corpus.R
#
# Output:
#   tokens.rds  — one row per word per paper, with id, year, and stemmed word
#
# Packages: tidyverse, tidytext, SnowballC
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(tidytext)
library(SnowballC)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir    <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/testing_workflow"
corpus_in   <- file.path(root_dir, "corpus.rds")
tokens_out  <- file.path(root_dir, "tokens.rds")
# ──────────────────────────────────────────────────────────────────────────────


# ── 1. LOAD CORPUS ────────────────────────────────────────────────────────────
corpus <- read_rds(corpus_in)
cat("✓ Corpus loaded:", nrow(corpus), "papers\n")

# ── 2. STOPWORDS ──────────────────────────────────────────────────────────────
# ▶▶ ADD UNWANTED WORDS TO THIS LIST as you spot them in your plots
# then re-run this script and plot_wordfreq.R

custom_stops <- tibble(word = c(
  # --- Generic noise ---
  "figure", "table", "fig", "et", "al", "pp", "vol",
  "paper", "conference", "results", "study", "data",
  # --- Units and element symbols ---
  "mm", "ha", "dm", "kg", "cm", "mn", "fe", "ga", "nt",
  # --- Place names ---
  "wagga", "wa", "australian", "australia",
  "aust", "queensland", "western", "southern", "northern",
  # --- Document artefacts ---
  "proceedings", "ert", "lst"
  # --- ADD NEW WORDS HERE ---
))

all_stops <- bind_rows(stop_words, custom_stops)
cat("Total stopwords:", nrow(all_stops), "\n")

# ── 3. TOKENISE ───────────────────────────────────────────────────────────────
# Splits each paper's text into one word per row.

tokens <- corpus |>
  select(id, year, text) |>
  unnest_tokens(word, text)

cat("Tokens after splitting:", nrow(tokens), "\n")

# ── 4. FILTER TO LETTERS ONLY ─────────────────────────────────────────────────
# Remove numbers, punctuation fragments, anything not plain lowercase letters.

tokens <- tokens |>
  filter(str_detect(word, "^[a-z]+$"))

cat("Tokens after letter filter:", nrow(tokens), "\n")

# ── 5. REMOVE STOPWORDS ───────────────────────────────────────────────────────
tokens <- tokens |>
  anti_join(all_stops, by = "word")

cat("Tokens after stopword removal:", nrow(tokens), "\n")

# ── 6. STEM ───────────────────────────────────────────────────────────────────
# Reduce words to root form: cropping → crop, rates → rate

tokens <- tokens |>
  mutate(word = wordStem(word))

cat("✓ Stemming complete\n")
cat("  Final token count:", nrow(tokens), "\n")
cat("  Unique words:     ", n_distinct(tokens$word), "\n")

# ── 7. SAVE ───────────────────────────────────────────────────────────────────
write_rds(tokens, tokens_out)
cat("✓ Tokens saved to:", tokens_out, "\n")
