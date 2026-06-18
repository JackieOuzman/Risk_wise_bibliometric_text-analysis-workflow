# ══════════════════════════════════════════════════════════════════════════════
# Script:   3_tokenise_and_clean.R
# Purpose:  Transform the raw text corpus into a clean, analysis-ready token
#           table — one stemmed word per row, with noise removed.
#
# What it does:
#   0. Pre-processes raw text to protect tokens of interest before tokenising
#   1. Loads corpus.rds (output of 2_build_corpus_run_once.R)
#   2. Tokenises: splits each paper's text into one word per row (tidytext)
#   3. Filters:   removes tokens that are not plain lowercase letters
#   4. Removes stopwords: standard English + custom domain-specific list
#   5. Stems:     reduces words to root form with SnowballC::wordStem()
#   6. Saves tokens_clean.rds for downstream scripts
#
# Inputs:   corpus.rds            (root_dir)
# Outputs:  tokens_clean.rds      (root_dir)
#           stopwords_used.csv    (root_dir)
# Packages: tidyverse, tidytext, SnowballC
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(tidytext)
library(SnowballC)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir   <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/Full_data_set/"
corpus_in  <- file.path(root_dir, "corpus.rds")
tokens_out <- file.path(root_dir, "tokens_clean.rds")
stops_out  <- file.path(root_dir, "stopwords_used.csv")
# ──────────────────────────────────────────────────────────────────────────────

# ── 1. LOAD CORPUS ────────────────────────────────────────────────────────────
corpus <- read_rds(corpus_in)
cat("Loaded corpus:", nrow(corpus), "papers\n")
cat("Year range:   ", min(corpus$year), "–", max(corpus$year), "\n\n")

# ── 1.1. PRE-PROCESSING: protect tokens of interest ────────────────────────────
# unnest_tokens() strips punctuation and digits, so tokens like $50 or CO2
# are mangled or lost before the filter even runs.
# Strategy: replace them with clean placeholder words BEFORE tokenising,
# so they survive as plain lowercase strings and can be counted like any word.

corpus <- corpus |>
  mutate(
    # Financial — order matters: catch amounts before lone $
    text = str_replace_all(text, regex("\\$[0-9,\\.]+[mMbBkK]?"), "dollaramount"),
    text = str_replace_all(text, "\\$", "dollarsign"),
    
    # Greenhouse gases and common agronomic chemicals
    text = str_replace_all(text, regex("CO2|co2"),   "carbondioxid"),
    text = str_replace_all(text, regex("N2O|n2o"),   "nitrousoxid"),
    text = str_replace_all(text, regex("H2O|h2o"),   "watermolecul"),
    text = str_replace_all(text, regex("NO3|no3"),   "nitrat"),
    text = str_replace_all(text, regex("NH4|nh4"),   "ammonium"),
    
    # Survey equipment
    text = str_replace_all(text, regex("EM38|em38"), "emsurvey"),
    
    # Australian states — dotted abbreviations and common short forms
    text = str_replace_all(text, regex("N\\.S\\.W\\.?|NSW"),            "newsouthwales"),
    text = str_replace_all(text, regex("Q\\.L\\.D\\.?|QLD|Qld"),        "queensland"),
    text = str_replace_all(text, regex("W\\.A\\.?(?!\\w)|WA(?!\\w)"),   "westernaustralia"),
    text = str_replace_all(text, regex("S\\.A\\.?(?!\\w)|SA(?!\\w)"),   "southaustralia"),
    text = str_replace_all(text, regex("V\\.I\\.C\\.?|VIC|Vic"),        "victoria"),
    text = str_replace_all(text, regex("N\\.T\\.?(?!\\w)|NT(?!\\w)"),   "northernterritory"),
    text = str_replace_all(text, regex("A\\.C\\.T\\.?|ACT(?!\\w)"),     "capitalterritory")
  )

cat("Pre-processing complete\n")

# ── 2. TOKENISE ───────────────────────────────────────────────────────────────
# unnest_tokens lowercases everything and strips punctuation.
# Pre-processing placeholders are already plain lowercase so pass through unchanged.

tokens_raw <- corpus |>
  select(id, year, text) |>
  unnest_tokens(word, text)

cat("Raw tokens:", nrow(tokens_raw), "\n")




# ── 3. FILTER: PLAIN LOWERCASE LETTERS ONLY (2+ chars) ───────────────────────
tokens_letters <- tokens_raw |>
  filter(str_detect(word, "^[a-z]{2,}$"))

cat("After letter filter:", nrow(tokens_letters), "\n")

# ── CHECK: what was removed from tokens_raw to tokens_letters ─────────────────
### for BB and RL - communicate what we have done:)

dropped <- tokens_raw |>
  filter(!word %in% tokens_letters$word) |>
  mutate(drop_reason = case_when(
    str_detect(word, "^[a-z]$")        ~ "single letter",
    str_detect(word, "^[0-9]+$")       ~ "number only",
    str_detect(word, "[0-9]")          ~ "contains digit",
    str_detect(word, "^[^a-z]")        ~ "starts with non-letter",
    TRUE                               ~ "other"
  ))

CHECK_filter_summary <- dropped |>
  count(drop_reason, sort = TRUE)

write_csv(CHECK_filter_summary,
          file.path(root_dir, "CHECK_filter_summary_with_drop_reason.csv"))

CHECK_filter_summary_examples <- dropped |>
  group_by(drop_reason) |>
  summarise(
    n_tokens  = n(),
    n_unique  = n_distinct(word),
    examples  = word |>
      table() |>
      sort(decreasing = TRUE) |>
      names() |>
      head(8) |>
      paste(collapse = ", ")
  ) |>
  arrange(desc(n_tokens))

write_csv(CHECK_filter_summary_examples,
          file.path(root_dir, "CHECK_filter_summary_examples.csv"))

CHECK_contains_digit_examples <- dropped |>
  filter(drop_reason == "contains digit") |>
  count(word, sort = TRUE) |>
  slice_head(n = 500)

write_csv(CHECK_contains_digit_examples,
          file.path(root_dir, "CHECK_contains_digit_top500.csv"))

CHECK_starts_with_non_letter_examples <- dropped |>
  filter(drop_reason == "starts with non-letter") |>
  count(word, sort = TRUE) |>
  slice_head(n = 500)

write_csv(CHECK_starts_with_non_letter_examples,
          file.path(root_dir, "CHECK_starts_with_non_letter_top500.csv"))

CHECK_other_examples <- dropped |>
  filter(drop_reason == "other") |>
  count(word, sort = TRUE) |>
  slice_head(n = 500)

write_csv(CHECK_other_examples,
          file.path(root_dir, "CHECK_other_top500.csv"))

# ── CLEAN UP ──────────────────────────────────────────────────────────────────
rm(list = ls(pattern = "^CHECK"))


# ── 4. STOPWORDS ──────────────────────────────────────────────────────────────

# 4a. Standard English stopwords (tidytext snowball list ~174 words)
data("stop_words")
standard_stops <- stop_words |>
  filter(lexicon == "snowball") |>
  pull(word)

# 4b. Custom domain-specific noise — add to this list as you iterate
custom_stops <- c(
  # Conference / publication artefacts
  "proceedings", "agronomy", "australia", "australian", "wagga", "conference",
  "society", "journal", "vol", "pp", "doi", "abstract",
  "author", "authors", "copyright", "university",
  
  # Generic academic filler
  "figure", "table", "fig", "et", "al", "eg", "ie", "cf",
  
  # Generic agronomic noise (uninformative across all years)
  "trial", "site", "treatment", "mean", "average", "result", "results",
  "study", "experiment", "data", "value", "values", "analysis",
  "significant", "significantly", "difference", "effect", "effects",
  
  # Units (multi-char — single chars already removed by letter filter)
  "ha", "mm", "kg", "cm", "ml", "mj", "mha", "kpa", "mpa",
  
  # Numeric fragments / artefacts
  "nil", "nd",
  
  # Stemming artefacts and generic filler — identified from plot review
  "use",      # stemming artefact (using/used/useful → use)
  "increas",  # ugly stem of increase/increasing
  "product",  # too generic — production/productivity collapse here
  "research", # generic filler
  "paper",    # conference artefact
  "show",     # generic filler
  "past",     # stemming artefact
  
  # Web artefacts slipping through letter filter
  "https", "http", "www", "pdf", "org", "com"
)
all_stops <- unique(c(standard_stops, custom_stops))

tokens_no_stops <- tokens_letters |>
  filter(!word %in% all_stops)

cat("After stopword removal:", nrow(tokens_no_stops), "\n")
cat("  Standard stops used:", length(standard_stops), "\n")
cat("  Custom stops used:  ", length(custom_stops), "\n")

# ── CHECK: what was removed from tokens_letters to tokens_no_stops ────────────
CHECK_step4_removed <- tokens_letters |>
  filter(!word %in% tokens_no_stops$word) |>
  count(word, sort = TRUE) |>
  mutate(stop_type = case_when(
    word %in% standard_stops ~ "standard",
    word %in% custom_stops   ~ "custom",
    TRUE                     ~ "unknown"
  ))

write_csv(CHECK_step4_removed,
          file.path(root_dir, "CHECK_step4_stopwords_removed.csv"))
  
  
# ── CLEAN UP ──────────────────────────────────────────────────────────────────
rm(list = ls(pattern = "^CHECK"))

# ── 5. STEM ───────────────────────────────────────────────────────────────────
# wordStem() is a function from the SnowballC R package.
# It implements the Porter stemming algorithm — a set of rules developed by
# Martin Porter in 1980 that systematically strips suffixes from English words
# to reduce them to a common root form (the "stem").
#
# The algorithm works through a series of rules in order, for example:
#   - remove plurals:         crops      → crop
#   - remove -ing:            cropping   → crop
#   - remove -ed:             cropped    → crop
#   - remove -tion:           irrigation → irrig
#   - remove -ment:           management → manag
#
# The goal is that word variants referring to the same concept get counted
# together — so "crop", "crops", "cropping", "cropped" all become "crop"
# and contribute to one count rather than four separate ones.
#
# Important: the Porter algorithm is rule-based, not dictionary-based.
# It does not know what words mean — it just applies suffix-stripping rules.
# This is fast and consistent but occasionally produces stems that are not
# real English words (e.g. rainfall → rainfal, management → manag).
# These "ugly stems" are harmless for counting purposes but look odd in plots
# — add them to custom_stops in step 4 if they appear in your visualisations.
#
# The language = "english" argument tells SnowballC to use the English ruleset.
# Other languages are available but not needed here.

tokens_stemmed <- tokens_no_stops |>
  mutate(word_original = word,
         word          = wordStem(word, language = "english"))

# Re-filter: stemming can create new stopword matches
tokens_clean <- tokens_stemmed |>
  filter(!word %in% all_stops)

cat("After stemming + re-filter:", nrow(tokens_clean), "\n")

# ── CHECK: full stem list with original words and counts ──────────────────────
CHECK_step5_stem_list <- tokens_stemmed |>
  group_by(word) |>
  summarise(
    n_times_in_corpus  = n(),
    n_unique_originals = n_distinct(word_original),
    original_words     = word_original |>
      unique() |>
      paste(collapse = ", ")
  ) |>
  arrange(desc(n_times_in_corpus))

write_csv(CHECK_step5_stem_list,
          file.path(root_dir, "CHECK_step5_stem_list.csv"))

# ── CHECK: words that changed during stemming ─────────────────────────────────
CHECK_step5_stemming_changes <- tokens_no_stops |>
  mutate(word_stemmed = wordStem(word, language = "english")) |>
  filter(word != word_stemmed) |>
  count(word, word_stemmed, sort = TRUE)

write_csv(CHECK_step5_stemming_changes,
          file.path(root_dir, "CHECK_step5_stemming_changes.csv"))

# ── CHECK: what was removed by the re-filter after stemming ───────────────────
CHECK_step5_refilter <- tokens_stemmed |>
  filter(word %in% all_stops) |>
  count(word, sort = TRUE)

write_csv(CHECK_step5_refilter,
          file.path(root_dir, "CHECK_step5_refilter_removed.csv"))

# ── CHECK: confirm pre-processing placeholders survived ───────────────────────
placeholders <- c("dollaramount", "dollarsign", "carbondioxid",
                  "nitrousoxid", "watermolecul", "nitrat", "ammonium",
                  "emsurvey", "newsouthwales", "queensland", "westernaustralia",
                  "southaustralia", "victoria", "northernterritory", "capitalterritory")

CHECK_placeholders <- tibble(
  placeholder = placeholders,
  survived    = placeholders %in% tokens_clean$word
)
print(CHECK_placeholders) 


# ── CLEAN UP ──────────────────────────────────────────────────────────────────
rm(dropped, list = ls(pattern = "^CHECK"))



# ── 6. SAVE ───────────────────────────────────────────────────────────────────
# tokens_clean is the main output — one row per token, with columns:
#   id            paper identifier (e.g. 1980_0001)
#   year          conference year
#   word          stemmed word (what downstream scripts use for counting)
#   word_original unstemmed word (for traceability while you get used to stemming)
#
# stopwords_used.csv is an audit log of every word removed in step 4,
# with counts and whether it came from the standard or custom list.
# Useful for sharing with BB and RL to explain what was filtered.

write_rds(tokens_clean, tokens_out)
cat("✓ tokens_clean saved to:", tokens_out, "\n")
cat("  Rows:   ", nrow(tokens_clean), "\n")
cat("  Columns:", paste(names(tokens_clean), collapse = ", "), "\n")

# Stopwords audit log
stopwords_log <- tibble(word = all_stops) |>
  left_join(
    tokens_letters |> count(word, name = "n_in_corpus"),
    by = "word"
  ) |>
  mutate(
    n_in_corpus = replace_na(n_in_corpus, 0),
    stop_type   = case_when(
      word %in% standard_stops ~ "standard",
      word %in% custom_stops   ~ "custom"
    )
  ) |>
  arrange(stop_type, desc(n_in_corpus))

write_csv(stopwords_log, stops_out)
cat("✓ Stopwords audit log saved to:", stops_out, "\n")

cat("\nDone. Load tokens in downstream scripts with:\n")
cat("  tokens <- read_rds('", tokens_out, "')\n", sep = "")

