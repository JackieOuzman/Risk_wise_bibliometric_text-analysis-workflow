# ══════════════════════════════════════════════════════════════════════════════
# Script:   7_risk_bigrams.R
# Purpose:  Identify two-word phrases (bigrams) containing "risk" or its
#           variants, to see WHAT KIND of risk is being discussed, and
#           whether that's shifted over time (e.g. financial risk framing
#           declining while climate risk framing rises).
#
# Why bigrams, not the stemmed tokens_clean.rds?
#   Single stemmed words lose context — "risk" alone doesn't distinguish
#   "risk management" from "climate risk" from "at risk of frost damage".
#   Bigrams are also more interpretable when NOT stemmed — "risk management"
#   reads clearly; "risk manag" doesn't add anything. So this script goes
#   back to corpus.rds (raw text) rather than tokens_clean.rds.
#
# Method:
#   1. Apply the same pre-processing substitutions as Script 3 (financial
#      amounts, chemicals) — necessary because unnest_tokens(token="ngrams")
#      strips punctuation/digits exactly like the single-word tokeniser does.
#   2. Tokenise into bigrams (adjacent word pairs) via unnest_tokens().
#   3. Remove bigrams where EITHER word is a stopword or non-letter token.
#   4. Normalise grammatical variants (risks/risky/risking -> risk,
#      higher/highest -> high, climatic -> climate, etc.) so fragmented
#      variants of the same phrase get counted together.
#   5. Filter to bigrams containing "risk" in either position.
#   6. Count per paper, aggregate to year-level prevalence (same % of
#      papers logic as Script 6), plot overall top phrases AND a faceted
#      by-year trend for the top phrases.
#
# Inputs:   corpus.rds                     (root_dir)
# Outputs:  risk_bigrams_by_paper.csv       (output_dir)
#           risk_bigrams_by_year.csv        (output_dir)
#           risk_bigrams_top.png            (output_dir) — overall top phrases
#           risk_bigrams_trend_facet.png    (output_dir) — top phrases by year
# Packages: tidyverse, tidytext
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(tidytext)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir      <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/Full_data_set/"
output_dir    <- file.path(root_dir, "outputs")
corpus_in     <- file.path(root_dir, "corpus.rds")
paper_out     <- file.path(output_dir, "risk_bigrams_by_paper.csv")
year_out      <- file.path(output_dir, "risk_bigrams_by_year.csv")
plot_out      <- file.path(output_dir, "risk_bigrams_top.png")
plot_out_facet <- file.path(output_dir, "risk_bigrams_trend_facet.png")

top_n_bigrams  <- 15   # most common risk-bigrams to show in the overall plot
top_n_facet    <- 9    # most common risk-bigrams to facet by year
min_papers     <- 5    # bigram must appear in at least this many distinct
# papers to be eligible — noise guard, same idea as Script 5b
# ──────────────────────────────────────────────────────────────────────────────

dir.create(output_dir, showWarnings = FALSE)

# ── 1. LOAD ───────────────────────────────────────────────────────────────────
corpus <- read_rds(corpus_in)
cat("Loaded", nrow(corpus), "papers\n\n")

# ── 2. PRE-PROCESS (same substitutions as Script 3) ───────────────────────────
# Copy this block from 3_tokenise_and_clean.R if it's grown since — keeping
# it identical matters less here since risk-bigrams are unlikely to involve
# these substitutions, but it avoids stray broken tokens in output.

corpus_pre <- corpus |>
  mutate(text = str_replace_all(text, "\\$\\s?[0-9,.]+\\s?[mMbBkK]?", " dollaramount "),
         text = str_replace_all(text, "\\$", " dollarsign "),
         text = str_replace_all(text, regex("\\bCO2\\b", ignore_case = TRUE), " carbondioxid "),
         text = str_replace_all(text, regex("\\bN2O\\b", ignore_case = TRUE), " nitrousoxid "))
# add any further substitutions here to match Script 3 if it has grown

# ── 3. TOKENISE INTO BIGRAMS ──────────────────────────────────────────────────
bigrams <- corpus_pre |>
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

cat("Total bigrams (pre-filter):", nrow(bigrams), "\n")

# ── 4. REMOVE STOPWORD BIGRAMS ────────────────────────────────────────────────
# Split into word1/word2, drop rows where either word is a stopword or non-
# letter token, then recombine for readability.

standard_stops <- get_stopwords()$word

bigrams_split <- bigrams |>
  separate(bigram, into = c("word1", "word2"), sep = " ", remove = FALSE) |>
  filter(
    str_detect(word1, "^[a-z]{2,}$"),
    str_detect(word2, "^[a-z]{2,}$"),
    !word1 %in% standard_stops,
    !word2 %in% standard_stops
  )

cat("Bigrams after stopword removal:", nrow(bigrams_split), "\n")

# ── 4b. NORMALISE WORD VARIANTS (before combining into bigrams) ──────────────
# Light-touch normalisation — NOT full stemming. Just enough to merge obvious
# grammatical variants that fragment the same underlying phrase.
# Extend this list as you spot more splits (e.g. "assessed"/"assessment").

norm_map <- c(
  "risks"     = "risk",
  "risky"     = "risk",
  "risking"   = "risk",
  "higher"    = "high",
  "highest"   = "high",
  "climatic"  = "climate",
  "lower"     = "low",
  "lowest"    = "low"
)

normalise_word <- function(w) if_else(w %in% names(norm_map), norm_map[w], w)

bigrams_split <- bigrams_split |>
  mutate(
    word1 = normalise_word(word1),
    word2 = normalise_word(word2)
  )

# ── 5. FILTER TO RISK-CONTAINING BIGRAMS ──────────────────────────────────────
risk_pattern <- "^risk(s|y|ing)?$"   # kept as a belt-and-braces check, though
# 4b should already have normalised these

risk_bigrams <- bigrams_split |>
  filter(str_detect(word1, risk_pattern) | str_detect(word2, risk_pattern)) |>
  mutate(bigram_clean = paste(word1, word2))

cat("Risk-containing bigrams:", nrow(risk_bigrams), "\n")
cat("Distinct risk bigram types:", n_distinct(risk_bigrams$bigram_clean), "\n\n")

# ── 6. COUNT PER PAPER ────────────────────────────────────────────────────────
bigram_paper_counts <- risk_bigrams |>
  count(id, year, bigram_clean, name = "n")

write_csv(bigram_paper_counts, paper_out)
cat("✓ Paper-level bigram counts saved to:", paper_out, "\n")

# ── 7. AGGREGATE TO YEAR ──────────────────────────────────────────────────────
# Same prevalence logic as Script 6: % of papers per year using each bigram,
# not just raw count — presence-based, not volume-based.

papers_per_year <- corpus |> count(year, name = "n_papers_total")

bigram_by_year <- bigram_paper_counts |>
  distinct(id, year, bigram_clean) |>
  count(year, bigram_clean, name = "n_papers_with_bigram") |>
  left_join(papers_per_year, by = "year") |>
  mutate(pct_papers = round(100 * n_papers_with_bigram / n_papers_total, 1))

write_csv(bigram_by_year, year_out)
cat("✓ Year-aggregated bigram prevalence saved to:", year_out, "\n")

# ── 8. TOP BIGRAMS OVERALL (across the whole corpus) ──────────────────────────
top_bigrams_overall <- bigram_paper_counts |>
  distinct(id, bigram_clean) |>
  count(bigram_clean, name = "n_papers") |>
  filter(n_papers >= min_papers) |>
  slice_max(order_by = n_papers, n = top_n_bigrams, with_ties = FALSE) |>
  mutate(bigram_clean = fct_reorder(bigram_clean, n_papers))

# ── 9. PLOT — TOP RISK BIGRAMS, WHOLE CORPUS ──────────────────────────────────
p <- ggplot(top_bigrams_overall, aes(x = n_papers, y = bigram_clean)) +
  geom_col(fill = "#2E75B6") +
  labs(
    title    = "Most common risk-related phrases — Agronomy Australia papers, 1980\u20132024",
    subtitle = paste0("Top ", top_n_bigrams, " two-word phrases containing \u201crisk\u201d; ",
                      "phrases in < ", min_papers, " papers excluded"),
    x = "Number of distinct papers using this phrase",
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "grey40")
  )

p

ggsave(plot_out, p, width = 8, height = 6, dpi = 150)
cat("✓ Plot saved to:", plot_out, "\n")

# ── 10. TOP BIGRAMS — SELECT FOR FACETING ─────────────────────────────────────
top_bigram_names <- bigram_paper_counts |>
  distinct(id, bigram_clean) |>
  count(bigram_clean, name = "n_papers") |>
  filter(n_papers >= min_papers) |>
  slice_max(order_by = n_papers, n = top_n_facet, with_ties = FALSE) |>
  pull(bigram_clean)

bigram_by_year_top <- bigram_by_year |>
  filter(bigram_clean %in% top_bigram_names) |>
  mutate(bigram_clean = factor(bigram_clean, levels = top_bigram_names))

# ── 11. PLOT — FACETED TREND LINES, TOP BIGRAMS BY YEAR ───────────────────────
p_facet <- ggplot(bigram_by_year_top, aes(x = year, y = pct_papers)) +
  geom_line(colour = "#2E75B6", linewidth = 1.5) +
  geom_point(colour = "#2E75B6", size = 1.3) +
  scale_x_continuous(breaks = seq(1980, 2024, 8)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  facet_wrap(~ bigram_clean, scales = "free_y", ncol = 3) +
  labs(
    title    = "Top risk-related phrases over time — Agronomy Australia papers",
    subtitle = "% of papers per year using each phrase (note: y-axis scales differ by panel)",
    x = NULL,
    y = "% of papers that year"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    strip.text    = element_text(size = 9, face = "bold"),
    panel.spacing = unit(1.2, "lines"),
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "grey40")
  )

p_facet

ggsave(plot_out_facet, p_facet, width = 10, height = 7, dpi = 150)
cat("✓ Faceted trend plot saved to:", plot_out_facet, "\n")

