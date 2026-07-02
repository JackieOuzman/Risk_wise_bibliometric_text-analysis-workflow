# ══════════════════════════════════════════════════════════════════════════════
# Script:   7b_risk_trigrams.R
# Purpose:  Three-word phrases containing "risk" — resolves the fragment
#           problem seen in bigrams (e.g. "risk associated" alone is
#           incomplete; "risk associated with" carries the connector word
#           that makes the phrase meaningful).
#
# Key difference from bigrams: middle-word stopwords are KEPT, not dropped.
# Connector words ("of", "to", "with", "in") in the middle position are
# often exactly what makes a trigram more informative than a bigram — e.g.
# "risk of frost" vs the incomplete bigram fragment "frost risk". Only the
# FIRST and LAST word are checked against the stopword list.
#
# Inputs:   corpus.rds                      (root_dir)
# Outputs:  risk_trigrams_by_paper.csv       (output_dir)
#           risk_trigrams_by_year.csv        (output_dir)
#           risk_trigrams_top.png            (output_dir)
# Packages: tidyverse, tidytext
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(tidytext)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir      <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/Full_data_set/"
output_dir    <- file.path(root_dir, "outputs")
corpus_in     <- file.path(root_dir, "corpus.rds")
paper_out     <- file.path(output_dir, "risk_trigrams_by_paper.csv")
year_out      <- file.path(output_dir, "risk_trigrams_by_year.csv")
plot_out      <- file.path(output_dir, "risk_trigrams_top.png")

top_n_trigrams <- 15
min_papers     <- 5   # likely to filter out MUCH more here than in bigrams —
# trigrams are sparser by nature. Consider lowering
# to 3 if the top-15 list looks too thin.
# ──────────────────────────────────────────────────────────────────────────────

dir.create(output_dir, showWarnings = FALSE)

# ── 1. LOAD ───────────────────────────────────────────────────────────────────
corpus <- read_rds(corpus_in)
cat("Loaded", nrow(corpus), "papers\n\n")

# ── 2. PRE-PROCESS (same as Script 7) ─────────────────────────────────────────
corpus_pre <- corpus |>
  mutate(text = str_replace_all(text, "\\$\\s?[0-9,.]+\\s?[mMbBkK]?", " dollaramount "),
         text = str_replace_all(text, "\\$", " dollarsign "),
         text = str_replace_all(text, regex("\\bCO2\\b", ignore_case = TRUE), " carbondioxid "),
         text = str_replace_all(text, regex("\\bN2O\\b", ignore_case = TRUE), " nitrousoxid "))

# ── 3. TOKENISE INTO TRIGRAMS ─────────────────────────────────────────────────
trigrams <- corpus_pre |>
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

cat("Total trigrams (pre-filter):", nrow(trigrams), "\n")

# ── 4. SPLIT AND FILTER — first/last word must not be a stopword ─────────────
# Middle word is DELIBERATELY not checked — connector words there are useful.

standard_stops <- get_stopwords()$word

trigrams_split <- trigrams |>
  separate(trigram, into = c("word1", "word2", "word3"), sep = " ", remove = FALSE) |>
  filter(
    str_detect(word1, "^[a-z]{2,}$"),
    str_detect(word2, "^[a-z]{2,}$"),
    str_detect(word3, "^[a-z]{2,}$"),
    !word1 %in% standard_stops,
    !word3 %in% standard_stops
    # word2 NOT filtered — connector words like "of", "to", "with" allowed here
  )

cat("Trigrams after filtering:", nrow(trigrams_split), "\n")

# ── 4b. NORMALISE WORD VARIANTS — same map as Script 7 ────────────────────────
norm_map <- c(
  "risks"     = "risk",
  "risky"     = "risk",
  "risking"   = "risk",
  "higher"    = "high",
  "highest"   = "high",
  "climatic"  = "climate",
  "lower"     = "low",
  "lowest"    = "low",
  # Newly added — verb form variants
  "reducing"  = "reduce",
  "reduces"   = "reduce",
  "reduced"   = "reduce",
  "increases" = "increase",
  "increased" = "increase",
  "increasing" = "increase",
  "minimising" = "minimise",
  "minimises"  = "minimise",
  "minimized"  = "minimise",   # US spelling variant
  "minimizing" = "minimise",
  "minimizes"  = "minimise"
)
normalise_word <- function(w) if_else(w %in% names(norm_map), norm_map[w], w)

trigrams_split <- trigrams_split |>
  mutate(
    word1 = normalise_word(word1),
    word2 = normalise_word(word2),
    word3 = normalise_word(word3)
  )

# ── 5. FILTER TO RISK-CONTAINING TRIGRAMS ─────────────────────────────────────
risk_pattern <- "^risk(s|y|ing)?$"

risk_trigrams <- trigrams_split |>
  filter(
    str_detect(word1, risk_pattern) |
      str_detect(word2, risk_pattern) |
      str_detect(word3, risk_pattern)
  ) |>
  mutate(trigram_clean = paste(word1, word2, word3))

cat("Risk-containing trigrams:", nrow(risk_trigrams), "\n")
cat("Distinct trigram types:", n_distinct(risk_trigrams$trigram_clean), "\n\n")

# ── 6. COUNT PER PAPER ─────────────────────────────────────────────────────────
trigram_paper_counts <- risk_trigrams |>
  count(id, year, trigram_clean, name = "n")

write_csv(trigram_paper_counts, paper_out)
cat("✓ Paper-level trigram counts saved to:", paper_out, "\n")

# ── 7. AGGREGATE TO YEAR ───────────────────────────────────────────────────────
papers_per_year <- corpus |> count(year, name = "n_papers_total")

trigram_by_year <- trigram_paper_counts |>
  distinct(id, year, trigram_clean) |>
  count(year, trigram_clean, name = "n_papers_with_trigram") |>
  left_join(papers_per_year, by = "year") |>
  mutate(pct_papers = round(100 * n_papers_with_trigram / n_papers_total, 1))

write_csv(trigram_by_year, year_out)
cat("✓ Year-aggregated trigram prevalence saved to:", year_out, "\n")

# ── 8. TOP TRIGRAMS OVERALL ───────────────────────────────────────────────────
n_papers_corpus <- n_distinct(corpus$id)   # total papers across the whole corpus

top_trigrams_overall <- trigram_paper_counts |>
  distinct(id, trigram_clean) |>
  count(trigram_clean, name = "n_papers") |>
  filter(n_papers >= min_papers) |>
  slice_max(order_by = n_papers, n = top_n_trigrams, with_ties = FALSE) |>
  mutate(
    trigram_clean = fct_reorder(trigram_clean, n_papers),
    label = paste0(n_papers, "/", n_papers_corpus)
  )

cat("Trigrams clearing min_papers threshold:", nrow(top_trigrams_overall), "\n")

# ── 9. PLOT ─────────────────────────────────────────────────────────────────────
p <- ggplot(top_trigrams_overall, aes(x = n_papers, y = trigram_clean)) +
  geom_col(fill = "#2E75B6") +
  geom_text(
    aes(label = label),
    hjust = -0.1, size = 3, colour = "grey30"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +  # headroom for label
  labs(
    title    = "Most common risk-related three-word phrases \u2014 Agronomy Australia papers, 1980\u20132024",
    subtitle = paste0("Top ", top_n_trigrams, " phrases containing \u201crisk\u201d; ",
                      "phrases in < ", min_papers, " papers excluded. Label shows papers with phrase / total papers in corpus."),
    x = "Number of distinct papers using this phrase",
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 8, colour = "grey40")
  )

p

ggsave(plot_out, p, width = 8, height = 6, dpi = 150)
cat("✓ Plot saved to:", plot_out, "\n")


# ── 10. TOP TRIGRAMS — SELECT FOR FACETING ────────────────────────────────────
top_n_facet <- 9   # how many trigrams to show as individual trend panels

top_trigram_names <- trigram_paper_counts |>
  distinct(id, trigram_clean) |>
  count(trigram_clean, name = "n_papers") |>
  filter(n_papers >= min_papers) |>
  slice_max(order_by = n_papers, n = top_n_facet, with_ties = FALSE) |>
  pull(trigram_clean)

trigram_by_year_top <- trigram_by_year |>
  filter(trigram_clean %in% top_trigram_names) |>
  mutate(trigram_clean = factor(trigram_clean, levels = top_trigram_names))

# ── 11b. LAST-YEAR LABEL DATA ─────────────────────────────────────────────────
# For each trigram, pull just the most recent year's row, to build a
# "n_papers_with_trigram / n_papers_total" label at the end of each line.

last_year_labels <- trigram_by_year_top |>
  group_by(trigram_clean) |>
  filter(year == max(year)) |>
  ungroup() |>
  mutate(label = paste0(n_papers_with_trigram, "/", n_papers_total))

# ── 11. PLOT — FACETED TREND LINES, TOP TRIGRAMS BY YEAR (with last-year label) ─
p_facet <- ggplot(trigram_by_year_top, aes(x = year, y = pct_papers)) +
  geom_line(colour = "#2E75B6", linewidth = 1.5) +
  geom_point(colour = "#2E75B6", size = 1.3) +
  geom_text(
    data = last_year_labels,
    aes(label = label),
    angle = 90, hjust = -0.1, vjust = 0.5,
    size = 2.6, colour = "grey30"
  ) +
  scale_x_continuous(breaks = seq(1980, 2024, 8)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     expand = expansion(mult = c(0.05, 0.25))) +  # more headroom for vertical label
  facet_wrap(~ trigram_clean, scales = "free_y", ncol = 3) +
  labs(
    title    = "Top risk-related three-word phrases over time \u2014 Agronomy Australia papers",
    subtitle = "% of papers per year using each phrase (note: y-axis scales differ by panel). Label shows most recent year's count as papers with phrase / total papers that year.",
    x = NULL,
    y = "% of papers that year"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    strip.text    = element_text(size = 8, face = "bold"),
    panel.spacing = unit(1.2, "lines"),
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 8, colour = "grey40")
  )

p_facet

ggsave(plot_out_facet, p_facet, width = 10, height = 7, dpi = 150)
cat("✓ Faceted trend plot saved to:", plot_out_facet, "\n")