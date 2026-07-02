# ══════════════════════════════════════════════════════════════════════════════
# Script:   5b_tfidf_paper_level.R
# Purpose:  Identify words DISTINCTIVE to each conference year using TF-IDF
#           computed at the PAPER level (not year level), then aggregated up
#           to year. More statistically sound than year-as-document TF-IDF —
#           see notes below.
#
# Why paper-level rather than year-level (5_tfidf_by_year.R)?
#   Year-as-document TF-IDF has only ~24 "documents" (one per conference
#   year), so IDF is very coarse — a word appearing in just 1 of 24 years
#   scores close to maximum IDF almost automatically, regardless of whether
#   it's a real thematic term or noise (e.g. a citation surname that only
#   appears in one paper's reference list that year).
#
#   Treating each PAPER as a document (~3,000 documents) gives IDF a much
#   finer-grained, more standard basis: a word is only "rare" if it's
#   genuinely rare across thousands of papers, not just absent from 23 other
#   years. Aggregating to year afterward, with a minimum-papers-per-year
#   guard, keeps a single paper's idiosyncratic vocabulary (reference lists,
#   one-off terminology) from dominating a year's results.
#
# Method:
#   1. Treat each paper (id) as a document; compute TF-IDF per (id, word)
#      using tidytext::bind_tf_idf() — standard corpus-level TF-IDF.
#   2. For each (year, word), keep only words used in at least min_papers
#      distinct papers that year (kills single-paper artefacts).
#   3. Aggregate to year by taking the MEAN TF-IDF across the papers that
#      used the word that year (not sum — sum would just reward words used
#      in many papers regardless of how distinctive they are per-paper).
#   4. Keep top N words per year by mean TF-IDF, plot as before.
#
# Inputs:   tokens_clean.rds            (root_dir)  — from 3_tokenise_and_clean.R
# Outputs:  tfidf_paper_level_top10.png (output_dir)
#           tfidf_paper_level_by_year.csv (output_dir)  — aggregated, per year
#           tfidf_paper_level_full.csv    (output_dir)  — full paper-level table
# Packages: tidyverse, tidytext
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(tidytext)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir     <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/Full_data_set/"
output_dir   <- file.path(root_dir, "outputs")
tokens_in    <- file.path(root_dir, "tokens_clean.rds")
plot_out     <- file.path(output_dir, "tfidf_paper_level_top10.png")
tfidf_year_out <- file.path(output_dir, "tfidf_paper_level_by_year.csv")
tfidf_full_out <- file.path(output_dir, "tfidf_paper_level_full.csv")
themes_in    <- file.path(root_dir, "conference_themes.csv")

top_n_words <- 10   # distinctive words to show per year
min_papers  <- 5    # word must appear in at least this many distinct papers
# in a year to be eligible — kills single-paper artefacts
# (reference-list surnames, one-off terminology)
# ──────────────────────────────────────────────────────────────────────────────

dir.create(output_dir, showWarnings = FALSE)

# ── 1. LOAD ───────────────────────────────────────────────────────────────────
tokens <- read_rds(tokens_in)
cat("Loaded", nrow(tokens), "tokens\n")
cat("Year range:", min(tokens$year), "–", max(tokens$year), "\n")
cat("Papers:    ", n_distinct(tokens$id), "\n\n")

# ── 2. COUNT WORDS PER PAPER ──────────────────────────────────────────────────
# Each paper (id) is one "document" for TF-IDF purposes — this is the
# standard corpus-level setup, giving IDF a fine-grained basis across
# ~3,000 documents rather than ~24.

paper_word_counts <- tokens |>
  count(id, year, word, name = "n")

cat("Paper × word combinations:", nrow(paper_word_counts), "\n")

# ── 3. COMPUTE TF-IDF AT PAPER LEVEL ──────────────────────────────────────────
tfidf_paper <- paper_word_counts |>
  bind_tf_idf(term = word, document = id, n = n) |>
  arrange(id, desc(tf_idf))

write_csv(tfidf_paper, tfidf_full_out)
cat("✓ Full paper-level TF-IDF table saved to:", tfidf_full_out, "\n")

# ── 4. AGGREGATE TO YEAR ──────────────────────────────────────────────────────
# For each (year, word):
#   n_papers    = how many distinct papers that year used the word
#   mean_tf_idf = average TF-IDF across those papers
# Words below min_papers are dropped before ranking — this is the key guard
# against single-paper idiosyncrasies dominating a year.

tfidf_by_year <- tfidf_paper |>
  group_by(year, word) |>
  summarise(
    n_papers    = n(),
    mean_tf_idf = mean(tf_idf),
    total_n     = sum(n),
    .groups = "drop"
  ) |>
  filter(n_papers >= min_papers) |>
  arrange(year, desc(mean_tf_idf))

write_csv(tfidf_by_year, tfidf_year_out)
cat("✓ Year-aggregated TF-IDF table saved to:", tfidf_year_out, "\n")
cat("Word × year combinations (after min_papers filter):",
    nrow(tfidf_by_year), "\n")

# ── 5. TOP N DISTINCTIVE WORDS PER YEAR ───────────────────────────────────────
top_tfidf <- tfidf_by_year |>
  group_by(year) |>
  slice_max(order_by = mean_tf_idf, n = top_n_words, with_ties = FALSE) |>
  ungroup() |>
  mutate(word = reorder_within(word, mean_tf_idf, year))

cat("Years in plot:", n_distinct(top_tfidf$year), "\n")

# ── 6. CONFERENCE THEME LABELS ────────────────────────────────────────────────
conference_themes <- read_csv(themes_in, show_col_types = FALSE)

top_tfidf_labelled <- top_tfidf |>
  left_join(conference_themes, by = "year") |>
  mutate(
    facet_label = if_else(
      is.na(theme),
      as.character(year),
      paste0(year, "\n", str_wrap(theme, width = 22))
    ),
    facet_label = fct_reorder(facet_label, year)
  )

# ── 7. PLOT ────────────────────────────────────────────────────────────────────
# Bar length = mean TF-IDF (distinctiveness, averaged across papers that used
# the word that year). Fill = raw total word count that year (total_n), so
# you can see whether a distinctive word was also widely used, or rare-but-
# concentrated.

n_years <- n_distinct(top_tfidf_labelled$year)
n_cols  <- 3
fig_w   <- n_cols * 6.5
fig_h   <- ceiling(n_years / n_cols) * 3.5

p <- ggplot(top_tfidf_labelled, aes(x = mean_tf_idf, y = word, fill = total_n)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.001)) +
  scale_fill_gradient(low = "#b2d8f5", high = "#08519c") +
  facet_wrap(~ facet_label, scales = "free_y", ncol = n_cols) +
  labs(
    title    = "Most distinctive words by conference year (paper-level TF-IDF) — Agronomy Australia papers",
    subtitle = paste0("Top ", top_n_words,
                      " stemmed words by mean TF-IDF; words in < ", min_papers,
                      " papers that year excluded"),
    x = "Mean TF-IDF across papers using the word that year",
    y = NULL,
    caption = "Bar fill: darker blue = higher total raw word count that year; lighter blue = lower.\nBar length shows how distinctive the word is on average, across papers that used it that year \u2014\nTF-IDF computed per paper against the full ~3,000-paper corpus, then averaged up to year."
  ) +
  theme_minimal(base_size = 9) +
  theme(
    strip.text         = element_text(size = 9, face = "bold", hjust = 0.5),
    axis.text.y        = element_text(size = 9),
    axis.text.x        = element_text(size = 9),
    axis.title.x       = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.spacing.x    = unit(1.4, "lines"),
    panel.spacing.y    = unit(1.2, "lines"),
    plot.title         = element_text(face = "bold", size = 11),
    plot.subtitle      = element_text(size = 8, colour = "grey40"),
    plot.caption       = element_text(size = 7, colour = "grey40", hjust = 0)
  )

p

ggsave(plot_out, p, width = fig_w, height = fig_h, dpi = 150)
cat("✓ Plot saved to:", plot_out, "\n")
cat("\nCompare against 5_tfidf_by_year.R (year-as-document version) and\n")
cat("4_term_frequency_visualise.R (raw frequency) — words appearing across\n")
cat("all three are the strongest year-defining signals.\n")
