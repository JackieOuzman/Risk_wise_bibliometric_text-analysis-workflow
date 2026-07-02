# ══════════════════════════════════════════════════════════════════════════════
# Script:   5_tfidf_by_year.R
# Purpose:  Identify words DISTINCTIVE to each conference year using TF-IDF
#           (term frequency–inverse document frequency), rather than just
#           common — visualised as horizontal bar charts, faceted by year.
#
# Why TF-IDF rather than raw frequency (Script 4)?
#   Raw counts are dominated by perennial terms — soil, crop, yield, water —
#   that appear every year and reveal little about what's changing.
#   TF-IDF down-weights words common across all years and up-weights words
#   unusually prominent in a specific year. A word that spikes in 2008 and
#   fades by 2015 shows up strongly distinctive for 2008, rather than being
#   swamped by perennials. Compare this plot alongside Script 4's raw
#   frequency plot for the richest interpretation — "always important" vs
#   "distinctively important in this year".
#
# Method:
#   Each conference year is treated as a single "document". bind_tf_idf()
#   calculates, per (year, word):
#     tf     = n / total words in that year        (how common in year)
#     idf    = log(n_years / n_years_containing_word)   (how rare across years)
#     tf_idf = tf × idf
#   A word scores high only if it's frequent in one year AND rare elsewhere.
#
# Inputs:   tokens_clean.rds       (root_dir)  — from 3_tokenise_and_clean.R
# Outputs:  tfidf_top10.png        (output_dir)
#           tfidf_by_year.csv      (output_dir)
# Packages: tidyverse, tidytext
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(tidytext)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir    <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/Full_data_set/"
output_dir  <- file.path(root_dir, "outputs")
tokens_in   <- file.path(root_dir, "tokens_clean.rds")
plot_out    <- file.path(output_dir, "tfidf_top10.png")
tfidf_out   <- file.path(output_dir, "tfidf_by_year.csv")
themes_in   <- file.path(root_dir, "conference_themes.csv")

top_n_words <- 10   # distinctive words to show per year
min_count   <- 3    # ignore words appearing < this many times in a year —
# suppresses rare-word TF-IDF inflation (hapax legomena).
# Lower this for sparse early years if they end up empty.
# ──────────────────────────────────────────────────────────────────────────────

dir.create(output_dir, showWarnings = FALSE)

# ── 1. LOAD ───────────────────────────────────────────────────────────────────
tokens <- read_rds(tokens_in)
cat("Loaded", nrow(tokens), "tokens\n")
cat("Year range:", min(tokens$year), "–", max(tokens$year), "\n")
cat("Columns:   ", paste(names(tokens), collapse = ", "), "\n\n")

# ── 2. COUNT WORDS PER YEAR ───────────────────────────────────────────────────
# Treating year as the document unit for TF-IDF. Words appearing fewer than
# min_count times in a given year are dropped before TF-IDF — a word used
# once in a year can score an artificially high TF-IDF just by being rare
# everywhere else, without being a meaningful signal.

word_year_counts <- tokens |>
  count(year, word, name = "n") |>
  filter(n >= min_count)

cat("Word × year combinations (after min_count filter):",
    nrow(word_year_counts), "\n")

# ── 3. COMPUTE TF-IDF ─────────────────────────────────────────────────────────
tfidf <- word_year_counts |>
  bind_tf_idf(term = word, document = year, n = n) |>
  arrange(year, desc(tf_idf))

write_csv(tfidf, tfidf_out)
cat("✓ Full TF-IDF table saved to:", tfidf_out, "\n")

# ── 4. TOP N DISTINCTIVE WORDS PER YEAR ───────────────────────────────────────
top_tfidf <- tfidf |>
  group_by(year) |>
  slice_max(order_by = tf_idf, n = top_n_words, with_ties = FALSE) |>
  ungroup() |>
  mutate(word = reorder_within(word, tf_idf, year))

cat("Years in plot:", n_distinct(top_tfidf$year), "\n")

# ── 5. CONFERENCE THEME LABELS ────────────────────────────────────────────────
# Same lookup as Script 4 — read from CSV rather than hardcoding.
# CSV should have two columns: year (numeric) and theme (text, NA if unknown).

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

# ── 6. PLOT ────────────────────────────────────────────────────────────────────
# Horizontal bar charts faceted by year — one panel per conference year.
# Bar length = TF-IDF score (how distinctive the word is to that year).
# Fill colour = raw word count that year, so you can see whether a
# distinctive word was also genuinely widely-used, or just rare-but-present.
#
# Wider, fewer columns (matches Script 4's relative frequency version) so
# panel titles and y-axis word labels have room to breathe.

n_years <- n_distinct(top_tfidf_labelled$year)
n_cols  <- 3
fig_w   <- n_cols * 6.5
fig_h   <- ceiling(n_years / n_cols) * 3.5

p <- ggplot(top_tfidf_labelled, aes(x = tf_idf, y = word, fill = n)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.001)) +
  scale_fill_gradient(low = "#b2d8f5", high = "#08519c") +
  facet_wrap(~ facet_label, scales = "free_y", ncol = n_cols) +
  labs(
    title    = "Most distinctive words by conference year (TF-IDF) — Agronomy Australia papers",
    subtitle = paste0("Top ", top_n_words,
                      " stemmed words by TF-IDF; words appearing < ", min_count,
                      " times in a year excluded"),
    x = "TF-IDF score (higher = more distinctive to that year)",
    y = NULL,
    caption = "Bar fill: darker blue = higher raw word count that year; lighter blue = lower raw word count.\nBar length shows how distinctive the word is to that year (high in this year, rare elsewhere) \u2014\na short, dark bar means a word was common in absolute terms but not unusual compared to other years."
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
cat("\nIteration tip:\n")
cat("  Compare against Script 4's raw frequency plot — words appearing in\n")
cat("  both are the true year-defining terms; words only here are rare but\n")
cat("  concentrated; words only in Script 4 are perennials with no distinctive spike.\n")
