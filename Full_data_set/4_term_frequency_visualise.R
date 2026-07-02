# ══════════════════════════════════════════════════════════════════════════════
# Script:   4_term_frequency_visualise.R
# Purpose:  Count word occurrences per conference year and visualise as
#           horizontal bar charts — top N words per year, faceted by year.
#
# Iteration workflow:
#   View plot → spot noise words → add to custom_stops in Script 3 →
#   re-run Scripts 3 and 4. Script 2 (PDF extraction) never repeats.
#
# Inputs:   tokens_clean.rds       (root_dir)  — from 3_tokenise_and_clean.R
# Outputs:  term_freq_top10.png    (output_dir)
#           term_freq_by_year.csv  (output_dir)
# Packages: tidyverse
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(tidytext)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir    <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/Full_data_set/"
output_dir  <- file.path(root_dir, "outputs")
tokens_in   <- file.path(root_dir, "tokens_clean.rds")
plot_out    <- file.path(output_dir, "term_freq_top10.png")
freq_out    <- file.path(output_dir, "term_freq_by_year.csv")

top_n_words <- 10
# ──────────────────────────────────────────────────────────────────────────────

dir.create(output_dir, showWarnings = FALSE)

# ── 1. LOAD ───────────────────────────────────────────────────────────────────
tokens <- read_rds(tokens_in)
cat("Loaded", nrow(tokens), "tokens\n")
cat("Year range:", min(tokens$year), "–", max(tokens$year), "\n")
cat("Columns:   ", paste(names(tokens), collapse = ", "), "\n\n")

# ── 2. COUNT PER YEAR ─────────────────────────────────────────────────────────
# Count how many times each stemmed word appears in each year,
# and calculate relative frequency (proportion of all words that year).
# Relative frequency is more meaningful than raw counts because the number
# of papers per conference year varies.

freq_by_year <- tokens |>
  count(year, word, name = "n") |>
  group_by(year) |>
  mutate(
    total_words = sum(n),
    freq        = n / total_words
  ) |>
  ungroup()

write_csv(freq_by_year, freq_out)
cat("✓ Full frequency table saved to:", freq_out, "\n")

# ── 3. TOP N WORDS PER YEAR ───────────────────────────────────────────────────
top_words <- freq_by_year |>
  group_by(year) |>
  slice_max(order_by = n, n = top_n_words, with_ties = FALSE) |>
  ungroup() |>
  mutate(word = reorder_within(word, n, year))

cat("Years in plot:", n_distinct(top_words$year), "\n")

# ── 4. CONFERENCE THEME LABELS ────────────────────────────────────────────────
# Read from CSV rather than hardcoding — update the CSV to add or correct themes.
# CSV should have two columns: year (numeric) and theme (text, NA if unknown).

themes_in <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/Full_data_set/conference_themes.csv"

conference_themes <- read_csv(themes_in, show_col_types = FALSE)

top_words_labelled <- top_words |>
  left_join(conference_themes, by = "year") |>
  mutate(
    facet_label = if_else(
      is.na(theme),
      as.character(year),
      paste0(year, "\n", str_wrap(theme, width = 35))
    ),
    facet_label = fct_reorder(facet_label, year)
  )


# ── 5. PLOT ───────────────────────────────────────────────────────────────────
# Horizontal bar charts faceted by year — one panel per conference year.
# Bars show raw word count (n), colour shade shows relative frequency (freq)
# so you can see both how common a word is and how dominant it is in that year.
#
# Facet labels show year + conference theme (from conference_themes.csv).
# Years with no theme show year only.
#
# top_n_words is set in CONFIGURATION at the top of the script — change it
# there to show more or fewer words per panel without touching this block.

n_years <- n_distinct(top_words_labelled$year)
n_cols  <- 4
fig_w   <- n_cols * 5.5
fig_h   <- ceiling(n_years / n_cols) * 3.5

p <- ggplot(top_words_labelled, aes(x = n, y = word, fill = freq)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  scale_fill_gradient(low = "#b2d8f5", high = "#08519c") +
  facet_wrap(~ facet_label, scales = "free_y", ncol = n_cols) +
  labs(
    title    = "Top words by conference year — Agronomy Australia papers",
    subtitle = paste0("Top ", top_n_words,
                      " stemmed words; stopwords and numbers removed"),
    x = "Word count",
    y = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(
    strip.text         = element_text(size = 7, face = "bold"),
    axis.text.y        = element_text(size = 7),
    panel.grid.major.y = element_blank(),
    plot.title         = element_text(face = "bold", size = 11),
    plot.subtitle      = element_text(size = 8, colour = "grey40")
  )

p

ggsave(plot_out, p, width = fig_w, height = fig_h, dpi = 150)
cat("✓ Plot saved to:", plot_out, "\n")
cat("\nIteration tip:\n")
cat("  Spot noise words in the plot → add to custom_stops in Script 3\n")
cat("  → re-run Scripts 3 and 4. Script 2 never needs to repeat.\n")


# ══════════════════════════════════════════════════════════════════════════════
# 6. RELATIVE FREQUENCY VERSION (top words re-ranked by freq, not raw count)
# ══════════════════════════════════════════════════════════════════════════════
# Same idea as section 3-5, but ranks/plots by relative frequency instead of
# raw count. Kept separate from the raw-count version above so both outputs
# are available side by side.

plot_out_rel <- file.path(output_dir, "term_freq_top10_relative.png")

top_words_rel <- freq_by_year |>
  group_by(year) |>
  slice_max(order_by = freq, n = top_n_words, with_ties = FALSE) |>
  ungroup() |>
  mutate(word = reorder_within(word, freq, year))

top_words_rel_labelled <- top_words_rel |>
  left_join(conference_themes, by = "year") |>
  mutate(
    facet_label = if_else(
      is.na(theme),
      as.character(year),
      paste0(year, "\n", str_wrap(theme, width = 22))
    ),
    facet_label = fct_reorder(facet_label, year)
  )

# Wider, fewer columns so panel titles and y-axis word labels have more room
n_years_rel <- n_distinct(top_words_rel_labelled$year)
n_cols_rel  <- 3
fig_w_rel   <- n_cols_rel * 6.5
fig_h_rel   <- ceiling(n_years_rel / n_cols_rel) * 3.5

p_rel <- ggplot(top_words_rel_labelled, aes(x = freq, y = word, fill = n)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  scale_x_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  scale_fill_gradient(low = "#b2d8f5", high = "#08519c") +
  facet_wrap(~ facet_label, scales = "free_y", ncol = n_cols_rel) +
  labs(
    title    = "Top words by conference year (by relative frequency) — Agronomy Australia papers",
    subtitle = paste0("Top ", top_n_words,
                      " stemmed words ranked by relative frequency; stopwords and numbers removed"),
    x = "Relative frequency (% of year's words)",
    y = NULL,
    caption = "Bar fill: darker blue = higher raw word count that year; lighter blue = lower raw word count.\nBar length shows relative frequency, so colour and length can disagree \u2014\na short, dark bar means a word was common in absolute terms but a small share of a large year's total words."
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

p_rel

ggsave(plot_out_rel, p_rel, width = fig_w_rel, height = fig_h_rel, dpi = 150)
cat("✓ Relative frequency plot saved to:", plot_out_rel, "\n")
cat("\nNote: top words may differ between the raw-count and relative-frequency\n")
cat("versions, especially in early sparse years — worth comparing both.\n")


# ══════════════════════════════════════════════════════════════════════════════
# 7. DIAGNOSTIC — investigate 2004 outlier (unusually dark bars in rel. freq. plot)
# ══════════════════════════════════════════════════════════════════════════════
# 2004 showed up with much darker fill (raw count) than any other year in the
# relative frequency plot. Checking whether this is a genuine spike in paper
# volume/word count, or a sign of a corpus problem for that year (duplicates,
# a small number of unusually long papers, or a PDF extraction issue).

# Total words per year, for context across the whole corpus
total_words_by_year <- freq_by_year |>
  distinct(year, total_words) |>
  arrange(year)

print(total_words_by_year, n = Inf)

# How does 2004 compare to the years around it?
total_words_by_year |>
  filter(year %in% c(2001, 2003, 2004, 2006, 2008))

# How many papers contributed to 2004 vs neighbouring years?
# (uses corpus.rds, loaded fresh here since Script 4 only loads tokens_clean.rds)
corpus_check <- read_rds(file.path(root_dir, "corpus.rds"))

papers_per_year <- corpus_check |>
  count(year, name = "n_papers") |>
  arrange(year)

print(papers_per_year, n = Inf)

papers_per_year |>
  filter(year %in% c(2001, 2003, 2004, 2006, 2008))

# Average words per paper by year — flags whether 2004 has more papers,
# or the same number of unusually long ones
corpus_check |>
  mutate(word_count = str_count(text, "\\S+")) |>
  group_by(year) |>
  summarise(
    n_papers        = n(),
    total_words     = sum(word_count),
    avg_words_paper = round(mean(word_count), 0)
  ) |>
  arrange(desc(total_words)) |>
  print(n = 15)

corpus_check |>
  filter(year == 2004) |>
  mutate(water_count = str_count(str_to_lower(text), "\\bwater\\b")) |>
  select(id, title, first_author, water_count) |>
  arrange(desc(water_count)) |>
  print(n = 15)
