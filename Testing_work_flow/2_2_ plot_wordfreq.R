# ══════════════════════════════════════════════════════════════════════════════
# Script:   plot_wordfreq.R
# Purpose:  Count top terms per conference year and plot as a bar chart.
#           Load tokens.rds and conference_themes.csv, count, and plot.
#
#           This script and tokenise.R are your main iterative loop:
#           spot a noise word → add to custom_stops in tokenise.R →
#           re-run tokenise.R → re-run this script.
#
# Inputs:
#   tokens.rds              — cleaned tokens from tokenise.R
#   conference_themes.csv   — year, theme and location for facet labels
#
# Output:
#   wordfreq_by_year.png    — bar chart of top 10 words per conference year
#
# Packages: tidyverse, tidytext
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(tidytext)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir     <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/testing_workflow"
tokens_in    <- file.path(root_dir, "tokens.rds")
themes_path  <- file.path(root_dir, "conference_themes.csv")
plot_out     <- file.path(root_dir, "wordfreq_by_year.png")
top_n_words  <- 10    # number of top words to show per year
# ──────────────────────────────────────────────────────────────────────────────

# ── 1. LOAD DATA ──────────────────────────────────────────────────────────────
tokens     <- read_rds(tokens_in)
conf_themes <- read_csv(themes_path, show_col_types = FALSE)

cat("✓ Tokens loaded:", nrow(tokens), "rows\n")
cat("✓ Themes loaded:", nrow(conf_themes), "years\n")

# ── 2. COUNT TOP TERMS ────────────────────────────────────────────────────────
top_terms <- tokens |>
  count(year, word, sort = TRUE) |>
  group_by(year) |>
  slice_max(n, n = top_n_words) |>
  ungroup() |>
  left_join(conf_themes, by = "year")

cat("✓ Top terms calculated\n")

# ── 3. BUILD FACET LABELS ─────────────────────────────────────────────────────
# Each panel title shows the year and conference theme.
facet_labels <- conf_themes |>
  mutate(label = paste0(year, "\n", theme)) |>
  select(year, label) |>
  deframe()

cat("✓ Facet labels ready\n")


# ── 4. PLOT ───────────────────────────────────────────────────────────────────
top_terms |>
  mutate(word = reorder_within(word, n, year)) |>
  ggplot(aes(n, word, fill = factor(year))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year,
             scales   = "free_y",
             ncol     = 4,
             labeller = as_labeller(facet_labels)) +
  scale_y_reordered() +
  scale_fill_viridis_d(option = "turbo") +
  labs(
    title    = "Most common words by conference year",
    subtitle = "Raw word frequency after stopword removal and stemming",
    x        = "Word count",
    y        = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text         = element_text(face = "bold", size = 8),
    panel.grid.major.y = element_blank()
  )

# ── 5. SAVE ───────────────────────────────────────────────────────────────────
ggsave(plot_out, width = 18, height = 22, dpi = 150)
cat("✓ Plot saved to:", plot_out, "\n")
