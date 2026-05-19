library(tidyverse)
library(fs)
library(pdftools)
library(tidytext)
library(SnowballC)
library(ggplot2)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
subset_dir   <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/papers_subset"
subset_index <- read_csv("N:/work/RiskWise/Brendan_Ag_Conf_papers/paper_index_subset.csv")
output_dir   <- "N:/work/RiskWise/Brendan_Ag_Conf_papers"

# ── LOAD CONFERENCE THEMES ────────────────────────────────────────────────────  # ◀◀ NEW
conf_themes <- read_csv("N:/work/RiskWise/Brendan_Ag_Conf_papers/conference_themes.csv")  # ◀◀ NEW

# ══════════════════════════════════════════════════════════════════════════════
# STEP 1: EXTRACT TEXT
# Run once then comment out and load from rds
# ══════════════════════════════════════════════════════════════════════════════

# corpus <- subset_index |>
#   mutate(
#     filepath = file.path(subset_dir, filename_new),
#     text     = map_chr(filepath, extract_text)
#   )
# write_rds(corpus, file.path(output_dir, "corpus.rds"))

corpus <- read_rds("N:/work/RiskWise/Brendan_Ag_Conf_papers/corpus.rds")
cat("✓ Corpus loaded:", nrow(corpus), "papers\n")

# ══════════════════════════════════════════════════════════════════════════════
# STEP 2: STOPWORDS
# ▶▶ ADD UNWANTED WORDS HERE, then re-run Steps 2 → 3 → 4 → 5
# ══════════════════════════════════════════════════════════════════════════════

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

# ══════════════════════════════════════════════════════════════════════════════
# STEP 3: TOKENISE
# Re-run whenever custom_stops changes
# ══════════════════════════════════════════════════════════════════════════════

tokens <- corpus |>
  select(id, year, text) |>
  unnest_tokens(word, text) |>
  filter(str_detect(word, "^[a-z]+$")) |>
  anti_join(all_stops, by = "word") |>
  mutate(word = wordStem(word))

cat("✓ Tokens ready:", nrow(tokens), "rows |", n_distinct(tokens$word), "unique words\n")

# ══════════════════════════════════════════════════════════════════════════════
# STEP 4: COUNT
# ══════════════════════════════════════════════════════════════════════════════

top_terms_freq <- tokens |>
  count(year, word, sort = TRUE) |>
  group_by(year) |>
  slice_max(n, n = 10) |>
  ungroup() |>
  left_join(conf_themes, by = "year")  # ◀◀ NEW - adds theme and location columns

# ══════════════════════════════════════════════════════════════════════════════
# STEP 5: PLOT
# ══════════════════════════════════════════════════════════════════════════════

# ◀◀ NEW - build facet labels that show year + theme
facet_labels <- conf_themes |>                          # ◀◀ NEW
  mutate(label = paste0(year, "\n", theme)) |>          # ◀◀ NEW
  select(year, label) |>                                # ◀◀ NEW
  deframe()                                             # ◀◀ NEW

top_terms_freq |>
  mutate(word = reorder_within(word, n, year)) |>
  ggplot(aes(n, word, fill = factor(year))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year,
             scales = "free_y",
             ncol   = 4,
             labeller = as_labeller(facet_labels)) +    # ◀◀ NEW - year + theme as panel title
  scale_y_reordered() +
  scale_fill_viridis_d(option = "turbo") +
  labs(
    title    = "Most common words by conference year",
    subtitle = "Raw word frequency after stopword removal",
    x        = "Word count",
    y        = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text         = element_text(face = "bold", size = 8),  # ◀◀ slightly smaller to fit theme
    panel.grid.major.y = element_blank()
  )

ggsave(file.path(output_dir, "wordfreq_by_year.png"),
       width = 18, height = 22, dpi = 150)               # ◀◀ slightly taller to fit theme labels

cat("✓ Plot saved\n")
