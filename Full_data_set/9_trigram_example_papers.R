# ══════════════════════════════════════════════════════════════════════════════
# Script:   9_trigram_example_papers.R
# Purpose:  For a given risk-related trigram (or list of trigrams), pull real
#           example papers containing it — title, author, year, and a text
#           snippet showing the phrase in context. For sharing concrete
#           examples with Rick/Brendan rather than just aggregate statistics.
#
# Inputs:   corpus.rds   (root_dir) — raw paper text
# Packages: tidyverse
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)

root_dir  <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/Full_data_set/"
corpus_in <- file.path(root_dir, "corpus.rds")

corpus <- read_rds(corpus_in)

# ── FUNCTION: find example papers containing a target phrase ─────────────────
# target_phrase: the exact trigram/bigram as it would appear in normal text
#                (e.g. "risk of frost", "reduce the risk") — matched loosely,
#                allowing for minor variation in spacing/case.
# n_examples:    how many example papers to return (defaults to 3)
# snippet_chars: how many characters of context to show around the match

find_trigram_examples <- function(target_phrase, n_examples = 3, snippet_chars = 200) {
  
  pattern <- regex(target_phrase, ignore_case = TRUE)
  
  matches <- corpus |>
    filter(str_detect(text, pattern)) |>
    mutate(
      match_pos = str_locate(text, pattern)[, 1],
      snippet_start = pmax(1, match_pos - snippet_chars),
      snippet_end   = pmin(str_length(text), match_pos + str_length(target_phrase) + snippet_chars),
      snippet = str_sub(text, snippet_start, snippet_end),
      snippet = paste0("...", str_squish(snippet), "...")
    ) |>
    select(id, year, title, first_author, snippet) |>
    slice_head(n = n_examples)
  
  if (nrow(matches) == 0) {
    cat("No papers found containing:", target_phrase, "\n")
    return(invisible(NULL))
  }
  
  cat("\n═══════════════════════════════════════════════════════════\n")
  cat("Phrase:", target_phrase, "\n")
  cat("Papers found:", nrow(corpus |> filter(str_detect(text, pattern))), "\n")
  cat("═══════════════════════════════════════════════════════════\n\n")
  
  for (i in seq_len(nrow(matches))) {
    cat(i, ". ", matches$title[i], "\n", sep = "")
    cat("   ", matches$first_author[i], " (", matches$year[i], ")\n", sep = "")
    cat("   \"", matches$snippet[i], "\"\n\n", sep = "")
  }
  
  invisible(matches)
}

# ── USAGE ──────────────────────────────────────────────────────────────────────
# Run for one phrase:
find_trigram_examples("risk of frost")

# Or loop through several trigrams you've already identified as common/interesting:
target_phrases <- c(
  "risk of frost",
  "risk of crop",
  "reduce the risk",
  "climate risk",
  "risk management"
)

all_examples <- map(target_phrases, find_trigram_examples, n_examples = 2)

# ── OPTIONAL: save all examples to a CSV for easy reference/copy-paste ───────
examples_table <- map2_dfr(target_phrases, all_examples, function(phrase, ex) {
  if (is.null(ex)) return(NULL)
  ex |> mutate(phrase = phrase, .before = 1)
})

write_csv(examples_table, file.path(root_dir, "outputs", "trigram_example_papers.csv"))
cat("\u2713 Example papers saved to trigram_example_papers.csv\n")


r# ── FUNCTION: find example papers spread across early/mid/late periods ───────
# Splits the study period into three roughly equal eras and pulls example
# papers from each, so you can show the phrase appearing across the full
# 1980–2024 span rather than just recent years.

find_trigram_examples_by_era <- function(target_phrase, n_per_era = 2, snippet_chars = 150) {

  pattern <- regex(target_phrase, ignore_case = TRUE)

  matches <- corpus |>
    filter(str_detect(text, pattern)) |>
    mutate(
      era = case_when(
        year <= 1996 ~ "Early (1980\u20131996)",
        year <= 2010 ~ "Mid (1998\u20132010)",
        TRUE         ~ "Late (2012\u20132024)"
      ),
      match_pos = str_locate(text, pattern)[, 1],
      snippet_start = pmax(1, match_pos - snippet_chars),
      snippet_end   = pmin(str_length(text), match_pos + str_length(target_phrase) + snippet_chars),
      snippet = paste0("...", str_squish(str_sub(text, snippet_start, snippet_end)), "...")
    ) |>
    select(id, year, era, title, first_author, snippet)

  if (nrow(matches) == 0) {
    cat("No papers found containing:", target_phrase, "\n")
    return(invisible(NULL))
  }

  examples <- matches |>
    group_by(era) |>
    slice_head(n = n_per_era) |>
    ungroup() |>
    mutate(era = factor(era, levels = c("Early (1980\u20131996)", "Mid (1998\u20132010)", "Late (2012\u20132024)"))) |>
    arrange(era, year)

  cat("\n═══════════════════════════════════════════════════════════\n")
  cat("Phrase:", target_phrase, "\n")
  cat("Total papers containing this phrase:", nrow(matches), "\n")
  cat("═══════════════════════════════════════════════════════════\n\n")

  for (e in levels(examples$era)) {
    era_rows <- examples |> filter(era == e)
    if (nrow(era_rows) == 0) {
      cat(e, ": no examples found\n\n")
      next
    }
    cat(e, ":\n", sep = "")
    for (i in seq_len(nrow(era_rows))) {
      cat("  \u2022 ", era_rows$title[i], " \u2014 ", era_rows$first_author[i],
          " (", era_rows$year[i], ")\n", sep = "")
    }
    cat("\n")
  }

  invisible(examples)
}

# ── RUN for a chosen phrase ───────────────────────────────────────────────────
climate_risk_examples <- find_trigram_examples_by_era("climate risk", n_per_era = 2)

# ── Or across your shortlist of phrases at once ───────────────────────────────
target_phrases <- c("risk of frost", "climate risk", "risk management", "reduce the risk")

all_era_examples <- map_dfr(target_phrases, function(p) {
  res <- find_trigram_examples_by_era(p, n_per_era = 2)
  if (!is.null(res)) res |> mutate(phrase = p, .before = 1)
})

write_csv(all_era_examples, file.path(root_dir, "outputs", "trigram_examples_by_era.csv"))
cat("\u2713 Era-stratified examples saved to trigram_examples_by_era.csv\n")
