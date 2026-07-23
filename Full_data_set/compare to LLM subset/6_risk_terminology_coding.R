# ══════════════════════════════════════════════════════════════════════════════
# Script:   6_risk_terminology_coding.R for subset of data
# Purpose:  Dictionary-based coding of risk-related terminology (Tier 1):
#           explicit terms (risk, uncertainty, variability, vulnerability),
#           probabilistic/scenario/sensitivity method language, behavioural
#           science terms, and confidence interval / p-value reporting.
#           Tracks presence per paper and prevalence by year — directly
#           addresses "are more papers tackling risk over time?"
#
# Two data sources used:
#   tokens_clean.rds — for word/stem-based dictionary categories
#   corpus.rds       — for regex-based CI/p-value detection, since numbers
#                      and punctuation are stripped during tokenisation and
#                      can't be recovered from tokens_clean.rds
#
# IMPORTANT: the dictionaries below are a first draft. Review and refine
# with Brendan/Rick before treating results as final — presence of a stem
# is a proxy for a concept, not a guarantee the paper is actually about it.
#
# Inputs:   tokens_clean.rds        (root_dir)
#           corpus.rds              (root_dir)
# Outputs:  risk_terms_by_paper.csv (output_dir) — paper x category matrix
#           risk_terms_by_year.csv  (output_dir) — % of papers per year, per category
#           risk_terms_trend.png    (output_dir) — trend line plot
# Packages: tidyverse
# ══════════════════════════════════════════════════════════════════════════════
#install.packages("patchwork")
#install.packages("ggh4x")
library(ggh4x)   
library(tidyverse)
library(patchwork)  

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir     <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/Full_data_set/"
output_dir   <- file.path(root_dir, "subset_for_LLM")
tokens_in    <- file.path(root_dir, "tokens_clean.rds")
corpus_in    <- file.path(root_dir, "corpus.rds")
paper_out    <- file.path(output_dir, "risk_terms_by_paper_3years.csv")
year_out     <- file.path(output_dir, "risk_terms_by_year_3years.csv")
plot_out     <- file.path(output_dir, "risk_terms_trend_3years.png")
# ──────────────────────────────────────────────────────────────────────────────

dir.create(output_dir, showWarnings = FALSE)
# ── 0. Years of interest  ───────────────────────────────────────────────────────────────────
# Filter to just these conference years — for comparison against the
# LLM-based "search for risk" analysis, which only covered these 3 years.
YEARS_OF_INTEREST <- c(1985, 1996, 2022)


# ── 1. LOAD ───────────────────────────────────────────────────────────────────
tokens <- read_rds(tokens_in)
corpus <- read_rds(corpus_in)

tokens <- tokens |> filter(year %in% YEARS_OF_INTEREST)
corpus <- corpus |> filter(year %in% YEARS_OF_INTEREST)



cat("Filtered to years:", paste(YEARS_OF_INTEREST, collapse = ", "), "\n")
cat("  ->", nrow(tokens), "tokens across", n_distinct(tokens$id), "papers\n")
cat("  ->", nrow(corpus), "papers (raw text)\n\n")

if (n_distinct(corpus$year) < length(YEARS_OF_INTEREST)) {
  missing_yrs <- setdiff(YEARS_OF_INTEREST, unique(corpus$year))
  warning("No papers found for year(s): ", paste(missing_yrs, collapse = ", "))
}

# ── 2. DICTIONARY DEFINITIONS ─────────────────────────────────────────────────
# Each category maps to one or more STEMS to match against tokens$word.
# These are Porter stems, matching how Script 3 processed the text — check
# a few by hand (e.g. SnowballC::wordStem("uncertainty")) if unsure.
#
# FIRST DRAFT — review with Brendan/Rick before treating as final.

risk_stem <- "risk"

# ── 3. CODE EACH PAPER — TOKEN-BASED CATEGORIES ───────────────────────────────
# For each paper, flag whether ANY word matches ANY stem in each category.
# Also count total matching tokens per category, for a secondary "how much"
# measure alongside simple presence/absence.

risk_token_counts <- tokens |>
  filter(str_detect(word, paste0("^", risk_stem))) |>
  count(id, year, name = "n_mentions_token")



# code_paper_tokens <- function(dict, tokens_df) {
#   map_dfr(names(dict), function(cat_name) {
#     stems <- dict[[cat_name]]
#     pattern <- paste0("^(", paste(stems, collapse = "|"), ")")
#     
#     tokens_df |>
#       filter(str_detect(word, pattern)) |>
#       count(id, year, name = "n_mentions") |>
#       mutate(category = cat_name)
#   })
# }
# 
# token_codes <- code_paper_tokens(term_dict, tokens)
# 
# cat("Token-based category matches found across",
#     n_distinct(token_codes$id), "papers\n")

# ── 4. CODE EACH PAPER — CI / P-VALUE DETECTION (regex on raw text) ──────────
# Matches common reporting conventions:
#   "95% CI", "confidence interval", "p < 0.05", "p=0.03", "p-value"
# Case-insensitive; deliberately loose to catch variant formatting.


risk_raw_counts <- corpus |>
  mutate(
    n_mentions_raw = str_count(text, regex("\\brisk\\w*", ignore_case = TRUE))
  ) |>
  select(id, year, title, first_author, n_mentions_raw)



# ci_pattern <- regex(
#   "\\bp\\s*[<>=]\\s*0?\\.\\d+|\\bp-?value|\\bconfidence interval|\\b\\d{2}%\\s*ci\\b",
#   ignore_case = TRUE
# )
# 
# ci_codes <- corpus |>
#   mutate(
#     has_ci_pvalue = str_detect(text, ci_pattern),
#     n_mentions    = str_count(text, ci_pattern)
#   ) |>
#   filter(has_ci_pvalue) |>
#   select(id, year, n_mentions) |>
#   mutate(category = "Confidence intervals / p-values")
# 
# cat("CI/p-value mentions found in", nrow(ci_codes), "papers\n\n")

# ── 5. COMBINE ─────────────────────────────────────────────────────────────────

risk_paper_comparison <- risk_raw_counts |>
  left_join(risk_token_counts |> select(id, n_mentions_token), by = "id") |>
  mutate(
    n_mentions_token = replace_na(n_mentions_token, 0),
    has_risk = n_mentions_raw > 0
  ) |>
  arrange(year, desc(n_mentions_raw))

write_csv(risk_paper_comparison,
          file.path(output_dir, "risk_paper_comparison_3years.csv"))

# - 6. 
llm_export <- corpus |>
  filter(year %in% YEARS_OF_INTEREST) |>
  select(id, year, title, first_author, text)

#Option A
write_csv(llm_export, file.path(output_dir, "corpus_for_llm_3years.csv"))
#Option B
txt_dir <- file.path(output_dir, "corpus_for_llm_3years_txt")
dir.create(txt_dir, showWarnings = FALSE)

llm_export |>
  rowwise() |>
  group_walk(~ {
    fname <- file.path(txt_dir, paste0(.x$id, ".txt"))
    header <- paste0("Title: ", .x$title, "\nAuthor: ", .x$first_author,
                     "\nYear: ", .x$year, "\n\n")
    writeLines(paste0(header, .x$text), fname)
  })
