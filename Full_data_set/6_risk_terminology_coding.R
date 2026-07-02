# ══════════════════════════════════════════════════════════════════════════════
# Script:   6_risk_terminology_coding.R
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

library(tidyverse)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
root_dir     <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/Full_data_set/"
output_dir   <- file.path(root_dir, "outputs")
tokens_in    <- file.path(root_dir, "tokens_clean.rds")
corpus_in    <- file.path(root_dir, "corpus.rds")
paper_out    <- file.path(output_dir, "risk_terms_by_paper.csv")
year_out     <- file.path(output_dir, "risk_terms_by_year.csv")
plot_out     <- file.path(output_dir, "risk_terms_trend.png")
# ──────────────────────────────────────────────────────────────────────────────

dir.create(output_dir, showWarnings = FALSE)

# ── 1. LOAD ───────────────────────────────────────────────────────────────────
tokens <- read_rds(tokens_in)
corpus <- read_rds(corpus_in)

cat("Loaded", nrow(tokens), "tokens across", n_distinct(tokens$id), "papers\n")
cat("Loaded", nrow(corpus), "papers (raw text)\n\n")

# ── 2. DICTIONARY DEFINITIONS ─────────────────────────────────────────────────
# Each category maps to one or more STEMS to match against tokens$word.
# These are Porter stems, matching how Script 3 processed the text — check
# a few by hand (e.g. SnowballC::wordStem("uncertainty")) if unsure.
#
# FIRST DRAFT — review with Brendan/Rick before treating as final.

term_dict <- list(
  "Term: Risk"          = c("risk"),
  "Term: Uncertainty"   = c("uncertain"),
  "Term: Variability"   = c("variabl"),
  "Term: Vulnerability" = c("vulner"),
  "Method: Probabilistic" = c("probabilist", "stochast", "likelihood"),
  "Method: Scenario-based" = c("scenario"),
  "Method: Sensitivity"   = c("sensit"),
  "Behavioural terminology" = c("heurist", "aversion", "biase", "cognit")
  # "framing" deliberately excluded — stem "fram" too likely to false-positive
)

# ── 3. CODE EACH PAPER — TOKEN-BASED CATEGORIES ───────────────────────────────
# For each paper, flag whether ANY word matches ANY stem in each category.
# Also count total matching tokens per category, for a secondary "how much"
# measure alongside simple presence/absence.

code_paper_tokens <- function(dict, tokens_df) {
  map_dfr(names(dict), function(cat_name) {
    stems <- dict[[cat_name]]
    pattern <- paste0("^(", paste(stems, collapse = "|"), ")")
    
    tokens_df |>
      filter(str_detect(word, pattern)) |>
      count(id, year, name = "n_mentions") |>
      mutate(category = cat_name)
  })
}

token_codes <- code_paper_tokens(term_dict, tokens)

cat("Token-based category matches found across",
    n_distinct(token_codes$id), "papers\n")

# ── 4. CODE EACH PAPER — CI / P-VALUE DETECTION (regex on raw text) ──────────
# Matches common reporting conventions:
#   "95% CI", "confidence interval", "p < 0.05", "p=0.03", "p-value"
# Case-insensitive; deliberately loose to catch variant formatting.

ci_pattern <- regex(
  "\\bp\\s*[<>=]\\s*0?\\.\\d+|\\bp-?value|\\bconfidence interval|\\b\\d{2}%\\s*ci\\b",
  ignore_case = TRUE
)

ci_codes <- corpus |>
  mutate(
    has_ci_pvalue = str_detect(text, ci_pattern),
    n_mentions    = str_count(text, ci_pattern)
  ) |>
  filter(has_ci_pvalue) |>
  select(id, year, n_mentions) |>
  mutate(category = "Confidence intervals / p-values")

cat("CI/p-value mentions found in", nrow(ci_codes), "papers\n\n")

# ── 5. COMBINE ─────────────────────────────────────────────────────────────────
all_codes <- bind_rows(token_codes, ci_codes)

write_csv(all_codes, paper_out)
cat("✓ Paper-level coding saved to:", paper_out, "\n")

# ── 6. AGGREGATE TO YEAR ──────────────────────────────────────────────────────
# Two measures per (year, category):
#   n_papers_with_term = how many DISTINCT papers mention it at all
#   pct_papers         = that as a % of all papers published that year
#   total_mentions     = total raw mentions across the year (secondary measure)

papers_per_year <- corpus |>
  count(year, name = "n_papers_total")

by_year <- all_codes |>
  distinct(id, year, category) |>
  count(year, category, name = "n_papers_with_term") |>
  left_join(papers_per_year, by = "year") |>
  mutate(pct_papers = round(100 * n_papers_with_term / n_papers_total, 1)) |>
  left_join(
    all_codes |> group_by(year, category) |>
      summarise(total_mentions = sum(n_mentions), .groups = "drop"),
    by = c("year", "category")
  ) |>
  arrange(category, year)

write_csv(by_year, year_out)
cat("✓ Year-aggregated prevalence saved to:", year_out, "\n")




# ── 7. PLOT — FACETED TREND LINES BY CATEGORY ─────────────────────────────────
# Each category gets its own panel — much easier to read individual trends
# than one shared spaghetti plot. Free y-axis per panel since categories like
# "Behavioural terminology" (near-zero throughout) would be flattened to
# invisible if forced onto the same scale as "Term: Variability" (up to 55%).

# Order panels by overall prevalence, most-discussed first, so the reading
# order roughly matches "most to least common"
category_order <- by_year |>
  group_by(category) |>
  summarise(avg_pct = mean(pct_papers), .groups = "drop") |>
  arrange(desc(avg_pct)) |>
  pull(category)

by_year <- by_year |>
  mutate(category = factor(category, levels = category_order))

by_year <- by_year |>
  mutate(category = fct_recode(category,
                               "CI / p-values" = "Confidence intervals / p-values"
  ))

p <- ggplot(by_year, aes(x = year, y = pct_papers)) +
  geom_line(colour = "#2E75B6", linewidth = 1.5) +
  geom_point(colour = "#2E75B6", size = 1.3) +
  scale_x_continuous(breaks = seq(1980, 2024, 8)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  facet_wrap(~ category, scales = "free_y", ncol = 3) +
  labs(
    title    = "Risk-related terminology in Agronomy Australia papers, 1980\u20132024",
    subtitle = "% of papers per year mentioning each term or method category (note: y-axis scales differ by panel)",
    x = NULL,
    y = "% of papers that year",
    caption = "Dictionary-based coding (first draft) \u2014 presence of a stem/pattern is a proxy for the concept,\nnot a guarantee the paper substantively addresses it. Review dictionaries before drawing conclusions."
  ) +
  theme_minimal(base_size = 9) +
  theme(
    strip.text      = element_text(size = 9, face = "bold"),
    panel.spacing   = unit(1.2, "lines"),
    plot.title      = element_text(face = "bold", size = 12),
    plot.subtitle   = element_text(size = 9, colour = "grey40"),
    plot.caption    = element_text(size = 7, colour = "grey40", hjust = 0)
  )

p

ggsave(plot_out, p, width = 10, height = 7, dpi = 150)
cat("✓ Plot saved to:", plot_out, "\n")

# ── 6b. TREND TEST — is each category's trend statistically significant? ──────
# Simple linear regression of pct_papers ~ year, per category.
# slope = percentage points change per year (x10 for per-decade change)
# p_value < 0.05 = trend unlikely to be due to chance

trend_test <- by_year |>
  group_by(category) |>
  summarise(
    model      = list(lm(pct_papers ~ year, data = pick(everything()))),
    .groups = "drop"
  ) |>
  mutate(
    slope_per_year   = map_dbl(model, ~ coef(.x)[["year"]]),
    slope_per_decade = round(slope_per_year * 10, 2),
    p_value          = map_dbl(model, ~ summary(.x)$coefficients["year", "Pr(>|t|)"]),
    r_squared        = map_dbl(model, ~ summary(.x)$r.squared),
    significant      = if_else(p_value < 0.05, "Yes", "No")
  ) |>
  select(category, slope_per_decade, r_squared, p_value, significant) |>
  arrange(p_value)

write_csv(trend_test, file.path(output_dir, "risk_terms_trend_test.csv"))
print(trend_test, n = Inf)
cat("✓ Trend test results saved to:", file.path(output_dir, "risk_terms_trend_test.csv"), "\n")

