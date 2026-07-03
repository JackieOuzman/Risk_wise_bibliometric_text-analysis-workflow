# ── CHECK: does the risk trend survive controlling for paper length? ─────────
# Converts presence-based "% of papers mentioning risk" logic into a rate
# (mentions per 1000 words), to test whether the trend is driven by genuine
# increased focus on risk, or partly just by papers getting longer after 1996
# (giving more "chances" for the word to appear at least once).

risk_rate_check <- tokens |>
  filter(str_detect(word, "^risk")) |>
  count(year, name = "risk_mentions") |>
  left_join(vocab_stats, by = "year") |>
  mutate(risk_per_1000_words = round(1000 * risk_mentions / total_words, 2))

print(risk_rate_check, n = Inf)

# Quick trend test on the rate, same approach as the prevalence trend test
rate_model <- lm(risk_per_1000_words ~ year, data = risk_rate_check)
cat("\nRate trend: slope =", round(coef(rate_model)[["year"]] * 10, 2),
    "per decade, R² =", round(summary(rate_model)$r.squared, 3),
    ", p =", format.pval(summary(rate_model)$coefficients["year", "Pr(>|t|)"], digits = 3), "\n")
