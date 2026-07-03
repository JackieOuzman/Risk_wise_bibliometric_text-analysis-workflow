# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 1 (annotated, v2) — drought shading on papers-per-year panel only;
# remote locations marked with circles, named in caption instead of on-plot
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)

root_dir    <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/Full_data_set/"
output_dir  <- file.path(root_dir, "outputs")

corpus_summary <- read_csv(file.path(output_dir, "corpus_summary_by_year.csv"),
                           show_col_types = FALSE)

fig1_data <- corpus_summary |>
  select(year, n_papers, avg_words_paper) |>
  pivot_longer(-year, names_to = "metric", values_to = "value") |>
  mutate(metric = recode(metric,
                         n_papers        = "Papers per conference year",
                         avg_words_paper = "Average paper length (words)"
  ),
  metric = factor(metric, levels = c(
    "Papers per conference year", "Average paper length (words)"
  )))

# Drought shading only applies to the papers-per-year panel now
drought_periods <- tibble(
  xmin = c(1991, 2001),
  xmax = c(1995, 2009),
  metric = "Papers per conference year"
) |> mutate(metric = factor(metric, levels = levels(fig1_data$metric)))

format_shift <- tibble(
  xintercept = 1996,
  metric = "Average paper length (words)"
) |> mutate(metric = factor(metric, levels = levels(fig1_data$metric)))

remote_locations <- fig1_data |>
  filter(metric == "Papers per conference year",
         year %in% c(2010, 2024))

p_fig1 <- ggplot(fig1_data, aes(x = year, y = value)) +
  geom_rect(
    data = drought_periods,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE, fill = "#D9534F", alpha = 0.12
  ) +
  geom_vline(
    data = format_shift,
    aes(xintercept = xintercept),
    linetype = "dashed", colour = "grey60", linewidth = 0.5
  ) +
  geom_line(colour = "#2E75B6", linewidth = 1.3) +
  geom_point(colour = "#2E75B6", size = 1.8) +
  geom_point(
    data = remote_locations,
    colour = "#D9534F", size = 3, shape = 1, stroke = 1.2
  ) +
  scale_x_continuous(breaks = seq(1980, 2024, 8)) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  labs(
    title = "Figure 1. Agronomy Australia Conference corpus overview, 1980\u20132024",
    x = "Conference year", y = NULL,
    caption = "Dashed line marks 1996, coinciding with an apparent shift in conference publication format\nfrom short to full-length contributed papers. Shaded bands mark major documented Australian\ndrought periods (1991\u20131995 Queensland drought; 2001\u20132009 Millennium Drought). Open circles\nmark conferences held at remote/international locations (2010 Lincoln, NZ; 2024 Albany, WA)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text    = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1.5, "lines"),
    plot.title    = element_text(face = "bold", size = 13),
    plot.caption  = element_text(size = 8, colour = "grey40", hjust = 0),
    axis.text.x   = element_text(size = 10)
  )

p_fig1

ggsave(file.path(output_dir, "fig1_corpus_overview_annotated.png"), p_fig1,
       width = 11, height = 5.5, dpi = 300)
cat("\u2713 Annotated Figure 1 saved\n")
