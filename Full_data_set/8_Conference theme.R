# ══════════════════════════════════════════════════════════════════════════════
# TABLE — Conference theme, year, location, number of papers
# ══════════════════════════════════════════════════════════════════════════════
themes_in <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/Full_data_set/conference_themes.csv"
conference_themes <- read_csv(themes_in, show_col_types = FALSE)

papers_per_year <- corpus |> count(year, name = "n_papers")

table_conf <- conference_themes |>
  left_join(papers_per_year, by = "year") |>
  select(year, theme, location, state, n_papers) |>
  arrange(year) |>
  rename(
    `Year`     = year,
    `Theme`    = theme,
    `Location` = location,
    `State`    = state,
    `N papers` = n_papers
  )

write_csv(table_conf, file.path(output_dir, "table_conference_overview.csv"))
print(table_conf, n = Inf)
cat("✓ Table saved to:", file.path(output_dir, "table_conference_overview.csv"), "\n"
    