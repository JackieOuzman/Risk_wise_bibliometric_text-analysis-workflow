#install.packages("stringdist")
library(tidyverse)
library(stringdist)


# ── CONFIGURATION ─────────────────────────────────────────────────────────────
index_path  <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/paper_index.csv"
output_path <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/example_duplicates.csv"
# ──────────────────────────────────────────────────────────────────────────────

# Load index fresh from CSV
index <- read_csv(index_path)

# Find exact title duplicates
exact_dupes <- index |>
  group_by(title) |>
  filter(n() > 1) |>
  ungroup()

# Pull 3 example duplicated titles and save in same format as index
example_titles <- exact_dupes |>
  distinct(title) |>
  slice(1:3) |>
  pull(title)

index |>
  filter(title %in% example_titles) |>
  arrange(title, year) |>
  write_csv(output_path)

