#subset the data

library(tidyverse)
library(fs)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
flat_dir   <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/papers_flat"
index_path <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/paper_index.csv"
subset_dir <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/papers_subset"
subset_out <- "N:/work/RiskWise/Brendan_Ag_Conf_papers/paper_index_subset.csv"

papers_per_year <- 5
set.seed(42)  # makes the sample reproducible
# ──────────────────────────────────────────────────────────────────────────────

index <- read_csv(index_path)

# Sample evenly across years
subset_index <- index |>
  group_by(year) |>
  slice_sample(n = papers_per_year) |>
  ungroup() |>
  arrange(year, paper_num)

# Check coverage
cat("Subset size:", nrow(subset_index), "papers\n")
cat("Papers per year:\n")
print(count(subset_index, year), n = 30)

# Copy files to subset folder
dir_create(subset_dir)

walk(subset_index$filename_new, \(f) {
  file_copy(
    file.path(flat_dir, f),
    file.path(subset_dir, f),
    overwrite = FALSE
  )
})

# Save subset index
write_csv(subset_index, subset_out)
cat("\n✓ Subset copied to:", subset_dir, "\n")
cat("✓ Subset index saved to:", subset_out, "\n")
