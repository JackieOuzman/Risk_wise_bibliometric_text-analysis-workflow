library(tidyverse)
library(pdftools)
library(tidytext)
library(SnowballC)
#install.packages("tidytext")



#


###################################################################################
#Step 1 — Start from the raw pages (page structure intact)
# Re-extract with pages intact for this paper
raw_pages <- suppressWarnings(
  pdf_text("D:/work/RiskWise/Brendan_Ag_Conf_papers/papers_subset/2024_0144.pdf")
)


# Collapse to one string (matching how your corpus was built)
paper_text <- paste(raw_pages, collapse = " ")

#Step 2 — Put into a mini dataframe matching your corpus structure

single_paper <- tibble(
  id   = "2024_0144",
  year = 2024,
  text = paper_text
)

#Step 3 — Tokenise

tokens <- single_paper %>%
  unnest_tokens(word, text)

nrow(tokens)  # how many raw tokens # 2321
head(tokens, 20)

#Step 4 — Filter to letters only
tokens_filtered <- tokens %>%
  filter(str_detect(word, "^[a-z]+$"))

nrow(tokens_filtered)  # how many survived - compare to above #2117 #we lost 204

#Step 5 — Remove stopwords

# Your custom stopword list
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
  "proceedings", "ert", "lst",
  # --- ADD NEW WORDS HERE ---
  "official", "albany"  # added from 2024_0144 diagnostic
))

tokens_clean <- tokens_filtered %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stops, by = "word")

nrow(tokens_clean) #1013

#Step 6 — Stem


tokens_stemmed <- tokens_clean %>%
  mutate(stem = wordStem(word))

# Useful to see: what words map to each stem?
tokens_stemmed %>%
  group_by(stem) %>%
  summarise(
    n          = n(),
    examples   = paste(unique(word)[1:min(3, n())], collapse = ", ")
  ) %>%
  arrange(desc(n)) %>%
  head(20)


#Step 7 — Count and view top terms
top_terms <- tokens_stemmed %>%
  count(stem, sort = TRUE) %>%
  slice_max(n, n = 20)

print(top_terms)


#Step 8 — Quick plot
top_terms %>%
  slice_max(n, n = 15) %>%
  mutate(stem = reorder(stem, n)) %>%
  ggplot(aes(x = n, y = stem)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Top 15 stems — 2024_0144",
    subtitle = "After filtering, stopword removal and stemming",
    x = "Count", y = NULL
  ) +
  theme_minimal()



#Step 9 
# Token journey summary
write_csv(tokens_stemmed, "D:/work/RiskWise/Brendan_Ag_Conf_papers/example workflow with one paper/token_journey_2024_0144.csv")

# Top terms
write_csv(top_terms, "D:/work/RiskWise/Brendan_Ag_Conf_papers/example workflow with one paper/top_terms_2024_0144.csv")

# Plot
ggsave("D:/work/RiskWise/Brendan_Ag_Conf_papers/example workflow with one paper/top_terms_2024_0144.png", 
       width = 8, height = 6, dpi = 150)
