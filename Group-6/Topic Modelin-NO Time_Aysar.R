# Load libraries
install.packages(c("tidytext", "dplyr", "topicmodels", "ggplot2", "tm", "tidyr", "textclean"))
install.packages("topicmodels")
library(tidytext)
library(dplyr)
library(topicmodels)
library(tm)
library(tidyr)
library(textclean)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)

# Load your dataset
data <- read.csv("sicss_data.csv", stringsAsFactors = FALSE)

# Clean and select relevant data
abstract_data <- data %>%
  select(ID = Key, text = Abstract.Note) %>%
  filter(!is.na(text)) %>%
  mutate(text = iconv(text, to = "ASCII//TRANSLIT"),
         text = tolower(text))

# Tokenize (word level)
tokens <- abstract_data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(str_detect(word, "^[a-z]+$"))  # keep only alphabetic words

# Create Document-Term Matrix
dtm <- tokens %>%
  count(ID, word) %>%
  cast_dtm(document = ID, term = word, value = n)

# Run LDA Topic Model
k_topics <- 5  # You can change this number
lda_model <- LDA(dtm, k = k_topics, control = list(seed = 123))

# Get Top Terms Per Topic
top_terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Visualize Topics
library(scales)
library(forcats)

ggplot(top_terms, aes(x = fct_reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 2) +
  coord_flip() +
  labs(
    title = "Top Terms per Topic",
    x = NULL,
    y = "Beta (Probability)"
  ) +
  theme_minimal()


