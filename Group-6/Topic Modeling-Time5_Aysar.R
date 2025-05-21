# load required packages
install.packages(c("tidytext", "dplyr", "topicmodels", "ggplot2", "tm", "tidyr", "textclean"))
library(tidytext)
library(dplyr)
library(topicmodels)
library(ggplot2)
library(tm)
library(tidyr)
library(textclean)
library(stringr)

# load your dataset
data <- read.csv("sicss_data.csv", stringsAsFactors = FALSE)

# repare years and group into 5-year periods
data <- data %>%
  rename(
    Year = Publication.Year,
    Abstract = Abstract.Note
  ) %>%
  filter(!is.na(Abstract)) %>%
  mutate(
    Year = as.integer(Year),
    Period = cut(
      Year,
      breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025, 2030),
      labels = c("1990–1994", "1995–1999", "2000–2004", "2005–2009",
                 "2010–2014", "2015–2019", "2020–2024", "2025–2029"),
      include.lowest = TRUE,
      right = FALSE
    )
  )

# set number of topics
k_topics <- 3
periods <- unique(data$Period)

# Run LDA for each time period and show the result
for (p in periods) {
  cat("\n🔄 Processing period:", p, "\n")
  
  abstract_data <- data %>%
    filter(Period == p) %>%
    select(ID = Key, text = Abstract) %>%
    mutate(
      text = iconv(text, to = "ASCII//TRANSLIT"),
      text = tolower(text)
    )
  
  if (nrow(abstract_data) < 1) {
    cat("⚠️ Not enough documents. Skipping.\n")
    next
  }
  
  tokens <- abstract_data %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words, by = "word") %>%
    filter(str_detect(word, "^[a-z]+$"))
  
  dtm <- tokens %>%
    count(ID, word) %>%
    cast_dtm(document = ID, term = word, value = n)
  
  if (nrow(dtm) < 1 || ncol(dtm) < 10) {
    cat("⚠️ DTM too sparse. Skipping.\n")
    next
  }
  
  lda_model <- LDA(dtm, k = k_topics, control = list(seed = 123))
  
  top_terms <- tidy(lda_model, matrix = "beta") %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup()
  
  plot <- ggplot(top_terms, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y") +
    coord_flip() +
    labs(
      title = paste("Top Terms per Topic:", as.character(p)),
      x = "Term",
      y = "Beta (Probability)"
    ) +
    theme_minimal()
  print(plot)
  ggsave(filename = paste0("topic_plot_", gsub("[^0-9]", "", as.character(p)), ".png"),
         plot = plot, width = 8, height = 5)
}





