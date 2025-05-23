# install packages
library(datasets)
library(tidyverse)
library(dplyr)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(showtext)
library(ggtext)

# import texts for 
font_add_google(name = "Press Start 2P", family = "arcade")
font_add_google(name = "Gloria Hallelujah", family = "handdrawn")
showtext_auto()

# function to compute density of keyword
get_keyword_density_df <- function(keywords, token_df, total_word_df) {
  keywords <- tolower(keywords)
  
  df_hits <- token_df %>%
    mutate(hit = word %in% keywords) %>%
    group_by(key, year) %>%
    summarize(keyword_hits = sum(hit), .groups = "drop")
  
  df_density <- df_hits %>%
    left_join(total_word_df, by = c("key", "year")) %>%
    mutate(density = keyword_hits / total_words)
  
  return(df_density)
}
# compute statistics for keyword density by year
summarize_density <- function(df_density) {
  df_density %>%
    group_by(year) %>%
    summarize(
      mean_density = mean(density),
      se = sd(density) / sqrt(n()),
      lower = mean_density - 1.96 * se,
      upper = mean_density + 1.96 * se,
      n = n(),
      .groups = "drop"
    )
}
# generate plot with spooky theme
plot_keyword_density_theme <- function(keywords, token_df, total_word_df) {
  df_density <- get_keyword_density_df(keywords, token_df, total_word_df)
  df_summary <- summarize_density(df_density)
  
  ggplot(df_summary, aes(x = year, y = mean_density)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4, size = 1, color = "#FFA500") +  # tomato red
    geom_point(size = 3, color = "#E05EEB") +  # cyan
    geom_hline(yintercept = 0, size = 1, color = "white", linetype = "dotted") +
    # geom_line(color = "#FFA500", size = 1.2) +  # orange line
    theme_minimal(base_family = "arcade", base_size = 24) +
    labs(
      title = paste0("Keyword: ", toupper(paste(keywords, collapse = ", "))),
      # subtitle = "A Ghostly Hero's Journey through Dissertation Abstracts",
      x = "Year",
      y = "Proportion (of words in abstract)"
    ) +
    theme(
      plot.background = element_rect(fill = "#242320", color = "#242320"),
      panel.background = element_rect(fill = "#242320"),
      panel.grid = element_blank(),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "#00FFFF", size = 16),
      plot.title = element_text(color = "#FFA500", size = 24, family = "arcade"),
      plot.subtitle = element_text(color = "#FFA500", size = 18, family = "handdrawn"),
      axis.line = element_line(color = "white")
    )
}

# Define a server for the Shiny app
function(input, output) {
  # Load and clean data
  df_raw <- read.csv("../data/epp_dissertations.csv") %>%
    rename_with(tolower) %>%
    rename(abstract = abstract.note,
           year = publication.year) %>%
    mutate(abstract_length = str_count(abstract, "\\S+")) %>%
    filter(abstract_length > 50) %>%
    group_by(year) %>%
    mutate(n_dissertations = n()) %>%
    filter(n_dissertations > 3)
  
  # Combine standard and domain-specific stop words
  stopwords_all <- stop_words %>%
    bind_rows(tibble(word = c("dissertation", "chapter", "study", "thesis", "research")))
  
  # Tokenize and remove stopwords
  df_tokens <- df_raw %>%
    select(key, year, abstract) %>%
    group_by(year , key) %>%
    unnest_tokens(word, abstract, drop = F) %>%
    anti_join(stopwords_all, by = "word") %>%
    filter(!str_detect(word, "\\d"))  # remove numeric tokens
  # Compute total words
  df_total_words <- df_tokens %>%
    group_by(key, year) %>%
    summarize(total_words = n(), .groups = "drop")
  
  # Fill in the spot we created for a plot
  output$wordPlot <- renderPlot({
    # Create plot
    plot_keyword_density_theme(c(input$word), df_tokens, df_total_words)

  })
}
