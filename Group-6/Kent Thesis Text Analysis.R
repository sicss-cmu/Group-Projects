
# Install needed packages if not already installed
packages <- c("dplyr", "tidytext", "SnowballC", "tidyr", "ggplot2", "stringr")
installed <- packages %in% installed.packages()
if (any(!installed)) install.packages(packages[!installed])

# Load all packages
lapply(packages, library, character.only = TRUE)

# read in data

data <- read.csv('/Users/garrettkent/Desktop/SICSS EPP Dissertations.csv') %>%
  select(date = Publication.Year, title = Title, text = Abstract.Note)

# tokenize

tidy_data <- data %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, text)

head(tidy_data)  

tidy_data %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head(20)

# stopword, noise word removal

data("stop_words")

tidy_data <- tidy_data %>%
  anti_join(stop_words)

tidy_data %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head(20)

# stemming

tidy_data <- tidy_data %>%
  mutate(word = wordStem(word, language = "en"))

tidy_data %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head(50)

library(stringr)

tidy_data <- tidy_data %>%
  filter(!word %in% c("model", "polici", "effect", "data", "result", "increas",
                      "impact", "thesi", "provid", "inform", "assess", "potenti",
                      "chang", "chapter", "studi", "base", "level"))

tidy_data %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head(50)

#  puncuation, whitespace, numbers removal

tidy_data$word <- gsub("[[:punct:]]", "", tidy_data$word)
tidy_data$word <- gsub("\\d+", "", tidy_data$word)
tidy_data <- tidy_data %>% filter(word != "")

tidy_data %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head(50)



top_words <- tidy_data %>%
  count(word) %>%
  arrange(desc(n))

top_words %>%
  slice(1:20) %>%
  ggplot(aes(x=reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Frequency")+
  xlab("")+
  ggtitle("Most Frequent Words in Theses")+
  guides(fill="none")

# by year:


# normalize by # of theses in that year


theses_per_year <- data %>%
  group_by(date) %>%
  summarise(thesis_count = n())

top_words_by_year <- tidy_data %>%
  count(date, word, sort = TRUE) %>%
  group_by(date) %>%
  slice_max(n, n = 5) %>%
  ungroup()

normalized_word_counts <- top_words_by_year %>%
  left_join(theses_per_year, by = "date") %>%
  mutate(words_per_thesis = n / thesis_count)

normalized_word_counts_filtered <- normalized_word_counts %>%
  filter(!date %in% c(2009, 2025))

ggplot(normalized_word_counts_filtered, aes(x = reorder_within(word, n, date), y = words_per_thesis, fill = word)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~date, scales = "free_x") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, size = 18)
  ) +
  ylab("Frequency") +
  xlab("") +
  ggtitle("Top Words by Year") +
  guides(fill = "none")


# normalized by # of words in that year

total_words_per_year <- tidy_data %>%
  group_by(date) %>%
  summarise(total_words = n())

top_words_by_year <- tidy_data %>%
  count(date, word, sort = TRUE) %>%
  group_by(date) %>%
  slice_max(n, n = 5) %>%
  ungroup()

relative_word_freq <- top_words_by_year %>%
  left_join(total_words_per_year, by = "date") %>%
  mutate(prop = n / total_words)


relative_word_freq_filtered <- relative_word_freq %>%
  filter(!date %in% c(2009, 2025))


ggplot(relative_word_freq_filtered, aes(x = reorder_within(word, n, date), y = prop, fill = word)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~date, scales = "free_x") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, size = 18)
  ) +
  ylab("Frequency") +
  xlab("") +
  ggtitle("Top Words by Year") +
  guides(fill = "none")


### TF-IDF (term frequency - inverse document frequency)
### highlights words in a document that are unusually frequent compared to 
### all documents in the dataset

tidy_data_tfidf <- tidy_data %>%
  count(date, word) %>%
  bind_tf_idf(word, date, n)

### Now let’s see what the most unusual words are (highest TF-IDF score)

top_tfidf <- tidy_data_tfidf %>%
  arrange(desc(tf_idf))

top_tfidf$word[1:10]

### least distinctive words

top_tfidf <- tidy_data_tfidf %>%
  arrange(tf_idf)

# View the top tf-idf word
top_tfidf$word[1:10]






# mapping specific terms usage over time

selected_words <- c("technolog")  # Use stemmed versions if stemming was applied

tidy_data %>%
  filter(word %in% selected_words) %>%
  count(date, word) %>%
  ggplot(aes(x = date, y = n, color = word)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Frequency of Selected Words by Year",
       x = "Year",
       y = "Frequency",
       color = "Word") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


#normalized based on words in that year


selected_words <- c("technolog")  # Use stemmed versions if stemming was applied

# Total words per year
total_words_per_year <- tidy_data %>%
  count(date, name = "total_words")

# Frequency of selected words
word_counts <- tidy_data %>%
  filter(word %in% selected_words) %>%
  count(date, word)

# Combine and calculate relative frequency
relative_freq <- word_counts %>%
  left_join(total_words_per_year, by = "date") %>%
  mutate(relative_frequency = n / total_words)


relative_freq_filtered <- relative_freq %>%
  filter(!date %in% c(2009, 2025))

# Plot
ggplot(relative_freq_filtered, aes(x = date, y = relative_frequency, color = word)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Relative Frequency of Selected Words by Year",
       x = "Year",
       y = "Relative Frequency",
       color = "Word") +
  scale_y_continuous(limits = c(0, 0.05)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


selected_words <- c("ev")  # Use stemmed versions if stemming was applied

# Total words per year
total_words_per_year <- tidy_data %>%
  count(date, name = "total_words")

# Frequency of selected words
word_counts <- tidy_data %>%
  filter(word %in% selected_words) %>%
  count(date, word)

# Combine and calculate relative frequency
relative_freq <- word_counts %>%
  left_join(total_words_per_year, by = "date") %>%
  mutate(relative_frequency = n / total_words)


relative_freq_filtered <- relative_freq %>%
  filter(!date %in% c(2009, 2025))

ggplot(relative_freq_filtered, aes(x = date, y = relative_frequency, color = word)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Relative Frequency of Selected Words by Year",
       x = "Year",
       y = "Relative Frequency",
       color = "Word") +
  scale_y_continuous(limits = c(0, 0.05)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


selected_words <- c("ev", "electr", "vehicl")  # Use stemmed versions if stemming was applied

# Total words per year
total_words_per_year <- tidy_data %>%
  count(date, name = "total_words")

# Frequency of selected words
word_counts <- tidy_data %>%
  filter(word %in% selected_words) %>%
  count(date, word)

# Combine and calculate relative frequency
relative_freq <- word_counts %>%
  left_join(total_words_per_year, by = "date") %>%
  mutate(relative_frequency = n / total_words)


relative_freq_filtered <- relative_freq %>%
  filter(!date %in% c(2009, 2025))

ggplot(relative_freq_filtered, aes(x = date, y = relative_frequency, color = word)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Relative Frequency of Selected Words by Year",
       x = "Year",
       y = "Relative Frequency",
       color = "Word") +
  scale_y_continuous(limits = c(0, 0.05)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16))



# bigrams to find the frequency of electric and vehicle together

tidy_data <- data %>%
  mutate(id = row_number()) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)



# plot usage of relevant bigrams on top of usage of words


selected_bigrams <- c("electric vehicle", "electric vehicles")  # Use stemmed versions if stemming was applied

# Total words per year
total_bigrams_per_year <- tidy_data %>%
  count(date, name = "total_bigrams")

# Frequency of selected words
bigram_counts <- tidy_data %>%
  filter(bigram %in% selected_bigrams) %>%
  count(date, bigram)


bigram_counts <- bigram_counts %>%
  mutate(variable = bigram, value = n)

word_counts <- word_counts %>%
  mutate(variable = word, value = n)

combined_df <- bind_rows(bigram_counts %>% select(date, variable, value),
                         word_counts %>% select(date, variable, value))

ggplot(combined_df, aes(x = date, y = value, color = variable, group = variable)) +
  geom_line(size = 1.2) +
  geom_point(size = 2)
  theme_minimal() +
  labs(title = "Counts of Selected Words/Bigrams by Year",
       x = "Year",
       y = "Count",
       color = "Word/Bigram") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))





















### Topic Modeling

library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)
library(stm)

# create document-term-matrix
  
tidy_data <- data %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, text)

dtm <- tidy_data %>%
  count(id, word) %>%
  cast_dtm(document = id, term = word, value = n)

# vary k as desired

data_topic_model<-LDA(dtm, k=5, control = list(seed = 321))

data_topics <- tidy(data_topic_model, matrix = "beta")

data_top_terms <- data_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

data_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()








