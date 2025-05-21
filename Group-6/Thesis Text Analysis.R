
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

tidy_data <- tidy_data %>%
  filter(!word %in% c("chapter", "data", "study", "thesis", "results", "analysis", "based",
                      "dissertation", "research", "model", "policy"))

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

# stemming

tidy_data <- tidy_data %>%
  mutate(word = wordStem(word, language = "en"))

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




# Count word frequencies per year
top_words_by_year <- tidy_data %>%
  count(date, word, sort = TRUE) %>%
  group_by(date) %>%
  slice_max(n, n = 5) %>%
  ungroup()

# Plot
ggplot(top_words_by_year, aes(x = reorder_within(word, n, date), y = n, fill = word)) +
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








### Topic Modeling

library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)
library(stm)

# create document-term-matrix

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










































