library(readxl)
library(dplyr)
library(ggplot2)
library(viridis)
library(tidyr)
library(stm)

# Install needed packages if not already installed
packages <- c("dplyr", "tidytext", "SnowballC", "tidyr", "ggplot2", "stringr")
installed <- packages %in% installed.packages()
if (any(!installed)) install.packages(packages[!installed])

# Load all packages
lapply(packages, library, character.only = TRUE)

#Importing the Dissertation Data 
SICSS_EPP_Dissertations <- read.csv("SICSS EPP Dissertations.csv")

SICSS_EPP_Dissertations <- SICSS_EPP_Dissertations %>%
  group_by(Publication.Year) %>%
  mutate(n_dissertations = n()) %>%
  filter(n_dissertations > 3)

#New datafram that has removed columns with no data 
SICCS_dis <- SICSS_EPP_Dissertations %>% select_if(~ !any(is.na(.)))
  
#Finding the most common words in the titles 
dis_title_abstract <- SICCS_dis %>%
  select(Publication.Year,Key, Title, Abstract.Note)
####
####PLOTTING TITLES
dis_data <- SICSS_EPP_Dissertations %>%
  select(date = Publication.Year, text = Title) 

#Aspect of tolkenizing 
tidy_dis <- dis_data %>%
  unnest_tokens(word, text)

#Removing the stop words 
data("stop_words")
custom_stopwords <- c("thesis", "abstract", "dissertation", "university", "research", "study", "studies", "student", "develop")

tidy_dis <- tidy_dis %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word %in% custom_stopwords) %>%
  mutate(word = wordStem(word, language = "en"))

high_count <- tidy_dis %>%
  count(word) %>%
  arrange(desc(n))

view(high_count)
#### PLOTTING THE COUNT OF DISSERTATIONS WORDS

high_count %>%
  slice(1:4) %>%
  ggplot(aes(x=reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme(
    plot.background = element_rect(fill = "#242320", color = "#242320"),
    panel.background = element_rect(fill = "#242320"),
    panel.grid = element_blank(),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "#00FFFF", size = 13),
    plot.title = element_text(color = "#FFA500", size = 10, family = "arcade"),
    plot.subtitle = element_text(color = "#FFA500", size = 18, family = "handdrawn"),
    axis.line = element_line(color = "white"),
    legend.position = c(0.5,0.2),
    legend.title = element_text(color = "#00FFFF"),
    legend.text = element_text(color = "#00FFFF"),
    legend.background = element_rect(fill = "#242320", color = "#242320")
  )+
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Frequency")+
  xlab("")+
  ggtitle("Most Frequent Words in \n Dissertation Abstracts")+
  guides(fill=FALSE)

#### ABSTRACTS 

#Selecting for the Date and the Title 
dis_data_abs <- SICSS_EPP_Dissertations %>%
  select(date = Publication.Year, text = Abstract.Note) 

#Aspect of tolkenizing 
tidy_abs <-dis_data_abs %>%
  unnest_tokens(word, text)

#Removing the stop words 
data("stop_words")

tidy_abs <- tidy_abs %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word %in% custom_stopwords) %>%
  mutate(word = wordStem(word, language = "en"))

#Counting the number of words 
high_count <- tidy_abs %>%
  count(word) %>%
  arrange(desc(n))

view(high_count)
####
high_count %>%
  slice(1:3) %>%
  ggplot(aes(x=reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Frequency")+
  xlab("")+
  ggtitle("Most Frequent Words in Dissertation Abstracts")+
  guides(fill=FALSE)


########


energy_term <- c("energy", "electricity","electric", "power", "grid", "utility", "smart grid",
  "transmission", "distribution", "electrification", "load", 
  "outage", "demand response", "microgrid", "storage", "battery", "energy justice")

climate_term <- c("climate", "climate change", "global warming", "emissions", "carbon",
  "carbon dioxide", "greenhouse gases", "GHG", "carbon footprint", 
  "climate mitigation", "climate adaptation", "resilience", "climate policy")

# 2. Convert date if not already
str(tidy_abs$word)  # Check the type

# 3. Filter rows with energy-related keywords in text (regex OR pattern)

energy_dis <- tidy_abs %>%
  filter(word %in% energy_term)

# 4. Tokenize and count matching words
energy_token_counts <- energy_dis %>%
  count(date, word)

# 5. Aggregate to daily usage
energy_yearly_counts <- energy_dis %>%
  count(date, word)

energy_annual_counts <- energy_yearly_counts %>%
  group_by(date) %>%
  summarise(n = sum(n), .groups = "drop")


# 6. Plot time trend
ggplot(energy_annual_counts, aes(x = date, y = n)) +
  geom_point(size = 3, color = "#FFA500") +
  geom_line(size = 1, color = "#FFA500") +
  #geom_smooth(alpha = 0.2) +
  theme(
    plot.background = element_rect(fill = "#242320", color = "#242320"),
    panel.background = element_rect(fill = "#242320"),
    panel.grid = element_blank(),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "#00FFFF", size = 16),
    plot.title = element_text(color = "#FFA500", size = 10, family = "arcade"),
    plot.subtitle = element_text(color = "#FFA500", size = 18, family = "handdrawn"),
    axis.line = element_line(color = "white"),
    legend.position = c(0.5,0.2),
    legend.title = element_text(color = "#00FFFF"),
    legend.text = element_text(color = "#00FFFF"),
    legend.background = element_rect(fill = "#242320", color = "#242320")
  ) +
  ylab("Energy-Related Term Count") +
  xlab("Year") +
  ggtitle("Energy Terms in Abstracts by Year")

# Maybe next step to normalize the number of thesis by the year? 

year_count <- SICCS_dis %>%
  group_by(Publication.Year) %>%
  count()

word_by_year <- energy_dis %>%
  group_by(date) %>%
  count()

year_count <- year_count %>%
  left_join(word_by_year, by = c("Publication.Year" = "date")) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  mutate(energy_per_year = n.y/n.x)

####
ggplot(year_count, aes(x = Publication.Year, y = energy_per_year)) +
  geom_line(color = "red", size = 0.8) +
  geom_point(color = "red", size = 2) +
  # geom_smooth(method = "lm") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 13),
    plot.title = element_text(hjust = 0.5, size = 18)
  ) +
  ylab("Ration of Total Energy Terms to Dissertation") +
  xlab("Year") +
  ggtitle("Energy-Related Terms Per Dissertation by Year")

summary(lm(energy_per_year ~ Publication.Year, data = year_count))

### ----------------------------
### ENERGY TERMS PER TOTAL ABSTRACT WORDS
### ----------------------------
library(lubridate)

# 2. Total abstract words per year
tidy_abs_clean <- tidy_abs %>%
  anti_join(stop_words)

total_words_per_year <- tidy_abs_clean %>%
  count(date, name = "total_words")

# 3. Energy words per year (you already filtered to `energy_dis`)
energy_words_per_year <- energy_dis %>%
  count(date, name = "energy_word")

# 4. Join datasets and compute energy words per total words
energy_word_ratio <- left_join(total_words_per_year, energy_words_per_year, by = "date") %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  mutate(ratio = energy_word / total_words)

# 5. Plot the ratio
ggplot(energy_word_ratio, aes(x = date, y = ratio)) +
  geom_line(color = "darkgreen", size = 0.8) +
  geom_point(color = "darkgreen", size = 2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 13),
    plot.title = element_text(hjust = 0.5, size = 18)
  ) +
  ylab("Ratio of Energy Terms") +
  xlab("Year") +
  ggtitle("Energy-Related Terms as Proportion of Total Abstract Words by Year")

### ----------------------------
### CLIMATE TERMS PER TOTAL ABSTRACT WORDS
### ----------------------------

climate_dis <- tidy_abs %>%
  filter(word %in% climate_term)

# 4. Tokenize and count matching words
climate_token_counts <- climate_dis %>%
  count(date, word)

# 5. Aggregate to daily usage
climate_yearly_counts <- climate_dis %>%
  count(date, word)

climate_annual_counts <- climate_yearly_counts %>%
  group_by(date) %>%
  summarise(n = sum(n), .groups = "drop")

# 3. Energy words per year (you already filtered to `energy_dis`)
climate_words_per_year <- climate_dis %>%
  count(date, name = "climate_word")

# 4. Join datasets and compute energy words per total words
climate_word_ratio <- left_join(total_words_per_year, climate_words_per_year, by = "date") %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  mutate(ratio = climate_word / total_words)

# 5. Plot the ratio
ggplot(climate_word_ratio, aes(x = date, y = ratio)) +
  geom_line(color = "darkgreen", size = 0.8) +
  geom_point(color = "darkgreen", size = 2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 13),
    plot.title = element_text(hjust = 0.5, size = 18)
  ) +
  ylab("Ratio of Climate Terms") +
  xlab("Year") +
  ggtitle("Climate-Related Terms as Proportion of Total Abstract Words by Year")


#FILTER OUT DISSERTATIONS WITH LESS THAN 3 DISSERTATIONS 
# Create one row per dissertation, per word

# Total words per dissertation
total_words_per_diss <- SICSS_EPP_Dissertations %>%
  select(Publication.Year, Key, text = Abstract.Note) %>%
  group_by(Publication.Year, Key) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  summarise(total_word_count = n(), .groups = "drop")

climate_dis_per_diss <- SICSS_EPP_Dissertations %>%
  select(Publication.Year, Key, text = Abstract.Note) %>%
  group_by(Publication.Year, Key) %>%
  unnest_tokens(word, text) %>%
  filter(word %in% climate_term) %>%
  summarise(climate_word_count = n(), .groups = "drop")

climate_ratio_per_diss <- left_join(total_words_per_diss, climate_dis_per_diss,
                                    by = c("Publication.Year", "Key")) %>%
  mutate(climate_word_count = replace_na(climate_word_count, 0)) %>%
  mutate(ratio = climate_word_count / total_word_count)

# 4. Compute summary statistics by year
climate_summary <- climate_ratio_per_diss %>%
  group_by(Publication.Year) %>%
  summarise(
    mean_ratio = mean(ratio, na.rm = TRUE),
    sd_ratio = sd(ratio, na.rm = TRUE),
    n = n(),
    se_ratio = sd_ratio / sqrt(n),
    .groups = "drop"
  )

# 5. Plot with error bars
ggplot(climate_summary, aes(x = Publication.Year, y = mean_ratio)) +
  geom_line(color = "darkgreen", size = 0.8) +
  geom_point(color = "darkgreen", size = 2) +
  geom_errorbar(aes(ymin = mean_ratio - se_ratio, ymax = mean_ratio + se_ratio),
                width = 0.2, color = "darkgreen") +
  geom_smooth() +
  theme_minimal() +
  labs(
    title = "Climate Terms as Proportion of Total Abstract Words Per Dissertation (with Error Bars)",
    x = "Year",
    y = "Mean Proportion of Climate Terms"
  )


ggplot(climate_summary, aes(x = Publication.Year, y = mean_ratio)) +
  # Line for trend
  geom_line(color = "#1b9e77", size = 1.2) +
  
  # Error bars
  geom_errorbar(
    aes(ymin = mean_ratio - se_ratio, ymax = mean_ratio + se_ratio),
    width = 0.3, color = "#7570b3", alpha = 0.5, linewidth = 1
  ) +
  
  # Data points with border
  geom_point(color = "white", fill = "#d95f02", size = 4, shape = 21, stroke = 1.2) +
  
  # Optional smoothing (commented out)
  # geom_smooth(method = "loess", se = FALSE, color = "gray40", linetype = "dashed") +
  
  # Axis and label formatting
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = seq(min(climate_summary$Publication.Year),
                                  max(climate_summary$Publication.Year), 1)) +
  
  # Themes and titles
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 14)
  ) +
  
  labs(
    title = "Use of Climate-Related Terms in Dissertations Over Time",
    subtitle = "Proportion of abstract words that are climate-related (with standard error)",
    x = "Publication Year",
    y = "Mean Proportion of Climate Terms"
  )

#################-----


#################-----

energy_dis_per_diss <- SICSS_EPP_Dissertations %>%
  select(Publication.Year, Key, text = Abstract.Note) %>%
  group_by(Publication.Year, Key) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word %in% custom_stopwords) %>%
  filter(word %in% energy_term) %>%
  summarise(energy_word_count = n(), .groups = "drop")

energy_ratio_per_diss <- left_join(total_words_per_diss, energy_dis_per_diss,
                                    by = c("Publication.Year", "Key")) %>%
  mutate(energy_word_count = replace_na(energy_word_count, 0)) %>%
  mutate(ratio = energy_word_count / total_word_count)

# 4. Compute summary statistics by year
energy_summary <- energy_ratio_per_diss %>%
  group_by(Publication.Year) %>%
  summarise(
    mean_ratio = mean(ratio, na.rm = TRUE),
    sd_ratio = sd(ratio, na.rm = TRUE),
    n = n(),
    se_ratio = sd_ratio / sqrt(n),
    .groups = "drop"
  )

# 5. Plot with error bars
ggplot(energy_summary, aes(x = Publication.Year, y = mean_ratio)) +
  geom_point(size = 3, color = "#FFA500") +
  #geom_line(size = 1, color = "#FFA500") +
  geom_smooth(alpha = 0.2) +
  theme(
    plot.background = element_rect(fill = "#242320", color = "#242320"),
    panel.background = element_rect(fill = "#242320"),
    panel.grid = element_blank(),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "#00FFFF", size = 16),
    plot.title = element_text(color = "#FFA500", size = 10, family = "arcade"),
    plot.subtitle = element_text(color = "#FFA500", size = 18, family = "handdrawn"),
    axis.line = element_line(color = "white"),
    legend.position = c(0.5,0.2),
    legend.title = element_text(color = "#00FFFF"),
    legend.text = element_text(color = "#00FFFF"),
    legend.background = element_rect(fill = "#242320", color = "#242320")
  ) +
  labs(
    title = "Energy Terms as Proportion of Total Abstract Words Per Dissertation",
    x = "Year",
    y = "Mean Proportion of energy Terms"
  )
