library(readxl)
library(dplyr)
library(ggplot2)
library(viridis)
library(tidyr)

# Install needed packages if not already installed
packages <- c("dplyr", "tidytext", "SnowballC", "tidyr", "ggplot2", "stringr")
installed <- packages %in% installed.packages()
if (any(!installed)) install.packages(packages[!installed])

# Load all packages
lapply(packages, library, character.only = TRUE)

#Importing the Dissertation Data 
SICSS_EPP_Dissertations <- read.csv("SICSS EPP Dissertations.csv")
View(SICSS_EPP_Dissertations)

#New datafram that has removed columns with no data 
SICCS_dis <- SICSS_EPP_Dissertations %>% select_if(~ !any(is.na(.)))
  
#Finding the most common words in the titles 
dis_title_abstract <- SICCS_dis %>%
  select(Publication.Year,Title, Abstract.Note)

####
####PLOTTING TITLES
dis_data <- SICSS_EPP_Dissertations %>%
  select(date = Publication.Year, text = Title) 

#Aspect of tolkenizing 
tidy_dis <- dis_data %>%
  unnest_tokens(word, text)

#Removing the stop words 
data("stop_words")

tidy_dis <- tidy_dis %>%
  anti_join(stop_words) #removes the list of stop words


#Counting the number of words 
high_count <- tidy_dis %>%
  count(word) %>%
  arrange(desc(n))

#### PLOTTING THE COUNT OF DISSERTATIONS WORDS

high_count %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Frequency")+
  xlab("")+
  ggtitle("Most Frequent Words in Dissertation Titles")+
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
  anti_join(stop_words) #removes the list of stop words


#Counting the number of words 
high_count <- tidy_abs %>%
  count(word) %>%
  arrange(desc(n))

####
high_count %>%
  slice(1:10) %>%
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

energy_term <-  c("energy", "electricity", "power", "renewable", "gas", 
  "efficiency", "emissions", "carbon", "decarbonization", "grid", "battery",
  "storage", "infrastructure", "utility", "distribution",
  "transmission", "consumption", "demand", "load", "smart grid",
  "greenhouse", "climate", "clean energy", "PV", "photovoltaic",
  "energy justice", "electrification")

# 2. Convert date if not already
str(tidy_abs$word)  # Check the type

#tidy_abs$date <- as.Date(tidy_abs$date)


#tidy_abs <- tidy_abs %>%
 # mutate(year = as.numeric(format(date, "%Y")))

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
  geom_line(color = "blue", size = 0.8) +
  geom_point(color = "blue", size = 2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 13),
    plot.title = element_text(hjust = 0.5, size = 18)
  ) +
  ylab("Energy-Related Term Count") +
  xlab("Year") +
  ggtitle("Energy Terms Usage by Year")

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

#### Energy-Related Terms per Total Words Per Year 

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
  ylab("Rationof Energy Terms") +
  xlab("Year") +
  ggtitle("Energy-Related Terms as Proportion of Total Abstract Words by Year")
