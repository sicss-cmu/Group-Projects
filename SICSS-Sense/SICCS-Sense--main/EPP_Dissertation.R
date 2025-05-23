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
#SICSS_EPP_Dissertations <- read.csv("SICSS EPP Dissertations.csv")
View(SICSS_EPP_Dissertations)

#New datafram that has removed columns with no data 
SICCS_dis <- SICSS_EPP_Dissertations %>% select_if(~ !any(is.na(.)))
  
#Finding the most common words in the titles 
title <- SICCS_dis %>%
  select(Date,Title, Abstract.Note)

####
####PLOTTING TITLES
dis_data <- SICSS_EPP_Dissertations %>%
  select(date = Date, text = Title) 

head(dis_data)

#Aspect of tolkenizing 
tidy_dis <- dis_data %>%
  unnest_tokens(word, text)

#Removing the stop words 
data("stop_words")

tidy_dis <- tidy_dis %>%
  anti_join(stop_words) #removes the list of stop words

head(tidy_dis)


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
  select(date = Date, text = Abstract.Note) 

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

tidy_abs$date <- as.Date(tidy_abs$date)


tidy_abs <- tidy_abs %>%
  mutate(year = as.numeric(format(date, "%Y")))

# 3. Filter rows with energy-related keywords in text (regex OR pattern)

energy_dis <- tidy_abs %>%
  filter(word %in% energy_term)

# 4. Tokenize and count matching words
energy_token_counts <- energy_dis %>%
  count(date, word)

# 5. Aggregate to daily usage
energy_yearly_counts <- energy_dis %>%
  mutate(year = year(date)) %>%
  count(year, word)

energy_annual_counts <- energy_yearly_counts %>%
  group_by(year) %>%
  summarise(n = sum(n), .groups = "drop")


# 6. Plot time trend
ggplot(energy_annual_counts, aes(x = year, y = n)) +
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

