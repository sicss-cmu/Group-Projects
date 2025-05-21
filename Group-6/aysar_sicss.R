# Load libraries
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# Read dataset
data <- read_csv("sicss_data.csv")

# Define methods (including model stems)
methods_keywords <- c(
  "case stud(y|ies)",
  "simulat(e|ed|ion|ions|ing)",
  "experiment(s|al|ing)",
  "qualitative(ly)",
  "interview(s|ed|ing)",
  "survey(s|ed|ing)",
  "questionnaire(s)",
  "statistical(ly)",
  "regression(s)",
  "focus group(s)",
  "observation(s|al)",
  "content analysis",
  "mixed method(s)",
  "time series",
  "longitudinal",
  "ethnograph(y|ic|ies)",
  "comparative stud(y|ies)",
  "quasi[- ]?experiment(al|s)",
  "secondary data",
  "archival data"
  # "model(s|ing|ed)" ← REMOVE this line!
)


# Model-specific detection logic
detect_model_labels <- function(text) {
  if (is.na(text)) return(character(0))
  text <- tolower(text)
  models <- c()
  if (str_detect(text, "simulation model|simulate|simulated|simulating|simulation")) {
    models <- c(models, "Simulation Model")
  }
  if (str_detect(text, "statistical model|regression|probability|likelihood|bayesian")) {
    models <- c(models, "Statistical Model")
  }
  if (str_detect(text, "computational model|algorithm|optimization|solver")) {
    models <- c(models, "Computational Model")
  }
  return(models)
}

# Function to detect all methods (including model types)
detect_all_methods <- function(text, keywords) {
  if (is.na(text)) return(NA)
  text <- tolower(text)
  methods_found <- keywords[sapply(keywords, function(k) str_detect(text, regex(k, ignore_case = TRUE)))]
  model_labels <- detect_model_labels(text)
  all_found <- unique(c(methods_found, model_labels))
  if (length(all_found) == 0) return(NA)
  paste(all_found, collapse = "; ")
}

# Apply detection
results <- data %>%
  mutate(
    Detected_Methods = sapply(`Abstract Note`, detect_all_methods, keywords = methods_keywords)
  ) %>%
  select(Title, Author, `Publication Year`, Detected_Methods, `Abstract Note`)

# Save results
write_csv(results, "theses_with_all_detected_methods.csv")

# Plot preparation: one row per method
methods_plot <- results %>%
  filter(!is.na(Detected_Methods)) %>%
  separate_rows(Detected_Methods, sep = ";\\s*") %>%
  group_by(Title, Detected_Methods) %>%  # Group by thesis and method to ensure unique counts per thesis
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(Detected_Methods) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Plot all methods including models
# Final bar chart with count labels
ggplot(methods_plot, aes(x = reorder(Detected_Methods, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = Count), hjust = -0.5, size = 3.5) +
  coord_flip() +
  labs(
    title = "Frequency of Detected Scientific Methods",
    x = "Detected Method",
    y = "Number of Theses"
  ) +
  theme_minimal() +
  ylim(0, max(methods_plot$Count) * 1.1)  # Add extra space for count labels

########################################

# Assuming you already have `results` from previous steps:
# results <- data %>% mutate(...)

library(ggplot2)
library(dplyr)
library(tidyr)

# Step 1: Expand methods into individual rows
method_year_trends <- results %>%
  filter(!is.na(Detected_Methods)) %>%
  separate_rows(Detected_Methods, sep = ";\\s*") %>%
  group_by(`Publication Year`, Detected_Methods) %>%
  summarise(Count = n(), .groups = "drop")

# Optional: filter out very rare methods for clarity
# method_year_trends <- method_year_trends %>%
#   group_by(Detected_Methods) %>%
#   filter(sum(Count) >= 3)  # show only methods that appeared 3+ times in total

# Step 2: Plot ONE
ggplot(method_year_trends, aes(x = `Publication Year`, y = Count, color = Detected_Methods, group = Detected_Methods)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = unique(method_year_trends$`Publication Year`)) +  # show all years
  labs(
    title = "Trends of Scientific Methods by Publication Year",
    x = "Year",
    y = "Number of Theses",
    color = "Method"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Step 2: Plot TWO
ggplot(method_year_trends, aes(x = `Publication Year`, y = Count, fill = Detected_Methods)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Stacked Bar Chart of Method Usage by Year",
    x = "Year",
    y = "Number of Theses",
    fill = "Method"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



