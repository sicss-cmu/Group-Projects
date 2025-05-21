setwd("/Users/hmorin/Documents/GitHub/SICSS/Group-Projects/Group-6")

library(tidyverse)
library(stringr)
library(dplyr)

data <- read.csv("SICSS EPP Dissertations.csv", stringsAsFactors = FALSE)

#-CMU Summer Institute CSS Network-------------------------------------------------
#
#install.packages("stringr","dplyr")
#setwd("/Users/hmorin/Documents/GitHub/SICSS/socialnetwork")

# Load the SICSS participant data
#sicss_data <- read.csv("data/sicss_all_people.csv", stringsAsFactors = FALSE)
sicss_data = select(data, c("Publication.Year",
                            "Author",
                            "Title",
                            "Abstract.Note"))
colnames(sicss_data) <- c("Year", "Author", "Title", "Abstract")

# Examine the data
head(sicss_data)

table(sicss_data$Year) # See distribution of roles

#
#-Define Key Terms to Extract---------------------------------------------------
#

# Define key terms relevant to computational social science
# These are the terms we'll look for in the Abstracts
#css_terms <- c(
#  "network", "data", "computational", "social", "research", 
#  "analysis", "machine learning", "ai", "policy", "visualization", 
#  "modeling", "simulation", "ethics", "education", "psychology", 
#  "sociology", "engineering", "design", "health", "economics", 
#  "community", "digital", "software", "technology", "science",
#  "faculty", "phd", "student", "library", "assistant", "teaching"
#)

# Define key terms relevant to computational social science
# These are the terms we'll look for in the Abstracts
css_terms <- c(
  "climate",
  "emissions",
  "energy",
  "pollution",
  "sequestration",
  "geoengineering",
  "privacy",
  "network",
  "electricty",
  "battery",
  "storage",
  "transmission",
  "wind",
  "solar",
  "gas",
  "coal",
  "nuclear",
  "co2",
  "hydrogen",
  "infrastructure",
  "transportation",
  "vehicle")
  
  
  
  #"uncertainty", "decision", "computational", "social", "research", 
  #"analysis", "electricity", "network", "energy", "pollution", 
  #"modeling", "simulation", "ethics", "education", "psychology", 
  #"quantitative", "engineering", "health", "economics", "software", 
  #"technology", "science"
#)

# Define methods (including model stems)
#methods_keywords <- c(
#  "simulat(e|ed|ion|ions|ing)",
#  "comparative stud(y|ies)",
#  "quasi[- ]?experiment(al|s)",
#  "expert elicitation(s)",
#  "interview(s|ed|ing)",
#  "MCDM",
#  "technoeconomic model(ing|s)",
#  "experiment(s|al|ing)",
#  "survey(s|ed|ing)",
#  "questionnaire(s)",
#  "statistical(ly)",
#  "regression(s)",
#  "focus group(s)",
#  "observation(s|al)",
#  "mixed method(s)"
#)
#css_terms <- methods_keywords

#
#-Extract Terms from Abstracts------------------------------------------------------
#

# Initialize a matrix to record which terms appear in which Abstracts
term_matrix <- matrix(0, nrow = nrow(sicss_data), ncol = length(css_terms))
colnames(term_matrix) <- css_terms
rownames(term_matrix) <- sicss_data$Author

# For each person and each term, check if the term appears in their Abstract
for (i in 1:nrow(sicss_data)) {
  Abstract_text <- tolower(sicss_data$Abstract[i])
  
  for (j in 1:length(css_terms)) {
    # Check if the term is present in the Abstract
    if (str_detect(Abstract_text, tolower(css_terms[j]))) {
      term_matrix[i, j] <- 1
    }
  }
}

# See which terms are most common
term_counts <- colSums(term_matrix)
term_summary <- data.frame(
  term = css_terms,
  count = term_counts
)
term_summary <- term_summary[order(term_summary$count, decreasing = TRUE),]
print("Most common terms in Abstracts:")
print(head(term_summary, 10)) # Top 10 most common terms

# Keep only terms that appear in at least 3 Abstracts
common_terms <- term_summary$term[term_summary$count >= 3]
term_matrix <- term_matrix[, common_terms]

# Examine our binary matrix
head(term_matrix)[,1:5] # Just looking at first 5 terms for first few people 

#
#-Creating the Term-Based Network-----------------------------------------------
#

# Calculate person-to-person similarity based on shared terms
# This matrix multiplication gives us counts of shared terms between individuals
similarity_matrix <- term_matrix %*% t(term_matrix)
diag(similarity_matrix) <- 0  # Remove self-connections

# Two people are connected if they share at least 2 terms
threshold <- 5
adjacency_matrix <- ifelse(similarity_matrix >= threshold, 1, 0)

# Convert to a network object
term_network <- as.network(adjacency_matrix, directed = FALSE)

# Add attributes to the network
term_network %v% "Author" <- sicss_data$Author
term_network %v% "Year" <- sicss_data$Year

# Inspect the network
summary(term_network)

#
#-Visualizing the Network-------------------------------------------------------
#

# Define colors based on role
year_colors <- c(
  "2009" = "blue",
  "2010" = "blue",
  "2011" = "blue",
  "2012" = "blue",
  "2013" = "blue",
  "2014" = "blue",
  "2015" = "red",
  "2016" = "red",
  "2017" = "red",
  "2018" = "red",
  "2019" = "red",
  "2020" = "green",
  "2021" = "green",
  "2022" = "green",
  "2023" = "green",
  "2024" = "green",
  "2025" = "orange"
)

library(viridisLite)
library(viridis)

# First, define the sorted unique years
years <- sort(unique(term_network %v% "Year"))

# Then, generate the colors and assign names
year_colors <- setNames(viridis(length(years)), years)


# Create color vector based on roles
node_colors <- unname(year_colors[as.character(term_network %v% "Year")])


# Plot the network
coords <- gplot(
  term_network,
  gmode = "graph",
  displaylabels = FALSE,
  label.cex = 0.7,
  vertex.col = node_colors,
  main = "SICSS Abstract Term Similarity Network",
)

# Add a legend
legend("bottomleft", 
       legend = names(year_colors),
       fill = year_colors, 
       cex = 0.8, 
       title = "Year")

# Try a different layout
gplot(
  term_network,
  gmode = "graph",
  displaylabels = TRUE,
  label.cex = 0.7,
  vertex.col = node_colors,
  main = "SICSS Abstract Term Similarity Network (Kamada-Kawai Layout)",
  mode = "kamadakawai"
)

#
#-Network Analysis--------------------------------------------------------------
#

# Calculate basic network metrics
density <- gden(term_network)
cat("Network Density:", density, "\n")

# Calculate degree centrality
degree_cent <- degree(term_network, gmode = "graph")
degree_df <- data.frame(
  name = term_network %v% "Author",
  role = term_network %v% "Year",
  degree = degree_cent
)

# Find people with highest degree centrality
top_central <- degree_df %>%
  arrange(desc(degree)) %>%
  head(5)

print("Top 5 central individuals (most shared terms):")
print(top_central)

# Let's also try betweenness centrality
between_cent <- betweenness(term_network)
between_df <- data.frame(
  name = term_network %v% "Author",
  role = term_network %v% "Year",
  betweenness = between_cent
)

top_between <- between_df %>%
  arrange(desc(betweenness)) %>%
  head(5)

print("Top 5 individuals by betweenness centrality:")
print(top_between)


#
#-Role Mixing Analysis----------------------------------------------------------
#

# Look at connections between roles using a mixing matrix
role_mix <- mixingmatrix(term_network, "Year")
print("Mixing matrix showing connections between roles:")
role_mix

# Create a role-level network from the mixing matrix
# Remove the sum row and column (the last row and column)
role_mix_network <- role_mix[-nrow(role_mix), -ncol(role_mix)]

# Plot the role mixing network
par(mfrow=c(1,2)) # Set up to show two plots side by side

# First, plot the original term network
gplot(
  term_network,
  gmode = "graph",
  displaylabels = FALSE,
  vertex.col = node_colors,
  main = "Individual Term Network",
  label.cex = 0.5
)

# Add a legend to the first plot
legend("bottomleft", 
       legend = names(year_colors),
       fill = year_colors, 
       cex = 0.6, 
       title = "Year")

# Second, plot the role mixing network
gplot(
  role_mix_network,
  displaylabels = TRUE,
  vertex.col = year_colors[rownames(role_mix_network)],
  label.cex = 0.9,
  main = "Role Mixing Network",
  edge.lwd = role_mix_network * .4  # Make edge thickness proportional to connection strength
)

# Reset the plot layout
par(mfrow=c(1,1))

# Save the network and coordinates
#save(term_network, coords, role_mix_network, file = "data/sicss_term_network.RData")



