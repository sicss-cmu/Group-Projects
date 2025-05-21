# Step 1: Load necessary packages
# =================================================================================
# Install packages
install.packages(c("tidyverse", "ggplot2", "igraph", "tidygraph", "ggraph"))

library(tidyverse)  # For data manipulation
library(ggplot2)    # For visualization
library(igraph)     # For network analysis
library(tidygraph)  # For tidy network operations
library(ggraph)     # For network visualization

# Step 2: Read and examine the data, remove missing values
# ====================================================================================
# Read the CSV file
trump19 <- read_csv("C:/Users/sc074/R programming/trumpworld.csv")

# Look at the first few rows
head(trump19)

## check for rows with missing values
trump19 %>% filter(if_any(everything(), is.na))

#No rows with missing values it seems. Just in case, remove rows with missing values ---------------------------------
trumpclean <- trump19 %>% drop_na()

# STEP 2.2 DESCRIPTIVE STATS?  initial visualizations
#==============================================================================================
# how many types of Entity A are there?
count(trumpclean,`Entity A Type`)

# Plot distribution of Entity Types (A)
ggplot(trumpclean, aes(x = `Entity A Type`, fill = `Entity A Type`)) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Entity A Types",
       x = "Entity Type",
       y = "Count")

ggsave("entity_a_distribution.png", width = 6, height = 9)

# how many types of Entity B are there?
count(trumpclean,`Entity B Type`)
# Plot distribution of Entity Types (B)
ggplot(trumpclean, aes(x = `Entity B Type`, fill = `Entity B Type`)) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Entity B Types",
       x = "Entity Type",
       y = "Count")
ggsave("entity_b_distribution.png", width = 6, height = 9)


#how many types of connections are there between the entities?

#table with freq count saved as data frame
Connection_Types<- as.data.frame(table(trumpclean$`Entity A Type`,trumpclean$`Entity B Type`))
print(Connection_Types)

#defining new var  connection_type to define whether the tie is org to org, person to person, or person to org
Connection_Types <- Connection_Types %>% 
  mutate ( connection_type = case_when (
    Var1 == "Person" & Var2 == "Person" ~ "Person to Person",
    Var1 == "Organization" & Var2 =="Organization" ~ "Org to Org",
    (Var1 =="Person"& Var2 == "Organization") | (Var1 =="Organization" & Var2 == "Person") ~ "Person to Org",
    Var1 == "Person" & Var2=="Federal Agency" ~ "Person to Fed",
    Var1 == "Organization" & Var2 == "Federal Agency" ~ "Org to Fed",
    TRUE ~ "Other"
  )
  )
print(Connection_Types)

# creating freq  table by  connection type

connection_summary <- Connection_Types %>%
  group_by(connection_type) %>%
  summarise(total_freq = sum(Freq))
print (connection_summary)

sorted_connection <- connection_summary %>%
  arrange(desc(total_freq))
print(sorted_connection)

# Plot distribution of Connection  Types 
#how do i sorted in decreasing order?????#####
ggplot(connection_table, aes(x = connection_type, y = total_freq, fill = connection_type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total Frequency by Connection Type",
    x = "Connection Type",
    y = "Total Frequency"
  ) +
  theme_minimal()

ggsave("frequency_by_connection_type.png", width = 6, height = 9)



# Step 3: Create nodes and edges, and convert to network data
# ======================================================================================

# Create node list by combining Entity A and Entity B, and retaining unique entities only
nodes <- trumpclean %>%
  select(`Entity A`, `Entity A Type`) %>%
  rename(name = `Entity A`, type = `Entity A Type`) %>%
  bind_rows(
    trump19 %>% select(`Entity B`, `Entity B Type`) %>%
      rename(name = `Entity B`, type = `Entity B Type`)
  ) %>%
  distinct()

# Create edge list
edges <- trumpclean %>%
  select(`Entity A`, `Entity B`, Connection) %>%
  rename(from = `Entity A`, to = `Entity B`, relation = Connection)


#  Create network object using tidygraph ---------------------
g <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
class(g)

#Network Stats
#Calculate basic network metrics
network_stats <- list(
  # Number of nodes and edges
  num_nodes = g %>% activate(nodes) %>% nrow(),
  num_edges = g %>% activate(edges) %>% nrow(),
  
  # Network density (actual connections / possible connections)
  density = edge_density(g),
  
  # Average degree (average number of connections per node)
  avg_degree = mean(degree(g))
)

# Print basic statistics
print("Basic Network Statistics:")
print(network_stats)

#-----------------------------------------------------------------------

# Step 4: DATA VIZ
# =====================================================================================
#NODES |  Visualize nodes by type (Person vs Organization vs Fed Agency) -----------------

#nodes by type and ties, little bit messy
nodes_by_type<- ggraph(g, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(color = type)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_manual(values = c("brown3", "springgreen3", "royalblue1")) +
  ggtitle("Node Type: Person vs Organization vs Federal Agency") +
  theme_void()
print(nodes_by_type)

# New graph: nodes by type, but not showing ties, makes it less messy. Will visualize ties separately. 
nodesnew_by_type<- ggraph(g, layout = "fr") +
  geom_node_point(aes(color = type)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 1) +
  scale_color_manual(values = c("brown3", "springgreen3", "royalblue1")) +
  ggtitle("Node Type: Person vs Organization vs Federal Agency") +
  theme_void()
print(nodesnew_by_type)
ggsave("nodesnew_by_type.pdf", plot = nodes_by_type, width = 10, height = 8)



# Step 5: VISUALZING DATA USING CENTRALITY MEASURES
# =======================================================================================
###########Centrality measures and graphs---------------------------------

# FIRST Compute centrality measures --------------------------------------
g <- g %>%
  mutate(
    degree = centrality_degree(),
    betweenness = centrality_betweenness(),
    closeness = centrality_closeness()
  )

# plotting all three centrality measures
# Common layout
layout_type <- "fr"

# Degree Centrality Plot
plot_degree <- ggraph(g, layout = layout_type) +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = degree, color = degree)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_viridis_c() +
  scale_size_continuous(range = c(1, 6)) +
  ggtitle("Degree Centrality") +
  theme_void()
print(plot_degree)
ggsave("Degree Centrality Plot.pdf", plot = plot_degree, width = 10, height = 8)

# Betweenness Centrality Plot
plot_betweenness <- ggraph(g, layout = layout_type) +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = betweenness, color = betweenness)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_viridis_c() +
  scale_size_continuous(range = c(1, 8 )) +
  ggtitle("Betweenness Centrality") +
  theme_void()

print(plot_betweenness)
ggsave("Betweenness Centrality Plot.pdf", plot = plot_degree, width = 10, height = 8)
#-------------------------------------------------------------------
#________________________________________________________
# View top central nodes by degree centrality
centrality_table <- as_tibble(g) %>%
  arrange(desc(degree))

print(centrality_table)

top20_nodes <- as_tibble(g) %>%
  arrange(desc(degree)) %>%
  slice(1:20) %>%
  pull(name)

# Create a subgraph of just top 20
g_top20 <- g %>%
  filter(name %in% top20_nodes)

# Plot with color, smaller size, labels, and legend
top20_plot <- ggraph(g_top20, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = degree, color = type), show.legend = TRUE) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_size_continuous(range = c(3, 8)) +  # Reduce node size range
  scale_color_manual(values = c("Person" = "tomato", "Organization" = "steelblue")) +
  ggtitle("Top 20 Nodes by Degree Centrality") +
  theme_void() +
  theme(legend.position = "right")

ggsave("top20_degree_plot_may19.pdf", plot = top20_plot, width = 10, height = 8)




#_____________________________________________________
#---------------------------------------------------------------------------

#-------------------------------------------------------------------
#________________________________________________________
# View top central nodes by betweenness centrality
betweenness_table <- as_tibble(g) %>%
  arrange(desc(betweenness))

print(betweenness_table)

top20_between_nodes <- as_tibble(g) %>%
  arrange(desc(betweenness)) %>%
  slice(1:20) %>%
  pull(name)

# Create a subgraph of just top 20
g_top20between <- g %>%
  filter(name %in% top20_between_nodes)

# Plot with color, smaller size, labels, and legend
top20_beween_plot <- ggraph(g_top20between, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = betweenness, color = type), show.legend = TRUE) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_size_continuous(range = c(3, 8)) +  # Reduce node size range
  scale_color_manual(values = c("Person" = "red", "Organization" = "blue")) +
  ggtitle("Top 20 Nodes by Betweenness Centrality") +
  theme_void() +
  theme(legend.position = "right")
print(top20_beween_plot)

ggsave("top20_betweenplot_may19.pdf", plot = top20_beween_plot, width = 10, height = 8)

#Plot nodes only (colored by type)
ggraph(graph, layout = "fr") + 
  geom_node_point(aes(color = type), size = 1) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  ggtitle("Nodes colored by Entity Type") ##nodesbyentity.png


#_____________________________________________________
#---------------------------------------------------------------------------
#-------------------------------------------------------------------
#________________________________________________________
# View top central nodes by closeness centrality
closeness_table <- as_tibble(g) %>%
  arrange(desc(closeness))

print(closeness_table)

top20_close_nodes <- as_tibble(g) %>%
  arrange(desc(closeness)) %>%
  slice(1:20) %>%
  pull(name)

# Create a subgraph of just top 20
g_top20closeness <- g %>%
  filter(name %in% top20_close_nodes)

# Plot closeness with color, smaller size, labels, and legend
top20_closeness_plot <- ggraph(g_top20closeness, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = closeness, color = type), show.legend = TRUE) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_size_continuous(range = c(3, 8)) +  # Reduce node size range
  scale_color_manual(values = c("Person" = "violetred1", "Organization" = "springgreen1")) +
  ggtitle("Top 20 Nodes by Closeness Centrality") +
  theme_void() +
  theme(legend.position = "right")
print(top20_closeness_plot)

ggsave("top20_closenessplot_may19.pdf", plot = top20_closeness_plot, width = 10, height = 8)
#_______________________________________________________________________________________________
#----------------------------------------------------------------------------------------------

# Step 6: VISUALZING EDGES
# ===============================================================
# craeting new edges df with from_type and to_type

edgesnew <- trumpclean %>%
  select(`Entity A`, `Entity B`, `Entity A Type`, `Entity B Type`,  Connection) %>%
  rename(from = `Entity A`, to = `Entity B`, 
         from_type = `Entity A Type`,  to_type = `Entity B Type` ,relation = Connection)

# Add a connection_type column based on from_type and to_type

edgesnew <- edgesnew  %>%
  mutate(connection_type = case_when(
    from_type == "Organization" & to_type == "Organization" ~ "org-org",
    from_type == "Person" & to_type == "Person" ~ "people-people",
    (from_type == "Person" & to_type == "Organization") |
      (from_type == "Organization" & to_type == "Person") ~ "people-org",
    TRUE ~ "other"  # Catch-all for unexpected combinations
  ))

# ---Create NEW network graph ---
newgraph <- tbl_graph(nodes = nodes, edges = edgesnew, directed = TRUE)


#create visualizations depicting connection type as a bar chart and a network graph
#bar chart
edgesnew %>%
  count(connection_type) %>%
  ggplot(aes(x = reorder(connection_type, -n), y = n, fill = connection_type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Number of Edges by Connection Type",
       x = "Connection Type", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")



# Create the network visualization
newplot5_20<- ggraph(newgraph, layout = "fr") +  # Fruchterman-Reingold layout for better spacing
  # Add edges colored by connection_type
  geom_edge_link(aes(color = connection_type), 
                 alpha = 0.6,  # Slightly transparent edges
                 show.legend = TRUE) +
  # Add nodes colored by type, no labels
  geom_node_point(aes(color = type), 
                  size = 1) +  # Adjust size as needed
  # Customize the theme
  theme_void() +
  # Add title and legend
  labs(title = "Network Visualization",
       color = "Node Type",
       edge_color = "Connection Type") +
  # Position legend at bottom
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
print(newplot5_20)
ggsave("edgesbytypemay20.pdf", plot = newplot5_20, width = 10, height = 8)


