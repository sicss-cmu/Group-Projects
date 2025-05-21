library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(scales)
library(grid)

# ---- Load Data ----
nodes <- read_csv("trump-nodes.csv", show_col_types = FALSE) %>%
  rename(id = Id, label = Label)

edges <- read_csv("trump-edges.csv", show_col_types = FALSE) %>%
  rename(from = Source, to = Target)

# ---- Filter Edges: Only Trump-Centered ----
trump_edges <- edges %>%
  filter(from == "Donald J Trump" | to == "Donald J Trump")

# ---- Get Connected Nodes ----
connected_ids <- unique(c(trump_edges$from, trump_edges$to))

ego_nodes <- nodes %>%
  filter(id %in% connected_ids)

# ---- Categorize Relationship Type ----
trump_edges <- trump_edges %>%
  mutate(type = case_when(
    str_detect(Relationship, regex("\\b(daughter|son|wife|spouse|family)\\b", ignore_case = TRUE)) ~ "kinship",
    str_detect(Relationship, regex("advisor|campaign|cabinet|strategist|political", ignore_case = TRUE)) ~ "political",
    str_detect(Relationship, regex("attorney|counsel|legal", ignore_case = TRUE)) ~ "legal",
    str_detect(Relationship, regex("business|executive|organization|ceo|cfo", ignore_case = TRUE)) ~ "business",
    TRUE ~ "other"
  ))

# ---- Assign Node Roles and Colors ----
# Identify Trump's counterpart in each edge
edge_roles <- trump_edges %>%
  mutate(other_party = ifelse(from == "Donald J Trump", to, from)) %>%
  group_by(other_party) %>%
  summarize(role_type = first(type))  # optional: prioritize if multiple

ego_nodes <- ego_nodes %>%
  left_join(edge_roles, by = c("id" = "other_party")) %>%
  mutate(role_type = replace_na(role_type, "other")) %>%
  mutate(color = case_when(
    role_type == "kinship" ~ "tomato",
    role_type == "political" ~ "purple",
    role_type == "legal" ~ "gray60",
    role_type == "business" ~ "gold",
    TRUE ~ "lightblue"
  ))

# ---- Compute Node Degree ----
degree_df <- trump_edges %>%
  pivot_longer(cols = c(from, to), values_to = "id") %>%
  count(id, name = "degree")

ego_nodes <- ego_nodes %>%
  left_join(degree_df, by = "id") %>%
  mutate(degree = replace_na(degree, 0),
         size = rescale(degree, to = c(2, 10)))

# ---- Create Graph ----
graph_tbl <- tbl_graph(nodes = ego_nodes, edges = trump_edges, directed = TRUE)

# ---- Export to PDF ----
pdf("trump_ego_network.pdf", width = 12, height = 10)

ggraph(graph_tbl, layout = "kk") +
  geom_edge_link(aes(label = type), alpha = 0.3,
                 arrow = arrow(length = unit(3, "mm")), end_cap = circle(3, 'mm')) +
  geom_node_point(aes(size = size, color = color)) +
  geom_node_text(aes(label = label), repel = TRUE, size = 3, max.overlaps = 50) +
  scale_color_identity(
    name = "Role",
    guide = "legend",
    labels = c(
      "tomato" = "Family",
      "purple" = "Political",
      "gray60" = "Legal",
      "gold" = "Business",
      "lightblue" = "Other"
    ),
    breaks = c("tomato", "purple", "gray60", "gold", "lightblue")
  ) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme_graph(base_family = "sans") +
  theme(legend.position = "bottom") +
  ggtitle("Donald J. Trump — Ego Network")

dev.off()
