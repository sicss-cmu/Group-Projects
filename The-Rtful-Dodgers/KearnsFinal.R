library(shiny)
library(visNetwork)
library(tidyverse)

# ---- LOAD & PREP DATA ----
nodes <- read_csv("trump-nodes.csv", show_col_types = FALSE) %>%
  rename(id = Id, label = Label) %>%
  mutate(id = as.character(id))

edges <- read_csv("trump-edges.csv", show_col_types = FALSE) %>%
  rename(from = Source, to = Target) %>%
  mutate(from = as.character(from), to = as.character(to))

# Filter: only include connected nodes
connected_ids <- unique(c(edges$from, edges$to))
nodes <- nodes %>% filter(id %in% connected_ids)
edges <- edges %>% filter(from %in% nodes$id & to %in% nodes$id)

# Identify relationship categories
family_ids <- edges %>%
  filter(str_detect(Relationship, regex("family", ignore_case = TRUE))) %>%
  select(from, to) %>%
  unlist() %>% unique()

political_ids <- edges %>%
  filter(str_detect(Relationship, regex("political", ignore_case = TRUE))) %>%
  select(from, to) %>%
  unlist() %>% unique()

# Compute node degree
degree_df <- edges %>%
  pivot_longer(cols = c(from, to), values_to = "id") %>%
  count(id, name = "degree")

nodes <- nodes %>%
  left_join(degree_df, by = "id") %>%
  mutate(
    degree = replace_na(degree, 0),
    size = degree + 5,
    color = case_when(
      id %in% family_ids & id %in% political_ids ~ "darkorchid",  # both
      id %in% family_ids ~ "tomato",
      id %in% political_ids ~ "purple",
      TRUE ~ "lightblue"
    )
  )

# ---- CREATE NETWORK OBJECT FOR EXPORT ----
network_graph <- visNetwork(nodes, edges) %>%
  visEdges(arrows = "to", label = edges$Relationship) %>%
  visNodes(color = list(background = nodes$color), size = nodes$size) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLegend(addNodes = list(
    list(label = "Family", shape = "dot", color = "tomato"),
    list(label = "Political", shape = "dot", color = "purple"),
    list(label = "Family + Political", shape = "dot", color = "darkorchid"),
    list(label = "Other", shape = "dot", color = "lightblue")
  )) %>%
  visLayout(randomSeed = 123)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Trump Network Viewer"),
  visNetworkOutput("network_plot", height = "700px"),
  downloadButton("download_html", "Download Network as HTML")
)

# ---- SERVER ----
server <- function(input, output) {
  output$network_plot <- renderVisNetwork({
    network_graph
  })
  
  output$download_html <- downloadHandler(
    filename = function() {
      "trump_network.html"
    },
    content = function(file) {
      visSave(network_graph, file = file)
    }
  )
}

# ---- RUN APP ----
shinyApp(ui = ui, server = server)
