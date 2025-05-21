# installing packages
install.packages(c("igraph", "tidyverse", "ggraph", "tidygraph", "gender", "remotes"))

# loading packages
library(igraph)
library(tidyverse)
library(ggraph)
library(tidygraph)
library(gender)
library(dplyr)
library(stringr)
library(ggrepel)
library(scales)
library(visNetwork)

# reading in datasets
nodes <- read_csv("trump-nodes.csv")
edges <- read_csv("trump-edges.csv")

# if you need it!
# par(mfrow = c(1,1))

# getting a first name variable 
nodes <- nodes %>%
  mutate(FirstName = word(Label, 1))

# install the genderdata package from GitHub
Sys.setenv(GITHUB_PAT = "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")
# at one point, these were all that worked. Leaving them in in case that ever happens again
#remotes::install_github("lmullen/gender-data-pkg")
#remotes::install_github("lmullen/gender-data-pkg", subdir = "data-raw/genderdata")

# predict gender for each unique first name, using the year 2012 <-- most recent year
predicted_genders <- gender(unique(nodes$FirstName), method = "ssa", years = 2012)

# joining to the trump-nodes dataset
nodes <- nodes %>%
  left_join(predicted_genders, by = c("FirstName" = "name")) %>%
  rename(Gender = gender)

# adding gender for remaining names manually
nodes %>%
  count(Gender, sort = TRUE)

no_gender <- nodes %>%
  filter(is.na(Gender))

# writing the no_gender data into a CSV for manual entry
#write.csv(no_gender, "no_gender.csv", row.names = FALSE)

# loading the manually entered gender sheet
no_gender_clean <- read_csv("no_gender_clean.csv")

# merging manually assigned genders back into nodes
nodes <- nodes %>%
  left_join(no_gender_clean %>% select(FirstName, ManualGender = Gender), by = "FirstName") %>%
  mutate(Gender = coalesce(Gender, ManualGender)) %>%
  select(-ManualGender)

# new gender count, with manually added gender
nodes %>%
  count(Gender, sort = TRUE)

# keeping only nodes that have been assigned a gender - redundant now that gender
# has been manually entered, but I'd rather not rename everything...
nodes_clean <- nodes %>%
  filter(Gender %in% c("male", "female"))

# adding first names variable to edges dataset
edges <- edges %>%
  mutate(
    SourceFirstName = word(Source, 1),
    TargetFirstName = word(Target, 1)
  )

# predicting source and target genders
source_predicted_genders <- gender(unique(edges$SourceFirstName), method = "ssa", years = 2012)
target_predicted_genders <- gender(unique(edges$TargetFirstName), method = "ssa", years = 2012)

# adding the source gender variable to the edges dataset
edges <- edges %>%
  left_join(source_predicted_genders, by = c("SourceFirstName" = "name")) %>%
  rename(SourceGender = gender)

# adding the target gender variable to the edges dataset
edges <- edges %>%
  left_join(source_predicted_genders, by = c("TargetFirstName" = "name")) %>%
  rename(TargetGender = gender)

# filtering out any rows where either source of target gender aren't assigned
edges_mf <- edges %>%
  filter(SourceGender %in% c("male", "female") & 
           TargetGender %in% c("male", "female"))

edges_clean <- edges %>%
  filter(Source %in% nodes_clean$Label & Target %in% nodes_clean$Label)

# assigning edge types based on source and target gender
edges_clean <- edges_clean %>%
  mutate(
    edge_gender_type = case_when(
      SourceGender == "male" & TargetGender == "male" ~ "Man-Man",
      SourceGender == "female" & TargetGender == "female" ~ "Woman-Woman",
      SourceGender %in% c("male", "female") &
        TargetGender %in% c("male", "female") &
        SourceGender != TargetGender ~ "Man-Woman",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(edge_gender_type))

# creating a graph
g <- graph_from_data_frame(edges_clean, vertices = nodes_clean, directed = FALSE)

# converting to tidygraph
# filtering out unconnected nodes (needs degree greater than zero)
# assigning node color based on either gender or whether the node is Trump
# assigning edge type based on gendered connections above
tg <- as_tbl_graph(g) %>%
  mutate(
    degree = centrality_degree()
  ) %>%
  filter(degree > 0) %>%
  mutate(
    node_color = case_when(
      Label == "Donald J. Trump" ~ "Trump",
      Gender == "male" ~ "Men",
      Gender == "female" ~ "Women"
    )
  ) %>%
  activate(edges) %>%
  mutate(
    edge_type = edges_clean$edge_gender_type
  ) %>%
  activate(nodes)

# identifying the 150 most central nodes (excluding Trump) so we can label them
top <- as_tibble(tg) %>%
  filter(Label != "Donald J. Trump") %>%
  arrange(desc(degree)) %>%
  slice_head(n = 150) %>%
  mutate(label_size = rescale(degree, to = c(15, 150)))

# creating a layout dataframe with coordinates
layout_df <- create_layout(tg, layout = "fr")

# merging 150 label font sizes into layout data
layout_df <- layout_df %>%
  left_join(top %>% select(name = Label, label_size), by = "name")

# plotting a static graph
ggraph(layout_df) +
  geom_edge_link(aes(color = edge_type), alpha = 0.7) + # edges assigned color by edge type
  geom_node_point(aes(size = degree, color = node_color)) + # nodes assigned color as above
  geom_text_repel(
    data = layout_df %>% filter(!is.na(label_size)),
    aes(x = x, y = y, label = name, size = label_size), # adding differently sized labels
    box.padding = 0,
    max.overlaps = Inf,
    seed = 42,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Trump" = "darkred",
      "Men" = "darkblue",
      "Women" = "hotpink"
    )
  ) +
  scale_edge_color_manual(
    values = c(
      "Man-Man" = "cornflowerblue",
      "Woman-Woman" = "hotpink2",
      "Man-Woman" = "mediumorchid3"
    )
  ) +
  guides(size = "none") +
  theme_graph()

# That was the static visual, onto the interactive visual!

# here, we're creating a dataframe with calculated degree centrality
degree_df <- data.frame(
  Label = V(g)$Label,
  degree = degree(g)
) %>%
  filter(degree > 0)  # only keeping connected nodes in this dataframe

# preparing node data with proper size scaling
nodes_filtered <- nodes_clean %>%
  filter(Label %in% degree_df$Label) %>%
  left_join(degree_df, by = "Label") %>%
  mutate(
    id = Label,  # ID must match exactly what's in the edges
    title = paste0("<p><b>", Label, "</b><br>Connections: ", degree, "</p>"),  # Tooltip
    label = Label,  # Keep all labels
    group = case_when(
      Gender == "female" ~ "Women",
      TRUE ~ "Men"  
    ),
    # Special color for Trump
    color = case_when(
      Label == "Donald J. Trump" ~ "darkred",
      Gender == "female" ~ "hotpink",
      TRUE ~ "darkblue"
    ),
    # save the raw degree for later
    value = degree  # visNetwork will handle scaling
  )

# also adding scaling for label fonts
nodes_filtered <- nodes_filtered %>%
  mutate(
    font_size = scales::rescale(degree, to = c(50, 250)),
    `font.size` = font_size,
    label = ifelse(Label == "Donald J. Trump", "", Label)  # Remove label for Trump,
    # who's so big he throws everything off
  )

# preparing edge data
edges_filtered <- edges_clean %>%
  filter(Source %in% nodes_filtered$id & Target %in% nodes_filtered$id) %>%
  mutate(
    from = Source,
    to = Target,
    title = paste0("<p>", Source, " — ", Target, "<br>", 
                   "Relationship: ", Relationship, "<br>",
                   "Type: ", edge_gender_type, "</p>")

# Couldn't get the following to work
    #color = case_when(
    #  edge_gender_type == "Man-Man" ~ "cornflowerblue",
    #  edge_gender_type == "Woman-Woman" ~ "hotpink2",
    #  edge_gender_type == "Man-Woman" ~ "mediumorchid3"
    )
  #) %>%
  #mutate(color = case_when(
  #  edge_gender_type == "Man-Man" ~ list(list(color = "cornflowerblue")),
  #  edge_gender_type == "Woman-Woman" ~ list(list(color = "hotpink2")),
  #  edge_gender_type == "Man-Woman" ~ list(list(color = "mediumorchid3"))
  #)) %>%  # wrap color for visNetwork
  #select(from, to, title, edge_gender_type )#, color)

# create the interactive network 
visNetwork(
  nodes = nodes_filtered, 
  edges = edges_filtered,
  width = "100%",
  height = "700px"
) %>%
  # configuring the nodes
  visNodes(
    scaling = list(
      min = 7.5,
      max = 75
    ),
    shape = "dot",
    shadow = TRUE,
    font = list(size = 'font.size')
  ) %>%
  # configuring the edges
  visEdges(
    smooth = TRUE,
    width = 1.2,
    shadow = FALSE,
    selectionWidth = 2,
    #color = list()
  ) %>%
  # node colors by gender
  visGroups(groupname = "Men", color = "darkblue") %>%
  visGroups(groupname = "Women", color = "hotpink") %>%
  # options for interactivity
  visOptions(
    highlightNearest = list(
      enabled = TRUE,
      degree = 1,
      hover = TRUE
    ),
    selectedBy = list(
      variable = "group",
      multiple = TRUE,
      main = "Gender"
    ),
    collapse = FALSE
  ) %>%
  visLayout(
    randomSeed = 42,
    improvedLayout = TRUE
  ) %>%
  visPhysics(
    stabilization = TRUE, # set to FALSE if you want it to move around
    solver = "forceAtlas2Based"
  ) %>%
  visInteraction(
    navigationButtons = TRUE, # lets us navigate the visual

  )


