library(shiny)
library(shinythemes)
library(wordcloud)
library(dplyr)
library(tidytext)
library(tm)
library(readr)
library(stringr)

EPP_data <- read_csv("EPP_data.csv") %>%
  rename(year = `Publication Year`, abstract = `Abstract Note`) %>%
  filter(!is.na(year), !is.na(abstract))

# defining ui 
ui <- fluidPage(
  titlePanel("EPP Thesis Abstract Word Cloud Dashboard (2009–2025)"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput",
                  "Select Year:",
                  min = 2009,
                  max = 2025,
                  value = c(2009, 2025))
    ),
    # MAIN PANEL: Output visualizations
    mainPanel(
      # Loading message while data is being prepared
      conditionalPanel(
        condition = "!output.dataReady",
        h4("Loading data, please wait...")
      ),
      # Plot output (only shown when data is ready)
      conditionalPanel(
        condition = "output.dataReady",
        plotOutput("wordcloudPlot", height = "600px")
      )
    )
  )
)


# server logic
server <- function(input, output, session) {
  # Variable to track if data is ready to display
  dataReady <- reactiveVal(FALSE)
  # Create reactive output for UI conditional panel
  output$dataReady <- reactive({ dataReady() })
  outputOptions(output, "dataReady", suspendWhenHidden = FALSE)
  
  output$wordcloudPlot <- renderPlot({
      filtered <- EPP_data %>%
        filter(year >= input$yearInput[1], year <= input$yearInput[2])
      
      # cleaning data, tokenization, stopwords
      tidy_words <- filtered %>%
        unnest_tokens(word, abstract) %>%
        filter(!word %in% stop_words$word) %>%
        filter(str_detect(word, "^[a-z]+$")) %>%
        count(word, sort = TRUE) %>%
        head(100)
      
      # Signal that data is ready to display
      dataReady(TRUE)
      
      # plotting word cloud
      if (nrow(tidy_words) == 0) {
        plot.new()
        text(0.5, 0.5, "No data available for selected years", cex = 1.5)
      } else {
        wordcloud(words = tidy_words$word,
                  freq = tidy_words$n,
                  min.freq = 1,
                  max.words = 100,
                  random.order = FALSE,
                  colors = brewer.pal(8, "Dark2"))
      }
  })
}

# run the application
shinyApp(ui = ui, server = server)