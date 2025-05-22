# functions:
# (1) word cloud for one or multiple years
# (2) search a keyword and show the trend of its number of appearance

# import packages
library(shiny)
library(shinythemes)
library(wordcloud)
library(dplyr)
library(tidytext)
library(tm)
library(readr)
library(stringr)
library(ggplot2)

# import datase
EPP_data <- read_csv("EPP_data.csv") %>%
  rename(year = `Publication Year`, abstract = `Abstract Note`) %>%
  filter(!is.na(year), !is.na(abstract))

# defining ui 
ui <- fluidPage(
  # titlePanel("2009~2025 CMU Engineering & Public Policy Thesis Abstract Keywords Dashboard"),
  # tags$h4("Data Source: Kilthub @ CMU Library"),
  div(
    h3("Dashboard: CMU Engineering & Public Policy Thesis Abstract Keywords, 2009~2025"),
    h4("Data Source: Kilthub @ CMU Library"),
    style = "margin-bottom: 20px;"
  ),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput",
                  "Year:",
                  min = 2009,
                  max = 2025,
                  value = c(2009, 2025)),
      textInput("trendKeyword", "Keyword:", 
                placeholder = "Enter the keyword to see yearly trend, e.g. cost")
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
        h4("Word Cloud"),
        plotOutput("wordcloudPlot", height = "600px"),
        
        h4("Keyword Trend Over Time"),
        plotOutput("keywordTrendPlot", height = "400px")
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
  
  # word cloud plot
  output$wordcloudPlot <- renderPlot({
    filtered <- EPP_data %>%
      filter(year >= input$yearInput[1], year <= input$yearInput[2])
    
    # cleaning data, tokenization, stopwords
    tidy_words <- filtered %>%
      unnest_tokens(word, abstract) %>%
      filter(!word %in% stop_words$word) %>%
      filter(str_detect(word, "^[a-z]+$")) %>%
      filter(!word %in% c("model", "polici", "effect", "data", "result", "increas",
                          "impact", "thesi", "provid", "inform", "assess", "potenti",
                          "chang", "chapter", "studi", "base", "level")) %>%
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
  
  # keyword trend plot
  output$keywordTrendPlot <- renderPlot({
    req(input$trendKeyword)
    keyword <- tolower(input$trendKeyword)
    
    trend_data <- EPP_data %>%
      mutate(
        abstract_lower = tolower(abstract),
        keyword_count = str_count(abstract_lower, paste0("\\b", keyword, "\\b"))
      ) %>%
      group_by(year) %>%
      summarise(total_mentions = sum(keyword_count, na.rm = TRUE)) %>%
      arrange(year)
    
    if (nrow(trend_data) == 0 || all(trend_data$total_mentions == 0)) {
      plot.new()
      text(0.5, 0.5, "No matching keyword found in any year!", cex = 1.2)
    } else {
      ggplot(trend_data, aes(x = year, y = total_mentions)) +
        geom_line(color = "blue", size = 1.2) +
        geom_point(color = "darkred", size = 2) +
        labs(
          title = paste("Yearly Mentions of Keyword:", input$trendKeyword),
          x = "Year",
          y = "Number of Mentions"
        ) +
        theme_minimal()
    }
  })
  
}

# run the application
shinyApp(ui = ui, server = server)