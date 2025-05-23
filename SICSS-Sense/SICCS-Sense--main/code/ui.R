# User interface
# Use a fluid Bootstrap layout
fluidPage(    
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Press+Start+2P&family=Gloria+Hallelujah&display=swap", rel = "stylesheet"),
    
    tags$style(HTML("
      body {
        background-color: #000000;
        color: #00FFFF;
        font-family: 'Gloria Hallelujah', cursive;
      }

      h2 {
        font-family: 'Press Start 2P', cursive;
        color: #FF0000;
        text-align: center;
        margin-top: 20px;
      }

      .sidebarPanel {
        background-color: #111111;
        border: 2px solid #FF6347;
        padding: 20px;
        color: #FFA500;
      }

      .mainPanel {
        background-color: #111111;
        border: 2px solid #00FFFF;
        padding: 20px;
      }

      input, .form-control {
        background-color: #222222;
        color: #00FFFF;
        border-color: #00FFFF;
      }

      hr {
        border-color: #FF00FF;
      }

      .help-block {
        color: #AAAAAA;
        font-size: 0.9em;
      }

      .shiny-text-output {
        color: #FF69B4;
      }

      .well {
        background-color: #222222;
      }
    "))
  ),
  
  # Give the page a title
  titlePanel("SICSS Sense"),
  
  p("Enter keywords to see how their share of EPP dissertation abstracts varies over time."),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      # selectInput("region", "Region:", 
      #             choices=colnames(WorldPhones)),
      textInput(label = "keyword", inputId = "word", value = "policy"),
      hr(),
      helpText("Word data collected from EPP dissertations on KiltHub between 2009-2025. Density computed as proportion of abstract words per disseration.")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("wordPlot")  
    )
    
  )
)
