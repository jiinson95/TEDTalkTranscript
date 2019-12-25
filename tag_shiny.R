library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

#Cheatsheet: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#Gallery: https://shiny.rstudio.com/gallery/

# #Load in Data
# df <- read.csv("G:\\My Drive\\MSA\\TextAnalytics\\tag_shiny.csv")
# 
binary <- read.csv("G:\\My Drive\\MSA\\TextAnalytics\\tag_freq.csv")
binary <- binary %>% filter(freq > 100)
binary$tags <- as.character(binary$tags)
colname <- c()
for (i in 1:nrow(binary)){
  colname[i] <- binary$tags[i]
}

# Define UI - what do you want your app to look like / contain?
ui <- fluidPage(
  
  titlePanel("Sentiment Correlation Analysis"),
  
  #Note, the first argument in an Input is the inputId. This is a unique variable name that will be used to pass the
  # selected value to the R server script.
  sidebarLayout(
    sidebarPanel(
      "Select tag",
      selectInput("tagInput", "Tag", colname, selected="X.technology.")
    )
    ,
    mainPanel(
      
      # h4("tags and number of view"),
      # tableOutput("tableOutput"),
      # br()
      h4("Valence vs Arousal"),
      plotOutput("scatterOutput"),
      br()
      # 
      
    )
  )
  
)

# Define server logic - what do you want the R code to do?
server <- function(input, output) {
  
  #Once we have an idea of our inputs and outputs, we need to actually create the outputs
  #Note the function above has two objects, input and output. 
  #Shiny will pass inputs to the input object, and we need to pass outputs to the output object
  
  # #Filter the data using the hwy inputs
  filtered_data <- reactive({
    tagname <- input$tagInput
    df %>% dplyr::select(contains(tagname), ID, valence, arousal) %>%
      filter(.[[1]]==1 & valence !=0) 
  })
  

  
  # create table with tags and views
  # output$tableOutput <- renderTable({
  #   filtered_data() %>% select('tags', 'views') %>% slice(1)
  # })
  
  # create scatter plot
  output$scatterOutput <-  renderPlot({

    ggplot(filtered_data(), aes(x=valence, y=arousal)) +
      geom_point() +
      xlim(0,9) +
      ylim(0,9)

  })
  
  
}

# Run the app 
shinyApp(ui = ui, server = server)
