
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

#Cheatsheet: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#Gallery: https://shiny.rstudio.com/gallery/

# #Load in Data
df <- read.csv("G:\\My Drive\\MSA\\TextAnalytics\\tag_shiny.csv")

# Define UI - what do you want your app to look like / contain?
ui <- fluidPage(
  
  titlePanel("Sentiment Correlation Analysis"),

  #Note, the first argument in an Input is the inputId. This is a unique variable name that will be used to pass the
  # selected value to the R server script.
  sidebarLayout(
    sidebarPanel(
      "Select transcript ID",
      selectInput("IDInput", "ID", unique(df$ID), selected=1)
    )
    ,
    mainPanel(
      
      h4("tags and number of view"),
      tableOutput("tableOutput"),
      br(),
      h4("Valence vs Arousal"),
      plotOutput("scatterOutput"),
      br(),
      h4("Arousal across the transcript"),
      plotOutput("arousalOutput"),
      br(),
      h4("Valence across the transcript"),
      plotOutput("valenceOutput"),
      br()
      

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
    df %>% filter(ID == input$IDInput)
  })
  
  # create arousal output
  output$arousalOutput <-  renderPlot({
    filtered_arousal <- filtered_data() %>% filter(valence !=0)
    filtered_laughter <- filtered_data() %>% filter(valence !=0 & laughter.frequency !=0)
    filtered_applause <- filtered_data() %>%  filter(valence !=0 & applause.frequency !=0)
    
    ggplot(filtered_arousal, aes(x=sentence_id, y=arousal)) +
       geom_line() +
       geom_smooth(method = lm) + theme_classic() +
       ylim(0,9) +
       geom_vline(xintercept=filtered_laughter$sentence_id, color='red',alpha=.3) + 
       geom_vline(xintercept=filtered_applause$sentence_id, color='blue',alpha=.3)
  })
  
  # create valence plot
  output$valenceOutput <-  renderPlot({
    filtered_valence <- filtered_data() %>% filter(valence !=0)
    filtered_laughter <- filtered_data() %>% filter(valence !=0 & laughter.frequency !=0)
    filtered_applause <- filtered_data() %>%  filter(valence !=0 & applause.frequency !=0)
    # ggplot(filtered_valence, aes(x=sentence_id, y=valence)) +
    #   geom_line() +
    ggplot() +
      geom_line(data = filtered_valence, aes(x = sentence_id, y = valence)) +
      geom_line(data = filtered_valence, aes(x = sentence_id, y = arousal), linetype = "longdash") +
      theme_classic() + theme(legend.position="top") +
      ylim(0,9) +
      geom_vline(xintercept=filtered_laughter$sentence_id, color = 'red', alpha = .3) +
      geom_vline(xintercept=filtered_applause$sentence_id, color = 'blue', alpha = .3)
      
      # geom_point(data = filtered_laughter,
      #            aes(x = sentence_id, y = laughter.frequency), colour = "red") +
      # geom_point(data = filtered_applause,
      #            aes(x = sentence_id, y = applause.frequency), colour = "blue")
  })
  
  # create table with tags and views
  output$tableOutput <- renderTable({
    filtered_data() %>% dplyr::select('tags', 'views') %>% slice(1)
  })
  
  # create scatter plot
  output$scatterOutput <-  renderPlot({
    filtered_scatter <- filtered_data() %>% filter(valence !=0 | arousal !=0)
    ggplot(filtered_scatter, aes(x=valence, y=arousal)) +
      geom_point() + theme_classic() +
      xlim(0,9) +
      ylim(0,9)
    
  })
 
  
}

# Run the app 
shinyApp(ui = ui, server = server)
