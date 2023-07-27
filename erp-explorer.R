# ---- ERP Explorer ----
#
# Author: Francesco Grassi
# GitHub: Fra-Gra
#
# A shiny app to explore ERP data

library(shiny)
library(tidyverse)
library(scico)

options(shiny.maxRequestSize=30*1024^2)

# Define UI
ui <- fluidPage(
  
  titlePanel("ERP Explorer"),
  
  sidebarLayout(
    
    # Input column
    sidebarPanel(
      # Button to input .csv file with raw data:
      fileInput("infile", "Select a file", accept = c(".csv")),  
      # Buttons to select which average to display:
      radioButtons(
        inputId = "avg_choice",
        label = "Average data across:",
        choiceNames = list("Subjects", "Channels", "Both"),
        choiceValues = list("subjects", "channels", "both"),
        selected = "both"
      )
    ),
    
    # Main plot panel:
    mainPanel(
      plotOutput("wave_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Import input file ----
  raw_data <- reactive({
    req(input$infile) # Since "input$infile" is initially set as NULL, wait till a file is actually loaded)
    read_csv(file = input$infile$datapath)  # read file
  })
 
  # Prepare and plot data ----
  output$wave_plot <- renderPlot({
    
    df <- raw_data()  # get raw data
    
    ## Handle input ----
    
    ### Average data ----
    
    # If user select one variable to average across, use the other as grouping variable:
    if (input$avg_choice == "subjects") {
      grouping_vars <- c("Channel")
    } else if (input$avg_choice == "channels") {
      grouping_vars <- c("ID")
    } else if (input$avg_choice == "both") {
      grouping_vars <- c()
    } 
    
    # Average data according to user input:
    df <- df %>% 
      group_by(across(all_of(c("Condition", grouping_vars, "Time")))) %>%  # add grouping variables that are always going to be used
      summarize(Amplitude = mean(Amplitude)) %>% 
      ungroup()
    
    ## Plot data ----
    wave_plot <- df %>% 
      ggplot(aes(x = Time, y = Amplitude, color = Condition)) +
      geom_line(linewidth = 0.5)
    
    ### Facet data ----
    # If data was not averaged for both channels and subjects, facet data by the remaining variable:
    if (!is.null(grouping_vars)){
      wave_plot <- wave_plot +
        facet_wrap(vars(!!sym(grouping_vars)))  # NOTE: not very sure why have to use "sym" here
    }
    
    wave_plot
  })
  

}

# Run app
shinyApp(ui, server)
