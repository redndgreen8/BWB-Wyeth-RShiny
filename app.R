#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(xml2)
library(parallel)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Complex Shiny Dashboard for Plotting"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType", 
                  "Select Plot Type:", 
                  choices = list("Eligibility Pie Chart" = "eligibility", 
                                 "Comb Pie Chart" = "comb")),
      actionButton("plotButton", "Generate Plot")  # Add this line for the plot button
    ),
    mainPanel(
      plotOutput("plotOutput")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  source("theme_DB.R")
 # source("clinical_data_plots.R")
  source("eligible.R")
  
  # Define the directory path

  # Reactive expression for processing eligibility data
  processedData <- reactive({
    # Assuming getEligiblity function processes and returns data
    ef <- getEligiblity(paste0(.dir, "demographics/Website Eligibility Survey 1.18.24.csv"))
    return(ef)
  })
  
  # Define race levels for the plots
  rLevels <- c("ASIAN", "BLACK", "HISPANIC", "WHITE", "MIXED", "NA.AME/P.ISLA", "UNKNOWN")
  
  # Observe Event for plotButton
  observeEvent(input$plotButton, {
    # Reactive expression for processing eligibility data
    processedData <- getEligiblity(paste0( "demographics/Website Eligibility Survey 1.18.24.csv"))
    
    # Render plot based on user selection
    output$plotOutput <- renderPlot({
      # Get the processed data
      ss.bcsb.ef <- processedData
      
      # Check the plot type selected by the user
      if (input$plotType == "eligibility") {
        # Call the function to generate the Eligibility Pie Chart
        res.bcsb.ef <- getPie(ss.bcsb.ef, rl = rLevels)
        return(res.bcsb.ef$gp)
      } else if (input$plotType == "comb") {
        # Call the function to generate the Comb Pie Chart
        res.bcsb.comb <- getPieComb(ss.bcsb.ef, rl = rLevels)
        return(res.bcsb.comb$gp)
      }
    })
  }, ignoreNULL = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
