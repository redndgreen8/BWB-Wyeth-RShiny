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
library(shiny)

ui <- fluidPage(
  titlePanel("Pie Chart Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fileInput", "Choose Web Eligibility CSV File"),
      actionButton("updateButton", "Update Charts")
    ),
    mainPanel(
      plotOutput("pieChart1"),
      plotOutput("pieChart2")
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  source("theme_DB.R")
 # source("clinical_data_plots.R")
  source("eligible.R")
  
  # Define the directory path
  .dir <- "~/Documents/" 
  
  # Define race levels for the plots
  rLevels <- c("ASIAN", "BLACK", "HISPANIC", "WHITE", "MIXED", "NA.AME/P.ISLA", "UNKNOWN")
  
  # Reactive expression for processing eligibility data
  processedData <- reactive({
    req(input$fileInput)
    inFile <- input$fileInput
    
    tryCatch({
      # Apply getEligiblity and additional data transformations
      ss.bcsb.ef <- getEligiblity(inFile$datapath) |> 
        dplyr::rename(Race = What.is.your.race.ethnicity.) |> 
        dplyr::mutate(Race = ifelse(is.na(Race) | Race %in% "Prefer not to answer", "UNKNOWN", Race),
                      Race = toupper(Race),
                      Race = trimws(Race),
                      Race = ifelse(grepl("BLACK|AFRICAN", Race), "BLACK", Race),
                      Race = ifelse(grepl("KOREAN|CHINESE|ASIAN|ARAB", Race), "ASIAN", Race),
                      Race = ifelse(grepl("PACIFIC|ISLANDER|NATIVE|INDIAN|ALASKAN", Race), "NA.AMERI/P.ISLA", Race),
                      Race = ifelse(grepl("MEXICAN|CENTRAL|HISPANIC", Race), "HISPANIC", Race),
                      diagnosis = ifelse(is.eligible, "BCSB ELIGIBLE", "BCSB INELIGIBLE")) |> 
        dplyr::select(Race, diagnosis)
      
      return(ss.bcsb.ef)
    }, error = function(e) {
      # Return NULL or a default value if there's an error
      shiny::showNotification(paste("Error processing data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Reactive expression for the first pie chart
  pieChart1Data <- reactive({
    ef_data <- req(processedData())
    tryCatch({
      # Assuming getPie function generates the first pie chart
      res <- getPie(ef_data, rl = rLevels)  # Update with appropriate parameters
      return(res$gp)
    }, error = function(e) {
      # Handle the error gracefully
      shiny::showNotification(paste("Error plotting 1 data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Reactive expression for the second pie chart
  pieChart2Data <- reactive({
    ef_data <- req(processedData())
    tryCatch({
      # Assuming getPieComb function generates the second pie chart
      res <- getPieComb(ef_data, rl = rLevels)  # Update with appropriate parameters
      return(res$gp)
    }, error = function(e) {
      # Handle the error gracefully
      shiny::showNotification(paste("Error plotting 2 data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  
  # Within server function
  
  output$pieChart1 <- renderPlot({
    pieChart1Data()
  })
  
  output$pieChart2 <- renderPlot({
    pieChart2Data()
  })
  
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
