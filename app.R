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
library(tidyverse)
library(ggmap)
library(DT)
library(knitr)
#library(rgdal)
library(tidygeocoder)


library(shiny)

ui <- fluidPage(
  titlePanel("BCSB Dashboard"),
  
#  sidebarLayout(
#    sidebarPanel(
      #fileInput("fileInput", "Choose Web Eligibility CSV File"),
     # fileInput("clinFileInput", "Choose Clinical CSV File"),
      
     # actionButton("updateButton", "Update Charts")
  #  ),
    mainPanel(
      tabsetPanel(
        tabPanel("Eligibility",
                 plotOutput("pieChart2"),
                 plotOutput("pieChart1")
        ),
        tabPanel("Diagnosis Date",
                 plotOutput("histChartDiag"),
                 plotOutput("pieChartDiag")
        ),
        tabPanel("Demographics",
                 plotOutput("barChartClin1"),
                 plotOutput("barChartClin2")
        ),
        tabPanel("Clinical Data",
                 plotOutput("pieChartClin1"),
                 plotOutput("pieChartClin2")
        ),
        tabPanel("Geographic Data",
                # plotOutput("GeoChart1"),
                 plotOutput("GeoChart2"),
                # plotOutput("GeoChart3"),
                 plotOutput("GeoChart4")
        )
      )
    )
#  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  source("theme_DB.R")
  source("eligible.R")
  source("clinical_data_plots.R")
  source("demographic_plots.R")
  
  # Define the directory path
  .dir <- "~/Documents/" 
  
  # Define race levels for the plots
  rLevels <- c("ASIAN", "BLACK", "HISPANIC", "WHITE", "MIXED", "NA.AME/P.ISLA", "UNKNOWN")
  
  processedGeoData <- reactive({
    tryCatch({
      df <- read.csv("HS2100716BodourSalhi-ResidenceHistory_DATA_2024-02-08_1653.csv")
      names(df)[1] <- "ID"
      names(df) <- gsub("\\.$", "", names(df))
      df$location <- paste0(df$currentstaddr1, ", ", df$currentcity, ", ", df$currentstate, " ", df$currentzip)
      #df$location <- paste0(df$Current.Street.Address.1, ", ", df$Current.City, ", ", df$Current.State, " ", df$Current.Zip)
      lat_longs <- df |> 
        tidygeocoder::geocode(location, method = 'osm', lat = latitude, long = longitude)
      return(lat_longs)
      
    })
    
    
  })
  
  # Reactive expression for processing eligibility data
  processedData <- reactive({
    tryCatch({
      # Apply getEligiblity and additional data transformations
      ss.bcsb.ef <- getEligiblity("Website Eligibiity Survey Entries_2.8.24.csv") |> 
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
      shiny::showNotification(paste("Error processing Elig data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  
  # Reactive expression for processing clinical data
  processedClinData <- reactive({
    #req(input$clinFileInput) 
    #clinFile <- input$clinFileInput
    tryCatch({
      clin_dat <- getClinDatSimple("MasterList_2.8.24.csv") %>%
        dplyr::mutate(date.Dx = as.Date(gsub("; .*", "", BreastCancerDiagnosisDate),
                                        tryFormats = "%m/%d/%Y"))
      return(clin_dat)
    }, error = function(e) {
      # Return NULL or a default value if there's an error
      shiny::showNotification(paste("Error processing Clin data:", e$message), type = "error")
      return(NULL)
    })
    
  })
  
  processedDemoData <- reactive({
    tryCatch({
      demog_dat <- getDemogInfo("DemographicsRaceEduc_variable labels_2.15.24.csv")
      return(demog_dat)
    }, error = function(e) {
      # Return NULL or a default value if there's an error
      shiny::showNotification(paste("Error processing Demog data:", e$message), type = "error")
      return(NULL)
    })
    
  })

  
  DiagChartData1 <- reactive({
    #clin <- req(processedClinData())
    tryCatch({
      Diag <- getYrSinceDiagnosis("HS2100716BodourSalhi-BaselineTimeFromDxTo_DATA_2024-02-08_1737.csv")
      return(Diag$gp)
    }, error = function(e) {
      # Handle the error gracefully
      shiny::showNotification(paste("Error plotting Diag 1 data:", e$message), type = "error")
      return(NULL)
    })
    
    
  })
  
  DiagChartData2 <- reactive({
    #clin <- req(processedClinData())
    tryCatch({
      Diag <- getYrSinceDiagnosis("HS2100716BodourSalhi-BaselineTimeFromDxTo_DATA_2024-02-08_1737.csv")
      return(Diag$gp2)
    }, error = function(e) {
      # Handle the error gracefully
      shiny::showNotification(paste("Error plotting Diag 2 data:", e$message), type = "error")
      return(NULL)
    })
    
    
  })
  
  geoChartData1 <- reactive({
    lat_longs <- req(processedGeoData())
    tryCatch({
      geoPlot1 <- qmplot(longitude, latitude, data = lat_longs, colour = I("red"), maptype = "toner-lite", zoom = 5)
      #ggsave("plots/BCSB_locations.png", height = 6, width = 6, dpi = 600)
      return(geoPlot1)
      
    }, error = function(e) {
      # Handle the error gracefully
      shiny::showNotification(paste("Error plotting Geog 1 data:", e$message), type = "error")
      return(NULL)
    })
    
    
  })
  
  geoChartData2 <- reactive({
    lat_longs <- req(processedGeoData())
    tryCatch({
      lat_longs_CUS <- lat_longs |> 
        filter(!currentstate %in% c("AK", "HI", "Alaska", "Hawaii"))
      geoPlot2 <- qmplot(longitude, latitude, data = lat_longs_CUS, colour = I("red"), maptype = "toner-lite", zoom = 5)
      return(geoPlot2)
      
    }, error = function(e) {
      # Handle the error gracefully
      shiny::showNotification(paste("Error plotting Geog 2 data:", e$message), type = "error")
      return(NULL)
    })
    
    
  })
  
  geoChartData3 <- reactive({
    lat_longs <- req(processedGeoData())
    tryCatch({
      lat_longs_CA <- lat_longs[grepl("^ca", ignore.case = T, lat_longs$currentstate), ]
      geoPlot3 <- qmplot(longitude, latitude, data = lat_longs_CA, colour = I("red"), maptype = "toner-lite", zoom = 8)
      return(geoPlot3)
      
    }, error = function(e) {
      # Handle the error gracefully
      shiny::showNotification(paste("Error plotting Geog 3 data:", e$message), type = "error")
      return(NULL)
    })
    
    
  })
  
  geoChartData4 <- reactive({
    lat_longs <- req(processedGeoData())
    tryCatch({
      lat_longs_LA <- lat_longs[lat_longs$currentzip %in% 91001:93000, ] 
      geoPlot4 <- qmplot(longitude, latitude, data = lat_longs_LA, colour = I("red"), maptype = "toner-lite", zoom = 10)
      return(geoPlot4)
    }, error = function(e) {
      # Handle the error gracefully
      shiny::showNotification(paste("Error plotting Geog 4 data:", e$message), type = "error")
      return(NULL)
    })
    
    
  })
  
  barChart1Data <- reactive({
    bar_data <- req(processedDemoData())
    tryCatch({
      # Assuming getPie function generates the first pie chart
      res <- getRaceBar(bar_data$race_df)  # Update with appropriate parameters
      return(res)
    }, error = function(e) {
      # Handle the error gracefully
      shiny::showNotification(paste("Error plotting Demog 1 data:", e$message), type = "error")
      return(NULL)
    })
  })  
  
  barChart2Data <- reactive({
    bar_data <- req(processedDemoData())
    tryCatch({
      # Assuming getPie function generates the first pie chart
      res <- getEduBar(bar_data$edu_df)  # Update with appropriate parameters
      return(res)
    }, error = function(e) {
      # Handle the error gracefully
      shiny::showNotification(paste("Error plotting Demog 2 data:", e$message), type = "error")
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
      shiny::showNotification(paste("Error plotting Elig 1 data:", e$message), type = "error")
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
      shiny::showNotification(paste("Error plotting Elig 2 data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  
  pieChartClin1Data <- reactive({
    clin_dat <- req(processedClinData())
    # Generate first clinical data pie chart
    tryCatch({
    resClin1 <- getClinPie(clin_dat)
    return(resClin1)
    },error = function(e) {
      # Handle the error gracefully
      shiny::showNotification(paste("Error plotting Clin data:", e$message), type = "error")
      return(NULL)
    
    })
  }) 
  
  pieChartClin2Data <- reactive({
    clin_dat <- req(processedClinData())
    # Generate first clinical data pie chart
    tryCatch({
      resClin1 <- getClinMissing(clin_dat)
      return(resClin1)
    },error = function(e) {
      # Handle the error gracefully
      shiny::showNotification(paste("Error plotting Clin 2 data:", e$message), type = "error")
      return(NULL)
      
    })
  }) 

  # Within server function
  
  output$pieChart1 <- renderPlot({
    pieChart1Data()
  })
  
  
  output$histChartDiag <- renderPlot({
    DiagChartData1()
  })
  
  output$pieChartDiag <- renderPlot({
    DiagChartData2()
  })
  
  
  output$pieChart2 <- renderPlot({
    pieChart2Data()
  })
  
  output$pieChartClin1 <- renderPlot({
    pieChartClin1Data()
  })
  
  
  output$pieChartClin2 <- renderPlot({
    pieChartClin2Data()
  })
  
  output$barChartClin1 <- renderPlot({
    barChart1Data()
  })

  output$barChartClin2 <- renderPlot({
    barChart2Data()
  })
  
  #output$GeoChart1 <- renderPlot({
  #  geoChartData1()
  #})
  
 # output$GeoChart2 <- renderPlot({
  #  geoChartData2()
  #})
  
  #output$GeoChart3 <- renderPlot({
  #  geoChartData3()
 # })
  #output$GeoChart4 <- renderPlot({
  #  geoChartData4()
#  })

}

# Run the application 
shinyApp(ui = ui, server = server)
