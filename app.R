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
library(stringdist)
library(tidygeocoder)
library(leaflet)
library(plotly)
library(RColorBrewer)


ui <- fluidPage(
  titlePanel("BCSB Dashboard - June 25, 2024"),
  mainPanel(
    tabsetPanel(id = "mainTabset",
                tabPanel("Web Eligibility",
                         fluidRow(
                           column(4, selectInput("highlight_race", "Highlight Race:", 
                                                 choices = c("None", "Asian", "Black", "Hispanic", "White", "Unknown"),
                                                 selected = "None")),
                           column(4, radioButtons("view_type", "View Type:",
                                                  choices = c("Count", "Percentage"),
                                                  selected = "Count"))
                         ),
                         plotlyOutput("pieChart2"),
                         plotlyOutput("pieChart1")
                ),
                tabPanel("Screening",
                         fluidRow(
                           column(9, plotOutput("PCO")),
                           column(3, plotOutput("PCOC"))
                         ),
                         fluidRow(
                           column(9,plotOutput("PCR")),  
                           column(3, plotOutput("PCRC"))
                         )
                ),
                tabPanel("Retention",
                         fluidRow(
                           column(9, plotOutput("RETr")),
                           column(3, plotOutput("RETCr"))
                         ),
                         fluidRow(
                           column(9,plotOutput("RETc")),  
                           column(3, plotOutput("RETCc"))
                         ),
                         fluidRow(
                           column(9,plotOutput("RETu")),  
                           column(3, plotOutput("RETCu"))
                         )      
                ),
                tabPanel("Enrollment",
                         fluidRow(
                           column(9, plotOutput("ER")),
                           column(3, plotOutput("ERC"))
                         ),                     
                         fluidRow(
                           column(9, plotOutput("ES")),
                           column(3, plotOutput("ESC"))
                         ),
                         fluidRow(
                           column(9,plotOutput("EB")),  
                           column(3, plotOutput("EBC"))
                         )      
                ),
                tabPanel("Demographics",
                         fluidRow(
                           column(9, plotOutput("DR")),
                           column(3, plotOutput("DRC"))
                         ),
                         fluidRow(
                           column(9, plotOutput("DE")),
                           column(3, plotOutput("DEC"))
                         )
                ),
                tabPanel("Years since Diagnosis",
                         plotOutput("histChartDiag"),
                         plotOutput("pieChartDiag")
                ),
                tabPanel("Molecular Subtype Data",
                         plotOutput("pieChartClin1"),
                         plotOutput("pieChartClin2")
                ),
                tabPanel("Geographic Data",
                         fluidRow(
                           column(width = 12,
                                  selectInput("selectedColumnG", "Select Column:",
                                              choices = c("BloodDrawStatus", 
                                                          # "ExternalRecordsRequestStatus",
                                                          #  "ExternalRecordsDataEntryStatus",
                                                          "Race", "RecruitmentSource", "edu"),
                                              selected = "Race")
                           )
                         ),
                         fluidRow(
                           column(width = 12,
                                  leafletOutput("GeoChart2", height = "calc(100vh - 200px)")
                           )
                         )
                ),
                tabPanel("Enrollment Assessment",
                         fluidRow(
                           column(width = 12,
                                  selectInput("selectedColumnEA", "Select Column:",
                                              choices = c("Race", "location", "edu"),
                                              selected = "Race")
                           )
                           #                           column(width = 6,
                           #                                  textInput("startDate1", "Start Date (YYYY-MM):", value = "2022-05")
                           #                           ),
                           #                           column(width = 6,
                           #                                  textInput("endDate1", "End Date (YYYY-MM):", value = "2024-03")
                           #                           )
                         ),
                         fluidRow(
                           column(width = 12,
                                  plotOutput("ENRAS")
                           )
                         ),
                         #                         fluidRow(
                         #                           column(width = 6,
                         #                                  textInput("startDate2", "Start Date (YYYY-MM):", value = "2022-05")
                         #                           ),
                         #                           column(width = 6,
                         #                                  textInput("endDate2", "End Date (YYYY-MM):", value = "2024-03")
                         #                           )
                         #                         ),
                         fluidRow(
                           column(width = 12,
                                  plotOutput("MENRAS")
                           )
                         )
                )
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  source("theme_DB.R")
  source("eligible.R")
  source("enrollment.R")
  source("screening.R")
  source("clinical_data_plots.R")
  source("demographic_plots.R")
  source("BCSB_map.R")
  source("str_list.R")
  source("retention.R")
  source("6month.R")
  # Define the directory path
  .dir <- "~/Documents/" 
  
  # Define race levels for the plots
  #rLevels <- c("ASIAN", "BLACK", "HISPANIC", "WHITE", "MIXED", "NA.AME/P.ISLA", "UNKNOWN")
  
  processedGeoData <- reactive({
    tryCatch({
      lat_longs <- get_LatLong(dx_str)
      return(lat_longs)
    }, error = function(e) {
      # Return NULL or a default value if there's an error
      shiny::showNotification(paste("Error processing Geog data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Reactive expression for processing eligibility data
  processedData <- reactive({
    tryCatch({
      # Apply getEligiblity and additional data transformations
      ss.bcsb.ef <- getEligiblity(eligible_str) |> 
        dplyr::rename(Race = What.is.your.race.ethnicity.) |> 
        dplyr::mutate(Race = ifelse(is.na(Race) | Race %in% "Prefer not to answer", "Unknown", Race),
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
      clin_dat <- getClinDatSimple(master_str) %>%
        dplyr::mutate(date.Dx = as.Date(gsub("; .*", "", BreastCancerDiagnosisDate),
                                        tryFormats = "%m/%d/%Y"))
      return(clin_dat)
    }, error = function(e) {
      # Return NULL or a default value if there's an error
      shiny::showNotification(paste("Error processing Clin data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  processedEnroll <- reactive({
    tryCatch({
      EC <- getEnrollment(enroll_str) 
      return(EC)
    }, error = function(e) {
      # Return NULL or a default value if there's an error
      shiny::showNotification(paste("Error processing Phone Consult:", e$message), type = "error")
      return(NULL)
    })
  })
  
  processedPhoneConsult <- reactive({
    tryCatch({
      PC <- getPhoneConsult(screen_str) 
      return(PC)
    }, error = function(e) {
      # Return NULL or a default value if there's an error
      shiny::showNotification(paste("Error processing Phone Consult:", e$message), type = "error")
      return(NULL)
    })
  })
  
  processedDemoData <- reactive({
    tryCatch({
      demog_dat <- getDemogInfo(demographics_str)
      return(demog_dat)
    }, error = function(e) {
      # Return NULL or a default value if there's an error
      shiny::showNotification(paste("Error processing Demog data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  
  
  # Within server function
  
  output$ENRAS <- renderPlot({
    #clin <- req(processedClinData())
    PC <- req(processedEnroll())
    startDate <- input$dateRange1[1]
    endDate <- input$dateRange1[2]
    tryCatch({
      selectedColumn <- input$selectedColumnEA
      
      # Diag <- getYrSinceDiagnosis(dx_str, clin)
      Diag <- getEarliestConsentDate(dx_strFull)
      chart <- getLineChartEveryone(PC, Diag, selectedColumn)
      #      chart <- getLineChartEveryone(PC, Diag$df.DxDate, "Race", startDate, endDate)
      return(chart)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting Assess 1 data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$MENRAS <- renderPlot({
    #clin <- req(processedClinData())
    PC <- req(processedEnroll())
    startDate <- input$dateRange2[1]
    endDate <- input$dateRange2[2]
    tryCatch({
      #Diag <- getYrSinceDiagnosis(dx_str, clin)
      Diag <- getEarliestConsentDate(dx_strFull)
      chart <- getLineChart(PC, Diag, "Race")
      #      chart <- getLineChart(PC, Diag$df.DxDate, "Race", startDate, endDate)
      return(chart)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting Assess 2 data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  
  output$GeoChart2 <- renderLeaflet({
    ll <- req(processedGeoData())
    enrolled <- req(processedEnroll())
    enrolled$RecruitmentSource <- enrolled$location
    enrolled[["Race"]] <- ifelse(grepl(",", enrolled[["Race"]]), "Multiple", enrolled[["Race"]])
    tryCatch({
      lat_longs <- inner_join(enrolled, ll, by = c("ID" = "ID"))
      
      # Get the selected column from input
      selectedColumn <- input$selectedColumnG
      
      # Create the color palette based on the selected column
      colorPalette <- colorFactor(palette = "Set1", domain = unique(lat_longs[[selectedColumn]]))
      
      geoPlot2 <- leaflet(data = lat_longs) %>%
        #addTiles() %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addCircleMarkers(
          ~longitude,
          ~latitude,
          color = ~colorPalette(lat_longs[[selectedColumn]]),
          radius = 3,
          popup = ~paste(
            selectedColumn, ": ", lat_longs[[selectedColumn]]
          )
        ) %>%
        addLegend(
          "bottomright",
          pal = colorPalette,
          values = ~lat_longs[[selectedColumn]],
          title = selectedColumn,
          opacity = 0.8
        ) %>%
        setView(
          lng = mean(lat_longs$longitude, na.rm = TRUE),
          lat = mean(lat_longs$latitude, na.rm = TRUE),
          zoom = 5
        )
      
      return(geoPlot2)
    }, error = function(e) {
      # Handle the error gracefully
      shiny::showNotification(paste("Error plotting Geog 2 data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$histChartDiag <- renderPlot({
    clin <- req(processedClinData())
    tryCatch({
      Diag <- getYrSinceDiagnosis(dx_str, clin)
      return(Diag$gp)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting Diag 1 data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  
  output$pieChartDiag <- renderPlot({
    clin <- req(processedClinData())
    tryCatch({
      Diag <- getYrSinceDiagnosis(dx_str, clin)
      return(Diag$gp2)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting Diag 2 data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$EBC <- renderPlot({
    PC <- req(processedEnroll())
    tryCatch({
      EBC <- getEBloodpieComb(PC)
      return(EBC)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting EBC Comb data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$EB <- renderPlot({
    PC <- req(processedEnroll())
    tryCatch({
      PCO <- getEBloodpie(PC)
      return(PCO)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting EB data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$DRC <- renderPlot({
    PC <- req(processedEnroll())
    tryCatch({
      DRC <- getDRpieComb(PC)
      return(DRC)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting DR Comb data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$DR <- renderPlot({
    PC <- req(processedEnroll())
    tryCatch({
      PCO <- getDRpie(PC)
      return(PCO)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting DR data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$DEC <- renderPlot({
    PC <- req(processedEnroll())
    tryCatch({
      DEC <- getDEpieComb(PC)
      return(DEC)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting DE Comb data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$DE <- renderPlot({
    PC <- req(processedEnroll())
    tryCatch({
      PCO <- getDEpie(PC)
      return(PCO)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting DE data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$ERC <- renderPlot({
    PC <- req(processedEnroll())
    tryCatch({
      ESC <- getERacepieComb(PC)
      return(ESC)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting ER Comb data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$ESC <- renderPlot({
    PC <- req(processedEnroll())
    tryCatch({
      ESC <- getESurveypieComb(PC)
      return(ESC)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting ES Comb data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$ER <- renderPlot({
    PC <- req(processedEnroll())
    tryCatch({
      PCO <- getERacepie(PC)
      return(PCO)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting ER data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$ES <- renderPlot({
    PC <- req(processedEnroll())
    tryCatch({
      PCO <- getESurveypie(PC)
      return(PCO)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting ES data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$RETCr <- renderPlot({
    PC <- req(processedPhoneConsult())
    tryCatch({
      PCO <- getRetCOMBpie(PC, "Refused", "'Refused'")
      return(PCO)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting Ret Comb r data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$RETr <- renderPlot({
    PC <- req(processedPhoneConsult())
    tryCatch({
      PCO <- getRetpie(PC, "Refused", "'Refused'")
      return(PCO)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting Ret r data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$RETCc <- renderPlot({
    PC <- req(processedPhoneConsult())
    tryCatch({
      PCO <- getRetCOMBpie(PC, "Considering", "'Considering'")
      return(PCO)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting Ret Comb c data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$RETc <- renderPlot({
    PC <- req(processedPhoneConsult())
    tryCatch({
      PCO <- getRetpie(PC, "Considering", "'Considering'")
      return(PCO)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting Ret c data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$RETCu <- renderPlot({
    PC <- req(processedPhoneConsult())
    tryCatch({
      PCO <- getRetCOMBpie(PC, "Unable to Contact", "'Unable to Contact'")
      return(PCO)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting Ret Comb u data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$RETu <- renderPlot({
    PC <- req(processedPhoneConsult())
    tryCatch({
      PCO <- getRetpie(PC, "Unable to Contact", "'Unable to Contact'")
      return(PCO)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting Ret u data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$PCOC <- renderPlot({
    PC <- req(processedPhoneConsult())
    tryCatch({
      PCO <- getPCFpieComb(PC)
      return(PCO)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting PCO Comb data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$PCO <- renderPlot({
    PC <- req(processedPhoneConsult())
    tryCatch({
      PCO <- getPCFpie(PC)
      return(PCO)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting PCO data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$PCRC <- renderPlot({
    PC <- req(processedPhoneConsult())
    tryCatch({
      PCO <- getPCRpieComb(PC)
      return(PCO)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting PCR Comb data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$PCR <- renderPlot({
    PC <- req(processedPhoneConsult())
    tryCatch({
      PCO <- getPCRpie(PC)
      return(PCO)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting PCR data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$demogPieChart1 <- renderPlot({
    bar_data <- req(processedDemoData())
    tryCatch({
      res <- getRacePie(bar_data$race_df)
      return(res)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting Demog 1 data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$demogPieChart2 <- renderPlot({
    bar_data <- req(processedDemoData())
    tryCatch({
      res <- getEduPie(bar_data$edu_df)
      return(res)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting Demog 2 data:", e$message), type = "error")
      return(NULL)
    })
  })
  # Update race choices based on available data
  observe({
    req(processedData())
    race_choices <- c("None", unique(processedData()$Race))
    updateSelectInput(session, "highlight_race", choices = race_choices)
  })
  
  # Create a color palette
  color_palette <- reactive({
    req(processedData())
    races <- unique(processedData()$Race)
    n <- length(races)
    colors <- brewer.pal(max(3, n), "Set3")[1:n]
    setNames(colors, races)
  })
  
  # Function to create pie chart
  create_pie_chart <- function(data, title) {
    if (input$view_type == "Percentage") {
      data <- data %>% mutate(value = count / sum(count) * 100)
      text_template <- "%{label}: %{value:.1f}%"
    } else {
      data <- data %>% mutate(value = count)
      text_template <- "%{label}: %{value}"
    }
    
    colors <- color_palette()[data$Race]
    if(input$highlight_race != "None") {
      colors[data$Race == input$highlight_race] <- "red"
    }
    
    plot_ly(data, labels = ~Race, values = ~value, type = 'pie',
            marker = list(colors = colors,
                          line = list(color = 'black', width = 1)),
            textposition = 'inside',
            textinfo = 'label+percent',
            hoverinfo = 'text',
            text = ~paste(Race, ":", count, "(", sprintf("%.1f%%", value/sum(value)*100), ")"),
            insidetextfont = list(color = '#FFFFFF')) %>%
      layout(title = list(text = title, font = list(size = 16)),
             showlegend = TRUE,
             legend = list(orientation = "h", y = -0.1),
             margin = list(t = 50, b = 50))
  }
  
  # Pie Chart 1: Overall Race Breakdown
  output$pieChart1 <- renderPlotly({
    req(processedData())
    
    race_data <- processedData() %>%
      group_by(Race) %>%
      summarise(count = n())
    
    create_pie_chart(race_data, "Overall Race Breakdown")
  })
  
  # Pie Chart 2: Race Breakdown by Diagnosis/Eligibility
  output$pieChart2 <- renderPlotly({
    req(processedData())
    
    diagnosis_data <- processedData() %>%
      group_by(diagnosis, Race) %>%
      summarise(count = n(), .groups = 'drop')
    
    if (input$view_type == "Percentage") {
      diagnosis_data <- diagnosis_data %>%
        group_by(diagnosis) %>%
        mutate(value = count / sum(count) * 100)
    } else {
      diagnosis_data <- diagnosis_data %>% mutate(value = count)
    }
    
    colors <- color_palette()[diagnosis_data$Race]
    if(input$highlight_race != "None") {
      colors[diagnosis_data$Race == input$highlight_race] <- "red"
    }
    
    plot_ly() %>%
      add_pie(data = subset(diagnosis_data, diagnosis == "BCSB ELIGIBLE"),
              labels = ~Race, values = ~value, name = "BCSB ELIGIBLE",
              domain = list(row = 0, column = 0),
              marker = list(colors = colors[diagnosis_data$diagnosis == "BCSB ELIGIBLE"],
                            line = list(color = 'black', width = 1)),
              textposition = 'inside',
              textinfo = 'label+percent',
              hoverinfo = 'text',
              text = ~paste(Race, ":", count, "(", sprintf("%.1f%%", value/sum(value)*100), ")"),
              insidetextfont = list(color = '#FFFFFF')) %>%
      add_pie(data = subset(diagnosis_data, diagnosis == "BCSB INELIGIBLE"),
              labels = ~Race, values = ~value, name = "BCSB INELIGIBLE",
              domain = list(row = 0, column = 1),
              marker = list(colors = colors[diagnosis_data$diagnosis == "BCSB INELIGIBLE"],
                            line = list(color = 'black', width = 1)),
              textposition = 'inside',
              textinfo = 'label+percent',
              hoverinfo = 'text',
              text = ~paste(Race, ":", count, "(", sprintf("%.1f%%", value/sum(value)*100), ")"),
              insidetextfont = list(color = '#FFFFFF')) %>%
      layout(title = list(text = "Race Breakdown by Eligibility", font = list(size = 16)),
             grid = list(rows = 1, columns = 2),
             showlegend = TRUE,
             legend = list(orientation = "h", y = -0.1),
             margin = list(t = 50, b = 50),      
             annotations = list(
               list(x = 0.2, y = -0.1, text = "BCSB ELIGIBLE", showarrow = FALSE, xref = "paper", yref = "paper"),
               list(x = 0.8, y = -0.1, text = "BCSB INELIGIBLE", showarrow = FALSE, xref = "paper", yref = "paper")
             ))
  })
  
  
  
  output$pieChartClin1 <- renderPlot({
    clin_dat <- req(processedClinData())
    tryCatch({
      resClin1 <- getClinPie(clin_dat)
      return(resClin1)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting Clin data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$pieChartClin2 <- renderPlot({
    PC <- req(processedEnroll())
    tryCatch({
      PCO <- getERrecpieComb(PC)
      return(PCO)
    }, error = function(e) {
      shiny::showNotification(paste("Error plotting Clin 2 data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
