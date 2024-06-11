
library(lubridate)


df.DxDate <- plt$df.DxDate
enrollField <- "Race"

getLineChartEveryone <- function(df.enroll, df.DxDate, enrollField, startDate = "2022-05", endDate = format(Sys.Date(), "%Y-%m") ) {
  # Merge the dataframes
  df.merged <- inner_join(df.enroll, df.DxDate, by = c("ID" = "bcsbusername"))
  
  # Convert 'EarliestConsentDate' to date format
  df.merged$EarliestConsentDate <- as.Date(df.merged$EarliestConsentDate)
  
  # Extract the year and month from 'EarliestConsentDate'
  df.merged$YearMonth <- format(df.merged$EarliestConsentDate, "%Y-%m")
  
  # Consolidate multiple race entries and filter out "White"
  if (enrollField == "Race") {
    df.merged[[enrollField]] <- ifelse(grepl(",", df.merged[[enrollField]]), "Multiple", df.merged[[enrollField]])
    df.merged <- df.merged[df.merged[[enrollField]] != "No race indicated", ]
  }
  
  
  # Filter the data based on the specified date range
  df.merged <- df.merged %>%
    filter(YearMonth >= startDate & YearMonth <= endDate)
  
  # Count enrollments by month and the specified field
  df.counts <- df.merged %>%
    group_by(YearMonth, !!sym(enrollField)) %>%
    summarise(Enrollment = n()) %>%
    ungroup()
  
  lineChart <- ggplot(df.counts, aes(x = YearMonth, y = Enrollment, color = !!sym(enrollField), group = !!sym(enrollField))) +
    geom_line(size = 1.2) +
    scale_x_discrete(name = "Month") +
    scale_y_continuous(name = "Enrollment Count") +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Enrollment Assessment by ",enrollField, " for date range ", as.character(startDate), " - ", as.character(endDate))) +
    labs(color = enrollField) +
    theme_minimal() +
    theme(
      legend.title = element_text(face = "bold"),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black")
    )
  ggsave(paste0("plots/EnrAssesment.png"), lineChart, height = 9, width = 16, dpi = 600)
  
  return(lineChart)
}

startDate <- "2022-05"
endDate <- format(Sys.Date(), "%Y-%m")
getLineChart <- function(df.enroll, df.DxDate, enrollField, startDate = "2022-05", endDate = format(Sys.Date(), "%Y-%m") ) {
  # Merge the dataframes
  df.merged <- inner_join(df.enroll, df.DxDate, by = c("ID" = "bcsbusername"))
  
  # Convert 'EarliestConsentDate' to date format
  df.merged$EarliestConsentDate <- as.Date(df.merged$EarliestConsentDate)
  
  # Extract the year and month from 'EarliestConsentDate'
  df.merged$YearMonth <- format(df.merged$EarliestConsentDate, "%Y-%m")
  
  # Consolidate multiple race entries and filter out "White"
  if (enrollField == "Race") {
    df.merged[[enrollField]] <- ifelse(grepl(",", df.merged[[enrollField]]), "Multiple", df.merged[[enrollField]])
    df.merged <- df.merged[df.merged[[enrollField]] != "White", ]
    df.merged <- df.merged[df.merged[[enrollField]] != "No race indicated", ]
    
  }
  
  
  # Filter the data based on the specified date range
  df.merged <- df.merged %>%
    filter(YearMonth >= startDate & YearMonth <= endDate)
  
  # Count enrollments by month and the specified field
  df.counts <- df.merged %>%
    group_by(YearMonth, !!sym(enrollField)) %>%
    summarise(Enrollment = n()) %>%
    ungroup()
  
  lineChart <- ggplot(df.counts, aes(x = YearMonth, y = Enrollment, color = !!sym(enrollField), group = !!sym(enrollField))) +
    geom_line(size = 1.2) +
    scale_x_discrete(name = "Month") +
    scale_y_continuous(name = "Enrollment Count") +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Minority Enrollment Assessment for date range ", as.character(startDate), " - ", as.character(endDate))) +
    labs(color = "Race") +
    theme_minimal() +
    theme(
      legend.title = element_text(face = "bold"),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black")
    )
  ggsave(paste0("plots/MinAssesment.png"), lineChart, height = 9, width = 16, dpi = 600)
  
  return(lineChart)
}
chart6month <- getLineChart(df.enroll, df.DxDate, "Race", "2023-07", "2024-02")
chart6month

chartStart <- getLineChart(df.enroll, df.DxDate, "Race")
chartStart

chartStartE <- getLineChartEveryone(df.enroll, df.DxDate, "Race")
chartStartE

