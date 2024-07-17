
library(lubridate)


df.DxDate <- plt$df.DxDate
#df.DxDate <- df.DxDa

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
  
  # Create a complete set of month-group combinations
  all_months <- seq(as.Date(paste0(startDate, "-01")), as.Date(paste0(endDate, "-01")), by = "month")
  all_groups <- unique(df.merged[[enrollField]])
  complete_grid <- expand.grid(
    YearMonth = format(all_months, "%Y-%m"),
    Group = all_groups
  )
  names(complete_grid)[2] <- enrollField
  
  # Count enrollments by month and the specified field, including all combinations
  df.counts <- df.merged %>%
    group_by(YearMonth, !!sym(enrollField)) %>%
    summarise(Enrollment = n()) %>%
    ungroup() %>%
    right_join(complete_grid, by = c("YearMonth", enrollField)) %>%
    replace_na(list(Enrollment = 0))
  
  # Create the line chart
  lineChart <- ggplot(df.counts, aes(x = YearMonth, y = Enrollment, color = !!sym(enrollField), group = !!sym(enrollField))) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +  # Add points to highlight zero values
    scale_x_discrete(name = "Month") +
    scale_y_continuous(name = "Enrollment Count") +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Enrollment Assessment by ", enrollField, " for date range ", as.character(startDate), " - ", as.character(endDate))) +
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
  
  # Create a complete set of month-race combinations
  all_months <- seq(as.Date(paste0(startDate, "-01")), as.Date(paste0(endDate, "-01")), by = "month")
  all_races <- unique(df.merged[[enrollField]])
  complete_grid <- expand.grid(
    YearMonth = format(all_months, "%Y-%m"),
    Race = all_races
  )
  names(complete_grid)[2] <- enrollField
  
  # Count enrollments by month and race, including all combinations
  df.counts <- df.merged %>%
    group_by(YearMonth, !!sym(enrollField)) %>%
    summarise(Enrollment = n()) %>%
    ungroup() %>%
    right_join(complete_grid, by = c("YearMonth", enrollField)) %>%
    replace_na(list(Enrollment = 0))
  
  # Create the line chart
  lineChart <- ggplot(df.counts, aes(x = YearMonth, y = Enrollment, color = !!sym(enrollField), group = !!sym(enrollField))) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +  # Add points to highlight zero values
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
  lineChart
  ggsave(paste0("plots/MinAssesment.png"), lineChart, height = 9, width = 16, dpi = 600)
  
  return(lineChart)
}
chart6month <- getLineChart(df.enroll, df.DxDate, "Race", "2022-05", "2024-07")
chart6month

chartStart <- getLineChart(df.enroll, df.DxDate, "Race")
chartStart

chartStartE <- getLineChartEveryone(df.enroll, df.DxDate, "Race")
chartStartE

