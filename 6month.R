
library(lubridate)
library(patchwork)


source("EnrolmentFancy.R")



df.DxDate <- plt$df.DxDate
#df.DxDate <- df.DxDa

enrollField <- "Race"

getLineChartEveryone <- function(df.enroll, df.DxDate, enrollField, startDate = "2022-05", endDate = format(Sys.Date(), "%Y-%m")) {
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
  
  # Create a complete set of quarter-group combinations
  # Create a complete set of month-group combinations
  all_months <- seq(as.Date(paste0(startDate, "-01")), as.Date(paste0(endDate, "-01")), by = "month")
  
  all_groups <- unique(df.merged[[enrollField]])
  complete_grid <- expand.grid(
    YearMonth = format(all_months, "%Y-%m"),
    Group = all_groups
  )
  names(complete_grid)[2] <- enrollField
  
  # Count enrollments by YearMonth and the specified field, including all combinations
  df.counts <- df.merged %>%
    group_by(YearMonth, !!sym(enrollField)) %>%
    summarise(Enrollment = n()) %>%
    ungroup() %>%
    right_join(complete_grid, by = c("YearMonth", enrollField)) %>%
    replace_na(list(Enrollment = 0))
  
  # Create the line chart with quarters on x-axis
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
      # Axis labels (x and y labels)
      axis.title.x = element_text(size = 22, face = "bold", margin = margin(t = 20)),
      axis.title.y = element_text(size = 22, face = "bold", margin = margin(r = 20)),
      
      # Axis text (the values on the axes)
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 18),
      
      # Legend elements (you've already set these, but adjusted for consistency)
      legend.title = element_text(face = "bold", size = 20),
      legend.text = element_text(size = 19),
      legend.position = "top",
      
      # Plot title
      plot.title = element_text(face = "bold", size = 24, hjust = 0.5, margin = margin(b = 20)),
      
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black")
    )
  #lineChart
  ggsave(paste0("plots/EnrAssesment.png"), lineChart, height = 9, width = 12, dpi = 600)
  
  return(lineChart)
}
chartStartE(df.enroll, df.DxDate, "location")

getLineChartEveryoneQ <- function(df.enroll, df.DxDate, enrollField, startDate = "2022-05", endDate = format(Sys.Date(), "%Y-%m")) {
  # Merge the dataframes
  df.merged <- inner_join(df.enroll, df.DxDate, by = c("ID" = "bcsbusername"))
  
  # Convert 'EarliestConsentDate' to date format
  df.merged$EarliestConsentDate <- as.Date(df.merged$EarliestConsentDate)
  
  # Extract the year and month from 'EarliestConsentDate'
  df.merged$YearMonth <- format(df.merged$EarliestConsentDate, "%Y-%m")
  
  # Add a quarter column
  df.merged$Quarter <- paste0(format(df.merged$EarliestConsentDate, "%Y-Q"), 
                              quarter(df.merged$EarliestConsentDate))
  
  # Consolidate multiple race entries and filter out "White"
  if (enrollField == "Race") {
    df.merged[[enrollField]] <- ifelse(grepl(",", df.merged[[enrollField]]), "Multiple", df.merged[[enrollField]])
    df.merged <- df.merged[df.merged[[enrollField]] != "No race indicated", ]
  }
  
  # Filter the data based on the specified date range
  df.merged <- df.merged %>%
    filter(YearMonth >= startDate & YearMonth <= endDate)
  
  # Create a complete set of quarter-group combinations
  start_date <- as.Date(paste0(startDate, "-01"))
  end_date <- as.Date(paste0(endDate, "-01"))
  all_quarters <- unique(paste0(format(seq(start_date, end_date, by = "month"), "%Y-Q"), 
                                quarter(seq(start_date, end_date, by = "month"))))
  all_groups <- unique(df.merged[[enrollField]])
  complete_grid <- expand.grid(
    Quarter = all_quarters,
    Group = all_groups
  )
  names(complete_grid)[2] <- enrollField
  
  # Count enrollments by quarter and the specified field, including all combinations
  df.counts <- df.merged %>%
    group_by(Quarter, !!sym(enrollField)) %>%
    summarise(Enrollment = n()) %>%
    ungroup() %>%
    right_join(complete_grid, by = c("Quarter", enrollField)) %>%
    replace_na(list(Enrollment = 0))
  
  # Create the line chart with quarters on x-axis
  lineChart <- ggplot(df.counts, aes(x = Quarter, y = Enrollment, color = !!sym(enrollField), group = !!sym(enrollField))) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +  # Add points to highlight zero values
    scale_x_discrete(name = "Quarter") +
    scale_y_continuous(name = "Enrollment Count") +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Enrollment Assessment by ", enrollField, " for date range ", as.character(startDate), " - ", as.character(endDate))) +
    labs(color = enrollField) +
    theme_minimal() +
    theme(
      legend.title = element_text(face = "bold"),
      legend.position = "top",
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black")
    )
  #lineChart
  ggsave(paste0("plots/EnrAssesment.png"), lineChart, height = 9, width = 12, dpi = 600)
  
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
  
  max_guideline <- max(
    30 * length(unique(df.counts$YearMonth)),  # Max value of 30x line
    24 * length(unique(df.counts$YearMonth)),  # Max value of 24x line
    df.counts %>%                              # Max value of actual data
      group_by(!!sym(enrollField)) %>% 
      arrange(YearMonth) %>% 
      mutate(Cumulative = cumsum(Enrollment)) %>% 
      pull(Cumulative) %>% 
      max()
  )
  
  
  lineChart2 <- ggplot() +
    geom_line(data = df.counts %>% 
                group_by(!!sym(enrollField)) %>% 
                arrange(YearMonth) %>%
                mutate(Cumulative = cumsum(Enrollment)),
              aes(x = YearMonth, y = Cumulative, color = !!sym(enrollField), group = !!sym(enrollField)), 
              size = 1.2) +
    geom_point(data = df.counts %>% 
                 group_by(!!sym(enrollField)) %>% 
                 arrange(YearMonth) %>%
                 mutate(Cumulative = cumsum(Enrollment)),
               aes(x = YearMonth, y = Cumulative, color = !!sym(enrollField)), 
               size = 2) +
    geom_line(data = df.counts %>% 
                distinct(YearMonth) %>%
                mutate(
                  year = substr(YearMonth, 1, 4),
                  month_num = as.numeric(substr(YearMonth, 6, 7)),
                  y30 = 30 * (month_num - 1),  # starts at 0 for first month
                  y24 = 24 * (month_num - 1)
                ),
              aes(x = YearMonth, y = y30), linetype = "dashed", color = "gray50") +
    geom_line(data = df.counts %>% 
                distinct(YearMonth) %>%
                mutate(
                  year = substr(YearMonth, 1, 4),
                  month_num = as.numeric(substr(YearMonth, 6, 7)),
                  y30 = 30 * (month_num - 1),
                  y24 = 24 * (month_num - 1)
                ),
              aes(x = YearMonth, y = y24), linetype = "dotted", color = "gray50") +
    scale_x_discrete(name = "Month") +
    scale_y_continuous(name = "Cumulative Enrollment") +
    scale_color_brewer(palette = "Set2") +
    ggtitle(paste0("Cumulative Minority Enrollment for date range ", as.character(startDate), " - ", as.character(endDate))) +
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
  lineChart2
  ggsave(paste0("plots/MinAssesment.png"), lineChart, height = 9, width = 16, dpi = 600)
  ggsave(paste0("plots/CumulativeMinorityAssesment.png"), lineChart2, height = 9, width = 16, dpi = 600)
  
  return(lineChart)
}
#chart6month <- getLineChart(df.enroll, df.DxDate, "Race", "2022-05", "2024-07")
#chart6month

chartStart <- getLineChart(df.enroll, df.DxDate, "Race")
chartStart



chartStartE <- function(df.enroll, df.DxDate, fields){
  df.enroll <- df.enroll %>%
    mutate(location = ifelse( location == "Web", location, "Clinic"))
  
lineC <- getLineChartPublicationReady(
    df.enroll = df.enroll,
    df.DxDate = df.DxDate,
    enrollField = "location",
    startDate = "2022-05",
    endDate = "2025-05",
    plot_title = "Monthly Enrollment by Location",
    color_palette = "viridis",
    x_interval = 2,                    # Show every 2nd month on x-axis
    legend_position = "right",
    output_file = "plots/enrollment_by_loc",
    output_format = "pdf",
    width = 10,
    height = 6
  )
 return(lineC)
}
chartStartE(df.enroll, df.DxDate, "location")
chartStartE
