
library(lubridate)


df.DxDate <- plt$df.DxDate
enrollField <- "Race"
getStackedAreaChart <- function(df.enroll, df.DxDate, enrollField) {
  # Merge the dataframes
  df.merged <- inner_join(df.enroll,  df.DxDate, by = c("ID" = "bcsbusername"))
  
  # Convert 'EarliestConsentDate' to date format
  df.merged$EarliestConsentDate <- as.Date(df.merged$EarliestConsentDate)
  
  # Extract the year and month from 'EarliestConsentDate'
  df.merged$YearMonth <- format(df.merged$EarliestConsentDate, "%Y-%m")
  
  # Count enrollments by month and the specified field
  df.counts <- df.merged %>%
    group_by(YearMonth, !!sym(enrollField)) %>%
    summarise(Enrollment = n()) %>%
    ungroup()
  
  # Order the levels of the specified field based on total enrollment
  df.counts[[enrollField]] <- reorder(df.counts[[enrollField]], df.counts$Enrollment, sum)
  
  # Create the stacked area chart
  stackedAreaChart <- ggplot(df.counts, aes(x = YearMonth, y = Enrollment, fill = !!sym(enrollField))) +
    geom_area(position = "stack") +
    scale_x_discrete(name = "Month") +
    scale_y_continuous(name = "Enrollment") +
    ggtitle("Minority Enrollment Assessment") +
    theme_minimal() +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(stackedAreaChart)
}

chart <- getStackedAreaChart(df.enroll, plt$df.DxDate, "Race")

getStackedAreaChartRange <- function(df.enroll, df.DxDate, enrollField, startDate, endDate) {
  # Merge the dataframes
  df.merged <- inner_join(df.enroll, df.DxDate, by = c("ID" = "bcsbusername"))
  
  # Convert 'EarliestConsentDate' to date format
  df.merged$EarliestConsentDate <- as.Date(df.merged$EarliestConsentDate)
  
  # Extract the year and month from 'EarliestConsentDate'
  df.merged$YearMonth <- format(df.merged$EarliestConsentDate, "%Y-%m")
  
  # Filter the data based on the specified date range
  df.merged <- df.merged %>%
    filter(EarliestConsentDate >= as.Date(startDate) & EarliestConsentDate <= as.Date(endDate))
  
  # Count enrollments by month and the specified field
  df.counts <- df.merged %>%
    group_by(YearMonth, !!sym(enrollField)) %>%
    summarise(Enrollment = n()) %>%
    ungroup()
  
  # Order the levels of the specified field based on total enrollment
  df.counts[[enrollField]] <- reorder(df.counts[[enrollField]], df.counts$Enrollment, sum)
  
  # Create the stacked area chart
  stackedAreaChart <- ggplot(df.counts, aes(x = YearMonth, y = Enrollment, fill = !!sym(enrollField))) +
    geom_area(position = "stack") +
    scale_x_discrete(name = "Month") +
    scale_y_continuous(name = "Enrollment") +
    ggtitle("Minority Enrollment Assessment") +
    theme_minimal() +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(stackedAreaChart)
}

chartRange <- getStackedAreaChartRange(df.enroll, df.DxDate, "Race", "2022-01-01", "2022-06-30")
