

source("str_list.R")
#source("BCSB_utils.R")

read_csv_skip_headers <- function(file_path) {
  # Read in the full file first to extract the column names from the 5th row
  headers <- readLines(file_path, n = 5)
  column_names <- unlist(strsplit(headers[5], ","))
  
  # Trim whitespace but keep spaces within column names
  column_names <- trimws(column_names)
  
  # Read the actual data, skipping the first 5 rows (4 header rows + column names row)
  data <- read.csv(file_path, 
                   skip = 5,                # Skip first 5 rows
                   header = FALSE,          # No header in the data portion
                   stringsAsFactors = FALSE, # Don't convert strings to factors
                   check.names = FALSE)     # Don't modify column names
  
  # Assign the extracted column names to the data frame
  names(data) <- column_names
  
  return(data)
}


#df <- read.csv()
df <- read_csv_skip_headers(openSpecimen)

library(dplyr)
library(lubridate)

# First, ensure date-time values are properly formatted and get unique records
result <- df %>%
  # Convert the date-time column to proper datetime format if needed
  mutate(`Specimen_Collection Event_Date and Time` = 
           as.POSIXct(`Specimen_Collection Event_Date and Time`, format="%m/%d/%Y %H:%M")) %>%
  # Group by Participant_PPID and Visit_Event Label to get unique records
  group_by(Participant_PPID, `Visit_Event Label`) %>%
  summarize(`Specimen_Collection Event_Date and Time` = first(`Specimen_Collection Event_Date and Time`),
            .groups = 'keep')


df.consent <- getEarliestConsentDate(dx_str)

joined_df_modified <- df.consent %>%
  mutate(
    # Extract numeric part (remove prefix)
    Numeric_ID = as.numeric(str_extract(bcsbusername, "\\d+")),
    # Remove leading zeros
    #    Simple_ID = as.character(Numeric_ID)
  )


resultTest <-  result %>%
  inner_join(joined_df_modified, by = c( "Participant_PPID" = "Numeric_ID")) 

# Now, add both baseline and interval time differences
result_with_diffs <- resultTest %>%
  # Make sure data is sorted by collection date within each participant
  arrange(Participant_PPID, `Specimen_Collection Event_Date and Time`) %>%
  group_by(Participant_PPID) %>%
  mutate(
    # Get the baseline date (first timepoint for each participant)
    Baseline_Date = first(`Specimen_Collection Event_Date and Time`),
    EarliestConsentDate = first(`EarliestConsentDate`),
    # Calculate time from baseline in days
    Time_From_Baseline_Days = as.numeric(difftime(`Specimen_Collection Event_Date and Time`, 
                                                  Baseline_Date, 
                                                  units = "days")),
    
    # Calculate time difference from previous collection in days
    Interval_Days = as.numeric(difftime(`Specimen_Collection Event_Date and Time`, 
                                        lag(`Specimen_Collection Event_Date and Time`), 
                                        units = "days")),
    
    # Calculate time from baseline in days
    Time_From_Consent_Days = as.numeric(difftime(`Specimen_Collection Event_Date and Time`, 
                                                 EarliestConsentDate, 
                                                  units = "days")),
    
  # First record for each participant will have NA for interval difference
    Interval_Days = ifelse(is.na(Interval_Days), 0, Interval_Days)
  ) %>%
  ungroup()




# Step 3: Final grouping and collapsing all columns
gresult_with_diffs <- result_with_diffs %>% 
  group_by(Participant_PPID) %>% 
  summarize(
    `Visit_Event Labels` = toString(`Visit_Event Label`),
    `Collection_Dates` = toString(`Specimen_Collection Event_Date and Time`),
    `EarliestConsentDate` = first(`EarliestConsentDate`),
    `Time_From_Baseline_Dayss` = toString(Time_From_Baseline_Days),
    `Time_From_Consent_Dayss` = toString(Time_From_Consent_Days),
    `Interval_Dayss` = toString(Interval_Days),
    .groups = 'drop'
  )

gresult_with_diffs <- gresult_with_diffs %>%
  mutate(
    Has_Baseline = grepl("Baseline", `Visit_Event Labels`),
    Has_Y1 = grepl("Y1 Follow Up", `Visit_Event Labels`),
    Has_Y2 = grepl("Y2 Follow Up", `Visit_Event Labels`),
    Visit_TypeCOMB = case_when(
      Has_Baseline & !Has_Y1 & !Has_Y2 ~ "Baseline Only",
      !Has_Baseline & Has_Y1 & !Has_Y2 ~ "Y1 Only",
      !Has_Baseline & !Has_Y1 & Has_Y2 ~ "Y2 Only",
      Has_Baseline & Has_Y1 & !Has_Y2 ~ "Baseline + Y1",
      Has_Baseline & !Has_Y1 & Has_Y2 ~ "Baseline + Y2",
      !Has_Baseline & Has_Y1 & Has_Y2 ~ "Y1 + Y2",
      Has_Baseline & Has_Y1 & Has_Y2 ~ "Baseline + Y1 + Y2",
      TRUE ~ "Other"
    )
  )

gresult_with_diffs <- gresult_with_diffs %>%
  mutate(
    Primary_Visit = case_when(
      Has_Baseline & !Has_Y1 & !Has_Y2 ~ "Baseline",
      Has_Baseline & Has_Y1 & !Has_Y2 ~ "Y1 Follow Up",
      Has_Baseline & Has_Y1 & Has_Y2 ~ "Y2 Follow Up",
      TRUE ~ "Other"
    )
  )


result_with_visit_types <- result_with_diffs %>%
  mutate(
    Has_Baseline = grepl("Baseline", `Visit_Event Label`),
    Has_Y1 = grepl("Y1 Follow Up", `Visit_Event Label`),
    Has_Y2 = grepl("Y2 Follow Up", `Visit_Event Label`),
    Visit_Type = case_when(
      Has_Baseline & !Has_Y1 & !Has_Y2 ~ "Baseline Only",
      !Has_Baseline & Has_Y1 & !Has_Y2 ~ "Y1 Only",
      !Has_Baseline & !Has_Y1 & Has_Y2 ~ "Y2 Only", 
      Has_Baseline & Has_Y1 & !Has_Y2 ~ "Baseline + Y1",
      Has_Baseline & !Has_Y1 & Has_Y2 ~ "Baseline + Y2",
      !Has_Baseline & Has_Y1 & Has_Y2 ~ "Y1 + Y2",
      Has_Baseline & Has_Y1 & Has_Y2 ~ "Baseline + Y1 + Y2",
      TRUE ~ "Other"
    ),
    Primary_Visit_Type = case_when(
      Has_Baseline & !Has_Y1 & !Has_Y2 ~ "Baseline",
      Has_Y1 ~ "Y1 Follow Up",
      Has_Y2 ~ "Y2 Follow Up",
      TRUE ~ "Other"
    )
  )

gresult_with_visit_types <- result_with_visit_types %>% 
  group_by(Participant_PPID) %>% 
  summarize(
    #`Visit_Event Labels` = toString(`Visit_Event Label`),
    #`Collection_Dates` = toString(`Specimen_Collection Event_Date and Time`),
    `Baseline_Dates` = toString(Baseline_Date),
    #`Time_From_Baseline_Days` = toString(Time_From_Baseline_Days),
    #`Interval_Days` = toString(Interval_Days),
    #`Has_Baselines` = toString(Has_Baseline),
    #`Has_Y1s` = toString(Has_Y1),
    #`Has_Y2s` = toString(Has_Y2),
    #`Visit_Types` = toString(Visit_Type),
    #`Primary_Visit_Types` = toString(Primary_Visit_Type),
    .groups = 'drop'
  )

merged_full <- result_with_visit_types %>% 
  select(Participant_PPID, "Visit_Event Label", Baseline_Date, Time_From_Baseline_Days, Time_From_Consent_Days,
         Interval_Days,Visit_Type,Primary_Visit_Type) %>%
  left_join(gresult_with_diffs, by = "Participant_PPID") 






# Calculate the difference for records where Primary_Visit_Type is "Baseline"
getDF <- function(merged_full){
  DF <- merged_full %>%
  mutate(
    Time_From_Baseline_Days = case_when(
      Primary_Visit_Type == "Baseline" ~ as.numeric(difftime(Baseline_Date, EarliestConsentDate, units = "days")),
      TRUE ~ Time_From_Baseline_Days  # Keep existing values for non-baseline visits
    ),
    Interval_Days = case_when(
      Primary_Visit_Type == "Baseline" ~ as.numeric(difftime(Baseline_Date, EarliestConsentDate, units = "days")),
      TRUE ~ Interval_Days  # Keep existing values for non-baseline visits
    ),
    Participant_PPID = paste0("BCSB", sprintf("%04s", Participant_PPID))  
  )
return(DF)
  }

getDFBaseline <- function(merged_full){
  DF <- merged_full %>%
    mutate(
      Time_From_Baseline_Days = case_when(
        Primary_Visit_Type == "Baseline" ~ as.numeric(difftime(Baseline_Date, EarliestConsentDate, units = "days")),
        TRUE ~ Time_From_Baseline_Days  # Keep existing values for non-baseline visits
      ),
      Interval_Days = case_when(
        Primary_Visit_Type == "Baseline" ~ as.numeric(difftime(Baseline_Date, EarliestConsentDate, units = "days")),
        TRUE ~ Interval_Days  # Keep existing values for non-baseline visits
      ),
      Participant_PPID = paste0("BCSB", sprintf("%04s", Participant_PPID))  
    ) %>%
    # Filter to keep only rows where Visit_Event Label is "Baseline"
    filter(`Visit_Event Label` == "Baseline")
  return(DF)
}

DF <- getDF(merged_full)

#full DF, part X of full super matrix
write_csv(DF,"out/BWB-BCSB-openspecimen_consent.matrix.csv")

library(ggplot2)
library(gridExtra) # For arranging multiple plots
library(scales)    # For better axis formatting

# Starting with result_with_diffs dataframe which contains:
# - Participant_PPID
# - Visit_Event Label
# - Specimen_Collection Event_Date and Time
# - Baseline_Date
# - Time_From_Baseline_Days
# - Interval_Days

# ===== 1. OVERVIEW STATISTICS =====

# Basic statistics for interval days
interval_stats <- DF %>%
  group_by(`Visit_Event Label`) %>%
  filter(Interval_Days > 0) %>%
  summarize(
    Mean_Interval = mean(Interval_Days, na.rm = TRUE),
    Median_Interval = median(Interval_Days, na.rm = TRUE),
    SD_Interval = sd(Interval_Days, na.rm = TRUE),
    CV_Interval = SD_Interval / Mean_Interval * 100, # Coefficient of variation
    Min_Interval = min(Interval_Days, na.rm = TRUE),
    Max_Interval = max(Interval_Days, na.rm = TRUE),
    Q1_Interval = quantile(Interval_Days, 0.25, na.rm = TRUE),
    Q3_Interval = quantile(Interval_Days, 0.75, na.rm = TRUE),
    IQR_Interval = Q3_Interval - Q1_Interval,
    Count = n(),
    Pct_Within_30d_Of_Year = mean(abs(Interval_Days - 365) <= 30, na.rm = TRUE) * 100
  )

# Basic statistics for time from baseline
baseline_stats <- merged_full %>%
  group_by(`Visit_Event Label`) %>%
  filter(Time_From_Baseline_Days > 0) %>%
  summarize(
    Mean_Baseline_Time = mean(Time_From_Baseline_Days, na.rm = TRUE),
    Median_Baseline_Time = median(Time_From_Baseline_Days, na.rm = TRUE),
    SD_Baseline_Time = sd(Time_From_Baseline_Days, na.rm = TRUE),
    CV_Baseline = SD_Baseline_Time / Mean_Baseline_Time * 100,
    Min_Baseline_Time = min(Time_From_Baseline_Days, na.rm = TRUE),
    Max_Baseline_Time = max(Time_From_Baseline_Days, na.rm = TRUE),
    Q1_Baseline_Time = quantile(Time_From_Baseline_Days, 0.25, na.rm = TRUE),
    Q3_Baseline_Time = quantile(Time_From_Baseline_Days, 0.75, na.rm = TRUE),
    IQR_Baseline = Q3_Baseline_Time - Q1_Baseline_Time,
    Count = n()
  )

consent_stats <- merged_full %>%
  group_by(`Visit_Event Label`) %>%
  #filter(Time_From_Consent_Days > 0) %>%
  summarise(
    Mean_Baseline_Time = mean(Time_From_Consent_Days, na.rm = TRUE),
    Median_Baseline_Time = median(Time_From_Consent_Days, na.rm = TRUE),
    SD_Baseline_Time = sd(Time_From_Consent_Days, na.rm = TRUE),
    Min_Baseline_Time = min(Time_From_Consent_Days, na.rm = TRUE),
    Max_Baseline_Time = max(Time_From_Consent_Days, na.rm = TRUE),
    Q1_Baseline_Time = quantile(Time_From_Consent_Days, 0.25, na.rm = TRUE),
    Q3_Baseline_Time = quantile(Time_From_Consent_Days, 0.75, na.rm = TRUE),
    IQR_Baseline = Q3_Baseline_Time - Q1_Baseline_Time,
    Count = n()
  )


