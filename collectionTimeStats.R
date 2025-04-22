

source("str_list.R")
#source("BCSB_utils.R")



#df <- read.csv()
df <- OS_Whole_Blood_CollectionDateTime

library(dplyr)
library(lubridate)

# First, ensure date-time values are properly formatted and get unique records
result <- df %>%
  # Convert the date-time column to proper datetime format if needed
  mutate(`Specimen_Collection Event_Date and Time` = 
           as.POSIXct(`Specimen_Collection Event_Date and Time`, format="%m-%d-%Y %H:%M")) %>%
  # Group by Participant_PPID and Visit_Event Label to get unique records
  group_by(Participant_PPID, `Visit_Event Label`) %>%
  summarize(`Specimen_Collection Event_Date and Time` = first(`Specimen_Collection Event_Date and Time`),
            .groups = 'keep')

# Now, add both baseline and interval time differences
result_with_diffs <- result %>%
  # Make sure data is sorted by collection date within each participant
  arrange(Participant_PPID, `Specimen_Collection Event_Date and Time`) %>%
  group_by(Participant_PPID) %>%
  mutate(
    # Get the baseline date (first timepoint for each participant)
    Baseline_Date = first(`Specimen_Collection Event_Date and Time`),
    
    # Calculate time from baseline in days
    Time_From_Baseline_Days = as.numeric(difftime(`Specimen_Collection Event_Date and Time`, 
                                                  Baseline_Date, 
                                                  units = "days")),
    
    # Calculate time difference from previous collection in days
    Interval_Days = as.numeric(difftime(`Specimen_Collection Event_Date and Time`, 
                                        lag(`Specimen_Collection Event_Date and Time`), 
                                        units = "days")),
    
    # First record for each participant will have NA for interval difference
    Interval_Days = ifelse(is.na(Interval_Days), 0, Interval_Days)
  ) %>%
  ungroup()


# Step 3: Final grouping and collapsing all columns
final_result <- result_with_diffs %>% 
  group_by(Participant_PPID) %>% 
  summarize(
    `Visit_Event Labels` = toString(`Visit_Event Label`),
    `Collection_Dates` = toString(`Specimen_Collection Event_Date and Time`),
    #`Baseline_Dates` = toString(Baseline_Date),
    `Time_From_Baseline_Days` = toString(Time_From_Baseline_Days),
    `Interval_Days` = toString(Interval_Days),
    .groups = 'drop'
  )



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
interval_stats <- result_with_diffs %>%
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
baseline_stats <- result_with_diffs %>%
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

# Print statistics
print("=== Interval Days Statistics ===")
print(interval_stats)
print("=== Time From Baseline Statistics ===")
print(baseline_stats)

# ===== 2. VISIT PATTERN ANALYSIS =====

# Add visit number for each participant
visit_patterns <- result_with_diffs %>%
  group_by(Participant_PPID) %>%
  arrange(Participant_PPID, Time_From_Baseline_Days) %>%
  mutate(
    Visit_Number = row_number(),
    Expected_Time = (Visit_Number - 1) * 365,
    Deviation_From_Expected = Time_From_Baseline_Days - Expected_Time
  ) %>%
  ungroup()

# Calculate statistics on visit adherence
visit_adherence <- visit_patterns %>%
  group_by(Participant_PPID) %>%
  summarize(
    Num_Visits = n(),
    Last_Visit_Time = max(Time_From_Baseline_Days),
    Expected_Last_Visit_Time = (n() - 1) * 365,
    Overall_Adherence = Last_Visit_Time / Expected_Last_Visit_Time,
    Avg_Absolute_Deviation = mean(abs(Deviation_From_Expected[Visit_Number > 1]), na.rm = TRUE),
    Max_Absolute_Deviation = max(abs(Deviation_From_Expected[Visit_Number > 1]), na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    Schedule_Status = case_when(
      abs(Overall_Adherence - 1) <= 0.1 ~ "On Schedule",
      Overall_Adherence < 0.9 ~ "Behind Schedule",
      Overall_Adherence > 1.1 ~ "Ahead of Schedule",
      TRUE ~ "Slightly Off Schedule"
    )
  )

# Summary of schedule adherence
adherence_summary <- visit_adherence %>%
  group_by(Schedule_Status) %>%
  summarize(
    Count = n(),
    Percent = Count / nrow(visit_adherence) * 100,
    .groups = 'drop'
  )

print("=== Schedule Adherence Summary ===")
print(adherence_summary)

# ===== 3. OUTLIER DETECTION =====

# Identify interval outliers
interval_outliers <- result_with_diffs %>%
  filter(Interval_Days > 0) %>%
  mutate(
    Deviation_From_Year = abs(Interval_Days - 365)
  ) %>%
  filter(Deviation_From_Year > 30) %>%
  select(Participant_PPID, `Visit_Event Label`, `Specimen_Collection Event_Date and Time`, 
         Interval_Days, Deviation_From_Year) %>%
  arrange(desc(Deviation_From_Year))

# Identify baseline time outliers
baseline_outliers <- visit_patterns %>%
  filter(Visit_Number > 1) %>%  # Skip first visit
  filter(abs(Deviation_From_Expected) > 30) %>%
  select(Participant_PPID, `Visit_Event Label`, Visit_Number, 
         Time_From_Baseline_Days, Expected_Time, Deviation_From_Expected) %>%
  arrange(desc(abs(Deviation_From_Expected)))

print("=== Interval Outliers (>30 days from expected 365-day interval) ===")
print(head(interval_outliers, 10))  # Show top 10 outliers
print(paste("Total interval outliers:", nrow(interval_outliers)))

print("=== Baseline Time Outliers (>30 days from expected pattern) ===")
print(head(baseline_outliers, 10))  # Show top 10 outliers
print(paste("Total baseline outliers:", nrow(baseline_outliers)))

# ===== 4. VISUALIZATION =====

# Set a common theme for all plots
my_theme <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# 1. Histogram of interval days
p1 <- ggplot(result_with_diffs %>% filter(Interval_Days > 0), 
             aes(x = Interval_Days)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = 365, color = "red", linetype = "dashed") +
  labs(title = "Distribution of Days Between Visits",
       x = "Interval (Days)",
       y = "Count") +
  annotate("text", x = 365, y = Inf, label = "1 Year", 
           vjust = 2, hjust = 1.1, color = "red") +
  my_theme

# 2. Histogram of time from baseline
p2 <- ggplot(result_with_diffs %>% filter(Time_From_Baseline_Days > 0), 
             aes(x = Time_From_Baseline_Days)) +
  geom_histogram(binwidth = 30, fill = "lightgreen", color = "black", alpha = 0.7) +
  geom_vline(xintercept = c(365, 730, 1095), 
             color = c("red", "blue", "purple"), 
             linetype = "dashed") +
  labs(title = "Distribution of Days From Baseline",
       x = "Time From Baseline (Days)",
       y = "Count") +
  annotate("text", x = 365, y = Inf, label = "1 Year", 
           vjust = 2, hjust = 1.1, color = "red") +
  annotate("text", x = 730, y = Inf, label = "2 Years", 
           vjust = 2, hjust = 1.1, color = "blue") +
  annotate("text", x = 1095, y = Inf, label = "3 Years", 
           vjust = 2, hjust = 1.1, color = "purple") +
  my_theme

# 3. Boxplot of interval days by visit event label
p3 <- ggplot(result_with_diffs %>% filter(Interval_Days > 0), 
             aes(x = reorder(`Visit_Event Label`, Interval_Days, FUN = median), 
                 y = Interval_Days)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  geom_hline(yintercept = 365, color = "red", linetype = "dashed") +
  labs(title = "Interval Days by Visit Type",
       x = "Visit Event Label",
       y = "Interval (Days)") +
  my_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Scatter plot of time from baseline by visit number
p4 <- ggplot(visit_patterns, 
             aes(x = Visit_Number, y = Time_From_Baseline_Days, color = Participant_PPID)) +
  geom_point(alpha = 0.7) +
  geom_line(aes(group = Participant_PPID), alpha = 0.4) +
  geom_abline(intercept = 0, slope = 365, linetype = "dashed", color = "red") +
  labs(title = "Time From Baseline by Visit Number",
       x = "Visit Number",
       y = "Time From Baseline (Days)") +
  my_theme +
  theme(legend.position = "none")  # Hide legend if many participants

# 5. Boxplot of interval days by visit number
p5 <- visit_patterns %>%
  filter(Interval_Days > 0) %>%
  ggplot(aes(x = factor(Visit_Number), y = Interval_Days)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  geom_hline(yintercept = 365, color = "red", linetype = "dashed") +
  labs(title = "Interval Days by Visit Number",
       x = "Visit Number",
       y = "Interval (Days)") +
  my_theme

# 6. Calendar heatmap - collection dates by month and year
month_year_data <- result_with_diffs %>%
  mutate(
    Month = month(`Specimen_Collection Event_Date and Time`),
    Year = year(`Specimen_Collection Event_Date and Time`)
  ) %>%
  count(Year, Month) %>%
  complete(Year, Month = 1:12, fill = list(n = 0))

p6 <- ggplot(month_year_data, aes(x = factor(Month), y = factor(Year), fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  scale_x_discrete(labels = month.abb) +
  labs(title = "Collection Dates Heatmap",
       x = "Month",
       y = "Year",
       fill = "Count") +
  my_theme

# 7. Deviation from expected pattern
p7 <- ggplot(visit_patterns %>% filter(Visit_Number > 1), 
             aes(x = Visit_Number, y = Deviation_From_Expected)) +
  geom_boxplot(aes(group = Visit_Number), fill = "salmon", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Deviation From Expected Visit Pattern",
       x = "Visit Number",
       y = "Deviation (Days)") +
  my_theme

# 8. Schedule adherence pie chart
p8 <- ggplot(adherence_summary, aes(x = "", y = Percent, fill = Schedule_Status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Visit Schedule Adherence", 
       fill = "Status") +
  theme_void() +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Arrange plots in a grid and save
grid_arrangement <- grid.arrange(
  p1, p2, p3, p4, 
  p5, p6, p7, p8,
  ncol = 2
)

ggsave("plots/all_visit_analysis_plots.png", grid_arrangement, width = 14, height = 16)

# Save all plots
# Save individual plots
ggsave("plots/interval_distribution.png", p1, width = 8, height = 6)
ggsave("plots/baseline_distribution.png", p2, width = 8, height = 6)
ggsave("plots/interval_by_visit_type.png", p3, width = 10, height = 6)
ggsave("plots/baseline_by_visit_number.png", p4, width = 8, height = 6)
ggsave("plots/interval_by_visit_number.png", p5, width = 8, height = 6)
ggsave("plots/collection_dates_heatmap.png", p6, width = 8, height = 6)
ggsave("plots/deviation_from_expected.png", p7, width = 8, height = 6)
ggsave("plots/schedule_adherence_pie.png", p8, width = 8, height = 6)


# ===== 5. EXPORT RESULTS =====

# Save statistics
write.csv(interval_stats, "out/interval_statistics.csv", row.names = FALSE)
write.csv(baseline_stats, "out/baseline_statistics.csv", row.names = FALSE)
write.csv(visit_adherence, "out/visit_adherence_by_participant.csv", row.names = FALSE)
write.csv(adherence_summary, "out/adherence_summary.csv", row.names = FALSE)

# Save outliers
write.csv(interval_outliers, "out/interval_outliers.csv", row.names = FALSE)
write.csv(baseline_outliers, "out/baseline_outliers.csv", row.names = FALSE)

# Create the final collapsed result
ffinal_result <- result_with_diffs %>% 
  group_by(Participant_PPID) %>% 
  summarize(
    `Visit_Event Labels` = toString(`Visit_Event Label`),
    `Collection_Dates` = toString(`Specimen_Collection Event_Date and Time`),
    `Baseline_Date` = first(Baseline_Date),
    `Time_From_Baseline_Days` = toString(Time_From_Baseline_Days),
    `Interval_Days` = toString(Interval_Days),
    `Avg_Interval` = mean(Interval_Days[Interval_Days > 0], na.rm = TRUE),
    `Max_Interval_Deviation` = max(abs(Interval_Days[Interval_Days > 0] - 365), na.rm = TRUE),
    `Last_Visit_Time` = max(Time_From_Baseline_Days),
    `Expected_Last_Visit_Time` = (n() - 1) * 365,
    `Overall_Adherence` = if_else(n() > 1, 
                                  max(Time_From_Baseline_Days) / ((n() - 1) * 365), 
                                  NA_real_),
    `Total_Visits` = n(),
    .groups = 'drop'
  ) %>%
  # Join with the schedule status
  left_join(select(visit_adherence, Participant_PPID, Schedule_Status), by = "Participant_PPID")

# Save final result
write.csv(final_result, "out/participant_visit_summary.csv", row.names = FALSE)

# Print completion message
print("Analysis complete. Results and visualizations have been saved.")