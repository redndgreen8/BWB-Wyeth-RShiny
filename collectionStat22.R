
source("collectionTimeStats.R")
library(ggplot2)
library(gridExtra) # For arranging multiple plots
library(scales)    # For better axis formatting


# Print statistics
print("=== Interval Days Statistics ===")
print(interval_stats)
print("=== Time From Baseline Statistics ===")
print(baseline_stats)

# ===== 2. VISIT PATTERN ANALYSIS =====

# Add visit number for each participant
visit_patterns <- DF %>%
  group_by(Participant_PPID) %>%
  arrange(Participant_PPID, Time_From_Baseline_Days, Time_From_Consent_Days) %>%
  mutate(
    Visit_Number = row_number(),
    Expected_Time = (Visit_Number - 1) * 365,
    Deviation_From_ExpectedB = Time_From_Baseline_Days - Expected_Time,
    Deviation_From_ExpectedC = Time_From_Consent_Days - Expected_Time,
  ) %>%
  ungroup()


Due_dates <-  DF %>%
  group_by(Participant_PPID) %>%
  #arrange(Participant_PPID, Time_From_Baseline_Days, Time_From_Consent_Days) %>%
  mutate(
    Visit_Number = row_number(),
    #Expected_Time = (Visit_Number + 1) * 330,
    Expected_Time = (Visit_Number -1) * 330,
    DueDate = as.Date(EarliestConsentDate) + days(Expected_Time),
    
    # For date differences, as.numeric() ensures proper numeric results
    DaysRemaining = as.numeric( DueDate - Sys.Date())
    
    
  ) %>%
  ungroup()


Due_datesCS <- Due_dates %>%
  group_by(Participant_PPID) %>%
  summarize(
    `Visit_Event` = toString(Visit_TypeCOMB),
    `Primary_Visit` = toString(Primary_Visit),
    `EarliestConsentDate` = toString(EarliestConsentDate),
    `Baseline_Date` = toString(Baseline_Date),
    `DueDate` = toString(DueDate),
    `DaysRemaining` = toString(DaysRemaining),
  )

Due_datesC <- Due_dates %>%
  group_by(Participant_PPID) %>%
  summarize(
    `Visit_Event` = first(Visit_TypeCOMB),
    `Primary_Visit` = first(Primary_Visit),
    `EarliestConsentDate` = first(EarliestConsentDate),
    `Baseline_Date` = first(Baseline_Date),
    `DueDate` = last(DueDate),
    `DaysRemaining` = last(DaysRemaining),
  )



write.csv(Due_datesC, "out/Due_dates.csv")


Due_datesC <- Due_datesC %>%
  mutate(DaysRemaining = as.numeric(DaysRemaining))

p <- ggplot(Due_datesC, aes(x = DaysRemaining, fill = Primary_Visit)) +
  geom_histogram(binwidth = 30, position = "dodge", alpha = 0.8) +
  labs(
    title = "Days Remaining Until Next Visit",
    x = "Days Remaining",
    y = "Count",
    fill = "Visit Type"
  ) +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  annotate("text", x = 5, y = 1, label = "Today", hjust = 0, color = "red")

p
# For "Other" visits, we can add text annotations after identifying their positions
# We'll need to do this in a separate step

# First, run the plot to see what it looks like
print(p)

# Create a faceted histogram instead
pp_facet <- ggplot(Due_datesC, aes(x = as.numeric(DaysRemaining))) +
  geom_histogram(binwidth = 30, fill = "steelblue", alpha = 0.8) +
  facet_wrap(~ Primary_Visit, scales = "free_y") +
  labs(
    title = "Days Remaining Until Next Visit",
    x = "Days Remaining",
    y = "Count"
  ) +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  # Add more breaks on the x-axis for clearer reading
  scale_x_continuous(
    breaks = seq(-180, 360, by = 30),  # Adjust this range based on your data
    labels = seq(-180, 360, by = 30)
  ) +
  # Improve x-axis label visibility
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(size = 10, face = "bold")
  )
print(pp_facet)



grid_arrangementDue <- grid.arrange(
  pp_facet,p)

grid_arrangementDue

ggsave("plots/DueDay_analysis_plots.png", grid_arrangementDue, width = 20, height = 16)


# Calculate statistics on visit adherence
visit_adherence <- visit_patterns %>%
  group_by(Participant_PPID) %>%
  summarize(
    Num_Visits = n(),
    Last_Visit_Time = max(Time_From_Baseline_Days),
    Expected_Last_Visit_Time = (n() - 1) * 365,
    Overall_Adherence = Last_Visit_Time / Expected_Last_Visit_Time,
    Avg_Absolute_Deviation = mean(abs(Deviation_From_ExpectedB[Visit_Number > 1]), na.rm = TRUE),
    Max_Absolute_Deviation = max(abs(Deviation_From_ExpectedB[Visit_Number > 1]), na.rm = TRUE),
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
  filter(abs(Deviation_From_ExpectedB) > 30) %>%
  select(Participant_PPID, `Visit_Event Label`, Visit_Number, 
         Time_From_Baseline_Days, ,  Expected_Time, Deviation_From_ExpectedB) %>%
  arrange(desc(abs(Deviation_From_ExpectedB)))

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

# Original ungrouped plot
p1 <- ggplot(DF %>% filter(Primary_Visit_Type == "Baseline") %>%
               filter(Interval_Days > 0), 
             aes(x = Interval_Days)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = 90, color = "red", linetype = "dashed") +
  labs(title = "Distribution of Days Between Consent and First Visit",
       x = "Interval (Days)",
       y = "Count") +
  annotate("text", x = 90, y = Inf, label = "3 months", 
           vjust = 2, hjust = 1.1, color = "red") +
  my_theme

p1
# Grouped by Primary_Visit_Type - faceted version
p1_facet <- ggplot(merged_full %>% filter(Interval_Days > 0), 
                   aes(x = Interval_Days)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = 365, color = "red", linetype = "dashed") +
  facet_wrap(~Primary_Visit_Type, scales = "free_y") +
  labs(title = "Distribution of Days Between Visits by Visit Type",
       x = "Interval (Days)",
       y = "Count") +
  annotate("text", x = 365, y = Inf, label = "1 Year", 
           vjust = 2, hjust = 1.1, color = "red") +
  my_theme
p1_facet


# 2. Histogram of time from baseline
p2 <- ggplot(merged_full %>% filter(Time_From_Baseline_Days > 0), 
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
p2


p2_facet <- ggplot(merged_full %>% filter(Time_From_Baseline_Days > 0), 
                   aes(x = Time_From_Baseline_Days)) +
  geom_histogram(binwidth = 30, fill = "lightgreen", color = "black", alpha = 0.7) +
  # Define the lines separately to avoid color mapping issues
  geom_vline(xintercept = 365, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 730, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = 1095, color = "purple", linetype = "dashed") +
  facet_wrap(~Primary_Visit_Type, scales = "free_y") +
  labs(title = "Distribution of Days From Baseline by Visit Type",
       x = "Time From Baseline (Days)",
       y = "Count") +
  annotate("text", x = 365, y = Inf, label = "1 Year", 
           vjust = 2, hjust = 1.1, color = "red") +
  annotate("text", x = 730, y = Inf, label = "2 Years", 
           vjust = 2, hjust = 1.1, color = "blue") +
  annotate("text", x = 1095, y = Inf, label = "3 Years", 
           vjust = 2, hjust = 1.1, color = "purple") +
  my_theme
p2_facet

p2_color <- ggplot(merged_full %>% filter(Time_From_Baseline_Days > 0), 
                   aes(x = Time_From_Baseline_Days, fill = Primary_Visit_Type)) +
  geom_histogram(binwidth = 30, position = "dodge", alpha = 0.7, color = "black") +
  geom_vline(xintercept = c(365, 730, 1095), 
             color = c("red", "blue", "purple"), 
             linetype = "dashed") +
  labs(title = "Distribution of Days From Baseline by Visit Type",
       x = "Time From Baseline (Days)",
       y = "Count",
       fill = "Visit Type") +
  annotate("text", x = 365, y = Inf, label = "1 Year", 
           vjust = 2, hjust = 1.1, color = "red") +
  annotate("text", x = 730, y = Inf, label = "2 Years", 
           vjust = 2, hjust = 1.1, color = "blue") +
  annotate("text", x = 1095, y = Inf, label = "3 Years", 
           vjust = 2, hjust = 1.1, color = "purple") +
  scale_fill_brewer(palette = "Set2") +
  my_theme
p2_color
# 3. Boxplot of interval days by visit event label
p3 <- ggplot(DF %>% filter(Interval_Days > 0), 
             aes(x = reorder(`Primary_Visit_Type`, Interval_Days, FUN = median), 
                 y = Interval_Days)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  geom_hline(yintercept = 365, color = "red", linetype = "dashed") +
  labs(title = "Interval Days by Visit Type",
       x = "Visit Event Label",
       y = "Interval (Days)") +
  my_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p3


grid_arrangement3 <- grid.arrange(
  p1, p1_facet, p2_color, p3)

grid_arrangement3

ggsave("plots/day_analysis_plots.png", grid_arrangement3, width = 20, height = 16)


p33 <- ggplot(merged_full %>% 
                filter(Interval_Days > 0), 
              aes(x = Primary_Visit_Type, y = Interval_Days)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  geom_hline(yintercept = 365, color = "red", linetype = "dashed") +
  labs(title = "Interval Days by Primary Visit Type",
       x = "Primary Visit Type",
       y = "Interval (Days)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
p33
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
             aes(x = Visit_Number, y = Deviation_From_ExpectedB)) +
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
  p1, p2_color, p3, p4, 
  p5, p6, p7, p8,
  ncol = 2
)

ggsave("plots/all_visit_analysis_plots.png", grid_arrangement, width = 14, height = 16)

# Save all plots
# Save individual plots
ggsave("plots/interval_distribution.png", p1, width = 8, height = 6)
ggsave("plots/baseline_distribution.png", p2_color, width = 8, height = 6)
ggsave("plots/interval_by_visit_type.png", p3, width = 10, height = 6)
ggsave("plots/interval_by_visit.png", p33, width = 10, height = 6)

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
