# Load necessary libraries
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

# Read the dataset (replace 'your_file.csv' with the actual filename)
#df <- read_csv("your_file.csv", col_types = cols())

# Group by the first column and collapse all other columns
df_grouped <- df %>%
  rename(StudyID = 3, StudyID2 = 4)  %>% # Replace with desired names
  group_by(across(1)) %>%  # Group by the first column (assumed to be Record ID or similar)
  summarise(
    Count = n(),  # Count occurrences in each group
    across(everything(), ~ {
    if (inherits(., "Date")) {
      str_c(ifelse(is.na(.), "-", format(., "%Y-%m-%d")), collapse = ",")  # Preserve date format
    } else {
      str_c(ifelse(is.na(.), "-", unique(.)), collapse = ",")  # Collapse other columns & replace NA
    }
  }))

# Print or save the output
print(df_grouped)
write_csv(df_grouped, "yearly_retention_Grouped_Summary.csv")  # Save output as CSV

plot_pie_chart <- function(data, column_name, title) {
  data %>%
    count(!!sym(column_name)) %>%  # Count occurrences
    mutate(Percentage = n / sum(n) * 100) %>%  # Calculate percentage
    ggplot(aes(x = "", y = n, fill = !!sym(column_name))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
              position = position_stack(vjust = 0.5), size = 5) +
    labs(title = title, fill = column_name) +
    theme_void()  # Remove background grid
}

# Function to create a histogram
plot_histogram <- function(data, column_name, title) {
  ggplot(data, aes(x = !!sym(column_name))) +
    geom_bar(fill = "steelblue", color = "black") +
    labs(title = title, x = column_name, y = "Count") +
    theme_minimal()
}

# Create plots
pie_survey <- plot_pie_chart(df_grouped, "Survey Status", "Survey Status Distribution")
pie_survey
pie_blood <- plot_pie_chart(df_grouped, 9, "Blood Draw Status Distribution")

hist_survey <- plot_histogram(df_grouped, "Survey Status", "Survey Status Histogram")
hist_survey

hist_blood <- plot_histogram(df_grouped, 9, "Blood Draw Status Histogram")

# Print plots
print(pie_survey)
print(pie_blood)
print(hist_survey)
print(hist_blood)