# Load necessary libraries
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(ggrepel)

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
    count(!!sym(column_name)) %>%
    mutate(Percentage = n / sum(n) * 100) %>%
    ggplot(aes(x = "", y = n, fill = !!sym(column_name))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_label_repel(aes(label = paste0(!!sym(column_name), "\n", round(Percentage, 1), "%")),
                     position = position_stack(vjust = 0.5), size = 3, show.legend = FALSE) +
    labs(title = title, fill = column_name) +
    theme_void()
}


# Function to create a histogram
plot_histogram <- function(data, column_name, title) {
  data %>%
    count(!!sym(column_name)) %>%
    mutate(Percentage = (n / sum(n)) * 100) %>%
    ggplot(aes(x = reorder(!!sym(column_name), n), y = n)) +  # Reorder bars by count
    geom_bar(stat = "identity", fill = "steelblue", color = "black") +
    geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
              hjust = -0.1, size = 5) +  # Add count & percentage labels
    labs(title = title, x = column_name, y = "Count") +
    theme_minimal() +
    coord_flip()  # Flips the bars horizontally
}

# Create plots
pie_survey <- plot_pie_chart(df_grouped, "Survey Status", "Survey Status Distribution")
pie_survey
pie_blood <- plot_pie_chart(df_grouped, colnames(df_grouped)[9], "Blood Draw Status Distribution")
pie_blood
hist_survey <- plot_histogram(df_grouped, "Survey Status", "Survey Status Histogram")
hist_survey

hist_blood <- plot_histogram(df_grouped, colnames(df_grouped)[9], "Blood Draw Status Histogram")

pdf("plots/survey_blood_plots.pdf", width = 10, height = 6)  # Open PDF file

print(pie_survey)   # Survey Status Pie Chart
print(pie_blood)    # Blood Draw Status Pie Chart
print(hist_survey)  # Survey Status Histogram
print(hist_blood)   # Blood Draw Status Histogram

dev.off()
