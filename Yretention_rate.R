# Load necessary libraries
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(ggrepel)

# Read the dataset (replace 'your_file.csv' with the actual filename)
dfY <- read_csv(enroll_str1, col_types = cols())

date_columns <- c(9,10,11,12,13,23,24,25)

df_grouped <- dfY %>%
  mutate(across(all_of(date_columns), ~as.Date(.))) %>%
  # Create a function to find earliest non-NA date
  rowwise() %>%
  mutate(across(all_of(date_columns), ~as.Date(.))) %>%
  # Create earliest consent date with proper NA handling
  rowwise() %>%
  mutate(
    # Check if any date column is not NA before calculating min
    HasAnyDate = any(!is.na(c_across(all_of(date_columns)))),
    EarliestConsentDate = if(HasAnyDate) {
      min(c_across(all_of(date_columns)), na.rm = TRUE)
    } else {
      as.Date(NA)
    },
    EarliestConsentYear = if(!is.na(EarliestConsentDate)) {
      format(EarliestConsentDate, "%Y")
    } else {
      NA_character_
    }
  ) %>%
  ungroup() %>%
  # Optional: Remove the helper column
  select(-HasAnyDate) %>%
  rename(StudyID = colnames(dfY)[3], StudyID2 = colnames(dfY)[4]) %>%  # Rename based on names, not positions
  group_by(across(1)) %>%  # Group by first column
  summarise(
    Count = n(),  # Count occurrences in each group
    across(everything(), ~ {
      if (inherits(., "Date")) {
        str_c(na.omit(format(., "%Y-%m-%d")), collapse = ";")  # Format dates & remove NA
      } else {
        str_c(na.omit(unique(.)), collapse = ";")  # Collapse unique non-date values
      }
    })
  ) %>%
  mutate(
    ConsentYear = str_extract(EarliestConsentYear, "^[0-9]{4}"),  # Extracts first 4-digit year
    across(15, ~ ifelse(grepl(",", .), "Multiple", .))  # Applies transformation to column index 3
  ) %>%
  ungroup()


df_groupedNA <- df_grouped[is.na(df_grouped$ConsentYear) ,]

write_csv(df_groupedNA, "missing_consent_info.csv")
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

plot_pie_charts_by_COL <- function(data, COL, column_name, title) {
  # First, group the data by Count column
  data[is.na(data)] <- "Missing"
  
  
  count_groups <- unique(data[[COL]])
  # Make sure we have at most 3 count groups
  #count_groups <- count_groups[1:min(4, length(count_groups))]
  
  # Create a list to store the plots
  plot_list <- list()
  
  # Create a pie chart for each Count group
  for (i in seq_along(count_groups)) {
    count_value <- count_groups[i]
    
    # Filter data for this Count value
    subset_data <- data %>% filter(.data[[COL]] == count_value)
    
    # Calculate total count for this subset
    total_count <- nrow(subset_data)
    
    # Create the pie chart
    plot <- subset_data %>%
      count(!!sym(column_name)) %>%
      mutate(Percentage = n / sum(n) * 100) %>%
      ggplot(aes(x = "", y = n, fill = !!sym(column_name))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_label_repel(aes(label = paste0(!!sym(column_name), "\n", 
                                          n, " (", round(Percentage, 1), "%)")),
                       position = position_stack(vjust = 0.5), size = 3, show.legend = FALSE) +
      labs(title = paste0(title," ", COL ," ", count_value, "\nTotal: ", total_count), 
           fill = column_name) +
      theme_void()
    
    plot_list[[i]] <- plot
  }
  
  # Return the list of plots to be printed separately
  return(plot_list)
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
#pie_survey <- plot_pie_chart(df_grouped, "Survey Status", "Survey Status Distribution")
#pie_survey
pie_survey_count <- plot_pie_charts_by_COL(df_grouped, "Count","Survey Status", "Survey Status Distribution")
pie_survey_count

(pie_survey_year <- plot_pie_charts_by_COL(df_grouped, "ConsentYear","Survey Status", "Survey Status Distribution"))

(pie_blood_year <- plot_pie_charts_by_COL(df_grouped, "ConsentYear",colnames(df_grouped)[9], "Blood Draw Status Distribution"))


pie_blood <- plot_pie_charts_by_count(df_grouped, colnames(df_grouped)[9], "Blood Draw Status Distribution")
pie_blood
hist_survey <- plot_histogram(df_grouped, "Survey Status", "Survey Status Histogram")
hist_survey

pie_consent <- plot_pie_charts_by_count(df_grouped, "Consent Signed", "Consent Status Distribution")
pie_clinic <- plot_pie_charts_by_count(df_grouped, "Clinic", "Consent Status Distribution")


hist_blood <- plot_histogram(df_grouped, colnames(df_grouped)[9], "Blood Draw Status Histogram")

pdf("plots/retention_survey_blood_plots.pdf", width = 10, height = 6)  # Open PDF file

(pie_survey_year <- plot_pie_charts_by_COL(df_grouped, "ConsentYear","Survey Status", "Survey Status Distribution"))

(pie_blood_year <- plot_pie_charts_by_COL(df_grouped, "ConsentYear",colnames(df_grouped)[9], "Blood Draw Status Distribution"))


(pie_race_year <- plot_pie_charts_by_COL(df_grouped, "ConsentYear",colnames(df_grouped)[15], "Race Distribution"))

(pie_edu_year <- plot_pie_charts_by_COL(df_grouped, "ConsentYear",colnames(df_grouped)[16], "Edu Distribution"))

(pie_clinic_year <- plot_pie_charts_by_COL(df_grouped, "ConsentYear",colnames(df_grouped)[6], "Clinic Distribution"))




dev.off()
