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












plot_pie_charts_by_double_group <- function(data, primary_col, secondary_col, value_col, main_title = "Distribution by Group") {
  # Replace NAs with "Missing"
  data[is.na(data)] <- "Missing"
  # Define all_levels here
  all_levels <- unique(data[[value_col]])
  # Get unique values for the primary grouping column
  primary_groups <- unique(data[[primary_col]])
  
  # Create a nested list to store plots by primary and secondary grouping
  all_plots <- list()
  
  # For each primary group
  for (p_idx in seq_along(primary_groups)) {
    primary_value <- primary_groups[p_idx]
    
    # Filter data for this primary group
    primary_subset <- data %>% filter(.data[[primary_col]] == primary_value)
    
    # Get unique values for the secondary grouping within this primary group
    secondary_groups <- sort(unique(primary_subset[[secondary_col]]))
    secondary_plots <- list()
    
    # For each secondary group
    for (s_idx in seq_along(secondary_groups)) {
      secondary_value <- secondary_groups[s_idx]
      
      # Filter data for this secondary group
      subset_data <- primary_subset %>% filter(.data[[secondary_col]] == secondary_value)
      
      # Calculate total count for this subset
      total_count <- nrow(subset_data)
      
      # Only create plot if there's data
      if (total_count > 0) {
        # Create the pie chart
        plot <- subset_data %>%
          count(!!sym(value_col)) %>%
          mutate(Percentage = n / sum(n) * 100) %>%
          ggplot(aes(x = "", y = n, fill = !!sym(value_col))) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar("y", start = 0) +
          geom_label_repel(aes(label = paste0(!!sym(value_col), "\n", 
                                              n, " (", round(Percentage, 1), "%)")),
                           position = position_stack(vjust = 0.5), size = 3, show.legend = FALSE) +
          labs(title = paste0(primary_col, ": ", primary_value, "\n", 
                              secondary_col, ": ", secondary_value, 
                              "\nTotal: ", total_count), 
               fill = value_col) +
          theme_void() +
          theme(legend.position = "none") +
          # Use scale_fill_discrete with all possible levels to ensure consistent colors
          scale_fill_discrete(drop = FALSE, limits = all_levels)
        secondary_plots[[s_idx]] <- plot
      }
    }
    
    all_plots[[p_idx]] <- secondary_plots
  }
  
  # Flatten the list to return all plots
  flat_plots <- unlist(all_plots, recursive = FALSE)
  
  # Return the list of plots to be arranged or printed separately
  return(flat_plots)
}



# Example usage
plots <- plot_pie_charts_by_double_group(
  data = df_grouped,
  primary_col = "Event Name",
  secondary_col = "ConsentYear", 
  value_col = "Clinic"
)

# To display all plots in a grid
library(gridExtra)
total_plots <- length(plots)

everyN <- 9
num_pages <- ceiling(total_plots / everyN)

# For each page, display 2 plots
for (i in 1:num_pages) {
  start_idx <- (i-1) * everyN + 1
  end_idx <- min(i * everyN, total_plots)
  
  # Create a subset of plots for this page
  page_plots <- plots[start_idx:end_idx]
  
  # Display this page
  do.call(grid.arrange, c(page_plots, ncol = 3))
  
  # Optional: add a pause between pages if viewing interactively
  if (i < num_pages) {
    readline(prompt = "Press Enter for next page...")
  }
}

(pie_clinic_event <- plot_pie_charts_by_COL(df_grouped, "Event Name","Clinic", "Clinic per year Distribution"))

pdf("plots/retention_survey_blood_plots.pdf", width = 10, height = 6)  # Open PDF file

(pie_survey_year <- plot_pie_charts_by_COL(df_grouped, "ConsentYear","Survey Status", "Survey Status Distribution"))

(pie_blood_year <- plot_pie_charts_by_COL(df_grouped, "ConsentYear",colnames(df_grouped)[9], "Blood Draw Status Distribution"))


(pie_race_year <- plot_pie_charts_by_COL(df_grouped, "ConsentYear",colnames(df_grouped)[15], "Race Distribution"))

(pie_edu_year <- plot_pie_charts_by_COL(df_grouped, "ConsentYear",colnames(df_grouped)[16], "Edu Distribution"))

(pie_clinic_year <- plot_pie_charts_by_COL(df_grouped, "ConsentYear",colnames(df_grouped)[6], "Clinic Distribution"))




dev.off()



plots_stack <- plot_stacked_bars(
  data = df_grouped,
  primary_col = "ConsentYear",
  secondary_col = "Event Name", 
  value_col = "Clinic"
)

pdf("plots/plot_stacks.pdf", width = 10, height = 6)  # Open PDF file

plots_stack
dev.off()





# Display in pages of 4 plots
total_plots <- length(plots_stack)
num_pages <- ceiling(total_plots / everyN)
everyN <- 1
# For each page, display 2 plots
for (i in 1:num_pages) {
  start_idx <- (i-1) * everyN + 1
  end_idx <- min(i * everyN, total_plots)
  
  # Create a subset of plots for this page
  page_plots <- plots_stack[start_idx:end_idx]
  
  # Display this page
  do.call(grid.arrange, c(page_plots, ncol = 1))
  
  # Optional: add a pause between pages if viewing interactively
  if (i < num_pages) {
    readline(prompt = "Press Enter for next page...")
  }
}

plot_stacked_bars <- function(data, primary_col, secondary_col, value_col) {
  # Replace NAs with "Missing"
  data[is.na(data)] <- "Missing"
  
  # Count observations for each combination
  plot_data <- data %>%
    count(!!sym(primary_col), !!sym(secondary_col), !!sym(value_col)) %>%
    group_by(!!sym(primary_col), !!sym(secondary_col)) %>%
    mutate(percentage = n / sum(n) * 100)
  
  # Create a function to generate one plot per primary group
  create_plot <- function(group_value) {
    filtered_data <- plot_data %>% 
      filter(!!sym(primary_col) == group_value)
    
    ggplot(filtered_data, 
           aes(x = !!sym(secondary_col), y = n, fill = !!sym(value_col))) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(n, "\ (",round(percentage, 1), ")%")), 
                position = position_stack(vjust = 0.5), size = 3) +
      labs(title = paste(primary_col, ":", group_value),
           x = secondary_col,
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # Get all primary groups
  primary_groups <- unique(data[[primary_col]])
  
  # Create a plot for each primary group
  plots <- lapply(primary_groups, create_plot)
  
  return(plots)
}








plots_heat <- plot_heatmap(
  data = df_grouped,
  primary_col = "ConsentYear",
  secondary_col = "Event Name", 
  value_col = "Clinic"
)
plots_heat

plot_heatmap <- function(data, primary_col, secondary_col, value_col) {
  # Replace NAs with "Missing"
  data[is.na(data)] <- "Missing"
  
  # Count and calculate percentages
  heatmap_data <- data %>%
    count(!!sym(primary_col), !!sym(secondary_col), !!sym(value_col)) %>%
    group_by(!!sym(primary_col), !!sym(secondary_col)) %>%
    mutate(percentage = n / sum(n) * 100)
  
  # Create heatmap
  ggplot(heatmap_data, 
         aes(x = !!sym(secondary_col), y = !!sym(value_col), fill = percentage)) +
    geom_tile() +
    geom_text(aes(label = paste0(round(percentage, 1), "%")), 
              size = 3) +
    facet_wrap(vars(!!sym(primary_col)), scales = "free") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = "Distribution Heatmap",
         fill = "Percentage") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}




plot_treemap <- function(data, primary_col, secondary_col, value_col) {
  # Make sure required packages are loaded
  if (!requireNamespace("treemapify", quietly = TRUE) ||
      !requireNamespace("ggfittext", quietly = TRUE)) {
    stop("Packages 'treemapify' and 'ggfittext' are needed for this function")
  }
  
  # Replace NAs with "Missing"
  data[is.na(data)] <- "Missing"
  
  # Function to create treemap for one primary group
  create_treemap <- function(group_value) {
    filtered_data <- data %>% 
      filter(!!sym(primary_col) == group_value) %>%
      count(!!sym(secondary_col), !!sym(value_col)) %>%
      arrange(desc(n))
    
    ggplot(filtered_data, 
           aes(area = n, fill = !!sym(value_col), 
               subgroup = !!sym(secondary_col),
               label = paste0(!!sym(value_col), "\n", n))) +
      treemapify::geom_treemap() +
      treemapify::geom_treemap_subgroup_border() +
      ggfittext::geom_treemap_text(colour = "white", place = "centre",
                                   grow = TRUE, min.size = 8) +
      labs(title = paste(primary_col, ":", group_value)) +
      theme(legend.position = "none")
  }
  
  # Get all primary groups
  primary_groups <- sort(unique(data[[primary_col]]))
  
  # Create a treemap for each primary group
  plots <- lapply(primary_groups, create_treemap)
  
  return(plots)
}



