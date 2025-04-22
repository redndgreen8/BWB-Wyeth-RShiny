# Load necessary libraries
source("str_list.R")

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(ggfittext)
library(treemapify)
# Read the dataset (replace 'your_file.csv' with the actual filename)
dfY <- read_csv(enroll_str1, col_types = cols())
#dfY[dfY == ""] <-NA
names(dfY) <- gsub("[[:punct:] ]", "", names(dfY))

date_columns <- c(9,10,11,12,13,23,24,25,28)

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
    Clinic = ifelse(is.na(Clinic),"Web",Clinic),
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
    RecruitLocation = ifelse(is.na(Clinic), "Web",
                             ifelse(startsWith(Clinic, "Web;"), Clinic,
                                    ifelse(grepl(";", Clinic), 
                                           trimws(strsplit(as.character(Clinic), ";")[[1]][1]), 
                                           Clinic))),
    Race = ifelse( grepl(",", WhatisyourracePleasemarkallthatapply) , "Multiple",WhatisyourracePleasemarkallthatapply),
    Edu = Whatisthehighestlevelofeducationyouhavereceived#,
    #Status = `Complete?`,
    #across(15, ~ ifelse(grepl(",", .), "Multiple", .))  # Applies transformation to column index 
  ) %>%
  ungroup()


dfungrouped <- df_grouped %>%
  separate_rows("EventName", sep = ";")# %>%
  # Optional: Trim whitespace if needed
 # mutate(column_name = trimws("Event Name"))


df_groupedNA <- df_grouped[is.na(df_grouped$ConsentYear) ,]

write_csv(df_groupedNA, "out/missing_consent_info.csv")
# Print or save the output
view(df_grouped)
write_csv(df_grouped, "out/yearly_retention_Grouped_Summary.csv")  # Save output as CSV




plot_stacked_bars <- function(data, primary_col, secondary_col, value_col, visual_col) {
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
      theme_minimal() #+
     # theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # Get all primary groups
  primary_groups <- unique(data[[primary_col]])
  
  # Create a plot for each primary group
  plots <- lapply(primary_groups, create_plot)
  
  return(plots)
}




(plots_stackR <- plot_stacked_bars(
  data = dfungrouped,
  primary_col = "ConsentYear",
  secondary_col = "EventName", 
  value_col = "Race",
  visual_col = "Race"
))

(plots_stackE <- plot_stacked_bars(
  data = dfungrouped,
  primary_col = "ConsentYear",
  secondary_col = "EventName", 
  value_col = "Edu",
  visual_col = "Edu"
))

(plots_stackL <- plot_stacked_bars(
  data = dfungrouped,
  primary_col = "ConsentYear",
  secondary_col = "EventName", 
  value_col = "RecruitLocation",
  visual_col = "RecruitLocation"
))
(plots_stackN <- plot_stacked_bars(
  data = dfungrouped,
  primary_col = "ConsentYear",
  secondary_col = "EventName", 
  value_col = "Complete",
  visual_col = "Complete"
))
(plots_stackS <- plot_stacked_bars(
  data = df_grouped,
  primary_col = "ConsentYear",
  secondary_col = "EventName", 
  value_col = "SurveyStatus",
  visual_col = "SurveyStatus"
))

(plots_stackB <- plot_stacked_bars(
  data = dfungrouped,
  primary_col = "ConsentYear",
  secondary_col = "EventName", 
  value_col = "BloodDrawStatus",
  visual_col = "BloodDrawStatus"
))


pdf("plots/plot_stacks2.pdf", width = 10, height = 6)  # Open PDF file
plots_stackL
plots_stackR
plots_stackE
dev.off()




pdf("plots/plot_stacksE.pdf", width = 10, height = 6)  # Open PDF file
plots_stackS
#plots_stackB
plots_stackN
dev.off()

  
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
    theme_minimal() 
}
plots_heat <- plot_heatmap(
  data = dfungrouped,
  primary_col = "ConsentYear",
  secondary_col = "EventName", 
  value_col = "RecruitLocation"
)
plots_heat
plots_heat_race <- plot_heatmap(
  data = dfungrouped,
  primary_col = "ConsentYear",
  secondary_col = "EventName", 
  value_col = "Race"
)
plots_heat_race
plots_heat_edu <- plot_heatmap(
  data = dfungrouped,
  primary_col = "ConsentYear",
  secondary_col = "EventName", 
  value_col = "Edu"
)
plots_heat_edu




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
plots_tree <- plot_treemap(
  data = dfungrouped,
  primary_col = "ConsentYear",
  secondary_col = "EventName", 
  value_col = "RecruitLocation"
)
plots_tree

plots_tree_race <- plot_treemap(
  data = dfungrouped,
  primary_col = "ConsentYear",
  secondary_col = "EventName", 
  value_col = "Race"
)
plots_tree_race

plots_tree_edu <- plot_treemap(
  data = dfungrouped,
  primary_col = "ConsentYear",
  secondary_col = "EventName", 
  value_col = "Edu"
)
plots_tree_edu

