library(ggplot2)
library(tidyverse)
source("theme_DB.R")
library(dplyr)
library(tidyr)

getDemogInfo_numerical <- function(demog_str) {
  df <- read.csv(demog_str)
  names(df)[1] <- "ID"
  
  # Adjusting column names to match the expected format
  race_columns <- c("American Indian or Alaskan Native", "Asian", "Black or African American", 
                    "Hispanic or Latino", "Native Hawaiian or other Pacific Islander", "White", 
                    "I prefer Not to Answer")
  
  # Rename df2 race columns to match the descriptive names
  names(df)[2:8] <- race_columns
  
  # Convert 0s and 1s to logical
  df[2:8] <- lapply(df[2:8], function(x) x == 1)
  
  # Transform the data from wide to long format
  df.race <- df %>%
    pivot_longer(cols = 2:8, names_to = "Race", values_to = "val") %>%
    group_by(ID) %>%
    mutate(Race = ifelse(val, Race, NA)) %>%
    summarise(Race = paste(na.omit(Race), collapse = ", "),
              Race = ifelse(Race == "", "I prefer Not to Answer", Race),
              Race = ifelse(n() > 1, "Multiple", Race)) %>%
    unique()
  
  # Further processing to handle "I prefer Not to Answer" and calculate percentages
  df.race <- df.race %>%
    mutate(Race = ifelse(Race == "I prefer Not to Answer", "No race indicated", Race))
  
  # Summarize the data to count each race category
  df.race_summary <- df.race %>%
    group_by(Race) %>%
    summarise(n = n(), .groups = 'drop')
  
  # Calculate percentages
  df.race_summary <- df.race_summary %>%
    mutate(pct = round(n / sum(n), 3))
  
  # Arrange by the number of responses to get the levels for the factor
  levelsrace <- df.race_summary %>%
    arrange(-n) %>%
    pull(Race)
  
  # Set the factor levels for Race based on the arranged order
  df.race_summary$Race <- factor(df.race_summary$Race, levels = levelsrace)
  
  
  # Education mapping
  educationLevels <- c("Grade school", "High school graduate", "Some college/technical school", 
                       "College graduate or beyond")
  df.edu <- select(df, ID, demo_highesteducation)
  names(df.edu)[2] <- "Education"
  df.edu$Education <- factor(mapvalues(df.edu$Education, from = c(1,3,4,5), to = educationLevels), levels = educationLevels)
  
  df.edu <- df.edu %>% 
    group_by(Education) %>% 
    summarize(n = n()) %>% 
    ungroup() %>% 
    mutate(pct = round(n / sum(n), 3))
  
  return(list(race_df = df.race_summary, edu_df = df.edu))
}

getDemogInfo <- function(demog_str) {
  df <- read.csv(demog_str)
  
  #names(df) <- c("Study ID: XXXX",
   #               "What is your race? (Please mark all that apply) (choice=American Indian or Alaskan Native)",
  #                "What is your race? (Please mark all that apply) (choice=Asian)",
   #               "What is your race? (Please mark all that apply) (choice=Black or African American)",
    #              "What is your race? (Please mark all that apply) (choice=Hispanic or Latino)",
     #             "What is your race? (Please mark all that apply) (choice=Native Hawaiian or other Pacific Islander)",
      #            "What is your race? (Please mark all that apply) (choice=White)",
       #           "What is your race? (Please mark all that apply) (choice=I prefer Not to Answer)",
        #          "What is the highest level of education you have received?",
         #         "Current Zip:")
  
#  df[2:8] <- lapply(df[2:8], function(x) ifelse(x == 1, "Checked", "Unchecked"))
  
 # educationMap <- setNames(c(NA, "Grade school", "High school graduate", "Some college/technical school", 
  #                           "College graduate or beyond"), c(NA, 1, 3, 4, 5))
  
  #df$"What is the highest level of education you have 
  #received?" <- educationMap[as.character(df$"What is the highest level of education you have received?")]
  
  #df <- df %>% select(1:9)
  names(df)[1] <- "ID"
  df.race <- select(df, ID, contains("What.is.your.race"))
  names(df.race) <- gsub(".*choice\\.|\\.$", "", names(df.race))
  df.race <- lapply(as.list(df.race), function(x) {
    x <- ifelse(x == "Checked", T, 
                ifelse(x == "Unchecked", F, x))
    return(x)
  }) |> bind_cols()
  
  df.race <- df.race %>% 
    gather(key, val, -ID) %>%  
    group_by(ID) |> 
    mutate(val = as.logical(val),
           Race = paste0(key[val], collapse = ", "),
           Race = ifelse(sum(val) > 1, "Multiple", Race),
           Race = ifelse(Race == "", "I prefer Not to Answer", Race),
           Race = gsub("\\.", " ", Race),
           Race = ifelse(Race %in% "I prefer Not to Answer", "No race indicated", Race)) |> 
    select(-key, -val) |> 
    unique()
  
  #saveRDS(df.race, "misc/race_info.rds")
  
  levelsrace <- df.race |> 
    group_by(Race) |> 
    summarize(n = n()) |> 
    arrange(-n) |> 
    pull(Race)
  
  df.race <- df.race |> 
    select(-ID) |> 
    group_by(Race) |> 
    summarize(n = n()) |> 
    ungroup() |> 
    mutate(pct = round(n/sum(n), 3))
  
  df.race$Race <- factor(df.race$Race, levelsrace)
  
  
  df.edu <- select(df, ID, contains("education")) 
  names(df.edu)[2] <- "Education"
  df.edu <- df.edu |> 
    mutate(Education = ifelse(Education == "", "Unknown", Education)) 
  
  levelsedu <- df.edu |> 
    group_by(Education) |> 
    summarize(n = n()) |> 
    arrange(-n) |> 
    pull(Education)
  df.edu <- df.edu |> 
    select(-ID) |> 
    group_by(Education) |> 
    summarize(n = n()) |> 
    ungroup() |> 
    mutate(pct = round(n/sum(n), 3))
  
  df.edu$Education <- factor(df.edu$Education, levelsedu)
  
  return(list(race_df=df.race, edu_df=df.edu))

}



getRaceBar <- function(race_df){
  gp <- ggplot(race_df, aes(x = Race, y = n, label = scales::percent(pct))) +
    geom_bar(stat = "identity", fill = "lightgray", color = "black") +
    geom_text(position = position_dodge(width = .9),   
              vjust = -0.5,   
              size = 3.5) +
    ylim(c(0, max(race_df['n']) + 20  )) +
    theme_DB(rotate.x = T) +
    theme(plot.title = element_text(hjust = 0.5),  
             plot.subtitle = element_text(hjust = 0.5)) + 
    ylab("Count") +
    ggtitle("Racial demographic distribution of all participants enrolled and completed surveys") + 
    labs(subtitle = "This includes participants recruited from the Web and Clinics.") 
  return(gp)
  #ggsave("plots/BCSB_race_barchart.png", height = 8, width = 4)
}

getEduBar <- function(edu_df){
  gp <- ggplot(edu_df, aes(x = Education, y = n, label = scales::percent(pct))) +
    geom_bar(stat = "identity", fill = "lightgray", color = "black") +
    geom_text(position = position_dodge(width = .9),
              vjust = -0.5, 
              size = 5) +
    ylim(c(0, max(edu_df['n']) + 20)) +
    theme_DB(rotate.x = T) + 
    theme(plot.title = element_text(hjust = 0.5),  
          plot.subtitle = element_text(hjust = 0.5)) +
    ylab("Count") +
    ggtitle("Education demographic distribution of all participants enrolled and completed surveys")   
  return(gp)
    # ggsave("plots/BCSB_education_barchart.png", height = 8, width = 4)
}


