library(ggplot2)
library(tidyverse)
source("theme_DB.R")
library(dplyr)

getDemogInfo <- function(demog_str) {
  df <- read.csv(demog_str)
  #df <- df %>% select(1:9)
  names(df)[1] <- "ID"
  df.race <- select(df, ID, contains("What.is.your.race"))
  names(df.race) <- gsub(".*choice\\.|\\.$", "", names(df.race))
  df.race <- lapply(as.list(df.race), function(x) {
    x <- ifelse(x == "Checked", T, 
                ifelse(x == "Unchecked", F, x))
    return(x)
  }) |> bind_cols()
  
  df.race <- df.race |> 
    gather(key, val, -ID) |> 
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
    ylab("Count") 
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
    ylab("Count") 
  return(gp)
    # ggsave("plots/BCSB_education_barchart.png", height = 8, width = 4)
}


