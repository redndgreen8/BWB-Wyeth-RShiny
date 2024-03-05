library(ggplot2)
library(tidyverse)
source("theme_DB.R")
library(dplyr)
library(tidyr)


getDemogInfo <- function(demog_str) {
  df <- read.csv(demog_str)
  
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

#demog_dat <- getDemogInfo("DemographicsRaceEduc_variable labels_2.15.24.csv")


getRacePie <- function(race_df){
  
  total_count <- sum(race_df$n)

  df2 <- race_df |> 
    # dplyr::group_by(RS3) |>
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/10 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", n, "\n", round(pct*100, 1), "%")) 
  df3 <- race_df |> 
    # dplyr::group_by(diagnosis) |> 
    dplyr::summarize(n = paste0("N=", sum(n)))
  
# Create the pie chart
  gp <- ggplot(race_df, aes(x = "", y = pct, fill = fct_inorder(Race))) +
    geom_col(width = 1, color = 1, linewidth = 0.5) +
    coord_polar(theta = "y") +  # This creates the pie chart
    scale_fill_brewer(palette = "Set3") +  # Set color palette
    geom_label_repel(data = df2,
                     aes(y = pos, label = label),
                     size = 3.75, 
                     nudge_x = 1,
                     show.legend = FALSE, 
                     label.padding = unit(0.75, "mm")) +
    geom_text(data=df3, x = -1.15, y = 0, aes(label = n), 
              colour = "black", inherit.aes = F, parse = F, size = 8) +
  #  geom_label(aes(label = scales::percent(pct)), 
   #            position = position_stack(vjust = 0.5)) +  # Add percentage labels
    
    theme_DB() +  # Remove axes and background
    ylab("")+
    xlab("")+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.y = element_blank(),
          strip.text = element_text(size = 15, face = "bold"),
          legend.text = element_text(size = 15, face = "bold"),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),  
          plot.subtitle = element_text(hjust = 0.5)) +
    guides(fill = guide_legend(ncol = 2)) +
    ggtitle("Racial demographic distribution of all participants 
              enrolled and completed surveys") + 
    labs(subtitle = "This includes participants recruited from the Web and Clinics.",
         fill = "Race") 
  return(gp)

}
#getRacePie(demog_dat$race_df)


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
    ggtitle("Racial demographic distribution of all participants
            enrolled and completed surveys") + 
    labs(subtitle = "This includes participants recruited from the Web and Clinics.") 
  return(gp)
  #ggsave("plots/BCSB_race_barchart.png", height = 8, width = 4)
}

#getRaceBar(demog_dat$race_df)

getEduPie <- function(edu_df){
  
  total_count <- sum(edu_df$n)
  
  df2 <- edu_df |> 
    # dplyr::group_by(RS3) |>
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/10 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", n, "\n", round(pct*100, 1), "%")) 
  df3 <- edu_df |> 
    # dplyr::group_by(diagnosis) |> 
    dplyr::summarize(n = paste0("N=", sum(n)))
  
  # Create the pie chart
  gp <- ggplot(edu_df, aes(x = "", y = pct, fill = fct_inorder(Education))) +
    geom_col(width = 1, color = 1, linewidth = 0.5) +
    coord_polar(theta = "y") +  # This creates the pie chart
    scale_fill_brewer(palette = "Set3") +  # Set color palette
    geom_label_repel(data = df2,
                     aes(y = pos, label = label),
                     size = 3.75, 
                     nudge_x = 1,
                     show.legend = FALSE, 
                     label.padding = unit(0.75, "mm")) +
    geom_text(data=df3, x = -1.15, y = 0, aes(label = n), 
              colour = "black", inherit.aes = F, parse = F, size = 8) +
    #  geom_label(aes(label = scales::percent(pct)), 
    #            position = position_stack(vjust = 0.5)) +  # Add percentage labels
    
    theme_DB() +  # Remove axes and background
    ylab("")+
    xlab("")+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.y = element_blank(),
          strip.text = element_text(size = 15, face = "bold"),
          legend.text = element_text(size = 15, face = "bold"),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),  
          plot.subtitle = element_text(hjust = 0.5)) +
    guides(fill = guide_legend(nrow = 3, byrow = T)) +
    ggtitle("Education demographic distribution of all participants 
            enrolled and completed surveys")
  return(gp)
  
}
#getEduPie(demog_dat$edu_df)


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
    ggtitle("Education demographic distribution of all participants 
            enrolled and completed surveys")   
  return(gp)
    # ggsave("plots/BCSB_education_barchart.png", height = 8, width = 4)
}
#getEduBar(demog_dat$edu_df)

