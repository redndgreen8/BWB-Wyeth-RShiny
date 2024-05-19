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
    gather(key, val, -ID) 
  
  df.race <- df.race %>%  
    group_by(ID) 
  
  df.race <- df.race |> 
    mutate(val = as.logical(val),
           Race = paste0(key[val], collapse = ", "),
           Race = ifelse(sum(val) > 1, "Multiple", Race),
           Race = ifelse(Race == "", "I prefer Not to Answer", Race),
           Race = gsub("\\.", " ", Race),
           Race = ifelse(Race %in% "I prefer Not to Answer", "No race indicated", Race)) 
  
  df.race <- df.race |> 
    select(-key, -val) |> 
    unique()

    #df.race <- df.race |> 
  #  group_by(ID) |>
    #summarise(Race = paste(unique(Race), collapse = ", "))
  
#  missing_rows <- anti_join(df, df.race, by = c("ID"))
  
 #  duplicated_rows <- df.race[duplicated(df.race) | duplicated(df.race, fromLast = TRUE), ]
  
  #saveRDS(df.race, "misc/race_info.rds")
  dfrace <- df.race
  
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
  
  dfedu <- df.edu
  
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
  
  return(list(race_df=df.race, edu_df=df.edu, r_df=dfrace, e_df=dfedu))

}

#demog_dat <- getDemogInfo("DemographicsRaceEduc_variable labels_3.19.24.csv")

#race_df<-demog_dat$race_df

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
  ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  
  return(gp)

}




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

getDRpie <- function(df.enroll){
  dfPCR <- df.enroll %>%
    dplyr::filter(ConsentSigned == "Yes") %>%
    dplyr::filter(survey == "Completed") %>%
    dplyr::mutate(Race = ifelse(grepl(",",Race), "Multiple", Race)) %>%
    dplyr::count(location, Race) %>%
    dplyr::group_by(location) %>%
    dplyr::mutate(total = sum(n)) %>%
    dplyr::mutate(pct = n / total) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(location, Race) %>%
    dplyr::filter(!is.na(location)) 
  
  
  df2PCR <- dfPCR %>%
    dplyr::group_by(location) %>%
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/2 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", n, "\n", round(pct*100, 1), "%")) %>%
    dplyr::ungroup()
  
  df3PCR <- dfPCR %>%
    dplyr::group_by(location) %>%
    dplyr::summarize(n = paste0("N=", sum(n))) %>%
    dplyr::ungroup()
  
  gpPCR <- ggplot(dfPCR, aes(x = "" , y = pct, fill = Race)) +
    geom_col(width = 1, color = "black", size = 0.5) +
    facet_grid(cols = vars(location) ) +
    coord_polar(theta = "y") +
    scale_fill_brewer(palette = "Set3") +
    geom_label_repel(data = df2PCR,
                     aes(y = pos, label = label),
                     size = 3.75, 
                     nudge_x = 1,
                     show.legend = FALSE, 
                     label.padding = unit(0.75, "mm")) +
    geom_text(data = df3PCR, x = -1.15, y = 0, aes(label = n), 
              colour = "black", inherit.aes = FALSE, parse = FALSE, size = 8) + 
    theme_DB() +
    ylab("") + 
    xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.y = element_blank(),
          strip.text = element_text(size = 15, face = "bold"),
          legend.text = element_text(size = 15, face = "bold"),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),  
          plot.subtitle = element_text(hjust = 0.5)) +
    guides(fill = guide_legend(ncol = 2)) +
    ggtitle("Race Breakdown Based on location of approach")  

  gpPCR
  ggsave("plots/PCR.png", gpPCR, height = 9, width = 16, dpi = 600)
  
  return(gpPCR)
  
}


getDRpieComb <- function(df.enroll){
  
  df.phoneConsultRace <- df.enroll |>
    select(-ID) |>
    filter(ConsentSigned == "Yes") |>
    filter(survey == "Completed") |>
    mutate(Race = ifelse(grepl(",",Race), "Multiple", Race)) |>
    group_by(Race) |>
    summarize(n = n()) |> 
    ungroup() |> 
    mutate(pct = round(n/sum(n), 3))
  
  levelsRace <- df.phoneConsultRace|>
    group_by(Race) |>
    summarize(n = n()) |> 
    arrange(-n) |>
    pull(Race)
  
  df.phoneConsultRace$Race <- factor(df.phoneConsultRace$Race, levelsRace)
  
  df2Race <- df.phoneConsultRace |> 
    # dplyr::group_by(RS3) |>
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/10 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", n, "\n", round(pct*100, 1), "%"))
  
  df3Race <- df.phoneConsultRace |>
    dplyr::summarize(n = paste0("N=", sum(n)))
  
  gpRace <- ggplot(df.phoneConsultRace, aes(x = "", y = pct, fill = fct_inorder(Race))) +
    geom_col(width = 1, color = 1, linewidth = 0.5) +
    coord_polar(theta = "y") +  # This creates the pie chart
    scale_fill_brewer(palette = "Set3") +  # Set color palette
    geom_label_repel(data = df2Race,
                     aes(y = pos, label = label),
                     size = 3.75, 
                     nudge_x = 1,
                     show.legend = FALSE, 
                     label.padding = unit(0.75, "mm")) +
    geom_text(data=df3Race, x = -1.15, y = 0, aes(label = n), 
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
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "none") +
    guides(fill = guide_legend(ncol = 2)) +
    ggtitle("Race Demographics") #+ 
    #labs(subtitle = "This includes participants from the Web and Clinics.",
     #    fill = "Race") 
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpRace  
  gpRace2 <- ggplot(df.phoneConsultRace, aes(x = "", y = pct, fill = fct_inorder(Race))) +
    geom_col(width = 1, color = 1, linewidth = 0.5) +
    coord_polar(theta = "y") +  # This creates the pie chart
    scale_fill_brewer(palette = "Set3") +  # Set color palette
    geom_label_repel(data = df2Race,
                     aes(y = pos, label = label),
                     size = 3.75, 
                     nudge_x = 1,
                     show.legend = FALSE, 
                     label.padding = unit(0.75, "mm")) +
    geom_text(data=df3Race, x = -1.15, y = 0, aes(label = n), 
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
          plot.title = element_text(hjust = 0.5, size = 25),  
          plot.subtitle = element_text(hjust = 0.5, size = 15)) +
    guides(fill = guide_legend(ncol = 2)) +
    ggtitle("Race Demographics")
  ggsave("plots/PCRC.png", gpRace2, height = 9, width = 16, dpi = 600)
  
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpFlag  
  return(gpRace)  
  
}

getDEpie <- function(df.enroll){
  dfPCR <- df.enroll %>%
    dplyr::filter(ConsentSigned == "Yes") %>%
    dplyr::filter(survey == "Completed") %>%
    dplyr::count(location, edu) %>%
    dplyr::group_by(location) %>%
    dplyr::mutate(total = sum(n)) %>%
    dplyr::mutate(pct = n / total) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(location, edu) %>%
    dplyr::filter(!is.na(location)) 
  
  
  df2PCR <- dfPCR %>%
    dplyr::group_by(location) %>%
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/2 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", n, "\n", round(pct*100, 1), "%")) %>%
    dplyr::ungroup()
  
  df3PCR <- dfPCR %>%
    dplyr::group_by(location) %>%
    dplyr::summarize(n = paste0("N=", sum(n))) %>%
    dplyr::ungroup()
  
  gpPCE <- ggplot(dfPCR, aes(x = "" , y = pct, fill = edu)) +
    geom_col(width = 1, color = "black", size = 0.5) +
    facet_grid(cols = vars(location) ) +
    coord_polar(theta = "y") +
    scale_fill_brewer(palette = "Set3") +
    geom_label_repel(data = df2PCR,
                     aes(y = pos, label = label),
                     size = 3.75, 
                     nudge_x = 1,
                     show.legend = FALSE, 
                     label.padding = unit(0.75, "mm")) +
    geom_text(data = df3PCR, x = -1.15, y = 0, aes(label = n), 
              colour = "black", inherit.aes = FALSE, parse = FALSE, size = 8) + 
    theme_DB() +
    ylab("") + 
    xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.y = element_blank(),
          strip.text = element_text(size = 15, face = "bold"),
          legend.text = element_text(size = 15, face = "bold"),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),  
          plot.subtitle = element_text(hjust = 0.5)) +
    guides(fill = guide_legend(ncol = 2)) +
    ggtitle("Education Breakdown Based on location of approach (Consented, Enrolled)")  +
    labs(subtitle = "NAs due to incomplete baseline survey")
  ggsave("plots/DE.png", gpPCE, height = 9, width = 16, dpi = 600)
  
  return(gpPCE)
  
}









getDEpieComb <- function(df.enroll){
  
  df.phoneConsultRace <- df.enroll |>
    select(-ID) |>
    filter(ConsentSigned == "Yes") |>
    filter(survey == "Completed") |>
    group_by(edu) |>
    summarize(n = n()) |> 
    ungroup() |> 
    mutate(pct = round(n/sum(n), 3))
  
  levelsRace <- df.phoneConsultRace|>
    group_by(edu) |>
    summarize(n = n()) |> 
    arrange(-n) |>
    pull(edu)
  
  df.phoneConsultRace$edu <- factor(df.phoneConsultRace$edu, levelsRace)
  
  df2Race <- df.phoneConsultRace |> 
    # dplyr::group_by(RS3) |>
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/10 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", n, "\n", round(pct*100, 1), "%"))
  
  df3Race <- df.phoneConsultRace |>
    dplyr::summarize(n = paste0("N=", sum(n)))
  
  gpEdu <- ggplot(df.phoneConsultRace, aes(x = "", y = pct, fill = fct_inorder(edu))) +
    geom_col(width = 1, color = 1, linewidth = 0.5) +
    coord_polar(theta = "y") +  # This creates the pie chart
    scale_fill_brewer(palette = "Set3") +  # Set color palette
    geom_label_repel(data = df2Race,
                     aes(y = pos, label = label),
                     size = 3.75, 
                     nudge_x = 1,
                     show.legend = FALSE, 
                     label.padding = unit(0.75, "mm")) +
    geom_text(data=df3Race, x = -1.15, y = 0, aes(label = n), 
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
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "none") +
    guides(fill = guide_legend(ncol = 2)) +
    ggtitle("Education Demographics") #+ 
    #labs(subtitle = "Consented, Enrolled",
#         fill = "edu") 
  #gpEdu
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpRace  
  gpEdu2 <- ggplot(df.phoneConsultRace, aes(x = "", y = pct, fill = fct_inorder(edu))) +
    geom_col(width = 1, color = 1, linewidth = 0.5) +
    coord_polar(theta = "y") +  # This creates the pie chart
    scale_fill_brewer(palette = "Set3") +  # Set color palette
    geom_label_repel(data = df2Race,
                     aes(y = pos, label = label),
                     size = 3.75, 
                     nudge_x = 1,
                     show.legend = FALSE, 
                     label.padding = unit(0.75, "mm")) +
    geom_text(data=df3Race, x = -1.15, y = 0, aes(label = n), 
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
          plot.title = element_text(hjust = 0.5, size = 25),  
          plot.subtitle = element_text(hjust = 0.5, size = 15)) +
    guides(fill = guide_legend(ncol = 2)) +
    ggtitle("Education Demographics")
  ggsave("plots/DEC.png", gpEdu2, height = 9, width = 16, dpi = 600)
  
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpFlag  
  return(gpEdu)  
  
}
