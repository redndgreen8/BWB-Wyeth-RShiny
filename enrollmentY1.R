library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(ggrepel)
source("theme_DB.R")
source("str_list.R")



getEnrollmentY <- function(enroll_str) {
  
  #df <- read.csv("HS2100716BodourSalhi-EnrollmentSummaryFol_DATA_LABELS_2024-11-11_1708.csv")
  df <- read.csv(enroll_str)
  df[df == ""] <-NA
  names(df) <- gsub("[[:punct:] ]", "", names(df))
  
  df.enroll <- df |>
    group_by(RecordID) |>
    mutate(row_id = seq_len(n())) |>
    tidyr::pivot_wider(
      id_cols = RecordID,
      names_from = row_id,
      values_from = -c(RecordID, row_id)
    )
  
  df.enroll <- select(df.enroll, RecordID, Clinic_1, ConsentSigned_1, SurveyStatus_1, BloodDrawStatus_1, SurveyStatus_2, BloodDrawStatus_2, SurveyStatus_3, BloodDrawStatus_3, Whatisthehighestlevelofeducationyouhavereceived_1, WhatisyourracePleasemarkallthatapply_1, WhatisyourRaceEthnicity_1, DateofWebsiteEligibilitySurvey_1)
  
  df.enroll <- df.enroll |>
    mutate(
      WhatisyourRaceEthnicity_1 = ifelse( WhatisyourRaceEthnicity_1 == "Hispanic or Latinx", "Hispanic or Latino", WhatisyourRaceEthnicity_1 ),
      WhatisyourRaceEthnicity_1 = ifelse( WhatisyourRaceEthnicity_1 == "Native Hawaiian or Pacific Islander", "Native Hawaiian or other Pacific Islander", WhatisyourRaceEthnicity_1 ),
      WhatisyourRaceEthnicity_1 = ifelse( WhatisyourRaceEthnicity_1 == "Prefer not to answer", "I prefer Not to Answer", WhatisyourRaceEthnicity_1 ),
      
      WhatisyourracePleasemarkallthatapply_1 = ifelse( is.na(WhatisyourracePleasemarkallthatapply_1), WhatisyourRaceEthnicity_1, WhatisyourracePleasemarkallthatapply_1),
      #Race = ifelse( WhatisyourracePleasemarkallthatapply_1 == "", WhatisyourRaceEthnicity_1, WhatisyourracePleasemarkallthatapply_1),
      Race = ifelse( WhatisyourracePleasemarkallthatapply_1 == "", "No race indicated", WhatisyourracePleasemarkallthatapply_1),
      
      Race = ifelse(Race %in% "I prefer Not to Answer", 
                    "No race indicated", Race),
      Race = ifelse( grepl(",", Race) , "Multiple", Race  ) ,
      location = ifelse(!is.na(DateofWebsiteEligibilitySurvey_1)  ,
                        "Web", as.character(Clinic_1)),
      edu = ifelse(is.na(Whatisthehighestlevelofeducationyouhavereceived_1), 
                   "NA", Whatisthehighestlevelofeducationyouhavereceived_1),
      #survey = ifelse(SurveyStatus_2 == "Completed", SurveyStatus_2, "Incomplete")
      ) |>
    select(-WhatisyourracePleasemarkallthatapply_1, -Whatisthehighestlevelofeducationyouhavereceived_1, -WhatisyourRaceEthnicity_1, -DateofWebsiteEligibilitySurvey_1, -Clinic_1) |>
    filter(BloodDrawStatus_1 == "Completed") |>
    rename(
      ID = RecordID,
      ConsentSigned = ConsentSigned_1,
      SurveyStatus = SurveyStatus_1,
      BloodDrawStatus = BloodDrawStatus_1,
      SurveyStatusY1 = SurveyStatus_2,
      BloodDrawStatusY1 = BloodDrawStatus_2,
      SurveyStatusY2 = SurveyStatus_3,
      BloodDrawStatusY2 = BloodDrawStatus_3) |>
    unique()
  
  return(df.enroll)
}



df.enroll1 <- getEnrollmentY(enroll_str1)
#RaceY1 <- getERacepieComb(df.enroll1)

getEBloodpieCombY1 <- function(df.enroll, yr){
  
  df.phoneConsultRace <- df.enroll |>
    select(-ID) |>
    filter(SurveyStatusY1 %in% c("Completed")) |>
    group_by(BloodDrawStatusY1) |>
    summarize(n = n()) |> 
    ungroup() |> 
    mutate(pct = round(n/sum(n), 3))
  
  levelsRace <- df.phoneConsultRace|>
    group_by(BloodDrawStatusY1) |>
    summarize(n = n()) |> 
    arrange(-n) |>
    pull(BloodDrawStatusY1)
  
  df.phoneConsultRace$BloodDrawStatusY1 <- factor(df.phoneConsultRace$BloodDrawStatusY1, levelsRace)
  
  df2Race <- df.phoneConsultRace |> 
    # dplyr::group_by(RS3) |>
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/10 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", n, "\n", round(pct*100, 1), "%"))
  
  df3Race <- df.phoneConsultRace |>
    dplyr::summarize(n = paste0("N=", sum(n)))
  
  
  # gpEdu
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpRace  
  gpEdu2 <- ggplot(df.phoneConsultRace, aes(x = "", y = pct, fill = fct_inorder(BloodDrawStatusY1))) +
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
          plot.title = element_text(size = 25, hjust=0.5),  
          plot.subtitle = element_text(size = 15, hjust=0.5)) +
    guides(fill = guide_legend(ncol = 2)) +
    ggtitle(paste(yr, "Blood Draw Status, (Survey Complete)"))
  ggsave(paste0("plots/EBC_",yr,".png"), gpEdu2, height = 9, width = 16, dpi = 600)
  
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpFlag  
  return(gpEdu2)  
  
}

getEdupieCombY1 <- function(df.enroll, yr){
  
  df.phoneConsultRace <- df.enroll |>
    select(-ID) |>
    filter(SurveyStatusY1 %in% c("Completed")) |>
    # filter(SurveyStatus != "Scheduled (if in-person)") |>
    #mutate(SurveyStatus = ifelse(SurveyStatus == "Survey Link Emailed (remote)", "Pending", SurveyStatus )) |>
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
          plot.title = element_text(size = 25, hjust=0.5),  
          plot.subtitle = element_text(size = 15, hjust=0.5)) +
    guides(fill = guide_legend(ncol = 2)) +
    ggtitle(paste(yr, "Education Breakdown, (Consent Signed)"))
  ggsave(paste0("plots/EEC_",yr,".png"), gpEdu2, height = 9, width = 16, dpi = 600)
  
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpFlag  
  return(gpEdu2)  
  
}

getERacepieCombY1 <- function(df.enroll, yr){
  
  df.phoneConsultRace <- df.enroll |>
    select(-ID) |>
    filter(SurveyStatusY1 %in% c("Completed")) |>
    # filter(SurveyStatus != "Scheduled (if in-person)") |>
    #mutate(SurveyStatus = ifelse(SurveyStatus == "Survey Link Emailed (remote)", "Pending", SurveyStatus )) |>
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
  

  gpEdu2 <- ggplot(df.phoneConsultRace, aes(x = "", y = pct, fill = fct_inorder(Race))) +
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
          plot.title = element_text(size = 25, hjust=0.5),  
          plot.subtitle = element_text(size = 15, hjust=0.5)) +
    guides(fill = guide_legend(ncol = 2)) +
    ggtitle(paste(yr, " Race Breakdown, (Consent Signed)"))
  ggsave(paste0("plots/ERC_",yr,".png"), gpEdu2, height = 9, width = 16, dpi = 600)
  
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpFlag  
  return(gpEdu2)  
  
}



RaceY1 <- getERacepieCombY1(df.enroll1, "Y1")
BloodY1 <- getEBloodpieCombY1(df.enroll1, "Y1")
EduY1 <- getEdupieCombY1(df.enroll1, "Y1")



getEBloodpieCombY2 <- function(df.enroll, yr){
  
  df.phoneConsultRace <- df.enroll |>
    select(-ID) |>
    filter(SurveyStatusY2 %in% c("Completed")) |>
    group_by(BloodDrawStatusY2) |>
    summarize(n = n()) |> 
    ungroup() |> 
    mutate(pct = round(n/sum(n), 3))
  
  levelsRace <- df.phoneConsultRace|>
    group_by(BloodDrawStatusY2) |>
    summarize(n = n()) |> 
    arrange(-n) |>
    pull(BloodDrawStatusY2)
  
  df.phoneConsultRace$BloodDrawStatusY2 <- factor(df.phoneConsultRace$BloodDrawStatusY2, levelsRace)
  
  df2Race <- df.phoneConsultRace |> 
    # dplyr::group_by(RS3) |>
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/10 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", n, "\n", round(pct*100, 1), "%"))
  
  df3Race <- df.phoneConsultRace |>
    dplyr::summarize(n = paste0("N=", sum(n)))
  
  
  # gpEdu
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpRace  
  gpEdu2 <- ggplot(df.phoneConsultRace, aes(x = "", y = pct, fill = fct_inorder(BloodDrawStatusY2))) +
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
          plot.title = element_text(size = 25, hjust=0.5),  
          plot.subtitle = element_text(size = 15, hjust=0.5)) +
    guides(fill = guide_legend(ncol = 2)) +
    ggtitle(paste(yr, "Blood Draw Status, (Survey Complete)"))
  ggsave(paste0("plots/EBC_",yr,".png"), gpEdu2, height = 9, width = 16, dpi = 600)
  
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpFlag  
  return(gpEdu2)  
  
}

getEdupieCombY2 <- function(df.enroll, yr){
  
  df.phoneConsultRace <- df.enroll |>
    select(-ID) |>
    filter(SurveyStatusY2 %in% c("Completed")) |>
    # filter(SurveyStatus != "Scheduled (if in-person)") |>
    #mutate(SurveyStatus = ifelse(SurveyStatus == "Survey Link Emailed (remote)", "Pending", SurveyStatus )) |>
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
          plot.title = element_text(size = 25, hjust=0.5),  
          plot.subtitle = element_text(size = 15, hjust=0.5)) +
    guides(fill = guide_legend(ncol = 2)) +
    ggtitle(paste(yr, "Education Breakdown, (Consent Signed)"))
  ggsave(paste0("plots/EEC_",yr,".png"), gpEdu2, height = 9, width = 16, dpi = 600)
  
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpFlag  
  return(gpEdu2)  
  
}

getERacepieCombY2 <- function(df.enroll, yr){
  
  df.phoneConsultRace <- df.enroll |>
    select(-ID) |>
    filter(SurveyStatusY2 %in% c("Completed")) |>
    # filter(SurveyStatus != "Scheduled (if in-person)") |>
    #mutate(SurveyStatus = ifelse(SurveyStatus == "Survey Link Emailed (remote)", "Pending", SurveyStatus )) |>
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
  
  
  gpEdu2 <- ggplot(df.phoneConsultRace, aes(x = "", y = pct, fill = fct_inorder(Race))) +
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
          plot.title = element_text(size = 25, hjust=0.5),  
          plot.subtitle = element_text(size = 15, hjust=0.5)) +
    guides(fill = guide_legend(ncol = 2)) +
    ggtitle(paste(yr, " Race Breakdown, (Consent Signed)"))
  ggsave(paste0("plots/ERC_",yr,".png"), gpEdu2, height = 9, width = 16, dpi = 600)
  
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpFlag  
  return(gpEdu2)  
  
}

RaceY1 <- getERacepieCombY1(df.enroll1, "Y1")
BloodY1 <- getEBloodpieCombY1(df.enroll1, "Y1")
EduY1 <- getEdupieCombY1(df.enroll1, "Y1")



RaceY2 <- getERacepieCombY2(df.enroll1, "Y2")
BloodY2 <- getEBloodpieCombY2(df.enroll1, "Y2")
EduY2 <- getEdupieCombY2(df.enroll1, "Y2")

