

source("str_list.R")
#source("BCSB_utils.R")

getEnrollment <- function(enroll_str) {
  
  #df <- read.csv("HS2100716BodourSalhi-EnrollmentSummary_DATA_LABELS_2024-03-22_1440.csv")
  df <- read.csv(enroll_str)
  df[df == ""] <-NA
  names(df) <- gsub("[[:punct:] ]", "", names(df))
  names(df)[1] <- "ID"
  
  
  df.enroll <- select(df, ID,  Clinic, ConsentSigned, SurveyStatus, BloodDrawStatus, Whatisthehighestlevelofeducationyouhavereceived, WhatisyourracePleasemarkallthatapply, DateofcompletionofWebsiteEligibilitySurvey, ExternalRecordsRequestStatus, ExternalRecordsDataEntryStatus)
  
  df.enroll <- df.enroll |>
    mutate(
      Race = ifelse( WhatisyourracePleasemarkallthatapply == "", "No race indicated", WhatisyourracePleasemarkallthatapply),
      Race = ifelse(Race %in% "I prefer Not to Answer", 
                    "No race indicated", Race),
      location = ifelse(!is.na(DateofcompletionofWebsiteEligibilitySurvey)  ,
                        "Web", as.character(Clinic)),
      edu = ifelse(is.na(Whatisthehighestlevelofeducationyouhavereceived), 
                   "NA", Whatisthehighestlevelofeducationyouhavereceived),
      survey = ifelse(SurveyStatus == "Completed", SurveyStatus, "Incomplete")) |>
    select(-WhatisyourracePleasemarkallthatapply, -Clinic, -Whatisthehighestlevelofeducationyouhavereceived, -DateofcompletionofWebsiteEligibilitySurvey) |>
    filter(ConsentSigned == "Yes") |>
    unique()
  
  return(df.enroll)
}

df.enroll <- getEnrollment(enroll_str)

#sS <- getPieChart(df.enroll, "SurveyStatus", "survey.png" , "Title", plot_by_location = FALSE)
#sS
getESurveypie <- function(df.enroll){
  dfPCR <- df.enroll %>%
    #dplyr::filter(SurveyStatus != "Survey Link Emailed (remote)") %>%
    #dplyr::filter(SurveyStatus !=   "Scheduled (if in-person)") %>%
    dplyr::mutate(SurveyStatus = ifelse(SurveyStatus == "Survey Link Emailed (remote)", "Pending", SurveyStatus )) %>%
    dplyr::mutate(SurveyStatus = ifelse(SurveyStatus == "Scheduled (if in-person)", "Pending", SurveyStatus )) %>%
    dplyr::mutate(SurveyStatus =  ifelse(SurveyStatus == "Completed", "Completed", SurveyStatus)) %>%
    dplyr::count(location, SurveyStatus) %>%
    dplyr::group_by(location) %>%
    dplyr::mutate(total = sum(n)) %>%
    dplyr::mutate(pct = n / total) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(location, SurveyStatus) %>%
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
  
  gpPCE <- ggplot(dfPCR, aes(x = "" , y = pct, fill = SurveyStatus)) +
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
    ggtitle("Survey Status Based on location of approach, (Consent Signed)")  #+
  #    labs(subtitle = "NAs due to incomplete baseline survey")
 # gpPCE
  ggsave("plots/ES.png", gpPCE, height = 9, width = 16, dpi = 600)
  
  return(gpPCE)
  
}

getESurveypieComb <- function(df.enroll){
  
  df.phoneConsultRace <- df.enroll |>
    select(-ID) |>
    #filter(SurveyStatus != "Survey Link Emailed (remote)") |>
   # filter(SurveyStatus != "Scheduled (if in-person)") |>
    mutate(SurveyStatus = ifelse(SurveyStatus == "Survey Link Emailed (remote)", "Pending", SurveyStatus )) |>
    mutate(SurveyStatus = ifelse(SurveyStatus == "Scheduled (if in-person)", "Pending", SurveyStatus )) |>
    group_by(SurveyStatus) |>
    summarize(n = n()) |> 
    ungroup() |> 
    mutate(pct = round(n/sum(n), 3))
  
  levelsRace <- df.phoneConsultRace|>
    group_by(SurveyStatus) |>
    summarize(n = n()) |> 
    arrange(-n) |>
    pull(SurveyStatus)
  
  df.phoneConsultRace$SurveyStatus <- factor(df.phoneConsultRace$SurveyStatus, levelsRace)
  
  df2Race <- df.phoneConsultRace |> 
    # dplyr::group_by(RS3) |>
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/10 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", n, "\n", round(pct*100, 1), "%"))
  
  df3Race <- df.phoneConsultRace |>
    dplyr::summarize(n = paste0("N=", sum(n)))
  
  gpEdu <- ggplot(df.phoneConsultRace, aes(x = "", y = pct, fill = fct_inorder(SurveyStatus))) +
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
    ggtitle("Survey Status, (Consent Signed)") #+ 
  #  labs(subtitle = "Consented, Enrolled",
     #    fill = "edu") 
 # gpEdu
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpRace 
  gpEdu2 <- ggplot(df.phoneConsultRace, aes(x = "", y = pct, fill = fct_inorder(SurveyStatus))) +
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
    ggtitle("Survey Status, (Consent Signed)")
  ggsave("plots/ESC.png", gpEdu2, height = 9, width = 16, dpi = 600)
  
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpFlag  
  return(gpEdu)  
  
}

getEBloodpieComb <- function(df.enroll){
  
  df.phoneConsultRace <- df.enroll |>
    select(-ID) |>
    filter(SurveyStatus %in% c("Completed")) |>
    group_by(BloodDrawStatus) |>
    summarize(n = n()) |> 
    ungroup() |> 
    mutate(pct = round(n/sum(n), 3))
  
  levelsRace <- df.phoneConsultRace|>
    group_by(BloodDrawStatus) |>
    summarize(n = n()) |> 
    arrange(-n) |>
    pull(BloodDrawStatus)
  
  df.phoneConsultRace$BloodDrawStatus <- factor(df.phoneConsultRace$BloodDrawStatus, levelsRace)
  
  df2Race <- df.phoneConsultRace |> 
    # dplyr::group_by(RS3) |>
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/10 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", n, "\n", round(pct*100, 1), "%"))
  
  df3Race <- df.phoneConsultRace |>
    dplyr::summarize(n = paste0("N=", sum(n)))
  
  gpEdu <- ggplot(df.phoneConsultRace, aes(x = "", y = pct, fill = fct_inorder(BloodDrawStatus))) +
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
    ggtitle("Blood Draw Status, (Survey Complete)") #+ 
  #  labs(subtitle = "Consented, Enrolled",
  #    fill = "edu") 
 # gpEdu
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpRace  
  gpEdu2 <- ggplot(df.phoneConsultRace, aes(x = "", y = pct, fill = fct_inorder(BloodDrawStatus))) +
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
    ggtitle("Blood Draw Status, (Survey Complete)")
  ggsave("plots/plots/EBC.png", gpEdu2, height = 9, width = 16, dpi = 600)
  
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpFlag  
  return(gpEdu)  
  
}


#c("Withdrew", "Lost to contact", "NA", "Pending")
getEBloodpie <- function(df.enroll){
  dfPCR <- df.enroll %>%
    dplyr::filter(SurveyStatus %in% c("Completed")) %>%
    dplyr::count(location, BloodDrawStatus) %>%
    dplyr::group_by(location) %>%
    dplyr::mutate(total = sum(n)) %>%
    dplyr::mutate(pct = n / total) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(location, BloodDrawStatus) %>%
    dplyr::filter(!is.na(location) )
  
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
  
  gpPCE <- ggplot(dfPCR, aes(x = "" , y = pct, fill = BloodDrawStatus)) +
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
    ggtitle("Blood Draw Status Based on location of approach, (Survey Complete)")  #+
  #    labs(subtitle = "NAs due to incomplete baseline survey")
  gpPCE
  ggsave("plots/EB.png", gpPCE, height = 9, width = 16, dpi = 600)
  
  return(gpPCE)
  
}




getERreqpieComb <- function(df.enroll){
  
  df.phoneConsultRace <- df.enroll |>
    select(-ID) |>
    filter(!is.na(ExternalRecordsRequestStatus) ) |>
    group_by(ExternalRecordsRequestStatus) |>
    summarize(n = n()) |> 
    ungroup() |> 
    mutate(pct = round(n/sum(n), 3))
  
  levelsRace <- df.phoneConsultRace|>
    group_by(ExternalRecordsRequestStatus) |>
    summarize(n = n()) |> 
    arrange(-n) |>
    pull(ExternalRecordsRequestStatus)
  
  df.phoneConsultRace$ExternalRecordsRequestStatus <- factor(df.phoneConsultRace$ExternalRecordsRequestStatus, levelsRace)
  
  df2Race <- df.phoneConsultRace |> 
    # dplyr::group_by(RS3) |>
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/10 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", n, "\n", round(pct*100, 1), "%"))
  
  df3Race <- df.phoneConsultRace |>
    dplyr::summarize(n = paste0("N=", sum(n)))
  
  gpEdu <- ggplot(df.phoneConsultRace, aes(x = "", y = pct, fill = fct_inorder(ExternalRecordsRequestStatus))) +
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
          plot.subtitle = element_text(hjust = 0.5)) +
    guides(fill = guide_legend(ncol = 2)) +
    ggtitle("External Records Request Status") #+ 
  #  labs(subtitle = "Consented, Enrolled",
  #    fill = "edu") 
  gpEdu
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpRace  
  ggsave("plots/ERreqC.png", gpEdu, height = 9, width = 16, dpi = 600)
  
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpFlag  
  return(gpEdu)  
  
}


getERrecpieComb <- function(df.enroll){
  
  df.phoneConsultRace <- df.enroll |>
    select(-ID) |>
    filter(ExternalRecordsRequestStatus == "Received" ) |>
    group_by(ExternalRecordsDataEntryStatus) |>
    summarize(n = n()) |> 
    ungroup() |> 
    mutate(pct = round(n/sum(n), 3))
  
  levelsRace <- df.phoneConsultRace|>
    group_by(ExternalRecordsDataEntryStatus) |>
    summarize(n = n()) |> 
    arrange(-n) |>
    pull(ExternalRecordsDataEntryStatus)
  
  df.phoneConsultRace$ExternalRecordsDataEntryStatus <- factor(df.phoneConsultRace$ExternalRecordsDataEntryStatus, levelsRace)
  
  df2Race <- df.phoneConsultRace |> 
    # dplyr::group_by(RS3) |>
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/10 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", n, "\n", round(pct*100, 1), "%"))
  
  df3Race <- df.phoneConsultRace |>
    dplyr::summarize(n = paste0("N=", sum(n)))
  
  gpEdu <- ggplot(df.phoneConsultRace, aes(x = "", y = pct, fill = fct_inorder(ExternalRecordsDataEntryStatus))) +
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
          plot.subtitle = element_text(hjust = 0.5)) +
    guides(fill = guide_legend(ncol = 2)) +
    ggtitle("External Records Entry Status") #+ 
  #  labs(subtitle = "Consented, Enrolled",
  #    fill = "edu") 
  gpEdu
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpRace  
  ggsave("plots/ERreqC.png", gpEdu, height = 9, width = 16, dpi = 600)
  
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpFlag  
  return(gpEdu)  
  
}

