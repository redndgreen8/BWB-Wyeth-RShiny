



source("str_list.R")






getPCRenrollpie <- function(df.phoneConsult){
  dfPCR <- df.phoneConsult %>%
    dplyr::filter(Dispositionflag =="Enrolled") %>%
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
    ggtitle("Race Breakdown Based on location of approach (Enrolled)")  
  ggsave("plots/PCRenroll.png", gpPCR, height = 9, width = 16, dpi = 600)
  
  return(gpPCR)
  
}

enroll_pie <- getPCRenrollpie(df.phoneConsult)
enroll_pie


getPCRenrollCOMBpie <- function(df.phoneConsult){
  
  df.phoneConsultRace <- df.phoneConsult |>
    select(-ID) |>
    filter(Dispositionflag == "Enrolled") |>
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
    ggtitle("Race Demographics of those 'Enrolled'") + 
    labs(subtitle = "This includes participants from the Web and Clinics.",
         fill = "Race") 
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpRace  
  ggsave("plots/PCRCenrollr.png", gpRace, height = 9, width = 16, dpi = 600)
  
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpFlag  
  return(gpRace)  
  
}

enroll_pieC <- getPCRenrollCOMBpie(df.phoneConsult)
enroll_pieC