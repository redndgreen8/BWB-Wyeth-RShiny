

source("str_list.R")
#source("BCSB_utils.R")
library(ggplot2)
library(ggrepel)
getEnrollment <- function(enroll_str) {
  
  #df <- read.csv("HS2100716BodourSalhi-EnrollmentSummary_DATA_LABELS_2024-03-22_1440.csv")
  df <- read.csv(enroll_str)
  df[df == ""] <-NA
  names(df) <- gsub("[[:punct:] ]", "", names(df))
  names(df)[1] <- "ID"
  
  
  df.enroll <- select(df, ID,  Clinic, ConsentSigned, SurveyStatus, BloodDrawStatus, Whatisthehighestlevelofeducationyouhavereceived, WhatisyourracePleasemarkallthatapply, WhatisyourRaceEthnicity, DateofWebsiteEligibilitySurvey, ExternalRecordsRequestStatus, ExternalRecordsDataEntryStatus)
  
  df.enroll <- df.enroll |>
    mutate(
      WhatisyourRaceEthnicity = ifelse( WhatisyourRaceEthnicity == "Hispanic or Latinx", "Hispanic or Latino", WhatisyourRaceEthnicity ),
      WhatisyourRaceEthnicity = ifelse( WhatisyourRaceEthnicity == "Native Hawaiian or Pacific Islander", "Native Hawaiian or other Pacific Islander", WhatisyourRaceEthnicity ),
      WhatisyourRaceEthnicity = ifelse( WhatisyourRaceEthnicity == "Prefer not to answer", "I prefer Not to Answer", WhatisyourRaceEthnicity ),
      
      WhatisyourracePleasemarkallthatapply = ifelse( is.na(WhatisyourracePleasemarkallthatapply), WhatisyourRaceEthnicity, WhatisyourracePleasemarkallthatapply),
      #Race = ifelse( WhatisyourracePleasemarkallthatapply == "", WhatisyourRaceEthnicity, WhatisyourracePleasemarkallthatapply),
      Race = ifelse( WhatisyourracePleasemarkallthatapply == "", "No race indicated", WhatisyourracePleasemarkallthatapply),
      
      Race = ifelse(Race %in% "I prefer Not to Answer", 
                    "No race indicated", Race),
      Race = ifelse( grepl(",", Race) , "Multiple", Race  ) ,
      location = ifelse(!is.na(DateofWebsiteEligibilitySurvey)  ,
                        "Web", as.character(Clinic)),
      edu = ifelse(is.na(Whatisthehighestlevelofeducationyouhavereceived), 
                   "NA", Whatisthehighestlevelofeducationyouhavereceived),
      survey = ifelse(SurveyStatus == "Completed", SurveyStatus, "Incomplete")) |>
    select(-WhatisyourracePleasemarkallthatapply, -Clinic, -Whatisthehighestlevelofeducationyouhavereceived, -DateofWebsiteEligibilitySurvey) |>
    filter(ConsentSigned == "Yes") |>
    unique()
  
  return(df.enroll)
}

df.enroll <- getEnrollment(enroll_str)

getERacepie <- function(df.enroll){
  dfPCR <- df.enroll %>%
    #dplyr::filter(SurveyStatus != "Survey Link Emailed (remote)") %>%
    #dplyr::filter(SurveyStatus !=   "Scheduled (if in-person)") %>%
    #dplyr::mutate(SurveyStatus = ifelse(SurveyStatus == "Survey Link Emailed (remote)", "Pending", SurveyStatus )) %>%
    #dplyr::mutate(SurveyStatus = ifelse(SurveyStatus == "Scheduled (if in-person)", "Pending", SurveyStatus )) %>%
    #dplyr::mutate(SurveyStatus =  ifelse(SurveyStatus == "Completed", "Completed", SurveyStatus)) %>%
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
  
  gpPCE <- ggplot(dfPCR, aes(x = "" , y = pct, fill = Race)) +
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
    ggtitle("Race breakdown Based on location of approach, (Consent Signed)")  #+
  #    labs(subtitle = "NAs due to incomplete baseline survey")
  # gpPCE
  ggsave("plots/ER.png", gpPCE, height = 9, width = 16, dpi = 600)
  
  return(gpPCE)
  
}

getERacepieStacked <- function(df.enroll){
  dfPCR <- df.enroll %>%
    dplyr::count(location, Race) %>%
    dplyr::group_by(location) %>%
    dplyr::mutate(total = sum(n)) %>%
    dplyr::mutate(pct = n / total) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(location, Race) %>%
    dplyr::filter(!is.na(location)) 
  
  # For pie charts
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
  
  # Create total counts for stacked bar - combined data
  dfTotal <- dfPCR %>%
    dplyr::group_by(Race) %>%
    dplyr::summarize(n = sum(n)) %>%
    dplyr::mutate(total = sum(n),
                  pct = n / total,
                  location = "Combined") %>%
    dplyr::ungroup()
  
  # Combine with original data for plotting
  dfAll <- bind_rows(dfPCR, dfTotal)
  
  # Create plot grob for pie charts
  # First, create a custom labeller function that includes the total N
  location_labels <- function(value) {
    totals <- setNames(
      c(paste0("Clinic\nN=", sum(dfPCR$n[dfPCR$location == "Clinic"])), 
        paste0("Web\nN=", sum(dfPCR$n[dfPCR$location == "Web"]))),
      c("Clinic", "Web")
    )
    return(totals[value])
  }
  
  # Use the custom labeller in your plot
  p1 <- ggplot(dfPCR, aes(x = "" , y = pct, fill = Race)) +
    geom_col(width = 1, color = "black", size = 0.5) +
    facet_grid(cols = vars(location), labeller = labeller(location = location_labels)) +
    coord_polar(theta = "y") +
    scale_fill_brewer(palette = "Set3") +
    geom_label_repel(data = df2PCR,
                     aes(y = pos, label = label),
                     size = 3.75, 
                     nudge_x = 1,
                     show.legend = FALSE, 
                     label.padding = unit(0.75, "mm")) +
    # Remove the original N count text since we now display it in the facet label
    # geom_text(data = df3PCR, x = -1.15, y = 0, aes(label = n), 
    #          colour = "black", inherit.aes = FALSE, parse = FALSE, size = 8) + 
    theme_DB() +
    ylab("") + 
    xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          # Axis labels (x and y labels)
        #  axis.title.x = element_text(size = 22, face = "bold", margin = margin(t = 20)),
        #  axis.title.y = element_text(size = 22, face = "bold", margin = margin(r = 20)),
          
          # Axis text (the values on the axes)
         # axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        #  axis.text.y = element_text(size = 18),
          
          # Legend elements (you've already set these, but adjusted for consistency)
          legend.title = element_text(face = "bold", size = 20),
          legend.text = element_text(size = 19),

          # Plot title
          #plot.title = element_text(face = "bold", size = 24, hjust = 0.5, margin = margin(b = 20)),
          
          
          panel.grid.major.y = element_blank(),
          strip.text = element_text(size = 15, face = "bold"),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))
  ggsave("plots/p1_combined.png", p1, height = 9, width = 18, dpi = 600)
  
  p1# Create stacked bar chart for Combined data
  
  
  
  
  
  p2 <- ggplot(dfTotal, aes(x = "Combined", y = n, fill = Race)) +
    geom_bar(stat = "identity", color = "black", width = 0.4, size = 0.5) + # Reduced width
    scale_fill_brewer(palette = "Set3") + # Keeps original colors
    # Create a named vector for the legend labels
    scale_fill_manual(
      values = scale_fill_brewer(palette = "Set3", type = "qual", aesthetics = "fill")$palette(length(unique(dfTotal$Race))),
      labels = function(x) {
        sapply(x, function(race) {
          row <- dfTotal[dfTotal$Race == race, ]
          paste0(race, " (", round(row$pct*100, 1), "%, N=", row$n, ")")
        })
      }
    ) +
    guides(fill = guide_legend(title = "Race", 
                               title.theme = element_text(size = 35, face = "bold"),
                               label.theme = element_text(size = 33))) +
    theme_DB() +
    ylab("Number of Participants") + 
    xlab("") +
    coord_flip() +
    # Reduce plot margins to decrease vertical footprint
    theme(#axis.text.x = element_text(size = 10),
          #axis.text.y = element_text(size = 12, face = "bold"),
          # Axis labels (x and y labels)
          axis.title.x = element_text(size = 35, face = "bold", margin = margin(t = 20)),
          axis.title.y = element_text(size = 35, face = "bold", margin = margin(r = 20)),
          
          # Axis text (the values on the axes)
          axis.text.x = element_text( hjust = 1, size = 35),
          axis.text.y = element_text(size = 35),
          
          # Legend elements (you've already set these, but adjusted for consistency)
          legend.title = element_text(face = "bold", size = 20),
          legend.text = element_text(size = 35),

          # Plot title
          
          legend.position = "right",
          legend.key.size = unit(0.8, "cm"),
          plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"))
  ggsave("plots/p2_combined.png", p2, height = 9, width = 27, dpi = 600)
  p2
  # Total count label for stacked bar
  #totalLabel <- data.frame(
  #  x = "Combined", 
  #  y = sum(dfTotal$n) * 1.05, 
  #  label = paste0("Total N=", sum(dfTotal$n))
  #)
  
  #p2 <- p2 + geom_text(data = totalLabel, aes(x = x, y = y, label = label),
              #         inherit.aes = FALSE, size = 6, fontface = "bold")
  
  #p1_no_legend <- p1 + theme(legend.position = "none")
  #p2_no_legend <- p2 + theme(legend.position = "none")
  
  #legend_plot <- ggplot(dfPCR, aes(x = Race, y = n, fill = Race)) + 
  #  geom_col() +
  #  scale_fill_brewer(palette = "Set3") +
  #  guides(fill = guide_legend(nrow = 1)) +
  #  theme(legend.position = "bottom",
  #        legend.title = element_blank(),
  #        legend.text = element_text(size = 15, face = "bold"))
  
  # Extract just the legend from this separate plot
  #legend_obj <- cowplot::get_legend(legend_plot)
  
  # Combine plots with patchwork (both without legends)
  plots_combined <- p2 / p1 + 
    plot_layout(heights = c(1, 1)) +
    plot_annotation(
      title = "Race Demographics",
      subtitle = paste0("Total Participants: N=", sum(dfTotal$n)),
      theme = theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5)
      )
    )
  plots_combined
  # Add the shared legend and title
  #combined_plot <- plots_combined / 
   # patchwork::wrap_elements(legend_obj) + 
  #  plot_layout(heights = c(10, 1)) +
  #  plot_annotation(
  #    title = "Race Demographics",
  #    theme = theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
  #  )
  #combined_plot
  
  # Save the combined plot
  ggsave("plots/ER_combined.png", plots_combined, height = 9, width = 18, dpi = 600)
  
  return(plots_combined)
}
ER_clinCombStacked <- function(df.enroll){
  df.enroll <- df.enroll %>%
    mutate(location = ifelse( location == "Web", location, "Clinic"))
 return(getERacepieStacked(df.enroll))
  
}

ER_clinCombStacked(df.enroll )

getERacepieDot <- function(df.enroll){
  dfPCR <- df.enroll %>%
    dplyr::count(location, Race) %>%
    dplyr::group_by(location) %>%
    dplyr::mutate(total = sum(n)) %>%
    dplyr::mutate(pct = n / total) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(location, Race) %>%
    dplyr::filter(!is.na(location)) 
  
  # For pie charts
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
  
  # Create total counts for combined data
  dfCombined <- dfPCR %>%
    dplyr::group_by(Race) %>%
    dplyr::summarize(n = sum(n)) %>%
    dplyr::mutate(total = sum(n),
                  pct = n / total) %>%
    # Sort by count for better visualization
    dplyr::arrange(desc(n)) %>%
    # Add row number for y-position in dot plot
    dplyr::mutate(y_pos = row_number())
  
  # Create plot grob for pie charts
  p1 <- ggplot(dfPCR, aes(x = "" , y = pct, fill = Race)) +
    geom_col(width = 1, color = "black", size = 0.5) +
    facet_grid(cols = vars(location)) +
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
          legend.position = "none",
          plot.title = element_text(hjust = 0.5))
  
  # Create Cleveland dot plot for combined data
  p2 <- ggplot(dfCombined, aes(x = n, y = y_pos, color = Race)) +
    # Add segments from y-axis to points
    geom_segment(aes(x = 0, xend = n, y = y_pos, yend = y_pos),
                 color = "gray70", size = 0.75) +
    # Add points
    geom_point(size = 5, aes(fill = Race), shape = 21, color = "black", stroke = 0.5) +
    # Use same color palette as pie charts for consistency
    scale_fill_brewer(palette = "Set3") +
    scale_color_brewer(palette = "Set3") +
    # Expand x-axis slightly for labels
    scale_x_continuous(expand = expansion(mult = c(0.2, 0.2))) +
    # Add race labels on the left
    geom_text(aes(x = -max(n)*0.05, label = Race), 
              hjust = 1, size = 4.5, fontface = "bold") +
    # Add count and percentage labels to the right of points
    geom_text(aes(x = n + max(n)*0.05, 
                  label = paste0("N=", n, " (", round(pct*100, 1), "%)")),
              hjust = 0, size = 4.5) +
    # Add total
    annotate("text", x = max(dfCombined$n)/2, y = max(dfCombined$y_pos) + 1.5,
             label = paste0("Total N=", sum(dfCombined$n)),
             size = 6, fontface = "bold") +
    # Customize theme
    theme_DB() +
    labs(title = "Combined") +
    xlab("Number of Participants") +
    ylab("") +
    # Remove y-axis elements as we're using direct labels
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))
  
  # Create a separate plot just for the legend
  legend_plot <- ggplot(dfPCR, aes(fill = Race)) + 
    geom_bar(aes(x = Race, y = n), stat = "identity") +
    scale_fill_brewer(palette = "Set3") +
    guides(fill = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 15, face = "bold"))
  
  # Extract just the legend
  legend_obj <- cowplot::get_legend(legend_plot)
  
  # Combine plots with patchwork - pie charts in row 1, dot plot in row 2
  combined_plot <- (p1 / p2 / patchwork::wrap_elements(legend_obj)) + 
    plot_layout(heights = c(1, 0.7, 0.2)) +
    plot_annotation(
      title = "Race Demographics",
      theme = theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
    )
  # Save the combined plot
  ggsave("plots/ER_combined.png", combined_plot, height = 12, width = 16, dpi = 600)
  
  return(combined_plot)
}

ER_clinCombDot <- function(df.enroll){
  df.enroll <- df.enroll %>%
    mutate(location = ifelse( location == "Web", location, "Clinic"))
  return(getERacepieDot(df.enroll))
  
}
ER_clinCombDot(df.enroll )


ER_clinComb <- function(df.enroll){
  df.enroll <- df.enroll %>%
    mutate(location = ifelse( location == "Web", location, "Clinic"))
  return(getERacepie(df.enroll))
  
}
ER_clinComb(df.enroll )


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

#enrolled[["Race"]] <- ifelse(grepl(",", enrolled[["Race"]]), "Multiple", enrolled[["Race"]])
getERacepieComb <- function(df.enroll){
  
  df.phoneConsultRace <- df.enroll |>
    select(-ID) |>
    #filter(SurveyStatus != "Survey Link Emailed (remote)") |>
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
  
  gpEdu <- ggplot(df.phoneConsultRace, aes(x = "", y = pct, fill = fct_inorder(Race))) +
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
    ggtitle("Race Breakdown, (Consent Signed)") #+ 
  #  labs(subtitle = "Consented, Enrolled",
  #    fill = "edu") 
  # gpEdu
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpRace 
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
    ggtitle("Racial Demographics")
  ggsave("plots/ERC.png", gpEdu2, height = 9, width = 16, dpi = 600)
  
  #ggsave("plots/Race_demog.png", gp, height = 9, width = 16, dpi = 600)
  #gpFlag  
  return(gpEdu)  
  
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
  ggsave("plots/EBC.png", gpEdu2, height = 9, width = 16, dpi = 600)
  
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

