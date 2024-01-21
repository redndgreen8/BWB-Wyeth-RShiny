library(ggplot2)
library(tidyverse)
library(ggrepel)
source("theme_DB.R")

.dir <- "~/Documents/" 

getClinDatSimple <- function(clin_str) {
  cd <- read.csv(paste0( clin_str))
  cd[cd == ""] <- NA
  cd$RS2 <- strsplit(cd$RS, ", ") |> 
    purrr::map(function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) return(NA)
      x <- trimws(x)
      x <- x[order(x)]
      return(paste0(x, collapse = ", "))
    }) |> unlist()
  
  cd <- split(cd, cd$StudyID)
  cd.u <- mclapply(cd, function(x) {
    z <- lapply(x, function(y) {
      y <- trimws(y)
      y <- y[!is.na(y)]
      if (length(y) == 0) return(NA)
      u <- unique(y)
      if (length(u) == 1) return(u)
      else return(paste0(u, collapse = "; "))
    }) |> 
      dplyr::bind_cols() |> 
      dplyr::mutate(SurgicalHistoryNumber = as.character(SurgicalHistoryNumber), 
                    SurgicalTumorSize = as.character(SurgicalTumorSize),
                    SurgicalMargins = as.character(SurgicalMargins),
                    TumorGrade = as.character(TumorGrade),
                    Ki67Value = as.character(Ki67Value),
                    ERPositiveCells = as.character(ERPositiveCells),
                    PRPositiveCells = as.character(PRPositiveCells),
                    ImagingDetailNumber = as.character(ImagingDetailNumber),
                    TumorSizeAtDiagnosis = as.character(TumorSizeAtDiagnosis),
                    BiopsyTissueSize = as.character(BiopsyTissueSize),
                    TumorSizeAtBiopsy = as.character(TumorSizeAtBiopsy),
                    ERStainingIntensity = as.character(ERStainingIntensity),
                    PRStainingIntensity = as.character(PRStainingIntensity))
    return(z)
  }, mc.cores = 8) |> dplyr::bind_rows()
  return(cd.u)
}



clindat <- getClinDatSimple("Master_list/MasterList_1.17.24.csv") |> 
  dplyr::mutate(date.Dx = as.Date(gsub("; .*", "", BreastCancerDiagnosisDate),
                                  tryFormats = "%m/%d/%Y"))


df.missing <- data.frame(ID = clindat$StudyID,
                         missing = rowSums(is.na(clindat))) |> 
  dplyr::mutate(all.missing = missing == ncol(clindat)-1,
                lab = ifelse(all.missing, "NO CLINICAL INFO", "CLINICAL INFO OBTAINED"))



if (1) {
  df <- df.missing |> 
    # dplyr::group_by(lab) |> 
    dplyr::reframe(count = as.numeric(table(lab)),
                   lab = names(table(lab)),
                   total = dplyr::n()) |>
    dplyr::mutate(pct = count/total,
                  lab = factor(lab, levels = c("NO CLINICAL INFO", "CLINICAL INFO OBTAINED"))) |> 
    dplyr::arrange(lab)
  
  df2 <- df |> 
    # dplyr::group_by(lab) |>
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/2 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", count, "\n", round(pct*100, 1), "%")) 
  
  df3 <- df |> 
    # dplyr::group_by(diagnosis) |> 
    dplyr::summarize(n = paste0("N=", sum(count)))
  
  gp <- ggplot(df, aes(x = "" , y = pct, fill = fct_inorder(lab))) +
    geom_col(width = 1, color = 1, size = 0.5) +
    # facet_grid(cols = vars(diagnosis)) +
    coord_polar(theta = "y") +
    scale_fill_brewer(palette = "Set3") +
    geom_label_repel(data = df2,
                     aes(y = pos, label = label),
                     size = 3.75, 
                     nudge_x = 1,
                     show.legend = FALSE, 
                     label.padding = unit(0.75, "mm")) +
    geom_text(data=df3, x = -1.15, y = 0, aes(label = n), 
              colour = "black", inherit.aes = F, parse = F, size = 8) + 
    theme_DB() +
    ylab("") + 
    xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.y = element_blank(),
          strip.text = element_text(size = 15, face = "bold"),
          legend.text = element_text(size = 15, face = "bold"),
          legend.title = element_blank()) +
    guides(fill = guide_legend(nrow = 2, byrow = T))
  ggsave("clinical_info_plots/frac_with_any_clinical_data.png", gp, height = 6, width = 6)
}

if (1) {
  
  df <- clindat %>%
    mutate(RS2 = ifelse(is.na(RS2), "UNKNOWN", RS2),
           RS2 = ifelse(grepl(";", RS2), "MULTIFOCAL", RS2)) %>%
    select(StudyID, RS2) %>%
    mutate(
      ER_Pos = grepl("ER\\+", RS2),
      PR_Pos = grepl("PR\\+", RS2),
      HER2_Pos = grepl("HER2\\+", RS2),
      ER_Neg = grepl("ER\\-", RS2),
      PR_Neg = grepl("PR\\-", RS2),
      HER2_Neg = grepl("HER2\\-", RS2)
    ) %>%
    mutate(RS3 = case_when(
      ER_Pos & PR_Pos & HER2_Pos ~ "Triple Positive",
      ER_Neg & PR_Neg & HER2_Neg ~ "Triple Negative",
      (ER_Pos | PR_Pos) & HER2_Neg ~ "Hormone Receptor +",
      ER_Neg & PR_Neg & HER2_Pos ~ "HER2 +",
      RS2 == "MULTIFOCAL" ~ "MULTIFOCAL",
      TRUE ~ "UNKNOWN"
    )) %>%
    filter(RS3 != "UNKNOWN") %>%
    group_by(RS3) %>%
    summarize(count = n(), .groups = 'drop') %>%
    mutate(pct = count/sum(count)) %>%
    arrange(RS3)

    multi2 <- df %>%
    filter(RS3 == "MULTIFOCAL")
  
  
  df2 <- df |> 
    # dplyr::group_by(RS3) |>
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/2 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", count, "\n", round(pct*100, 1), "%")) 
  
  df3 <- df |> 
    # dplyr::group_by(diagnosis) |> 
    dplyr::summarize(n = paste0("N=", sum(count)))
  
  gp <- ggplot(df, aes(x = "" , y = pct, fill = fct_inorder(RS3))) +
    geom_col(width = 1, color = 1, linewidth = 0.5) +
    # facet_grid(cols = vars(diagnosis)) +
    coord_polar(theta = "y") +
    scale_fill_brewer(palette = "Set3") +
    geom_label_repel(data = df2,
                     aes(y = pos, label = label),
                     size = 3.75, 
                     nudge_x = 1,
                     show.legend = FALSE, 
                     label.padding = unit(0.75, "mm")) +
    geom_text(data=df3, x = -1.15, y = 0, aes(label = n), 
              colour = "black", inherit.aes = F, parse = F, size = 8) + 
    theme_DB() +
    ylab("") + 
    xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.y = element_blank(),
          strip.text = element_text(size = 15, face = "bold"),
          legend.text = element_text(size = 15, face = "bold"),
          legend.title = element_blank()) +
    guides(fill = guide_legend(nrow = 3, byrow = T))
  ggsave("clinical_info_plots/RS_pie_chart.png", gp, height = 7, width = 7)
}

df.DxDate <- clindat |> 
  select(StudyID, date.Dx) |> 
  mutate(Days.since.Dx = as.Date("2023-10-01")-date.Dx,
         Years.since.Dx = as.numeric(floor(Days.since.Dx/365.25)),
         Years.since.Dx2 = ifelse(Years.since.Dx > 6, "7+", Years.since.Dx),
         lab = paste0(Years.since.Dx2, " YEARS"),
         lab = ifelse(grepl("^NA", lab), "UNKNOWN", lab)) |> 
  filter(lab != "UNKNOWN")

if (1) {
  df <- df.DxDate |> 
    # dplyr::group_by(lab) |> 
    dplyr::reframe(count = as.numeric(table(lab)),
                   lab = names(table(lab)),
                   total = dplyr::n()) |>
    dplyr::mutate(pct = count/total,
                  lab = factor(lab)) |> 
    dplyr::arrange(lab)
  
  df2 <- df |> 
    # dplyr::group_by(lab) |>
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/2 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", count, "\n", round(pct*100, 1), "%")) 
  
  df3 <- df |> 
    # dplyr::group_by(diagnosis) |> 
    dplyr::summarize(n = paste0("N=", sum(count)))
  
  gp <- ggplot(df, aes(x = "" , y = pct, fill = fct_inorder(lab))) +
    geom_col(width = 1, color = 1, size = 0.5) +
    # facet_grid(cols = vars(diagnosis)) +
    coord_polar(theta = "y") +
    scale_fill_brewer(palette = "Set3") +
    geom_label_repel(data = df2,
                     aes(y = pos, label = label),
                     size = 3.75, 
                     nudge_x = 1,
                     show.legend = FALSE, 
                     label.padding = unit(0.75, "mm")) +
    geom_text(data=df3, x = -1.15, y = 0, aes(label = n), 
              colour = "black", inherit.aes = F, parse = F, size = 8) + 
    theme_DB() +
    ylab("") + 
    xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.y = element_blank(),
          strip.text = element_text(size = 15, face = "bold"),
          legend.text = element_text(size = 15, face = "bold"),
          legend.title = element_blank()) +
    guides(fill = guide_legend(nrow = 2, byrow = T))
 # ggsave("clinical_info_plots/diagnosis_date_pie_chart.png", gp, height = 6, width = 6)
  
  yearBreaks <- seq(0, 1e5, 365.25)
  names(yearBreaks) <- paste0(1:length(yearBreaks), "Y")
  gp <- ggplot(df.DxDate, aes(x = Days.since.Dx)) +
    geom_histogram(binwidth = 100, fill = "lightgray", color = "black") +
    ylab("Number of Patients") +
    xlab("Years Since Diagnosis") +
    scale_x_continuous(breaks = yearBreaks, labels = names(yearBreaks)) +
    theme_DB(bold.axis.title = T, grid.x = T, rotate.x = T) 
#  ggsave("clinical_info_plots/diagnosis_date_histogram.png", gp, height = 6, width = 9)
}

if (1) {
  files <- list.files("demographics/", "DATA_LABELS.*csv$", recursive = T, full.names = T)
  files <- files[!grepl("Nicotine History|date of diagnosis", files)]
  df <- lapply(files, function(f) {
    message(f)
    d <- read.csv(f) 
    d <- d |> 
      select(-contains("OTHER.POSSIBLE.CATEGORIES.TO.CONSIDER"),
             -contains("Other.specify"),
             -matches("^X\\.[0-9]{1,2}$"),
             -matches("^Complete\\.$"),
             -matches("^X$")) |> 
      mutate(sheet = gsub("\\/.*", "", gsub(".*current\\/", "", f))) |> 
      select(-1) |>
      reshape2::melt(id.vars = "sheet") 
    return(d)
  }) |> bind_rows()
  
  df$value[grepl("choice.Prefer.not.to.answer", df$variable, ignore.case = T) & 
             grepl("checked", df$value, ignore.case = T)] <- 
    "I prefer not to answer"
  
  df <- df |>
    dplyr::mutate(class = ifelse(is.na(value) | value %in% "", "MISSING",
                                 ifelse(value == "I prefer not to answer", "PREFER NOT TO ANSWER", "ANSWERED"))) |> 
    # dplyr::group_by(RS3) |> 
    dplyr::reframe(count = as.numeric(table(class)),
                   class = names(table(class)),
                   total = dplyr::n()) |>
    dplyr::mutate(pct = count/total,
                  class = factor(class)) |> 
    dplyr::arrange(class)
  
  df2 <- df |> 
    # dplyr::group_by(class) |>
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/2 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", count, "\n", round(pct*100, 1), "%")) 
  
  df3 <- df |> 
    # dplyr::group_by(diagnosis) |> 
    dplyr::summarize(n = paste0("N=", sum(count)))
  
  gp <- ggplot(df, aes(x = "" , y = pct, fill = fct_inorder(class))) +
    geom_col(width = 1, color = 1, size = 0.5) +
    # facet_grid(cols = vars(diagnosis)) +
    coord_polar(theta = "y") +
    scale_fill_brewer(palette = "Set3") +
    geom_label_repel(data = df2,
                     aes(y = pos, label = label),
                     size = 3.75, 
                     nudge_x = 1,
                     show.legend = FALSE, 
                     label.padding = unit(0.75, "mm")) +
    geom_text(data=df3, x = -1.15, y = 0, aes(label = n), 
              colour = "black", inherit.aes = F, parse = F, size = 8) + 
    theme_DB() +
    ylab("") + 
    xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.y = element_blank(),
          strip.text = element_text(size = 15, face = "bold"),
          legend.text = element_text(size = 15, face = "bold"),
          legend.title = element_blank()) +
    guides(fill = guide_legend(nrow = 2, byrow = T))
 # ggsave("clinical_info_plots/survey_missing_answers_pie.png", gp, height = 6, width = 6)
}








