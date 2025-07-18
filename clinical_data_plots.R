library(ggplot2)
library(tidyverse)
library(ggrepel)
source("theme_DB.R")
source("str_list.R")

.dir <- "~/Documents/" 


getClinDatSimple <- function(master_str) {
  cd <- read.csv(paste0( master_str))
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
#clin_str <- "MasterList_Clinical Information_3.19.24.csv"
#clind <- getClinDatSimple(clin_str)
#df.DxDate <- clind %>% 
 # select(StudyID, date.Dx) %>% 
  #mutate(Days.since.Dx = as.Date("2023-10-01")-date.Dx,
   #      Years.since.Dx = as.numeric(floor(Days.since.Dx/365.25)),
    #     Years.since.Dx2 = ifelse(Years.since.Dx > 6, "7+", 
     #                             Years.since.Dx),
      #   lab = paste0(Years.since.Dx2, " YEARS"),
       #  lab = ifelse(grepl("^NA", lab), "UNKNOWN", lab)) %>% 
  #filter(lab != "UNKNOWN")

#missing_clin <- getClinMissing(clind)


clind <- getClinDatSimple(master_str)


getclind2 <- function(clind){
  clind2 <- clind %>%
    mutate(RS2 = ifelse(is.na(RS2), "UNKNOWN", RS2),
           RS2 = ifelse(grepl(";", RS2), "MULTIFOCAL", RS2),
           ER_Pos = grepl("ER\\+", RS2),
           PR_Pos = grepl("PR\\+", RS2),
           HER2_Pos = grepl("HER2\\+", RS2),
           ER_Neg = grepl("ER\\-", RS2),
           PR_Neg = grepl("PR\\-", RS2),
           HER2_Neg = grepl("HER2\\-", RS2),
           RS3 = case_when(
             ER_Pos & PR_Pos & HER2_Pos ~ "Triple Positive",
             (ER_Pos | PR_Pos) & HER2_Pos ~ "Triple Positive",      
             ER_Neg & PR_Neg & HER2_Neg ~ "Triple Negative",
             (ER_Pos | PR_Pos) & HER2_Neg ~ "HR+/HER2-",
             ER_Neg & PR_Neg & HER2_Pos ~ "HR-/HER2+",
             RS2 == "MULTIFOCAL" ~ "MULTIFOCAL",
             TRUE ~ "UNKNOWN"
           )) 
  
  clind2 <- clind2 %>%
    mutate(BreastCancerDiagnosisDate = str_extract_all(BreastCancerDiagnosisDate, "\\d{1,2}/\\d{1,2}/\\d{4}") %>%
             lapply(function(dates) {
               if (length(dates) == 0) return(NA)
               parsed_dates <- mdy(dates)
               if (all(is.na(parsed_dates))) return(NA)
               format(max(parsed_dates, na.rm = TRUE), "%Y-%m-%d")
             }) %>%
             unlist())
  
  return(clind2)
}

getClinMissing <-function(clind) {
  
  df.missing <- data.frame(ID = clind$StudyID,
                           missing = rowSums(is.na(clind))) |> 
    dplyr::mutate(all.missing = missing == ncol(clind)-1,
                  lab = ifelse(all.missing, "NO CLINICAL INFO", 
                               "CLINICAL INFO OBTAINED"))
  df <- df.missing |> 
    # dplyr::group_by(lab) |> 
    dplyr::reframe(count = as.numeric(table(lab)),
                   lab = names(table(lab)),
                   total = dplyr::n()) |>
    dplyr::mutate(pct = count/total,
                  lab = factor(lab, levels = c("NO CLINICAL INFO", 
                                               "CLINICAL INFO OBTAINED"))) |> 
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
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),  
          plot.subtitle = element_text(hjust = 0.5)) +
    guides(fill = guide_legend(nrow = 2, byrow = T)) +
    ggtitle("Clinical information entered for participants consented and active on study.") +   #
    labs(subtitle = "There is a natural backlog that occurs due to the nature of the OSH record 
         request process through record review and data entry into the CRF.",
        caption ="***N's from Subtype and Clinical Info might not match 
         since some records have partial information entered.(i.e. No Subtype info)")
  return(list(gp=gp,miss=df.missing))
#  ggsave("plots/clinical_info_plots/frac_with_any_clinical_data.png",
  #gp, height = 6, width = 6)
}

getClinPie <- function(clind) {
  
  df <- clind %>%
    mutate(RS2 = ifelse(is.na(RS2), "UNKNOWN", RS2),
           RS2 = ifelse(grepl(";", RS2), "MULTIFOCAL", RS2))
    
  df <- df %>%  
    select(StudyID, RS2)
  
  df <- df %>%
    mutate(
      ER_Pos = grepl("ER\\+", RS2),
      PR_Pos = grepl("PR\\+", RS2),
      HER2_Pos = grepl("HER2\\+", RS2),
      ER_Neg = grepl("ER\\-", RS2),
      PR_Neg = grepl("PR\\-", RS2),
      HER2_Neg = grepl("HER2\\-", RS2))
  
  df <- df %>%
    mutate(RS3 = case_when(
      ER_Pos & PR_Pos & HER2_Pos ~ "Triple Positive",
      (ER_Pos | PR_Pos) & HER2_Pos ~ "Triple Positive",      
      ER_Neg & PR_Neg & HER2_Neg ~ "Triple Negative",
      (ER_Pos | PR_Pos) & HER2_Neg ~ "HR+/HER2-",
      ER_Neg & PR_Neg & HER2_Pos ~ "HR-/HER2+",
      RS2 == "MULTIFOCAL" ~ "MULTIFOCAL",
      TRUE ~ "UNKNOWN"
    )) 
  
  
  df <- df %>%
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
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 25),  
          plot.subtitle = element_text(hjust = 0.5, size = 15)) +
    guides(fill = guide_legend(nrow = 3, byrow = T)) +
    ggtitle("Subtypes for participants that have completed Baseline Session") +   #
    labs(subtitle = "Multifocal - participants that had multiple tumors 
    with different subtypes
         HR - Hormone Receptor (ER/PR)")  #
  ggsave("plots/RS_pie_chart.png", gp, height = 9, width = 16, dpi = 600)
  return(gp)
}




getEarliestConsentDate <- function(dx_str){
  cd <- read.csv(paste0( dx_str))
  cd[cd == ""] <- NA
  # Process cd (file_df)
  na_rows <- cd %>% filter(is.na(dateconsentsignedbypt) & 
                             is.na(dateconsentsignedbypt_v2) & 
                             is.na(dateconsentsignedbypt_v2_v3) & 
                             is.na(dateconsentsignedbypt_v4) & 
                             is.na(dateconsentsignedbypt_v2sp) & 
                             is.na(dateconsentsignedbypt_ch_v1) & 
                             is.na(dateconsentsignedbypt_ko_v1) &
                             is.na(dtconsentsignedbypt_sp_v2))
  df.DxDa <- cd %>%
    mutate(
      dateconsentsignedbypt = as.Date(dateconsentsignedbypt),
      dateconsentsignedbypt_v2 = as.Date(dateconsentsignedbypt_v2),
      dateconsentsignedbypt_v2_v3 = as.Date(dateconsentsignedbypt_v2_v3),
      dateconsentsignedbypt_v4 = as.Date(dateconsentsignedbypt_v4),
      dateconsentsignedbypt_v2sp = as.Date(dateconsentsignedbypt_v2sp),
      dateconsentsignedbypt_ch_v1 = as.Date(dateconsentsignedbypt_ch_v1),
      dateconsentsignedbypt_ko_v1 = as.Date(dateconsentsignedbypt_ko_v1),
      dtconsentsignedbypt_sp_v2 = as.Date(dtconsentsignedbypt_sp_v2),
      EarliestConsentDate = pmin(dateconsentsignedbypt, dateconsentsignedbypt_v2, 
                                 dateconsentsignedbypt_v2_v3, dateconsentsignedbypt_v4,
                                 dateconsentsignedbypt_v2sp,dtconsentsignedbypt_sp_v2,
                                 dateconsentsignedbypt_ch_v1, dateconsentsignedbypt_ko_v1,
                                 na.rm = TRUE),
      breastcancerdiagdate = as.Date(breastcancerdiagdate)
    ) %>%
    filter(!is.na(EarliestConsentDate))
  return(df.DxDa)
  
}
#Diag <- getEarliestConsentDate(dx_str)


getYrSinceDiagnosis <- function(dx_str, clind) {
  cd <- read.csv(paste0( dx_str))
  cd[cd == ""] <- NA
  # Process cd (file_df)
  na_rows <- cd %>% filter(is.na(dateconsentsignedbypt) & 
                             is.na(dateconsentsignedbypt_v2) & 
                             is.na(dateconsentsignedbypt_v2_v3) & 
                             is.na(dateconsentsignedbypt_v4) & 
                             is.na(dateconsentsignedbypt_v2sp) & 
                             is.na(dateconsentsignedbypt_ch_v1) & 
                             is.na(dateconsentsignedbypt_ko_v1) &
                             is.na(dtconsentsignedbypt_sp_v2))
  df.DxDa <- cd %>%
    mutate(
      dateconsentsignedbypt = as.Date(dateconsentsignedbypt),
      dateconsentsignedbypt_v2 = as.Date(dateconsentsignedbypt_v2),
      dateconsentsignedbypt_v2_v3 = as.Date(dateconsentsignedbypt_v2_v3),
      dateconsentsignedbypt_v4 = as.Date(dateconsentsignedbypt_v4),
      dateconsentsignedbypt_v2sp = as.Date(dateconsentsignedbypt_v2sp),
      dateconsentsignedbypt_ch_v1 = as.Date(dateconsentsignedbypt_ch_v1),
      dateconsentsignedbypt_ko_v1 = as.Date(dateconsentsignedbypt_ko_v1),
      dtconsentsignedbypt_sp_v2 = as.Date(dtconsentsignedbypt_sp_v2),
      EarliestConsentDate = pmin(dateconsentsignedbypt, dateconsentsignedbypt_v2, 
                                 dateconsentsignedbypt_v2_v3, dateconsentsignedbypt_v4,
                                 dateconsentsignedbypt_v2sp,dtconsentsignedbypt_sp_v2,
                                 dateconsentsignedbypt_ch_v1, dateconsentsignedbypt_ko_v1,
                                 na.rm = TRUE),
      breastcancerdiagdate = as.Date(breastcancerdiagdate)
    ) %>%
    filter(!is.na(EarliestConsentDate))
  
  # Process clindat (df)
  multiple_dates_df <- clind %>%
    select(StudyID, BreastCancerDiagnosisDate) %>%
    mutate(SplitDates = strsplit(BreastCancerDiagnosisDate, ";")) %>%
    filter(sapply(SplitDates, length) > 1)
  write.csv(multiple_dates_df$StudyID, file = "out/multiple_dx.txt", row.names = FALSE)
  
  clind <- clind %>%
    select(StudyID, BreastCancerDiagnosisDate) %>%
    mutate(SplitDates = strsplit(BreastCancerDiagnosisDate, ";")) %>%
    filter(sapply(SplitDates, length) <= 1)
  
  # Left join cd and clindat
  joined_df <- merge(df.DxDa, clind, by.x = "bcsbusername", by.y = "StudyID", all.x = TRUE)
  
#  write.csv(joined_df, file = "out/Dx_joined_Consent.txt", row.names = FALSE)
  
  
  
  # Address NA in diagnosisdate
  joined_df <- joined_df %>%
    mutate(BreastCancerDiagnosisDate = as.Date(BreastCancerDiagnosisDate, format = "%m/%d/%Y"),
      diagnosisdate = ifelse(is.na(BreastCancerDiagnosisDate), breastcancerdiagdate, BreastCancerDiagnosisDate),
      diagnosisdate = as.Date(diagnosisdate)) %>%
    filter(!is.na(diagnosisdate))
  
  # Calculate years since diagnosis
  joined_df <- joined_df %>%
    mutate(
      Days.since.Dx = EarliestConsentDate - diagnosisdate,
      Years.since.Dx = floor(Days.since.Dx / 365.25),
      Years.since.Dx2 = ifelse(Years.since.Dx > 6, "7+", as.character(Years.since.Dx)),
      lab = paste0(Years.since.Dx2, " YEARS")) %>%
  filter(!is.na(Years.since.Dx2) & Years.since.Dx2 != "NA YEARS")
  
  outlier_days_since_dx <- joined_df %>%
    filter(Days.since.Dx < 0 | Days.since.Dx >= 3652.5)
  write.csv(outlier_days_since_dx$bcsbusername, file = "out/outlierCase_dx.txt", row.names = FALSE)
  
  edgeCase_days_since_dx <- joined_df %>%
    filter(Days.since.Dx > 2555 & Days.since.Dx <= 3652.5)
  write.csv(edgeCase_days_since_dx$bcsbusername, file = "out/edgeCase_dx.txt", row.names = FALSE)
  
  
  joined_df <- joined_df %>%
    filter(Days.since.Dx >= 0)
  
  df <- joined_df %>% 
    # dplyr::group_by(lab) |> 
    dplyr::reframe(count = as.numeric(table(lab)),
                   lab = names(table(lab)),
                   total = dplyr::n()) %>%
    dplyr::mutate(pct = count/total,
                  lab = factor(lab)) %>% 
    dplyr::arrange(lab)
  
  df2 <- df %>% 
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
          legend.title = element_blank(), 
          plot.title = element_text(hjust = 0.5),  
          plot.subtitle = element_text(hjust = 0.5)) +
    guides(fill = guide_legend(nrow = 2, byrow = T)) +
    ggtitle("Years since diagnosis to Consent date") +   #
    labs(subtitle = "Data for participants on study who have completed Baseline Session, 
         signed consent and completed surveys. 
         (blood sample and/or clinical data may be pending)")

  #gp
  
  days_in_year <- 365.25
  
  joined_df$Days.since.Dx <- as.numeric(joined_df$Days.since.Dx)
  max_days_since_dx <- as.numeric(max(joined_df$Days.since.Dx))
  
  
  yearBreaks <- seq(0, max(joined_df$Days.since.Dx) + days_in_year, by = days_in_year)
  names(yearBreaks) <- paste0(0:(length(yearBreaks) - 1), "Y")
  
  # Create the histogram
  gp2 <- ggplot(joined_df, aes(x = Days.since.Dx)) +
    geom_histogram(binwidth = days_in_year, fill = "lightgray", color = "black", 
                 boundary = 0) +
    ylab("Number of Patients") +
    xlab("Years Since Diagnosis") +
    scale_x_continuous(breaks = yearBreaks, labels = names(yearBreaks),  limits = c(0, 3652.5) ) +
    theme_DB(bold.axis.title = T, grid.x = T, rotate.x = T) 
  
  #print(gp2)
  return(list(noConsent = na_rows, outlier_days = outlier_days_since_dx, multiple_dates = multiple_dates_df, df.DxDate = joined_df, gp = gp, gp2 = gp2))

}


write_csv(clind,"out/BWB-BCSB-clind_subtype_unconsolidated.matrix.csv")
write_csv(clind2,"out/BWB-BCSB-clind_subtype_consolidated.matrix.csv")



plt <- getYrSinceDiagnosis(dx_str, clind)
#plt$gp
#plt$gp2
write_csv(plt$df.DxDate,"out/BWB-BCSB-enrolled-diagnosis_date.matrix.csv")

if (0) {
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
 # ggsave("plots/clinical_info_plots/survey_missing_answers_pie.png", gp, height = 6, width = 6)
}








