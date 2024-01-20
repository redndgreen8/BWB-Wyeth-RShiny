#library(bsseq)
library(ggplot2)
source("theme_DB.R")

library(parallel)
library(xml2)




clean_text <- function(txt) {
  # Remove punctuation
  txt <- gsub("[[:punct:]]", "", txt)
  # Convert to lowercase
  txt <- tolower(txt)
  # Trim leading and trailing white spaces
  txt <- trimws(txt)
  return(txt)
}



isEligible <- function(ef) {
  #ef <- tolower(ef)
  ef[ef == ""] <- NA
  ef[ef == "Yes"] <- T
  ef[ef == "No"] <- F
 # view(ef)
  yes.reqd <- ef[, 7:10] |> purrr::map(as.logical) |> bind_cols()
  no.reqd <- ef[, 11:13] |> purrr::map(as.logical) |> bind_cols()
  cc <- complete.cases(yes.reqd) & complete.cases(no.reqd)
  eli <- rowSums(!yes.reqd) == 0 & rowSums(no.reqd) == 0
  eli[is.na(eli)] <- F
  #view(eli)
  return(eli)
}

isEligible1 <- function(ef) {
  ef <- tolower(ef)
  ef[ef == ""] <- NA
  ef[ef == "yes"] <- TRUE
  ef[ef == "no"] <- FALSE
  
  yes_reqd <- ef[, 7:10] |> purrr::map_dfc(as.logical)
  
  no_reqd <- ef[, 11:13] |> purrr::map_dfc(as.logical)
  
  # Check for complete cases in both yes.required and no.required
  complete_cases <- complete.cases(yes_reqd, no_reqd)
  
  # Check eligibility criteria
  eli <- (rowSums(yes_reqd, na.rm = TRUE) == rowSums(!is.na(yes_reqd))) & 
    (rowSums(no_reqd, na.rm = TRUE) == 0) & 
    complete_cases
  
  return(eli)
}

isEligible2 <- function(ef) {
  #ef <- tolower(ef)
  ef[ef == ""] <- NA
#  view(ef)
  
  ef[ef == "Yes"] <- TRUE
  ef[ef == "No"] <- FALSE
 # view(ef)
  
  # Convert specified columns to logical
  yes_reqd <- ef[, 7:10] |> purrr::map_dfc(as.logical)
  no_reqd <- ef[, 11:13] |> purrr::map_dfc(as.logical)
  # Initialize an empty logical vector for eligibility
  eli <- logical(nrow(ef))
  
  # Check each row for eligibility
  for (i in 1:nrow(ef)) {
    # Check all 'yes' required columns are TRUE (and no NA)
    yes_check <- all(yes_reqd[i, ], na.rm = FALSE) && !any(is.na(yes_reqd[i, ]))
    
    # Check all 'no' required columns are FALSE (and no NA)
    no_check <- all(!no_reqd[i, ], na.rm = FALSE) && !any(is.na(no_reqd[i, ]))
    
    # Assign eligibility
    eli[i] <- yes_check & no_check
  }
  view(eli)
  
  return(eli)
}



getEligiblity <- function(elig_str) {
  ef <- read.csv(paste0(.dir, elig_str))
  
  interested_cols <- c( 1,2,4,5)
  
  char_cols <- which(sapply(ef[interested_cols], is.character))
  for (col in interested_cols) {
    ef[[col]] <- sapply(ef[[col]], clean_text)
  }
  #write.csv(ef, file = "pre_dup.csv", row.names = FALSE)
  ef <- ef[!duplicated(ef[, 1:5]),]
  ef <- ef[!duplicated(ef[, 4]),]
  ef <- ef[!duplicated(ef[, 5]),]
  
  ef$is.eligible <- isEligible(ef)
#  write.csv(ef, file = "post_dup.csv", row.names = FALSE)
  
  return(ef)
}


getPieaRating <- function(ss, rl) {
  df <- ss |> 
    dplyr::reframe(count = as.numeric(table(Race)),
                   Race = names(table(Race)),
                   total = dplyr::n()) |>
    dplyr::mutate(pct = count/total,
                  Race = factor(Race, levels = rl)) |> 
    dplyr::arrange(Race)
  
  df2 <- df |> 
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/2 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", count, "\n", round(pct*100, 1), "%")) 
  
  df3 <- df |> 
    dplyr::summarize(n = paste0("N=", sum(count)))
  
  gp <- ggplot(df, aes(x = "" , y = pct, fill = fct_inorder(Race))) +
    geom_col(width = 1, color = 1, size = 0.5) +
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
  gp
  return(list(gp = gp, df = df))
}






getPieComb <- function(ss, rl) {
  df <- ss |> 
    dplyr::reframe(count = as.numeric(table(Race)),
                   Race = names(table(Race)),
                   total = dplyr::n()) |>
    dplyr::mutate(pct = count/total,
                  Race = factor(Race, levels = rl)) |> 
    dplyr::arrange(Race)
  
  df2 <- df |> 
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/2 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", count, "\n", round(pct*100, 1), "%")) 
  
  df3 <- df |> 
    dplyr::summarize(n = paste0("N=", sum(count)))
  
  gp <- ggplot(df, aes(x = "" , y = pct, fill = fct_inorder(Race))) +
    geom_col(width = 1, color = 1, size = 0.5) +
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
  gp
  return(list(gp = gp, df = df))
}


getPie <- function(ss, rl) {
  df <- ss |> 
    dplyr::group_by(diagnosis) |> 
    dplyr::reframe(count = as.numeric(table(Race)),
                   Race = names(table(Race)),
                   total = dplyr::n()) |>
    dplyr::mutate(pct = count/total,
                  Race = factor(Race, levels = rl)) |> 
    dplyr::arrange(Race)
  
  df2 <- df |> 
    dplyr::group_by(diagnosis) |> 
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/2 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", count, "\n", round(pct*100, 1), "%")) 
  
  df3 <- df |> 
    dplyr::group_by(diagnosis) |> 
    dplyr::summarize(n = paste0("N=", sum(count)))
  
  gp <- ggplot(df, aes(x = "" , y = pct, fill = fct_inorder(Race))) +
    geom_col(width = 1, color = 1, size = 0.5) +
    facet_grid(cols = vars(diagnosis)) +
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
  gp
  return(list(gp = gp, df = df))
}

ss.bcsb.ef <- getEligiblity("demographics/Website Eligibility Survey 1.18.24.csv") |> 
  dplyr::rename(Race = What.is.your.race.ethnicity.) |> 
  dplyr::mutate(Race = ifelse(is.na(Race) | Race %in% "Prefer not to answer" , "UNKNOWN", Race),
                Race = toupper(Race),
                Race = trimws(Race),
                Race = ifelse(grepl("BLACK|AFRICAN", Race), "BLACK", Race),
                Race = ifelse(grepl("KOREAN|CHINESE|ASIAN|ARAB", Race), "ASIAN", Race),
                Race = ifelse(grepl("PACIFIC|ISLANDER|NATIVE|INDIAN|ALASKAN", Race), "NA.AMERI/P.ISLA", Race),
                Race = ifelse(grepl("MEXICAN|CENTRAL|HISPANIC", Race), "HISPANIC", Race),
                diagnosis = ifelse(is.eligible, "BCSB ELIGIBLE", "BCSB INELIGIBLE")) |> 
  dplyr::select(Race, diagnosis)
rLevels <- c("ASIAN", "BLACK", "HISPANIC", "WHITE", "MIXED", "NA.AMERI/P.ISLA", "UNKNOWN")

res.bcsb.ef <- getPie(ss.bcsb.ef, rl = rLevels)
res.bcsb.ef$gp
ggsave("demographics//Eligibility_pie_chart.png", res.bcsb.ef$gp, height = 7, width = 7)

res.bcsb.comb <- getPieComb(ss.bcsb.ef, rl = rLevels)
res.bcsb.comb$gp
ggsave("demographics//Eligibility_pie_chart_COMB.png", res.bcsb.comb$gp, height = 7, width = 7)
