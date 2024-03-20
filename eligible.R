#library(bsseq)
library(ggplot2)
source("theme_DB.R")

library(parallel)
library(xml2)


.dir <- "~/Documents/" 


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

#elig_str <- "BCSB Web eligibility survey_3.15.24.csv"
getEligiblity <- function(elig_str) {
  ef <- read.csv(paste0( elig_str))
  
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
  #write.csv(ef, file = "post_dup.csv", row.names = FALSE)
  
  return(ef)
}


getPieComb <- function(ss) {  # Removed 'rl' from the function parameters
  df <- ss |> 
    dplyr::count(Race) |>  # Simplified counting using dplyr::count
    dplyr::mutate(pct = n / sum(n))  # Calculate percentage
  
  # Removed the factor level setting using 'rl'
  df <- df |> 
    dplyr::arrange(Race)  # Keeping the arrange in case ordering by Race is desired
  
  df2 <- df |> 
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/2 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", n, "\n", round(pct*100, 1), "%"))  # Changed 'count' to 'n' to reflect dplyr::count usage
  
  df3 <- df |> 
    dplyr::summarize(n = paste0("N=", sum(n)))  # Summarize total count
  
  gp <- ggplot(df, aes(x = "" , y = pct, fill = Race)) +  # Removed fct_inorder since we're not specifying levels
    geom_col(width = 1, color = "black", size = 0.5) +  # Changed color to black for consistency
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
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "none") +
    guides(fill = guide_legend(ncol = 2)) +
    ggtitle("Demographic Distribution of all Web Eligibility Survey Entries") +   
    labs(subtitle = "Does not include clinic recruitment information")
  
  return(list(gp = gp, df = df))
}


getPie <- function(ss) {
  df <- ss %>%
    dplyr::count(diagnosis, Race) %>%
    dplyr::group_by(diagnosis) %>%
    dplyr::mutate(total = sum(n)) %>%
    dplyr::mutate(pct = n / total) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(diagnosis, Race)
  
  df2 <- df %>%
    dplyr::group_by(diagnosis) %>%
    dplyr::mutate(csum = rev(cumsum(rev(pct))), 
                  pos = pct/2 + dplyr::lead(csum, 1),
                  pos = dplyr::if_else(is.na(pos), pct/2, pos),
                  label = paste0("N=", n, "\n", round(pct*100, 1), "%")) %>%
    dplyr::ungroup()
  
  df3 <- df %>%
    dplyr::group_by(diagnosis) %>%
    dplyr::summarize(n = paste0("N=", sum(n))) %>%
    dplyr::ungroup()
  
  gp <- ggplot(df, aes(x = "" , y = pct, fill = Race)) +
    geom_col(width = 1, color = "black", size = 0.5) +
    facet_grid(cols = vars(diagnosis)) +
    coord_polar(theta = "y") +
    scale_fill_brewer(palette = "Set3") +
    geom_label_repel(data = df2,
                     aes(y = pos, label = label),
                     size = 3.75, 
                     nudge_x = 1,
                     show.legend = FALSE, 
                     label.padding = unit(0.75, "mm")) +
    geom_text(data = df3, x = -1.15, y = 0, aes(label = n), 
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
    ggtitle("Demographic Distribution of 
    Eligible vs. Ineligible 
            Web Eligibility Survey Entries")  
  
  return(list(gp = gp, df = df))
}



