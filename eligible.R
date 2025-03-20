#library(bsseq)
library(ggplot2)
source("theme_DB.R")
source("str_list.R")

library(parallel)
library(xml2)
library(stringdist)

.dir <- "~/Documents/" 


clean_text <- function(txt) {
  # Remove punctuation
  txt <- gsub("[[:punct:]]", "", txt)
  
  # Convert to lowercase
  txt <- tolower(txt)
  
  # Trim leading, trailing, and middle white spaces
  txt <- gsub("\\s+", " ", txt)
  txt <- trimws(txt)
  
  return(txt)
}

isEligible <- function(ef) {
  #ef <- tolower(ef)
  ef[ef == ""] <- NA
  ef[ef == "Yes"] <- T
  ef[ef == "No"] <- F
  #view(ef)
  #yes.reqd <- ef[, 7:10] |> purrr::map(as.logical) |> bind_cols()
  #no.reqd <- ef[, 11:13] |> purrr::map(as.logical) |> bind_cols()
  
  yes.reqd <- ef[, 7:10] |> purrr::map(as.logical) |> bind_cols()
  no.reqd <- ef[, 12:14] |> purrr::map(as.logical) |> bind_cols()
  
  
  
  cc <- complete.cases(yes.reqd) & complete.cases(no.reqd)
  eli <- rowSums(!yes.reqd) == 0 & rowSums(no.reqd) == 0
  eli[is.na(eli)] <- F
  

  #view(eli)
  return(eli)
}




isStage4 <- function(ef) {
  #ef <- tolower(ef)
  ef[ef == ""] <- NA
  ef[ef == "Yes"] <- T
  ef[ef == "No"] <- F
  #view(ef)
  #yes.reqd <- ef[, 7:10] |> purrr::map(as.logical) |> bind_cols()
  #no.reqd <- ef[, 11:13] |> purrr::map(as.logical) |> bind_cols()
  
  yes.reqd <- ef[, c(7,9,12)] |> purrr::map(as.logical) |> bind_cols()
 # no.reqd <- ef[, c(8,12)] |> purrr::map(as.logical) |> bind_cols()
  
  cc <- complete.cases(yes.reqd)# & complete.cases(no.reqd)
  eli <- rowSums(!yes.reqd) == 0 #& rowSums(no.reqd) == 0
  eli[is.na(eli)] <- F
  
  
  return(eli)
}

isRecur <- function(ef) {
  #ef <- tolower(ef)
  ef[ef == ""] <- NA
  ef[ef == "Yes"] <- T
  ef[ef == "No"] <- F
  #view(ef)
  #yes.reqd <- ef[, 7:10] |> purrr::map(as.logical) |> bind_cols()
  #no.reqd <- ef[, 11:13] |> purrr::map(as.logical) |> bind_cols()
  
  yes.reqd <- ef[, c(7,9,13)] |> purrr::map(as.logical) |> bind_cols()
  # no.reqd <- ef[, c(8,12)] |> purrr::map(as.logical) |> bind_cols()
  
  cc <- complete.cases(yes.reqd)# & complete.cases(no.reqd)
  eli <- rowSums(!yes.reqd) == 0 #& rowSums(no.reqd) == 0
  eli[is.na(eli)] <- F
  
  
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
  
  ef <- ef[!grepl("mailinatorcom", ef[, 4], ignore.case = TRUE), ]
  
  ef$is.eligible <- isEligible(ef)
  
  ef$is.stage4 <- isStage4(ef)
  
  ef$is.recur <- isRecur(ef)
  #write.csv(ef, file = "post_dup.csv", row.names = FALSE)
  
  return(ef)
}


isNormal <- function(ef) {
  # Create a logical vector of the same length as the number of rows in ef
  result <- rep(FALSE, nrow(ef))
  
  # Check which rows have column 7 as "Yes" and column 8 as "No"
  result <- ef[, 7] == "Yes" & ef[, 8] == "No"
  
  # Replace NA values with FALSE
  result[is.na(result)] <- FALSE
  
  return(result)
}

getNormalEligiblity <- function(elig_str) {
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
  
  ef <- ef[!grepl("mailinatorcom", ef[, 4], ignore.case = TRUE), ]
  
  ef$is.normal <- isNormal(ef)
    #write.csv(ef, file = "post_dup.csv", row.names = FALSE)
  
  return(ef)
}


NormalCohort <- getNormalEligiblity(eligible_str)

df.eligTest <- getEligiblity(eligible_str) 
df.elig <- getEligiblity(eligible_str) |> 
  dplyr::rename(Race = What.is.your.race.ethnicity.) |> 
  dplyr::rename(Fname = What.is.your.name...First) |> 
  dplyr::rename(Lname = What.is.your.name...Last) |> 
  dplyr::mutate(Race = ifelse(is.na(Race) | Race %in% "Prefer not to answer", "Unknown", Race)) |>
  dplyr::mutate(Minority = ifelse( Race == "White", "FALSE", "TRUE")) |>
  dplyr::select(Fname, Lname, Email, Phone.Number, Race, is.eligible, Minority,  is.stage4, is.recur)

Normal.elig <- getNormalEligiblity(eligible_str) |> 
  dplyr::rename(Race = What.is.your.race.ethnicity.) |> 
  dplyr::rename(Fname = What.is.your.name...First) |> 
  dplyr::rename(Lname = What.is.your.name...Last) |> 
  dplyr::mutate(Race = ifelse(is.na(Race) | Race %in% "Prefer not to answer", "Unknown", Race)) |>
  dplyr::mutate(Minority = ifelse( Race == "White", "FALSE", "TRUE")) |>
  dplyr::select(Fname, Lname, Email, Phone.Number, Race, Minority,  is.normal)

df.cohort.merged <- df.elig %>%
  dplyr::full_join(Normal.elig, 
                   by = c("Fname", "Lname", "Email", "Phone.Number", "Race", "Minority"))

write_csv(df.cohort.merged, "BCSB_cohorts.csv")  # Save output as CSV



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

  gp2 <- ggplot(df, aes(x = "" , y = pct, fill = Race)) +  # Removed fct_inorder since we're not specifying levels
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
          plot.title = element_text(size = 25, hjust = 0.5),  
          plot.subtitle = element_text(size = 15, hjust = 0.5) ) +
    guides(fill = guide_legend(ncol = 2)) +
    ggtitle("Demographic Distribution of all Web Eligibility Survey Entries") +   
    labs(subtitle = "Does not include clinic recruitment information")
  
  
  
    ggsave("plots/Elig_comb.png", gp2, height = 9, width = 16, dpi = 600)
  
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
  ggsave("plots/Elig.png", gp, height = 9, width = 16, dpi = 600)
  
  return(list(gp = gp, df = df))
}


cleanCalend <- function(calend_str) {
  ef <- read.csv(paste0( calend_str))
  
  interested_cols <- c( 4,5,6,13)
  
  char_cols <- which(sapply(ef[interested_cols], is.character))
  for (col in interested_cols) {
    ef[[col]] <- sapply(ef[[col]], clean_text)
  }

  
  
  return(ef)
}


calendly_df <- cleanCalend(calend_str) |>
  dplyr::select(Invitee.First.Name, Invitee.Last.Name, Invitee.Email, 
                Invitee.Time.Zone, Start.Date...Time, End.Date...Time, Location)



# Define the threshold for fuzzy matching
threshold <- 0.83

# Perform fuzzy matching and intersection
# Define the field pairs for matching
field_pairs <- c("Invitee.Email" = "Email")#, "Location" = "Phone.Number")

# Initialize an empty data frame to store the concatenated results
final_data <- data.frame()

# Iterate over the field pairs
for (i in seq_along(field_pairs)) {
  calendly_field <- names(field_pairs)[i]
  elig_field <- field_pairs[i]
  
  # Perform fuzzy matching and intersection based on the current field pair
  intersected_data <- calendly_df %>%
    rowwise() %>%
    mutate(match_id = possibly(
      df.elig %>%
          mutate(dist = stringdist(!!sym(elig_field), !!sym(calendly_field), method = "lv")) %>%
          filter(dist <= nchar(!!sym(calendly_field)) * (1 - threshold)) %>%
          pull(row_number()), otherwise = NA_integer_ )()
    ) %>%
    ungroup() %>%
    left_join(df.elig, by = setNames(elig_field, calendly_field))
  
  
  # Concatenate the matched data
  final_data <- bind_rows(final_data, intersected_data)
}

write.csv(final_data, file = "calendly_race.csv")
write.csv(final_data[which(final_data$Minority == TRUE), ], file = "calendly_minorities.csv")

#eligible_subset <- eligible[, c(6,15,16,17,18,20,21)]
#eligible_subset$index <- rownames(eligible)

# Reorder to place Index as the first column (optional)
#eligible_subset <- eligible_subset[, c("Index", names(eligible_subset)[-length(names(eligible_subset))])]
#write.csv(eligible_subset, file = "eligibility_entries.csv", row.names = FALSE)

write.csv(df.eligTest[, c(6,15,16,17,18,20,21)], file = "eligibility_entries.1.csv")



