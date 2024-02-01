setwd(("Documents/wyeth_script/"))

library(dplyr)
library(ggplot2)
library(readr)

df2 <- read.csv("website eligibility survey_raw data_10.27.23.csv")
summary(df2)

names(df2) <- c( "name", "DOB", "email", "phone", "Race", "Participate", 
                 "stage3", "adult", "recent", "stage4", "recurrence", 
                 "activeTreatment", "foundUs", "SMoutlet", "source" , 
                 "rating", "suggestion","EntryDate")

df_unique <- df2 %>% distinct("name", "DOB", .keep_all = TRUE)
