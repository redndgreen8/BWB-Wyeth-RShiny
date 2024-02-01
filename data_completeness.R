library(dplyr)
library(ggplot2)
source("theme_DB.R")

files <- list.files("patient_information/current/", "csv$", recursive = T, full.names = T)
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

df.sheet <- df |> 
  summarise(n = n(),
            numNA = sum(is.na(value) | value %in% ""),
            numNoAns = sum(value %in% "I prefer not to answer"),
            numAns = length(value) - numNA - numNoAns,
            Missing = numNA/n,
            Prefer_not_to_answer = numNoAns/n,
            Answered = numAns/n) 

df.sheet2 <- df |> 
  group_by(sheet) |> 
  summarise(n = n(),
            numNA = sum(is.na(value) | value %in% ""),
            numNoAns = sum(value %in% "I prefer not to answer"),
            numAns = length(value) - numNA - numNoAns,
            Missing = numNA/n,
            Prefer_not_to_answer = numNoAns/n,
            Answered = numAns/n) 

df.plt <- df.sheet |> 
  select(Missing, Prefer_not_to_answer, Answered) |> 
  reshape2::melt()

df.plt2 <- df.sheet2 |> 
  select(Missing, Prefer_not_to_answer, Answered, sheet) |> 
  reshape2::melt(id.vars = "sheet")

ggplot(df.plt, aes(x = variable, y = value, label = scales::percent(value))) +
  geom_bar(stat = "identity", fill = "lightgray", color = "black") +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, 
            size = 5) +
  xlab("") +
  scale_y_continuous(labels = scales::percent, 
                     name = "Percent of questions",
                     limits = c(0, 1.1)) +
  theme_DB(rotate.x = T) +
  theme(strip.text = element_text(size = 8)) 
ggsave("data_completeness.png", height = 6, width = 4)

ggplot(df.plt2, aes(x = variable, y = value, label = scales::percent(round(value, 3)))) +
  geom_bar(stat = "identity", fill = "lightgray", color = "black") +
  facet_wrap(. ~ sheet) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, 
            size = 4) +
  xlab("") +
  scale_y_continuous(labels = scales::percent, 
                     name = "Percent of questions",
                     limits = c(0, 1.1)) +
  theme_DB(rotate.x = T) +
  theme(strip.text = element_text(size = 8))
ggsave("data_completeness_by_sheet.png", height = 10, width = 10)



