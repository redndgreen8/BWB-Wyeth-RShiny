setwd(("Documents/wyeth_script/"))


library(ggplot2)
library(readr)

df <- read_csv("StateDistribution_10.27.23.csv")#, col_names = FALSE)
ggplot(df, aes(x=df[["Current State"]])) +
  geom_bar(fill="blue", alpha=0.7) +
  labs(title="Distribution of States",
       x="State", y="Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

