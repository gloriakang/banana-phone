
library(dplyr)
library(ggplot2)

data = read.csv("surveydata.csv")

str(data)
head(data)

table(data$Q1) %>% barplot()
