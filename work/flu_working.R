# Flu survey data analysis
setwd("~/git/banana-phone/work")

library(dplyr)
library(tidyr)
library(ggplot2)

data <- read.csv("surveydata.csv", na = "#NULL!")
class(data)
View(data)


# subset question data
q7.df <- data %>%
  select(Q7_1:Q7_otherText) %>%
  rename("Bus" = Q7_1,
         "Carpool" = Q7_2,
         "Subway" = Q7_3,
         "Train" = Q7_4,
         "Taxi" = Q7_5,
         "Airplane" = Q7_6,
         "Other" = Q7_7,
         "Refused" = Q7_8,
         "Other text" = Q7_otherText
  )

# gather into columns
q7 <- q7.df %>%
  gather("option", "response", 1:8)

# show counts
q7 %>%
  group_by(response, option) %>%
  summarise(count = n())



