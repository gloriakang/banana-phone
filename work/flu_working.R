# Flu survey data analysis
setwd("~/git/banana-phone/work")

library(dplyr)
library(tidyr)
library(ggplot2)

data <- read.csv("surveydata.csv", na = "#NULL!")


# select() question data
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

# gather() into columns for plotting
q7 <- q7.df %>%
  gather("q", "r", 1:8)

# group_by() and summarise() counts
q7 %>%
  group_by(r, q) %>%
  summarise(count = n())




##### old test code #####

q1.df = select(data, Q1)
q1 = gather(q1.df, "q", "r", 1)
summary(q1.df)

q2.df = select(data, Q2)
q2 = gather(q2.df, "option", "response", 1)
summary(q2.df)

q7.df = select(data, Q7_1:Q7_otherText)
names(q7.df) = c("Bus", "Carpool", "Subway",
                 "Train", "Taxi", "Airplane",
                 "Other", "Refused", "Other text")
q7 = gather(q7.df, "q", "r", 1:8)
summary(q7.df)




