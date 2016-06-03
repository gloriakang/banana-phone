# Flu survey data analysis
setwd("~/git/banana-phone/work")

library(dplyr)
library(tidyr)
library(ggplot2)

data <- read.csv("surveydata.csv", na = "#NULL!")


# select() question columns
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

# gather() into rows for plotting
q7 <- q7.df %>%
  gather("q", "r", 1:8)

# group_by() and summarise() counts
q7 %>%
  group_by(q, r) %>%
  count(q, r)


## all together now:
(
q7_long <- q7.df %>%
  gather("q", "r", 1:8) %>%
  group_by(q, r) %>%
  count(q, r)
)

ggplot(data = q7_long[!is.na(q7_long$r), ], aes(x = q, y = n, fill = r)) +
  geom_bar(stat='identity', position=position_dodge())


### ---------------







