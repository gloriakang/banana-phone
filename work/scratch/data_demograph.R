# demographic variables

library(dplyr)
library(tidyr)
library(ggplot2)

data <- read.csv("surveydata.csv", na = c("#NULL!", "", "Refused"))
data2 <- read.csv("data2.csv", na = c("#NULL!", "", "Refused"))


