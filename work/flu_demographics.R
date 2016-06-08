# demographic variables

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

data <- read.csv("surveydata.csv", na = c("#NULL!", ""))
data2 <- read.csv("data2.csv", na = c("#NULL!", ""))

