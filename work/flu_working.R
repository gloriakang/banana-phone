# Flu survey data analysis

setwd("~/git/banana-phone/work")
library(dplyr)
library(tidyr)

data <- read.csv("surveydata.csv")
class(data)


str(data)
View(data)

# subsetting with dplyr
names(data)