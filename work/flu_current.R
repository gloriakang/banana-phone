setwd("~/git/banana-phone/work")
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)

# source("data_cleaning.R")
load("clean/all.Rdata")

# set new names
names(data2) <- new_name


# Q1 tables
with(data2, table(Q1))
data2 %>%
  count(Q1)

