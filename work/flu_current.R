setwd("~/git/banana-phone/work")
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)

data_W <- read.csv("data/surveydata.csv", na = c("#NULL!", "", "Refused"), stringsAsFactors = FALSE)
data_UNW <- read.csv("data/surveydata_unw.csv", na = c("#NULL!", "", "Refused"), stringsAsFactors = FALSE)

# source("data_cleaning.R")
load("clean/all.Rdata")

# set new names
names(data2) <- new_name


# Q1 tables
with(data2, table(Q1))
data2 %>%
  count(Q1)

# unweighted Q1
with(data_UNW, table(Q1))
data_UNW %>%
  count(Q1)


# Q2
with(data2, table(Q2))
data2 %>%
  count(Q2)

with(data_UNW, table(Q2))
data_UNW %>%
  count(Q2)

