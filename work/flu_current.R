setwd("~/git/banana-phone/work")
rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)

#data_W <- read.csv("data/surveydata.csv", na = c("#NULL!", "", "Refused"), stringsAsFactors = FALSE)
#data_UNW <- read.csv("data/surveydata_unw.csv", na = c("#NULL!", "", "Refused"), stringsAsFactors = FALSE)

# source("data_cleaning.R")
load("clean/all.Rdata")

# set new names
names(data2) <- new_name


#------- start here --------#

# get freq counts into a list where each element in a list is a dataframe
lapply(X = data2[3:19], FUN = function(x){aggregate(data.frame(count = x), list(value = x), length)})









# ----------------------#

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


##### ----- Working analysis ----- #####

# did you have the flu last year?
with(data2, table(Q2))
with(data2, addmargins(table(Q2, PPGENDER)))

#' of those sick, 56% were female, 43% were male
with(sick, prop.table(table(PPGENDER)))

# income of sick group
with(data2, addmargins(table(PPINCIMP, Q2)))
with(data2, prop.table(table(PPINCIMP, Q2)), margin = 1)

# filter sick group only
sick <- data2 %>%
  filter(Q2=='Yes')

sick %>%
  select(PPGENDER, PPAGE, PPEDUC, PPETHM, PPINCIMP, PPWORK, Q7_1_Bus:Q7_otherText) %>%
  gather("q", "r", Q7_1_Bus:Q7_7_Other) %>%
  group_by(PPGENDER, q, r) %>%
  summarise(n = n())

# sick demographics
sick %>%
  select(PPGENDER, PPAGE, PPEDUC, PPETHM, PPINCIMP, PPWORK) %>%
  group_by(PPGENDER) %>%
  summarize(n = n())


# ------------------------- #


# Q1 proportions
with(data2, prop.table(table(Q1)))

# Q1 and GENDER
with(data2, table(Q1, PPGENDER))
with(data2, prop.table(table(PPGENDER, Q1), margin = 1))
with(data2, prop.table(table(PPGENDER, Q1), margin = 2))


# Q1 and Q2 counts
#with(data2, by(Q1, Q2, summary))
with(data2, table(Q1, Q2))

# Q1 and Q2 proportions
with(data2, prop.table(table(Q1, Q2)))
with(data2, prop.table(table(Q1, Q2), margin = 1))
with(data2, prop.table(table(Q1, Q2), margin = 2))


# sick and getting flu vaccine
with(sick, table(Q13))
with(data2, table(Q2, Q13))


# -------------------------- #

# perceived risk
with(Q11, table(q, r))

# high risk
risk <- Q11 %>%
  filter(r=='High Risk, Very Likely')


# Q7.
with(Q7, table(q, r))

# Q7. and gender
with(Q7, table(q, r, PPGENDER))



# Q33. How many people, including yourself, reside in your household?
with(data2, summary(Q33))
ggplot(data2, aes(x = Q33)) + geom_histogram(binwidth = 2)

# ethnicity
with(data2, by(Q33, PPETHM, summary))
ggplot(data2, aes(x = PPETHM, y = Q33)) + geom_boxplot() + coord_flip()



# Q2 those infected + Q3 HHC infected
with(data2, table(Q2, Q3))
# by gender
test <- filter(data2, Q2=='Yes', Q3=='Yes')
with(test, table(PPGENDER))


# chi square?
summary(with(data2, table(Q2, Q3)))
with(data2, table(Q2))

# remake table?
testq2 <- c(rep("Yes", 385), rep("No", 1598))
testq3 <- c(rep("Yes", 236), rep("No", 149), rep("Yes", 145), rep("No",1453))
table(testq2, testq3)
summary(table(testq2, testq3))




########## --- using psych --- ##########
a <- describe(data2)
View(a)




