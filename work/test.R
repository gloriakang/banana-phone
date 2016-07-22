setwd("~/git/banana-phone/work")
rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(prettyR)
library(gmodels)

# data_W <- read.csv("data/surveydata.csv", na = c("#NULL!", "", "Refused"), stringsAsFactors = FALSE)
# source("data_cleaning.R")

load("clean/cleaning_2.Rdata")
load("clean/plotting1.Rdata")
names(data2) <- old_name


# ------------ start here ------------- #
# contingency tables

# crosstab of gender by Q1; odds ratio, risk ratio
xtab(PPGENDER ~ Q1, data2)


# joint freq, conditional %, marginal prop
with(data2, CrossTable(PPGENDER, Q1, prop.chisq = F))
with(data2, CrossTable(PPETHM, Q1, prop.chisq = F))

#
with(data2, addmargins(table(PPINCIMP, Q1), 1:2))
with(data2, prop.table(table(PPINCIMP, Q1), 2))








## ----------------------------------------- ##
# make separate file focusing on subset of data
# sick vs. not sick









# -------------------------------------------#
# TO DO: re-group income levels






# ------------------------------------- #
# get freq counts into a list where each element in a list is a dataframe

# table of variable counts
lapply(X = data2[5:19], FUN = function(x){aggregate(data.frame(count = x), list(response = x), length)})
# Q1 - Q6
lapply(X = data2[28:33], FUN = function(x){aggregate(data.frame(count = x), list(response = x), length)})
# Q7
lapply(X = data2[34:42], FUN = function(x){aggregate(data.frame(count = x), list(response = x), length)})
# Q8
lapply(X = data2[43:50], FUN = function(x){aggregate(data.frame(count = x), list(response = x), length)})

# tapply
tapply(data2$Q1, data2$PPGENDER, FUN = function(x){aggregate(data.frame(count = x), list(group = x), length)})




# ----------------------------------- #

# Q1 tables
with(data2, table(Q1))
data2 %>%
  count(Q1)

# Q2
with(data2, table(Q2))
data2 %>%
  count(Q2)


##### -------------- Working analysis --------------- #####

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



# --------------------------------------- #

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


# -------------------------------------------- #

# perceived risk
with(Q11, table(q, r))

# high risk
risk <- Q11 %>%
  filter(r=='High Risk, Very Likely')


# Q7
with(Q7, table(q, r))
# Q7 and gender
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




### ----------- library(psych) ----------- ###

a <- describe(data2)
View(a)




### --------- trying to make a function --------- ###

# tables
t <- function(x, ...) {
  print(addmargins(table(x, ...))) # counts
  print(addmargins(prop.table(table(x, ...))))
}

t2 <- function(x, y, ...) {
  print(addmargins(table(x, y)))
  print(addmargins(prop.table(table(x, y, ...))))
}

x = data2$Q1
y = data2$PPGENDER

t(data2$Q1)
t2(x, y)

y = data2$PPETHM

with(data2, prop.table(table(Q1, Q2)))
with(data2, prop.table(table(Q1, Q2), margin = 1))
with(data2, prop.table(table(Q1, Q2), margin = 2))

