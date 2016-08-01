# Re-coding and re-grouping variables for survey design
#setwd("~/git/banana-phone/work")
rm(list = ls(all.names = TRUE))

load('clean/cleaning2.RData')
library(car)
library(dplyr)

datar <- data2

# for reference:
# yn.lab = c("Yes", "No")
# dataf$Q1 <- factor(data$Q1, levels = yn.lab)
# dataf$Q2 <- relevel(dataf$Q2, "Yes")

# use code function for response variables
# reset the "default" level on categorical variables
code <- function(col, map, ref) {
  relevel(as.factor(map[col]), ref=ref)
}


# ethnicity
# probably don't need recoding
summary(data2$PPETHM)
datar$white <- car::recode(data2$PPETHM, recodes = "'White, Non-Hispanic' = 1; NA = NA; else = 0")
datar$black <- car::recode(data2$PPETHM, recodes = "'Black, Non-Hispanic' = 1; NA = NA; else = 0")
datar$hispanic <- car::recode(data2$PPETHM, recodes = "'Hispanic' = 1; NA = NA; else = 0")
datar$otherrace <- car::recode(data2$PPETHM, recodes = "'Other, Non-Hispanic' = 1; NA = NA; else = 0")
datar$mixedrace <- car::recode(data2$PPETHM, recodes = "'2+ Races, Non-Hispanic' = 1; NA = NA; else = 0")


# gender = female
# probably can just relevel = relevel(data2$PPGENDER, "Female")
summary(data2$PPGENDER)
datar$female <- car::recode(data2$PPGENDER, recodes = "'Female' = 1; 'Male' = 0")


# regroup income level
summary(data2$PPINCIMP)
income.map <- c(rep("under $20k", 6),
                rep("$20k to $40k", 4),
                rep("$40k to $75k", 3),
                rep("over $75k", 6))
datar$income <- code(data2$PPINCIMP, income.map, "under $20k")


# regroup marital staus
summary(data2$PPMARIT)
marital.map <- c("single", "partnered", "partnered", "single", "single", "single")
datar$marital <- code(data2$PPMARIT, marital.map, "single")
#summary(data2$PPMARIT)
#summary(datar$marital)


# regroup work status
str(datar$work)
str(data2$PPWORK)
work.map <- c(rep("unemployed", 5),
              rep("employed", 2))
datar$work <- code(data2$PPWORK, work.map, "unemployed")


##### -- survey questions --- #####


# Q1. flu knowledge
#datar$q1 <- car::recode(data2$Q1, recodes = "'Yes' = 1; 'No' = 0; NA = NA")
#summary(datar$q1)
#summary(data2$Q1)


# Q2. sick with flu last year
#datar$sick <- car::recode(data2$Q2, recodes = "'Yes' = 1; 'No' = 0; NA = NA")
#datar$sick <- datar$q2
#summary(datar$q2)
#summary(datar$sick)


# Q3. household sick last year
#datar$q3 <- car::recode(data2$Q3, recodes = "'Yes' = 1; c('No', 'Don_t know') = 0; NA = NA")
#datar$sickhh <- datar$q3

# regroup Q4. job requires contact with public
#levels(data2$Q4)
q4.map <- c("Yes", rep("No", 2))
datar$q4 <- code(data2$Q4, q4.map, "Yes")
#summary(datar$Q4)
#summary(data2$Q4)


# regroup Q13. flu vaccine
q13.map <- c("Yes", "Yes", "No")
datar$q13 <- code(data2$Q13, q13.map, "Yes")


# Q26. children in household
#datar$q26 <- data2$Q26 %>%
#  car::recode(recodes = "'Yes' = 1; NA = NA; 'No' = 0")

#summary(data2$Q26)
#summary(datar$q26)





# if you get confused
#str(datar$Q26)
#str(datar$q26)


##### ----- save ----- #####
save(datar, code, file = "clean/recoding1.RData")
rm(list = ls(all.names = TRUE))

