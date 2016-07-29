# Re-coding and re-grouping variables for survey design
#setwd("~/git/banana-phone/work")
rm(list = ls(all.names = TRUE))

load('clean/cleaning2.RData')
library(car)
library(dplyr)

# reset the "default" level on categorical variables
code <- function(col, map, ref) {
  relevel(as.factor(map[col]), ref=ref)
}


# ethnicity
summary(data2$PPETHM)
data2$white <- recode(data2$PPETHM, recodes = "'White, Non-Hispanic' = 1; NA = NA; else = 0")
data2$black <- recode(data2$PPETHM, recodes = "'Black, Non-Hispanic' = 1; NA = NA; else = 0")
data2$hispanic <- recode(data2$PPETHM, recodes = "'Hispanic' = 1; NA = NA; else = 0")
data2$otherrace <- recode(data2$PPETHM, recodes = "'Other, Non-Hispanic' = 1; NA = NA; else = 0")
data2$mixedrace <- recode(data2$PPETHM, recodes = "'2+ Races, Non-Hispanic' = 1; NA = NA; else = 0")

# female
summary(data2$PPGENDER)
data2$female <- recode(data2$PPGENDER, recodes = "'Female' = 1; 'Male' = 0")

# income level
summary(data2$PPINCIMP)
income.map <- c(rep("under $20k", 6),
                rep("$20k to $40k", 4),
                rep("$40k to $75k", 3),
                rep("over $75k", 6))
data2$income <- code(data2$PPINCIMP, income.map, "under $20k")

# marital staus
summary(data2$PPMARIT)
marital.map <- c("single", "partnered", "partnered", "single", "single", "single")
data2$marital <- code(data2$PPMARIT, marital.map, "single")
table(data2$marital)

# work status
summary(data2$PPWORK)
work.map <- c(rep("unemployed", 5),
              rep("employed", 2))
data2$work <- code(data2$PPWORK, work.map, "employed")


##### -- survey questions --- #####

# Q1. flu knowledge
data2$know <- recode(data2$Q1, recodes = "'Yes' = 1; 'No' = 0; NA = NA")

# Q2. sick with flu last year
data2$sick <- recode(data2$Q2, recodes = "'Yes' = 1; 'No' = 0; NA = NA")

# Q3. household sick last year
data2$sickhh <- recode(data2$Q3, recodes = "'Yes' = 1; c('No', 'Don_t know') = 0; NA = NA")

# Q4. job requires contact with public
q4.map <- c("Yes", rep("No", 2))
data2$pubjob <- code(data2$Q4, q4.map, "Yes")


# Q13. flu vaccine
q13.map <- c("Yes", "Yes", "No")
data2$vax <- code(data2$Q13, q13.map, "Yes")


# Q26. children in household
data2$childhh <- data2$Q26 %>%
  recode(recodes = "'Yes' = 1; NA = NA; 'No' = 0")

# if you get confused
str(data2Q26)
str(data2$childhh)



##### ----- save ----- #####
datar <- data2
save(datar, file = "clean/recoding1.RData")

rm(list = ls(all.names = TRUE))

