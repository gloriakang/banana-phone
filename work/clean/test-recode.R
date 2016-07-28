# Recoding and re-grouping variables

setwd("~/git/banana-phone/work")
rm(list = ls(all.names = TRUE))
load('clean/cleaning2.RData')

# reset the "default" level on categorical variables
recode = function(col, map, ref) {
  relevel(as.factor(map[col]), ref=ref)
}

# income level
levels(data2$PPINCIMP)
income.map = c(rep("under $20k", 6),
               rep("$20k to $40k", 4),
               rep("$40k to $75k", 3),
               rep("over $75k", 6))
data2$income = recode(data2$PPINCIMP, income.map, "under $20k")
table(data2$income)


# marital staus
levels(data2$PPMARIT)
marital.map <- c("single", "partnered", "partnered", "single", "single", "single")
data2$marital = recode(data2$PPMARIT, marital.map, "single")
table(data2$marital)


# work status
levels(data2$PPWORK)
work.map <- c(rep("unemployed", 5),
              rep("employed", 2))
data2$work = recode(data2$PPWORK, work.map, "employed")
table(data2$work)


