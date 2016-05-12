# flu survey analysis

setwd("~/git/banana-phone/work")
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)

data = read.csv("surveydata.csv")
str(data)
View(data)


# Q10 types of transportation
# Q11 how do you rate your risk of getting influenza at the following locations


# subsetting with dplyr
names(data)
q7.df = select(data, Q7_1:Q7_7)
q8.df = select(data, Q8_1:Q8_7)
q10.df = select(data, Q10_1:Q10_9)
q11.df = select(data, Q11_1:Q11_11)
q12.df = select(data, Q12_1:Q12_15)
q18.df = select(data, Q18_1:Q18_11)
q22.df = select(data, Q22_1:Q22_9)
q23.df = select(data, Q23_1:Q23_11)
q24.df = select(data, Q24_1:Q24_7)

str(q7.df)
summary(q7.df)

# gather with tidyr, into columns
q7 = gather(q7.df, "opt", "res", 1:7)
q8 = gather(q8.df, "opt", "res", 1:7)
q10 = gather(q10.df, "opt", "res", 1:9)
q11 = gather(q11.df, "opt", "res", 1:11)
q12 = gather(q12.df, "opt", "res", 1:15)
q18 = gather(q18.df, "opt", "res", 1:11)
q22 = gather(q22.df, "opt", "res", 1:9)
q23 = gather(q23.df, "opt", "res", 1:11)
q24 = gather(q24.df, "opt", "res", 1:7)

str(q12) # need to rename


# barplot
# ggplot(q7) + geom_bar(aes(x = opt, fill = res), position = "dodge")
# ggplot(q7.df) + geom_bar(aes(x = Q7_1), position = "dodge")

ggplot(q7) + geom_bar(aes(x = res), position = "dodge") + facet_wrap(~opt)
ggplot(q8) + geom_bar(aes(x = res), position = "dodge") + facet_wrap(~opt)
ggplot(q10) + geom_bar(aes(x = res), position = "dodge") + facet_wrap(~opt)
ggplot(q11) + geom_bar(aes(x = res), position = "dodge") + facet_wrap(~opt)
ggplot(q12) + geom_bar(aes(x = res), position = "dodge") + facet_wrap(~opt)
ggplot(q18) + geom_bar(aes(x = res), position = "dodge") + facet_wrap(~opt)
ggplot(q22) + geom_bar(aes(x = res), position = "dodge") + facet_wrap(~opt)
ggplot(q23) + geom_bar(aes(x = res), position = "dodge") + facet_wrap(~opt)
ggplot(q24) + geom_bar(aes(x = res), position = "dodge") + facet_wrap(~opt)







