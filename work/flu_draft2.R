# flu survey analysis

setwd("~/git/banana-phone/work")
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)

data = read.csv("surveydata.csv", na = '#NULL!')
str(data)
View(data)

# subsetting with dplyr
names(data)

q1.df = select(data, Q1)
q2.df = select(data, Q2)
q3.df = select(data, Q3)
q4.df = select(data, Q4)
q5.df = select(data, Q5)
q6.df = select(data, Q6)
q7.df = select(data, Q7_1:Q7_otherText)
q8.df = select(data, Q8_1:Q8_otherText)
q10.df = select(data, Q10_1:Q10_otherText)
q11.df = select(data, Q11_1:Q11_OtherText_Codes)
q12.df = select(data, Q12_1:Q12_OtherText_Codes)
q13.df = select(data, Q13)
q18.df = select(data, Q18_1:Q18_otherText)
q22.df = select(data, Q22_1:Q22_OtherText_Codes)
q23.df = select(data, Q23_1:Q23_OtherText_Codes)
# RI_Grid?
q24.df = select(data, Q24_1:Q24_otherText)
q25.df = select(data, Q25_1:Q25_OtherText_Codes)
q27.df = select(data, Q27_1:Q27_OtherText_Codes)
q29.df = select(data, Q29_1:Q29_OtherText_Codes)
q30.df = select(data, Q30_1:Q30_OtherText_Codes)
q31.df = select(data, Q31:Q31_Codes)
q32.df = select(data, Q32:Q32_Codes)
q33.df = select(data, Q33:Q33_Codes)

# household member questions
hh.df = select(data, PRG_Q34_01:Q50)
summary(hh.df)

# HHM1
hhm1.df = select(data, Q35:Q42)
q35.df = select(data, Q35)
q36.df = select(data, Q36)
q37.df = select(data, Q37)
q38.df = select(data, Q38)
q39.df = select(data, Q39)
q40.df = select(data, Q40)
q41.df = select(data, Q41)
q42.df = select(data, Q42)

# HHM2
hhm2.df = select(data, Q43:Q50)
q43.df = select(data, Q43)
q44.df = select(data, Q44)
q45.df = select(data, Q45)
q46.df = select(data, Q46)
q47.df = select(data, Q47)
q48.df = select(data, Q48)
q49.df = select(data, Q49)
q50.df = select(data, Q50)

# gather with tidyr into columns
q7 = gather(q7.df, "opt", "resp", 1:7)
q8 = gather(q8.df, "opt", "resp", 1:7)
q10 = gather(q10.df, "opt", "resp", 1:9)
q11 = gather(q11.df, "opt", "resp", 1:11)
q12 = gather(q12.df, "opt", "resp", 1:15)
q18 = gather(q18.df, "opt", "resp", 1:11)
q22 = gather(q22.df, "opt", "resp", 1:9)
q23 = gather(q23.df, "opt", "resp", 1:11)
q24 = gather(q24.df, "opt", "resp", 1:7)
q25 = gather(q25.df, "opt", "resp", 1:11)
q27 = gather(q27.df, "opt", "resp",  1:4)
q29 = gather(q29.df, "opt", "resp", 1:6)
q30 = gather(q30.df, "opt", "resp", 1:6)



# ----- summary tables & plots
# q1 did you know influenza is different from the stomach flu?
summary(q1.df)

# q13 do you get the flu vaccine?
summary(q13.df)

a = with(data, table(Q1, Q13)); a
summary(a)

# q2
summary(q2.df)

# q3
summary(q3.df)

# q7
ggplot(q7) + geom_bar(aes(x = resp, fill = resp), position = "dodge") + facet_wrap(~opt)
summary(q7.df)

# q8
ggplot(q8) + geom_bar(aes(x = resp, fill = resp), position = "dodge") + facet_wrap(~opt)
summary(q8.df)

# q10 types of transportation
ggplot(q10) + geom_bar(aes(x = resp, fill = resp), position = "dodge") + facet_wrap(~opt)
summary(q10.df)

# q11 how do you rate your risk of getting influenza at the following locations?
ggplot(q11) + geom_bar(aes(x = resp, fill = resp), position = "dodge") + facet_wrap(~opt)
summary(q11.df)

# q12
ggplot(q12) + geom_bar(aes(x = resp, fill = resp), position = "dodge") + facet_wrap(~opt)
summary(q12.df)

# q18
ggplot(q18) + geom_bar(aes(x = resp, fill = resp), position = "dodge") + facet_wrap(~opt)
summary(q18.df)

# q22
ggplot(q22) + geom_bar(aes(x = resp, fill = resp), position = "dodge") + facet_wrap(~opt)
summary(q22.df)

# q23
ggplot(q23) + geom_bar(aes(x = resp, fill = resp), position = "dodge") + facet_wrap(~opt)
summary(q23.df)

ggplot(q24) + geom_bar(aes(x = resp, fill = resp), position = "dodge") + facet_wrap(~opt)
summary(q24.df)

summary(hhm1.df)
summary(hhm2.df)


# ---
# need to remove na
# ggplot(q7) + geom_bar(aes(x = opt, fill = resp), position = "dodge")
# ggplot(data = data) + geom_bar(mapping = aes(x = Q1)) + stat_sum()

