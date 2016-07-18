# Flu working analysis
setwd("~/git/banana-phone/work")
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)
#library(foreign)

# weighted
data <- read.csv("data/surveydata.csv", na = c("#NULL!", "", "Refused"))

load("clean/new_name")
load("clean/old_name")
names(data) <- new_name
names(data) <- old_name

# renamed cols
data2 <- read.csv("surveydata2.csv", na = c("#NULL!", "", "Refused"))
# unweighted
data_unw <- read.spss("datafile.sav", to.data.frame = TRUE)



# weighted
with(data, table(Q1))
with(data, prop.table(table(Q1)))

with(data, table(Q1, PPGENDER))
with(data, prop.table(table(Q1, PPGENDER)))

with(data, table(Q1, Q2))
with(data, by(Q1, Q2, summary))
with(data, summary(Q33))
with(Q7, table(q, r))
with(Q7, table(PPGENDER, r, q))

data$weight


# those infected + HHC infected
with(data, table(Q2, Q3))
a <- filter(data, Q2=='Yes', Q3=='Yes')
# by gender
with(a, table(PPGENDER))


# example plots
ggplot(data, aes(x = Q1)) + geom_bar()
ggplot(data, aes(x = Q33)) + geom_histogram(binwidth = 2)


# stats

##################

##### ----- question factors ----- #####

# list factors
q11.lab <- c("High Risk, Very Likely", "Medium Risk, Somewhat Likely",
             "Low Risk, Not Likely", "Don_t Know", "Refused")

# names of factors
(n = data1 %>%
  select(starts_with("Q11_")) %>%
  select(-contains("Text")) %>%
  names())

# list column numbers
grep('Q11', names(data1))

# set factors to columns
data1$Q11_1_Work <- factor(data1$Q11_1_Work, levels = q11.lab, exclude = NA)
data1$Q11_2_Schools <- factor(data1$Q11_2_Schools, levels = q11.lab, exclude = NA)
data1$Q11_3_Day.care <- factor(data1$Q11_3_Day.care, levels = q11.lab, exclude = NA)
data1$Q11_4_Stores <- factor(data1$Q11_4_Stores, levels = q11.lab, exclude = NA)
data1$Q11_5_Restaurants <- factor(data1$Q11_5_Restaurants, levels = q11.lab, exclude = NA)
data1$Q11_6_Libraries <- factor(data1$Q11_6_Libraries, levels = q11.lab, exclude = NA)
data1$Q11_7_Hospitals <- factor(data1$Q11_7_Hospitals, levels = q11.lab, exclude = NA)
data1$Q11_8_Doctor_s.office <- factor(data1$Q11_8_Doctor_s.office, levels = q11.lab, exclude = NA)
data1$Q11_9_Public.transportation <- factor(data1$Q11_9_Public.transportation, levels = q11.lab, exclude = NA)
data1$Q11_10_Family.or.friends <- factor(data1$Q11_10_Family.or.friends, levels = q11.lab, exclude = NA)
data1$Q11_11_Other <- factor(data1$Q11_11_Other, levels = q11.lab, exclude = NA)


##########################



### Q1
# by gender
with(data, table(Q1, PPGENDER))
(q1_gen <- data %>%
  group_by(Q1, PPGENDER) %>%
  count(Q1, PPGENDER)
)
ggplot(q1_gen, aes(x = Q1, y = n, fill = PPGENDER)) +
  geom_bar(stat = 'identity', position = position_dodge())



### Q7
# Q7: What types of public transportation do you regularly use?
# Q7 <- data %>%
#   select(PPGENDER, PPAGE, PPEDUC, PPETHM, PPINCIMP, PPWORK, Q7_1:Q7_otherText) %>%
#   rename("Bus" = Q7_1,
#          "Carpool" = Q7_2,
#          "Subway" = Q7_3,
#          "Train" = Q7_4,
#          "Taxi" = Q7_5,
#          "Airplane" = Q7_6,
#          "Other" = Q7_7,
#          "Refused" = Q7_8,
#          "Other text" = Q7_otherText) %>%
#     gather("q", "r", Bus:Other)

q7_long <- Q7 %>%  # gather() into rows for plotting
  gather("q", "r", 1:7)

q7_long %>%  # save as q7
  group_by(q, r) %>%
  count(q, r)

# all together now:
(q7 <- Q7 %>%
  gather("q", "r", 1:7) %>%  # makes q7_long
  group_by(q, r) %>%  # group
  count(q, r)  # total count
)
ggplot(data = q7[!is.na(q7$r), ], aes(x = q, y = n, fill = r)) +
  geom_bar(stat = 'identity', position = position_dodge())

## gender and Q7:
(q7_gen <- data %>%
  select(PPGENDER, Q7_1:Q7_otherText) %>%
  gather("q", "r", Q7_1:Q7_7) %>%
  group_by(PPGENDER, q, r) %>%
  count(PPGENDER, q, r)
)
ggplot(q7_gen[!is.na(q7_gen$r), ], aes(x = r, y = n, fill = PPGENDER)) +
  geom_bar(stat = 'identity', position = position_dodge()) + facet_wrap(~q)



##### test #####
# rename some columns
# save new data file to use

data2 <- data %>%
  rename("Bus" = Q7_1,
         "Carpool" = Q7_2,
         "Subway" = Q7_3,
         "Train" = Q7_4,
         "Taxi" = Q7_5,
         "Airplane" = Q7_6,
         "Other" = Q7_7,
         "Refused" = Q7_8,
         "Other_text" = Q7_otherText)

(q7_test <- data2 %>%
  select(PPGENDER, 34:42) %>%
  gather("q", "r", Bus:Other) %>%
  group_by(PPGENDER, q, r) %>%
  count(PPGENDER, q, r)
)
ggplot(q7_test[!is.na(q7_test$r), ], aes(x = r, y = n, fill = PPGENDER)) +
  geom_bar(stat = 'identity', position = position_dodge()) + facet_wrap(~q)



################################
# from "flu_draft.R"
################################



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

# all household member questions
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
# q31



##### test code #####
# same as above, code block for q1
q1.df = select(data, Q1)
q1 = gather(q1.df, "q", "r", 1)
summary(q1.df)

q2.df = select(data, Q2)
q2 = gather(q2.df, "q", "r", 1)
summary(q2.df)

q7.df = select(data, Q7_1:Q7_otherText)
names(q7.df) = c("Bus", "Carpool", "Subway",
                 "Train", "Taxi", "Airplane",
                 "Other", "Refused", "Other text")
q7 = gather(q7.df, "q", "r", 1:8)
summary(q7.df)

# ----- alternatively

# pattern <- 'Q7_[1-6]'
# columns <- grep(pattern = pattern, x = names(data))
# q7 <- data[, columns]

# q7_long <- q7 %>%
#   gather('q', 'r', 1:6) %>%
#   group_by(q, r) %>%
#   count(q, r)

# summary(q7)

# ggplot(data=q7_long[!is.na(q7_long$r), ], aes(x=q, y=n, fill=r)) +
#   geom_bar(stat='identity', position=position_dodge())



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





