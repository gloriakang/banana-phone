# Data cleaning part 2
# load renamed file, switch back to old names, list factors
# to do: save copy of data2 as numeric
# apply new names, save as data2
# output = cleaning_all.Rdata
setwd("~/git/banana-phone/work")
rm(list=ls(all.names=TRUE))

# load new_name data
load('clean/cleaning1.Rdata')
data <- read.csv("clean/data_new_name.csv", na = c("#NULL!", "", "Refused", "NA"))

# use old names
names(data) <- old_name
data1 <- data



##### ----- create factors ----- #####
#
#levels(data1$ppagecat)
#ppagecat.lab <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
#data1$ppagecat <- ordered(data$ppagecat, levels = ppagecat.lab)

#
#levels(data1$ppagect4)
#ppagect4.lab <- c("18-29", "30-44", "45-59", "60+")
#data1$ppagect4 <- ordered(data$ppagect4, levels = ppagect4.lab)

#
levels(data1$PPEDUC)
PPEDUC.lab <- c("No formal education", "1st, 2nd, 3rd, or 4th grade", "5th or 6th grade",
                "7th or 8th grade", "9th grade", "10th grade",
                "11th grade", "12th grade NO DIPLOMA",
                "HIGH SCHOOL GRADUATE - high school DIPLOMA or the equivalent (GED)",
                "Some college, no degree", "Associate degree", "Bachelors degree",
                "Masters degree", "Professional or Doctorate degree")
data1$PPEDUC <- factor(data$PPEDUC, levels = PPEDUC.lab)

#
levels(data1$PPEDUCAT)
PPEDUCAT.lab <- c("Less than high school", "High school", "Some college", "Bachelor_s degree or higher")
data1$PPEDUCAT <- factor(data$PPEDUCAT, levels = PPEDUCAT.lab)

#
levels(data1$PPETHM)
PPETHM.lab <- c("White, Non-Hispanic", "Black, Non-Hispanic",
                "Hispanic", "Other, Non-Hispanic", "2+ Races, Non-Hispanic")
data1$PPETHM <- factor(data$PPETHM, levels = PPETHM.lab)


#
levels(data1$PPINCIMP)
PPINCIMP.lab <- c("Less than $5,000", "$5,000 to $7,499", "$7,500 to $9,999",
                  "$10,000 to $12,499", "$12,500 to $14,999", "$15,000 to $19,999",
                  "$20,000 to $24,999", "$25,000 to $29,999", "$30,000 to $34,999",
                  "$35,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999",
                  "$60,000 to $74,999", "$75,000 to $84,999", "$85,000 to $99,999",
                  "$100,000 to $124,999", "$125,000 to $149,999", "$150,000 to $174,999",
                  "$175,000 or more")
data1$PPINCIMP <- factor(data$PPINCIMP, levels = PPINCIMP.lab)

#
levels(data1$PPMARIT)
PPMARIT.lab <- c("Never married", "Living with partner", "Married",
                 "Separated", "Divorced", "Widowed")
data1$PPMARIT <- factor(data$PPMARIT, levels = PPMARIT.lab)


##### ----- question factors ----- #####


yesnodk.lab <- c("Yes", "No", "Don_t know")
q11.lab <- c("High Risk, Very Likely", "Medium Risk, Somewhat Likely",
             "Low Risk, Not Likely", "Don_t Know")
always.lab <- c("Always", "Sometimes", "Never")
likely.lab <- c("Yes, more likely", "No, no effect", "No, less likely")


data1$Q1 <- relevel(data1$Q1, "Yes")
data1$Q2 <- relevel(data1$Q2, "Yes")
data1$Q3 <- factor(data1$Q3, levels = yesnodk.lab)
levels(data1$Q3)
data1$Q4 <- relevel(data1$Q4, "Yes")
data1$Q5 <- relevel(data1$Q5, "Yes")
data1$Q6 <- relevel(data1$Q6, "Yes")
data1$Q7_1 <- relevel(data1$Q7_1, "Yes")
data1$Q7_2 <- relevel(data1$Q7_2, "Yes")
data1$Q7_3 <- relevel(data1$Q7_3, "Yes")
data1$Q7_4 <- relevel(data1$Q7_4, "Yes")
data1$Q7_5 <- relevel(data1$Q7_5, "Yes")
data1$Q7_6 <- relevel(data1$Q7_6, "Yes")
data1$Q7_7 <- relevel(data1$Q7_7, "Yes")
data1$Q7_8 <- relevel(data1$Q7_8, "Yes")
data1$Q8_1 <- relevel(data1$Q8_1, "Yes")
data1$Q8_2 <- relevel(data1$Q8_2, "Yes")
data1$Q8_3 <- relevel(data1$Q8_3, "Yes")
data1$Q8_4 <- relevel(data1$Q8_4, "Yes")
data1$Q8_5 <- relevel(data1$Q8_5, "Yes")
data1$Q8_6 <- relevel(data1$Q8_6, "Yes")
data1$Q8_7 <- relevel(data1$Q8_7, "Yes")
data1$Q9 <- factor(data1$Q9, levels = yesnodk.lab)
levels(data1$Q9)
data1$Q10_1 <- relevel(data1$Q10_1, "Yes")
data1$Q10_2 <- relevel(data1$Q10_2, "Yes")
data1$Q10_3 <- relevel(data1$Q10_3, "Yes")
data1$Q10_4 <- relevel(data1$Q10_4, "Yes")
data1$Q10_5 <- relevel(data1$Q10_5, "Yes")
data1$Q10_6 <- relevel(data1$Q10_6, "Yes")
data1$Q10_7 <- relevel(data1$Q10_7, "Yes")
data1$Q10_8 <- relevel(data1$Q10_8, "Yes")
data1$Q10_9 <- relevel(data1$Q10_9, "Yes")
levels(data1$Q10_9)
data1$Q11_1 <- factor(data1$Q11_1, levels = q11.lab)
data1$Q11_2 <- factor(data1$Q11_2, levels = q11.lab)
data1$Q11_3 <- factor(data1$Q11_3, levels = q11.lab)
data1$Q11_4 <- factor(data1$Q11_4, levels = q11.lab)
data1$Q11_5 <- factor(data1$Q11_5, levels = q11.lab)
data1$Q11_6 <- factor(data1$Q11_6, levels = q11.lab)
data1$Q11_7 <- factor(data1$Q11_7, levels = q11.lab)
data1$Q11_8 <- factor(data1$Q11_8, levels = q11.lab)
data1$Q11_9 <- factor(data1$Q11_9, levels = q11.lab)
data1$Q11_10 <- factor(data1$Q11_10, levels = q11.lab)
data1$Q11_11 <- factor(data1$Q11_11, levels = q11.lab)
data1$Q12_1 <- factor(data1$Q12_1, levels = always.lab)
data1$Q12_2 <- factor(data1$Q12_2, levels = always.lab)
data1$Q12_3 <- factor(data1$Q12_3, levels = always.lab)
data1$Q12_4 <- factor(data1$Q12_4, levels = always.lab)
data1$Q12_5 <- factor(data1$Q12_5, levels = always.lab)
data1$Q12_6 <- factor(data1$Q12_6, levels = always.lab)
data1$Q12_7 <- factor(data1$Q12_7, levels = always.lab)
data1$Q12_8 <- factor(data1$Q12_8, levels = always.lab)
data1$Q12_9 <- factor(data1$Q12_9, levels = always.lab)
data1$Q12_10 <- factor(data1$Q12_10, levels = always.lab)
data1$Q12_11 <- factor(data1$Q12_11, levels = always.lab)
data1$Q12_12 <- factor(data1$Q12_12, levels = always.lab)
data1$Q12_13 <- factor(data1$Q12_13, levels = always.lab)
data1$Q12_14 <- factor(data1$Q12_14, levels = always.lab)
data1$Q12_15 <- factor(data1$Q12_15, levels = always.lab)
levels(data1$Q12_15)
data1$Q13 <- factor(data1$Q13, levels = c("Yes, every year", "Yes, some years", "No, never"))
data1$Q14 <- factor(data1$Q14, levels = c("$0", "Less than $30", "$30 to $60", "More than $60", "Don_t know"))
data1$Q15 <- factor(data1$Q15, levels = likely.lab)
data1$Q16 <- factor(data1$Q16, levels = likely.lab)
data1$Q18_1 <- relevel(data1$Q18_1, "Yes")
data1$Q18_2 <- relevel(data1$Q18_2, "Yes")
data1$Q18_3 <- relevel(data1$Q18_3, "Yes")
data1$Q18_4 <- relevel(data1$Q18_4, "Yes")
data1$Q18_5 <- relevel(data1$Q18_5, "Yes")
data1$Q18_6 <- relevel(data1$Q18_6, "Yes")
data1$Q18_7 <- relevel(data1$Q18_7, "Yes")
data1$Q18_8 <- relevel(data1$Q18_8, "Yes")
data1$Q18_9 <- relevel(data1$Q18_9, "Yes")
data1$Q18_10 <- relevel(data1$Q18_10, "Yes")
data1$Q18_11 <- relevel(data1$Q18_11, "Yes")
levels(data1$Q18_1)

data1$Q19 <- relevel(data1$Q19, "Yes")
data1$Q20 <- factor(data1$Q20, levels = c("Very effective", "Somewhat effective", "It varies from season to season", "Not effective", "Don_t know"))
data1$Q21 <- factor(data1$Q21, levels = c("Yes, the full cost is paid", "Yes, but only part of the cost is paid", "No", "Don_t know"))





data1$Q39 <- factor(data1$Q39, levels = c("More than once per year", "Once per year", "Less than once per year", "Never", "Don_t know"))
data1$Q40 <- factor(data1$Q40, levels = c("Never", "1 to 2 times", "3 to 5 times", "6 to 10 times", "More than 10", "Don_t know"))
data1$Q41 <- factor(data1$Q41, levels = c("Never", "Once", "2 times", "3 times", "More than 3", "Don_t know"))
data1$Q42 <- factor(data1$Q42, levels = c("Yes, always", "Yes, sometimes", "No, never", "Don_t know"))




data1$Q47 <- factor(data1$Q47, levels = c("More than once per year", "Once per year", "Less than once per year", "Never", "Don_t know"))
data1$Q48 <- factor(data1$Q48, levels = c("Never", "1 to 2 times", "3 to 5 times", "6 to 10 times", "More than 10", "Don_t know"))
levels(data1$Q48)
data1$Q49 <- factor(data1$Q49, levels = c("Never", "Once", "2 times", "3 times", "More than 3", "Don_t know"))
data1$Q50 <- factor(data1$Q50, levels = c("Yes, always", "Yes, sometimes", "No, never", "Don_t know"))



##### ----- save r object ----- #####
# apply new names
names(data1) <- new_name
# save as data2
data2 <- data1
save(data2, new_name, old_name, file = "clean/cleaning_all.Rdata")





