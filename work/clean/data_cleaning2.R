# Data cleaning part 2
# load renamed file, switch back to old names, set/recode factors
# apply new names, save as data2
# output = cleaning_all.Rdata

#setwd("~/git/banana-phone/work")
rm(list = ls(all.names = TRUE))

library(car)

# load data_rename
load("clean/cleaning1.RData")
#load("~/git/banana-phone/work/clean/cleaning1.RData")

data <- read.csv("clean/data_rename.csv", na = c("#NULL!", "", "Refused", "NA"), stringsAsFactors = F)
dataf <- read.csv("clean/data_rename.csv", na = c("#NULL!", "", "Refused", "NA"))  #  with factors

# use old names
names(data) <- old_name
names(dataf) <- old_name


############### list and sort factors ################

# age cat 7
unique(data$ppagecat)
dataf$ppagecat <- as.factor(data$ppagecat)
summary(dataf$ppagecat)


# age cat 4
unique(data$ppagect4)
dataf$ppagect4 <- as.factor(data$ppagect4)
summary(dataf$ppagect4)


# education cat 14
unique(data$PPEDUC)
PPEDUC.lab <- c("No formal education", "1st, 2nd, 3rd, or 4th grade", "5th or 6th grade",
                "7th or 8th grade", "9th grade", "10th grade",
                "11th grade", "12th grade NO DIPLOMA",
                "HIGH SCHOOL GRADUATE - high school DIPLOMA or the equivalent (GED)",
                "Some college, no degree", "Associate degree", "Bachelors degree",
                "Masters degree", "Professional or Doctorate degree")
dataf$PPEDUC <- factor(data$PPEDUC, levels = PPEDUC.lab)
summary(dataf$PPEDUC)


# education cat 4
unique(data$PPEDUCAT)
table(data$PPEDUCAT)
PPEDUCAT.lab <- c("Less than high school", "High school", "Some college", "Bachelor_s degree or higher")
dataf$PPEDUCAT <- factor(dataf$PPEDUCAT, levels = PPEDUCAT.lab)
summary(dataf$PPEDUCAT)


# ethnicity cat 5
unique(data$PPETHM)
PPETHM.lab <- c("White, Non-Hispanic", "Black, Non-Hispanic",
                "Hispanic", "Other, Non-Hispanic", "2+ Races, Non-Hispanic")
dataf$PPETHM <- factor(dataf$PPETHM, levels = PPETHM.lab)
summary(dataf$PPETHM)


# gender
dataf$PPGENDER <- relevel(dataf$PPGENDER, "Female")
str(dataf$PPGENDER)


# head of household
dataf$PPHHHEAD <- relevel(dataf$PPHHHEAD, "Yes")
summary(dataf$PPHHHEAD)


# income cat 19
unique(data$PPINCIMP)
PPINCIMP.lab <- c("Less than $5,000", "$5,000 to $7,499", "$7,500 to $9,999",
                  "$10,000 to $12,499", "$12,500 to $14,999", "$15,000 to $19,999",
                  "$20,000 to $24,999", "$25,000 to $29,999", "$30,000 to $34,999",
                  "$35,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999",
                  "$60,000 to $74,999", "$75,000 to $84,999", "$85,000 to $99,999",
                  "$100,000 to $124,999", "$125,000 to $149,999", "$150,000 to $174,999",
                  "$175,000 or more")
dataf$PPINCIMP <- factor(data$PPINCIMP, levels = PPINCIMP.lab)
summary(dataf$PPINCIMP)


# marital status cat 6
unique(data$PPMARIT)
PPMARIT.lab <- c("Never married", "Living with partner", "Married",
                 "Separated", "Divorced", "Widowed")
dataf$PPMARIT <- factor(data$PPMARIT, levels = PPMARIT.lab)
summary(dataf$PPMARIT)

# rent
summary(dataf$PPRENT)

# location
summary(dataf$PPSTATEN)

# employment status
unique(data$PPWORK)
dataf$PPWORK <- as.factor(dataf$PPWORK)
summary(dataf$PPWORK)

# internet status
dataf$PPNET <- relevel(dataf$PPNET, "Yes")
summary(dataf$PPNET)


################# question factors ####################
yn.lab <- c("Yes", "No")
yesnodk.lab <- c("Yes", "No", "Don_t know")
q11.lab <- c("High Risk, Very Likely", "Medium Risk, Somewhat Likely",
             "Low Risk, Not Likely", "Don_t Know")
always.lab <- c("Always", "Sometimes", "Never")
likely.lab <- c("Yes, more likely", "No, no effect", "No, less likely")


dataf$Q1 <- factor(data$Q1, levels = yn.lab)
dataf$Q2 <- relevel(dataf$Q2, "Yes")
dataf$Q3 <- factor(data$Q3, levels = yesnodk.lab)
levels(dataf$Q3)
dataf$Q4 <- relevel(dataf$Q4, "Yes")
dataf$Q5 <- relevel(dataf$Q5, "Yes")
dataf$Q6 <- relevel(dataf$Q6, "Yes")
dataf$Q7_1 <- relevel(dataf$Q7_1, "Yes")
dataf$Q7_2 <- relevel(dataf$Q7_2, "Yes")
dataf$Q7_3 <- relevel(dataf$Q7_3, "Yes")
dataf$Q7_4 <- relevel(dataf$Q7_4, "Yes")
dataf$Q7_5 <- relevel(dataf$Q7_5, "Yes")
dataf$Q7_6 <- relevel(dataf$Q7_6, "Yes")
dataf$Q7_7 <- relevel(dataf$Q7_7, "Yes")
dataf$Q7_8 <- relevel(dataf$Q7_8, "Yes")
dataf$Q8_1 <- relevel(dataf$Q8_1, "Yes")
dataf$Q8_2 <- relevel(dataf$Q8_2, "Yes")
dataf$Q8_3 <- relevel(dataf$Q8_3, "Yes")
dataf$Q8_4 <- relevel(dataf$Q8_4, "Yes")
dataf$Q8_5 <- relevel(dataf$Q8_5, "Yes")
dataf$Q8_6 <- relevel(dataf$Q8_6, "Yes")
dataf$Q8_7 <- relevel(dataf$Q8_7, "Yes")
dataf$Q9 <- factor(data$Q9, levels = yesnodk.lab)
levels(dataf$Q9)
dataf$Q10_1 <- relevel(dataf$Q10_1, "Yes")
dataf$Q10_2 <- relevel(dataf$Q10_2, "Yes")
dataf$Q10_3 <- relevel(dataf$Q10_3, "Yes")
dataf$Q10_4 <- relevel(dataf$Q10_4, "Yes")
dataf$Q10_5 <- relevel(dataf$Q10_5, "Yes")
dataf$Q10_6 <- relevel(dataf$Q10_6, "Yes")
dataf$Q10_7 <- relevel(dataf$Q10_7, "Yes")
dataf$Q10_8 <- relevel(dataf$Q10_8, "Yes")
dataf$Q10_9 <- relevel(dataf$Q10_9, "Yes")
levels(dataf$Q10_9)
dataf$Q11_1 <- factor(data$Q11_1, levels = q11.lab)
dataf$Q11_2 <- factor(data$Q11_2, levels = q11.lab)
dataf$Q11_3 <- factor(data$Q11_3, levels = q11.lab)
dataf$Q11_4 <- factor(data$Q11_4, levels = q11.lab)
dataf$Q11_5 <- factor(data$Q11_5, levels = q11.lab)
dataf$Q11_6 <- factor(data$Q11_6, levels = q11.lab)
dataf$Q11_7 <- factor(data$Q11_7, levels = q11.lab)
dataf$Q11_8 <- factor(data$Q11_8, levels = q11.lab)
dataf$Q11_9 <- factor(data$Q11_9, levels = q11.lab)
dataf$Q11_10 <- factor(data$Q11_10, levels = q11.lab)
dataf$Q11_11 <- factor(data$Q11_11, levels = q11.lab)
levels(dataf$Q11_1)

dataf$Q12_1 <- factor(data$Q12_1, levels = always.lab)
dataf$Q12_2 <- factor(data$Q12_2, levels = always.lab)
dataf$Q12_3 <- factor(data$Q12_3, levels = always.lab)
dataf$Q12_4 <- factor(data$Q12_4, levels = always.lab)
dataf$Q12_5 <- factor(data$Q12_5, levels = always.lab)
dataf$Q12_6 <- factor(data$Q12_6, levels = always.lab)
dataf$Q12_7 <- factor(data$Q12_7, levels = always.lab)
dataf$Q12_8 <- factor(data$Q12_8, levels = always.lab)
dataf$Q12_9 <- factor(data$Q12_9, levels = always.lab)
dataf$Q12_10 <- factor(data$Q12_10, levels = always.lab)
dataf$Q12_11 <- factor(data$Q12_11, levels = always.lab)
dataf$Q12_12 <- factor(data$Q12_12, levels = always.lab)
dataf$Q12_13 <- factor(data$Q12_13, levels = always.lab)
dataf$Q12_14 <- factor(data$Q12_14, levels = always.lab)
dataf$Q12_15 <- factor(data$Q12_15, levels = always.lab)
levels(dataf$Q12_15)

dataf$Q13 <- factor(data$Q13, levels = c("Yes, every year", "Yes, some years", "No, never"))
dataf$Q14 <- factor(data$Q14, levels = c("$0", "Less than $30", "$30 to $60", "More than $60", "Don_t know"))
dataf$Q15 <- factor(data$Q15, levels = likely.lab)
dataf$Q16 <- factor(data$Q16, levels = likely.lab)

dataf$Q18_1 <- relevel(dataf$Q18_1, "Yes")
dataf$Q18_2 <- relevel(dataf$Q18_2, "Yes")
dataf$Q18_3 <- relevel(dataf$Q18_3, "Yes")
dataf$Q18_4 <- relevel(dataf$Q18_4, "Yes")
dataf$Q18_5 <- relevel(dataf$Q18_5, "Yes")
dataf$Q18_6 <- relevel(dataf$Q18_6, "Yes")
dataf$Q18_7 <- relevel(dataf$Q18_7, "Yes")
dataf$Q18_8 <- relevel(dataf$Q18_8, "Yes")
dataf$Q18_9 <- relevel(dataf$Q18_9, "Yes")
dataf$Q18_10 <- relevel(dataf$Q18_10, "Yes")
dataf$Q18_11 <- relevel(dataf$Q18_11, "Yes")
levels(dataf$Q18_1)

dataf$Q19 <- relevel(dataf$Q19, "Yes")
dataf$Q20 <- factor(data$Q20, levels = c("Very effective", "Somewhat effective", "It varies from season to season", "Not effective", "Don_t know"))
dataf$Q21 <- factor(data$Q21, levels = c("Yes, the full cost is paid", "Yes, but only part of the cost is paid", "No", "Don_t know"))
dataf$Q22_1 <- factor(data$Q22_1, levels = always.lab)
dataf$Q22_2 <- factor(data$Q22_2, levels = always.lab)
dataf$Q22_3 <- factor(data$Q22_3, levels = always.lab)
dataf$Q22_4 <- factor(data$Q22_4, levels = always.lab)
dataf$Q22_5 <- factor(data$Q22_5, levels = always.lab)
dataf$Q22_6 <- factor(data$Q22_6, levels = always.lab)
dataf$Q22_7 <- factor(data$Q22_7, levels = always.lab)
dataf$Q22_8 <- factor(data$Q22_8, levels = always.lab)
dataf$Q22_9 <- factor(data$Q22_9, levels = always.lab)
levels(dataf$Q22_9)

dataf$Q23_1 <- factor(dataf$Q23_1, levels = always.lab)
dataf$Q23_2 <- factor(dataf$Q23_2, levels = always.lab)
dataf$Q23_3 <- factor(dataf$Q23_3, levels = always.lab)
dataf$Q23_4 <- factor(dataf$Q23_4, levels = always.lab)
dataf$Q23_5 <- factor(dataf$Q23_5, levels = always.lab)
dataf$Q23_6 <- factor(dataf$Q23_6, levels = always.lab)
dataf$Q23_7 <- factor(dataf$Q23_7, levels = always.lab)
dataf$Q23_8 <- factor(dataf$Q23_8, levels = always.lab)
dataf$Q23_9 <- factor(dataf$Q23_9, levels = always.lab)
dataf$Q23_10 <- factor(dataf$Q23_10, levels = always.lab)
dataf$Q23_11 <- factor(dataf$Q23_11, levels = always.lab)
levels(dataf$Q23_11)

dataf$Q24_1 <- relevel(dataf$Q24_1, "Yes")
dataf$Q24_2 <- relevel(dataf$Q24_2, "Yes")
dataf$Q24_3 <- relevel(dataf$Q24_3, "Yes")
dataf$Q24_4 <- relevel(dataf$Q24_4, "Yes")
dataf$Q24_5 <- relevel(dataf$Q24_5, "Yes")
dataf$Q24_6 <- relevel(dataf$Q24_6, "Yes")
dataf$Q24_7 <- relevel(dataf$Q24_7, "Yes")
levels(dataf$Q24_7)

dataf$Q25_1 <- factor(dataf$Q23_1, levels = always.lab)
dataf$Q25_2 <- factor(dataf$Q23_2, levels = always.lab)
dataf$Q25_3 <- factor(dataf$Q23_3, levels = always.lab)
dataf$Q25_4 <- factor(dataf$Q23_4, levels = always.lab)
dataf$Q25_5 <- factor(dataf$Q23_5, levels = always.lab)
dataf$Q25_6 <- factor(dataf$Q23_6, levels = always.lab)
dataf$Q25_7 <- factor(dataf$Q23_7, levels = always.lab)
dataf$Q25_8 <- factor(dataf$Q23_8, levels = always.lab)
dataf$Q25_9 <- factor(dataf$Q23_9, levels = always.lab)
dataf$Q25_10 <- factor(dataf$Q23_10, levels = always.lab)
dataf$Q25_11 <- factor(dataf$Q23_11, levels = always.lab)
levels(dataf$Q25_11)

dataf$Q26 <- relevel(dataf$Q26, "Yes")

dataf$Q27_1 <- factor(dataf$Q27_1, levels = always.lab)
dataf$Q27_2 <- factor(dataf$Q27_2, levels = always.lab)
dataf$Q27_3 <- factor(dataf$Q27_3, levels = always.lab)
dataf$Q27_4 <- factor(dataf$Q27_4, levels = always.lab)
levels(dataf$Q27_4)

dataf$Q28 <- relevel(dataf$Q28, "Yes")

dataf$Q29_1 <- factor(dataf$Q29_1, levels = always.lab)
dataf$Q29_2 <- factor(dataf$Q29_2, levels = always.lab)
dataf$Q29_3 <- factor(dataf$Q29_3, levels = always.lab)
dataf$Q29_4 <- factor(dataf$Q29_4, levels = always.lab)
dataf$Q29_5 <- factor(dataf$Q29_5, levels = always.lab)
dataf$Q29_6 <- factor(dataf$Q29_6, levels = always.lab)
levels(dataf$Q29_1)

dataf$Q30_1 <- factor(dataf$Q30_1, levels = always.lab)
dataf$Q30_2 <- factor(dataf$Q30_2, levels = always.lab)
dataf$Q30_3 <- factor(dataf$Q30_3, levels = always.lab)
dataf$Q30_4 <- factor(dataf$Q30_4, levels = always.lab)
dataf$Q30_5 <- factor(dataf$Q30_5, levels = always.lab)
dataf$Q30_6 <- factor(dataf$Q30_6, levels = always.lab)
levels(dataf$Q30_1)

#View(dataf[188:288])

dataf$Q39 <- factor(dataf$Q39, levels = c("Never", "Less than once per year", "Once per year", "More than once per year", "Don_t know"))
levels(dataf$Q39)

dataf$Q40 <- factor(dataf$Q40, levels = c("Never", "1 to 2 times", "3 to 5 times", "6 to 10 times", "More than 10", "Don_t know"))
levels(dataf$Q40)

dataf$Q41 <- factor(dataf$Q41, levels = c("Never", "Once", "2 times", "3 times", "More than 3", "Don_t know"))
levels(dataf$Q41)

dataf$Q42 <- factor(dataf$Q42, levels = c("Yes, always", "Yes, sometimes", "No, never", "Don_t know"))
levels(dataf$Q42)

#
dataf$Q47 <- factor(dataf$Q47, levels = c("Never", "Less than once per year", "Once per year", "More than once per year", "Don_t know"))
levels(dataf$Q47)

dataf$Q48 <- factor(dataf$Q48, levels = c("Never", "1 to 2 times", "3 to 5 times", "6 to 10 times", "More than 10", "Don_t know"))
levels(dataf$Q48)

dataf$Q49 <- factor(dataf$Q49, levels = c("Never", "Once", "2 times", "3 times", "More than 3", "Don_t know"))
levels(dataf$Q49)

dataf$Q50 <- factor(dataf$Q50, levels = c("Yes, always", "Yes, sometimes", "No, never", "Don_t know"))
levels(dataf$Q50)


## save
# data = original, dataf = cleaned factors, data2 = copy of dataf
data2 <- dataf
save(data2, new_name, old_name, file = "clean/cleaning2.RData")



