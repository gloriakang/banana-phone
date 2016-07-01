# Data cleaning part 2
# load renamed file, switch back to old names, 


# load new_name data
setwd("~/git/banana-phone/work")
load("clean/cleaning1.Rdata")
data <- read.csv("clean/data_new_name.csv", na = c("#NULL!", "", "Refused", "NA"))

# use old names
names(data) <- old_name
data1 <- data


##### ----- create factors ----- #####
#
levels(data1$ppagecat)
ppagecat.lab <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
data1$ppagecat <- ordered(data$ppagecat, levels = ppagecat.lab)

#
levels(data1$ppagect4)
ppagect4.lab <- c("18-29", "30-44", "45-59", "60+")
data1$ppagect4 <- ordered(data$ppagect4, levels = ppagect4.lab)

#
levels(data1$PPEDUC)
PPEDUC.lab <- c("No formal education", "1st, 2nd, 3rd, or 4th grade", "5th or 6th grade",
                "7th or 8th grade", "9th grade", "10th grade",
                "11th grade", "12th grade NO DIPLOMA", "HIGH SCHOOL GRADUATE or GED",
                "Some college, no degree", "Associate degree", "Bachelors degree",
                "Masters degree", "Professional or Doctorate degree")
data1$PPEDUC <- ordered(data$PPEDUC, levels = PPEDUC.lab)

#
levels(data1$PPEDUCAT)
PPEDUCAT.lab <- c("Less than HS", "HS", "Some college", "Bachelor or higher")
data1$PPEDUCAT <- ordered(data$PPEDUCAT, levels = PPEDUCAT.lab)

#
levels(data1$PPETHM)
PPETHM.lab <- c("White, Non-Hispanic", "Black or African American, Non-Hispanic",
                "Other, Non-Hispanic", "Hispanic", "2+ races, Non-Hispanic")
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
data1$PPINCIMP <- ordered(data$PPINCIMP, levels = PPINCIMP.lab)

#
levels(data1$PPMARIT)
PPMARIT.lab <- c("Married", "Widowed", "Divorced",
                 "Separated", "Never married", "Living with partner")
data1$PPMARIT <- factor(data$PPMARIT, levels = PPMARIT.lab)


##### ----- question factors ----- #####

# note: names of factors should be old names
#data1 %>%
#  select(starts_with("Q11_")) %>%
#  select(-contains("Text")) %>%  # prob don't need this
#  names()


q11.lab <- c("High Risk, Very Likely", "Medium Risk, Somewhat Likely",
             "Low Risk, Not Likely", "Don_t Know")
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

data1$Q13 <- factor(data1$Q13, levels = c("Yes, every year", "Yes, some years", "No, never"))

data1$Q14 <- factor(data1$Q14, levels = c("$0", "Less than $30", "$30 to $60", "More than $60", "Don_t know"))

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

data2 <- data1
save(data2, new_name, old_name, file = "clean/all.Rdata")





