# Flu working analysis
setwd("~/git/banana-phone/work")
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)
library(foreign)

# weighted
data <- read.csv("surveydata.csv", na = c("#NULL!", "", "Refused"))
# renamed cols
data2 <- read.csv("surveydata2.csv", na = c("#NULL!", ""))
# unweighted
data_unw <- read.spss("datafile.sav", to.data.frame = TRUE)
View(data_unw)


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

# unweighted
with(data_unw, table(Q1))


# those infected + HHC infected
with(data, table(Q2, Q3))
a <- filter(data, Q2=='Yes', Q3=='Yes')
# by gender
with(a, table(PPGENDER))


# example plots
ggplot(data, aes(x = Q1)) + geom_bar()
ggplot(data, aes(x = Q33)) + geom_histogram(binwidth = 2)


# stats



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

###








