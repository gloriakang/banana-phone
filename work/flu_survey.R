library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)

data = as.data.frame(read_csv("surveydata.csv", na = '#NULL!'))


# Q7

Q7 = data[ ,substr(names(data), 1, 2) == 'Q7']
head(Q7); ncol(Q7)

names(Q7) <- c(
  Q7_1 = "Bus",
  Q7_2 = "Carpool",
  Q7_3 = "Subway",
  Q7_4 = "Train",
  Q7_5 = "Taxi",
  Q7_6 = "Airplane",
  Q7_7 = "Other",
  Q7_8 = "Refused",
  Q7_otherText = "Other")
str(Q7)

pattern <- 'Q7_[1-6]'
columns <- grep(pattern = pattern, x = names(data))
q7 <- data[, columns]


q7_long <- q7 %>%
  gather('q', 'r', 1:6) %>%
  group_by(q, r) %>%
  count(q, r)

summary(q7)

ggplot(data=q7_long[!is.na(q7_long$r), ], aes(x=q, y=n, fill=r)) +
         geom_bar(stat='identity', position=position_dodge())



#likert(q7)
#summary(lQ7)

# subset of Q7 = lQ7
#lQ7 = Q7[ ,1:6]
#head(lQ7); ncol(lQ7)



lQ7 = likert(Q7[ ,1:3], grouping = data$PPGENDER)


likert(lQ7)

summary(lQ7, center=1.5)
summary(lQ7, center=2)



# --------
# tables
# ppeduc = as.data.frame(table(ds$PPEDUC))
# q111 = as.data.frame(table(ds$Q11_1))

# demographics
# a = ggplot(ds) + scale_fill_brewer()
# a + geom_bar(mapping = aes(x = ppagect4, fill = ppagect4)) + xlab("age group")
# a + geom_bar(mapping = aes(x = PPEDUCAT, fill = PPEDUCAT)) + xlab("education")
# a + geom_bar(mapping = aes(x = PPETHM, fill = PPETHM)) + xlab("ethnicity")
# a + geom_bar(mapping = aes(x = PPGENDER, fill = PPGENDER)) + xlab("gender")

# C
#a + geom_bar(mapping = aes(x = Q11_1, binwidth=.5, position = "identity")) + xlab("work")
#a + geom_bar(mapping = aes(x = Q11_1, fill = Q11_1, position = position_dodge()))


