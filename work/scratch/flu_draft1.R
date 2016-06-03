# draft
library(dplyr)
library(ggplot2)

data = read.csv("surveydata.csv")


table(data$Q1) %>% barplot()

with(data, summary(Q11_1))
with(data, table(Q11_1, Q11_2))


# deleting columns from a dataframe
dat = subset(dat, select = -badcol)

# renaming columns
names(dat) = c("name1", "name2", "name3")

# reordering columns in a dataframe
dat = dat[c("col1", "col3", "col2")]





# from flu_survey.R
# data = as.data.frame(read_csv("surveydata.csv", na = '#NULL!'))

# ----- to rename and group -----
# names(Q7) <- c(
#   Q7_1 = "Bus",
#   Q7_2 = "Carpool",
#   Q7_3 = "Subway",
#   Q7_4 = "Train",
#   Q7_5 = "Taxi",
#   Q7_6 = "Airplane",
#   Q7_7 = "Other",
#   Q7_8 = "Refused",
#   Q7_otherText = "Other")
# str(Q7)

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

# ------------


# # subset of Q7 = lQ7
# # lQ7 = Q7[ ,1:6]


# # demographics
# # a = ggplot(ds) + scale_fill_brewer()
# # a + geom_bar(mapping = aes(x = ppagect4, fill = ppagect4)) + xlab("age group")
# # a + geom_bar(mapping = aes(x = PPEDUCAT, fill = PPEDUCAT)) + xlab("education")
# # a + geom_bar(mapping = aes(x = PPETHM, fill = PPETHM)) + xlab("ethnicity")
# # a + geom_bar(mapping = aes(x = PPGENDER, fill = PPGENDER)) + xlab("gender")

# C
# a + geom_bar(mapping = aes(x = Q11_1, binwidth=.5, position = "identity")) + xlab("work")
# a + geom_bar(mapping = aes(x = Q11_1, fill = Q11_1, position = position_dodge()))

# ppeduc = as.data.frame(table(ds$PPEDUC))
# q111 = as.data.frame(table(ds$Q11_1))




