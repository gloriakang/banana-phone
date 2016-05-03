library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

ds = read_csv("surveydata.csv")
str(ds)

# tables
ppeduc = as.data.frame(table(ds$PPEDUC))
q111 = as.data.frame(table(ds$Q11_1))


# demographics
a = ggplot(ds) + scale_fill_brewer()
a + geom_bar(mapping = aes(x = ppagect4, fill = ppagect4)) + xlab("age group")
a + geom_bar(mapping = aes(x = PPEDUCAT, fill = PPEDUCAT)) + xlab("education")
a + geom_bar(mapping = aes(x = PPETHM, fill = PPETHM)) + xlab("ethnicity")
a + geom_bar(mapping = aes(x = PPGENDER, fill = PPGENDER)) + xlab("gender")

# C
#a + geom_bar(mapping = aes(x = Q11_1, binwidth=.5, position = "identity")) + xlab("work")
#a + geom_bar(mapping = aes(x = Q11_1, fill = Q11_1, position = position_dodge()))



