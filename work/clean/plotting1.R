# plotting1.R
rm(list = ls())
library(ggplot2)
load("clean/cleaning_all.Rdata")


g1 <- ggplot(data2[!is.na(data2$Q1), ])
g2 <- ggplot(data2[!is.na(data2$Q2), ])
g3 <- ggplot(data2[!is.na(data2$Q3), ])
g4 <- ggplot(data2[!is.na(data2$Q4), ])
g5 <- ggplot(data2[!is.na(data2$Q5), ])
g6 <- ggplot(data2[!is.na(data2$Q6), ])
#
g9 <- ggplot(data2[!is.na(data2$Q9), ])
#
g13 <- ggplot(data2[!is.na(data2$Q13), ])
g14 <- ggplot(data2[!is.na(data2$Q14), ])
g15 <- ggplot(data2[!is.na(data2$Q15), ])
g16 <- ggplot(data2[!is.na(data2$Q16), ])
g17 <- ggplot(data2[!is.na(data2$Q17), ])
#
g19 <- ggplot(data2[!is.na(data2$Q19), ])
g20 <- ggplot(data2[!is.na(data2$Q20), ])
g21 <- ggplot(data2[!is.na(data2$Q21), ])
#
g26 <- ggplot(data2[!is.na(data2$Q26), ])
g28 <- ggplot(data2[!is.na(data2$Q28), ])
#
g31 <- ggplot(data2[!is.na(data2$Q31), ])
g32 <- ggplot(data2[!is.na(data2$Q32), ])
g33 <- ggplot(data2[!is.na(data2$Q33), ])



rm(new_name); rm(old_name)
save(list = ls(all.names = TRUE), file ="clean/plotting1.RData")
