library(readr)
cutlet <- read.csv(file.choose())
View(cutlet)
attach(cutlet)
install.packages("nortest")
#normality test
library(nortest)
#ad.test(fabric$Fabric_length)      ###Anderson-Darling test

#Variance test#

var.test(Unit.A,Unit.B)#variance test
# p-value = 0.3136 > 0.05 so p high null fly => Equal variances

#Normality test#

shapiro.test(cutlet$Unit.A)
# p-value = 0.32 >0.05 so p high null fly => It follows normal distribution



shapiro.test(cutlet$Unit.B)
# p-value = 0.5225 >0.05 so p high null fly => It follows normal distribution


#2 sample T Test #

t.test(Unit.A,Unit.B,alternative = "two.sided",conf.level = 0.95,correct = TRUE)#two sample T.Test
# alternative = "two.sided" means we are checking for equal and unequal means
# null Hypothesis -> Equal means
# Alternate Hypothesis -> Unequal Hypothesis
# p-value = 0.4723 > 0.05 accept null Hypothesis 
# unequal means

?t.test
t.test(Unit.A,Unit.B,alternative = "greater",var.equal = T)
#p-value = 0.2361 > 0.5 means accept Ho
# alternative = "D1 cutlet > D2 cutlet
# Null Hypothesis "D1 cutlet < D2 cutlet"