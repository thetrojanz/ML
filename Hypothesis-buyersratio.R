library(readr)
BuyerRat <- read.csv(file.choose())
View(BuyerRat)

Stacked_Data <- stack(BuyerRat)
View(Stacked_Data)
attach(Stacked_Data)

#########Chi Square(Bahaman Research)#################

attach(BuyerRat)
table(values,ind)
chisq.test(table(values,ind))
# p-value = 0.2931 > 0.05  => Accept null hypothesis
# => All countries have equal proportions 

