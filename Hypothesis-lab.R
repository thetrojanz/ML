library(readr)
TurnTime <- read.csv(file.choose())
View(TurnTime)

Stacked_Data <- stack(TurnTime)
View(Stacked_Data)
attach(Stacked_Data)

#############Normality test###############
library(nortest)
ad.test(Stacked_Data$values) 

#Since p-value = 0.5072 >0.05 Hence accept Ho i.e.Data is normal

############# Variance test ###############
install.packages("car")
library(car)
leveneTest(Stacked_Data$values~Stacked_Data$ind, data = Stacked_Data)   #Test for equal Variance
#Since p-value = 0.5161 > 0.05 Hence accept Ho i.e.variances are  equal will go for one way ANOVA test
################ One-way Anova ########
Anova_results <- aov(values~ind,data = Stacked_Data)

summary(Anova_results)
##Since p-value = 2e-16 < 0.05 accept Ha(Mean Turn Around Time (TAT) of at least one TurnTimeoratory is not equal).