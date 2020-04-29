library(readr)
comp <- read.csv(file.choose())
View(comp)
attach(comp)

##creating dummy variable for category data
library(dummy)
comp_dummy <- dummy(comp)
colnames(comp_dummy)
View(comp_dummy)
attach(comp_dummy)

comp1 <- cbind(price,speed,hd,ram,screen,ads,trend,cd_no,cd_yes,multi_no,multi_yes,premium_no,premium_yes)
View(comp1)

##Since the data's are not in same unit we need to normalize them to make unitless
comp2 <- scale(comp1)
comp2 <- data.frame(comp2)
View(comp2)
str(comp2)

###EDA###
summary(comp2)

###correlation 
cor(comp2)
### Partial Correlation matrix - Pure Correlation  b/n the varibles

library(corpcor)
cor2pcor(cor(comp2))
pairs(comp2)

#model building function
model<- lm(price~speed+hd+ram+screen+ads+trend+cd_no+cd_yes+multi_no+multi_yes+premium_no+premium_yes, data = comp2)
summary(model)

#standardizing the variable
model1<-lm(price~(speed+hd+ram+screen+ads+trend+cd_no+cd_yes+multi_no+multi_yes+premium_no+premium_yes)*(speed+hd+ram+screen+ads+trend+cd_no+cd_yes+multi_no+multi_yes+premium_no+premium_yes),data = comp2)
summary(model1)

#predicted values
pred <- predict(model1)
pred

#error
error <- price - pred
error

#evaluation
plot(model)
AIC(model)

