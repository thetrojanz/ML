library(readr)
toyota <- read.csv(file.choose())
View(toyota)
attach(toyota)

library(dummy)
toyota_dummy <- dummy(toyota)
colnames(toyota_dummy)
View(toyota_dummy)
attach(toyota_dummy)

#"Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight"
toy1 <- cbind(Price,Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight)
str(toyota)

###Since the data is having different units we need to normalize
toy11 <- scale(toy1)
toy1 <- data.frame(toy11)
str(toy1)

####Scatter PLot
plot(toy1)

library(psych)
pairs.panels(toy1)

###Corrrelation
cor(toy1)
### Partial Correlation matrix - Pure Correlation  b/n the varibles
library(corpcor)
cor2pcor(cor(toy1))

###model building process
model <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
summary(model)

##model build b/w Price and cc
model_cc <- lm(Price~cc)
summary(model_cc)
##model build b/w Price and Doors
model_doors <- lm(Price~Doors)
summary(model_doors)
##model build b/w Price and cc and Doors
model_dcc <- lm(Price~cc+Doors)
summary(model_dcc)

##To find the influential observations
library(car)
influence.measures(model)
influenceIndexPlot(model,id.n=3)
influencePlot(model,id.n=3)

#removing the influenctial observation from the model
model1 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = toy1[-81,])
summary(model1)

influence.measures(model1)
influenceIndexPlot(model1)
influencePlot(model1)

#Doors variable is not having significant values so tried all possible ways 
#to make it significant and failed so its better to remove the variable
#building the final model by removing the doors variable
model2 <- lm(Price~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight,data = toy1[-81,])
summary(model2)

#Multiple R-squared:  0.8693>0.85 hence the model built is the best model

#Confidenceintervals
confint(model2,level = 0.95)
#predicted values
pred <- predict(model2)
pred

#To find the error
error <- pred - Price
error

#Model Evaluation:
plot(model2)
AIC(model2)

