library(readr)
startup <- read.csv(file.choose())
View(startup)
startup <- startup
rdspend <- R.D.Spend
admin <- Administration
mark <- Marketing.Spend

attach(startup)
######EDA#######
#Measure of central tendancy

mean(rdspend)
mean(admin)
mean(mark)
mean(Profit)

median(rdspend)
median(admin)
median(mark)
median(Profit)

#Measure of Dispersion

var(startup)
sd(rdspend)
sd(admin)
sd(mark)
sd(Profit)


#3rd BM 

install.packages("moments")
library(moments)
skewness(startup)

#4th BM

kurtosis(startup)

##Grapghical representation
barplot(rdspend,Profit)
barplot(admin,Profit)
barplot(mark,Profit)
hist(rdspend)
hist(admin)
hist(mark)
hist(Profit)
boxplot(startup)
qqplot(rdspend,Profit)
qqplot(mark,Profit)
qqplot(admin,Profit)

summary(startup)

##########scatter plot###########
pairs(startup)
plot(startup)

str(startup)

##The dataset has one discrete column so we need preprocess it by using dummy
install.packages("dummy")
library(dummy)
startup_dummy <- dummy(startup)
colnames(startup_dummy)
View(startup_dummy)
attach(startup_dummy)

startup0 <- cbind(rdspend,admin,mark,Profit,State_California,State_Florida,State_New.York)

##Since the data's are not in same unit we need to normalize them to make unitless
startup1 <- scale(startup0)
startup1 <- data.frame(startup0)
View(startup1)
str(startup1)

##correlation
cor(startup1)

install.packages("corpcor")
library(corpcor)
cor2pcor(cor(startup1))
# The Linear Model of interest
model <- lm(Profit~rdspend+admin+mark+State_California+State_Florida+State_New.York,data = startup1)
summary(model)

model_rdspend <- lm(Profit~rdspend)
summary(model_rdspend)

model_ad <- lm(Profit~admin)
summary(model_ad)

model_ma <- lm(Profit~mark)
summary(model_ma)

model_states <- lm(Profit~State_California+State_Florida+State_New.York,data = startup1)
summary(model_states)

model_11 <- lm(Profit~rdspend+mark)
summary(model_11)

install.packages("psych")
library(psych)
pairs.panels(startup1)

influence.measures(model)
library(car)
influenceIndexPlot(model,id.n=3)
influencePlot(model,id.n=3)

########building the model by removing the observation

model3 <- lm(Profit~rdspend+admin+mark)
summary(model3)
influence.measures(model3)
influenceIndexPlot(model3,id.n=3)
influencePlot(model3,id.n=3)

## VIF and AV plot has given us an indication to delete "wt" variable
vif(model3)

model4 <- lm(Profit~rdspend+admin+mark,data = startup1[-c(46,47,50),])
summary(model4)

influence.measures(model4)
influenceIndexPlot(model4,id.n=3)
influencePlot(model4,id.n=3)
vif(model4)

#building the model by removing admin variable
model5 <- lm(Profit~rdspend+mark,data = startup1)
summary(model5)

influence.measures(model5)
influenceIndexPlot(model5,id.n=3)
influencePlot(model5,id.n=3)
vif(model5)
avPlots(model5,id.n=2,id.cex=0.7)

###Final model
model6 <- lm(Profit~rdspend+mark,data = startup1[-c(47,50),])
summary(model6)

###Predict model
pred <- predict(model6)
pred
error <- Profit - pred
error
#Evaluate model LINE assumptions 
plot(model6)

#Residual plots,QQplot,std-Residuals Vs Fitted,Cook's Distance 
qqPlot(model6,id.n = 5)
# QQ plot of studentized residuals helps in identifying outlier 

###Model Accuracy

AIC(model)
AIC(model5)
AIC(model6)

