# Load ca_co.csv dataset
library(readr)
ca_co<-read.csv(file.choose())
View(ca_co)

# Exploratory data analysis
summary(ca_co)

colnames(ca_co)[1]<-"Weight.Gained"
colnames(ca_co)
colnames(ca_co)[2]<-"Calories.Consumed"
View(ca_co)

#Scatter plot
plot(ca_co$Weight.Gained, ca_co$Calories.Consumed)  # plot(X,Y)

?plot

attach(ca_co)


#Correlation Coefficient (r)
cor(Weight.Gained,Calories.Consumed)             # cor(X,Y)

# Simple Linear Regression model
reg <- lm(Weight.Gained ~ Calories.Consumed) # lm(Y ~ X)

summary(reg)

#predict delivery time using sorting time#

# Load ca_co.csv dataset
library(readr)
delv_time<-read.csv(file.choose())
View(delv_time)

plot(delv_time$Delivery.Time, delv_time$Sorting.Time)  # plot(X,Y)

?plot

attach(delv_time)


#Correlation Coefficient (r)
cor(Delivery.Time,Sorting.Time)             # cor(X,Y)

# Simple Linear Regression model
reg <- lm(Delivery.Time ~ Sorting.Time) # lm(Y ~ X)

summary(reg)

#Build a prediction model for Churn_out_rate#

# Load emp data dataset

library(readr)
emp_dta<-read.csv(file.choose())
View(emp_dta)

plot(emp_dta$Salary_hike, emp_dta$Churn_out_rate)  # plot(X,Y)

?plot

attach(emp_dta)

#Correlation Coefficient (r)
cor(Salary_hike,Churn_out_rate)             # cor(X,Y)

# Simple Linear Regression model
reg <- lm(Salary_hike ~ Churn_out_rate) # lm(Y ~ X)

summary(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(emp_dta ))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = emp_dta, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_dta, aes(x=Salary_hike, y= Churn_out_rate))
 
