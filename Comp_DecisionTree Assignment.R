library(caret)
library(C50)
library(readr)
library(rpart)

data <- read.csv("D://R Assignments//Company_Data.csv")
View(data)

#converting the sale variable to categorical
sale_catg <- cut(data$Sales,breaks = c(0,5,10,15,20),labels=c('A','B','C','D'),right = F)
View(sale_catg)

data <- cbind(data,sale_catg)
View(data)

data <- data[,-1]
View(data)

# Data partion
inTrainingloc <- createDataPartition(data$sale_catg,p=.75,list=F)
train <- data[inTrainingloc,]
View(train)
dim(train)
test <- data[-inTrainingloc,]
View(test)
dim(test)

#model building

model <- C5.0(train$sale_catg~.,data = train)
#pruning = ,rules = FALSE, weights = NULL,control = C5.0Control(subset = TRUE,noGlobalPruning = FALSE, CF = 0.25, minCases = 2)

#model <- ctree(training$Species~.,data = training)
#trials	an integer specifying the number of boosting iterations. A value of one indicates that a single model is used.

#Generating the model summary
summary(model)
pred <- predict(model,test[,-11])
a <- table(testing$sale_catg,pred)
a
sum(diag(a)/sum(a))

plot(model)


#Bagging#
acc <- c()
for(i in 1:50){
  print(i)
  inTrainingloc<-createDataPartition(data$sale_catg=.85,list=F)
  training1<-data[inTrainingloc,]
  testing1<-data[-inTrainingloc,]
  
  fittree<-C5.0(training1$sale_catg~.,data=training1)
  pred<-predict.C5.0(fittree,testing1[,-11])
  a<-table(testing1$sale_catg,pred)
  
  acc<-c(acc,sum(diag(a))/sum(a))
}
summary(acc)
acc
plot(fittree)
