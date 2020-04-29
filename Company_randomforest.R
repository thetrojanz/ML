library(randomForest)
library(caret)
rforest <- read.csv("D://R Assignments//Company_Data.csv")
View(rforestorest)

###converting the sale variable to categorical
sale_catg <- cut(rforest$Sales,breaks = c(0,5,10,15,20),labels=c('A','B','C','D'),right = F)
View(sale_catg)

rforest <- cbind(rforest,sale_catg)
rforest <- rforest[,-1]
View(rforest)

locale <- createDataPartition(rforest$sal_cat,p=0.75,list = F)
training <- rforest[locale,]
dim(training)

testing <- rforest[-locale,]
dim(testing)

model <- randomForest(sale_catg~.,data = training,na.action=na.roughfix,importance=TRUE)
summary(model)
model$ntree
pred <- predict(model,newdata = testing[,-11])

#Accuracy 
mean(pred==testing$sale_catg) #74.74%
#a <- table(pred,testing$sal_cat)
#acc <- sum(diag(a)/sum(a))
#acc
library(gmodels)
rpref <- CrossTable(testing$sal_cat,pred,prop.r = F,prop.chisq = F,dnn = c('actual default', 'predicted default'))
