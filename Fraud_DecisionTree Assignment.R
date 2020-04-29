library(C50)
library(caret)
library(dplyr)
library(rpart)

fraud <- read.csv("D://R Assignments//Fraud_check.csv")
View(fraud)

## data explorer is to give intro about the dataset 
install.packages("DataExplorer")
library(DataExplorer)
plot_intro(fraud)
summary(fraud)

#converting tax numerical into categorical
max(fraud$Taxable.Income)
tax <- cut(fraud$Taxable.Income,breaks = c(0,30000,99619),labels = c('risky','good'),right = F)
fr_1<- cbind(fraud,tax)
fr_1 <- fr_1[,-3]
View(fr_1)
#converting pop numerical into categorical
max(fraud1$City.Population)
populatn <- cut(fraud1$City.Population,breaks = c(0,40000,80000,120000,160000,200000),labels = c('<40k','40k<80K','80k<120k','120k<160k','160k<20k'),right = F)
fr_1<- cbind(fr_1,pop)
fr_1 <- fr_1[,-3]


#converting  work experience numerical into categorical
exp1 <- cut(fraud1$Work.Experience,breaks = c(0,5,10,15,20,25,30,35),labels = c('A','B','C','D','E','F','G'),right = F)
fr_1 <- cbind(fr_1,exp1)
fr_1 <- fr_1[,-3]
View(fr_1)

colnames(fr_1)
training1<- createDataPartition(fr_1$tax,p=0.85,list = F)

#model building
train <- fr_1[training1,]
dim(train)
test <- fr_1[-training1,]
dim(test)
model <- C5.0(train$tax~.,data = train)

summary(model)
predi <- predict.C5.0(model,newdata = test[,-4])
a <- table(test$tax,predi)
a

sum(diag(a)/sum(a))
plot(model)
#glimpse(model)

#######bagging and boosting method
acc <- c()

for (i in 1:20) {
  local_partition <- createDataPartition(fr_1,p=0.75,list = F)
  train1 <- fr_1[local_partition,]
  test1 <- fr_1[-local_partition,]
  
  model1 <- C5.0(train1$tax~.,data = train1)
  predi1 <- predict.C5.0(model1,newdata = test1[,-4])
  a <- table(test1$tax,predi1)
  acc <- c(acc,(diag(a)/(sum(a))*100))
  print(i)
}

summary(acc)
acc
plot(model1)
