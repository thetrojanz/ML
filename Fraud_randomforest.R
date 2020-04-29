library(randomForest)
library(caret)

fraud <- read.csv("D://R Assignments//Fraud_check.csv")
View(fraud)

# data explorer is to give intro about the dataset 
install.packages("DataExplorer")
library(DataExplorer)
plot_intro(fraud)
summary(fraud)

######converting tax numerical into categorical
max(fraud$Taxable.Income)
tax <- cut(fraud$Taxable.Income,breaks = c(0,30000,99619),labels = c('risky','good'),right = F)
fr_1<- cbind(fraud,tax)
fr_1 <- fr_1[,-3]
View(fr_1)
######converting pop numerical into categorical
max(fr_1$City.Population)
pop <- cut(fr_1$City.Population,breaks = c(0,40000,80000,120000,160000,200000),labels = c('<40k','40k<80K','80k<120k','120k<160k','160k<20k'),right = F)
fr_1<- cbind(fr_1,pop)
fr_1 <- fr_1[,-3]


######converting  work experience numerical into categorical
exp1 <- cut(fr_1$Work.Experience,breaks = c(0,5,10,15,20,25,30,35),labels = c('A','B','C','D','E','F','G'),right = F)
fr_1 <- cbind(fr_1,exp1)
fr_1 <- fr_1[,-3]
View(fr_1)

colnames(fr_1)
training1<- createDataPartition(fr_1$tax,p=0.85,list = F)

######model building
train <- fr_1[training1,]
dim(train)
test <- fr_1[-training1,]
dim(test)

model <- randomForest(tax~.,data = train,na.action=na.roughfix,importance=TRUE,trails=20)
summary(model)
model$ntree
pred <- predict(model,newdata = test[,-4])

#Accuracy 
mean(pred==test$tax) #77.52%
#a <- table(pred,testing$sal_cat)
#acc <- sum(diag(a)/sum(a))
#acc
library(gmodels)
rpref <- CrossTable(test$tax,pred,prop.r = F,prop.chisq = F,dnn = c('actual default', 'predicted default'))
