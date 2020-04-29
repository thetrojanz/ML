library(caret)
library(e1071)

salary_train<-read.csv(file.choose())
salary_test<-read.csv(file.choose())
View(salary_train)
View(salary_test)

dim(salary_test)
dim(salary_train)

class(salary_test)
str(salary_test)

class(salary_train)
str(salary_train)

sal <- rbind(salary_train,salary_test)
View(sal)
#building the model by using naivebayes
model_nb_salary<-naiveBayes(salary_train$Salary~.,data=salary_train,trails=20)
model_nb_salary
#predict the model
pred_salary_nb<-predict(model_nb_salary,newdata=salary_test)
mean(pred_salary_nb==salary_test$Salary)
###The accuracy of the above model is 81.93%

####Bagging method to increase the model performance
acc <- c()
for (i in 1:10) {
 locale <- createDataPartition(sal$Salary,p=0.75,list = F)
 training <- sal[locale,]
 testing <- sal[-locale,]
 model <- naiveBayes(Salary~.,data = training)
 pred <- predict(model,newdata = testing[,-14])
 a <-  mean(pred==testing$Salary)
 acc <- c(acc,a)
}
summary(acc)
acc
