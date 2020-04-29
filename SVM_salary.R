library(readr)
library(kernlab)
library(caret)

salary_train <- read.csv("D://R Assignments//SalaryData_Train.csv")
salary_test <- read.csv("D://R Assignments//SalaryData_Test.csv")
colnames(salary_train)
View(salary_train)
View(salary_test)

#####model building step 
# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"
#vanilladot
model_vdot_sal<-ksvm(Salary~.,data=salary_train,kernel="vanilladot")
model_vdot_sal
pred_salary_vdot<-predict(model_vdot_sal,newdata=salary_test)
mean(pred_salary_vdot==salary_test$Salary)#84.6

#rbfdot
model_rbfdot_sal<-ksvm(Salary~.,data=salary_train,kernel="rbfdot")
pred_salary_rbfdot<-predict(model_rbfdot_sal,newdata=salary_test)
View(pred_salary_rbfdot)
mean(pred_salary_rbfdot==salary_test$Salary)#85.4


# kernal = besseldot
model3<-ksvm(Salary ~.,data = salary_train,kernel = "besseldot")
model3_predi<-predict(model3,newdata=salary_test)
mean(model3_predi==salary_test$Salary)
##model accuracy is 71.02%

# kernel = polydot

model4<-ksvm(Salary ~.,data = salary_train,kernel = "polydot")
model4_predi<-predict(model4,newdata = salary_test)
mean(model4_predi==salary_test$Salary) # 83.925
##model accuracy is 97.19%