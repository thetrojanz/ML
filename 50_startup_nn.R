library(neuralnet)
salary<-read.csv(file.choose())
View(salary)
salary1<-scale(salary[,-4])
salary1<-cbind(salary1,salary[,4])
View(salary1)

salary_train<-as.data.frame(salary1[1:30,])
salary_test<-as.data.frame(salary1[31:50,])
View(salary_test)

salary_model<-neuralnet(formula =Profit~.,data = salary_train,hidden = 1)
plot(salary_model)
salary_results<-compute(salary_model,salary_test)
pred_strength<-salary_results$net.result
pred_strength
cor(pred_strength, salary_test$Profit )#82%
plot(pred_strength, salary_test$Profit)

#hidden =5
salary_model1<-neuralnet(formula =Profit~.,data = salary_train,hidden = 4)
plot(salary_model1)
salary_results1<-compute(salary_model1,salary_test)
pred_strength1<-salary_results1$net.result
pred_strength1
cor(pred_strength1, salary_test$Profit)#67%
plot(pred_strength1, salary_test$Profit)


#The model which is built with 1 hidden layer is the best to predict than with a hidden layer with 4 units. 
#The correlation of the model with 1 hidden layer is having 83%.