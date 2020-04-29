library(readr)
concrete <- read.csv(file.choose())
View(concrete)
str(concrete)
attach(concrete)
#normal_concrete<-scale(concrete) or 
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete,FUN=normalize))
View(concrete_norm)
summary(concrete_norm$strength)
#summary(normal_concrete)
summary(concrete$strength)
concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]

# Using multilayered feed forward nueral network
# package nueralnet
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)
library(nnet)

# Building simple neural network model without hidden layers
concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train)
str(concrete_model)
plot(concrete_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate output for the model built
model_results <- compute(concrete_model,concrete_test[1:8])
predicted_strength <- model_results$net.result
predicted_strength
####Evaluation
cor(predicted_strength,concrete_test$strength)#72%
plot(predicted_strength,concrete_test$strength)

#Building simple neural network model with hidden layers having 5 units.
model_5<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= concrete_norm,hidden = 5)
plot(model_5)
model_5_res<-compute(model_5,concrete_test[1:8])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,concrete_test$strength)#91%
plot(pred_strn_5,concrete_test$strength)
# SSE has reduced and training steps had been increased as the number of neurons 
# under hidden layer are increased

#Building simple neural network model with 2 hidden layers having 5 units
model_5<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= concrete_norm,hidden = c(5,5))
plot(model_5)
model_5_res<-compute(model_5,concrete_test[1:8])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,concrete_test$strength)#92%
plot(pred_strn_5,concrete_test$strength)

#Out of multiple model, the model with 2 hidden layers having 5 units is the best.