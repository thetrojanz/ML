library(kernlab)
library(caret)
Forestfires <- read.csv("D://R Assigments//forestfires.csv")
View(Forestfires)
attach(Forestfires)

fires_train <- Forestfires[1:410,]
fires_test <- Forestfires[411:517,]
write.csv(fires,file = "fires.csv",row.names = F,col.names = F)

#model building step 
# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

model1 <- ksvm(size_category~.,data = fires_train,kernel="vanilladot")
model1_pred <- predict(model1,newdata = fires_test)
mean(model1_pred==fires_test$size_category)
### model accuracy is 97.19%


#model building by using rbfdot
model2 <- ksvm(size_category~.,data = fires_train,kernel='rbfdot')
model2_predi <- predict(model2,newdata = fires_test)
mean(model2_predi==fires_test$size_category)
##model accuracy is 71.02%


# kernal = besseldot
model3<-ksvm(size_category ~.,data = fires_train,kernel = "besseldot")
model3_predi<-predict(model3,newdata=fires_test)
mean(model3_predi==fires_test$size_category)
##model accuracy is 71.02%

# kernel = polydot
model4<-ksvm(size_category ~.,data = fires_train,kernel = "polydot")
model4_predi<-predict(model4,newdata = fires_test)
mean(model4_predi==fires_test$size_category) # 83.925
##model accuracy is 97.19%
