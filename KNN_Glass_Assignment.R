library(readr)
glass <- read.csv("C://Users//Dell//Downloads//Excel R//Assignments//Machine Learning//Supervised//2 Classification//KNN//glass.csv")
View(glass)
table(glass$Type)

round(prop.table(table(glass$Type))*100,1)

norm <- function(x){
  return(x - min(x)/(max(x) - min(x)))
}

glass_n <- as.data.frame(lapply(glass[-10],norm))
View(glass_n)

#partitioning the train and test datas
train_gls <- glass_n[1:180,]
test_gls <- glass_n[181:214,]

##Partitioning the train and test labels to test accuracy
train_gls_labels <-glass[1:180,10]
test_gls_labels <- glass[181:214,10]

library(class)
train_acc <- NULL
test_acc <- NULL

for (i in seq(3,214,2)) {
  train_pred <- knn(train = train_gls,test = test_gls,cl=train_gls_labels,k=i)
  train_acc <- c(train_acc,mean(train_pred == train_gls_labels))
  test_pred <- knn(train = train_gls,test = test_gls,cl = train_gls_labels,k=i)
  test_acc <- c(test_acc,mean(test_pred==test_gls_labels))
}
# Testing Accuracy 
# Plotting 2 different graphs on same window

CrossTable(x =test_gls_labels,y=test_pred)
#x <- table(test_pred,test_gls_labels)
#accy<- (diag(x))/sum(x)*100


par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,214,2),train_acc,type="l",main="train_acc",col="blue")
plot(seq(3,214,2),test_acc,type="l",main="test_acc",col="red")

acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(3,214,2)))
# Plotting 2 different graphs on same co-ordinate axis
#install.packages("ggplot2")
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))
