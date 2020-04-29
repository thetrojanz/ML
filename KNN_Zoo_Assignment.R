library(readr)
library(class)

zoo <-  read.csv(file.choose())
View(zoo)
class(zoo)
str(zoo)
zoo1 <- zoo[-c(1,18)]
View(zoo1)

#normalize the data by using scale func/by manually writting the  formula

#z1 <- lapply(zoo1,norm)
zoo1 <- scale(zoo1)
View(zoo1)
###Partitioning the data by train and test
zoo_train <- zoo1[1:75,]
zoo_test <- zoo1[76:101,]

zoo_trl <- zoo[1:75,18]
View(zoo_trl)
zoo_tsl <- zoo[76:101,18]
train_acc <- NULL
test_acc <- NULL
for (i in seq(3,75,2)) {
zoo_predi <- knn(train = zoo_train,test = zoo_test,cl=zoo_trl,k=i)
train_acc <- c(train_acc,mean(zoo_predi==zoo_trl))
zoo_predi_tes <- knn(train = zoo_train, test = zoo_test, cl = zoo_trl, k=i)
test_acc <- c(test_acc,mean(zoo_predi==zoo_tsl))
}
round(prop.table(table(zoo$type))*100,1)
library(gmodels)
##Check the performance
CrossTable(x=zoo_tsl,y=zoo_predi)

library(ggplot2)
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,75,2),train_acc,type="l",main="train_acc",col="blue")
plot(seq(3,75,2),test_acc,type="l",main="test_acc",col="red")

acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(3,75,2)))
# Plotting 2 different graphs on same co-ordinate axis
#install.packages("ggplot2")
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))


