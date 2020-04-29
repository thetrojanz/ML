forest <- read.csv("C://Users//Dell//Downloads//Excel R//Assignments//Machine Learning//Supervised//2 Classification//NN//forestfires.csv")
View(forest)

#creating dummy vatiable for size_category
forest1 <- as.numeric(x = forest$size_category,levels = c('small','large'),labels = c('0','1'))
forest1 <- as.data.frame(forest1)
View(forest1)
forest <- cbind(forest,forest1)
forest <- forest[-c(1,2,31)]
colnames(forest)[29] <- 'size_category'
str(forest)

#normalizing
norm1 <- function(x){
  return( (x - min(x))/(max(x)-min(x)) )
}

forest_norm <- as.data.frame(lapply(forest,FUN = norm1))                           
# forest_norm <- cbind(forest_norm,forest[29])
View(forest_norm)

##Spliting the data into train and test model
set.seed(222)
locale <- sample(1:nrow(forest_norm),420)
forest_train <- forest_norm[locale,]
forest_test <- forest_norm[-locale,]
attach(forest_norm)

##model building
library(neuralnet)
# library(dplyr) - It should not be used while using neuralnet bcoz it'll through an error while doing compute.
model=neuralnet(size_category~.,data = forest_train,hidden = 4,linear.output = F)
summary(model)
str(model)
plot(model)

#Predicting the values
pre <- compute(model,forest_test[,-29])
pre1 <- pre$net.result
pre1

#correlation is 93% which means it is good
cor(pre1,forest_test$size_category)*100
plot(pre1,forest_test$size_category)

n1 <- ifelse(pre1>0.5,1,0)
n2 <- ifelse(forest_test$size_category>0.5,1,0)

accuracy <- table(n1,n2)
accuracy

acc<- sum(diag(accuracy))/(sum(accuracy))
acc*100
#Accuracy is 97.93%