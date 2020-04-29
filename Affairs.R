data(Affairs,package="AER")
aff <- Affairs
View(aff)
attach(aff)

extra <- ifelse(affairs>=1,'1','0')
as.numeric(extra)
View(extra)
aff[,"exaffair"] <- as.numeric(extra)
View(aff)
attach(aff)

aff1 <- glm(exaffair~gender+age+yearsmarried+children+religiousness+education+occupation+rating)
summary(aff1)
plot(aff1)

aff1 <- glm(exaffair~factor(gender)+age+yearsmarried+factor(children)+religiousness+education+occupation+rating)
summary(aff1)

#factor influence the chances of extra marital affair are age,years married,religiousness and rating
prob <- predict(aff1,aff[,-1],type = 'response')
confusion <- table(prob>0.5,aff$exaffair)
confusion
##          0   1
##FALSE 441 133
##TRUE   10  17

####Accuracy
accuracy <- sum(diag(confusion))/sum(confusion)
accuracy    #0.7620632

Error <- 1 - accuracy
Error      #0.2379368

library(ROCR)
rocprediction <- prediction(prob,aff$exaffair)
rocperformance <- performance(rocprediction,'tpr','fpr')
plot(rocperformance,colorize = TRUE)

rocr_cutoff <- data.frame(cut_off = rocperformance@alpha.values[[1]],fpr=rocperformance@x.values,tpr=rocperformance@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

