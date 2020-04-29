library(readr)
bank <- read.csv(file.choose())
View(bank)
bank <- as.data.frame(bank)
attach(bank)

sum(is.na(bank))
colnames(bank)

library(dummy)
extra<- dummy(bank)
View(extra)

#building the model

mod <- glm(y~age+factor(job)+factor(marital)+factor(education)+factor(default)+balance+factor(housing)+factor(loan)+factor(contact)+day+factor(month)+duration+campaign+pdays+previous+factor(poutcome), family="binomial")
View(mod)
summary(mod)
plot(mod)

#Predict
prob <- predict(mod,type = 'response')
confusion <- table(prob>0.5,y)
confusion

#Accuracy
acc <- sum(diag(confusion))/sum(confusion)
acc

#error
error <- 1 - acc

library(ROCR)
rocpredict <- prediction(prob,y)
rorperf <- performance(rocpredict,'tpr','fpr')
plot(rorperf,colorize = TRUE)

rocr_cutoff <- data.frame(cut_off = rorperf@alpha.values[[1]],fpr=rorperfr@x.values,tpr=rorperfr@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)
