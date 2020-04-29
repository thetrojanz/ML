library(xlsx)
library(forecast)
library(fpp)
library(smooth)
library(tseries)

airlines <- read.xlsx(file.choose(),1)
View(airlines)

x<-data.frame(outer(rep(month.abb,length=96),month.abb,"==")+0)
colnames(x)<-month.abb
View(x)              
plot(airlines$Passengers,type = "o")
##From the plot we come to know about the data is having upward trend with seasonality so we can go for model based approach
airlines_data<-cbind(airlines,x)
View(airlines_data)

colnames(airlines_data)
airlines_data["t"]<-1:96
View(airlines_data)
airlines_data["t_sqr"]<- airlines_data$t*airlines_data$t
airlines_data["log_psnger"]<-log(airlines_data$Passengers)
View(airlines_data)

attach(airlines_data)
train<-airlines_data[1:60,]
test<-airlines_data[61:96,]

##linearmodel##44.35663

linearmodel<- lm(Passengers~t,data = train)
summary(linearmodel)
linear_pred<-data.frame(predict(linearmodel, interval="predict", newdata = test))
View(linear_pred)

rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear

##exponential##283.2553
expmodel<-lm(log_psnger~t,data = train)
summary(expmodel)
expmodel_pred<-data.frame(predict(expmodel,interval="predict", newdata = test))
View(expmodel_pred)
rmse_expo<-sqrt(mean((test$Passengers-expmodel_pred$fit)^2,na.rm = T))
rmse_expo

##quadratic model##40.31203
quadmodel<-lm(Passengers~t+t_sqr,data = train)
summary(quadmodel)
quadmodel_pred<-data.frame(predict(quadmodel,interval = "predict",newdata = test))
rmse_quad<-sqrt(mean((test$Passengers-quadmodel_pred$fit)^2,na.rm = T))
rmse_quad


##additive seasionality##119.9707
sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add


## Additive Seasonality with Linear ##29.49606
Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_add_sea_lm<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm = T))
rmse_add_sea_lm


## Additive Seasonality with Quadratic ##25.49269
Add_sea_Quad_model<-lm(Passengers~t+t_sqr+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm = T))
rmse_Add_sea_Quad


## Multiplicative Seasonality #283.8273
multi_sea_model<-lm(log_psnger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi<-sqrt(mean((test$Passengers-multi_sea_pred$fit)^2,na.rm = T))
rmse_multi


## Multiplicative Seasonality Linear trend #283.2261
multi_add_sea_model<-lm(log_psnger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-multi_add_sea_pred$fit)^2,na.rm = T))
rmse_multi_add_sea


#Add_sea_Linear_model#96.27174
Add_sea_Linear_newmodel<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=airlines_data)
summary(Add_sea_Linear_newmodel)
Add_sea_Linear_newpred<-data.frame(predict(Add_sea_Linear_newmodel,interval='predict',newdata=test))
rmse_add_sea_lm<-sqrt(mean((Add_sea_Linear_newpred$fit-airlines_data$Passengers)^2,na.rm = T))
rmse_add_sea_lm#95.30578
residuals_add_sea<-residuals(Add_sea_Linear_newmodel)
windows()
acf(residuals_add_sea,lag.max = 10)

# Preparing table on model and it's RMSE values
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_quad","rmse_sea_add","rmse_add_sea_lm","rmse_Add_sea_Quad","rmse_multi","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_quad,rmse_sea_add,rmse_add_sea_lm,rmse_Add_sea_Quad,rmse_multi,rmse_multi_add_sea))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

write.csv(airlines_data,file = 'airpass.csv')
#Out of multiple models Additive Seasonality with Quadratic is the best model with the least RMSE value of 25.49.
#so we are going to deploy that model for the model apply both train and test data
new_model <- read.csv(file.choose())
new_model <- new_model[,-1]
View(new_model)
pred_new <- data.frame(predict(Add_sea_Quad_model,newdata = new_model,interval = 'predict'))
pred_new
res <- residuals(Add_sea_Quad_model)
acf(res,lag.max = 6)
#from the plot to find the significant variable to predict 
rmse_mod <- sqrt(mean(test$Passengers - pred_new$fit))
rmse_mod##RMSE = 7.752395
