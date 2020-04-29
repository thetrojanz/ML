PlasticSales <- read.csv(file.choose())
#View(PlasticSales)
library(tseries)
install.packages("Metrics")
library(Metrics)
x<-data.frame(outer(rep(month.abb,length=60),month.abb,"==")+0)
colnames(x)<-month.abb
#View(x)              
plot(PlasticSales$Sales,type = "o")
PlasticSales_data<-cbind(PlasticSales,x)
#View(PlasticSales_data)
colnames(PlasticSales_data)
PlasticSales_data["t"]<-1:60
View(PlasticSales_data)
PlasticSales_data["t_sqr"]<- PlasticSales_data$t*PlasticSales_data$t
PlasticSales_data["log_sales"]<-log(PlasticSales_data$Sales)
#View(PlasticSales_data)
attach(PlasticSales_data)
train<-PlasticSales_data[1:45,]
test<-PlasticSales_data[46:60,]

##linearmodel##241.3659

linearmodel<- lm(Sales~t,data = train)
summary(linearmodel)
linear_pred<-data.frame(predict(linearmodel, interval="predict", newdata = test))
#View(linear_pred)

rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear

##exponential##1351.284
expmodel<-lm(log_sales~t,data = train)
summary(expmodel)
expmodel_pred<-data.frame(predict(expmodel,interval="predict", newdata = test))
#View(expmodel_pred)
rmse_expo<-sqrt(mean((test$Sales-expmodel_pred$fit)^2,na.rm = T))
rmse_expo
##quadratic model##276.164

quadmodel<-lm(Sales~t+t_sqr,data = train)
summary(quadmodel)
quadmodel_pred<-data.frame(predict(quadmodel,interval = "predict",newdata = test))
rmse_quad<-sqrt(mean((test$Sales-quadmodel_pred$fit)^2,na.rm = T))
rmse_quad

##additive seasionality##257.1331
sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add
## Additive Seasonality with Linear ##117.0469

Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_add_sea_lm<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm = T))
rmse_add_sea_lm

## Additive Seasonality with Quadratic ##151.1677

Add_sea_Quad_model<-lm(Sales~t+t_sqr+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm = T))
rmse_Add_sea_Quad
## Multiplicative Seasonality ##1351.53

multi_sea_model<-lm(log_sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi<-sqrt(mean((test$Sales-multi_sea_pred$fit)^2,na.rm = T))
rmse_multi

## Multiplicative Seasonality Linear trend ##

multi_add_sea_model<-lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-multi_add_sea_pred$fit)^2,na.rm = T))
rmse_multi_add_sea
# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_quad","rmse_sea_add","rmse_add_sea_lm","rmse_Add_sea_Quad","rmse_multi","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_quad,rmse_sea_add,rmse_add_sea_lm,rmse_Add_sea_Quad,rmse_multi,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)


#Add_sea_Linear_model

Add_sea_Linear_newmodel<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=PlasticSales_data)
summary(Add_sea_Linear_newmodel)
Add_sea_Linear_newpred<-data.frame(predict(Add_sea_Linear_newmodel,interval='predict',newdata=test))
rmse_add_sea_lm<-sqrt(mean((Add_sea_Linear_newpred$fit-PlasticSales_data$Sales)^2,na.rm = T))
rmse_add_sea_lm#117.0469
residuals_add_sea<-residuals(Add_sea_Linear_newmodel)
windows()
acf(residuals_add_sea,lag.max = 10)
