library(forecast)
library(fpp)
library(smooth)
library(readxl)
library(tseries)

cocacola <- read_xlsx(file.choose(),1)
View(cocacola)

cocacola<-ts(cocacola$Sales,frequency = 4,start=c(86))
View(cocacola)

plot(cocacola,type = 'o')
#From the plot we can say that there is trend,level,with no seasonality so we can go for Data driven methods
train<-cocacola[1:32]
test<-cocacola[33:42]
train<-ts(train,frequency = 4)
test<-ts(test,frequency = 4)

#### USING HoltWinters function ################
# Optimum values with alpha = 0.2 which is default value
# Assuming time series data has only level parameter

hw_a<-HoltWinters(train,alpha = 0.2,beta = F,gamma = F)# simple exponential smooting
summary(hw_a)
hwa_pred<-data.frame(predict(hw_a,n.ahead=10))#n.ahead = 4 to forecast next 4 month data
# By looking at the plot the forecasted values are not showing any characters of train data 
plot(forecast(hw_a,h=4))#h = 4: plot next 4 data.
#MAPE(actual, forecast)
hwa_mape<-MAPE(test,hwa_pred$fit)*100
hwa_mape

# with alpha = 0.2, beta = 0.1 Assuming time series data has level and trend parameter 
hw_ab<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = F)
hw_ab
hwab_pred<-data.frame(predict(hw_ab,n.ahead = 10))
# by looking at the plot the forecasted values are still missing some characters exhibited by train data
plot(forecast(hw_ab,h=4))
hwab_mape<-MAPE(test,hwab_pred$fit)*100
hwab_mape

#with alpha = 0.2, beta = 0.1, gamma = 0.1 Assuming time series data has level,trend and seasonality 
hw_abg<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
hw_abg
hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 10))
# by looking at the plot the characters of forecasted values are closely following historical data
plot(forecast(hw_abg,h=4))
hwabg_mape<-MAPE(test,hwabg_pred$fit)*100
hwabg_mape

#Without optimum values 
hw_na<-HoltWinters(train,beta = F,gamma = F)
hw_na
hwna_pred<-data.frame(predict(hw_na,n.ahead = 10))
hwna_pred
plot(forecast(hw_na,h=4))
hwna_mape<-MAPE(test,hwna_pred$fit)*100
hwna_mape

hw_nab<-HoltWinters(train,gamma=F)
hw_nab
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=10))
hwnab_pred
plot(forecast(hw_nab,h=4))
hwnab_mape<-MAPE(test,hwnab_pred$fit)*100
hwnab_mape

hw_nabg<-HoltWinters(train)
hw_nabg
hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =10))
hwnabg_pred
plot(forecast(hw_nabg,h=4))
hwnabg_mape<-MAPE(test,hwnabg_pred$fit)*100
hwnabg_mape

df_mape<-data.frame(c("hwa_mape","hwab_mape","hwabg_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c(hwa_mape,hwab_mape,hwabg_mape,hwna_mape,hwnab_mape,hwnabg_mape))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)

#Based on the MAPE value the holtswinter for alpha,beta, gamma is having the least MAPE value so we can deploy that model.
#applying entire data to the best model
model <- HoltWinters(cocacola,alpha = 0.2,beta = 0.1,gamma = 0.1)
plot(forecast(new_model,n.ahead=4))
# Forecasted values for the next 4 quarters
forecast_new <- data.frame(predict(new_model,n.ahead=10))
mape_model <- MAPE(test,forecast_new$fit)*100
mape_model#23.7896

############## USING ses,holt,hw functions ##########################
# Optimum values
# with alpha = 0.2
# Simple Exponential smoothing 
ses_a<-ses(train,alpha = 0.2) 
ses_a
sesa_pred<-data.frame(predict(ses_a,n.ahead=10))
plot(forecast(ses_a,h=4))
sesa_mape<-MAPE(sesa_pred$Point.Forecast,test)*100

# with alpha = 0.2, beta = 0.1
holt_ab<-holt(train,alpha = 0.2,beta = 0.1)
holt_ab
holtab_pred<-data.frame(predict(holt_ab,n.ahead=10))
plot(forecast(holt_ab,h=4))
holtab_mape<-MAPE(test,holtab_pred$Point.Forecast)*100

# with alpha = 0.2, beta = 0.1, gamma = 0.1
hw_abg_new<-hw(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
hw_abg_new
hwabg_pred_new<-data.frame(predict(hw_abg_new,n.ahead = 10))
plot(forecast(hw_abg_new,h=4))
hwabg_mape_new<-MAPE(test,hwabg_pred_new$Point.Forecast)*100

# Without optimum values simple exponential method

ses_na<-ses(train,alpha=NULL)
ses_na
sesna_pred<-data.frame(predict(ses_na,n.ahead = 10))
sesna_pred
plot(forecast(ses_na,h=4))
sesna_mape<-MAPE(test,sesna_pred$Point.Forecast)*100

# Holts winter method 
holt_nab<-holt(train,alpha = NULL,beta = NULL)
holt_nab
holtnab_pred<-data.frame(predict(holt_nab,h=10))
holtnab_pred
plot(forecast(holt_nab,h=4))
holtnab_mape<-MAPE(test,holtnab_pred$Point.Forecast)*100
holtnab_mape
# Holts winter Exponential method

hw_nabg_new<-hw(train,alpha=NULL,beta=NULL,gamma = NULL)
hw_nabg_new
hwnabg_pred_new<-data.frame(predict(hw_nabg_new,n.ahead=10))
hwnabg_pred_new
plot(forecast(hw_nabg_new,h=4))
hwnabg_mape_new<-MAPE(test,hwnabg_pred_new$Point.Forecast)*100

df_mapes_new<-data.frame(c("sesa_mape","holtnab_mape","hwabg_mape_new","sesna_mape","holtnab_mape","hwnabg_mape_new","ma_mape"),c(sesa_mape,holtnab_mape,hwnabg_mape_new,sesna_mape,holtnab_mape,hwnabg_mape_new,ma_mape))
colnames(df_mapes_new)<-c("MAPE","VALUE")
View(df_mapes_new)

# MOVING AVERAGE 
ma_model1<-sma(train)
ma_pred<-data.frame(predict(ma_model1,n.ahead=10))
ma_pred
plot(forecast(ma_model1))
ma_mape<-MAPE(test,ma_pred$Point.Forecast)*100
ma_mape

#Based on the MAPE value the holtswinter for alpha,beta, gamma is having the least MAPE value so we can deploy that model.


############ARIMA Model is for Model based technique###########
######Auto.Arima model on the price
model_AA <- auto.arima(train)
test_arima <- auto.arima(test)
class(model_AA)
pred_AA <- data.frame(forecast(model_AA,h=10))
rmse_AA <- sqrt(mean((pred_AA$Point.Forecast-test_arima$fitted)^2))
AA_mape<-MAPE(test_arima$fitted,pred_AA$Point.Forecast)*100
AA_mape#14.67283
acf(model_AA$residuals)
pacf(model_AA$residuals)#doubt
plot(forecast(model_AA,h=12),xaxt="n")

