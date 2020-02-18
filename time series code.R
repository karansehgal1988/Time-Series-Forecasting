options(scipen = 999)

# Importing of the data
ts_data = read.csv("//Users//karansehgal//Downloads//UK Outward Passengers Movement.csv")

# Data wrangling
library(dplyr)
ts_data = na.omit(ts_data)
ts_data1 = ts_data[,c("Year","Quarter","X.1")]
ts_data1 = rename(ts_data1,total=X.1)

# Treating of Outliers and Missing values
library(tseries)
ts_data1$total = tsclean(ts_data1$total)

# Convert the Data into TS object
library(forecast)
ts_data2 = ts(ts_data1$total, start=c(1996, 1), end=c(2005, 4), frequency= 4) 
plot(ts_data2)

# ------------------------ Using Auto Arima Modelling -------------------- # 
library(forecast)

model1 = auto.arima(ts_data2,trace = T)
summary(model1)

# ---------------------------- X --------------------------------- #

# to improve accuracy of the model we will find the order of the p, d & q for the arima model using acf and Pacf plots.

# Decomposing the time series data
library(tseries)
plot(ts_data2)
decomp_data = decompose(ts_data2,type = "multiplicative")
plot(decomp_data)

# ----------  Makking the series stationary to find the order pf p, d and q ------------#


adf.test(ts_data2)
kpss.test(ts_data2)

# As the series is non stationary, hence deseasonalizing the series to make its staionary
ts_data3 = ts_data2/decomp_data$seasonal
plot(ts_data3)
adf.test(ts_data3)
kpss.test(ts_data3)

# Again series is non stationary and to make it detrend, hence differencing the deseasonaled series with order 1
ts_data4 = diff(ts_data3,differences = 1) # it helps in finding the order of d = 1
plot(ts_data4)
adf.test(ts_data4)
kpss.test(ts_data4)


# As the series has become stationary, so find the order of p, d and q using acf and pacf plot.

acf(ts_data4) # it will help in find the order of q = 2



pacf(ts_data4) # it will help in finding the order of p = 0

# As there is seasonal trend also, so we use arima model with seasonal order also (p,d,q) and (P,D,Q)m
model2 = arima(ts_data2,c(0,1,2),seasonal = list(order = c(0,1,2),period = 12))
summary(model2)

# Evaluation of model1 for auto arima  and model2 - finding the practically finding the order of (p,d,q) and (P,D,Q)m 

# For model 1
accuracy(model1)

# For model 2
accuracy(model2)

# ------------- for prediciton of next four quarters i.e. for year 2016 -------------- #

# As the model2 has lower value of MAPE, MPE & RMSE hence we will select the model2 for future forecasting

pred1 = forecast(model2, h = 4)

pred2  = data.frame(pred1) 

pred3 = pred2$Point.Forecast

pred3 = round(pred3,digits = 0)
pred3

# ------------------------------- End of Forecasting ----------------------------- #

# Ploting of predicted time series data #
pred3 = as.data.frame(pred3)
pred3$Quarter = c(1,2,3,4)
pred3$Year = 2016
pred3 = as.data.frame(pred3)
pred3 = rename(pred3, total = pred3)
pred3 = pred3[,c("Year","Quarter","total")]
data2 = rbind(ts_data1,pred3)

data2 = ts(data2$total, start=c(1996, 1), end=c(2006, 4), frequency= 4) 
plot(data2)


# -------------------------- End of Time Series Modelling ------------------------- #
