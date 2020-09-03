
library(aTSA)
library(TSA)
library(forecast)
library(xts)
data1=read.csv("D:\\downloads\\108.09-108.12.csv",header=TRUE,sep=",")

par(mfrow=c(1,1))
ts.plot(data1$amount)
adf.test(data1$amount)
par(mfrow=c(1,2))
acf(data1$amount,lag=50)
pacf(data1$amount,lag=50)

par(mfrow=c(1,1))
w=diff(xx)
ts.plot(w)
acf(w,50)
pacf(w,50)

data1.est1=Arima(xx,order=c(0,1,1))
acf(data1.est1$res)
pacf(data1.est1$res)

###最佳
par(mfrow=c(1,2))
#data1.est=Arima(data1$amount,order=c(0,0,7))
data1.est=Arima(data1$amount,order=c(0,0,1),seasonal=list(order=c(0,1,1),period=7))
adf.test(data1$amount)
acf(data1.est$res)
pacf(data1.est$res)
data1.est$aic
tsdiag(data1.est)

data1.est=Arima(data1$amount,order=c(0,0,1),seasonal=list(order=c(0,1,2),period=7))
adf.test(data1$amount)
acf(data1.est$res)
pacf(data1.est$res)
data1.est$aic

data1.est=Arima(data1$amount,order=c(1,0,1),seasonal=list(order=c(0,1,2),period=7))
adf.test(data1$amount)
acf(data1.est$res)
pacf(data1.est$res)
data1.est$aic


data1.est=Arima(data1$amount,order=c(1,0,1),seasonal=list(order=c(0,1,1),period=7))
adf.test(data1$amount)
acf(data1.est$res)
pacf(data1.est$res)
data1.est$aic








par(mfrow=c(2,1))
data1.est=Arima(data1$amount,order=c(0,0,7))
data1.est=Arima(data1$amount,order=c(0,0,1),seasonal=list(order=c(1,0,1),period=7))
acf(data1.est$res)
pacf(data1.est$res)
data1.est$aic
tsdiag(data1.est)

par(mfrow=c(1,2))
data1.est=Arima(data1$amount,order=c(0,0,7))
data1.est=Arima(data1$amount,order=c(1,0,1),seasonal=list(order=c(1,0,1),period=7))
acf(data1.est$res)
pacf(data1.est$res)
data1.est$aic

par(mfrow=c(1,2))
data1.est=Arima(data1$amount,order=c(0,0,7))
data1.est=Arima(data1$amount,order=c(1,0,1),seasonal=list(order=c(1,0,2),period=7))
acf(data1.est$res)
pacf(data1.est$res)
data1.est$aic

#目前最佳
par(mfrow=c(1,2))
data1.est=Arima(data1$amount,order=c(0,0,7))
data1.est=Arima(data1$amount,order=c(0,0,2),seasonal=list(order=c(1,0,1),period=7))
acf(data1.est$res)
pacf(data1.est$res)
data1.est$aic
tsdiag(data1.est)


par(mfrow=c(1,1))
forecast(data1.est)
plot(forecast(data1.est))
