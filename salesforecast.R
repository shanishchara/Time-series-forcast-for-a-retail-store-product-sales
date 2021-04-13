#Problem statement: Forecast beverages sales for a retail company:
# time series data is from 1992 to 2017

library(ggplot2)
library(plotly)
library(astsa)
library(forecast)
sales_data1 <- read.csv("sales dataset.csv", header = T)
head(sales_data1)

#convert the data into time series:

sales_data <- ts(sales_data1[,2], start = c(1992,1), frequency = 12)
str(sales_data)

summary(sales_data)


#plot the time series data:
plot(sales_data, xlab="years", ylab="Sales")

#we can observe the upward trend in the data and the series is non stationary with seasonality.
#we will convert it into stationary series and take seasonal differencing.
#stationary series will have zero mean and constant variance
#a look at acf and pacf:

acf2(sales_data, max.lag = 24)

#ACF and PACF graphs decays slowely, signifies that it is a non stationary series,

#a look into seasonally differenced sales:

sales_diff12 <- diff(sales_data,12) #we have monthly sales data:
plot.ts(sales_diff12, col="darkgreen")

#check of acf and pacf for seasonally differenced sales:
acf2(sales_diff12, max.lag = 24)
#acf and pacf for differencing decays rather quickly.

#trend and seasonally differenced retail sales:
sales_diff1and12 <- diff(sales_diff12,1)
plot(sales_diff1and12, col="darkblue")
acf2(sales_diff1and12, max.lag = 36)


#identify an appropriate model based on acf and pacf:
#let's begin with (2,1,1)(2,1,2)_12  

summary(modelone)

#Box-Ljung to check whether residuals are white noise:
acf(residuals(modelone))

Box.test(residuals(modelone), lag=24, fitdf = 1, type="Ljung")

#rebuild ARIMA(6 1 1)(2 1 2) model with different non seasonal terms

modeltwo <- arima(sales_data, order = c(6,1,1),
                  seasonal = list(order = c(2,1,2), period = 12))
summary(modeltwo)

#has smaller AIC value

acf(residuals(modeltwo))
Box.test(residuals(modeltwo), lag = 24, fitdf = 1, type = "Ljung")
#well, we got our model. now let's forecast:

sales_forecast <- forecast(modeltwo, h=30)
sales_forecast
plot(sales_forecast, ylab="Sales (in millions dollars", xlab = "Year", col = "darkred")





  
