library(prophet)
library(lubridate)
library(ggplot2)

#Read in Ethereum data
data <- read.csv(file.choose(),header = T)
str(data)
head(data)
data$Date <- dmy(data$Date)

#Plot
qplot(Date,Close, data = data,
      main = "Ethereum Closing Prices 2015-2019")
#Log Transformation
ds <- data$Date
y <- log(data$Close)
df <- data.frame(ds,y)
qplot(ds,y,data = df,
      main = "Ethereum Closing Prices 2015-2019 (Log Transformation)")

#Forecasting
m <- prophet(df)
future <- make_future_dataframe(m, periods = 365)
tail(future)
forecast <- predict(m, future)

#Plot Forecast
plot(m,forecast)
dyplot.prophet(m,forecast)
prophet_plot_components(m,forecast)

#Model Performance
pred <- forecast$yhat[1:1544]
actual <- m$history$y
plot(actual,pred)
abline(lm(pred~actual),col = 'red')
summary(lm(pred~actual))
x <- cross_validation(m,365,units="days")
performance_metrics(x, rolling_window = 0.1)
plot_cross_validation_metric(x,
                             metric = "rmse",
                             rolling_window = 0.1)
