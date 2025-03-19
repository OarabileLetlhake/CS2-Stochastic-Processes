ts_data <- ts(read.csv('fittingmodelEg1.csv', header = FALSE))

layout(matrix(c(1,2,1,3),2, 2))

ts.plot(ts_data, main = "What Model To Fit?", ylab = "Data: Example 1")

acf(ts_data, main ="")
pacf(ts_data, main = "")

par(mfrow = c(1,1))

PP.test(ts_data)

ts_data2 <- ts(read.csv('fittingmodelEg2.csv', header = FALSE))

layout(matrix(c(1,1,2,3), byrow = TRUE, nrow = 2))

ts.plot(ts_data2, main = 'MA(q) Example', ylab = 'Data: Example 2')
acf(ts_data2, main = '') # ACF cuts off after lag 3, suggesting possibly an MA(3) since data is fairly stationary
pacf(ts_data2, main = '')

par(mfrow = c(1,1))

ts_data4 <- ts(read.csv('fittingmodelEg4.csv', header = FALSE))

layout(matrix(c(1,1,2,3), byrow = TRUE, nrow = 2))

ts.plot(ts_data4, main = "Coding Exercise Example", ylab="")
acf(ts_data4, main='')
pacf(ts_data4, main = '')

ts.plot(diff(ts_data4), main = "Coding Exercise After Differencing", ylab="")
acf(diff(ts_data4), main='')
pacf(diff(ts_data4), main = '')

ts.plot(diff(ts_data4, differences = 2), main = "Coding Exercise After Differencing", ylab="")
acf(diff(ts_data4, differences = 2), main='')
pacf(diff(ts_data4, differences = 2), main = '')

var(ts_data4)
var(diff(ts_data4))
var(diff(ts_data4, differences = 2))

#### Difference twice and cutt off after lag 3 so possibly ARIMA(0,2,3)

ts_data3 <- ts(read.csv('fittingmodelEg3.csv', header = FALSE))

ar_model <- arima(ts_data3, order = c(2,0,0))

ar_model

tsdiag(ar_model)

Box.test(ar_model$residuals, lag = 10, fitdf = 2)
