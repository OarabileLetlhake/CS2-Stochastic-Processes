testing.stationarity <- read.table("src/timeSeries/testing.stationarity.txt")

head(testing.stationarity)

testing.stationarity <- ts(testing.stationarity)

acf(testing.stationarity, main = "ACF for Testing Data", ylab = "ACF")
# Doesn't decrease exponentially towards zero

PP.test(testing.stationarity) # p-value greater than 5% means we have evidence of stochastic trend

set.seed(1901)

n <- 365

data <- arima.sim(model = list(ma = c(-1.4, 0.8)), n = n)

acf(data, main = "Time series: data", ylab = "Sample ACF")

PP.test(data) # p-value less than 0.05 means we don't have to difference

Xt <- diff(testing.stationarity, lag = 1, differences = 1)

layout(matrix(c(1,2,3,4), byrow = TRUE, nrow = 2))

ts.plot(testing.stationarity, main = 'Data: testing.stationarity', ylab = 'value')

ts.plot(Xt, main = 'Data: Xt', ylab='Value')

acf(testing.stationarity, main = "Data: testing.stationarity", ylab = "Sample ACF")

acf(Xt, main = "Data: Xt", ylab = "Sample ACF")

PP.test(Xt)

par(mfrow = c(1,1))

var(testing.stationarity)

var(Xt)

d2t <- diff(Xt, lag = 1, differences = 1)

var(d2t)

set.seed(123)

sim <- arima.sim(model = list(ar = 0.9), n = 1000)

xt <- sim + 2 + 0.05*time(sim)

plot(xt, main = 'Time Series with Deterministic Trend')

fit <- lm(xt~time(sim))

yt <- fit$fitted.values

zt <- fit$residuals

layout(matrix(c(1,2,3,3), nrow = 2, byrow = TRUE))

plot(xt, col ='blue', main = "Regression Example", ylab = 'Data')
abline(fit, col = 'red')

acf(xt, main = 'ACF for Xt', ylab = 'Sample ACF')

plot(ts(zt), type = 'l', col = "dark green", ylab = 'Residuals')

PP.test(zt)

layout(matrix(c(1,1)))

acf(zt, ylab = 'Sample ACF', main = 'Residuals ACF')

var(zt)

var(diff(zt))

# Variance decreased with differencing, but the PP test suggested no evidence to difference.

################################################################################

# Checking for seasonality
par(mfrow = c (1,1))
plot(ldeaths, main = 'Monthly deaths from lung disease in the UK', ylab = 'Deaths')
points(ldeaths, pch = 10)

abline(v=1974:1980, col = 'red', lwd = 2)

par(mfrow = c(2,2))

acf(ldeaths, lag.max = 36)

acf(ldeaths, lag.max = 24)

acf(ldeaths, lag.max = 15)

acf(ldeaths, lag.max = 2)

ldeaths.df <- data.frame(year = rep(1974:1979, each = 12))