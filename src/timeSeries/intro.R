acf <- ARMAacf(ar = .7, lag.max = 12)

pacf <- ARMAacf(ar = .7, lag.max = 12, pacf = TRUE)

par(mfrow = c(1, 2))

barplot(acf[-1], xlab = 'LAG', ylab = 'ACF', main = 'ACF of AR(1)', col = 'RED')

barplot(pacf, xlab = 'LAG', ylab = 'PACF', main = 'PACF of AR(1)', col = 'RED', names.arg = 1:12) 

Yt <- arima.sim(model = list( ar = c(0.5, -0.1), ma = c(0.2)), n = 100, sd = 3)

Wt <- 10 + arima.sim(model = list( ar = c(0.8), ma = c(0.4, 0.1)), n = 50, sd = sqrt(5))

Zt <- Wt + 0.5*seq(1, length(Wt))

Xt <- ts(cumsum(Yt))
