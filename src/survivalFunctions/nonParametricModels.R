install.packages('survival')

data <- read.csv('src/survivalFunctions/surv_data.csv')

data$EventCode <- ifelse(data$Reason == 'Death', 1, 0)

data$EventDuration <- data$End - data$Op_time

surv.obj <- survival::Surv(data$EventDuration, data$EventCode)

fitKM <- survival::survfit(surv.obj~1, conf.type = 'plain', conf.int = .95)

summary(fitKM)

fitNA <- survival::survfit(surv.obj~1, conf.type = 'plain', stype = 2, conf.int = .95)

summary(fitNA)

plot(fitKM)

plot(fitNA, conf.int = FALSE)

plot(fitKM, main = "KM and NA survival function estimates", xlab =
"time (days)", ylab = "S(t)", col = "orange")

lines(fitNA, col = "blue", lwd = 1)

legend("topright", legend = c("KM", "NA"), col = c("orange",
"blue"), lwd = 1)

fitKM_group <- survival::survfit(surv.obj~data$Group, conf.type = 'plain', conf.int = .95)

plot(fitKM_group, col = c('orange', 'royal blue'))

legend('topright', legend = c('A', 'B'), col = c('orange', 'royal blue'), lwd = 2)

deaths <- table(data$EventDuration[data$EventCode == 1])

deaths_df <- as.data.frame(deaths)

colnames(deaths_df) <- c('tj', 'dj')

deaths_df$tj = as.numeric(deaths_df$tj)