library(TSA)
library(tseries)
library(zoo)
library(forecast)

# import data
data <- read.csv2("heathrow_data.csv")
traffic <- ts(data[,3], start = c(2005,1), frequency = 12)

#=================================
# step 0: stationarization of data
#=================================

# plot original data
plot.ts(traffic, main = "Times Series of Traffic")
acf(ts(traffic, frequency = 1))
pacf(ts(traffic, frequency = 1))

# log transformation
traffic.log <- log(traffic)
plot(cbind(traffic, traffic.log), main = "Traffic and Its Log Transformation")
acf(ts(traffic.log, frequency = 1))
pacf(ts(traffic.log, frequency = 1))

# 1st order difference: remove the trend
traffic.log.d <- diff(traffic.log, 1)
plot(traffic.log.d, main = "1st Order Difference")
acf(ts(traffic.log.d, frequency = 1))
pacf(ts(traffic.log.d, frequency = 1))

# 12th order difference: remove the seasonality
traffic.log.d12 <- diff(traffic.log.d, 12)
plot(traffic.log.d12, main = "12th Order Difference")
acf(ts(traffic.log.d12, frequency = 1))
pacf(ts(traffic.log.d12, frequency = 1))

#======================================
# step 1: identifications of order p, q
#======================================

# p = P = 2
# q = Q = 1
# d = D = 1
# s = 12

#========================================
# step 2: estimation of ARMA coefficients
#========================================

mod1 <- arima(traffic.log, order = c(2, 1, 1), seasonal = list(order = c(2, 1, 1), period = 12), method = "ML")
mod1

# plot the fitted value
fit1 <- fitted(mod1)
plot.ts(cbind(traffic.log, fit1), plot.type = "single", col = c("blue", "red"))

#=======================
# step 3: model checking
#=======================

# significance of coefficients
tstat <- mod1$coef/sqrt(diag(mod1$var.coef))
pvalue1 <- 2 * (1 - pnorm(abs(tstat)))
pvalue1

# coefficients are mostly insignificant
# go back to step 2 for re-estimation

# p = 1
# P = 0
# q = 1
# Q = 1
# d = D = 1
# s = 12

mod2 <- arima(traffic.log, order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12), method = "ML")
mod2

# significance of coefficients
tstat <- mod2$coef/sqrt(diag(mod2$var.coef))
pvalue2 <- 2 * (1 - pnorm(abs(tstat)))
pvalue2

# Homooscedasticity
h.test <- McLeod.Li.test(mod2, plot = T)
h.test

# residuals analysis
res <- mod2$residuals
plot(res)

# plot the fitted value
fit2 <- fitted(mod2)
plot.ts(cbind(traffic.log, fit2), plot.type = "single", col = c("blue", "red"))

# check the normality of residuals
jarque.bera.test(res)

# qq plot
qqnorm(res)
qqline(res)

# autocorrelation of residuals (white noise assumption)
acf(ts(res, frequency = 1))
pacf(ts(res, frequency = 1))
Box.test(res, lag = 20, type = "Ljung-Box")

# normal distribution
res_norm <- res/sqrt(mod2$sigma2)
summary(res_norm)

# find the outlier  
out1 <- which(res_norm < -2)
out1
index(res_norm)[out1] # date of the outlier
res_norm[out1] # value of the outlier
traffic[out1] # traffic data of the outlier

# Create a dummy variable
data$dum_1 <- 0
data$dum_1[out1] <- 1
mod3 <- arima(traffic.log, order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12), method = "ML", xreg = data$dum_1)
mod3

# plot the fitted value
fit3 <- fitted(mod3)
plot.ts(cbind(traffic.log, fit3), plot.type = "single", col = c("blue", "red"))

# significance of coefficients
tstat <- mod3$coef/sqrt(diag(mod3$var.coef))
pvalue3 <- 2 * (1 - pnorm(abs(tstat)))
pvalue3

# residuals analysis
res3 <- mod3$residuals
plot(res3)

shapiro.test(res3)
jarque.bera.test(res3)

# qq plot
qqnorm(res3)
qqline(res3)

# Homooscedasticity
h.test <- McLeod.Li.test(mod3, plot = T)
h.test

# check SBC information criteria for model selection
AIC(mod1, mod2, mod3)
BIC(mod1, mod2, mod3)

#===================
# step 4: prediction
#===================

# in sample and out sample analysis
data.train <- window(traffic.log, start=c(2005,1), end=c(2015,1))
data.test <- window(traffic.log, start=c(2015,2), end=c(2019,12))

mod.train <- arima(data.train, c(1, 1, 1), seasonal = list(order=c(0, 1, 1), period = 12), method = 'ML')
pred.test <- predict(mod.train, n.ahead = 59)

# check the accuracy of the forecast
accuracy(pred.test$pred, data.test)

# quality of the fit
cb80 <- mod2$sigma2 ^ .5 * qnorm(0.9)
plot(cbind(traffic.log, fit2 - cb80, fit2 + cb80), plot.type = "single", lty = c(1,2,2))

# proportion of points in the confidence bound
indi <- (traffic.log - (fit2 - cb80))>0&(fit2 + cb80 - traffic.log)>0
prop <- 100 * sum(indi)/length(indi)
prop

# prediction
p <- predict(mod2, n.ahead = 3)
ts.plot(traffic, 2.718 ^ p$pred, log = "y", lty = c(1,3))
ts.plot(traffic, xlim = c(2016, 2020.167))
lines(2.718 ^ (p$pred), col = "red")
lines(2.718 ^ (p$pred - 1.96 * p$se), col = 4,lty = 2)
lines(2.718 ^ (p$pred + 1.96 * p$se), col = 4,lty = 2)
