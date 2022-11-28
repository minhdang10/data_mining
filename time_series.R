coffee = read.table('Coffee_prices_2013.txt', sep = '\t', header = TRUE)
str(coffee)
# 1. time series plot price~time. What components?
plot(coffee$time, coffee$price, xlab = "Coffee Price", 
     ylab = "Time", main = "Time Series of Price Against Time")
lines(coffee$time, coffee$price)

#2. SMA(2) and (8). Compare!
install.packages("TTR")
library(TTR)

sma1 = SMA(coffee$price, n = 2)
sma2 = SMA(coffee$price, n = 8)

par(mfrow = c(1,2))

plot(coffee$time, coffee$price, xlab = "Coffee Price", 
     ylab = "Time", main = "SMA of Length 2")
lines(coffee$time, coffee$price)
lines(coffee$time, sma1, col = "red")

plot(coffee$time, coffee$price, xlab = "Coffee Price", 
     ylab = "Time", main = "SMA of Length 8")
lines(coffee$time, coffee$price)
lines(coffee$time, sma2, col = "green")

#3. SES(0.8) and (0.2). Compare! n=1 la cai gi?
ema1 = EMA(coffee$price, ratio = 0.8, n = 1)
ema2 = EMA(coffee$price, ratio = 0.2, n = 1)

#par(mfrow = c(1,2))

plot(coffee$time, coffee$price, xlab = "Coffee Price", 
     ylab = "Time", main = "EMA (alpha = 0.8)")
lines(coffee$time, coffee$price)
lines(coffee$time, sma1, col = "orange")

plot(coffee$time, coffee$price, xlab = "Coffee Price", 
     ylab = "Time", main = "EMA (alpha = 0.2)")
lines(coffee$time, coffee$price)
lines(coffee$time, sma2, col = "purple")

# 4. autocorrelation (5), autoregressive (> 0.8). Write down model, add smooth; 
# which lag is dependent on most???? Why?
acf(coffee$price, lag.max = 5, plot = FALSE)

arm = ar(coffee$price, aic = FALSE, order.max = 3, demean = FALSE,
         intercept = TRUE, method = "ols"); arm
arm
fitted.arm = coffee$price - arm$resid

par(mfrow = c(1,2))

plot(coffee$time, coffee$price, xlab = "Coffee Price", 
     ylab = "Time", main = "AR(3)")
lines(coffee$time, coffee$price)
lines(coffee$time, fitted.arm, col = "blue")

plot(coffee$time, arm$resid, xlab = "Year", 
     ylab = "Residuals", main = "Residuals Plot")
abline(0,0)

# 5. y = 138.90. APE for each, which one is the best?
y.sma1 = sma1[length(sma1)]
y.sma2 = sma2[length(sma2)]
y.ema1 = ema1[length(ema1)]
y.ema2 = ema2[length(ema2)]
y.arm = predict(arm, n.head = 1, se.fit = FALSE) #--> 1: # of time pts want to forecast

y.next = 138.90
abs(y.next - y.sma1)/abs(y.next)*100
abs(y.next - y.sma2)/abs(y.next)*100
abs(y.next - y.ema1)/abs(y.next)*100
abs(y.next - y.ema2)/abs(y.next)*100
abs(y.next - y.arm)/abs(y.next)*100
