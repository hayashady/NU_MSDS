
#   Assignment 6 

require(fpp2)
require(fGarch)
require(devtools)
require(quantmod)
require(ggplot2)
require(nortest)
require(TSA)
require(forecast)
require(NTS)
require(fpp2)
require(fpp3)
require(urca)
require(fUnitRoots)
require(stats)
require(DescTools)
require(lmtest)
source("/Users/dylanhayashi/Desktop/Northwestern/413 - Time-Series Analysis & Forecasting/TS6/garchM.R")
source("/Users/dylanhayashi/Desktop/Northwestern/413 - Time-Series Analysis & Forecasting/TS6/Igarch.R")
source("/Users/dylanhayashi/Desktop/Northwestern/413 - Time-Series Analysis & Forecasting/TS6/Tgarch11.R")
options(scipen = 999)

# Model Diagnostics


# Model diagnostics
checkresiduals(resi,lag=20)
basicStats(resi)
# Auto-Correlation
Box.test(resi,lag=12,type='Ljung')    # H0: independent vs. Ha: not independent
# Normality
Skew(resi, conf.level = 0.95, ci.type = 'norm')
Kurt(resi, conf.level = 0.95, ci.type = 'norm')
# Stationarity
t.test(resi)
McLeod.Li.test(y=resi)   # any lags <= 0.05?
adfTest(resi,lags=5,type="nc")
kpss <- ur.kpss(resi,type="tau",lags="short")
summary(kpss)

# Business Cycles
(p2 <- c(1,-coefficients(m)))
(s2 <- polyroot(p2))
(z = unique(round(sapply(all_complex(s2),period),digits=3)))   # lengths of business cycles






##############################################
#   Problem 1


# Read Data
symbol.vec <- c("MSFT")
getSymbols(symbol.vec, from ="2010-01-04", to = "2022-09-30")
MSFT <- MSFT[, "MSFT.Adjusted", drop=F]
X <- as.data.frame(MSFT)
X$Day = c(1:3208)
X$Price = X$MSFT.Adjusted  

# EDA
ggplot() + geom_line(data=X,aes(x =Day,y=Price)) + ggtitle("Line Plot of MSFT Price")
basicStats(X$Price)
ggplot(X,aes(x=Price))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of Price")+labs(x='Price',y='Density')
ggplot(X,aes(sample=Price))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of Price")+labs(x='Theoretical',y='Sample')
Skew(X$Price, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000) # Skew 
Kurt(X$Price, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)  # Excess Kurtosis

# Log Data
X$log_price = log(X$Price)
ggplot() + geom_line(data=X,aes(x =Day,y=log_price)) + ggtitle("Line Plot of Log MSFT Price")
ggplot(X,aes(x=log_price))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of Log Price")+labs(x='Price',y='Density')
ggplot(X,aes(sample=log_price))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of Log Price")+labs(x='Theoretical',y='Sample')
Skew(X$log_price, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000) # Skew 
Kurt(X$log_price, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)  # Excess Kurtosis
t.test(X$log_price)

# Diff Log Data
Y = data.frame(diff(X$log_price))
Y$Day = c(1:3207)
Y$diff_log_price= Y$diff.X.log_price.
ggplot() + geom_line(data=Y,aes(x =Day,y=diff_log_price)) + ggtitle("Line Plot of Differenced Log MSFT Price")
t.test(Y$diff_log_price)

# Create timeseries
msft <- ts(diff(log(X$MSFT.Adjusted)))

# Stationarity
adfTest(msft,lags=5,type="nc") # ADF - Random Walk Stationarity
kpss <- ur.kpss(msft,type="tau",lags="short") # KPSS - Linear Trend Stationarity
summary(kpss)    

# Decomposition 
msft <- ts(diff(log(X$MSFT.Adjusted)),frequency=365)
decomp <- stl(msft,"periodic")
autoplot(decomp)


# Auto-Correlation
ggAcf(msft)
ggPacf(msft)


# Section 1.2 - Mean Model
m <- Arima(msft,order=c(1,0,1),include.mean=TRUE)
(m12 <- m)

arch.test(resid(m),arch="box",alpha=0.05,lag.max = 30)   # arch effects? change lag.max


# Section 1.3 - GARCH Model with Gaussian distribution
m <- garchFit(~arma(1,1)+garch(1,1),data=msft,trace=F)  # arma from EDA
m13 <- m
summary(m)
autoplot(ts(m@residuals))
ggAcf(m@residuals)
ggAcf(m@residuals^2)
qplot(sample=m@residuals) + stat_qq_line(color='Blue') + ggtitle("QQ-Plot of Residuals" )+labs(x='Theoretical',y='Sample')
resi = m@residuals
arch.test(resi,arch="box",alpha=0.05,lag.max = 30)   # arch effects? change lag.max



# Section 1.4 - GARCH Model with Student's t-distribution
m <- garchFit(~arma(0,1)+garch(1,1),data=msft,trace=F,cond.dist="std")   # Student's t innovations
m14 <- m
summary(m)
autoplot(ts(m@residuals))
ggAcf(m@residuals)
ggAcf(m@residuals^2)
qplot(sample=m@residuals) + stat_qq_line(color='Blue') + ggtitle("QQ-Plot of Residuals" )+labs(x='Theoretical',y='Sample')
resi = m@residuals
arch.test(resi,arch="box",alpha=0.05,lag.max = 30)   # arch effects? change lag.max

# Section 1.5 - Forecasts
pm <- predict(m,90)
head(pm)
pred <- c(ts(msft), pm$meanForecast)
autoplot(pred) +
	ggtitle("Daily Microsoft Returns") +
	geom_vline(xintercept = length(msft), col="red") +
	xlab("Day") + ylab("Return")



# Section 1.6 - IGARCH
m <- Igarch(msft)
m16 <- m
str(m)
sigma.t <- m$volatility
resi <- msft/sigma.t
autoplot(resi)
ggAcf(resi)
ggAcf(resi^2)
Box.test(resi,lag=10,type='Ljung-Box')
Box.test(resi^2,lag=10,type='Ljung-Box')
basicStats(resi)
qplot(sample=resi) + stat_qq_line(color='Blue') + ggtitle("QQ-Plot of Residuals" )+labs(x='Theoretical',y='Sample')





# use your Igarch model parameter 
pred <- msft
for (i in 1:5) {
	length(pred)
	v1 <- (1-m$par)*pred[length(pred)]^2 + m$par*sigma.t[length(pred)]^2
	sqrt(v1)
	pred <- c(pred,sqrt(v1))
	}

autoplot(ts(pred)) +
	ggtitle("Microsoft Volatility") +
	geom_vline(xintercept = length(msft), col="red") +
	xlab("Day") + ylab("Return")

autoplot(ts(tail(pred,n=90))) +
	ggtitle("Microsoft Volatility") +
	geom_vline(xintercept = (length(tail(pred,n=90))-5), col="red") +
	xlab("Day") + ylab("Return")




##############################################
#   Problem 2

# Read Data
symbol.vec <- c("BA")
getSymbols(symbol.vec, from ="2010-01-04", to = "2022-09-30")
BA <- BA[, "BA.Adjusted", drop=F]
X <- as.data.frame(BA)
X$Day = c(1:3208)
X$Price = X$BA.Adjusted  

# EDA
ggplot() + geom_line(data=X,aes(x =Day,y=Price)) + ggtitle("Line Plot of BA Price")
basicStats(X$Price)
ggplot(X,aes(x=Price))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of Price")+labs(x='Price',y='Density')
ggplot(X,aes(sample=Price))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of Price")+labs(x='Theoretical',y='Sample')
Skew(X$Price, conf.level = 0.95, ci.type = 'norm') # Skew 
Kurt(X$Price, method = 3, conf.level = 0.95, ci.type = "norm", R = 1000)  # Excess Kurtosis



# Log Data
X$log_price = log(X$Price)
ggplot() + geom_line(data=X,aes(x =Day,y=log_price)) + ggtitle("Line Plot of Log BA Price")
ggplot(X,aes(x=log_price))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of Log Price")+labs(x='Price',y='Density')
ggplot(X,aes(sample=log_price))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of Log Price")+labs(x='Theoretical',y='Sample')
Skew(X$log_price, conf.level = 0.95, ci.type = 'norm') # Skew 
Kurt(X$log_price, method = 3, conf.level = 0.95, ci.type = "norm", R = 1000)  # Excess Kurtosis
t.test(X$log_price)
basicStats(X$log_price)

# Diff Log Data
Y = data.frame(diff(X$log_price))
Y$Day = c(1:3207)
Y$diff_log_price= Y$diff.X.log_price.
ggplot() + geom_line(data=Y,aes(x =Day,y=diff_log_price)) + ggtitle("Line Plot of Differenced Log BA Price")
t.test(Y$diff_log_price)
basicStats(Y$diff_log_price)

# Create time series
ba <- ts(diff(log(X$BA.Adjusted)))   # or your choice of transformed ts

# Decomposition
decomp <- stl(ba,"periodic")
autoplot(decomp)

# ACF/PACF
ggAcf(ba)
ggPacf(ba)






# Uhhhh...?
at <- ba-mean(ba)
Box.test(at^2,lag=12,type='Ljung-Box')



# Section 2.2 - GARCH with Gaussian distribution
m <- garchFit(~garch(1,1),data=ba,trace=F)
m22 <- m
resi = resid(m)
summary(m)
qplot(sample=resid(m)) + stat_qq_line(color='Blue') + ggtitle("QQ-Plot of Residuals" )+labs(x='Theoretical',y='Sample')
ggAcf(resid(m))
ggAcf(resid(m)^2)



# Section 2.3 - GARCH with Student's t-distribution
m <- garchFit(~garch(1,1),data=ba,trace=F,cond.dist="sstd")   # skew-Student's t
m <- m23
summary(m)
resi = m@residuals
qplot(sample=m@residuals) + stat_qq_line(color='Blue') + ggtitle("QQ-Plot of Residuals" )+labs(x='Theoretical',y='Sample')
ggAcf(m@residuals)
ggAcf(m@residuals^2)


# Section 2.4 - GARCHM Model
m24 <- garchM(ba,type=1)
m <- m24
summary(m)
resi = resid(m)
qplot(sample=resi) + stat_qq_line(color='Blue') + ggtitle("QQ-Plot of Residuals" )+labs(x='Theoretical',y='Sample')
ggAcf(resi)
ggAcf(resi^2)


# Section 2.5 - TGARCH Model
m25 <- Tgarch11(ba)
m <- m25
summary(m)
resi = resid(m)
qplot(sample=resi) + stat_qq_line(color='Blue') + ggtitle("QQ-Plot of Residuals" )+labs(x='Theoretical',y='Sample')
ggAcf(resi)
ggAcf(resi^2)



##############################################
