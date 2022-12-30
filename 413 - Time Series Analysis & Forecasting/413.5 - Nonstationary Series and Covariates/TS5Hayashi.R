
######   Assignment 5   ##########

require(ggplot2)  
require(TSA)
require(forecast)
require(NTS)
require(fpp2)
require(fpp3)
require(urca)
require(fUnitRoots)
require(stats)
require(DescTools)
require(nortest)
require(lmtest)
options(scipen = 999)

# Model diagnostics
checkresiduals(m,lag=20)
basicStats(resid(m))
# Auto-Correlation
Box.test(resid(m),lag=12,type='Ljung')    # H0: independent vs. Ha: not independent
# Normality
shapiro.test(resid(m)) # Shapiro Test
lillie.test(resid(m)) # Lillie Test
ad.test(resid(m)) # AD Test
Skew(resid(m), method = 3, conf.level = 0.05, ci.type = "bca", R = 1000)
Kurt(resid(m), method = 3, conf.level = 0.05, ci.type = "bca", R = 1000)
# Stationarity
t.test(resid(m))
McLeod.Li.test(y=resid(m))   # any lags <= 0.05?
adfTest(resid(m),lags=5,type="nc")
kpss <- ur.kpss(resid(m),type="tau",lags="short")
summary(kpss)
# Business Cycles
(p2 <- c(1,-m$coef))
(s2 <- polyroot(p2))
(z = unique(round(sapply(all_complex(s2),period),digits=3)))   # lengths of business cycles
# Backtest
backtest(m,X[,2],length(X[,2])-4,4)

####### Section 1 - Outlier Management #######

## Section 1.1 - EDA ##

# Read data
X <- read.table("/Users/dylanhayashi/Desktop/Northwestern/413 - Time-Series Analysis & Forecasting/TS5/m-PastorStambaugh.txt",header=T)

# Time Series Validation
dc <- as.character(X$DATE)
dc <- paste0(substr(dc,1,6),"01")
dc <- paste0(substr(dc,1,4),"-",substr(dc,5,6),"-",substr(dc,7,8))
str(dc)
dt <- as.Date(dc)
str(dt)
X$DATE <- dt
rm(dc,dt)
str(X)
df <- round(diff(X$DATE)/(365.25/12))
table(df)


# Line plot of raw data
ggplot() + geom_line(data = X,aes(x=DATE,y=PS_LEVEL)) + ggtitle('Line Plot of Liquidity Measure') + labs(x='Year',y='PS Level')
shapiro.test(X$PS_LEVEL) # Shapiro Test
lillie.test(X$PS_LEVEL) # Lillie Test
ad.test(X$PS_LEVEL) # AD Test


# Check stationarity  - No
basicStats(X$PS_LEVEL)
t.test(X$PS_LEVEL) # Not mean zero 


# Create dataframe with differenced variable 
Y = data.frame(diff(X$PS_LEVEL))
Y$Value = Y$diff.X.PS_LEVEL.
Y$Date = X$DATE[2:605]


# Create lineplot with differenced data
ggplot() + geom_line(data = Y,aes(x=Date,y=Value)) + ggtitle('Line Plot of Liquidity Measure') + labs(x='Year',y='PS Level')


# Confirm stationarity - Yes
basicStats(Y$Value) # summary table
t.test(Y$Value) # Mean zero?
Y_ts = ts(Y$Value)
adfTest(Y_ts,lags=5,type="nc") # ADF - Random Walk Stationarity
kpss <- ur.kpss(Y_ts,type="tau",lags="short") # KPSS - Linear Trend Stationarity
summary(kpss)    


# Check for normality - Nope
ggplot(Y,aes(x=Value))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of Differenced Liquidity Score")+labs(x='Index',y='Density')
ggplot(Y,aes(sample=Value))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of Differenced Liquidity Score")+labs(x='Theoretical',y='Sample')
Skew(Y$Value, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000) # Skew 
Kurt(Y$Value, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)  # Excess Kurtosis
shapiro.test(Y$Value) # Shapiro Test
lillie.test(Y$Value) # Lillie Test
ad.test(Y$Value) # AD Test


# Seasonal Decomposition
Y_ts_period = ts(X$PS_LEVEL,frequency=12)
decomp <- stl(Y_ts_period,"periodic")
autoplot(decomp)



# Section 1.2 - Model 1

# Find p, q for ARIMA
ggAcf(X$PS_LEVEL) # ACF raw data - MA
ggPacf(X$PS_LEVEL) # PACF raw data - AR


# Create timeseries object for model building
H <- X
X <- ts(H)


# Model 1
m <- Arima(X[,2],order=c(3,1,6),include.mean=F)   # likely 5,0,0
m1 = m

# Section 1.3 - Model 2

# Be sure the outlier is index value 303
which.min(m$residuals)
dim(X)
i303 <- rep(0,605)
i303[303] <- 1

# Model 2
m <- Arima(X[,2],order=c(3,1,6),xreg=i303,include.mean=F)   # order same as model m11

# Section 1.4 - Model 3

# t test for coefficient significance from model m12
parameterTest(m)   # diff.order defaults to 0, no differencing

# Model 3
c11 = c(NA,NA,0,NA,NA,NA,NA)
m <- Arima(X[,2],order=c(0,1,6),xreg=i303,fixed=c11)   # order same as model m11



##############################################
#   Problem 2

X <-read.table("/Users/dylanhayashi/Desktop/Northwestern/413 - Time-Series Analysis & Forecasting/TS5/m-FamaBlissdbndyields.txt",header=T)
dc <- as.character(X$qdate)
dc <- paste0(substr(dc,1,6),"01")
dc <- paste0(substr(dc,1,4),"-",substr(dc,5,6),"-",substr(dc,7,8))
str(dc)
dt <- as.Date(dc)
str(dt)
X$qdate <- dt
rm(dc,dt)
str(X)
df <- round(diff(X$qdate)/(365.25/12))
table(df)


# Section 2.1 - EDA

# Line Plot
ggplot() + geom_line(data=X,aes(x=qdate,y=yield3)) + ggtitle('Line Plot of 3-Year Bond Yield') + labs(x='Year',y='Yield')
basicStats(X$yield3)

# Normally Distributed?
ggplot(X,aes(x=yield3))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of 3-Year Bond Yield")+labs(x='Yield',y='Density')
ggplot(X,aes(sample=yield3))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of 3-Year Bond Yield")+labs(x='Theoretical',y='Sample')
Skew(X$yield3, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000) # Skew 
Kurt(X$yield3, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)  # Excess Kurtosis
shapiro.test(X$yield3) # Shapiro Test
lillie.test(X$yield3) # Lillie Test
ad.test(X$yield3) # AD Test

# Apply log transformation
X$yield3_log = log(X$yield3)
y3t <- X[,3]
xt <- log(y3t)


# Normally Distributed? - No
ggplot(X,aes(x=yield3_log))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of Log Transformed 3-Year Bond Yield")+labs(x='Yield',y='Density')
ggplot(X,aes(sample=yield3_log))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of Log Transformed 3-Year Bond Yield")+labs(x='Theoretical',y='Sample')
Skew(xt, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000) # Skew 
Kurt(xt, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)  # Excess Kurtosis
shapiro.test(xt) # Shapiro Test
lillie.test(xt) # Lillie Test
ad.test(xt) # AD Test

# Stationary? - No
basicStats(X$yield3_log)
t.test(X$yield3_log)
McLeod.Li.test(y=ts(X$yield3_log))

# Apply first differencing
Y = data.frame(diff(X$yield3_log))
Y$yield3_log = Y$diff.X.yield3_log.
Y$qdate = X$qdate[2:636]

# Line Plot
ggplot() + geom_line(data=Y,aes(x=qdate,y=yield3_log)) + ggtitle('Line Plot of Differenced Log 3-Year Bond Yield') + labs(x='Year',y='Yield')


# Stationary? - No
basicStats(Y$yield3_log)
t.test(Y$yield3_log)
adfTest(Y$yield3_log,lags=5,type="nc")
kpss <- ur.kpss(Y$yield3_log,type="tau",lags="short")
summary(kpss)
McLeod.Li.test(y=ts(Y$yield3_log))


# Normal? - No
ggplot(Y,aes(x=yield3_log))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of Differenced Log 3-Year Bond Yield")+labs(x='Yield',y='Density')
ggplot(Y,aes(sample=yield3_log))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of Differenced Log 3-Year Bond Yield")+labs(x='Theoretical',y='Sample')
Skew(Y$yield3_log, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000) # Skew 
Kurt(Y$yield3_log, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)  # Excess Kurtosis
shapiro.test(Y$yield3_log) # Shapiro Test
lillie.test(Y$yield3_log) # Lillie Test
ad.test(Y$yield3_log) # AD Test

# Decomposition
decomp <- stl(ts(Y$yield3_log,frequency=12),"periodic")
autoplot(decomp)



# Section 2.2 - Model 1

# Determine p, q
ggAcf(diff(xt))
ggPacf(xt)

# Get auto arima recommendation
auto.arima(xt)

# Model 1 
m <- Arima(xt,order=c(1,1,1))
m21 = m


# Section 2.3 - Model 2
m <- Arima(xt,order=c(0,1,1),seasonal=list(order=c(0,0,1),period=4))
(m22 <- m)


# Section 2.4 - Model Comparison
backtest(m21,xt,length(xt)-1,1)
backtest(m22,xt,length(xt)-1,1)


##############################################
#   Problem 3

X <- read.table("/Users/dylanhayashi/Desktop/Northwestern/413 - Time-Series Analysis & Forecasting/TS5/m-FamaBlissdbndyields.txt",header=T)
dim(X)
head(X)
y1t <- X[,2]; y3t <- X[,3]


# Section 3.1 - Model 1
m <- lm(y3t~y1t)
m31 <- m
summary(m)


# Section 3.2 - Model 2

# lm model nonstationary
d1t <- diff(y1t); d3t=diff(y3t)

# Differenced 1 year bond yield EDA
y1 = data.frame(d1t)
y1$date = X$qdate[2:636]
ggplot() + geom_line(data=y3,aes(x=date,y=d1t)) + ggtitle('Time Plot of Differenced 1-Year Bond Yield') + labs(x='Year',y='Yield')
ggplot() + geom_line(data=y3,aes(x=date,y=d3t)) + ggtitle('Time Plot of Differenced 3-Year Bond Yield') + labs(x='Year',y='Yield')
adfTest(d1t,lags=5,type="nc")
kpss <- ur.kpss(d1t,type="tau",lags="short")
summary(kpss)
McLeod.Li.test(y=d1t)
ggplot(y1,aes(x=d1t))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of Differenced 1-Year Bond Yield")+labs(x='Yield',y='Density')
ggplot(y1,aes(sample=d1t))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of Differenced 1-Year Bond Yield")+labs(x='Theoretical',y='Sample')
shapiro.test(y1$d1t) # Shapiro Test
lillie.test(y1$d1t) # Lillie Test
ad.test(y1$d1t) # AD Test
decomp <- stl(ts(y1$d1t,frequency=12),"periodic")
autoplot(decomp)


# Differened 3 year bond yield EDA
y3 = data.frame(d3t)
y3$date = X$qdate[2:636]
adfTest(d3t,lags=5,type="nc")
kpss <- ur.kpss(d3t,type="tau",lags="short")
summary(kpss)
McLeod.Li.test(y=d3t)
ggplot(y3,aes(x=d3t))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of Differenced 3-Year Bond Yield")+labs(x='Yield',y='Density')
ggplot(y3,aes(sample=d3t))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of Differenced 3-Year Bond Yield")+labs(x='Theoretical',y='Sample')
shapiro.test(y3$d3t) # Shapiro Test
lillie.test(y3$d3t) # Lillie Test
ad.test(y3$d3t) # AD Test
decomp <- stl(ts(y3$d3t,frequency=12),"periodic")
autoplot(decomp)


# Model 2
m <- lm(d3t ~ -1 + d1t)    # linear regression, remove intercept => force through zero
m32 <- m
summary(m)
ggAcf(resid(m))







# Section 3.3 - Model 3
m <- ar(ts(resid(m)), method="mle")   # find p for model m33
m33 <- m
m$order
m <- Arima(ts(d3t), order=c(m33$order,0,0), xreg=d1t, include.mean=F)
m33 = m



# Section 3.4

# t test for coefficient significance from model m34
# source("parameterTest.R")
m <- 34
parameterTest(m)   # diff.order defaults to 0, no differencing

#         1  2  3  4  5  6   etc based on model m12
c31 <- c(NA,NA, 0,NA,NA,NA)
m <- Arima(d3t, order=c(5,0,0), xreg=d1t, include.mean=F, fixed=c31)
m35 = m












# This function implements the k = … equation from Tsay p. 42 (the one with a and b, not phi).
# Curtesy of Mark Heiple
source('BusCycle.R')
m <- m35
#build polynomial (1 + the negative of the coefficients)
(p2 <- c(1,-m$coef))
(s2 <- polyroot(p2))    # if no imaginary part, no cycles
#complex roots are in pairs, so use unique() 
#using round() because there may be very small differences
(z = unique(round(sapply(all_complex(s2),period),digits=3)))   # lengths of business cycles

# inverse of the absolute values without xreg coef
p1 <- c(1,-m$coef[-6])
s1 <- polyroot(p1)
s1
Mod(s1)
1/Mod(s1)



m = m31

m31 <- Arima(y3t, order=c(0,0,0), xreg=y1t, include.mean=F)
backtest(m31,y3t,length(y3t)-4,4,include.mean=F)


backtest(m34,d3t,length(d3t)-4,4)



m = m34
y1tf <- forecast(auto.arima(y1t, d=1, stationary=F), h=12)

m <- Arima(y3t, order=c(5,1,0), xreg=diff(y1t), include.mean=F, fixed=c31)


#fc <- forecast(m, xreg=y1tf$mean )  # 12 weeks
nweeks <- (length(y1tf$fitted)-25):length(y1tf$fitted)
fc <- forecast(m, xreg=y1tf$fitted[nweeks] ) # 26 week-forcast
autoplot(fc) +
  ggtitle(paste("Fama-Bliss Forecasted Yields for ", length(nweeks), "Weeks")) +
  geom_vline(xintercept = nrow(X), col="red") +
  xlab("Week") + ylab("Yield")
accuracy(fc)


##############################################
