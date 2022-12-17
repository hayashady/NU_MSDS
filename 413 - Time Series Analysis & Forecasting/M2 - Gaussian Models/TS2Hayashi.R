# Dylan Hayashi
# MSDS 413
# Assignment 2

require("fpp2")
require("DescTools")
require("urca")
require("fUnitRoots")
require("TSA")
require("NTS")
require("forecast")



################################################
# Problem 1
# UM Consumer Sentiment


# Input and condition the data
UMCSENT <- read.csv("/Users/dylanhayashi/Desktop/Northwestern/413 - Time-Series Analysis & Forecasting/TS2/tbmics.csv",header=T,stringsAsFactors=F)
summary(UMCSENT);nrow(UMCSENT)

# Working data
W <- UMCSENT

# Convert month name to digit
Mon <- match(W$Month, month.name)
month <- paste0(as.character(W$YYYY),"-",as.character(Mon),"-01")
lt <- as.POSIXlt(as.Date(month))
num <- lt$year*12 + lt$mon
W$mon.num <- num - min(num)
head(W$mon.num)
dt <- diff(W$mon.num)
head(dt,n=100)
not1 <- max(which(dt!=1))

W$Index <- W$ICS_ALL
X <- W[(not1+1):nrow(W),-3]
head(X)
str(X)

rm(Mon, month, lt, num, dt, not1)


# Insert your EDA code here.

# Summary
head(X)
str(X)
summary(X$Index)

# Section 1.2 - Exploratory Data Analysis
# Summary Statistics
round(basicStats(X$Index),3)
# Time-Plot
ggplot()+geom_line(data=X,aes(x=mon.num,y=Index))+ggtitle('Time Plot of Consumer Sentiment Index')+labs(x='Month')
# Histogram
ggplot(X,aes(x=Index))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of Consumer Sentiment Index")+labs(x='Index',y='Density')
# QQ-Plot
ggplot(X,aes(sample=Index))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of Consumer Sentiment Index")+labs(x='Theoretical',y='Sample')
# Skew
Skew(X$Index, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)
# Kurtosis
Kurt(X$Index, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)  # gives excess kurtosis


################################################
# Problem 2

Index <- ts(X$Index)   # make a time series object

# Based on your EDA, use
d <- diff(Index)
d <- diff(log(Index))

# Insert your EDA code here
ggplot() + geom_line(aes(x=X$mon.num[2:530],y=d)) + ggtitle('Time Plot of Differenced Log Consumer Sentiment Index') + labs(x='Month',y='Differenced Log(Index)')

t.test(d)         # H0: mean = 0 vs. Ha mean != 0

Box.test(d,lag=12,type='Ljung')    # H0: independent vs. Ha: not independent

ggplot() + geom_histogram(aes(d))
# Histogram
ggplot(d,aes(x=d))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of Differenced Log Index")+labs(x='Index',y='Density')
# QQ-Plot
ggplot(X,aes(sample=Index))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of Consumer Sentiment Index")+labs(x='Theoretical',y='Sample')
# Skew
Skew(d, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)
# Kurtosis
Kurt(d, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)  # gives excess kurtosis


################################################
# Problem 3

# ARIMA(0,0,0)
m <- arima(d,order=c(0,0,0),include.mean=T)

# Model summary
(m1 <- m)

# Residuals panel
checkresiduals(m,lag=24)

# T Test for residuals mean = 0
t.test(resid((m)))

# Skew of residuals
Skew(resid(m), method = 3, conf.level = 0.05, ci.type = "bca", R = 1000)

# Kurtosis of residuals 
Kurt(resid(m), method = 3, conf.level = 0.05, ci.type = "bca", R = 1000)

# KPSS Test
# Test or unit roots. If no roots, stationary under H0
# KPSS test, H0: no unit roots vs. Ha unit roots
# Value <= critical, fail to reject
kpss <- ur.kpss(resid(m),type="tau",lags="short")
summary(kpss)    # If test stat <= 5pct, fail to reject

# ADF Test
# Unit roots. H0: unit roots vs. Ha: no unit roots. No roots => stationary

adfTest(resid(m),lags=5,type="nc")

# McLeod Test for constant varaince
McLeod.Li.test(m)    # any lags <= 0.05?

# Business Cycles
#build polynomial (1 + the negative of the coefficients)
(p2 <- c(1,-m$coef))
(s2 <- polyroot(p2))    # if no imaginary part, no cycles

# This function implements the k = … equation from Tsay p. 42 (the one with a and b, not phi).
# Curtesy of Mark Heiple
source('BusCycle.R')
#complex roots are in pairs, so use unique() 
#using round() because there may be very small differences
(z = unique(sapply(all_complex(s2),period)))  # lengths of business cycles




##### Problem 4 #####

## Section 4.1 - Autocorrelation ##
Box.test(d,lag=12,type='Ljung')    # H0: independent vs. Ha: not independent
ggAcf(d)
ggPacf(d)



## Section 2 - ARIMA (12,0,0) ##
# ARIMA(12,0,0)
m <- Arima(d,order=c(12,0,0),include.mean=F)
# Model summary
(m2 <- m)
# Residuals panel
checkresiduals(m,lag=24)
# T test for residuals mean = 0
t.test(resid(m))
# Skew of residuals
Skew(resid(m), method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)
# Kurtosis of residuals
Kurt(resid(m), method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)
# KPSS Test
# Test or unit roots. If no roots, statiionary
# KPSS test, H0: no roots vs. Ha roots
# Test stat < critical value, fail to reject 
kpss <- ur.kpss(resid(m),type="tau",lags="short")
summary(kpss)
# ADF Test
# Unit roots. H0: unit roots vs. Ha: no roots, stationary
adfTest(resid(m),lags=12,type="nc")
# Busisness Cycles
#build polynomial (1 + the negative of the coefficients)
(p2 <- c(1,-m$coef))
(s2 <- polyroot(p2))
#This function implements the k = … equation from Tsay p. 42 (the one with a and b, not phi).
# Curtesy of Mark Heiple
source('BusCycle.R')
#complex roots are in pairs, so use unique() 
#using round() because there may be very small differences
(z = unique(round(sapply(all_complex(s2),period),digits=3)))   # lengths of business cycles
parameterTest(m)




## Section 4.4 - Simplified AR Model ##
# Simplified AR model
c1 <- c(0,NA,0,0,NA,0,0,0,0,0,0,0)
# Model creation
m <- arima(d,order=c(12,0,0),include.mean=F,fixed=c1)
# Mode summary
(m3 <- m)
# Residuals panel
checkresiduals(m,lag=24)
# T test for residuals mean
t.test(resid(m))
# Skew of residuals
Skew(resid(m), method = 3, conf.level = 0.05, ci.type = "bca", R = 1000)
# Kurtosis of residuals
Kurt(resid(m), method = 3, conf.level = 0.05, ci.type = "bca", R = 1000)
# Test or unit roots. If no roots, statiionary
# KPSS test, H0: no roots vs. Ha roots
# Test stat < critical value, fail to reject 
kpss <- ur.kpss(resid(m),type="tau",lags="short")
summary(kpss)
# Unit roots. H0: unit roots vs. Ha: no roots, stationary
adfTest(resid(m),lags=12,type="nc")
#build polynomial (1 + the negative of the coefficients)
(p2 <- c(1,-coef(m)))
(s2 <- polyroot(p2))
#This function implements the k = … equation from Tsay p. 42 (the one with a and b, not phi).
# Curtesy of Mark Heiple
#complex roots are in pairs, so use unique() 
#using round() because there may be very small differences
(z = unique(round(sapply(all_complex(s2),period),digits=3)))   # lengths of business cycles






# Backtest, choose your "better" model m2 or m3

backtest(m2, d, 400, 4, xre = NULL, fixed = NULL, include.mean = F)
backtest(m3, d, 400, 4, xre = NULL, fixed = NULL, include.mean = F)


# Use your choice of m2 or m3
m <- m2
m <- m3
mp <- predict(m,4)   # prediction 1 to 4-step ahead
names(mp)
mp$pred
mp$se



lcl = m
(ucl <- cs[(length(cs)-3):length(cs)]+1.96*mp$se)


# Plot these or

m <- Arima(Index,order=c(12,0,0),include.mean=F)
fc <- forecast(m,h=30)
autoplot(fc) + 
	ggtitle("UofM Transformed Consumer Sentiment") +
	geom_vline(xintercept = nrow(X), col="red") +
	xlab("Month by Year Index") + ylab("Sentiment")

c1 <- c(0,NA,0,0,NA,0,0,0,0,0,0,0)
m <- Arima(Index,order=c(12,1,0),include.mean=F,fixed=c1)
(m4 <- m)
f <- forecast(m,h=30)
fc - f
autoplot(f) + 
	ggtitle("UofM Consumer Sentiment") +
	geom_vline(xintercept = nrow(X), col="red") +
	xlab("Month by Year Index") + ylab("Sentiment")


################################################
