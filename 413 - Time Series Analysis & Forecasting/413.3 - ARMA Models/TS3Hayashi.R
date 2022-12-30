# Dylan Hayashi
# MSDS 413 - Time Series Analysis & Forecasting

require(TSA)
require(ggplot2)
require(fpp2)
require(DescTools)
require(urca)
require(fUnitRoots)
require(forecast)
require(NTS)
require(stats)
options(scipen = 999)


####### Load COVID-19 Data #######
url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
fn <- "Covid19.csv"
download.file(url, fn)
Covid19 <- read.csv("Covid19.csv",header=T)
names(Covid19);nrow(Covid19)
X <- Covid19[Covid19$iso_code=="USA",]
nrow(X);head(X$date);tail(X$date)
X <- na.omit(X[,c("date","total_cases")])
X$date <- as.Date(X$date)
rm(url,fn)
####### Load COVID-19 Data #######



####### Section 1 - EDA #######



####### ORIGINAL DATA #######
# Line Plot
ggplot()+geom_line(data=X,aes(x=date,y=total_cases),color='Blue')+ggtitle('Time Plot of US COVID-19 Cases')+labs(y='Total Cases',x='Date')
# Time Series Test
# Condition 1
length(unique(X$date))
length(X$date)
length(X$total_cases)
# Condition 2
dif <- diff(as.Date(X$date,"%Y-%m-%d"))
nrow(X)
table(dif)   # nrow - 1?
# Summary table
summary(X$total_cases)
basicStats(X$total_cases)
# T test
t.test()
# Auto-correlation
Box.test(X$total_cases,lag=12,type='Ljung')    # H0: independent vs. Ha: not independent
# Normality
ggplot(X,aes(x=total_cases))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of US COVID-19 Cases")+labs(x='Index',y='Density')
ggplot(X,aes(sample=total_cases))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of US COVID-19 Cases")+labs(x='Theoretical',y='Sample')
Skew(X$total_cases, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)
Kurt(X$total_cases, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)  # gives excess kurtosis
# STL Decomposition
ts_x = ts(X$total_cases,frequency=25)
decomp <- stl(ts_x,"periodic")
autoplot(decomp)
# Stationarity
t.test(ts_x) # T Test for mean 0
adfTest(ts_x,lags=5,type="nc") # ADF - Random Walk Stationarity
kpss <- ur.kpss(X_TS,type="tau",lags="short") # KPSS - Linear Trend Stationarity
summary(kpss)    
####### ORIGINAL DATA #######



####### SECOND DIFFERENCED DATA #######
diff_data = ts(diff(X$total_cases,differences=2))
# ggplot() + geom_line(data=diff_data,aes(x=X$date[3:991],y=diff_data),color='blue') + ggtitle('Time Plot of Second Differenced US COVID-19 Cases') + labs(x='year',y='cases')
t.test(diff_data)
# basicStats(diff_data)
adfTest(diff_data,lags=5,type="nc") 
kpss <- ur.kpss(diff_data,type="tau",lags="short")
summary(kpss)    # If test stat <= 5pct, fail to reject
Box.test(diff_data,lag=12,type='Ljung')    # H0: independent vs. Ha: not independent
ggplot(diff_data,aes(x=diff_data))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of Twice-Differenced US COVID-19 Cases")+labs(x='Cases',y='Density')
ggplot(diff_data,aes(sample=diff_data))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of Twice-Differenced US COVID-19 Cases")+labs(x='Theoretical',y='Sample')
Skew(diff_data, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)
Kurt(diff_data, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)  # gives excess kurtosis
####### SECOND DIFFERENCED DATA #######



# Transformation - sqrt transform and second differencing
y <- ts(diff(sqrt(X[,2]),differences=2))
ggplot()+geom_line(data=y,aes(x=X$date[3:991],y=y),color='Blue')+ggtitle('Time Plot of US COVID-19 Cases')+labs(y='Total Cases',x='Date')




################################################
# Problem 2


####### Section 2.1 - ACF Plot and Model Order ####### 
ggAcf(y)


####### Section 2.2 - Auto ARIMA and Model Order ####### 
auto.arima(y,d = 0,max.p=0,stationary=TRUE)


####### Section 2.3 - Model Fitting ######
# MA model from ACF
m <- Arima(y,order=c(0,0,9),include.mean=F)  # assumes trend-stationary
(m21 <- m)
# Parameter test 
parameterTest(m)
# New model
c1 <- c(NA,NA,NA,0,0,NA,NA,NA)
m <- arima(y,order=c(0,0,8),include.mean=F,fixed=c1)
(m21 <- m)

residuals = resid(m21)
checkresiduals(m,lag=20)
basicStats(resid(m))
t.test(resid((m)))
Box.test(resid(m),lag=12,type='Ljung')    # H0: independent vs. Ha: not independent
Skew(resid(m), method = 3, conf.level = 0.05, ci.type = "bca", R = 1000)
Kurt(resid(m), method = 3, conf.level = 0.05, ci.type = "bca", R = 1000)
McLeod.Li.test(m)    # any lags <= 0.05?
adfTest(resid(m),lags=5,type="nc")
kpss <- ur.kpss(resid(m),type="tau",lags="short")
summary(kpss)

# Business Cycles
(p2 <- c(1,-m$coef))
(s2 <- polyroot(p2))
(z = unique(round(sapply(all_complex(s2),period),digits=3)))   # lengths of business cycles
# Backtest 
m <- m21
backtest(m,y,500,7)  # one week

# Section 2.3 - Point Ahead Forecasts
pred = predict(m,7)
ucl = pred$pred + (1.96 * pred$se)
lcl = pred$pred - (1.96 * pred$se)

# Section 2.4 - Forecasts
m <- Arima(X$total_cases,order=c(0,2,8),include.mean=T,lambda="auto",fixed=c1)  # use transformation
(m24 <- m)
f <- forecast(m24,h=(7*8))  # approx 8-week forecast
f21 <- f
tail(f$fitted)
accuracy(f)
autoplot(f)






################################################
# Problem 3

y <- ts(diff(sqrt(X[,2]+1),differences=2))


ggAcf(y)
ggPacf(y)
auto.arima(y,d = 0,stationary=TRUE)

# RMA model from ACF and PACF
m31 <- Arima(y,order=c(2,0,2),include.mean=F)
(m <- m31)

# t test for coefficient significance
# source("parameterTest.R")
parameterTest(m)


# Set Residuals
residuals = resid(m31)
# Residuals panel
checkresiduals(m,lag=30)
# Summary Statistics of Residuals
basicStats(resid(m))
# T Test for residuals mean = 0
t.test(resid((m)))
# Box-Ljung for Auto-Covariance
Box.test(resid(m),lag=12,type='Ljung')    # H0: independent vs. Ha: not independent
# Skew of residuals
Skew(resid(m), method = 3, conf.level = 0.05, ci.type = "bca", R = 1000)
# Kurtosis of residuals 
Kurt(resid(m), method = 3, conf.level = 0.05, ci.type = "bca", R = 1000)
# Stationarity
# ADF Test
adfTest(resid(m),lags=5,type="nc")
# KPSS Test
kpss <- ur.kpss(resid(m),type="tau",lags="short")
summary(kpss)
# McLeod Li
McLeod.Li.test(m)    # any lags <= 0.05?
# Business Cycles
(p2 <- c(1,-m$coef))
(s2 <- polyroot(p2))
(z = unique(round(sapply(all_complex(s2),period),digits=3)))   # lengths of business cycles
# Backtest 
backtest(m,y,500,7)  # one week



# Section 3.3 - Point Ahead Forecasts
prediction = predict(m,7)
lcl = prediction$pred - (1.96*prediction$se)
ucl = prediction$pred + (1.96*prediction$se)

# Section 2.4 - Forecasts
m <- Arima(X$total_cases,order=c(2,2,2),include.mean=T,lambda="auto")  # use transformation
f <- forecast(m,h=(7*8))  # approx 8-week forecast
f21 <- f
tail(f$fitted)
accuracy(f)
autoplot(f)


################################################

