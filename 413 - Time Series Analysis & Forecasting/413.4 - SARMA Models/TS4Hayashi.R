# Dylan Hayashi
# MSDS 413 
# TS4 - Global Land and Ocean Temperature Anomalies

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
options(scipen = 999)

########## Load Temperature Data ##########
X <- read.csv("https://www.ncdc.noaa.gov/cag/global/time-series/globe/land_ocean/1/11/1880-2020/data.csv",header=T,skip=4)
str(X)
# convert years into decades
X <- X[X$Year<2020,]
X$decade <- rep(1:trunc(nrow(X)/10),each=10)
X <- X[,-1]
ggplot() + geom_point(data = X, aes(x = decade, y = Value)) + stat_smooth(aes(x = X$decade, y = X$Value), colour="red") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  ggtitle('Time Plot of Temparture Deviation Grouped by Decade') +  labs(x='Decade',y='Temperature (Degrees Celsius)')
############################################


############################################
# Section 1 - EDA

# Line Plot
ggplot()+geom_line(data=X,aes(x=Year,y=Value),color='Blue')+ggtitle('Time Plot of Temperature Deviation')+labs(y='Temperature (Degrees Celsius)',x='Year')

# Summary Statistics
basicStats(X$Value)

# Proof of Time Series
# Condition 1
length(unique(X$Year))
length(X$Year)
length(X$Value)
# Condition 2
dif <- diff(as.Year(X$Year,"%Y"))
nrow(X)
table(dif)   # nrow - 1?

# Proof of Linear Trend
X$Year_based = X$Year - 1879
linear_trend_regression = lm(formula = X$Value ~ X$Year)
linear_trend_regression
t.test($)
t.test(X$Value)

# Apply first differencing
Y = data.frame(X$Year[2:140])
Y$Value = diff(X$Value)
Y$Year = Y$X.Year.2.140.
ggplot()+geom_line(data=Y,aes(x=Year,y=Value),color='Blue')+ggtitle('Time Plot of First Differenced Temperature Deviation')+labs(y='Temperature (Degrees Celsius)',x='Year')

# Summary Table
basicStats(Y$Value)

# Check mean 0
t.test(Y$Value)

# Check stationarity
Y_ts = ts(Y$Value)
adfTest(Y_ts,lags=5,type="nc") # ADF - Random Walk Stationarity
kpss <- ur.kpss(Y_ts,type="tau",lags="short") # KPSS - Linear Trend Stationarity
summary(kpss)    

# Check Normality
ggplot(Y,aes(x=Value))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of Differenced Temperature Deviation")+labs(x='Index',y='Density')
ggplot(Y,aes(sample=Value))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of Differenced Temperature Deviation")+labs(x='Theoretical',y='Sample')
Skew(Y$Value, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000) # Skew 
Kurt(Y$Value, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)  # Excess Kurtosis
shapiro.test(Y$Value) # Shapiro Test
lillie.test(Y$Value) # Lillie Test
ad.test(Y$Value) # AD Test

# Auto-Correlation
Box.test(Y$Value,lag=12,type='Ljung')    # H0: independent vs. Ha: not independent

# Decomposition
Y_ts_period = ts(Y$Value,frequency=250)
decomp <- stl(Y_ts_period,"periodic")
autoplot(decomp)

# Extended Auto-Correlation Function
eacf(diff(X[,1]))
############################################


############################################
# Problem 2


# Section 2.1 - Trend Removal
linear_trend_regression_two = lm(Y$Value ~ Y$Year)
linear_trend_regression_two
t.test(linear_trend_regression_two$coefficients)


y <- ts(X[,1], start=X[1,2], frequency=10)  # cycles per decade


# Section 2.2 - Create ARMA(p,q) Model
ggAcf(diff(y), lag.max=30) # ACF - Determine 
ggPacf(diff(y), lag.max=30)
m <- Arima(y,order=c(3,1,1),include.mean=T)
(m21 <- m)
# Check residuals
checkresiduals(m,lag=20)
basicStats(resid(m))
# Auto-Correlation
Box.test(resid(m),lag=12,type='Ljung')    # H0: independent vs. Ha: not independent
# Normality
shapiro.test(resid(m)) # Shapiro Test
lillie.test(resid(m)) # Lillie Test
ad.test(resid(m)) # AD Test
# Stationarity
t.test(resid(m))
McLeod.Li.test(m)   # any lags <= 0.05?
adfTest(resid(m),lags=5,type="nc")
kpss <- ur.kpss(resid(m),type="tau",lags="short")
summary(kpss)
# Business Cycles
(p2 <- c(1,-m$coef))
(s2 <- polyroot(p2))
(z = unique(round(sapply(all_complex(s2),period),digits=3)))   # lengths of business cycles
# Backtest
m <- m21
backtest(m,y,length(y)-4,4)





# Section 2.3 - Create SARMA(0,0,0)(1,1,1)
y <- ts(X[,1], start=X[1,2], frequency=10)  # cycles per decade

m <- Arima(y, order=c(0,0,0), seasonal=list(order=c(1,1,1)), include.mean=F)
parameterTest(m)
(m22 <- m)
# Check residuals
checkresiduals(m,lag=20)
basicStats(resid(m))
# Auto-Correlation
Box.test(resid(m),lag=12,type='Ljung')    # H0: independent vs. Ha: not independent
# Normality
Skew(resid(m), method = 3, conf.level = 0.05, ci.type = "bca", R = 1000)
Kurt(resid(m), method = 3, conf.level = 0.05, ci.type = "bca", R = 1000)
shapiro.test(resid(m)) # Shapiro Test
lillie.test(resid(m)) # Lillie Test
ad.test(resid(m)) # AD Test
# Stationarity
t.test(resid(m))
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
backtest(m,y,length(y)-4,4)





############################################
# Problem 3

y <- ts(X[,1], start=X[1,2], frequency=10)  # cycles per decade
m <- Arima(y, order=c(0,1,1), seasonal=list(order=c(0,1,1)), include.mean=F)
parameterTest(m)
# Check residuals
checkresiduals(m,lag=20)
basicStats(resid(m))
# Auto-Correlation
Box.test(resid(m),lag=12,type='Ljung')    # H0: independent vs. Ha: not independent
# Normality
Skew(resid(m), method = 3, conf.level = 0.05, ci.type = "bca", R = 1000)
Kurt(resid(m), method = 3, conf.level = 0.05, ci.type = "bca", R = 1000)
shapiro.test(resid(m)) # Shapiro Test
lillie.test(resid(m)) # Lillie Test
ad.test(resid(m)) # AD Test
# Stationarity
t.test(resid(m))
adfTest(resid(m),lags=5,type="nc")
kpss <- ur.kpss(resid(m),type="tau",lags="short")
summary(kpss)
# Business Cycles
(p2 <- c(1,-m$coef))
(s2 <- polyroot(p2))
(z = unique(round(sapply(all_complex(s2),period),digits=3)))   # lengths of business cycles
# Backtest
backtest(m,y,length(y)-4,4)






# Forecasts

# choose model
f <- forecast(m,h=30)  # approx 4 decade forecast
f21 <- f
accuracy(f)
# choose your forecast model
autoplot(f) +
	ggtitle("Climate Change vs. Decade") +
	geom_vline(xintercept = X$decade[length(resid(m))]+1, col="red") +
	xlab("Decade") + ylab("Value")


############################################
