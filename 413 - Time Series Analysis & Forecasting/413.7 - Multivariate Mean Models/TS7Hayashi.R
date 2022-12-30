
#################################################
#   Assignment 7

require(ggplot2)
require(MVN)
require(fpp2)
require(MTS) # ccm function
require(vars)
require(fBasics)
require(TSA)
require(fUnitRoots) # ADF Test
require(urca) # KPSS Test
require(DescTools) # SKew and Kurtosis
source('/Users/dylanhayashi/Desktop/Northwestern/413 - Time-Series Analysis & Forecasting/TS2/BusCycle.R')
options(scipen=999)
options(digits=5)




#################################################
#   Problem 1

# Section 1.1 - EDA

data <- read.table("/Users/dylanhayashi/Desktop/Northwestern/413 - Time-Series Analysis & Forecasting/TS7/q-fdebt.txt",header=T)
data$quarter = c(1:171) # Create quarter index


# Untransformed Data
# Scatter plot
reg = lm(data$hbfrbn ~ data$hbfin)
ggplot(data=data,aes(x=hbfin,y=hbfrbn)) + geom_smooth(method='lm',se=FALSE) + geom_point()

# Time Plots
ggplot() + geom_line(data=data,aes(x=quarter,y=hbfin)) + ggtitle('Time Plot of Debt Held by Foregin Investors') / ggplot() + geom_line(data=data,aes(x=quarter,y=hbfrbn)) + ggtitle('Time Plot of Debt Held by Federal Reserve Banks')
# Basic Stats
basicStats(data$hbfin) / basicStats(data$hbfrbn)
# Histogram
ggplot(data,aes(x=hbfin))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of Debt Held by Foreign Investors")+labs(x='Debt',y='Density') / ggplot(data,aes(x=hbfrbn))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of Debt Held by Federal Reserve Banks")+labs(x='Debt',y='Density')
# QQ Plots
ggplot(data,aes(sample=hbfin))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of Debt Held by Foreign Investors")+labs(x='Theoretical',y='Sample') / ggplot(data,aes(sample=hbfrbn))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of Debt Held by Federal Reserve Banks")+labs(x='Theoretical',y='Sample')
# Skew
Skew(data$hbfin, method = 3, conf.level = 0.95, ci.type = "norm", R = 1000) / Skew(data$hbfrbn, method = 3, conf.level = 0.95, ci.type = "norm", R = 1000)
# Kurtosis
Kurt(data$hbfin, method = 3, conf.level = 0.95, ci.type = "norm", R = 1000) / Kurt(data$hbfrbn, method = 3, conf.level = 0.95, ci.type = "norm", R = 1000)
# Test for multivariate normal
mvn(data[,c(3,4)], mvnTest="mardia")


# Log Transformed Data
# Apply Log Transformation
data$log_hbfin = log(data$hbfin) 
data$log_hbfrbn = log(data$hbfrbn)
# Basic Stats
basicStats(data$log_hbfin)
basicStats(data$log_hbfrbn)
# Time Plots
ggplot() + geom_line(data=data,aes(x=quarter,y=log_hbfin)) + ggtitle('Time Plot of Log Debt Held by Foregin Investors') / ggplot() + geom_line(data=data,aes(x=quarter,y=log_hbfrbn)) + ggtitle('Time Plot of Log Debt Held by Federal Reserve Banks')
# Histograms
ggplot(data,aes(x=log_hbfin))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of Log Debt Held by Foreign Investors")+labs(x='Debt',y='Density') / ggplot(data,aes(x=log_hbfrbn))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of Log Debt Held by Federal Reserve Banks")+labs(x='Debt',y='Density')
# QQ Plots
ggplot(data,aes(sample=log_hbfin))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of Log Debt Held by Foreign Investors")+labs(x='Theoretical',y='Sample') / ggplot(data,aes(sample=log_hbfrbn))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of Log Debt Held by Federal Reserve Banks")+labs(x='Theoretical',y='Sample')
# Time Plots
ggplot() + geom_line(data=data,aes(x=quarter,y=log_hbfin)) + ggtitle('Time Plot of Log Debt Held by Foregin Investors') /ggplot() + geom_line(data=data,aes(x=quarter,y=log_hbfrbn)) + ggtitle('Time Plot of Log Debt Held by Federal Reserve Banks')
# Skew
Skew(data$log_hbfin, method = 3, conf.level = 0.95, ci.type = "norm", R = 1000) / Skew(data$log_hbfrbn, method = 3, conf.level = 0.95, ci.type = "norm", R = 1000) # Skew 
# Kurtosis
Kurt(data$log_hbfin, method = 3, conf.level = 0.95, ci.type = "norm", R = 1000) / Kurt(data$log_hbfrbn, method = 3, conf.level = 0.95, ci.type = "norm", R = 1000)  # Excess Kurtosis
# Test for multivariate normal
mvn(data[,c(5,6)], mvnTest="mardia")


# Differenced Data
# Linear Trend - t-test
t.test(data$log_hbfin) / t.test(data$log_hbfrbn)
# Apply First Differencing
foreign_investor = diff(data$log_hbfin,differences=2)
federal_reserve = diff(data$log_hbfrbn,differences=2)
quarter = c(1:169)
differenced_log_data = data.frame(foreign_investor,federal_reserve,quarter)
# Basic stats
basicStats(foreign_investor) / basicStats(federal_reserve)
# Time Plots
ggplot() + geom_line(data=differenced_log_data,aes(x=quarter,y=foreign_investor)) + ggtitle('Time Plot of Differenced Log Debt Held by Foregin Investors') / ggplot() + geom_line(data=differenced_log_data,aes(x=quarter,y=federal_reserve)) + ggtitle('Time Plot of Differenced Log Debt Held by Federal Reserve Banks')

# t-tests
t.test(differenced_log_data$foreign_investor) / t.test(differenced_log_data$federal_reserve)
# ADF Tests
adfTest(ts(differenced_log_data$foreign_investor),lags=5,type="nc") / adfTest(ts(differenced_log_data$federal_reserve),lags=5,type="nc") 
# KPSS Tests
ur.kpss(ts(differenced_log_data$foreign_investor),type="tau",lags="short")  / ur.kpss(ts(differenced_log_data$federal_reserve),type="tau",lags="short") 

# Box Tests
Box.test(differenced_log_data$foreign_investor,lag=10,type='Ljung-Box')  / Box.test(differenced_log_data$federal_reserve,lag=10,type='Ljung-Box')
# ACF Plots
ggAcf(differenced_log_data$foreign_investor) / ggAcf(differenced_log_data$federal_reserve)
# McLeod-Li tests
McLeod.Li.test(y=differenced_log_data$foreign_investor) / McLeod.Li.test(y=differenced_log_data$federal_reserve)
# Histograms
ggplot(differenced_log_data,aes(x=foreign_investor))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Differenced Log Debt - Foreign Investors")+labs(x='Debt',y='Density') / ggplot(differenced_log_data,aes(x=federal_reserve))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Differenced Log Debt - Federal Reserve Banks")+labs(x='Debt',y='Density')
# QQ Plots
ggplot(differenced_log_data,aes(sample=foreign_investor))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("Differenced Log Debt - Foreign Investors")+labs(x='Theoretical',y='Sample') / ggplot(differenced_log_data,aes(sample=federal_reserve))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("Differenced Log Debt - Federal Reserve Banks")+labs(x='Theoretical',y='Sample')
# Skew
Skew(differenced_log_data$foreign_investor, method = 3, conf.level = 0.95, ci.type = "norm", R = 1000) / Skew(differenced_log_data$federal_reserve, method = 3, conf.level = 0.95, ci.type = "norm", R = 1000) # Skew 
# Kurtosis
Kurt(differenced_log_data$foreign_investor, method = 3, conf.level = 0.95, ci.type = "norm", R = 1000) / Kurt(differenced_log_data$federal_reserve, method = 3, conf.level = 0.95, ci.type = "norm", R = 1000)  # Excess Kurtosis
# Test for multivariate normal
mvn(differenced_log_data[,c(1,2)], mvnTest="mardia")
# Portmanteau test for multivariate time series models, Multivariate ARCH test
MarchTest(ts(z), lag=10)
# Decomp
foreign = ts(differenced_log_data$foreign_investor,frequency=4)
decomp <- stl(foreign,"periodic")
autoplot(decomp)
federal = ts(differenced_log_data$federal_reserve,frequency=4)
decomp = stl(federal,"periodic")
autoplot(decomp)


# Take appropriate transformations and differences. E.g.:
# Define this new data as z.
logdata <- log(data[,c(3,4)])
z <- apply(logdata,2,diff)
z <- data.frame(z)
colnames(z) <- c("hbfin", "hbfrbn")


# Section 1.2 - Cross Correlation Matrix
ccm(z,5)   # test for specific cross-correlations

# Section 1.3 - Correlation Hypothesis testing
mq(z,10)    # test for any cross-correlations




#################################################
#  Problem 2

data("mts-examples",package="MTS")
head(qgdp)

# Section 2.1 - EDA
qgdp$quarter = c(1:126)


# Raw Data
# Time Plots
ggplot() + geom_line(data=qgdp,aes(x=quarter,y=us)) + ggtitle('Time Plot of US GDP')
ggplot() + geom_line(data=qgdp,aes(x=quarter,y=uk)) + ggtitle('Time Plot of UK GDP')
ggplot() + geom_line(data=qgdp,aes(x=quarter,y=ca)) + ggtitle('Time Plot of CA GDP')
# basicStats
for (i in c(5,3,4))
  {
  print(basicStats(qgdp[,i]))
}
# Scatter plots
reg = lm(qgdp$uk ~ qgdp$ca)
ggplot(data=qgdp,aes(x=uk,y=ca)) + geom_smooth(method='lm',se=FALSE) + geom_point()

# Histogram & QQ Plot
for (i in c(5,3,4))
  {
  print(ggplot(qgdp,aes(x=qgdp[,i]))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue'))
  print(ggplot(qgdp,aes(sample=qgdp[,i]))+stat_qq(color='Blue')+stat_qq_line()+labs(x='Theoretical',y='Sample'))
}
# Skew and Excess Kurtosis
for (i in c(5,3,4))
{
  print(Skew(qgdp[,i], method = 3, conf.level = 0.95, ci.type = "norm", R = 1000))
  print(Kurt(qgdp[,i], method = 3, conf.level = 0.95, ci.type = "norm", R = 1000))
}
# Multivariate Normal Test
mvn(qgdp[,c(5,3,4)], mvnTest="mardia")


# Apply log transformation
qgdp$log_us = log(qgdp$us)
qgdp$log_uk = log(qgdp$uk)
qgdp$log_ca = log(qgdp$ca)
# Time plots
ggplot() + geom_line(data=qgdp,aes(x=quarter,y=log_us)) + ggtitle('Time Plot of Log US GDP')
ggplot() + geom_line(data=qgdp,aes(x=quarter,y=log_uk)) + ggtitle('Time Plot of Log UK GDP')
ggplot() + geom_line(data=qgdp,aes(x=quarter,y=log_ca)) + ggtitle('Time Plot of Log CA GDP')
# basic stats
for (i in c(7,8,9))
{
  print(basicStats(qgdp[,i]))
}
# Histogram & QQ Plot
for (i in c(7,8,9))
{
  print(ggplot(qgdp,aes(x=qgdp[,i]))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue'))
  print(ggplot(qgdp,aes(sample=qgdp[,i]))+stat_qq(color='Blue')+stat_qq_line()+labs(x='Theoretical',y='Sample'))
}
# Skew and Excess Kurtosis
for (i in c(7,8,9))
{
  print(Skew(qgdp[,i], method = 3, conf.level = 0.95, ci.type = "norm", R = 1000))
  print(Kurt(qgdp[,i], method = 3, conf.level = 0.95, ci.type = "norm", R = 1000))
}
# MVN
mvn(qgdp[,c(7,8,9)], mvnTest="mardia")
# T Test
for (i in c(7,8,9))
{
  print(t.test(qgdp[,i]))
}


# Apply Differencing
diff_quarter = c(1:124)
gdp = data.frame(diff_quarter)
gdp$log_us_diff = diff(qgdp$log_us,differences=2)
gdp$log_uk_diff = diff(qgdp$log_uk,differences=2)
gdp$log_ca_diff = diff(qgdp$log_ca,differences=2)

# Time plots
ggplot() + geom_line(data=gdp,aes(x=diff_quarter,y=log_us_diff)) + ggtitle('Time Plot of Differenced Log US GDP') / ggplot() + geom_line(data=gdp,aes(x=diff_quarter,y=log_uk_diff)) + ggtitle('Time Plot of Differenced Log UK GDP') / ggplot() + geom_line(data=gdp,aes(x=diff_quarter,y=log_ca_diff)) + ggtitle('Time Plot of Differenced Log CA GDP')

for (i in c(2,3,4))
{
  # print(basicStats(gdp[,i])) # Basic stats
  # print(t.test(gdp[,i])) # t-test
  # print(adfTest(ts(gdp[,i]),lags=5,type="nc")) # ADF Test
  # print(ur.kpss(ts(gdp[,i]),type="tau",lags="short")) # KPSS Test
  # print(Box.test(gdp[,i],lag=10,type='Ljung-Box')) # Ljung-Box Test
  # print(ggAcf(gdp[,i])) # ACF plot
  # print(ggplot(gdp,aes(x=gdp[,i]))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')) # Historgam
  # print(ggplot(gdp,aes(sample=gdp[,i]))+stat_qq(color='Blue')+stat_qq_line()+labs(x='Theoretical',y='Sample')) # QQ Plot
  # print(Skew(gdp[,i], method = 3, conf.level = 0.95, ci.type = "norm", R = 1000)) # Skew 
  # print(Kurt(gdp[,i], method = 3, conf.level = 0.95, ci.type = "norm", R = 1000)) # Kurtosis
  print(McLeod.Li.test(y=gdp[,i]))
}
MarchTest(gdp[,c(2,3,4)], lag=10)

# Cross Correlation Examination
ccm(gdp[,c(2,3,4)],5)
mq(gdp[,c(2,3,4)],10)
MarchTest(ts(z), lag=5)

# MVN
mvn(gdp[,c(2,3,4)], mvnTest="mardia")

# Decomp
for (i in c(2,3,4))
{
  timeseries = ts(gdp[,i],frequency=4)
  decomp = (stl(timeseries,"periodic"))
  print(autoplot(decomp))
}







# Secition 2.2 - VAR(4) Model

dat2 <- data.frame(qgdp$uk,qgdp$ca,qgdp$us)
colnames(dat2) <- c("uk", "ca", "us")
logdat2 <- log(dat2)
datgrowth <- apply(logdat2,2,diff)
growth <- 100*datgrowth

# Create Model
m1 <- MTS::VAR(growth,p=4)

# Create residuals dataframe
quarter_resi = c(1:121)
m1_residuals = data.frame(quarter_resi,m1$residuals[,3],m1$residuals[,1],m1$residuals[,2])

# Model diagnostics
for (i in c(2,3,4))
{
  # print(Skew(m1_residuals[,i], method = 3, conf.level = 0.95, ci.type = "norm", R = 1000)) # Skew
  # print(Kurt(m1_residuals[,i], method = 3, conf.level = 0.95, ci.type = "norm", R = 1000)) # Kurtosis
  # print(ggplot(m1_residuals,aes(x=m1_residuals[,i]))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')) # Histogram
  # print(ggplot(m1_residuals,aes(sample=m1_residuals[,i]))+stat_qq(color='Blue')+stat_qq_line()+labs(x='Theoretical',y='Sample')) # QQ Plot
  # print(ggAcf(m1_residuals[,i])) # ACF Plot
  # print(t.test(m1_residuals[,i])) # t.test
  # print(adfTest(ts(m1_residuals[,i]),lags=5,type="nc")) # ADF Tests
  # print(ur.kpss(ts(m1_residuals[,i]),type="tau",lags="short")) # KPSS Test
  # print(Box.test(m1_residuals[,i]),lag=10,type='Ljung-Box')  # Box Test
  # print(McLeod.Li.test(y=m1_residuals[,i])) McLeod-Li Test
}

# MVN on Residuals
mvn(m1_residuals[,c(2,3,4)], mvnTest="mardia")

# Cross-correlation
ccm(m1_residuals[,c(2,3,4)],5)
mq(m1_residuals[,c(2,3,4)],10)
MarchTest(m1_residuals[,c(2,3,4)], lag=10)

# Business Cycles
for (i in c(3,1,2))
{
  (p2 <- c(1,-m1$coef[,i]))
  (s2 <- polyroot(p2))
  print((z = unique(round(sapply(all_complex(s2),period),digits=3))))
}








# Section 2.3

# Create Model
m2 <- refVAR(m1, thres=1.96)


# Create residuals dataframe
quarter_resi = c(1:121)
m2_residuals = data.frame(quarter_resi,m2$residuals[,3],m2$residuals[,1],m2$residuals[,2])

# Model diagnostics
for (i in c(2,3,4))
{
  # print(Skew(m2_residuals[,i], method = 3, conf.level = 0.95, ci.type = "norm", R = 1000)) # Skew
  # print(Kurt(m2_residuals[,i], method = 3, conf.level = 0.95, ci.type = "norm", R = 1000)) # Kurtosis
  # print(ggplot(m2_residuals,aes(x=m2_residuals[,i]))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')) # Histogram
  # print(ggplot(m2_residuals,aes(sample=m2_residuals[,i]))+stat_qq(color='Blue')+stat_qq_line()+labs(x='Theoretical',y='Sample')) # QQ Plot
  # print(ggAcf(m2_residuals[,i])) # ACF Plot
  # print(t.test(m2_residuals[,i])) # t.test
  # print(adfTest(ts(m2_residuals[,i]),lags=5,type="nc")) # ADF Tests
  # print(ur.kpss(ts(m2_residuals[,i]),type="tau",lags="short")) # KPSS Test
  # print(Box.test(m2_residuals[,i]),lag=10,type='Ljung-Box')  # Box Test
  # print(McLeod.Li.test(y=m2_residuals[,i])) # McLeod-Li Test
}

# MVN on Residuals
mvn(m2_residuals[,c(2,3,4)], mvnTest="mardia")

# Cross-correlation
ccm(m2_residuals[,c(2,3,4)],5)
mq(m2_residuals[,c(2,3,4)],10)
MarchTest(m2_residuals[,c(2,3,4)], lag=10)

# Business Cycles
for (i in c(3,1,2))
{
  (p2 <- c(1,-m2$coef[,i]))
  (s2 <- polyroot(p2))
  print((z = unique(round(sapply(all_complex(s2),period),digits=3))))
}





# Section 2.4 - Create Forecasts

# A la Hyndman for multivariate forecasting

X <- ts(growth)
VARselect(X, lag.max=8, type="const")[["selection"]]   # gives optimal lag num

m <- vars::VAR(X, p=1, type="const")
mv1 <- m
	serial.test(m, lags.pt=10, type="PT.asymptotic")
	
m <- vars::VAR(X, p=2, type="const")
mv2 <- m
	serial.test(m, lags.pt=10, type="PT.asymptotic")

fm <- forecast(m)
# (loc <- "../Plots/7_module/VAR5forecast.png")
# png(loc)
	autoplot(fm) + xlab("Month") + ggtitle("Forecasts of US, UK, and CA GDP Growth Rates by VAR(4) Model")
	dev.off()


#################################################
