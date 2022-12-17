# Dylan Hayashi
# TS8

require(fBasics)
require(fpp2)
require(car)
require(curl)
require(devtools)
require("quantmod") # devtools::install_github("joshuaulrich/quantmod", ref="157_yahoo_502")
require(FinTS) # install.packages("FinTS", repos="http://R-Forge.R-project.org")
source("/Users/dylanhayashi/Desktop/Northwestern/413 - Time-Series Analysis & Forecasting/TS8/covEWMA.R")
require(fUnitRoots) # ADF Test
require(urca) # KPSS Test
require(DescTools) # SKew and Kurtosis
require(PerformanceAnalytics) # Calculate retuns function
require("ggpmisc") # uncertain (stat poly eq?)
require(MVN) # MVN Test
require(tseries) # arch tests
require(MTS) # ccm function
require(ggpubr)
require(fGarch) # fgrarch

options(digits=4)
options(scipen=10)


# Correlation
cor.fun <- function(x){
  cor(x)[1,2]
}

# Covariance
cov.fun <- function(x){
  cov(x)[1,2]
}

######## Question 1 #######
# Get stock price data
symbol.vec <- c("MSFT", "^GSPC")
getSymbols(symbol.vec, from ="2000-01-03", to = "2021-10-31")
MSFT <- MSFT[, "MSFT.Adjusted", drop=F] # Extract adjusted closing prices
GSPC <- GSPC[, "GSPC.Adjusted", drop=F] # Extract adjusted closing prices



####### Section 1.1 - EDA #######
# Time plots of price 
plot(MSFT)
plot(GSPC)
# MVN of price
mvn_df = data.frame(MSFT,GSPC)
mvn(mvn_df, mvnTest = 'mardia')
# Plot log price
plot(log(MSFT$MSFT.Adjusted))
plot(log(GSPC$GSPC.Adjusted))


# Calculate  returns
MSFT.ret <- CalculateReturns(MSFT, method="log")
GSPC.ret <- CalculateReturns(GSPC, method="log")
# Plot  of returns
plot(MSFT.ret)
plot(GSPC.ret)
# remove first NA observation
MSFT.ret <- MSFT.ret[-1,]
GSPC.ret <- GSPC.ret[-1,]
# create combined data series
MSFT.GSPC.ret <- merge(MSFT.ret,GSPC.ret)
# Normality
mvn(MSFT.GSPC.ret[,c(1,2)], mvnTest='mardia')
# Create new dataframe for EDA
returns_data = MSFT.GSPC.ret
colnames(returns_data)[1] = 'MSFT'
colnames(returns_data)[2] = 'GSPC'
# Iterate EDA over columns
for (i in c(1,2))
{
  # print(t.test(returns_data[,i])) # t.test for mean zero
  # print(adfTest(ts(returns_data[,i]),lags=5,type="nc")) # ADF Test
  # print(ur.kpss(ts(returns_data[,i]),type="tau",lags="short")) # KPSS Test
  # print(McLeod.Li.test(y=returns_data[,i])) # McLeod-Li test for constant variance
}
# March test
MarchTest(MSFT.GSPC.ret[,c(1,2)], lag=10)
# CCM - Auto and cross correlations
ccm(MSFT.GSPC.ret[,c(1,2)],5)
mq(gdp[,c(2,3,4)],10)
# Scatter plot
reg <- y ~ -1 + x
X <- data.frame(MSFT.GSPC.ret)
ggplot(data = X, aes(x = MSFT, y = GSPC)) +
  geom_smooth(method = "lm", se=FALSE, color="red", formula = reg) +
  stat_poly_eq(formula = reg,eq.with.lhs = "italic(hat(y))~`=`~",aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")), parse = TRUE) +         
  geom_point()


# EDA Decomp
returns_data = data.frame(log(MSFT$MSFT.Adjusted),log(GSPC$GSPC.Adjusted))
colnames(returns_data) = c('MSFT','GSPC')
msft_ts = ts(returns_data$MSFT,frequency=252)
decomp = stl(msft_ts,"periodic")
autoplot(decomp)


####### Section 1.2 #######
# Arch tests
ArchTest(MSFT.ret, lags=5)
ArchTest(GSPC.ret, lags=5)
# Create ARCH models
m12a <- tseries::garch(MSFT.ret, order=c(0,5), trace = F)
m12b <- tseries::garch(GSPC.ret, order=c(0,5), trace = F)
# Run ARCH model summaries
summary(m12a)
summary(m12b)
# Residuals summary
plot(m12a)
plot(m12b)
# Stationarity - ADF and KPSS tests
adfTest(ts(resid(m12a)),lags=5,type="nc") 
ur.kpss(ts(resid(m12a)),type="tau",lags="short")
adfTest(ts(resid(m12b)),lags=5,type="nc") 
ur.kpss(ts(resid(m12b)),type="tau",lags="short")



####### Section 1.3 #######
# Create GARCH models
m13a <- fGarch::garchFit(MSFT.ret ~ garch(1, 1), data = MSFT.ret, trace = F)
m13b <- fGarch::garchFit(GSPC.ret ~ garch(1, 1), data = GSPC.ret, trace = F)
# Run GARCH model summaries
summary(m13a)
summary(m13b)
sum(coef(m13a))
sum(coef(m13b))
# Residuals summary
plot(m13a)
resi = data.frame(m13b@residuals)
ggplot(resi,aes(x=m13b.residuals))+geom_histogram(aes(y=..density..),color='Black',fill='Grey')+geom_density(alpha=0.6,color='Blue')+ggtitle("Histogram of GSPC residuals")+labs(x='Debt',y='Density') 
ggAcf((m13b@residuals))
# Stationarity - ADF and KPSS tests
adfTest(ts(m13a@residuals),lags=5,type="nc") 
ur.kpss(ts(m13b@residuals),type="tau",lags="short")
adfTest(ts(m13a@residuals),lags=5,type="nc") 
ur.kpss(ts(m13b@residuals),type="tau",lags="short")


####### Section 1.4 #######
# 20 day rolling cov/cor
chart.RollingCorrelation(MSFT.ret, GSPC.ret, width=20)
roll.cov <- rollapply(as.zoo(MSFT.GSPC.ret), FUN=cov.fun, width=20, by.column=FALSE, align="right")
roll.cor <- rollapply(as.zoo(MSFT.GSPC.ret), FUN=cor.fun, width=20, by.column=FALSE, align="right")
par(mfrow=c(2,1))
plot(roll.cov, main="20-day rolling covariances",ylab="covariance", lwd=2, col="blue") + grid(col="gray") + abline(h=cov(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
plot(roll.cor, main="20-day rolling correlations",ylab="correlation", lwd=2, col="blue") + grid(col="gray") + abline(h=cor(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))
# calculate EWMA covariances and correlations
lambda <- 0.94
cov.ewma <- covEWMA(as.data.frame(MSFT.GSPC.ret), lambda=lambda)
MSFT.GSPC.cond.cov <- cov.ewma[,2,1] # conditional variance
t <- length(cov.ewma[,1,1]) # conditional correlation
MSFT.GSPC.cond.cor<- rep(0,t) # conditional correlation
for (i in 1:t) {
  MSFT.GSPC.cond.cor[i] <- cov2cor(cov.ewma[i,,])[1,2]
}
# Plots
par(mfrow=c(2,1))
plot(x=time(as.zoo(MSFT.GSPC.ret)), y=MSFT.GSPC.cond.cov,type="l", xlab="Time", ylab="Covariance", lwd=2, col="blue", main="EWMA Covariance between MSFT and S&P500") + grid(col="gray") + abline(h=cov(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
plot(x=time(as.zoo(MSFT.GSPC.ret)), y=MSFT.GSPC.cond.cor,type="l", xlab="Time", ylab="Correlation", lwd=2, col="blue",main="EWMA Correlation between MSFT and S&P500") + grid(col="gray") + abline(h=cor(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))
# compute rolling covariances and correlations using longer window
roll.cov <- rollapply(as.zoo(MSFT.GSPC.ret), FUN=cov.fun, width=252, by.column=FALSE, align="right")
roll.cor <- rollapply(as.zoo(MSFT.GSPC.ret), FUN=cor.fun, width=252, by.column=FALSE, align="right")
par(mfrow=c(2,1))
plot(roll.cov, main="252-day rolling covariances",ylab="covariance", lwd=2, col="blue") + grid(col="gray") + abline(h=cov(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
plot(roll.cor, main="252-day rolling correlations",ylab="correlation", lwd=2, col="blue") + grid(col="gray") + abline(h=cor(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))
# compute EWMA covariances and correlations using longer half-life
half.life = 125 
lambda <- exp(log(0.5)/half.life)
cov.ewma <- covEWMA(as.data.frame(MSFT.GSPC.ret), lambda=lambda)
MSFT.GSPC.cond.cov <- cov.ewma[,2,1] # conditional variance
t <- length(cov.ewma[,1,1]) # conditional correlation 
MSFT.GSPC.cond.cor<- rep(0,t) # conditional correlation
for (i in 1:t) {
  MSFT.GSPC.cond.cor[i] <- cov2cor(cov.ewma[i,,])[1,2]
}
# Plots
par(mfrow=c(2,1))
plot(x=time(as.zoo(MSFT.GSPC.ret)), y=MSFT.GSPC.cond.cov,type="l", xlab="Time", ylab="Covariance", lwd=2, col="blue",main="EWMA Covariance between MSFT and S&P500") + grid(col="gray") + abline(h=cov(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
plot(x=time(as.zoo(MSFT.GSPC.ret)), y=MSFT.GSPC.cond.cor, type="l", xlab="Time", ylab="Correlation", lwd=2, col="blue",main="EWMA Correlation between MSFT and S&P500") + grid(col="gray") + abline(h=cor(MSFT.GSPC.ret)[1,2], lwd=2, col="red") + par(mfrow=c(1,1))
par(mfrow=c(1,1))



####### Section 1.5 #######

# univariate normal GARCH(1,1) for each series

garch11.spec <- ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "norm")
m1.51 <- ugarchfit(data = MSFT.GSPC.ret[,1], spec = garch11.spec)
m1.52 <- ugarchfit(data = MSFT.GSPC.ret[,2], spec = garch11.spec)

# DCC estimation
install.packages("rmgarch")
require(rmgarch)
require(rugarch)
# dcc specification - GARCH(1,1) for conditional correlations
dcc.garch11.spec <- dccspec(uspec = multispec( replicate(2, garch11.spec) ), dccOrder = c(1,1), distribution = "mvnorm")
dcc.garch11.spec



dcc.fit <- dccfit(dcc.garch11.spec, data = MSFT.GSPC.ret)
class(dcc.fit)
slotNames(dcc.fit)
names(dcc.fit@mfit)
names(dcc.fit@model)
print(dcc.fit)    # includes conditional correlation parameters ([Joint])
sum(coef(dcc.fit)[3:4])  # Mag
sum(coef(dcc.fit)[7:8])  # Counts
sum(coef(dcc.fit)[9:10]) # joint

# covariance and correlation series
cov.fit <- rcov(dcc.fit)[1,2,]
ts.plot(cov.fit);grid(col="gray")

cor.fit <- rcor(dcc.fit)[1,2,]
ts.plot(cor.fit);grid(col="gray")



####### Section 1.6 #######

# Use code cov.fit and cor.fit from above

par(mfrow=c(2,1))
plot(cov.fit, main="20-day rolling covariances",
     ylab="covariance", lwd=2, type="l", col="blue")
grid(col="gray")
abline(h=cov(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
plot(cor.fit, main="20-day rolling correlations",
     ylab="correlation", lwd=2, type="l", col="blue")
grid(col="gray")
abline(h=cor(MSFT.GSPC.ret)[1,2], lwd=2, col="red")
par(mfrow=c(1,1))


##########################################
# Part 1.7
# forecasting conditional volatility and correlations
#

dcc.fcst <- dccforecast(dcc.fit, n.ahead=100)
class(dcc.fcst)
slotNames(dcc.fcst)
class(dcc.fcst@mforecast)
names(dcc.fcst@mforecast)

# many method functions - see help on DCCforecast class
# rshape, rskew, fitted, sigma, plot, rcor, rcov, show

# show forecasts
cv <- rcov(dcc.fcst)[[1]][1,2,]
plot.ts(c(cov.fit[2800:3081],cv));grid(col="gray")

cr <- rcor(dcc.fcst)[[1]][1,2,]
plot.ts(c(cor.fit,cr));grid(col="gray")


##########################################







# Normality

msft_hist = ggplot(MSFT,aes(x=MSFT))+geom_histogram(aes(y=..density..),color='Black',fill='Grey') + geom_density(alpha=0.6,color='Blue') + ggtitle('Histogram of MSFT Price') + labs(x='Price',y='Density')
log_msft_hist = ggplot(MSFT,aes(x=log(MSFT)))+geom_histogram(aes(y=..density..),color='Black',fill='Grey') + geom_density(alpha=0.6,color='Blue') + ggtitle('Histogram of Log MSFT Price') + labs(x='Price',y='Density')
log_return_msft_hist = ggplot(MSFT.ret,aes(x=MSFT.ret))+geom_histogram(aes(y=..density..),color='Black',fill='Grey') + geom_density(alpha=0.6,color='Blue') + ggtitle('Histogram of Log MSFT Returns') + labs(x='Price',y='Density')

msft_qq = ggplot(MSFT,aes(sample=MSFT))+stat_qq(color='Blue')+stat_qq_line() + ggtitle('QQ Plot of MSFT Price') + labs(x='Theoretical',y='Sample')
log_msft_qq = ggplot(MSFT,aes(sample=log(MSFT)))+stat_qq(color='Blue')+stat_qq_line() + ggtitle('QQ Plot of Log MSFT Price') + labs(x='Theoretical',y='Sample')
log_return_msft_qq = ggplot(MSFT.ret,aes(sample=MSFT.ret))+stat_qq(color='Blue')+stat_qq_line() + ggtitle('QQ Plot of Log MSFT Returns') + labs(x='Theoretical',y='Sample')

sp_hist = ggplot(GSPC,aes(x=GSPC))+geom_histogram(aes(y=..density..),color='Black',fill='Grey') + geom_density(alpha=0.6,color='Blue') + ggtitle('Histogram of GSPC Price')+ labs(x='Price',y='Density')
log_sp_hist = ggplot(GSPC,aes(x=log(GSPC)))+geom_histogram(aes(y=..density..),color='Black',fill='Grey') + geom_density(alpha=0.6,color='Blue') + ggtitle('Histogram of Log GSPC Price')+ labs(x='Price',y='Density')
log_return_sp_hist = ggplot(GSPC.ret,aes(x=GSPC.ret))+geom_histogram(aes(y=..density..),color='Black',fill='Grey') + geom_density(alpha=0.6,color='Blue') + ggtitle('Histogram of Log GSPC Returns')+ labs(x='Price',y='Density')

sp_qq = ggplot(GSPC,aes(sample=GSPC))+stat_qq(color='Blue')+stat_qq_line() + ggtitle('QQ Plot of GSPC Price') + labs(x='Theoretical',y='Sample')
log_sp_qq = ggplot(GSPC,aes(sample=log(GSPC)))+stat_qq(color='Blue')+stat_qq_line() + ggtitle('QQ Plot of Log GSPC Price') + labs(x='Theoretical',y='Sample')
log_return_sp_qq = ggplot(MSFT.ret,aes(sample=MSFT.ret))+stat_qq(color='Blue')+stat_qq_line() + ggtitle('QQ Plot of Log MSFT Returns') + labs(x='Theoretical',y='Sample')


ggarrange(msft_hist,log_msft_hist,log_return_msft_hist,msft_qq,log_msft_qq,log_return_msft_qq,sp_hist,log_sp_hist,log_return_sp_hist,sp_qq,log_sp_qq,log_return_sp_qq,nrow=4,ncol=3)

