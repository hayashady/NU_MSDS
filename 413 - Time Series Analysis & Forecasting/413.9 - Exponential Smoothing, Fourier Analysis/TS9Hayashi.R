
# Assignment 9 #

# Section 1 #
require(fBasics) # basicStats function
require(fpp2) # holt-winters model
require(stats) # stl decomp function
require(TSA) # mcleod li test
require(ggplot2)
require(ggpubr) # ggarrange

# Section 2 #
library("tidyverse")
library("lubridate")
library("prophet") # prophet model
library("forecast")
library("fBasics")
library("ggplot2")
source("ProphetOpt.R")

options(scipen=999)


####### Section 1 - Exponential Smoothing #######

# Read data
X <- read.csv("/Users/dylanhayashi/Desktop/Northwestern/413 - Time-Series Analysis & Forecasting/TS9/AustralianWines.csv",header=T)
y <- ts(X$Fortified[1:180],frequency=12)





# Section 1.1 - EDA 
plot(y);grid(col="darkgray") # Line plot
basicStats(y) # Summary statistics, including variance
autoplot(stl(y,"periodic")) # Decomposition
McLeod.Li.test(y=y) # McLeod-Li test
ggAcf(y)





# Section 1.2 - Holt-Winters multiplicative model 
# Holt-Winters' multiplicative method
fit_multi <- hw(y,seasonal="multiplicative")
plot(forecast(fit_multi));grid(col="darkgray")





# Section 1.3 - Holt-Winters multiplicative model with exponential trend
# Exponential trend
fit_multi_exp <- hw(y,seasonal="multiplicative",exponential=TRUE)
plot(forecast(fit_multi_exp));grid(col="darkgray")

# Damped
fit_multi_damped <- hw(y,seasonal="multiplicative",damped=TRUE)
plot(forecast(fit_multi_damped));grid(col="darkgray")

# Exponential and damped
fit_multi_exp_damped <- hw(y, seasonal="multiplicative", exponential=TRUE, damped=TRUE)
plot(forecast(fit_multi_exp_damped));grid(col="darkgray")





# Section 1.4 
# Compare the RMSE
accuracy(fit_multi)
accuracy(fit_multi_exp)
accuracy(fit_multi_damped)
accuracy(fit_multi_exp_damped)





# Section 1.5 - Additional model creation

# Create models 
fit_mam <- ets(y, model="MAM") # ETS model
fit_ana_box <- ets(y,additive.only=TRUE,lambda=TRUE) # Additive ETS model with Box-Cox transformation
fit_stld <- stlf(y,method="ets",lambda=TRUE) # STL decomposition with Box-Cox transformation on ETS
fit_naive <- snaive(y,lambda=TRUE) # Seasonal naive with Box-Cox transformation

# Plot forecasts
par(mfrow = c(5,1))
plot(fit_multi);grid(col="darkgray")
plot(forecast(fit_mam));grid(col="darkgray")
plot(forecast(fit_ana_box));grid(col="darkgray")
plot(forecast(fit_naive));grid(col="darkgray")
plot(forecast(fit_stld));grid(col="darkgray")

# Histograms of residuals
par(mfrow = c(5,3))
hist(residuals(fit_multi),nclass=20)
hist(residuals(fit_mam),nclass=20)
hist(residuals(fit_ana_box),nclass=20)
hist(residuals(fit_naive),nclass=20)
hist(residuals(fit_stld),nclass=20)

# QQ Plots of residuals
par(mfrow = c(5,1))
qqnorm(residuals(fit_multi),) + qqline(residuals(fit_multi))
qqnorm(residuals(fit_mam)) + qqline(residuals(fit_mam))
qqnorm(residuals(fit_ana_box)) + qqline(residuals(fit_ana_box))
qqnorm(residuals(fit_naive)) + qqline(residuals(fit_naive))
qqnorm(residuals(fit_stld)) + qqline(residuals(fit_stld))

# Plots or residuals
par(mfrow = c(5,1))
plot(residuals(fit_multi));grid(col="darkgray")
plot(residuals(fit_mam));grid(col="darkgray")
plot(residuals(fit_ana_box));grid(col="darkgray")
plot(residuals(fit_naive));grid(col="darkgray")
plot(residuals(fit_stld));grid(col="darkgray")

## QQ plots
acf_one = ggAcf(residuals(fit_multi))
acf_two = ggAcf(residuals(fit_mam))
acf_three = ggAcf(residuals(fit_ana_box))
acf_four = ggAcf(residuals(fit_naive))
acf_five = ggAcf(residuals(fit_stld))
ggarrange(acf_one,acf_two,acf_three,acf_four,acf_five,ncol=1,nrow=5)

# Accuracy statistics
accuracy(fit_multi)
accuracy(fit_mam)
accuracy(fit_ana_box)
accuracy(fit_naive)
accuracy(fit_stld)





####### Section 2 - Prophet I ######


# Read data (2 years, 2011 - 2012)
bikes <- read_csv('/Users/dylanhayashi/Desktop/Northwestern/413 - Time-Series Analysis & Forecasting/TS9/bikes.csv')
X <- bikes
X$datetime <- date(X$datetime)
str(X)
H <- X   # holding matrix





######## Section 2.1 - EDA #######

# EDA: add yours here
ggplot() + geom_line(data = X, aes(x = datetime, y = count)) # Time plot of counts
daily_counts = data.frame(aggregate(X$count, list(X$datetime), sum))   # Calculate daily sums
ggplot() + geom_line(data=daily_counts,aes(x=Group.1,y=x)) + labs(x='Date',y='Daily Count') # Time plot of daily sums
basicStats(daily_counts$x) # Variance 
daily_counts_ts = ts(daily_counts$x, frequency = findfrequency(daily_counts$x)) # Create daily counts time series for decomposition
autoplot(stl(daily_counts_ts,'periodic')) # Decomposition
ggAcf(daily_counts$x) # ACF Plot

# Create training set, 2011
A <- X[X$datetime < ymd("2012-01-01"), c(1,12)]    # X$datetime,X$count
Tr <- aggregate(A$count, list(A$datetime), sum)   # sum training set by day
names(Tr) <- c('ds', 'y')   # Prophet uses only these names
rm(A)
training_plot = ggplot() + geom_line(data = Tr, aes(x = ds, y = y)) + ggtitle('Training Set (2011)')

# Create validation set, 1.12 - 6.12
A <- X[X$datetime >= ymd("2012-01-01") & X$datetime <= ymd("2012-06-30"), c(1,12)] 
V <- aggregate(A$count, list(A$datetime), sum)   # training set
names(V) <- c('ds', 'y')
rm(A)
val_plot = ggplot() + geom_line(data = V, aes(x = ds, y = y)) + ggtitle('Validation Set (1.2012 - 6.2012)')

# Create test set 7.12 - 12.12
A <- X[X$datetime >= ymd("2012-07-01"), c(1,12)]    # X$datetime,X$count
Te <- aggregate(A$count, list(A$datetime), sum)   # training set
names(Te) <- c('ds', 'y')   # Prophet uses only these names
str(Te)
rm(A)
test_plot = ggplot() + geom_line(data = Te, aes(x = ds, y = y)) + ggtitle('Test Set (7.2012 - 12.2012)')

# Display Plots
ggarrange(training_plot, val_plot, test_plot, ncol=1, nrow=3)




####### Section 2.2 - Holidays #######

# Set holidays
h <- unique(X[X$holiday==1, 1])
h$holiday = c('Martin Luther King', 'Emancipation Day', 'Independence Day',
              'Labor Day', 'Columbus Day', 'Veterans Day', 'New Year', 
              'Martin Luther King', 'Emancipation Day', 'Independence Day',
              'Labor Day', 'Columbus Day', 'Veterans Day')
names(h) <- c('ds', 'holiday')
str(h)




####### Section 2.3 - Model Fit #######

# Prophet default model on training set
m <- prophet(Tr, holidays = h, yearly.seasonality = TRUE)
m1 <- m
 
future <- make_future_dataframe(m, periods = 184)    # 6 months of daily
Fc <- predict(m, future)
F1 <- Fc
forecast::accuracy(Fc[ymd(Fc$ds) %in% V$ds, ]$yhat, V$y)[ , c("RMSE","MAPE")]

# Default model plots
plot(m, Fc)
prophet_plot_components(m, Fc)

gp <- ggplot() + 
    geom_point(data = Tr, aes(x = as.POSIXct(ds), y = y), size = 0.7) +
    geom_point(data = V, aes(x = as.POSIXct(ds), y = y), size = 0.7, color = 'green4') +
    geom_point(data = Te, aes(x = as.POSIXct(ds), y = y), size = 0.7, color = 'red') +
    geom_line(data = Fc, aes(x = ds, y = yhat), color = "blue4") +
    geom_ribbon(data = Fc, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "blue", alpha = 0.2) +
    labs(subtitle = "Default Prophet Model", x = "Date")
gp








####### Section 3 - Prophet II #######


####### Section 3.1 - Parameter Matrix #######
# Look for optimum values: all possible combinations, can be sequences
A <- expand.grid(
				 cps = c(0.05, 0.25, 0.5), # changepoint prior scale, default 0.8
                 sps = c(1, 10),    # seasonality prior scale, default 10, try lower
                 hps = c(1, 10),    # holidays prior scale, default 10, try lower
                 capacity = c(7000, 8000), # max training ~6000, try higher as in Te
                 growth = "logistic"     # rather than linear
                 )
A$growth <- as.character(A$growth)

# Resample for best parameters
source('/Users/dylanhayashi/Desktop/Northwestern/413 - Time-Series Analysis & Forecasting/TS9/ProphetOpt.R')
fit.stats <- vector(mode = 'numeric', length = nrow(A))
fit.stats <- apply(A, 1, m.opt, X.train=Tr, X.validation=V, holidays=h)
fit.stats <- t(fit.stats)

A <- cbind(A, fit.stats)
(MAPE.min <- A[A$MAPE == min(fit.stats), ])



####### Section 3.3 - Retraining with validation set ########
# Retrain using training and validation set
    reTr <- bind_rows(Tr, V)
reTr$cap <- MAPE.min$capacity

m <- prophet(reTr, growth=MAPE.min$growth, holidays = h,
			 seasonality.prior.scale = MAPE.min$sps,
			 changepoint.prior.scale = MAPE.min$cps,
             holidays.prior.scale = MAPE.min$hps,
             yearly.seasonality = TRUE)
m2 <- m

    future <- make_future_dataframe(m, periods = 184)  # 6 months
future$cap <- MAPE.min$capacity



####### Section 3.4 - Forecasts #######

Fc <- predict(m, future)
forecast::accuracy(Fc[ymd(Fc$ds) %in% Te$ds, ]$yhat, Te$y)[ , 'MAPE']

plot(m, forecast)
prophet_plot_components(m, Fc)

# Optimized model plot
gp <- ggplot() + 
    geom_point(data=Tr,  aes(x=as.POSIXct(ds), y=y), size=0.7) +
    geom_point(data=V,   aes(x=as.POSIXct(ds), y=y), size=0.7, color="green4") +
    geom_point(data=Te,  aes(x=as.POSIXct(ds), y=y), size=0.7, color="red") +
    geom_line( data=Fc,  aes(x=ds, y = yhat), color="blue4") +
    geom_ribbon(data=Fc, aes(x=ds, ymin=yhat_lower, ymax=yhat_upper), fill="blue", alpha=0.2) +
    labs(subtitle="Optimized Prophet Model", x="Date")
gp




######## Section 2.5 - Model Fit and Comparison #######
Xv <- cross_validation(m2, horizon=30, units="days")
P <- performance_metrics(Xv)
P
plot_cross_validation_metric(Xv, metric="rmse")

########################################
