###########################################
#########   Assignment 10   ###############
###########################################

source("/Users/dylanhayashi/Desktop/Northwestern/413 - Time-Series Analysis & Forecasting/TS10/RFfunctions.R")
require(forecast) # forecast
require(randomForest) # random forest model
require(fBasics) # basicStats function
require (ggplot2) # pplots
require(ggpubr) # ggarrange
require(DescTools) # SKew and Kurtosis
require(TSA) # mcleod li test
require(fUnitRoots) # ADF Test
require(urca) # KPSS Test
library(depmixS4) # hmm ts
library(gridExtra) # hmm ts
library(reshape2) # hmm ts
library(dplyr) # hmm ts
library("tidymodels") # deepAR
library("tidyverse") # deepAR
library("timetk") # deepAR
library(xcrun) #deepAR
library("modeltime.gluonts") # deepAR
options(scipen=999) # set scientific notation off





###########################################
## Section 1. Random Forest Time Series ###
###########################################

# Read data 
X <- read.csv("/Users/dylanhayashi/Desktop/Northwestern/413 - Time-Series Analysis & Forecasting/TS10/DTload.csv")

### Section 1.1 - EDA ### 

# There are multiple seasonalities
mpd <- 48   # 48 30-minute time intervals per day
days <- 7  # 1 week

# Time plots 
par(mfrow = c(1,1))
(span <- 7*mpd)
x <- seq(1,(7+1),1/mpd)
plot(x[-length(x)],X[1:span,2],type="l",ylab="Usage",xlab="Days");grid(col="gray")
(span <- 28*mpd)
x <- seq(1,(28+1),1/mpd)
plot(x[-length(x)],X[1:span,2],type="l",ylab="Usage",xlab="Days");grid(col="gray")
(span <- 119*mpd)
x <- seq(1,(119+1),1/mpd)
plot(x[-length(x)],X[1:span,2],type="l",ylab="Usage",xlab="Days");grid(col="gray")

# Confirmation of time series
basicStats(X$value) # Variance -> stochastic

# Normality 
hist_1 = ggplot(X,aes(x=value)) + # Histogram
  geom_histogram(aes(y=..density..),color='Black',fill='Grey')+
  geom_density(alpha=0.6,color='blue')+
  ggtitle("Histogram of Electricity Usage")+
  labs(x='value',y='Density')
qq_1 = ggplot(X,aes(sample=value))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of Electricty Usage")+labs(x='Theoretical',y='Sample') # QQ-Plot
acf_1 = ggAcf(X$value) #ACF - Autocorrelation
ggarrange(hist_1,qq_1,acf_1,nrow=1,ncol=3) # Plot plots
Skew(X$value, method = 3, conf.level = 0.95, ci.type = "norm", R = 1000) #Skew
Kurt(X$value, method = 3, conf.level = 0.95, ci.type = "norm", R = 1000) # Excess kurtosis

# Stationarity
t.test(X$value) # Mean zero
McLeod.Li.test(y=X$value) # McLeod-Li (constant variance)
adfTest(ts(X$value),lags=5,type="nc") # ADF Test
ur.kpss(ts(X$value),type="tau",lags="short") # KPSS Test

# Decomposition
autoplot(stl(ts(X$value,frequency=48),"periodic"))

### Section 1.2 - Fast Fourier Transformatin for Period Identification ###

tv <- ts(X$value,frequency=336)   # 48 observations/day times 7 days
plot(tv[1:60],type="l");grid(col="gray")      
sv <- stl(tv,s.window="periodic")
plot(sv)
str(sv)
sp <- spectrum(sv$time.series[,1],method="ar");grid(col="gray")
sp <- spectrum(tv,method="ar");grid(col="gray")
str(sp)
sp$period <- 1/sp$freq
spp <- cbind(sp$freq[2:500],sp$period[2:500],sp$spec[2:500,1])
colnames(spp) <- c("Freq","Period","Spec Power")
spp[spp[,3]>9999,]

### Section 1.3 - Model Creation ###

# this data set has 2 seasons
df <- msts(as.vector(X$value),seasonal.periods = c(2,48,336))

# build model, specifying a 2-day forecast
mrf <- tsrf(train = df, hf = 96, ntree = 500, mtry = 3, nodesize = 3, KK = 1)

# Variable MSE Table
var.imp <- data.frame(importance(mrf$rf, type=1))

# make row names as columns     c=cosine, s=sine, lag=seasonal lag
var.imp$vars <- row.names(var.imp)
var.order <- data.frame(var.imp[order(var.imp$X.IncMSE,decreasing = T),], row.names=2)
names(var.order)[1] <- "Decreasing%MSE"
var.order

# Variable Node Purity Table
var.imp <- data.frame(importance(mrf$rf, type=2))

# make row names as columns     c=cosine, s=sine, lag=seasonal lag
var.imp$vars <- row.names(var.imp)
var.order <- data.frame(var.imp[order(var.imp$IncNodePurity,decreasing = T),], row.names=2)
names(var.order)[1] <- "DecreasingNodePurity"
var.order

# build forecast
f <- forecast(mrf)
# show forecast
summary(f)

par(mfrow = c(3,1))
plot(f);grid(col="gray") # plot forecast
tx <- time(f$x) # plot again, but zoom into forecast
tf <- time(f$mean)
end <- tf[length(tf)]
start <- tx[length(tx)-48*14] # last 2 weeks
plot(f,xlim=c(start,end));grid(col="black")
start = tx[length(tx)-48*7] #last week
plot(f,xlim=c(start,end));grid(col="black")





###########################################
#     Recurrent Neural Network Time Series
###########################################

###########################################
#	deepAR
###########################################

# Installation (GitHub Version (Dancho:2022wi), https://business-science.github.io/modeltime.gluonts/)
# There are 2 key components to installing modeltime.gluonts:
# 1. Download the R-Package, modeltime.gluonts. This installs the R-Bindings which interfaces with GluonTS.
# 2. Set up the Python Environment so modeltime.gluonts can connect to the gluonts python package.
# remotes::install_github("business-science/modeltime.gluonts")
# install.packages(cxrun)
# install_gluonts()
# Default Python Environment Setup
# A helper function, install_gluonts(), prepares and binds to a python environment containing gluonts and the required python packages.
# Run this one time only.
# Each time library(modeltime.gluonts), the package includes this environment in it’s search process.
# install_gluonts(fresh_install = TRUE, include_pytorch = TRUE)
# Now shutdown your R session and restart only if "install_gluonts" was the 1st-time run
# https://business-science.github.io/modeltime.gluonts/articles/getting-started.html
# We’ll use the walmart_sales_weekly dataset, which contains 7 weekly time series of sales data for various departments in a Walmart Store.
library("modeltime.gluonts")
install_gluonts()

# Read data 
data <- walmart_sales_weekly %>%
  select(id, Date, Weekly_Sales) %>%
  set_names(c("id", "date", "value"))

data %>%
  group_by(id) %>%
  plot_time_series(
    date, 
    value, 
    .facet_ncol = 3, 
    .interactive = FALSE
  )


data %>%
  group_by(id) %>%
    ggplot(data,aes(x=date,y=value)) + geom_line()

ggplot(data, aes(x = date, y = value[1])) + geom_line() # Line plot



### Section 3.1 - EDA ###
data %>%
  group_by(id)

data %>%
  group_by(id) %>%
    gpplot()


### Section 3.2 - ###

# Set forecast region 1 week (24x7 timestamps)

HORIZON <- 52    # Set to 13 for 1 quarter

new_data <- data %>%
  group_by(id) %>%
  future_frame(.length_out = HORIZON) %>%
  ungroup()

new_data


# Create a DeepAR model using the deep_ar() function.
# This is a univariate modeling algorithm that uses Deep Learning and Autoregression.
# For quarterly forecast, use more that 2 lookback leengths.

model_fit_deepar <- deep_ar(
  id                    = "id",
  freq                  = "W",
  prediction_length     = HORIZON,
  lookback_length       = 2*HORIZON,
  epochs                = 5
) %>%
  set_engine("gluonts_deepar") %>%
  fit(value ~ date + id, data)


# Generate a forecast for the multiple time series groups

modeltime_forecast_tbl <- modeltime_table(
  model_fit_deepar
) %>%
  modeltime_forecast(
    new_data    = new_data,
    actual_data = data,
    keep_data   = TRUE
  ) %>%
  group_by(id) 


# We can visualize the forecast with plot_modeltime_forecast().

modeltime_forecast_tbl %>%
  plot_modeltime_forecast(
    .conf_interval_show = FALSE, 
    .facet_ncol         = 3, 
    .facet_scales       = "free",
    .interactive        = FALSE
  )

# Forecast analysis

str(modeltime_forecast_tbl)
Y <- as.data.frame(modeltime_forecast_tbl[,c(2:6)])  # choose appropriate columns
str(Y)
hist(Y$.value[Y$.key=="prediction"])

# install.packages("npmv")
library("npmv")
Yp <- Y[Y$.key=="prediction",]

nonpartest(.value ~ id, Yp, permreps=1000, plot=F)




# Saves all of the model files if desired. Choose path.
model_fit_deepar %>%
  save_gluonts_model(path = "deepar_model", overwrite = TRUE)

# Reload the model into R using load_gluonts_model().
model_fit_deepar <- load_gluonts_model("deepar_model")






###########################################
#     HMM Time Series
###########################################

# Read data
X <- read.csv("/Users/dylanhayashi/Desktop/Northwestern/413 - Time-Series Analysis & Forecasting/TS10/Exo.csv")

### Section 3.1 - EDA ###

ggplot(X, aes(x = detect, y = obs)) + geom_line() # Line plot

# Confirmation of time series
basicStats(X$obs) # Variance -> stochastic

# Normality 
hist_1 = ggplot(X,aes(x=obs)) + # Histogram
  geom_histogram(aes(y=..density..),color='Black',fill='Grey')+
  geom_density(alpha=0.6,color='blue')+
  ggtitle("Histogram of Obs")+
  labs(x='value',y='Density')
qq_1 = ggplot(X,aes(sample=obs))+stat_qq(color='Blue')+stat_qq_line()+ggtitle("QQ-Plot of Obs")+labs(x='Theoretical',y='Sample') # QQ-Plot
acf_1 = ggAcf(X$obs) #ACF - Autocorrelation
ggarrange(hist_1,qq_1,acf_1,nrow=1,ncol=3) # Plot plots
Skew(X$obs, method = 3, conf.level = 0.95, ci.type = "norm", R = 1000) #Skew
Kurt(X$obs, method = 3, conf.level = 0.95, ci.type = "norm", R = 1000) # Excess kurtosis


# Stationarity
t.test(X$obs) # Mean zero
McLeod.Li.test(y=X$obs) # McLeod-Li (constant variance)
adfTest(ts(X$obs),lags=5,type="nc") # ADF Test
ur.kpss(ts(X$obs),type="tau",lags="short") # KPSS Test


# Decomposition
autoplot(stl(ts(X$obs,frequency=10),"periodic"))

Ecols <- c("darkmagenta", "turquoise")
cols <- ifelse(X$state == "E1", Ecols[1], Ecols[2])

# Construct hmm
m <- HMM(X)
`
plot.decode(m)


# install.packages("OptimalCutpoints")
library("OptimalCutpoints")
# accuracy measures:


OC <- data.frame(fit=as.numeric(factor(m$X$est.state.labels))-1, truth=as.numeric(factor(m$X$state))-1)

optimal.cutpoint.Youden <- optimal.cutpoints(X = fit  ~ truth , tag.healthy = 0, 
        methods = "Youden", pop.prev = NULL,  data=OC,
        control = control.cutpoints(), ci.fit = FALSE, conf.level = 0.95, trace = FALSE)

(ms <- summary(optimal.cutpoint.Youden))

plot(optimal.cutpoint.Youden)

# Sensitivity/Specificity (Se/Sp)
# Predictive Values (Positive Predictive Values/Negative Predictive Values)
# Diagnostic Likelihood Ratios (Positive/Negative)
# Prevalence (False Positive/False Negative)
# Optimal criterion (optimum cut between positive and negative test)


########################################
