# Dylan Hayashi
# MSDS 413 - Time-Series Analysis & Forecasting
# TS1 - Assignment


require(ggplot2)
require(DescTools)
require(lmtest)
require(fBasics)
require(fpp3)
require(forecast)

ggQQ <- function(LM, kind="Residuals") {  # argument: a linear model
  y <- quantile(LM$resid[!is.na(LM$resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  p <- ggplot(LM, aes(sample=.resid)) +
    stat_qq(alpha = 0.5) +
    geom_abline(slope = slope, intercept = int, color="blue") +
    ggtitle(paste(kind,"Q-Q Plot")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position="bottom")
  return(p)}

## Section 1 - Gauss-Markov Assumptions Review 


# Read data
shingles <- read.table("/Users/dylanhayashi/Desktop/Northwestern/413 - Time-Series Analysis & Forecasting/Module 1/TS1_2/Roofing.txt", sep="", header=F, col.names=c("sales", "expend", "accounts", "comp", "potential"), stringsAsFactors= F)
str(shingles)
X <- shingles
set.seed(20214)
X$sales <- X$sales * runif(1,0.8,1.2)
summary(X)


# Section 1.2
m <- lm(sales ~ expend + accounts + comp + potential, data=X)

# Section 1.3
summary(residuals(m))
ggQQ(m)

# ggsave("Plots/RoofingNQQ.png")
ggplot(X, aes(x=residuals(m))) + geom_histogram(aes(y=..density..)) + geom_density(alpha=0.6) + ggtitle('Histogram of Residuals')
# install.packages("DescTools")
t.test(resid(m))    # test for mean = 0
Skew(resid(m), conf.level=0.95)
Kurt(resid(m), conf.level=0.95)   # excess kurtosis, H0: k=0 vs Ha: k!= 0


# ASSUMPTIONS: CONSTANT VARIANCE

plot(residuals(m) ~ predict(m), pch=8, col="blue");grid(col="gray")


#	TEST FOR CONSTANT VARIANCE, Breusch-Pagan, H0: constant variance
bptest(m)


# PARAMETER ESTIMATES AND F-TEST
summary(m)


#############################################
# Part 2

# Acquire the Covid-19 data
url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
fn <- "Covid19.csv"
download.file(url, fn)
Covid19 <- read.csv("Covid19.csv",header=T)

names(Covid19);nrow(Covid19)

# Select US data
X <- Covid19[Covid19$iso_code=="USA",]
nrow(X);head(X$date);tail(X$date)

# Select date, total_cases, and Population
X <- X[,c("date","total_cases","population")]
X <- na.omit(X)
X$date <- as.Date(X$date)
summary(X);nrow(X)

# Time series plots
# Plots require titles and axis labels. Add them as needed.
# install.packages("ggplot2")
options(scipen=999)
ggplot() + geom_line(data = X, color = 'Blue', aes(x = date, y = total_cases)) + ggtitle("Total Cases Over Time") + labs(x = 'Year', y='Total Cases')
ggplot() + geom_line(data = X, color = 'Blue', aes(x = date, y = population)) + ggtitle("Population Over Time") + labs(x = 'Year', y='Population')

# t in {1,2,...}. If lengths are equal, all t are unique. H0: length the same
length(unique(X$date))
length(X$date)
length(X$total_cases)
length(X$population)

# Any missing dates and does t+1 - t = c. H0: dihh = 1
dif <- diff(as.Date(X$date,"%Y-%m-%d"))
nrow(X)
table(dif)   # nrow - 1?


#############################################
# Part 3

# basic stats, histograms, and QQ plots of raw data
# install.packages("fBasics")
# install.packages("DescTools")
basicStats(X$total_cases)   # gives excess kurtosis
ggplot(X, aes(x=total_cases)) + geom_histogram(aes(y=..density..)) + geom_density(alpha=0.6) + ggtitle("Distribution of Total Cases") + labs(x = 'Total Cases', y='Density')
ggplot(X, aes(sample=total_cases)) + stat_qq() + stat_qq_line(color='blue') + ggtitle("QQPlot of Total Cases") + labs(x = 'Theoretical', y='Sample')

basicStats(X$population)
ggplot(X, aes(x=population)) + geom_histogram(aes(y=..density..)) + geom_density(alpha=0.6)+ ggtitle("Distribution of Population") + labs(x = 'Population', y='Density')
ggplot(X) + geom_qq(aes(sample = population))+ ggtitle("QQPlot of Population") + labs(x = 'Theoretical', y='Sample')

t.test(X$total_cases)   # get CI for mean
t.test(X$population)

# zero in CIs?
Skew(X$toital_acses, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)
Kurt(X$total_cases, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)  # gives excess kurtosis

var.CI = function(data, conf.level = 0.95) {
	df = length(data) - 1
	chilower = qchisq((1 - conf.level)/2, df)
	chiupper = qchisq((1 - conf.level)/2, df, lower.tail = FALSE)
	v = var(data)
	x <- c(df * v/chiupper, v, df * v/chilower)
	names(x) <- c("lower.ci", "variance", "upper.ci")
	return(x)
	}
# zero in CIs?
var.CI(X$total_cases)    # get CI for variance


#############################################
# Part 4

y <- log(X$total_cases+1)

ggplot() + geom_point(data = X, aes(x = date, y = y)) + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + ggtitle('Log Total Cases') + labs(x = 'Date', y='Total Cases')
basicStats(y)

ggplot(X, aes(x= y)) + geom_histogram(aes(y=..density..)) + geom_density(alpha=0.6) + ggtitle('Histogram of Log Total Cases') + labs(x='Log Total Cases',y='Density')

ggplot(X, aes(sample=y)) + stat_qq() + e(color='blue') + ggtitle("QQPlot of Log Total Cases") + labs(x = 'Theoretical', y='Sample')


ggplot(X, aes(sample=y)) + stat_qq() + stat_qq_line()

t.test(y)   # H0: mean = 0 => no linear trend

Skew(y, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)
Kurt(y, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)  # gives excess kurtosis


y <- log(X$population+1)

ggplot() + geom_point(data = X, aes(x = date, y = y)) + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + ggtitle('Log Population') + labs(x = 'Date', y='Total Cases') + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
basicStats(y)

ggplot(X, aes(x= y)) + geom_histogram(aes(y=..density..)) + geom_density(alpha=0.6) + ggtitle('Histogram of Log Population') + labs(x='Log Total Cases',y='Density')

ggplot(X, aes(sample=y)) + stat_qq() + stat_qq_line(color='blue') + ggtitle("QQPlot of Log Population") + labs(x = 'Theoretical', y='Sample')


ggplot(X, aes(sample=y)) + stat_qq() + stat_qq_line()

t.test(y)   # H0: mean = 0 => no linear trend

Skew(y, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)
Kurt(y, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)  # gives excess kurtosis




#############################################
# Part 5


# https://www.ncdc.noaa.gov/cag/global/time-series
# Units: Degrees Celsius
# Base Period: 1901-2000
# Missing: -999

url <- "https://www.ncdc.noaa.gov/cag/global/time-series/globe/land_ocean/1/12/1880-2020/data.csv"
fn <- "Climate.csv"
download.file(url, fn)
Climate <- read.csv("Climate.csv",header=T,skip=4)
X <- Climate
str(X)

# convert years into decades
X <- X[X$Year<2020,]
decade <- rep(1:trunc(nrow(X)/10),each=10)
X$Decade <- paste0(decade, " (", X$Year,")")

ggplot() + geom_point(data = X, aes(x = Year, y = Value)) + 
	stat_smooth(aes(x = X$Year, y = X$Value), colour="red") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

# EDA here
basicStats(X$Value)
t.test(X$Value)
# histogram here
# normal Q-Q plot here
# tests here, etc.

# make X a time series
X <- data.frame(Decade=decade,Value=X$Value)
X <- ts(X[,], start=X[1,1], frequency=10)  # cycles by decade
str(X)
main <- "Annual Temperature (C) vs. Decade (1880-2019)"
autoplot(X[,2], main=main, ylab="Annual Temperature (C)", xlab="Decade")

# Make s.window as large as possible while keeping trend smooth
decomp <- stl(X$total_cases,frequency=1)
autoplot(decomp)

trend <- decomp$time.series[,1]   
basicStats(trend)
seasonal <- decomp$time.series[,2]   
basicStats(seasonal)



y <- decomp$time.series[,3]   # remainder

basicStats(y)

ggplot(as.data.frame(y), aes(x= y)) + geom_histogram(aes(y=..density..)) + geom_density(alpha=0.6)
ggplot(X, aes(sample=y)) +
	stat_qq() +
	stat_qq_line()


Skew(y, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)
Kurt(y, method = 3, conf.level = 0.05, ci.type = "norm", R = 1000)  # gives excess kurtosis


#############################################
