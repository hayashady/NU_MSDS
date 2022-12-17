# Test 3

# Question 2
answer2 <- pnorm(130,100,15)
answer2

# Question 3
# What is the critical value for a one-tailed test with cf = 0.95
answer3 <- qnorm(0.95, 0 , 1)
answer3

# Question 4
# Provide 95% CI for population variance
n = 20
xbar = 13.8
ssd = 3.9
svar = ssd**2
estpopvar = function(n, svar, conf.level = 0.95){
  df <- n - 1
  lowerbound <- qchisq((1 - conf.level)/2, df, lower.tail = TRUE)
  upperbound <- qchisq((1 - conf.level)/2, df,lower.tail = FALSE)
  samvar <- svar 
  confint <- c(df * samvar/upperbound, df * samvar/lowerbound)
}
answer4 <- estpopvar(n, svar)
answer4

# Question 5
# Find required sample size for estimating population proportion
zscore <- qnorm(0.99,0,1)
moe <- 0.011
cl <- 0.98
nreq <- ((zscore**2)*(0.5 * (1 - 0.5))) / (moe **2)
nreq

# Question 6
q6zscore <- qnorm(0.995,0,1)
q6zscore
ssd <- (((67.3 - 65.7)/2) / q6zscore)
ssd
q6zscore2 <- qnorm(0.975,0,1)
q6zscore2
xbar <- 65.7 + ((67.3 - 65.7)/2)
xbar
answer6 <- c((xbar - (q6zscore2 * ssd)), ((xbar + (q6zscore2 * ssd))))
answer6

# Question 7
# Find required sample size for estimating population mean
moe = 110
sd = 500
q7zscore <- qnorm(0.975,0,1)
nreq <- (q7zscore * (sd / (moe)))**2
nreq

# Question 9
# Sample size required to determine population proportion with 95% CL and MOE = 0.03
q9zscore <- qnorm(0.975,0,1)
moe <- 0.06
nreq <- ((q9zscore**2)*(0.5 * (1 - 0.5))) / (moe **2)
nreq

# Question 10