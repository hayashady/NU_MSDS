# R Lesson 6 - Continuous Distributions

# Question 1
# Normal distribution of shoppers spending with mean of $81.14 and std dev of $20.71
# A) Probability of spending less than 75
pnorm(75, 81.14, 20.71, lower.tail = TRUE)
# Answer: 0.3834
# B) Proportion of shoppers that spend more than 100
pnorm(100,81.14,20.71, lower.tail = FALSE)
# Answer: 0.1812
# C) Proportion that spend between 50 and 100
1 - pnorm(50,81.14,20.71, lower.tail = TRUE) - pnorm(100,81.14,20.71, lower.tail = FALSE)
# Answer: 0.7524

# Question 2
# Normal distribution of shoppers spending with mean of $97.11 and std dev of $39.46
# What is 90th percentile spending amount
qnorm(0.9, 97.11, 39.46)
# Answer: $147.68
# What is median spending amount?
qnorm(0.5, 97.11, 39.46)
# Answer: $97.11 (mean)

# Question 3
# Create sample size of 50 from normal distribution with mean of 100 and std dev of 4
# Find mean, std dev, and standrd error of created sample
sampleone <- rnorm(50,100,4)
mean(sampleone)
sd(sampleone)
std.error(sampleone)
# Answer: 99.9284, 3.680356, 0.520481
# Repeat
sampletwo <- rnorm(50,100,4)
mean(sampletwo)
sd(sampletwo)
std.error(sampletwo)
# Answer: 100.2056, 3.657283, 0.5172179
# Repeat with n = 5000
samplethree <- rnorm(5000, 100, 4)
mean(samplethree)
sd(samplethree)
std.error(samplethree)
# Answer: 100, 3.975626, 0.05622385

# Question 4
# Biased coin flips heads 1/3 of time. Estimate r = 250, n = 600 Compare binomial and normal distribution.
n = 600
r = 250
p = 1/3
q = 1 - 1/3
sprintf('%.10f', pbinom(r,n,p,lower.tail = FALSE))
sprintf('%.10f', pnorm(r,(n*p),(sqrt(n*p*q)),lower.tail = FALSE))
# Answer: 0.0000085228, 0.0000074512

# Question 5
# Create a uniform distribution and make histograms of samples drawn with different sizes
par(mfrow=c(1,3))
hist(runif(25,0,1))
hist(runif(100,0,1))
hist(runif(400,0,1))

# Question 6
# Use Salaries data set and create histograms / qq-plots for age
hist(salaries$AGE)
qqnorm(salaries$AGE)
qqline(salaries$AGE)