# R Lesson 8 - Statistical Inference Estimation for Single Populations

# Question 1
# Draw sample of size 100 from normal distribution with variance = 1, xbar = 50
# Find 95% confidence interval for population mean
n = 100
xbar = 50
popvar = 1
popstddev = 1
marginoferror <- qnorm(1-(0.05/2))*(popstddev/sqrt(n))
confint<- c(xbar - marginoferror, xbar + marginoferror)
confint
# Answer: (49.8 - 50.2)

# Question 2
# Normal distribution with std dev = 100
# Want to estimate population mean where range = 8 and conf = 0.95
zscore <- qnorm(0.025, 0, 1, lower.tail = FALSE)
n <- (zscore * (100 / 4))**2
n
# Answer = 2401

# Question 3
# Sample of 1600 voters resulted in pbar = 0.6 for approval.
# Find 95% confidence interval for population proportion.
p = 0.6
n = 1600
prop.test(x = (n * p), n = n, alternative = 'two.sided',conf.level = 0.95)
# Answer: (0.5754686 - 0.6240461)

# Question 4
# Sample of consumers resulted in pbar = 0.85 of favoring one soda over another
# What sample size would be needed to construct a 95% conf int with margin of error = 2?
zscore <- qnorm(0.025,0,1,lower.tail = FALSE)
p = 0.85
E = 0.01
nreq <- ((zscore**2)*(p * (1 - p))) / (E **2)
nreq
# Answer: 4897.86

# Question 5
# Dataset: Hot Dogs
# Find 95% conf int for calories of each type of meat
beef <- subset(Hot_Dogs, subset = (Hot_Dogs$Type == 'Beef'))
meat <- subset(Hot_Dogs, subset = (Hot_Dogs$Type == 'Meat'))
poul <- subset(Hot_Dogs, subset = (Hot_Dogs$Type == 'Poultry'))
with(beef, t.test(Calories)$conf.int)
# Answer: (146.2532 - 167.4468)
with(meat, t.test(Calories)$conf.int)
# Answer: (145.7308 - 171.6809)
with(poul, t.test(Calories)$conf.int)
# Answer: (107.1698 - 130.3596)
# Find 99% conf int for calories of each type of meat, one-sided left
with(beef, t.test(Calories, alternative = 'less', conf.level = 0.99)$conf.int)
# Answer: 169.7072
with(meat, t.test(Calories, alternative = 'less', conf.level = 0.99)$conf.int)
# Answer: 174.5183
with(poul, t.test(Calories, alternative = 'less', conf.level = 0.99)$conf.int)
# Answer: 132.8951

# Question 6
# Create 95% conf int for variance of calories in each type
estpopvar = function(x, conf.level = 0.95){
  df <- length(x) - 1
  lowerbound <- qchisq((1 - conf.level)/2, df, lower.tail = TRUE)
  upperbound <- qchisq((1 - conf.level)/2, df,lower.tail = FALSE)
  samvar <- var(x) 
  confint <- c(df * samvar/upperbound, df * samvar/lowerbound)
}
beefvar <- estpopvar(beef$Calories)
beefvar
# Answer: (296.495 - 1093.643)
meatvar <- estpopvar(meat$Calories)
meatvar
# Answer: (353.2469 - 1475.1049)
poulvar <- estpopvar(poul$Calories)
poulvar
# Answer: (282.0926 - 1177.9754)