# R Lesson 9 - Statistical Inference Hypothesis Testing for Single Populations

# Question 1
# Dataset: Hot Dogs
# Use hypothesis testing to determine which hot dog type has mean calories below 140 at 95% conf
beef <- subset(Hot_Dogs, subset = (Type == 'Beef'))
meat <- subset(Hot_Dogs, subset = (Type == 'Meat'))
poul <- subset(Hot_Dogs, subset = (Type == 'Poultry'))
with(beef, t.test(Calories, mu = 140, alternative = 'less')$conf.int[2] < 140)
with(meat, t.test(Calories, mu = 140, alternative = 'less')$conf.int[2] < 140)
with(poul, t.test(Calories, mu = 140, alternative = 'less')$conf.int[2] < 140)
# Type 'Meat' has entire 95% confidence interval below 140

# Question 2
# Use hypothesis testing to determine which hot dog type has a mean sodium level != 425
with(beef, t.test(Sodium, mu = 425, alternative = 'two.sided'))
with(meat, t.test(Sodium, mu = 425, alternative = 'two.sided'))
with(poul, t.test(Sodium, mu = 425, alternative = 'two.sided'))
# None of the p-values in the tests above is less than 0.05, so we do not reject the null hypothesis

# Question 3
# Use hypothesis testing to determine whether or not beef sodium variance = 6000 with 95% confidence
estpopvar = function(x, conf.level = 0.95){
  df <- length(x) - 1
  lowerbound <- qchisq((1 - conf.level)/2, df, lower.tail = TRUE)
  upperbound <- qchisq((1 - conf.level)/2, df,lower.tail = FALSE)
  samvar <- var(x) 
  confint <- c(df * samvar/upperbound, df * samvar/lowerbound)
}
beefsodvar <- estpopvar(beef$Sodium)
beefsodvar
# 6000 is in the rejection zone, so we reject the null hypothesis that beef sodium variance = 6000

# Question 4
# Sample with n = 100 is drawn from population with unknown mean and variance.
# Sample has mean = 50 and std dev = 2
# Use hypothesis testing to determine whether or not the population mean = 56 and 40
n = 100
xbar = 50
samstddev = 2
df = n - 1
test1mean = 56
test1t = (50 - 56) / (samstddev/sqrt(n))
test1p <- 2 * pt(-abs(test1t), df = df)
0.05 > test1p
# Answer: p-value is less than 0.05, we can reject null hypothesis
test2mean = 40
test2t = (50 - 40) / (samstddev/sqrt(n))
test2p <- 2 * pt(-abs(test2t), df = df)
0.05 > test2p
# Answer: p-value is less than 0.05, we can reject null hypothesis

# Question 5
# Coin is flipped 100 times, p should equal 0.5
# Test at the 95% confidence level whether or not the coin is unbiased with pbar = .43 and .6
pbar1 = 43
n = 100
mu = 50
cl = 0.95
prop.test(pbar1,n, alternative = 'less')
# Answer: p value of test 1 is > 0.05, we cannot reject null hypothesis
pbar2 = 60
prop.test(pbar2,n, alternative = 'greater')
# Answer: p value of test 2 is > 0.05, we cannot reject null hypothesis

# Question 6
# Dataset: Salaries
# Use hypothesis testing to determine whether or not at least 50% of CEOs are at least 45 yo
age <- salaries$AGE >= 45
agesum <- sum(age)
agecount <- length(age)
prop.test(agesum, agecount, alternative = 'greater')
# Answer: p-value of test 1 < 0.05, we can reject the null hypothesis

# Use hypothesis testing to determie whether or not at least 50% of CEOS make less than $500,000
salary <- salaries$SAL < 500000
salarysum <- sum(salary)
salarycount <- length(salary)
prop.test(salarysum, salarycount, alternative = 'greater')
# Answer: p-value of test 2 < 0.05, we can reject the null hypothesis