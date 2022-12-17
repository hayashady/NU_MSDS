# R Lesson 10 - Statistical Inference About Two Populations

# Question 1
# Clinical trial with two populations, n = 100, p1 = 0.85, p2 = 0.65
n = 100
p1 = 85
p2 = 65
q1matrix = matrix(data = c(p1, (100-p1), p2, (100-p2)), ncol = 2, nrow = 2, byrow = TRUE)
q1matrix
# Is there a statistically significant difference at the 95% confidence level?
# Use a one-sidede test and provide confidence interval.
prop.test(q1matrix, alternative = "greater", conf.level = 0.95)
# Answer: p-value = 0.0009, < 0.05, we reject null hypothesis that there is no difference
# Answer: (0.09199653, 1)

# Question 2
# Two baseball players, hitting ratios: 85 / 267 and 89 / 248
p1tot = 267
p1hit = 85
p2tot = 248
p2hit = 89
q2matrix = matrix(data = c(p1hit, (p1tot - p1hit), p2hit, (p2tot - p2hit)), ncol = 2, nrow = 2, byrow = TRUE)
q2matrix
# Is there a statistically significant difference between the two players at 95%?
prop.test(q2matrix, alternative = "two.sided", conf.level = 0.95)
# Answer: p-value = 0.3799, > 0.05, we cannot reject the null hypothesis that there is no difference

# Question 3
# Dataset: Home Prices
# Hypothesis testing of homes in northeast vs rest of the city
northeast = subset(Home_Prices$PRICE, Home_Prices$NBR == "YES")
other = subset(Home_Prices$PRICE, Home_Prices$NBR == "NO")
t.test(northeast, other, two.sided = TRUE, conf.level = 0.95)
# Answer: p-value = 0.1134, > 0.05, we cannot reject the null hypothesis that there is no difference
# Hypothesis testing of homes in and out of corner lots
corner = subset(Home_Prices$PRICE, Home_Prices$CORNER == "YES")
notcorner = subset(Home_Prices$PRICE, Home_Prices$CORNER == "NO")
t.test(corner,notcorner,two.sided = TRUE, conf.level = 0.95)
# Answer: p-value = 0.6685, > 0.05, we cannot reject the null hypothesis that there is no difference

# Question 4
# Dataset: nsalary
# Hypothesis test of mean salaries of rural and non-rule areas at 95% (two sided t)
rural = subset(nsalary$NSAL, nsalary$RURAL == "YES")
nonrural = subset(nsalary$NSAL, nsalary$RURAL == "NO")
t.test(rural, nonrural, two.sided = TRUE, conf.level = 0.95)
# Answer: p-value = 8.504e-06, < 0.05, we can reject the null hypothesis that there is no difference
# Boxplot of mean salaries of rural and non-rule areas
boxplot(rural)
boxplot(nonrural)

# Question 5
# Dataset: tires
# Hypothesis test of treadwear of two methods
t.test(tires$WGT, tires$GRO, paired = TRUE, conf.level = 0.95)
# Answer: p-value = 4.614e-05, < 0.05, we can reject the null hypothesis that there is no difference