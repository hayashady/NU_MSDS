# R Lesson 4 - Probability

# Dataset: Shoppers
spending <- shoppers$Spending

# Question 1

# 1a. Probability of selecting shopper who spent $40 =<, < 10
table(spending >= 40)
8 / 50
table(spending<10)
5 / 50
# Answer: 0.16, 0.1

# 1b. Probability of selecting pair with one >= 40 and one < 10
# Combinations function
combinations <- function(n,r){
     factorial(n) / (factorial(r)*factorial(n-r))
# Number of viable combinations
5 * 8
# Probability
40 / (combinations(50,2))
# Answer = 0.03265306

# 1c. Probability of selecting pair with 10 < spending < 40
m <- sum((spending >= 10) & (spending <= 40))
n <- 50
(m*(m-1)) / (n*(n-1))
# Answer: 0.5436735

# 1d. Probability of selecting four with features listed above
n <- 50
n1 <- sum(spending < 10)
n2 <- sum(spending > 40)
n3 <- sum((spending >= 10) & (spending <= 40))
numerator <- n1 * n2 * n3 * (n3 - 1) / 2
denominator <- (n * (n-1) * (n-2) * (n-3)) / factorial(4)
# Answer: 0.1156752

# 1e. Probability a shopper spent > 40 give spent > 30
sum(spending > 40) / sum(spending>30)
# Answer: 0.4705882

# Question 2

# 2a. 
set.seed(1234)
exceptions <- 0
for(i in 1:100){
  currentsample <- sample(seq(1:365),22,replace=TRUE)
  if(length(currentsample) != length(unique(currentsample)))
    exceptions <- exceptions + 1
    }
exceptions / 100
