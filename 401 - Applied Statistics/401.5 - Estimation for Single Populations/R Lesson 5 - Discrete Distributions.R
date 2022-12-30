# R Lesson 5 - Discrete Distributions

# Question 1 - Horse Races

# 1a. Winning all four races
a <- dbinom(4,4,(1/6))
# answer = P = 0.00077

# 1b. Losing all four races
b <- dbinom(0,4,(1/6))
# answer: P = 0.48225

# 1c. Wining exactly one race
c <- dbinom(1,4,(1/6))
# answer: P = 0.3858

# 1d. Winning at least one race
d <- 1 - b
# answer: P = 0.5177

# Question 2 - Tea
# 2a. Correctly guessing at least 15 cups
a <- pbinom(15,20,0.5,FALSE)
# answer: P = 0.0059
# 2b. 

# Question 3 - Emergency Room Visits

# Question 4 - Production Defects
# 4a. 
# 4b. 