# R Lesson 1 - Introduction to Statistics
homedata <- X2_Data_Set_home_prices

# Question 3 - SRS with n = 12 from PRICE, calculate mean
SRS <- sample(homedata$PRICE, 15)
print(SRS)
mean(SRS)

# Question 4 - Systematic Sample with n = 12 from PRICE, calculate mean
ssample <- homedata$PRICE[seq(from=7, to=117, by=10)]
mean(ssample)

# Question 6 - Historgram and Stem-and-Lead Plots for SRS & SS
hist(SRS)
stem(SS)

hist(SRS)
stem(SS)

# Attempted SS Function
ssample <- function(x){
  ss <- x[7]
  k = 17s
  while(k<=length(x)){
  append(ss,x[k])
  k <- k + 10
    }
  return(ss)
  }