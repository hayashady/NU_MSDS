# R Lesson 3 - Descriptive Statistics

# Question 1

# Dataset: Mileage
cars <- X2_Data_Set_mileage
str(cars)
# a. Mean and Std. Dev of MPG for each vehicle class
class <- list(cars$CLASS)
aggregate(cars$MPG, class, FUN = 'mean')
aggregate(cars$MPG, class, FUN = 'sd')

# b. Mean and Std. Dev. of HP for each vehicle class
aggregate(cars$HP, class, FUN = 'mean')
aggregate(cars$HP, class, FUN = 'sd')

# Question 2

# Dataset: Shoppers
shoppers <- X3_Data_Set_shoppers
str(shoppers)
spending <- shoppers$Spending

# 2a. Summary statistics for dataset
mean(spending)
median(spending)
range(spending)
sd(spending)
var(spending)
summary(spending)

# Function Definition
questionanswer <- function(x) {
  stats <- data.frame(rbind(
    mean(x, na.rm = TRUE),
    median(x, na.rm = TRUE),
    range(x),
    sd(x, na.rm = TRUE),
    var(x, na.rm = TRUE),
    quantile(x, probs = c(0.25), na.rm = TRUE),
    quantile(x, probs = c(0.75), na.rm = TRUE),
    quantile(x, probs = c(0.10), na.rm = TRUE)),
    row.names = c("Mean", "Median", "Range", "StdDev", "Var", "Q1", "Q3", "P10"))
  colnames(stats) <- "Value"
  return(stats) }

# 2b. Historgram
hist(spending)

# Question 3

# Dataset: PONTUS
pontus <- X4_Data_Set_pontus
str(pontus)

# 3a. Summary Stats for presidents' ages
questionanswer(pontus$Age)

# 3b. Summary Stats for height of presidents and their opponents
questionanswer(pontus$Ht)
questionanswer(pontus$HtOpp)

# 3d. Create histogram and box plot of height differences
heightdiff <- pontus$Ht - pontus$HtOpp
hist(heightdiff)
box(heightdiff)

# Question 4

# Dataset: Geyser
geyser <- X5_Data_Set_geyser
str(geyser)

# 4a. Create summaries, histograms, and boxplots for the two geysers
summary(geyser$WEEK1)
summary(geyser$WEEK2)
hist(geyser$WEEK1)
boxplot(geyser$WEEK1)
boxplot(geyser$WEEK2)
