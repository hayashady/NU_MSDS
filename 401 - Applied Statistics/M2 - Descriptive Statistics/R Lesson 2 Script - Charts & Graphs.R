# R Lesson 2 - Charts & Graphs
homedata <- X2_Data_Set_home_prices

# Question 1

# 1a. Create histogram for PRICE
price <- homedata$PRICE
hist(price)

# 1b. Create histogram for TAX
tax <- homedata$PRICE
hist(tax)

# 1c. Create scatterplot of PRICE & TAX
plot(price,tax)

# 1d. Create stem-and-leaf plot for TAX w/ & w/o rounding
X <- tax / 100
stem(round(X))
stem(tax)

# 1e. Create window with specific dimensions for histograms
par(mfrow = c(1,2))
with(homedata, hist(price))
with(homedata, hist(tax))

# Question 2

# 2a. Create histogram of price with specified buckets
buckets <- seq(1300,5500,by = 600)
hist(price,breaks = buckets)

# 2b. Create histogram or tax with specified buckets
newbuckets <- seq(500,5500,by = 500)
hist(tax,breaks = newbuckets)