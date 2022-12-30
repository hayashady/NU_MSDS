# R Lesson 7 - Sampling and Sampling Distributions

# Question 1
# Create uniform distribution over (0,1)
# Draw 100 samples of size 10, 50, 500 calculate the means & var/std dev, plot via hist
sampleonemeans = c()
sampleonevars = c()
for (i in 1:100) {
  unif <- runif(10,0,1)
  sampleonemeans <- append(sampleonemeans,mean(unif))
  sampleonevars <- append(sampleonevars,var(unif))
  }
# Repeat with sample size of 50
sampletwomeans = c()
sampletwovars = c()
for (i in 1:100) {
  unif <- runif(50,0,1)
  sampletwomeans <- append(sampletwomeans,mean(unif))
  sampletwovars <- append(sampletwovars,var(unif))
}
# Repeat with sample size of 500
samplethreemeans = c()
samplethreevars = c()
for (i in 1:100) {
  unif <- runif(500,0,1)
  samplethreemeans <- append(samplethreemeans,mean(unif))
  samplethreevars <- append(samplethreevars,var(unif))
}
# Plot
par(mfrow=c(2,3))
hist(sampleonemeans)
hist(sampletwomeans)
hist(samplethreemeans)
hist(sampleonevars)
hist(sampletwovars)
hist(samplethreevars)

# Question 2
# Find quantiles of sampletwomeans and sampletwovars
# Create a normal distribution with domain(0,1) with mean & std dev = uniform distribution as samplefour
# Find quantiles of new means/vars
# Create samplefourmeans and samplefourvars
samplefourmeans = c()
samplefourvars = c()
for (i in 1:100) {
  norm <- rnorm(50, (1 - 0 ) / 2, (1 - 0) / sqrt(12))
  samplefourmeans <- append(samplefourmeans,mean(norm))
  samplefourvars <- append(samplefourvars, var(norm))
}
# Compare the two sets of quartiles
quantile(sampletwomeans)
quantile(sampletwovars)
quantile(samplefourmeans)
quantile(samplefourvars)

# Question 3
# Draw 100 samples of size 10 for binom dist with p = 0.5, create vector of means and var
samplefivemeans = c()
samplefivevars = c()
for (i in 1:100) {
  binom <- rbinom(10,1,0.5)
  samplefivemeans <- append(samplefivemeans,mean(binom))
  samplefivevars <- append(samplefivevars,var(binom))
}
# Draw size with size 50, create vector of means and var
samplesixmeans = c()
samplesixvars = c()
for (i in 1:100) {
  binom <- rbinom(50,1,0.5)
  samplesixmeans <- append(samplesixmeans,mean(binom))
  samplesixvars <- append(samplesixvars,var(binom))
}
# Draw with size 500, create vector of means and var
samplesevenmeans = c()
samplesevenvars = c()
for (i in 1:100) {
  binom <- rbinom(500,1,0.5)
  samplesevenmeans <- append(samplesevenmeans,mean(binom))
  samplesevenvars <- append(samplesevenvars,var(binom))
}
# Create hist and compare
par(mfrow=c(2,3))
hist(samplefivemeans)
hist(samplesixmeans)
hist(samplesevenmeans)
hist(samplefivevars)
hist(samplesixvars)
hist(samplesevenvars)

# Question 4
# Find quantiles of sample six means and vars
# Create normal distribution with true mean and vars of binom with same properties, collect means and vars
sampleeightmeans = c()
sampleeightvars = c()
for (i in 1:100){
  norm <- rnorm(50, 0.5, sqrt(0.25))
  sampleeightmeans <- append(sampleeightmeans, mean(norm))
  sampleeightvars <- append(sampleeightvars, var(norm))
}
# Compare quantiles
quantile(samplesixmeans)
quantile(samplesixvars)
quantile(sampleeightmeans)
quantile(sampleeightvars)


# Question 5
# Draw 100 samples of size 10 for binom dist with p = 0.1, create vector of means and var
sampleninemeans = c()
sampleninevars = c()
for (i in 1:100) {
  binom <- rbinom(10,1,0.1)
  sampleninemeans <- append(sampleninemeans,mean(binom))
  sampleninevars <- append(sampleninevars,var(binom))
}
# Draw size with size 50, create vector of means and var
sampletenmeans = c()
sampletenvars = c()
for (i in 1:100) {
  binom <- rbinom(50,1,0.1)
  sampletenmeans <- append(sampletenmeans,mean(binom))
  sampletenvars <- append(sampletenvars,var(binom))
}
# Draw with size 500, create vector of means and var
sampleelevenmeans = c()
sampleelevenvars = c()
for (i in 1:100) {
  binom <- rbinom(500,1,0.1)
  sampleelevenmeans <- append(sampleelevenmeans,mean(binom))
  sampleelevenvars <- append(sampleelevenvars,var(binom))
}
# Compare via hist
# Create hist and compare
par(mfrow=c(2,3))
hist(sampleninemeans)
hist(sampletenmeans)
hist(sampleelevenmeans)
hist(sampleninevars)
hist(sampletenvars)
hist(sampleelevenvars)

# Question 6
# Find quantiles of sample ten means and vars
# Create normal distribution with true mean and vars of binom with same properties, collect means and vars
sampletwelvemeans = c()
sampletwelvevars = c()
for (i in 1:100){
  norm <- rnorm(50, 0.1, sqrt(0.09))
  sampletwelvemeans <- append(sampletwelvemeans, mean(norm))
  sampletwelvevars <- append(sampletwelvevars, var(norm))
}
# Compare quantiles
quantile(sampletenmeans)
quantile(sampletenvars)
quantile(sampletwelvemeans)
quantile(sampletwelvevars)