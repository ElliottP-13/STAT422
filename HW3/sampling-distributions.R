############################
### STAT 422 ##
############################ 

# MAKE SURE YOU READ ALL OF THE TEXT IN THIS EXAMPLE CODE!! POST 
# QUESTIONS YOU HAVE ON THE DISCUSSION BOARD. 

## Lady tasting tea - code for example from notes
## ------------------------------------------------------------------------
# create a function to plot the exact sampling distribution for 
# the total number of cups guessed correctly, where each guess 
# is assumed to be independent and follow a Bernoulli(p = 0.5) 
# distribution.
# highlight and run the entire function to load it into R
exact_binom <- function(n, p){
  # create vector for the sample space for Y = X1 +... X10
  sum_x <- c(0,seq(1:n))
  # compute probabilities for each y in Supp(Y), P(Y = y)
  prob <- dbinom(x = sum_x, size = n, prob = p)
  # plot the pmf f_Y(y)
  plot(x = sum_x, y = prob, xlab = "y = x1 + x2 + ... + x10", 
       ylab = expression(f[Y](y)), 
       main = paste(paste("Exact sampling distribution 
                          \n Y ~ Binomial(n =", n), 
                    paste(", p = ", p, ")")), pch = 20, type = "h")
  points(sum_x, prob, pch = 20)
  # return pdf
  return("pdf" = cbind("Y = Sum(X_i)" = sum_x, "P(Y = y)" = prob))
}

# use the function to create an object named "exact" that
# creates the exact sampling distribution for Y = X1 + X2 + ... + X10
exact <- exact_binom(n = 10, p = 0.5)
# look the object that's created
exact
# play around with different n and p to see what shapes a binomial 
# distribution can take on! Note that you can run the function 
# without storing it as an object 
# e.g., 
exact_binom(n = 5, p = 0.1)

## ------------------------------------------------------------------------
# create an approximate sampling dist for 
# the total number of correct guesses out of 10 trials, Y = X1 + ... _ X_10, 
# when we assume each guess (i.e., each X_i) is a Bernoulli(p = 0.5) RV 
# that is, she is just guessing -- essentially flipping a coin to pick milk or tea

# In R we can use the rbinom function to simulate 10 independent Bernoulli(p= 0.5)
# trials by setting n = 1 through the "size" argument in the rbinom function. 
# To generate a RS of size 10 from a Bernoulli(p = 0.5), 
# we run the following: X_1,...,X_10 ~ Bernoulli(p = 0.5) (i.e.,X_1,...X_10 are iid!)

samp <- rbinom(n = 10, size = 1, prob = 0.5)

# look at the observed result from this experiment, x1,x2,...,x10

samp

# now, we are interested in the sum of the xis. Our estimator is 
# Y = X_1 +... + X_10, and our estimate for this experiment is just 
# the observed sum. 

y_obs <- sum(samp)

# check that it makes sense with the sample
samp
y_obs

# Now we want to keep this value and do this whole process again. An 
# easy (but not the most computationally efficient) way to do this is 
# to use a for loop. First, we set up a storage vector for saving the 
# observed y = sum(x1,...,x10) from many experiements; let's start with 100 

y_vec <- rep(NA, 100)

# look at what the code above created...empty vector of length 100
y_vec

# we want to fill these spots with observed values for our statistic of
# interest, so let's just do what we did above 100 times and put the results
# in the ith spot of the sum_xi vector
for(i in 1:100){
  # first generate the results from 10 independent guesses (the observed xis)
  samp <- rbinom(n = 10, size = 1, prob = 0.5)
  # put y = sum(x1,...,x10) into the ith storage spot in y_vec
  y_vec[i] <- sum(samp)
  # the for loop will then do this for the i+1th spot, and then the 
  # i+2nd spot, all the way up to the 100th spot and then it will stop
}

# look at what values we observed for the 100 experiments, 
# make sure they are all integer values >= 0 and <= 10 because 
# that's the support of y!
y_vec

# look at the empricial probabilities: table(y_vec) creates a table of  
# the number of simulations that resulted in each potential outcome for 
# the Y = sum(X_1,...,X_10). Dividing by n_sim (the total number of simulations)
# creates an empirical probability for each of the potential outcomes for 
# this particular simulation
freq_tab <- table(y_vec)/100
freq_tab

# plot the sampling distribution 
plot(freq_tab, xlim = c(0,10), 
     xlab = "simulated y", type = "h", 
     ylab = "Relative Frequency", 
     main = "Approximate Sampling dist: Y = X1 + ... + X10, n_sim = 100 \n
                        samples of 10 independent Bernoulli(p = 0.5) RVs") 
points(y = freq_tab, x = as.numeric(names(freq_tab)), 
       col = "magenta")

# We may want to do this for different numbers of experiments quickly, 
# so let's write a function to create an approximate sampling dist for 
# the total number of correct guesses out of 10 trials, when we 
# assume each guess follows a Bernoulli(p = 0.5) distribution.
# To use this function, highlight the whole thing and then run it in R
approx_dist <- function(n_sim, n, p, seed){
  # set seed so that psuedo-random number generator is set and
  # results can be repeated
  set.seed(seed)
  # create storage vector for y_vec, this vector has length n_sim (that is, 
  # it has n_sim slots for storing the observed ys). Each slot begins 
  # by being filled with an NA (or null value)
  y_vec <- rep(NA, n_sim)
  # generate n_sim simulations of size n Bernouli p RVs and compute the sum
  # of the x_is for each sample using a for loop. 
  for(i in 1:n_sim){
    # note Bernoulli is binomial with samp size = 1, so the 
    # following line takes a random sample of size n from 
    # a Bernoulli(p) distribution
    samp <- rbinom(n = n, size = 1, prob = p)
    # compute the statistic of interest (in this case the sum) and 
    # store it in the ith position of the storage vector. That is, 
    # this next line stores the observed sum_xis in the ith position 
    # of the storage vector. 
    y_vec[i] <- sum(samp)
  }
  # create relative frequency table
  freq_tab <- table(y_vec)/n_sim
  # plot empirical or approximate sampling distribution
  plot(freq_tab, xlim = c(0,n), 
       xlab = "simulated y", type = "h", 
       ylab = "Relative Frequency", 
       main = paste(paste("Approximate Sampling dist: Y = X1 + ... + X10, n_sim = ", 
                    n_sim), 
       paste("samples of 10 independent Bernoulli(", p,") RVs")))
  points(freq_tab, x = as.numeric(names(freq_tab)), 
         col = "magenta")
  return(freq_tab)
}

# after the function is loaded, 
# use the function to create a sampling distribution with 100 draws
# save this as n_100a
n_100a <- approx_dist(n_sim = 100, n = 10, p = 0.5, seed = 122)

# look at table form of approximate sampling dist 
n_100a

# use the function to create a sampling distribution with 1000 draws, 
# save this as n_1000
n_1000 <- approx_dist(n_sim = 1000, n = 10, p = 0.5, seed = 122)
# look at table form of approximate sampling dist 
n_1000


# use the function to create a sampling distribution with 10000 draws
n_10000 <- approx_dist(n_sim = 10000, n = 10, p = 0.5, seed = 122)

# look at table form of approximate sampling dist 
n_10000
## Many phenomenon can be described assuming the parent population is 
# a normal distribution, oftentimes we're the sample mean is a statistic of 
# interest. 

## X_1, ...., X_n iid N(mu, sigma) - example from notes
## -----------------------------------------------------------------------
# sampling distributions for the sample mean, when X_is are iid N(mu, sigma)
# split the plotting window into three panels
par(mfrow = c(1,3))
# population distribution of X N(mu = 20, sigma = 5)
curve(dnorm(x, mean = 20, sd = 5), from = 0, to = 40, 
      xlab = "sample mean (x-bar)", ylab = "f_x-bar(x)", 
      main = "Population distribution", lwd = 2)
# exact sampling distribution of Xbar, where X_i iid N(mu = 20, sigma = 5)
curve(dnorm(x, mean = 20, sd = 5/sqrt(15)), from = 0, to = 40, 
      xlab = "sample mean (x-bar)", ylab = "f_x-bar(x)", 
      main = "Exact sampling dist. of the sample mean", lwd = 2)

# empirical sampling distribution of Xbar, where X_i iid N(mu = 20, sigma = 5)
# let's write a function! Challenge yourself to think about what this function is 
# doing. To work through a function line by line, define the arguments in the 
# global environment by uncommenting and running the following code 
# n_sim <- 10 
# n <- 15
# mu <- 20
# sigma <- 5
# and then work through lines inside the function (202 - 216) 
# and look at each object that's created line-by-line! 
xbar_norm <- function(n_sim, n, mu, sigma){
  # initialize empty vector to store sample means in 
  xbar <- rep(NA, n_sim)
  # draw many RS from the pop distribution, compute sample mean 
  for(i in 1:n_sim){
    samp <- rnorm(n = n, mean = mu, sd = sigma)
    xbar[i] <- mean(samp)
  }
  # plot the sample means 
  hist(xbar, xlim = c(0,40), xlab = "sample mean (x-bar)", 
       ylab = "Relative Frequency", freq = F, 
       main = "Approximate sampling dist. of the sample mean", nclass = 20)
  # add the exact distribution for comparison purposes 
  curve(dnorm(x, mean = 20, sd = 5/sqrt(15)), 
        from = 0, to = 40, add = T, lwd = 2)
  legend("topright", lty = 1, lwd = 2, legend = "N(20, 5/15)", bty = "n")
}

# use the function 
xbar_norm(n_sim = 100, n = 15, mu = 20, sigma = 5)


# play around with this function to see what happens as n_sim increases, try 
# 1000, 5000, 10000, and any other values you're interested in. 
# POST YOUR FINDINGS TO TO THE DISCUSSION BOARD. Your post will be worth 
# points on this HW assignment. If someone has already posted what you 
# would like to say, add something to their post, or post a question 
# you have about approximate sampling distributions. 

# Use the function(s) above as starting points for HW 3. You will need to change 
# the parent distribution for problem d. Run ? rexp to learn about the functions 
# r uses to generate iid realizations from an exponential distribution. 
# For problem f: you will need to change the statistic/esetimator of interest. 
# For both problems, you may write functions or just write code to create 
# and plot sampling distributions. For approximate samp. distributions, 
# make sure you use at least 1000 simulations. 

## More code from the lecture notes...
## ------------------------------------------------------------------------
# plot exact sampling distribution, the probabilities are in the 
# second column of exact (exact[,2]) and the sumx_i values are in 
# the first column of exact (exact[,1])
plot(x = exact[,1], y = exact[,2], type = "h", 
     xlab = "sum(xi)", ylab = "", 
     main = "Comparison of sampling distributions for \n 
     Total number of correct guesses out of 10")
# add points
points(x = exact[,1], y = exact[,2], pch = 20)
# add n = 100
points(x = as.numeric(names(n_100a)), y = n_100a, col = "red")
# add n = 1000
points(x = as.numeric(names(n_1000)), y = n_1000, col = "orange", pch = 2)
points(x = as.numeric(names(n_10000)), y = n_10000, col = "green", pch = 3)
legend("topright", pch = c(20,1,2,3), col = c(1,"red", "orange", "green"), 
       legend = c("exact", "n_sim = 100", "n_sim = 1000", "n_sim = 10000"), 
       bty = "n")

## ------------------------------------------------------------------------
# Plot chi-square distributions with nu = 5, 10, and 50 degrees of freedom
curve(dchisq(x = x, df = 5), xlim = c(0,90), 
      main = "Chi-square Distributions", lty = 1, col = 1, lwd = 2, 
      ylab = expression(f[x](x)))
# add nu = 10 distribution
curve(dchisq(x = x, df = 10), xlim = c(0,90), lty = 2, add = TRUE, 
      col = "blue", lwd = 2)

# add nu = 50 distribution
curve(dchisq(x = x ,df = 50), xlim = c(0,90), lty = 3, add = TRUE, 
      col = "magenta", lwd = 2)
legend("topright", legend = c("nu = 5", "nu = 10", "nu = 50"),
       lty = c(1,2,3), col = c(1,"blue","magenta"), lwd = c(2,2,2),
       bty = "n")

## note that you can do this with other distributions as well, just change 
# dchisq to the "d" function for the distribution your interested in, and 
# specify appropriate x limits given the support for the distribution! 

