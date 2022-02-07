#################################
### STAT 422 HW2 R code      ####
#################################

# plot the parent distribution X ~ Exp(3) from which the RS was taken
# note the R parameterization uses 1/beta for the rate parameter - this is 
# different from the back of your book. Think about what each piece 
# of the curve function does!
curve(dexp(x, rate = 1/3), from = 0, to = 20, 
      ylab = expression(f[X](x)), 
      main = "pdf of X ~ exponential(3)")

# to load the functions below, highlight all of the code and run it 
# in the R console. That is, take you cursor and highlight from line 
# 17 - 27 and run all of that code. 

# function for density of X(n) for a RS of size n from X ~ Exp(3)
max_exp <- function(x, n){
  fxn = (n/3)*(1-exp(-x/3))^{n-1}*exp(-x/3)
  return(fxn)
}

# function for density of X(1) for a RS of size n from X ~ Exp(3)
min_exp <- function(x, n){
  fx1 = (n/3)*exp(-(n*x)/3)
  return(fx1)
}

# now you can use the functions! 
# for example to find the density for X_(1) evaluated at 12 (f_X(1)(12)) when n = 2
# run the follwoing, does this value make sense? try some other values 
# and other sample sizes! 
min_exp(12, n = 2)

# similarly the density for X_(n) evaluated at 12 (f_X(n)(12)) when n = 2
# run the follwoing, does this value make sense? try some other values and other 
# sample sizes! 
max_exp(12, n = 2)

# the sampling distributions for the two order statistics are defined by the 
# densities evaluated at all potential values of X_(1) and X_(n) (0,infinity)
# use the density functions to plot the sampling distributions for both order 
# stats for the two different n. 

# split the plotting window in to two columns
par(mfrow = c(1,2))

# use the curve function and the min_exp function to 
# plot one of the desired pdfs for one of the ns...
# you'll have to fill in what pdf and what n is! 
curve(min_exp(x, n = 2), from = 0, to = 50, 
      ylab = "",
      main = "",
      lwd = 2)

# add the next density (note, add = T in the curve function)
curve(min_exp(x, n = 100), from = 0, to = 50, 
      add = T, lty = 2, lwd = 2)

# add another
curve(max_exp(x, n = 2), from = 0, to = 50,
      ylab = "",
      main = "", lwd = 2)

# add the last density
curve(max_exp(x, n = 100), from = 0, to = 50,
      add = T, lty = 2, lwd = 2)

##############################################
## functions for comparing many different n ##
##############################################

# again, highlight the whole function before you use it!
# for now, don't worry too much about the inside of these 
# functions...

# function for samplind dist of X(n) for a RS of size n from X ~ Exp(3)
plot_max_exp <- function(n_vec){
  # grab the length of the vector with the sample sizes of interest
  num_n <- length(n_vec)
  # create colors for the different distributions
  gray <- gray.colors(n = num_n, start = 0.8, end = 0)
  # initialize a plotting window with distribuiton of X_(n) 
  # for first n of interest
  curve(max_exp(x, n = n_vec[1]), from = 0, to = 50,
        ylab = expression(f[X[(n)]](x)), col = gray[1],
        main = "Density of RV X_(n); X ~ Exp(3)", lwd = 2)
  # add the rest of the densities for different n to the plot
  for(i in 2:num_n){
    curve(max_exp(x, n = n_vec[i]), from = 0, to = 50,
          col = gray[i], add = T, lwd = 2)
  }
  legend("topright", legend = paste0("n = ", n_vec), 
         col = gray, lwd = rep(2, num_n), cex = 0.85, bty = "n")
}

# function for samplind dist of X(1) for a RS of size n from X ~ Exp(3)
plot_min_exp <- function(n_vec){
  # grab the length of the vector with the sample sizes of interest
  num_n <- length(n_vec)
  # create colors for the different distributions
  gray <- gray.colors(n = num_n, start = 0.8, end = 0)
  # initialize a plotting window with distribuiton of X_(n) 
  # for first n of interest
  curve(min_exp(x, n = n_vec[1]), from = 0, to = 50,
        ylab = expression(f[X[(1)]](x)), col = gray[1],
        main = "Density of RV X_(1); X ~ Exp(3)", lwd = 2)
  # add the rest of the densities for different n to the plot
  for(i in 2:num_n){
    curve(min_exp(x, n = n_vec[i]), from = 0, to = 50,
          col = gray[i], add = T, lwd = 2)
  }
  legend("topright", legend = paste0("n = ", n_vec), 
         col = gray, lwd = rep(2, num_n), cex = 0.85, bty = "n")
}

# to use the function you will need to feed it a vector of 
# different sample sizes, so
# create a vector of sample sizes you are interested in 
# investigating 
n_vec <- c(2, 5, 10, 25, 50, 100, 200, 500, 1000)

# now use the function to compare the sampling distribution for 
# X_(n) for many different n! Try your own values!
plot_max_exp(n_vec)

# now use the function to compare the sampling distribution for 
# X_(1) for many different n! Try your own values!
plot_min_exp(n_vec)