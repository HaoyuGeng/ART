rm(list = ls())
library(MASS)
library(KernSmooth)
set.seed(20250821)
p <- 200
s0=0.1*p
c= 2/sqrt(s0)
n <- 1000     
n1 <- 50
M <- 3000    
source("reg_3dep/model.R")
compute_f_rank <- function(X) {
  n <- length(X)   
  R <- rank(X)   
  R_bar <- (n + 1) / 2  
  cum_sum <- cumsum(R - R_bar)  
  max_deviation <- max(abs(cum_sum))  
  result <- max_deviation / n^(3/2)
  return(result)
}



f_X <- numeric(M)
f_Y <- numeric(M)


tau=0.5
X1 <- mvrnorm(tau*n,rep(0,p),Sigma0)
X2 <- mvrnorm((1-tau)*n,rep(0,p),Sigma0)
Z <- rnorm(n)
error <- rnorm(n)
for (t in 1:n) {
  z_t     <- Z[t]
  z_t1    <- if (t - 1 >= 1) Z[t - 1] else 0
  z_t2    <- if (t - 2 >= 1) Z[t - 2] else 0
  z_t3    <- if (t - 3 >= 1) Z[t - 3] else 0
  error[t] <- (1*z_t + 0.1 * z_t1 - 0.1 * z_t2 + 0.1 * z_t3) 
}
y1 <- 0 + X1 %*% beta1 
y2 <- 0 + X2 %*% beta2  
X <- rbind(X1,X2) #n x p
y <- rbind(y1,y2) + error
beta_can <- glmnet(X,y,family = "gaussian",lambda = sqrt(log(p)/n))$beta
Y_sample <- abs(y - X %*% beta_can)


for (m in 1:M) {
  indices <- sample(1:(n-n1), 1)   
  X_sample <- rnorm(n)
  
  f_X[m] <- compute_f_rank(X_sample[indices:(indices+n1)])
  f_Y[m] <- compute_f_rank(Y_sample[indices:(indices+n1)])
}


bw <- 0.05
kde_X <- bkde(f_X, bandwidth = bw)
kde_Y <- bkde(f_Y, bandwidth = bw)


grid <- sort(unique(c(kde_X$x, kde_Y$x)))

p_X <- approx(kde_X$x, kde_X$y, xout = grid, rule = 2)$y
p_Y <- approx(kde_Y$x, kde_Y$y, xout = grid, rule = 2)$y

delta_x <- mean(diff(grid))
TV <- 0.5 * sum(abs(p_X - p_Y)) * delta_x

cat("Estimated Total Variation distance between f(X) and f(Y):", TV, "\n")
TV_est <- TV   


