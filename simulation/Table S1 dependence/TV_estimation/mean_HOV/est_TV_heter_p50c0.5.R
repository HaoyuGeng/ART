rm(list = ls())
library(MASS)
library(KernSmooth)
set.seed(20250804)
p <- 50
s0 = 0.1*p
c = 0.5/sqrt(s0)   
n <- 1000    
n1 <- 50
M <- 1000     

source("mean_HOV/mean_model_hd.R")
compute_f_rank <- function(X) {
  n <- length(X)   
  R <- rank(X, ties.method = "random")   
  R_bar <- (n + 1) / 2   
  cum_sum <- cumsum(R - R_bar) 
  max_deviation <- max(abs(cum_sum)) 
  result <- max_deviation / n^(3/2)
  return(result)
}


f_X <- numeric(M)
f_Y <- numeric(M)

tau=0.5
error <- matrix(0,nrow=p,ncol=n)
for (i in 1:p) {
  error_groups <- rep((1:n)/n, length.out = n)  
  error[i,] <- rnorm(n, mean = 0, sd = error_groups)
}
y0 <- matrix(rep(mu0,tau*n), ncol = tau*n, byrow = F)
y1 <- matrix(rep(mu1,(1-tau)*n), ncol = (1-tau)*n, byrow = F)
y <- cbind(y0,y1) + error ##y:p x n
v1 <- sparsepca::spca(t(y), k = 1, alpha = 0, beta = 0,
                      center = FALSE, scale = FALSE, verbose = 0
)$loadings[, 1]
Y_sample <- v1%*%y


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

