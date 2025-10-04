library(MASS)
library(glmnet)
set.seed(20240801)
mu0 = rep(0,p)
mu1 = rep(0,p)
mu2 = rep(0,p)
index <- sample(c(1:p),s0)
index <- sort(index)
mu1[index] = sample(c(1,-1),s0,replace = T)* rep(1,s0)*c
tau_all <- c(0, 0.3, 0.6, 1) 
Sigma_error = diag(1, p)




