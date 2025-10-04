library(MASS)
library(glmnet)
#set.seed(20240801)
p=100
s0=0.1*p
mu0 = rep(0,p)
mu1 = rep(0,p)
index <- sample(c(1:p),s0)
index <- sort(index)
mu1[index] = rep(1,s0)*c


Sigma_error = diag(0.25, p)










