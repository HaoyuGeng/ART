library(MASS)
library(glmnet)
set.seed(20240801)
p=10  
s0=3
mu0 = rep(0,p)
delta1 = rep(0,p)
delta2 = rep(0,p)
delta3 = rep(0,p)
index1 = sample(c(1:p),s0)
index2 = sample(c(1:p),s0)
index3 = sample(c(1:p),s0)
delta1[index1] = rep(1,s0)*c
delta2[index2] = rep(1,s0)*c
delta3[index3] = rep(1,s0)*c
mu1 = mu0 + delta1
mu2 = mu1 + delta2
mu3 = mu2 + delta3
mu_all <- cbind(mu0,mu1,mu2,mu3)
tau_all = floor(0:4/4*n)    
Sigma_error = diag(1, p)





