######setting######
library(MASS)
library(glmnet)
set.seed(20250101)
beta1 = rep(0,p)
beta1[1:5] = c(1,-1,1,-1,1) 
delta = rep(0,p)
index = sample(1:p,s0,replace = F)
delta[index] = sample(c(-1,1),s0,replace = T) * c
beta2 = beta1 + delta
Sigma0 = matrix(0,p,p)
rho = 0.3
for (i_pos in 1:p) {
  for (j_pos in 1:p) {
    Sigma0[i_pos,j_pos] = rho^(abs(i_pos-j_pos))
  }
}


















