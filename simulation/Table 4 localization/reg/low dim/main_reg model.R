library(MASS)
library(glmnet)
set.seed(20250710)
n=200 #400
p=5 #10
s0=3
c=2
tau_all0 = c(0, 0.3, 0.6, 1)*n
beta1 = rep(0,p)
beta1[1:5] = c(1,-1,1,-1,1)*c
delta = rep(0,p)
index = sample(c(1:p),s0,replace = FALSE)
delta[index] = sample(c(1,-1),s0,replace = TRUE)*c
beta2 = beta1 + delta
beta3 = beta2 + delta
beta_all = cbind(beta1, beta2, beta3)
Sigma0 = matrix(0,p,p)
rho = 0.3
for (i_pos in 1:p) {
  for (j_pos in 1:p) {
    Sigma0[i_pos,j_pos] = rho^(abs(i_pos-j_pos))
  }
}





