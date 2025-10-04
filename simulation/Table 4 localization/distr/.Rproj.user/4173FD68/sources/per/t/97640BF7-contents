library(MASS)
library(glmnet)
set.seed(20250710)
n=400
p=100
tau_all0 = c(0, 0.3, 0.6, 1)*n
Sigma0 = diag(1,p)
Sigma2 = matrix(0,p,p)
rho2 = 0.8
for (i_pos in 1:p) {
  for (j_pos in 1:p) {
    Sigma2[i_pos,j_pos] = rho2^(abs(i_pos-j_pos))
  }
}





