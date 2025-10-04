#####exact test#####
rm(list = ls())
#library(RSpectra)
library(RcppCNPy)
library(InspectChangepoint)
set.seed(20250106)    

source("post_fun.R")

n=150
tau_all = c(0,0.4,0.6,1)*n

####123###-----------------------------------------------------------------------
cl_clustering05 <- read.csv("labels/label_123.csv")[,1]
inter_l = 40
####CF#####
tau_hat <- sort(c(60,90))
tau_hat <- sort(tau_hat)

tau_hat <- tau_hat[tau_hat>(inter_l/2) & tau_hat<(n-inter_l/2)]
true_tau_hat.num <- sapply(1:length(tau_hat), function(i){
  sum( (sum( ((tau_hat[i] - inter_l/2) < tau_all) & ((tau_hat[i] + inter_l/2) > tau_all) ) >0) +0 )
})
true_tau_hat.num = sum(true_tau_hat.num)
d_inspect <- pracma::hausdorff_dist(tau_hat,tau_all[2:3])
print(paste("tau_hat=",tau_hat))
print(paste("true_tau_hat.num=",true_tau_hat.num))
print(paste("tau_hat.num=",length(tau_hat)))
print(paste("hd_inspect=",d_inspect))

#####post#####
post_result <- main_ART_post_fun(tau_hat, tau_all, cl_clustering05, alpha0=0.1, inter_l)
print(post_result)  
post_tau_hat <- post_result$tau_hat_update
d_art <- pracma::hausdorff_dist(post_tau_hat,tau_all[2:3])
print(paste("hd_art=",d_art))






