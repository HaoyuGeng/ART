#####exact test#####
rm(list = ls())
#library(RSpectra)
library(RcppCNPy)
library(InspectChangepoint)
set.seed(20250104)         
source("post_fun.R")

n=150
tau_all = c(0,0.4,0.6,1)*n
inter_l = 20*2

####383###-----------------------------------------------------------------------
cl_clustering383 <- read.csv("labels/label_383.csv")[,1]
####Inspect tau_hat#####
x <- npyLoad("datasets/383_selected_x_60_and_30_flat.npy")/255
y <- npyLoad("datasets/383_selected_y_60_and_30.npy")
matrix_normal <- matrix(rnorm(length(x)), nrow = nrow(x), ncol = ncol(x))
x = x+0.1*matrix_normal
p = dim(x)[2]
threshold <- compute.threshold(n,p,nrep=100,show_progress = F)
result <- inspect(t(x), threshold = 1*threshold)
cusum_value <- result$changepoints[,"max.proj.cusum"]
tau_hat_inspect <- result$changepoints[,"location"]
tau_hat <- find_max_positions(cusum_value,tau_hat_inspect,buffer = 20)  #
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
post_result <- main_ART_post_fun(tau_hat, tau_all, cl_clustering383, alpha0=0.1, inter_l)
print(post_result)  
post_tau_hat <- post_result$tau_hat_update
d_art <- pracma::hausdorff_dist(post_tau_hat,tau_all[2:3])
print(paste("hd_art=",d_art))






