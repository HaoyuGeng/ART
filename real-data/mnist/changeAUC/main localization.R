rm(list = ls())
source("get_multiple_change_point_v1.R")
source("post_fun.R")
set.seed(20250806)   


###0to5###---------------------------
data <- read.csv("0to5_selected_x_100.csv", header = FALSE)  
n <- dim(data)[1]
tau_all = seq(100,500,100)

changeAUC_result <- get_multiple_change_point(
  sample_ = data,
  left = 1,
  right = nrow(data),
  classifier = "RF",
  split_trim = 0.15,
  auc_trim = 0.05,
  no_of_perm = 99,
  min_length = 100,
  decay = sqrt(2),
  return_output = "all"
)


tau_hat <- c(100, 202, 123, 302, 401, 509) 
inter_l = 20*2
tau_hat <- tau_hat[tau_hat>(inter_l/2) & tau_hat<(n-inter_l/2)]
true_tau_hat.num <- sapply(1:length(tau_hat), function(i){
  sum( (sum( ((tau_hat[i] - inter_l/2) < tau_all) & ((tau_hat[i] + inter_l/2) > tau_all) ) >0) +0 )
})
true_tau_hat.num = sum(true_tau_hat.num)
d_cAUC <- pracma::hausdorff_dist(tau_hat,tau_all)
print(paste("tau_hat=",tau_hat))
print(paste("true_tau_hat.num=",true_tau_hat.num))
print(paste("tau_hat.num=",length(tau_hat)))
print(paste("hd_cAUC=",d_cAUC))


#####post#####
cl_clustering05 <- read.csv("label_0to5.csv")[,1]
post_result <- main_ART_post_fun(tau_hat, tau_all, cl_clustering05, alpha0=0.1, inter_l)
print(post_result)
post_tau_hat <- post_result$tau_hat_update
d_art <- pracma::hausdorff_dist(post_tau_hat,tau_all)
print(paste("hd_art=",d_art))




###123###---------------------------
data <- read.csv("123_selected_x_60_and_30.csv", header = FALSE)  
n <- dim(data)[1]
tau_all = c(60,90)

changeAUC_result <- get_multiple_change_point(
  sample_ = data,
  left = 1,
  right = nrow(data),
  classifier = "RF",
  split_trim = 0.15,
  auc_trim = 0.05,
  no_of_perm = 99,
  min_length = 30,
  decay = sqrt(2),
  return_output = "all"
)


tau_hat <- c(10, 59, 95, 129) 

inter_l = 20*2
tau_hat <- tau_hat[tau_hat>(inter_l/2) & tau_hat<(n-inter_l/2)]
true_tau_hat.num <- sapply(1:length(tau_hat), function(i){
  sum( (sum( ((tau_hat[i] - inter_l/2) < tau_all) & ((tau_hat[i] + inter_l/2) > tau_all) ) >0) +0 )
})
true_tau_hat.num = sum(true_tau_hat.num)
d_cAUC <- pracma::hausdorff_dist(tau_hat,tau_all)
print(paste("tau_hat=",tau_hat))
print(paste("true_tau_hat.num=",true_tau_hat.num))
print(paste("tau_hat.num=",length(tau_hat)))
print(paste("hd_cAUC=",d_cAUC))


#####post#####
cl_clustering123 <- read.csv("label_123.csv")[,1]
post_result <- main_ART_post_fun(tau_hat, tau_all, cl_clustering123, alpha0=0.1, inter_l)
print(post_result)
post_tau_hat <- post_result$tau_hat_update
d_art <- pracma::hausdorff_dist(post_tau_hat,tau_all)
print(paste("hd_art=",d_art))






###383###---------------------------
data <- read.csv("383_selected_x_60_and_30.csv", header = FALSE)  
n <- dim(data)[1]
tau_all = c(60,90)

changeAUC_result <- get_multiple_change_point(
  sample_ = data,
  left = 1,
  right = nrow(data),
  classifier = "RF",       
  split_trim = 0.15,
  auc_trim = 0.05,
  no_of_perm = 99,         
  min_length = 30,        
  decay = sqrt(2),
  return_output = "all"
)


tau_hat <- c(8, 60, 91, 134) 

inter_l = 20*2
tau_hat <- tau_hat[tau_hat>(inter_l/2) & tau_hat<(n-inter_l/2)]
true_tau_hat.num <- sapply(1:length(tau_hat), function(i){
  sum( (sum( ((tau_hat[i] - inter_l/2) < tau_all) & ((tau_hat[i] + inter_l/2) > tau_all) ) >0) +0 )
})
true_tau_hat.num = sum(true_tau_hat.num)
d_cAUC <- pracma::hausdorff_dist(tau_hat,tau_all)
print(paste("tau_hat=",tau_hat))
print(paste("true_tau_hat.num=",true_tau_hat.num))
print(paste("tau_hat.num=",length(tau_hat)))
print(paste("hd_cAUC=",d_cAUC))


#####post#####
cl_clustering383 <- read.csv("label_383.csv")[,1]
post_result <- main_ART_post_fun(tau_hat, tau_all, cl_clustering383, alpha0=0.1, inter_l)
print(post_result)
post_tau_hat <- post_result$tau_hat_update
d_art <- pracma::hausdorff_dist(post_tau_hat,tau_all)
print(paste("hd_art=",d_art))




