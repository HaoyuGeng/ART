rm(list = ls())
source("test_fun.R")
source("localization.R")
source("funs.R")
seed_num = 20250726 
print(seed_num)
set.seed(seed_num)   
n=1826
labels <- read.csv("cluster_labels.csv")[,2]
result <- main_ART_cp_fun(labels,"seed")
print(result$tau_hat_all)
print(index_to_date(result$tau_hat_all))
print(cbind(result$interval_select_all[,1] - result$tau_hat_all, result$interval_select_all[,2] - result$tau_hat_all))


