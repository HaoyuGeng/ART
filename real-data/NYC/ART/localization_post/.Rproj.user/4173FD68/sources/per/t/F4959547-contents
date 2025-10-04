rm(list = ls())
set.seed(20250725)     
source("post_fun.R")
source("funs.R")
n=1826
inter_l = 90 
labels <- read.csv("cluster_labels.csv")[,2]
#####post changeforest#####
CF_tau_hat <- date_to_index(c("2019-04-01", "2020-06-25", "2021-09-01"))
post_result <- main_ART_post_fun(CF_tau_hat, labels, alpha0=0.1, inter_l)

print(post_result)
print(index_to_date(post_result))

