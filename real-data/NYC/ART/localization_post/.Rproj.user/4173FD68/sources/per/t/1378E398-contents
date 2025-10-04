rm(list = ls())
set.seed(20250725)     
source("post_fun.R")
source("funs.R")
n=1826
inter_l = 90 
labels <- read.csv("cluster_labels.csv")[,2]
#####post changeAUC#####
changeAUC_tau_hat <- date_to_index(c("2019-01-31","2019-08-31","2020-03-17", "2020-11-29", "2021-10-31","2022-04-03"))
post_result <- main_ART_post_fun(changeAUC_tau_hat, labels, alpha0=0.1, inter_l)

print(post_result)
print(index_to_date(post_result))

