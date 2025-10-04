#####exact test#####
rm(list = ls())
source("test_fun.R")
source("localization.R")
set.seed(20250106)  

n=150
tau_all = c(0,0.4,0.6,1)*n
inter_l = 0.2*n

cl_clustering0 <- read.csv("labels/label_0.csv")[,1]

p_value <- main_multi_test_fun(cl_clustering0)
print(p_value)

















