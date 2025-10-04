rm(list = ls())
source("test_fun.R")
source("funs.R")
seed_num = 20250726 
print(seed_num)
set.seed(seed_num)   
n=1826
labels <- read.csv("cluster_labels.csv")[,2]
inter_l = 90
p_value <- main_multi_test_fun(labels,inter_l)
print(p_value)
