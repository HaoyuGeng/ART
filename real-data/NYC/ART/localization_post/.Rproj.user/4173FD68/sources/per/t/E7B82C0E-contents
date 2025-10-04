rm(list = ls())
set.seed(20250728)    
source("post_fun.R")
source("funs.R")
n=1826
inter_l = 90  
labels <- read.csv("cluster_labels.csv")[,2]
#####Inspect#####
library(InspectChangepoint)
heatmaps_array <- as.matrix(readRDS("heatmaps_array.rds"))
n <- 1826
p <- dim(heatmaps_array)[2]
matrix_normal <- matrix(rnorm(length(heatmaps_array)), nrow = nrow(heatmaps_array), ncol = ncol(heatmaps_array))
heatmaps_array = heatmaps_array+0.1*matrix_normal
threshold <- 25.91588 #compute.threshold(n,p)
result <- inspect(t(heatmaps_array), threshold = threshold, missing_data='meanImpute')
cusum_value <- result$changepoints[,"max.proj.cusum"]
tau_hat_inspect <- result$changepoints[,"location"]
tau_hat <- find_max_positions(cusum_value,tau_hat_inspect,buffer = inter_l/2)  
inspect_tau_hat <- tau_hat[tau_hat>(40) & tau_hat<(n-40)]

post_result <- main_ART_post_fun(inspect_tau_hat, labels, alpha0=0.1, inter_l)
print(post_result)  
print(index_to_date(post_result))

