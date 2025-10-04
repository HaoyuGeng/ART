rm(list = ls())
source("get_multiple_change_point_v1.R")
source("post_fun.R")
set.seed(20250806)   


###0to5###---------------------------
data <- read.csv("0to5_selected_x_100.csv", header = FALSE)  # header=TRUE 
cAUC_test_result <- get_change_point(data, 
                 classifier = "RF",    
                 split_trim = 0.15, 
                 auc_trim = 0.05,
                 perm_pval = TRUE,     
                 no_of_perm = 199, 
                 tau = 0.5, 
                 verbose = TRUE)
print(cAUC_test_result$pval)


###123###---------------------------
data <- read.csv("123_selected_x_60_and_30.csv", header = FALSE)  # header=TRUE 
cAUC_test_result <- get_change_point(data, 
                                     classifier = "RF",    
                                     split_trim = 0.15, 
                                     auc_trim = 0.05,
                                     perm_pval = TRUE,     
                                     no_of_perm = 199, 
                                     tau = 0.5, 
                                     verbose = TRUE)

print(cAUC_test_result$pval)


###383###---------------------------
data <- read.csv("383_selected_x_60_and_30.csv", header = FALSE)  # header=TRUE 
cAUC_test_result <- get_change_point(data, 
                                     classifier = "RF",    
                                     split_trim = 0.15, 
                                     auc_trim = 0.05,
                                     perm_pval = TRUE,     
                                     no_of_perm = 199, 
                                     tau = 0.5, 
                                     verbose = TRUE)

print(cAUC_test_result$pval)




###0###---------------------------
data <- read.csv("0_selected_x_150.csv", header = FALSE)  # header=TRUE 
cAUC_test_result <- get_change_point(data, 
                                     classifier = "RF",    
                                     split_trim = 0.15, 
                                     auc_trim = 0.05,
                                     perm_pval = TRUE,     
                                     no_of_perm = 199, 
                                     tau = 0.5, 
                                     verbose = TRUE)

print(cAUC_test_result$pval)

