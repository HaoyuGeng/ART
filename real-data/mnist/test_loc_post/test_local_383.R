#####exact test#####
rm(list = ls())
source("test_fun.R")
source("localization.R")
set.seed(20250106)  


n=150
tau_all = c(0,0.4,0.6,1)*n
inter_l = 0.2*n

####383###-----------------------------------------------------------------------
cl_clustering383 <- read.csv("labels/label_383.csv")[,1]
p_value <- main_multi_test_fun(cl_clustering383)
print(p_value)

result <- main_ART_cp_fun(cl_clustering383,"All")
print(result)
#coverage
COVERAGE <- coverage_fun(result$interval_select_all, tau_all )
#average correct_interval_num
CORRECT_INTERVAL_NUM <- correct_interval_num_fun(result$interval_select_all, tau_all)
#average internal num
INTERVAL_NUM <- dim(result$interval_select_all)[1]
#proportion of correct_interval
PROP_CORRECT_INTERVAL <- CORRECT_INTERVAL_NUM / INTERVAL_NUM
#average length of correct intervals
LENGTH_CORRECT_INTERVAL <- length_correct_interval_fun(result$interval_select_all, tau_all)
LENGTH_CORRECT_INTERVAL <- LENGTH_CORRECT_INTERVAL[LENGTH_CORRECT_INTERVAL!=0]
AV_LENGTH_CORRECT_INTERVAL <- mean(LENGTH_CORRECT_INTERVAL)
#estimation error of tau_hat
HD_tau_error <- pracma::hausdorff_dist(result$tau_hat_all, tau_all[2:3])
result_list <- list( INTERVAL_NUM=INTERVAL_NUM, CORRECT_INTERVAL_NUM=CORRECT_INTERVAL_NUM, PROP_CORRECT_INTERVAL=PROP_CORRECT_INTERVAL,
                     AV_LENGTH_CORRECT_INTERVAL=AV_LENGTH_CORRECT_INTERVAL, HD_tau_error=HD_tau_error )
#write.csv(result_list, file = paste0("ART 383 ", ".csv"))

