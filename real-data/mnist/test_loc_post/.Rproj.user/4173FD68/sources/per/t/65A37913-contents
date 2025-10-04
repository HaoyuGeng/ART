rm(list = ls())
source("test_fun.R")
source("localization.R")
set.seed(20250106)

####0to5####
n=600
tau_all = (0:6)/6*n
inter_l = 30

cl_clustering05 <- read.csv("labels/label_0to5.csv")[,1]

p_value <- main_multi_test_fun(cl_clustering05,inter_l)
print(p_value)


result <- main_ART_cp_fun(cl_clustering05,"All")
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
HD_tau_error <- pracma::hausdorff_dist(result$tau_hat_all, tau_all[2:6])
result_list <- list( INTERVAL_NUM=INTERVAL_NUM, CORRECT_INTERVAL_NUM=CORRECT_INTERVAL_NUM, PROP_CORRECT_INTERVAL=PROP_CORRECT_INTERVAL,
                     AV_LENGTH_CORRECT_INTERVAL=AV_LENGTH_CORRECT_INTERVAL, HD_tau_error=HD_tau_error )
#write.csv(result_list, file = paste0("ART 0to5 ", ".csv"))



