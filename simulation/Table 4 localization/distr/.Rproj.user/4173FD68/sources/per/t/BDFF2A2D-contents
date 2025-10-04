rm(list = ls())

library(not)
library(foreach)
library(doParallel)
library(Matrix)
library(CPAT)
library(nsp)
library(pracma)

cl <- makeCluster(20)
registerDoParallel(cl)

rep_num =  1000
error_type_all <- c("Alter1","Alter2")
inter_type_all <- c("seed")

for (ee in 1:length(error_type_all)) {
  for (ii in 1:length(inter_type_all)) {
    source("main_functions.R")
    RES <- foreach(i=1:rep_num,.combine = "rbind") %dopar%
      main_ART_fun(i, error_type_all[ee],inter_type_all[ii])
    RES_ind <- sapply(1:rep_num, function(i){
      is.null(RES[i,"interval_select_all"][[1]])
    })
    RES_ind <- which(RES_ind==F)
    #####measurement#####
    source("main_dist model.R")
    #coverage
    COVERAGE <- foreach(i=RES_ind,.combine = "rbind") %dopar%
      coverage_fun(RES[i,"interval_select_all"], tau_all0)
    COVERAGE_rate <- 1 - sum(COVERAGE==0)/ rep_num
    #average correct_interval_num
    CORRECT_INTERVAL_NUM <- foreach(i=RES_ind,.combine = "rbind") %dopar%
      correct_interval_num_fun(RES[i,"interval_select_all"], tau_all0)
    AV_CORRECT_INTERVAL_NUM <- mean(CORRECT_INTERVAL_NUM)
    #average internal num
    INTERVAL_NUM <- foreach(i=RES_ind,.combine = "rbind") %dopar%
      dim(RES[i,"interval_select_all"][[1]])[1]
    AV_INTERVAL_NUM <- mean(INTERVAL_NUM)
    #proportion of correct_interval
    PROP_CORRECT_INTERVAL <- mean(CORRECT_INTERVAL_NUM / INTERVAL_NUM)
    #average length of correct intervals
    LENGTH_CORRECT_INTERVAL <- foreach(i=RES_ind,.combine = "rbind") %dopar%
      length_correct_interval_fun(RES[i,"interval_select_all"], tau_all0)
    LENGTH_CORRECT_INTERVAL <- LENGTH_CORRECT_INTERVAL[LENGTH_CORRECT_INTERVAL!=0]
    AV_LENGTH_CORRECT_INTERVAL <- mean(LENGTH_CORRECT_INTERVAL)
    #estimation error of tau_hat
    HD_tau_error <- foreach(i=RES_ind,.combine = "rbind") %dopar%
      pracma::hausdorff_dist(RES[i,"tau_hat_all"][[1]], tau_all0[2:3])
    AV_HD_tau_error <-mean(HD_tau_error)

    print(paste0(error_type_all[ee],".",inter_type_all[ii]," p=", p))
    print(paste("FWER",1-COVERAGE_rate))
    print(paste("PROP_CORRECT_INTERVAL=",PROP_CORRECT_INTERVAL))
    print(paste("AV_CORRECT_INTERVAL_NUM=", AV_CORRECT_INTERVAL_NUM))
    print(paste("AV_INTERVAL_NUM=",AV_INTERVAL_NUM))
    print(paste("AV_LENGTH_CORRECT_INTERVAL=",AV_LENGTH_CORRECT_INTERVAL))
    print(paste("AV_estimation error of tau_hat",AV_HD_tau_error))
  }
}

stopCluster(cl)





