rm(list = ls())
library(foreach)
library(doParallel)
library(Matrix)

cl <- makeCluster(10) 
registerDoParallel(cl)

n <- 600
c_can <- c(1, 2)  
error_type_can <- c("Gaussian","t")
for (kk in 1:1) {
  
  result_size_Power <- NULL
  error_type = error_type_can[kk]
  
  for (k in 1:length(c_can)) {
    c <- c_can[k]
    alpha0=0.1
    inter_l = 0.1*n
    rep_num = 1000
    
    source("model_mean.R")
    source("mean_fun.R")
    
    print(paste("error_type=",error_type,"c=",c, "K=3"))
    
    ####ART##### 
    RES1 <- foreach(i=1:rep_num,.combine = "rbind") %dopar%
      tryCatch({main_ART_fun(i, alpha0, error_type, inter_l)},
               error=function(e) return(paste("sample",i,"caused the error")))
    F_rej <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
      if(is.numeric(RES1[i,]$false_rejection)){RES1[i,]$false_rejection}
    FWER <- sum(F_rej) / rep_num
    correct_tau_num <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
      if(is.numeric(RES1[i,]$correct_tau_num)){RES1[i,]$correct_tau_num}

    correct_rejection <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
      if(is.numeric(RES1[i,]$correct_rejection)){RES1[i,]$correct_rejection}
    Power <- sum(correct_rejection) / sum(correct_tau_num)
    print(paste("ART FWER", FWER))
    print(paste("ART Power", Power))
    result_size_Power.k1 <- cbind(Method= "ART", n=n, c=c, error_type=error_type, FWER=FWER, Power=Power)
 
      
    ###TUNE-Wald#####
    RES1 <- foreach(i=1:rep_num,.combine = "rbind") %dopar%
      main_TUNE_wald_mean_fun(i, alpha0, error_type, inter_l)
    F_rej <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
      if(is.numeric(RES1[i,]$false_rejection)){RES1[i,]$false_rejection}
    FWER <- sum(F_rej) / rep_num
    correct_tau_num <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
      if(is.numeric(RES1[i,]$correct_tau_num)){RES1[i,]$correct_tau_num}
    correct_rejection <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
      if(is.numeric(RES1[i,]$correct_rejection)){RES1[i,]$correct_rejection}
    Power <- sum(correct_rejection) / sum(correct_tau_num)
    print(paste("TUNE.wald FWER", FWER))
    print(paste("TUNE.wald Power", Power))
    result_size_Power.k2 <- cbind(Method= "TUNE.Wald", n=n, c=c, error_type=error_type, FWER=FWER, Power=Power)


    ###TUNE-score#####
    RES1 <- foreach(i=1:rep_num,.combine = "rbind") %dopar%
      main_TUNE_score_mean_fun(i, alpha0, error_type, inter_l)
    F_rej <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
      if(is.numeric(RES1[i,]$false_rejection)){RES1[i,]$false_rejection}
    FWER <- sum(F_rej) / rep_num
    correct_tau_num <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
      if(is.numeric(RES1[i,]$correct_tau_num)){RES1[i,]$correct_tau_num}
    correct_rejection <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
      if(is.numeric(RES1[i,]$correct_rejection)){RES1[i,]$correct_rejection}
    Power <- sum(correct_rejection) / sum(correct_tau_num)
    print(paste("TUNE.score FWER", FWER))
    print(paste("TUNE.score Power", Power))
    result_size_Power.k3 <- cbind(Method= "TUNE.score", n=n, c=c, error_type=error_type, FWER=FWER, Power=Power)
    result_size_Power <- rbind(result_size_Power,result_size_Power.k1,result_size_Power.k2, result_size_Power.k3)
  }
  print(paste("finish", error_type))
}


stopCluster(cl)





