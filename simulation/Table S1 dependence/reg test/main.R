rm(list = ls())
library(foreach)
library(doParallel)
library(Matrix)

cl <- makeCluster(10) 
registerDoParallel(cl)

p_can = c(100,200,300)
n_can = 1000 
s0_can = 0.1*p_can
c_can = c(0,1,2) 


error_type_can <- c("ARMA22","3-dep","heter")

result_all <- NULL
for (pp in 1:length(p_can)) {
  p <- p_can[pp]
  s0 <- s0_can[pp]

  for (kk in 1:length(error_type_can)) {
    result_size_Power <- NULL
    error_type = error_type_can[kk]
    
    for (cc in 1:length(c_can)) {
      c <- c_can[cc]
      c <- c/sqrt(s0)
  
      for (rr in 1:length(n_can)) {
        n=n_can[rr]
        alpha0=0.1
        tau = 0.5
        rep_num = 1000
        
        source("model.R")
        source("functions.R")
        print(paste("error_type=",error_type, "n=",n,"p=",p,"c=",c))
        
        
        if(error_type=="ARMA22"){
          TV_est_list_reg <- readRDS("TV_est_list_reg_ARMA.rds")
        }else if(error_type=="3dep"){
          TV_est_list_reg <- readRDS("TV_est_list_reg_3dep.rds")
        }else if(error_type=="HOV"){
          TV_est_list_reg <- readRDS("TV_est_list_re_HOV.rds")
        }
        
        TV_est <- TV_est_list_reg$TV[which(TV_est_list_reg$p == p_can[pp] & TV_est_list_reg$c == c_can[cc])]
        alpha0_new = alpha0 - TV_est
        print(alpha0_new)
        

        #main ART
        RES <- foreach(i=1:rep_num,.combine = "rbind") %dopar%
          tryCatch({main_ART_fun(i, alpha0, alpha0_new, error_type)},
                   error=function(e) return(paste("sample",i,"caused the error")))
        p_value <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
          if(is.numeric(RES[i,]$p_value)){RES[i,]$p_value}
        rejection <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
          if(is.numeric(RES[i,]$rejection)){RES[i,]$rejection}
        size_power_ART <- colMeans(rejection)
        print(paste("ART",size_power_ART))
        
        rejection_new <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
          if(is.numeric(RES[i,]$rejection_new)){RES[i,]$rejection_new}
        size_power_ART_new <- colMeans(rejection_new)
        print(paste("ART adjust",size_power_ART_new))

        print("Finish")
        
        result <- data.frame(error_type=error_type_can[kk],p=p,c=c_can[cc], ART = size_power_ART, ART.a = size_power_ART_new)
        result_all <- rbind(result_all,result)
        
      }
    }
  }
}


stopCluster(cl)





