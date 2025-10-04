#####reg test#####
rm(list = ls())
library(foreach)
library(doParallel)
library(Matrix)

cl <- makeCluster(20) 
registerDoParallel(cl)

p_can = c(100,200) 
n_can = c(200,300)   
s0_can = c(10,10) 
c_can = 0 
error_type_can <- c("Gaussian","t")

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
        tau = 0.4
        rep_num = 1000
        
        source("model.R")
        source("functions.R")
        print(paste("error_type=",error_type, "n=",n,"p=",p,"c=",c))
        
        #main ART
        RES <- foreach(i=1:rep_num,.combine = "rbind") %dopar%
          main_ART_fun(i, alpha0, error_type)
        p_value <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
          if(is.numeric(RES[i,]$p_value)){RES[i,]$p_value}
        rejection <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
          if(is.numeric(RES[i,]$rejection)){RES[i,]$rejection}
        size_power_de <- colMeans(rejection)
        print(paste("ART",size_power_de))
        
        #main QFCUSUM
        RES <- foreach(i=1:rep_num,.combine = "rbind") %dopar%
          QFCUSUM(i, alpha0, error_type)
        p_value <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
          if(is.numeric(RES[i,]$p_value)){RES[i,]$p_value}
        rejection <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
          if(is.numeric(RES[i,]$rejection)){RES[i,]$rejection}
        size_power_QFCUSUM <- colMeans(rejection)
        print(paste("QFCUSUM",size_power_QFCUSUM))

        
        result_size_Power_de <- data.frame(Method= c("ART","QF"), n=rep(n,2), c=rep(c,2), p=rep(p,2),
                                           error_type=rep(error_type,2), size_power=c(size_power_de,size_power_QFCUSUM))

        result_size_Power <- rbind(result_size_Power,result_size_Power_de)
        print("Finish")
        
      }
    }
  }
}

stopCluster(cl)





