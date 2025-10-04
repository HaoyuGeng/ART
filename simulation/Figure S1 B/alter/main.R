rm(list = ls())
library(foreach)
library(doParallel)
library(Matrix)

cl <- makeCluster(10) 
registerDoParallel(cl)
n = 200  
p = 100
s0 = 0.1*p
c = 1.5/sqrt(s0)   
tau = 0.5
n1 <- n*tau
n2 <- n*(1-tau)
result_size_Power <- NULL
error_type_can <- c("t","Gaussian")

for (kk in 1:length(error_type_can)) {
    error_type = error_type_can[kk]

    alpha0=0.1
    rep_num = 1000
    B_can <- c(50,100,150,200,250,300)
    
  for(bb in 1:length(B_can)){
    B <- B_can[bb]
    result_size_Power <- NULL
    
    for (rep_box in 1:100) {
    
        source("mean_model_hd.R")
        source("mean_fun_hd.R")
        
        print(paste("rep times=",rep_box,"error_type=",error_type, "n=",n,"p=",p,"c=",c))
        
        RES <- foreach(i=1:rep_num,.combine = "rbind") %dopar%
          tryCatch({ main_ART_fun(i, alpha0, error_type, floor(sqrt(p)))},
                   error=function(e) return(paste("sample",i,"caused the error")))
        print(RES)
        p_value <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
          if(is.numeric(RES[i,]$p_value)){RES[i,]$p_value}
        print(length(p_value))
        rejection <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
          if(is.numeric(RES[i,]$rejection)){RES[i,]$rejection}
        size_power_ART <- colMeans(rejection)
        size_power_ART <- data.frame(B=B,size_power_ART=size_power_ART, Error= error_type)
        print(size_power_ART)
        
        result_size_Power <- rbind(result_size_Power, size_power_ART)
    }
  }
}














