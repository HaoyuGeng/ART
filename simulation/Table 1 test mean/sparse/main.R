#####mean test#####
rm(list = ls())
library(foreach)
library(doParallel)
library(Matrix)


cl <- makeCluster(10)  
registerDoParallel(cl)

p_can = c(300,400)
s0_can = 0.1*p_can   

n_can <- c(50,100,150)    
result_size_Power <- NULL
error_type_can <- c("Gaussian","t")

for (kk in 1:length(error_type_can)) {
  error_type = error_type_can[kk]
  result_size_Power <- NULL
  
  for (pp in 1:length(p_can)) {
    p <- p_can[pp]
    s0 <- s0_can[pp]
    c <- 2.5/sqrt(s0)     
    
    for (k in 1:length(n_can)) {
      n <- n_can[k]
      
      source("mean_model_hd.R")
      source("mean fun.R")
      tau = 0.4
      alpha0 = 0.1
      lambda_s = floor(sqrt(p))
      rep_num = 1000
      
      print(paste("error_type=",error_type, "n=",n,"p=",p,"c=",c))

      ######ART######------------------------------
      RES <- foreach(i=1:rep_num,.combine = "rbind") %dopar%
        main_ART_fun(i, alpha0, error_type, lambda_s) 
      p_value <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
        if(is.numeric(RES[i,]$p_value)){RES[i,]$p_value}
      rejection <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
        if(is.numeric(RES[i,]$rejection)){RES[i,]$rejection}
      size_power_ART_dev <- colMeans(rejection)
      print(paste("ART",size_power_ART_dev))


      #########DMS######--------------------------------------------------------
      RES <- foreach(i=1:rep_num,.combine = "rbind") %dopar%
        main_mean_DMS(i, alpha0, error_type)
      p_value <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
        if(is.numeric(RES[i,]$p_value)){RES[i,]$p_value}
      rejection <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
        if(is.numeric(RES[i,]$rejection)){RES[i,]$rejection}
      size_power_DMS <- colMeans(rejection)
      print(paste("DMS",size_power_DMS))

      #########LZZL######-------------------------------------------------------
      RES <- foreach(i=1:rep_num,.combine = "rbind") %dopar%
        main_mean_LZZL(i, alpha0, error_type) 
      p_value <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
        if(is.numeric(RES[i,]$p_value)){RES[i,]$p_value}
      rejection <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
        if(is.numeric(RES[i,]$rejection)){RES[i,]$rejection}
      size_power_LZZL <- colMeans(rejection)
      print(paste("LZZL",size_power_LZZL))

      print("Finish")
      Method = c('ART_dev','DMS','LZZL')
      size_power = c(size_power_ART_dev, size_power_DMS,size_power_LZZL)
      result_size_Power.k <- cbind(Method=Method, n=rep(n,3), p=rep(p,3), c=rep(c,3), error_type=rep(error_type,3), size_power=size_power)
      result_size_Power <- rbind(result_size_Power,result_size_Power.k)
    }
  }
}


#write.csv(result_size_Power,"sparse.csv")











