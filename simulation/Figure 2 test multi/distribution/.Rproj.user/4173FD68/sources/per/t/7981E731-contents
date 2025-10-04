rm(list = ls())
library(foreach)
library(doParallel)
library(Matrix)
library(CPAT)

cl <- makeCluster(10)  
registerDoParallel(cl)

p = 10 
n <- 400  
result_size_Power <- NULL
error_type = "case1"  #case1 case2 case3 

result_size_Power <- NULL
source("dist_model_hd.R")
source("fun.R")
alpha0=0.1
rep_num = 1000

print(paste("error_type=",error_type, "n=",n,"p=",p))

####ART
RES <- foreach(i=1:rep_num,.combine = "rbind") %dopar%
  tryCatch({ main_ART_fun(i, alpha0, error_type, lambda_s)},
           error=function(e) return(paste("sample",i,"caused the error")))
p_value <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
  if(is.numeric(RES[i,]$p_value)){RES[i,]$p_value}
rejection <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
  if(is.numeric(RES[i,]$rejection)){RES[i,]$rejection}
size_power_ART_dev <- colMeans(rejection)
print(paste("ART",size_power_ART_dev))

#ECP
RES <- foreach(i=1:rep_num,.combine = "rbind") %dopar%
  tryCatch({ main_ECP_fun(i, alpha0, error_type)},
           error=function(e) return(paste("sample",i,"caused the error")))
p_value <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
  if(is.numeric(RES[i,]$p_value)){RES[i,]$p_value}
rejection <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
  if(is.numeric(RES[i,]$rejection)){RES[i,]$rejection}
size_power_ECP <- colMeans(rejection)
print(paste(error_type,"ECP", size_power_ECP))

#changeAUC
RES <- foreach(i=1:rep_num,.combine = "rbind") %dopar%
  main_changeAUC_fun(i, alpha0, error_type)
p_value <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
  if(is.numeric(RES[i,]$p_value)){RES[i,]$p_value}
rejection <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
  if(is.numeric(RES[i,]$rejection)){RES[i,]$rejection}
size_power_AUC <- colMeans(rejection)
print(paste(error_type,"changeAUC","n=",n,size_power_AUC))


print("Finish")
Method = c('ART_dev','ECP')
size_power = c(size_power_ART_dev, size_power_ECP)
result_size_Power.k <- cbind(Method=Method, n=rep(n,2), p=rep(p,2), error_type=rep(error_type,2), size_power=size_power)
result_size_Power <- rbind(result_size_Power,result_size_Power.k)

      
      
      













