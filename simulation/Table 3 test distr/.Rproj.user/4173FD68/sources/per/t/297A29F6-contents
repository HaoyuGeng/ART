#####distribution test#####
rm(list = ls())
library(foreach)
library(doParallel)

cl <- makeCluster(10) 
registerDoParallel(cl)
  
size_power_all <- NULL
n_all <- c(50,100,200,400)  

for (nn in 1:4) {
  n <- n_all[nn]

  for (cc in 1:3) {
  error_type = paste("case",cc,sep="")  
  
  source("nonpar_model.R")
  source("nonpar_fun.R")
  alpha0=0.1
  rep_num = 1000
  
  #ART
  RES <- foreach(i=1:rep_num,.combine = "rbind") %dopar%
    main_ART_fun(i, n, p, alpha0, error_type)
  p_value <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
    if(is.numeric(RES[i,]$p_value)){RES[i,]$p_value}
  rejection <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
    if(is.numeric(RES[i,]$rejection)){RES[i,]$rejection}
  size_power_ART <- colMeans(rejection)
  print(paste(error_type,"ART","n=",n,size_power_ART))

  #ECP
  RES <- foreach(i=1:rep_num,.combine = "rbind") %dopar%
    main_ECP_fun(i, alpha0, error_type)
  p_value <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
    if(is.numeric(RES[i,]$p_value)){RES[i,]$p_value}
  rejection <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
    if(is.numeric(RES[i,]$rejection)){RES[i,]$rejection}
  size_power_ECP <- colMeans(rejection)
  print(paste(error_type,"ECP","n=",n,size_power_ECP))

  #changeAUC
  RES <- foreach(i=1:rep_num,.combine = "rbind") %dopar%
    main_changeAUC_fun(i, alpha0, error_type)
  p_value <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
    if(is.numeric(RES[i,]$p_value)){RES[i,]$p_value}
  rejection <- foreach(i=1:rep_num,.combine = "rbind")%dopar%
    if(is.numeric(RES[i,]$rejection)){RES[i,]$rejection}
  size_power_AUC <- colMeans(rejection)
  print(paste(error_type,"changeAUC","n=",n,size_power_AUC))


  size_power_all <- rbind(size_power_all,
                          data.frame(n=rep(n,3), p=rep(p,3), Case=rep(error_type,3), Method=c("ART","ECP","cAUC"),
                                     size_power=c(size_power_ART,size_power_ECP,size_power_AUC)) )
  }
}


stopCluster(cl)


    













