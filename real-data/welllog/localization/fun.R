generate_intervals <- function(n) {
  grid <- expand.grid(i = 1:n, j = 1:n)
  valid_intervals <- subset(grid, i < (j-10))
  return(as.matrix(valid_intervals))  
}




create_seed_int <- function(T, a=1/sqrt(2), d = 2) {
  if (a < 0.5 || a >= 1) {
    stop("Decay parameter a must be in [1/2, 1).")
  }
  intervals <- list()
  intervals[[1]] <- matrix(c(0, T), ncol = 2, byrow = TRUE)
  K <- ceiling(log(T, base = 1/a))
  for (k in 2:K) {
    l_k <- T * a^(k-1)                  
    if (l_k < d) break                   
    
    n_k <- 2 * ceiling((1/a)^(k-1)) - 1 
    if (n_k <= 1) next
    
    s_k <- (T - l_k) / (n_k - 1)         
    
    layer_intervals <- matrix(NA, nrow = n_k, ncol = 2)
    for (i in 1:n_k) {
      left <- floor((i-1) * s_k)
      right <- ceiling((i-1) * s_k + l_k)
      layer_intervals[i, ] <- c(left, right)
    }
    
    intervals[[k]] <- layer_intervals
  }
  
  all_intervals <- do.call(rbind, intervals)
  all_intervals <- unique(all_intervals)
  
  colnames(all_intervals) <- c("start", "end")
  return(all_intervals)
}


Sn_t_abs_sup <- function(V, s, e){ #return sup|Sn_t|
  V_se <- rank(V[s:e],ties.method = "random")
  Sn_t_abs <- sapply(2:(length(V_se)-2), function(i){
    abs(sum(V_se[1:i] - mean(V_se)))
  }) / (sqrt(e-s))^3
  Sn_t_abs <- c(0, Sn_t_abs)
  Sn <- max(Sn_t_abs)
  tau_hat <- which.max(Sn_t_abs) + s - 1
  return(list(Sn=Sn, tau_hat = tau_hat))
}


not_each_iter <- function(V, intervals, Sn_Il_all, tau_est_all, int_s, int_e, lambda_alpha){
  int_s = as.numeric(int_s)
  int_e = as.numeric(int_e)
  
  can_index = which((intervals[,1]>int_s) & (intervals[,2]<int_e) )
  intervals = intervals[ can_index, , drop=FALSE]
  Sn_all = Sn_Il_all[ can_index]
  tau_est_all = tau_est_all[can_index]
  if(((int_e-int_s)<=2)|(dim(intervals)[1]==0)){
    stop_pro = 1
    interval_select = c(NULL,NULL)
    tau_hat = NULL
    next_int_s1 <- NULL
    next_int_e1 <- NULL
    
    next_int_s2 <- NULL
    next_int_e2 <- NULL
  }else{
    len_intervals <- sapply(1:dim(intervals)[1], function(i){
      intervals[i,2] - intervals[i,1] + 1
    })
    
    O_intervals <- which(Sn_all>lambda_alpha)
    if(length(O_intervals)==0){
      stop_pro = 1
      interval_select = c(NULL,NULL)
      tau_hat = NULL
      next_int_s1 <- NULL
      next_int_e1 <- NULL
      
      next_int_s2 <- NULL
      next_int_e2 <- NULL
      
    }else{
      stop_pro = 0
      l_select_ind = which.min(len_intervals[O_intervals])
      l_select = O_intervals[l_select_ind]
      interval_select = intervals[l_select,]
      
      tau_hat = tau_est_all[l_select]
      
      next_int_s1 <- int_s
      next_int_e1 <- intervals[l_select,1] - 1
      
      next_int_s2 <- intervals[l_select,2]
      next_int_e2 <- int_e
      
    }
    
  }
  
  
  return(list(stop_pro = stop_pro, interval_select = interval_select, tau_hat = tau_hat,
              next_int_s1 = next_int_s1, next_int_e1 = next_int_e1,
              next_int_s2 = next_int_s2, next_int_e2 = next_int_e2, Sn_all=Sn_all ))
}


react_cp <- function(V, intervals,Sn_Il_all, tau_est_all, n, lambda_alpha){
  
  interval_select_all = NULL
  tau_hat_all = NULL
  
  iter_num=100
  for (i in 1:iter_num) {
    if(i==1){
      not_result <- not_each_iter(V,intervals,Sn_Il_all, tau_est_all, 1,n,lambda_alpha)
      interval_select_all <- rbind(interval_select_all, not_result$interval_select)
      tau_hat_all <- rbind(tau_hat_all, not_result$tau_hat)
      next_int_s_all <- rbind(not_result$next_int_s1, not_result$next_int_s2)
      next_int_e_all <- rbind(not_result$next_int_e1, not_result$next_int_e2)
      
      continue_index <- which(not_result["stop_pro"]==0)  
      if(length(continue_index)==0){  
        break
      }
    }else{
      interval_initial <- cbind(next_int_s_all, next_int_e_all)
      interval_num <- dim(interval_initial)[1]
      not_result <- sapply(1:interval_num, function(i){
        not_each_iter(V,intervals, Sn_Il_all, tau_est_all, interval_initial[i,1],interval_initial[i,2],lambda_alpha)
      })
      
      #start and end point of next iteration
      continue_index <- which(not_result["stop_pro",]==0)  
      if(length(continue_index)==0){  
        break
      }else{
        not_result_interval_select <- matrix(unlist(not_result["interval_select",][continue_index]), byrow = T, ncol = 2)
        interval_select_all <- rbind(interval_select_all, not_result_interval_select)
        
        tau_hat <- matrix(unlist(not_result["tau_hat",][continue_index]), ncol = 1)
        tau_hat_all <- rbind(tau_hat_all, tau_hat)
        next_int_s_all <- c(not_result["next_int_s1",][continue_index], not_result["next_int_s2",][continue_index])
        next_int_e_all <- c(not_result["next_int_e1",][continue_index], not_result["next_int_e2",][continue_index])
      }
    }
  }
  
  
  return(list(interval_select_all=interval_select_all, tau_hat_all = tau_hat_all))
  
}


####main ART####
main_ART_cp_fun <- function(data){
  
  library(MASS)
  library(glmnet)
  n <- length(data)

  V <- data
  
  #seeded 
  intervals = create_seed_int(n,d=6) 
  intervals = intervals + cbind(rep(1,dim(intervals)[1]),rep(0,dim(intervals)[1]))
  
  #compute all Sn_Il and corresponding tau_hat
  Sn_tau_all_interval <- sapply(1:dim(intervals)[1], function(i){
    Sn_t_abs_sup(V, intervals[i,1], intervals[i,2])
  })
  Sn_Il_all <- unlist(Sn_tau_all_interval["Sn",])
  tau_est_all <- unlist(Sn_tau_all_interval["tau_hat",])
  
  #compute threshold
  lambda_alpha <- threshold_alpha_fun(intervals, n, 0.1)
  react_result <- react_cp(V, intervals, Sn_Il_all, tau_est_all, n, lambda_alpha)
  
  return(react_result)
}



main_nsp_fun <- function(data){
  
  library(MASS)
  library(glmnet)
  library(wbs)
  library(nsp)
  
  #nsp
  nsp_result <- nsp_poly(data, 3501, deg = 0,alpha = 0.1) 
  interval_select_all <- nsp_result$intervals[,c("starts","ends")]
  tau_hat_all <- nsp_result$intervals[,"midpoints"]
  return(list(interval_select_all=interval_select_all, tau_hat_all = tau_hat_all))
  
}



main_rnsp_fun <- function(data){
  
  library(MASS)
  library(glmnet)
  library(wbs)
  library(nsp)
  source("rnsp.R")
  
  #rnsp
  rnsp_result <- rnsp(data, 3501, deg = 0,alpha = 0.1)  
  interval_select_all <- rnsp_result$intervals[,c("starts","ends")]
  tau_hat_all <- floor(rowMeans(interval_select_all))
  return(list(interval_select_all=interval_select_all, tau_hat_all = tau_hat_all))
  
}



#######-------------------------------------------------------------------------
#####thresholds#####
threshold_alpha_fun <- function(intervals, n, alpha0){
  library(foreach)
  library(doParallel)
  library(Matrix)
  library(CPAT)
  
  cl <- makeCluster(10) 
  registerDoParallel(cl)

  Sn_t_abs_sup <- function(V, s, e){ 
    V_se <- rank(V[s:e],ties.method = "random")
    Sn_t_abs <- sapply(2:(length(V_se)-2), function(i){
      abs(sum(V_se[1:i] - mean(V_se)))
    }) / (sqrt(e-s))^3
    Sn_t_abs <- c(0, Sn_t_abs)
    Sn <- max(Sn_t_abs)
    tau_hat <- which.max(Sn_t_abs) + s - 1
    return(list(Sn=Sn, tau_hat = tau_hat))
  }
  
  Sn_max_compute <- function(intervals, n){
    V0 <- rank(rep(0,n), ties.method="random")
    Sn_max0 <- sapply(1:dim(intervals)[1], function(i){
      Sn_t_abs_sup(V0, intervals[i,1], intervals[i,2])$Sn
    })
    Sn_max0 <- Sn_max0[is.na(Sn_max0)==F]
    Sn_max0 <- max(unlist(Sn_max0))
    return(Sn_max0)
  }
  
  B = 200
  Sn_max0_all <- foreach(i=1:B,.combine = "rbind") %dopar%
    Sn_max_compute(intervals, n)
  
  stopCluster(cl)
  
  threshold_alpha <- sort(Sn_max0_all)[floor(B*(1-alpha0))]
  
  return(threshold_alpha)
  
}



data_process <- function(dataset){
  dataset[which(dataset<=10^5)] = 10^5
  return(dataset)
}

  
  
  
  
  
  
  
  
  
  



