data_gen <- function(i,n,p,error_type){
  set.seed(20250706 + 2*i)
  if(error_type=="case1"){                            #NULL
    X <- mvrnorm(n,rep(0,p),Sigma0)
  }else if(error_type=="case2"){                      #Full change
    X <- rbind(mvrnorm(0.3*n,rep(0,p),Sigma0), mvrnorm(0.3*n,rep(0,p),0.5*Sigma0+Sigma2), 
               mvrnorm(0.4*n,rep(0,p),Sigma0))
  }else if(error_type=="case3"){                      #partial change
    X <- mvrnorm(n,rep(0,p),Sigma0)
    Sigma3=diag(1,p)
    Sigma3[(1:4),(1:4)] = 0.5*Sigma0[(1:4),(1:4)] + Sigma2[(1:4),(1:4)]
    X[(0.3*n+1):(0.6*n),] <- mvrnorm(0.3*n,rep(0,p),Sigma3)
  }
  return(X)
}


main_ART_fun <- function(i, alpha0, error_type, lambda_s){
  library(MASS)

  X <- data_gen(i,n,p,error_type)
  score <- rowSums(X^2)
  rank_score_all <- rank(score,ties.method = "random")
  
  inter_l = 0.15*n
  inter_center = seq(inter_l,300-inter_l,45) 
  
  Sn <- NULL
  for (ii_inter in 1:length(inter_center)) {
    ind_low <- inter_center[ii_inter] - inter_l +1
    ind_up <- inter_center[ii_inter] + inter_l
    n_l <- inter_l
    rank_score <- rank_score_all[ind_low:ind_up]
    rank_score <- rank(rank_score,ties.method = "random")
    
    Sn_t_all <- max(
      sapply(1:(n_l-1), function(i){
        abs(sum(rank_score[1:i]) - i*mean(rank_score))
      })
    )
    Sn <- c(Sn, Sn_t_all)
    print(Sn)
  }
  Sn <- max(Sn)

  #threshold 
  B=200
  Sn_B <- rep(0,B)
  for (bb in 1:B) {
    rank_bb_all <-  rank(runif((n),0,1), ties.method = "random")
    Sn_bb <- NULL
    for (ii_inter in 1:length(inter_center)) {
      ind_low <- inter_center[ii_inter] - inter_l +1
      ind_up <- inter_center[ii_inter] + inter_l
      n_l <- inter_l
      rank_bb = rank_bb_all[ind_low:ind_up]
      rank_bb <- rank(rank_bb,ties.method = "random")
      Sn_bb_all <- max(
        sapply(1:(n_l-1), function(i){
          abs(sum(rank_bb[1:i]) - i*mean(rank_bb))
        })
      )
      Sn_bb <- c(Sn_bb, Sn_bb_all)
    }
    Sn_B[bb] <- max(Sn_bb)
  }
  
  p_value <- (sum(Sn < Sn_B) + runif(1,0,1) * (sum((Sn == Sn_B)+0)+1) ) / (B+1)
  if(p_value < alpha0){
    rejection <- 1
  }else{
    rejection <- 0
  }
  result_list <- list(p_value = p_value, rejection = rejection)
  return(result_list)
}



main_ECP_fun <- function(i, alpha0, error_type){
  library(MASS)
  library(glmnet)
  library(ecp)
  
  X <- data_gen(i,n,p,error_type)
  ecp_result <- e.divisive(X=X, sig.lvl=0.1, R=200, k=NULL, min.size=5, alpha=1)
  p_value <- ecp_result$p.values[1]
  if(p_value < alpha0){
    rejection <- 1
  }else{
    rejection <- 0
  }
  result_list <- list(p_value = p_value, rejection = rejection)
  return(result_list)
}





main_changeAUC_fun <- function(i, alpha0, error_type){
  library(MASS)
  
  X <- data_gen(i,n,p,error_type)
  source("get_change_point_v1.R")
  AUC_result <- get_change_point(X, classifier = "RF",
                                 split_trim = 0.15,
                                 auc_trim = 0.05,
                                 perm_pval = TRUE,
                                 no_of_perm = 199,
                                 tau = 0.5, verbose = TRUE)
  
  p_value <- AUC_result$pval
  if(p_value < alpha0){
    rejection <- 1
  }else{
    rejection <- 0
  }
  result_list <- list(p_value = p_value, rejection = rejection)
  return(result_list)
}







