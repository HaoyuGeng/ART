#####functions#####
data_gen <- function(i,n,p,error_type){
  set.seed(20250803 + 2*i)
  source("nonpar_model.R")
  if(error_type=="case1"){          #NULL
    X <- mvrnorm(n,rep(0,p),Sigma0)
  }else if(error_type=="case2"){    #Full change
    X <- rbind(mvrnorm(0.5*n,rep(0,p),Sigma0), mvrnorm(0.5*n,rep(0,p),0.5*Sigma0+Sigma2))
  }else if(error_type=="case3"){    #partial change
    X1 <- mvrnorm(0.5*n,rep(0,p),Sigma0)
    if(p==5){
      Sigma3=diag(1,p)
      Sigma3[(1:2),(1:2)] = 0.5*Sigma0[(1:2),(1:2)] + Sigma2[(1:2),(1:2)]
      X2 <- mvrnorm(0.5*n,rep(0,p),Sigma3)
    }else if(p==10){
      Sigma3=diag(1,p)
      Sigma3[(1:4),(1:4)] = 0.5*Sigma0[(1:4),(1:4)] + Sigma2[(1:4),(1:4)]
      X2 <- mvrnorm(0.5*n,rep(0,p),Sigma3)
    }
    X <- rbind(X1,X2)
  }
  return(X)
}


main_ART_fun <- function(i, n, p, alpha0, error_type){
  library(MASS)
  X <- data_gen(i,n,p,error_type)
  score <- rowSums(X^2)
  n = length(score)
  rank_score <- rank(score,ties.method = "random")
  Sn <- max(
    sapply(1:(n-1), function(i){
      abs(sum(rank_score[1:i]) - i*mean(rank_score))
    })
  )
  
  ###threshold for rejection
  B=200
  Sn_B <- rep(0,B)
  for (bb in 1:B) {
    rank_bb <- rank(runif(n,0,1), ties.method = "random")
    Sn_bb <- max(
      sapply(1:(n-1), function(i){
        abs(sum(rank_bb[1:i]) - i*mean(rank_bb))
      })
    )
    Sn_B[bb] <- Sn_bb
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




















