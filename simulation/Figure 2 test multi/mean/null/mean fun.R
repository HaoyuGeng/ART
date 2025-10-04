#####functions#####
data_gen <- function(i,n,p,Sigma_error,error_type,tau){
  set.seed(20250706+8*i)
  if(error_type=="Gaussian"){
    error <- mvrnorm(n, rep(0,p), Sigma_error)
  }else if(error_type=="t"){
    error <- matrix( rt(n*p,4)/sqrt(2), n, p)
  }
  y0 <- matrix(rep(mu0,tau[2]*n), ncol = tau[2]*n, byrow = F)
  y1 <- matrix(rep(mu1,round((tau[3]-tau[2])*n)), ncol = round((tau[3]-tau[2])*n), byrow = F)
  y2 <- matrix(rep(mu2,(tau[4]-tau[3])*n), ncol = (tau[4]-tau[3])*n, byrow = F)
  y <- cbind(y0,y1,y2) + t(error)
  
  return(y)
}


###ART
main_ART_fun <- function(i, alpha0, error_type, lambda_s){
  library(MASS)
  library(sparsepca)
  
  y <- data_gen(i,n,p,Sigma_error,error_type,tau_all)
  v1 <- sparsepca::spca(t(y), k = 1, alpha = 0, beta = 0,
                           center = FALSE, scale = FALSE, verbose = 0
  )$loadings[, 1]
  score <- v1%*%y
  rank_score_all <- rank(score,ties.method = "random")
  
  inter_l = 0.15*n
  inter_center = seq(inter_l,n-inter_l,inter_l)  
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
  
  #threshold for rejection
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
  
  #pvalue
  p_value <- (sum(Sn < Sn_B) + runif(1,0,1) * (sum((Sn == Sn_B)+0)+1) ) / (B+1)
  if(p_value < alpha0){
    rejection <- 1
  }else{
    rejection <- 0
  }
  
  result_list <- list(p_value = p_value, rejection = rejection)
  return(result_list)
}


#DMS
main_mean_DMS <- function(i, alpha0, error_type){
  library(MASS)
  library(glmnet)
  library(DMS)
  y <- data_gen(i,n,p,Sigma_error,error_type,tau_all)
  p_value <- DMS(t(y))$pv.DMS
  if(p_value < alpha0){
    rejection <- 1
  }else{
    rejection <- 0
  }
  result_list <- list(p_value = p_value, rejection = rejection)
  return(result_list)
}


#LZZL
main_mean_LZZL <- function(i, alpha0, error_type){
  library(MASS)
  library(glmnet)
  library(DMS)
  
  y <- data_gen(i,n,p,Sigma_error,error_type,tau_all)
  p_value <- LZZL20(t(y))$pv
  if(p_value < alpha0){
    rejection <- 1
  }else{
    rejection <- 0
  }
  result_list <- list(p_value = p_value, rejection = rejection)
  return(result_list)
}






