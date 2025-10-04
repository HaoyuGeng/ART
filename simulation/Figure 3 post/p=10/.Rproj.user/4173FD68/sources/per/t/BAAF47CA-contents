#####functions#####
data_gen <- function(i,n,p,Sigma_error,error_type,tau_all){
  set.seed(202507026+8*i)
  if(error_type=="Gaussian"){
    error <- mvrnorm(n, rep(0,p), Sigma_error)
  }else if(error_type=="t"){
    error <- matrix( rt(n*p,4)/sqrt(2), n, p)
  }
  error <- t(error) #p x n
  y <- NULL
  for (i in 1:(length(tau_all)-1)) {
    y <- cbind(y, matrix(rep(mu_all[,i],tau_all[i+1]-tau_all[i]),nrow=p,byrow = F))
  }
  y <- y + error
  return(y)
}


main_ART_fun <- function(i, alpha0, error_type, inter_l){
  library(MASS)
  library(glmnet)
  library(wbs)
  library(InspectChangepoint)
  
  y <- data_gen(i,n,p,Sigma_error,error_type,tau_all)
  
  threshold <- 4  #compute.threshold(n,p)
  tau_hat <- inspect(y, threshold = threshold)
  tau_hat <- tau_hat$changepoints[,"location"]
  
  v1 <- sparsepca::spca(t(y), k = 1, alpha = 0, beta = 0,
                        center = FALSE, scale = FALSE, verbose = 0
  )$loadings[, 1]
  score <- v1%*%y
  score <- score[1,]
  
  t_alpha <- 249  #compute_thre(n, inter_l, B, alpha0)
  
  ###---score ---
  Sn <- NULL
  for (ii in 1:length(tau_hat)) {
      ind_low <- tau_hat[ii] - inter_l/2 +1
      ind_up <- tau_hat[ii] + inter_l/2
      n_l <- inter_l
   
    score_ii <- score[ind_low:ind_up]
    rank_score <- rank(score_ii,ties.method = "random")
    Sn_t_all <- max(
      sapply(1:(n_l-1), function(i){
        abs(sum(rank_score[1:i]) - i*mean(rank_score))
      })
    )
    Sn <- c(Sn, Sn_t_all)
  }

  correct_tau_hat <- rep(0,length(tau_hat)) 
  for (i in 1:length(tau_hat)) {
    correct_tau_hat[i] <- sum( ((tau_hat[i] - inter_l/2) < tau_all) & ((tau_hat[i] + inter_l/2) > tau_all) ) 
  }
  correct_tau_num <- sum(correct_tau_hat)
  correct_rejection <- sum( (Sn>t_alpha)&(correct_tau_hat==1) )
  false_rejection_all <- ((Sn>t_alpha)&(correct_tau_hat==0)) + 0
  if(sum(false_rejection_all)==0){
    false_rejection <- 0
  }else{
    false_rejection <- 1
  }
  if(is.na(correct_rejection)){
    false_rejection <- 0
    correct_tau_num <- 0
    correct_rejection <- 0
  }
  
  result_list <- list(false_rejection=false_rejection, correct_tau_num=correct_tau_num, correct_rejection=correct_rejection)
  return(result_list)
}





##--------compute thresholds t_alpha---------------
compute_thre <- function(n, inter_l, B, alpha0){
  B=400
  Sn_B <- rep(0,B)
  n_l <- inter_l
  for (bb in 1:B) {
    rank_bb_all <-  rank(runif((n),0,1), ties.method = "random")
    Sn_bb_all <- NULL
    
    for (ii_inter in (inter_l/2 +1):(n-inter_l/2)) {
      ind_low <- ii_inter - inter_l/2 +1
      ind_up <- ii_inter + inter_l/2
      
      n_l <- inter_l
      rank_bb = rank_bb_all[ind_low:ind_up]
      rank_bb <- rank(rank_bb,ties.method = "random")
      
      Sn_bb <- max(
        sapply(1:(n_l-1), function(i){
          abs(sum(rank_bb[1:i]) - i*mean(rank_bb))
        })
      )
      Sn_bb_all <- c(Sn_bb_all, Sn_bb)
    }
    Sn_B[bb] <- max(Sn_bb_all)
  }
  t_alpha <- sort(Sn_B)[B*(1-alpha0)]
  return(t_alpha)
}






main_TUNE_wald_mean_fun <- function(i, alpha0, error_type, inter_l){
  library(MASS)
  library(glmnet)
  library(wbs)
  library(InspectChangepoint)
  
  y <- data_gen(i,n,p,Sigma_error,error_type,tau_all)
  
  threshold <- 4  #compute.threshold(n,p)
  tau_hat <- inspect(y, threshold = threshold)
  tau_hat <- tau_hat$changepoints[,"location"]
  

  source("TUNE_Wald.R")
  source("TUNE_R.R")
  TUNE_result <- TUNE(t(y), cps=tau_hat, h=inter_l/2,
                  test.stat = "Wald",
                  type = "mean",
                   B=200, alpha=alpha0, thres=NULL)
    
  TUNE_result <- sapply(1:length(tau_hat), function(i){
    sum(tau_hat[i] == TUNE_result)
  })
   
    
  correct_tau_hat <- rep(0,length(tau_hat))
  for (i in 1:length(tau_hat)) {
    correct_tau_hat[i] <- sum( ((tau_hat[i] - inter_l/2) < tau_all) & ((tau_hat[i] + inter_l/2) > tau_all) ) 
  }
  correct_tau_num <- sum(correct_tau_hat)
  correct_rejection <- sum( TUNE_result&(correct_tau_hat==1) )
  false_rejection_all <- (TUNE_result&(correct_tau_hat==0)) + 0
  if(sum(false_rejection_all)==0){
    false_rejection <- 0
  }else{
    false_rejection <- 1
  }
  if(is.na(correct_rejection)){
    false_rejection <- 0
    correct_tau_num <- 0
    correct_rejection <- 0
  }
  
  result_list <- list(false_rejection=false_rejection, correct_tau_num=correct_tau_num, correct_rejection=correct_rejection)
  return(result_list)
}



main_TUNE_score_mean_fun <- function(i, alpha0, error_type, inter_l){
  library(MASS)
  library(glmnet)
  library(wbs)
  library(InspectChangepoint)
  
  y <- data_gen(i,n,p,Sigma_error,error_type,tau_all)
  
  threshold <- 4  #compute.threshold(n,p)
  tau_hat <- inspect(y, threshold = threshold)
  tau_hat <- tau_hat$changepoints[,"location"]
  
  source("TUNE_score.R")
  source("TUNE_R.R")
  TUNE_result <- TUNE(t(y), cps=tau_hat, h=inter_l/2,
                      test.stat = "score", norm="Linf",
                      type = "mean",
                      B=200, alpha=alpha0, thres=NULL)   
  TUNE_result <- sapply(1:length(tau_hat), function(i){
    sum(tau_hat[i] == TUNE_result)
  })
  
  correct_tau_hat <- rep(0,length(tau_hat)) 
  for (i in 1:length(tau_hat)) {
    correct_tau_hat[i] <- sum( ((tau_hat[i] - inter_l/2) < tau_all) & ((tau_hat[i] + inter_l/2) > tau_all) ) 
  }
  correct_tau_num <- sum(correct_tau_hat)
  correct_rejection <- sum( TUNE_result&(correct_tau_hat==1) )
  false_rejection_all <- (TUNE_result&(correct_tau_hat==0)) + 0
  if(sum(false_rejection_all)==0){
    false_rejection <- 0
  }else{
    false_rejection <- 1
  }
  if(is.na(correct_rejection)){
    false_rejection <- 0
    correct_tau_num <- 0
    correct_rejection <- 0
  }
  
  result_list <- list(false_rejection=false_rejection, correct_tau_num=correct_tau_num, correct_rejection=correct_rejection)
  return(result_list)
}
