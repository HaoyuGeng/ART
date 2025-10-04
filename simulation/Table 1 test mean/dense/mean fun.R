#####functions#####
data_gen <- function(i,n,p,Sigma_error,error_type,tau){
  set.seed(20250803+8*i)
  if(error_type=="Gaussian"){
    error <- mvrnorm(n, rep(0,p), Sigma_error)
  }else if(error_type=="t"){
    error <- matrix( rt(n*p,4)/sqrt(2), n, p)
  }
  y0 <- matrix(rep(mu0,tau*n), ncol = tau*n, byrow = F)
  y1 <- matrix(rep(mu1,(1-tau)*n), ncol = (1-tau)*n, byrow = F)
  y <- cbind(y0,y1) + t(error) ##y:p x n
  
  return(y)
}



###ART 
main_ART_fun <- function(i, alpha0, error_type, lambda_s){
  library(MASS)
  library(sparsepca)
  
  #data generation
  y <- data_gen(i,n,p,Sigma_error,error_type,tau)
  #deviance transformation
  v1 <- sparsepca::spca(t(y), k = 1, alpha = 0, beta = 0,
                           center = FALSE, scale = FALSE, verbose = 0
  )$loadings[, 1]
  score <- v1%*%y
  
  #Sn
  rank_score <- rank(score,ties.method = "random")
  Sn <- max(
    sapply(1:(n-1), function(i){
      abs(sum(rank_score[1:i]) - i * mean(rank_score))
    })
  )
  
  #threshold
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
  library(InspectChangepoint)
  library(DMS)
  
  #data generation
  y <- data_gen(i,n,p,Sigma_error,error_type,tau)
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
  library(InspectChangepoint)
  library(DMS)
  
  #data generation
  y <- data_gen(i,n,p,Sigma_error,error_type,tau)
  p_value <- LZZL20(t(y))$pv
  if(p_value < alpha0){
    rejection <- 1
  }else{
    rejection <- 0
  }
  result_list <- list(p_value = p_value, rejection = rejection)
  return(result_list)
}






