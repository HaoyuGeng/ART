###functions###
data_gen <- function(i,n,p,Sigma0,error_type,tau){
  #data generation
  set.seed(20250806+8*i)
  X1 <- mvrnorm(tau*n,rep(0,p),Sigma0)
  X2 <- mvrnorm((1-tau)*n,rep(0,p),Sigma0)
  if(error_type=="ARMA22"){
    arrho1 <- 0.2
    arrho2 <- -0.2
    matheta1 <- 0.1
    matheta2 <- -0.1
    error <- arima.sim(n = n, model = list(ar = c(arrho1,arrho2), ma = c(matheta1, matheta2))) 
  }else if(error_type=="3-dep"){
    Z <- rnorm(n)
    error <- rnorm(n)
    for (t in 1:n) {
      z_t     <- Z[t]
      z_t1    <- if (t - 1 >= 1) Z[t - 1] else 0
      z_t2    <- if (t - 2 >= 1) Z[t - 2] else 0
      z_t3    <- if (t - 3 >= 1) Z[t - 3] else 0
      error[t] <- (1*z_t + 0.3 * z_t1 - 0.2 * z_t2 + 0.1 * z_t3) /sqrt(1+0.3^2+0.2^2+0.1^2)
    }
  }else if(error_type=="heter"){
    error_groups <- rep((1:n)/(15*n)+1, length.out = n)  
    error <- rnorm(n, mean = 0, sd = error_groups)
  }
  
  y1 <- 0 + X1 %*% beta1
  y2 <- 0 + X2 %*% beta2
  X <- rbind(X1,X2) #n x p
  y <- rbind(y1,y2) + error
  return(list(X=X,y=y))
}



main_ART_fun <- function(i, alpha0,alpha0_new, error_type){
  library(MASS)
  library(glmnet)
  
  data_X_y <- data_gen(i,n,p,Sigma0,error_type,tau) 
  X <- data_X_y$X
  y <- data_X_y$y
  
  ###score###
  beta_can <- glmnet(X,y,family = "gaussian",lambda = sqrt(log(p)/n))$beta
  score <- abs(y - X %*% beta_can)
  rank_score <- rank(score,ties.method = "random")
  
  Sn <- max(
    sapply(1:(n-1), function(i){
      abs(sum(rank_score[1:i]) - i * mean(rank_score))
    })
  )
  
  #threshold for rejection
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
  
  p_value <- (sum(Sn < Sn_B) + runif(1,0,1) * (sum((Sn == Sn_B)+0)+1) ) / (length(Sn_B)+1)
  if(p_value < alpha0){
    rejection <- 1
  }else{
    rejection <- 0
  }
  if(p_value < alpha0_new){
    rejection_new <- 1
  }else{
    rejection_new <- 0
  }
  
  result_list <- list(p_value = p_value, rejection = rejection,rejection_new=rejection_new)
  return(result_list)
}



