#####functions#####
data_gen <- function(i,n,p,Sigma_error,error_type,tau){
  set.seed(20250706+8*i)
  if(error_type=="3-dep"){
    error <- matrix(0,nrow=p,ncol=n)
    for (ii in 1:p) {
      Z <- rnorm(n)
      error[ii,] <- rnorm(n)
      for (t in 1:n) {
        z_t     <- Z[t]
        z_t1    <- if (t - 1 >= 1) Z[t - 1] else 0
        z_t2    <- if (t - 2 >= 1) Z[t - 2] else 0
        z_t3    <- if (t - 3 >= 1) Z[t - 3] else 0
        error[ii,t] <- (1*z_t + 0.1 * z_t1 - 0.1 * z_t2 + 0.1 * z_t3) 
      }
    }
  }else if(error_type=="ARMA22"){
    arrho1 <- 0.1
    arrho2 <- -0.1
    matheta1 <- 0.1
    matheta2 <- -0.1
    error <- matrix(0,nrow=p,ncol=n)
    for (ee in 1:p) {
      error[ee,] <- arima.sim(n = n, model = list(ar = c(arrho1,arrho2), ma = c(matheta1, matheta2)))
    }
  }else if(error_type=="HOV"){
    error <- matrix(0,nrow=p,ncol=n)
    for (ee in 1:p) {
      error_groups <- rep((1:n)/n+0.5, length.out = n)   
      error[ee,] <- rnorm(n, mean = 0, sd = error_groups)
    }
  }

  y0 <- matrix(rep(mu0,tau*n), ncol = tau*n, byrow = F)
  y1 <- matrix(rep(mu1,(1-tau)*n), ncol = (1-tau)*n, byrow = F)
  y <- cbind(y0,y1) + error  
  return(y)
}


###ART 
main_ART_fun <- function(i, alpha0, alpha0_new, error_type){
  library(MASS)
  library(sparsepca)
  
  y <- data_gen(i,n,p,Sigma_error,error_type,tau)
  v1 <- sparsepca::spca(t(y), k = 1, alpha = 0.01, beta = 0,
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
  
  #pvalue
  p_value <- (sum(Sn < Sn_B) + runif(1,0,1) * (sum((Sn == Sn_B)+0)+1) ) / (B+1)
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
  
  result_list <- list(p_value = p_value, rejection = rejection,rejection_new=rejection_new, i=i)
  return(result_list)
}


