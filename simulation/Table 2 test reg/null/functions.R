###functions###
data_gen <- function(i,n,p,Sigma0,error_type,tau){
  set.seed(20250801+8*i)
  X1 <- mvrnorm(tau*n,rep(0,p),Sigma0)
  X2 <- mvrnorm((1-tau)*n,rep(0,p),Sigma0)
  if(error_type=="Gaussian"){
    error1 <- rnorm(tau*n,0,1)
    error2 <- rnorm((1-tau)*n,0,1)
  }else if(error_type=="t"){
    error1 <- rt(tau*n,4)/sqrt(2)
    error2 <- rt((1-tau)*n,4)/sqrt(2)
  }
  
  y1 <- 0 + X1 %*% beta1 + error1
  y2 <- 0 + X2 %*% beta2 + error2
  X <- rbind(X1,X2) #n x p
  y <- rbind(y1,y2)
  
  return(list(X=X,y=y))
}



main_ART_fun <- function(i, alpha0, error_type){
  library(MASS)
  library(glmnet)
  
  data_X_y <- data_gen(i,n,p,Sigma0,error_type,tau) 
  X <- data_X_y$X
  y <- data_X_y$y
  
  ###score###
  beta_can <- glmnet(X,y,family = "gaussian",lambda = 2*sqrt(log(p)/n))$beta
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
  
  result_list <- list(p_value = p_value, rejection = rejection)
  return(result_list)
}




##QFCUSUM
QFCUSUM <- function(i, alpha0, error_type) {
  library(MASS)
  library(glmnet)
  
  data_X_y <- data_gen(i,n,p,Sigma0,error_type,tau) 
  X <- data_X_y$X
  y <- data_X_y$y
  
  zeta = 0.15
  X_pre <- X[1:floor(n * zeta), ]
  y_pre <- y[1:floor(n * zeta)]
  X_post <- X[floor(n * (1 - zeta)):n, ]
  y_post <- y[floor(n * (1 - zeta)):n]
  
  cv_pre <- cv.glmnet(
    X_pre, y_pre,
    intercept = FALSE,
    standardize = FALSE,
    nfolds = 10 
  )
  lambda_pre <- cv_pre$lambda.min 
  beta_hat_pre <- as.vector(coef(cv_pre, s = "lambda.min"))[-1]

  cv_post <- cv.glmnet(
    X_post, y_post,
    intercept = FALSE,
    standardize = FALSE,
    nfolds = 10
  )
  lambda_post <- cv_post$lambda.min 
  beta_hat_post <- as.vector(coef(cv_post, s = "lambda.min"))[-1]

  s_hat_pre <- s0+5  
  s_hat_post <- s0+5   
  
  noise_error_pre <- sqrt(sum((y_pre - X_pre %*% beta_hat_pre)^2) / (floor(n * zeta) - s_hat_pre))
  noise_error_post <- sqrt(sum((y_post - X_post %*% beta_hat_post)^2) / (n - floor(n * (1 - zeta)) + 1 - s_hat_post))
  
  s_hat <- (s_hat_pre + s_hat_post) / 2
  noise_error_hat <- (noise_error_pre + noise_error_post) / 2
  lambda_lasso <- (lambda_pre + lambda_post) / 2
  
  sigma_xi <- s_hat * log(p) / sqrt(n) * log(log(n))
  xi_all <- rnorm(n, mean = 0, sd = sigma_xi)
  
  t_range <- floor(n * zeta):floor(n * (1 - zeta))
  Tn_all <- numeric(n)
  
  for (t in t_range) {
    X1 <- X[1:t, ]
    y1 <- y[1:t]
    X2 <- X[(t + 1):n, ]
    y2 <- y[(t + 1):n]
    
    beta_hat1 <- as.vector(glmnet(X1, y1, lambda = lambda_lasso, intercept = FALSE, standardize = FALSE)$beta)
    beta_hat2 <- as.vector(glmnet(X2, y2, lambda = lambda_lasso, intercept = FALSE, standardize = FALSE)$beta)
    
    Sigma_hat1 <- crossprod(X1) / t
    Sigma_hat2 <- crossprod(X2) / (n - t)
    
    Delta_hat_t <- beta_hat1 - beta_hat2
    
    # S_n(t)
    S_n_t <- 0.5 * (t(Delta_hat_t) %*% Sigma_hat1 %*% Delta_hat_t + t(Delta_hat_t) %*% Sigma_hat2 %*% Delta_hat_t)
    for (i in 1:t) {
      S_n_t <- S_n_t + (1 / t) * (2 * t(Delta_hat_t) %*% X1[i, ] + xi_all[i]) * (y1[i] - sum(X1[i, ] * beta_hat1))
    }
    for (i in 1:(n - t)) {
      S_n_t <- S_n_t - (1 / (n - t)) * (2 * t(Delta_hat_t) %*% X2[i, ] + xi_all[i + t]) * (y2[i] - sum(X2[i, ] * beta_hat2))
    }
    
    # T_n(t)
    noise_scale_factor <- 1
    T_n_t <- 1 / (sigma_xi * noise_scale_factor) * sqrt(t * (n - t) / n) * S_n_t
    Tn_all[t] <- T_n_t
  }
  
  
  
  
  calculate_G_alpha_size <- function(alpha_size = alpha0, B = 10000, n_B = 500, zeta) {
    sup_G_all <- numeric(B)
    
    for (b in 1:B) {
      dt <- 1 / n_B
      z <- numeric(n_B + 1)
      for (i in 1:n_B) {
        z[i + 1] <- z[i] + sqrt(dt) * rnorm(1)
      }
      z <- z[-1]  
      
    
      r_range <- floor(zeta * n_B):floor((1 - zeta) * n_B)
      G <- numeric(length(r_range))
      
      for (r in r_range) {
        G[r] <- (z[r] - r / n_B * z[1]) / sqrt(r / n_B * (1 - r / n_B))
      }
      
      sup_G_all[b] <- max(G) 
    }
    
    sup_G_all_sorted <- sort(sup_G_all)
    G_alpha_size <- sup_G_all_sorted[ceiling(B * (1 - alpha_size))]
    
    return(G_alpha_size)
  }
  
  #G_alpha_size <- calculate_G_alpha_size(alpha_size = alpha0, B = 10000, n_B = 500, zeta=0.15)
  G_alpha_size <- 3.571
  
  if(max(Tn_all, na.rm = TRUE) > G_alpha_size){
    rejection <- 1
  }else{
    rejection <- 0
  }
  
  result_list <- list(max_Tn = max(Tn_all, na.rm = TRUE), rejection = rejection)
  return(result_list)
}









