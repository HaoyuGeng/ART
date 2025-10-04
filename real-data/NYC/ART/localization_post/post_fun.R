main_ART_post_fun <- function(tau_hat, score, alpha0, inter_l){
  library(MASS)
  library(glmnet)
  library(wbs)
  library(InspectChangepoint)
  
  n <- length(score)
  
  Sn <- NULL
  for (ii in 1:length(tau_hat)) {
    if(((tau_hat[ii] - inter_l/2 +1)>0) & ((tau_hat[ii] + inter_l/2)<=n) )
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
  
  
  t_alpha <- 479.5 #post_thre(n, inter_l)  
  
  return( tau_hat[Sn>t_alpha])
}




post_thre <- function(n, inter_l, alpha0=0.1){
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





find_max_positions <- function(values, positions_vector, buffer = 20) {
  selected_positions <- c() 
  
  while (length(values) > 0) {
    max_pos <- which.max(values) 
    selected_positions <- c(selected_positions, positions_vector[max_pos]) 
    
    current_position <- positions_vector[max_pos]
    
    left_bound <- current_position - buffer
    right_bound <- current_position + buffer
    
    to_remove <- which(positions_vector >= left_bound & positions_vector <= right_bound)
    
    values <- values[-to_remove]
    positions_vector <- positions_vector[-to_remove]
  }
  
  return(sort(selected_positions))
}









