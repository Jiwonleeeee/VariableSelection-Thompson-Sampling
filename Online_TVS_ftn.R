split_data <- function(Y_ip, X_ip, s_ip, Time_ip){
  
  group <- sample(rep(1:Time_ip, each=s_ip))
  merge <- data.frame(Y= Y_ip, X = X_ip)
  merge <- cbind(merge, group)
  list_split <- list()
  for(tt in 1:Time_ip){
    temp <- subset(merge, group==tt)
    list_split[[tt]] <- temp[,-ncol(temp)]
  }
  
  return(list_split)
  
}


# input: p, TV_time, a and b vector, D, M, C_tilde, X, Y
# return: pi_save, time_spent
running_online_TVS <- function(X_split, p, TV_time, a, b, D, M, C_tilde){
  
  theta <- numeric(p)
  pi_save <- matrix(0, nrow=TV_time, ncol=p)
  
  current_time <- Sys.time()
  for(t in 1:TV_time){
    
    X_use <- X_split[[t]][,-1]
    Y_use <- X_split[[t]][,1]
    
    # sampling theta_j(t)
    for(j in 1:p){
      theta[j] <- rbeta(1, a[j], b[j])
    }
    
    # compute S_t
    index <- which(theta>=C_tilde)
    S_t <- X_use[,index]
    
    # compute the local reward
    bartFit <- wbart(S_t, Y_use, nskip=250, ndpost=M, sparse = T, ntree=D)
    
    # update a and b
    for(r in 1:length(index)){
      if(bartFit$varcount.mean[r]>0){
        a[index[r]] <- a[index[r]] + 1
      }else{
        b[index[r]] <- b[index[r]] + 1
      }
    }
    
    for(j in 1:p){
      pi_save[t, j] <- a[j]/(a[j]+b[j])
    }
    
    print(t) 
  }

  time_spent <- (Sys.time()-current_time) 
  return(list(time_spent=time_spent, pi_save = pi_save))
}




running_online_TVS_multiple_pass <- function(X_split, p, TV_time, a, b, D, M, C_tilde, pass_num){
  
  theta <- numeric(p)
  total_T <- TV_time*pass_num
  pi_save <- matrix(0, nrow=total_T, ncol=p)
  
  current_time <- Sys.time()
  rep_index <- rep(1:TV_time, each=pass_num)
  cnt <- 1
  for(t in rep_index){
    
    X_use <- X_split[[t]][,-1]
    Y_use <- X_split[[t]][,1]
    
    # sampling theta_j(t)
    for(j in 1:p){
      theta[j] <- rbeta(1, a[j], b[j])
    }
    
    # compute S_t
    index <- which(theta>=C_tilde)
    S_t <- X_use[,index]
    
    # compute the local reward
    bartFit <- wbart(S_t, Y_use, nskip=250, ndpost=M, sparse = T, ntree=D)
    
    # update a and b
    for(r in 1:length(index)){
      if(bartFit$varcount.mean[r]>0){
        a[index[r]] <- a[index[r]] + 1
      }else{
        b[index[r]] <- b[index[r]] + 1
      }
    }
    
    for(j in 1:p){
      pi_save[cnt, j] <- a[j]/(a[j]+b[j])
    }
    
    print(t) 
    cnt <- cnt + 1
  }
  
  time_spent <- (Sys.time() - current_time) 
  return(list(time_spent=time_spent, pi_save = pi_save))
}
