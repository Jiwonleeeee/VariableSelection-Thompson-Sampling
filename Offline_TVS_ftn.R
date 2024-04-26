# input: p, TV_time, a and b vector, D, M, C_tilde, X, Y
# return: pi_save, time_spent
running_offline_TVS <- function(X, Y, p, TV_time, a, b, D, M, C_tilde){
  theta <- numeric(p)
  pi_save <- matrix(0, nrow=TV_time, ncol=p)
  
  current_time <- Sys.time()
  for(t in 1:TV_time){
    
    # sampling theta_j(t)
    for(j in 1:p){
      theta[j] <- rbeta(1, a[j], b[j])
    }
    
    # compute S_t
    index <- which(theta>=C_tilde)
    S_t <- X[,index]
    
    # compute the local reward
    bartFit <- wbart(S_t, Y, nskip=250, ndpost=M, sparse = T, ntree=D, printevery=1000)
    
    # update a and b
    for(r in 1:length(index)){
      if(bartFit$varcount[M, r]>=1){
        a[index[r]] <- a[index[r]] + 1
      }else{
        b[index[r]] <- b[index[r]] + 1
      }
    }
    
    for(j in 1:p){
      pi_save[t, j] <- a[j]/(a[j]+b[j])
    }
    
    if(t %% 100==0) print(t)
    
  }
  
  time_spent <- (Sys.time()-current_time) 
  return(list(time_spent=time_spent, pi_save = pi_save))
}