#Data Generating Process
#Simple DGP
Simple_dgp<- function(n,noise,noise_var){
  X_1 <- rnorm(n,0,1)
  X_2 <- rnorm(n,0,1)
  X_3 <- rnorm(n,0,1)
  vec <- ifelse(X_1 > 0,1,0)
  u = rnorm(n,mean = 0 ,sd = noise)
  if (noise_var != 0){
    pesudo_matrix <- matrix(0,noise_var,noise_var) 
    mean_vector <- rep(0,noise_var)
    for (i in 1:noise_var){
      for(j in 1:noise_var){
        if(i == j){
          pesudo_matrix[i,j] = 1
        }else{
          pesudo_matrix[i,j] = 0.5
        }
      }
    }
    pesudo_covariate <- mvrnorm(n = n, mu = mean_vector, Sigma = pesudo_matrix)
    Y = vec*(X_2 + X_3) + 2*X_1 + X_2 + X_3 + u
    dgp_return <- data.frame(Y,vec,X_1,X_2,X_3,pesudo_covariate)
    return(dgp_return)
  }else{
    Y = vec*(X_2 + X_3) + 2*X_1 + X_2 + X_3 + u
    dgp_return <- data.frame(Y,vec,X_1,X_2,X_3)
  }
  return(dgp_return)
}

#Medium DGP
Medium_dgp<- function(n,noise,noise_var){
  X_1 <- rnorm(n,0,1)
  X_2 <- rnorm(n,0,1)
  X_3 <- rnorm(n,0,1)
  vec <- ifelse(X_1 > 0,1,0)
  u = rnorm(n,mean = 0 ,sd = noise)
  if (noise_var != 0){
    pesudo_matrix <- matrix(0,noise_var,noise_var) 
    mean_vector <- rep(0,noise_var)
    for (i in 1:noise_var){
      for(j in 1:noise_var){
        if(i == j){
          pesudo_matrix[i,j] = 1
        }else{
          pesudo_matrix[i,j] = 0.5
        }
      }
    }
    pesudo_covariate <- mvrnorm(n = n, mu = mean_vector, Sigma = pesudo_matrix)
    Y = 2 + vec*(1 + X_2+ X_2^2 + X_3 + X_3^2) + 2*X_1 + 1.5*X_1^2 + X_2 + X_3 + u
    dgp_return <- data.frame(Y,vec,X_1,X_2,X_3,pesudo_covariate)
    return(dgp_return)
  }else{
    Y = 2 + vec*(1 + X_2+ X_2^2 + X_3 + X_3^2) + 2*X_1 + 1.5*X_1^2 + X_2 + X_3 + u
    dgp_return <- data.frame(Y,vec,X_1,X_2,X_3)
  }
  return(dgp_return)
}


#complex DGP
Complex_dgp<- function(n,noise,noise_var){
  X_1 <- rnorm(n,0,1)
  X_2 <- rnorm(n,0,1)
  X_3 <- rnorm(n,0,1)
  vec <- ifelse(X_1 > 0,1,0)
  u = rnorm(n,mean = 0 ,sd = noise)
  if (noise_var != 0){
    pesudo_matrix <- matrix(0,noise_var,noise_var) 
    mean_vector <- rep(0,noise_var)
    for (i in 1:noise_var){
      for(j in 1:noise_var){
        if(i == j){
          pesudo_matrix[i,j] = 1
        }else{
          pesudo_matrix[i,j] = 0.5
        }
      }
    }
    pesudo_covariate <- mvrnorm(n = n, mu = mean_vector, Sigma = pesudo_matrix)
    Y = 2 + vec*(1 + X_1+ X_1^2+ X_2+ X_2^2 + X_3 + X_3^2) + 2*X_1 + 1.5*X_1^2 + X_2 + X_3 + u
    dgp_return <- data.frame(Y,vec,X_1,X_2,X_3,pesudo_covariate)
    return(dgp_return)
  }else{
    Y = 2 + vec*(1 + X_1+ X_1^2+ X_2+ X_2^2 + X_3 + X_3^2) + 2*X_1 + 1.5*X_1^2 + X_2 + X_3 + u
    dgp_return <- data.frame(Y,vec,X_1,X_2,X_3)
  }
  return(dgp_return)
}



