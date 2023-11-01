sim_data = function(mu = 0, sigma = 1, n = 500, option = 1, option_zero = 1){
  X_1 = rbeta(n, 2, 3)
  X_2 = rbeta(n, 2, 3)
  X_3 = rbeta(n, 2, 3)
 
  W_1 = exp(X_1 - 1)
  W_2 = (X_1 + X_2 ** 2) / 4
  W_3 = sin(X_3)
 
  prop_score_A = exp(0.5 * X_1 + 0.5 * X_2 - 1) / (1 + exp(0.5 * X_1 + 0.5 * X_2 - 1))
  A = rbinom(n, 1, prop_score_A)
  
  prop_score_Z = exp(-1.7 + 1.5 * A + 0.5 * (X_3 ** 2)) / 
    (1 + exp(-1.7 + 1.5 * A + 0.5 * (X_3 ** 2)))
  
  Z = rbinom(n, 3, prop_score_Z)
  
  lambda_1 = 1.2
  lambda_2 = 1.5
  gamma_2 = 1.2
  gamma_3 = 1.2
  
  if (option_zero == 1){
    lambda_1 = 0
  }
  
  if (option_zero == 2){
    lambda_2 = 0
  }
  
  if (option_zero == 3){
    gamma_2 = 0
  }
  
  if (option_zero == 4){
    gamma_3 = 0
  }
  
  prop_score_M = exp(-1.5 + lambda_1 * Z + lambda_2 * A + 0.4 * X_2 + 0.2 * X_3) / 
    (1 + exp(-1.5 + lambda_1 * Z + lambda_2 * A + + 0.4 * X_2 + 0.2 * X_3))
 
  M = rbinom(n, 3, prop_score_M)

  prop_score_Y = exp(0.4 * M + gamma_2 * Z + gamma_3 * A - 0.5 * cos(X_1) - 1.5) /
    (1 + exp(0.4 * M + gamma_2 * Z + gamma_3 * A - 0.5 * cos(X_1) - 1.5))
  
  Y = rbinom(n, 1, prop_score_Y)
  
  if (option == 1){
    sim_data = data.frame(X_1 = X_1, X_2 = X_2, X_3 = X_3, 
                          A = A, Z = Z, M = M, y = Y)
  }
  else if (option == 2){
    sim_data = data.frame(X_1 = W_1, X_2 = W_2, X_3 = W_3, 
                          A = A, Z = Z, M = M, y = Y)
  }
  else{
    stop("option should be either 1 or 2, where 1 means X, 2 means W")
  }
  
  return(list(sim_data=sim_data))
}