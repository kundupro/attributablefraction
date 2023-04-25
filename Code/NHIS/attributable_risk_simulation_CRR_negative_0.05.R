#---------Continuous E-----#
#E continuous, U continuous
n_sim = 1000
Prob_D_given_E_hypo = c()
Prob_D_data_C = c()
Prob_D_data_B = c()
Prob_D_given_E_obs = c()
Prob_D_given_E_MR_corrected = c()
n_sample = 100000
no_of_confounders = 1
mu_param = rep(0, no_of_confounders+1)
mu_param[2] = 0.55
sigma_param_single_U = matrix(c(1,-0.13,-0.13,1), no_of_confounders+1,no_of_confounders+1)
#beta_effect = rep(log(1.6), no_of_confounders)
CRR = c()
beta_effect = rep(1, no_of_confounders)
for(sim in 1:n_sim)
{
  j = 3*(sim-1)
  set.seed(j+1)
  U_variables = rmvnorm(n = n_sample, mean = mu_param, sigma = sigma_param_single_U)
  
  U = as.matrix(U_variables[, -1]) %*% as.numeric(beta_effect)
  
  X = cbind(1, U_variables[, 1], U)
  set.seed(j+3)
  beta_MR = rnorm(1, log(1.4), 0.01)
  beta_true = c(-2.8, log(1.4), 1)
  
  #p_true = locfit::expit(as.numeric(X%*%beta_true))
  p_true = exp(as.numeric(X%*%beta_true))
  set.seed(j+1)
  D = rbinom(n_sample, 1, p_true)
  data_B = as.data.frame(cbind(D, X))
  colnames(data_B)[2:4] = c("V", "E", "U")
  if(sum(is.na(D)) > 0)
  {
    data_B = data_B[-which(is.na(data_B$D) == T),]
  }
  
  theta_observed = glm(D ~ E , data = data_B, family = poisson())$coefficients
  #theta_observed
  #theta_observed = glm(D ~ E , data = data_B, family = poisson())$coefficients
  #theta_observed
  temp = glm(D ~ E + U , data = data_B, family = poisson())$coefficients
  CRR = c(CRR, ((theta_observed[2] - temp[2])/temp[2])*100)
  #glm(D ~ E + U , data = data_B, family = poisson())$coefficients
  Prob_D_data_B = c(Prob_D_data_B, mean(data_B$D))
  data_B$E = quantile(data_B$E, probs = 0.05)
  obs_prob = mean(exp(as.matrix(data_B)[,c(2:3)] %*% theta_observed))
  Prob_D_given_E_obs = c(Prob_D_given_E_obs, obs_prob)
  rho_hat = theta_observed[2] - beta_MR
  Prob_D_given_E_MR_corrected = c(Prob_D_given_E_MR_corrected, obs_prob * exp((rho_hat^2)/2 - (rho_hat*unique(data_B$E))))
  if(sum(is.na(D)) > 0)
  {
    X = X[-which(is.na(D) == T), ]
    D = D[-which(is.na(D) == T)]
  }
  
  X[,2] = quantile(X[,2], probs = 0.05)
  
  data_C = as.data.frame(cbind(D, X))
  colnames(data_C)[2:4] = c("V", "E", "U")
  Prob_D_given_E_hypo = c(Prob_D_given_E_hypo, mean(exp(as.matrix(data_C)[,2:4] %*% beta_true)))
  print(sim)
}
PAR_observed = 1 - mean(Prob_D_given_E_obs)/mean(Prob_D_data_B)
#  0.2218242
PAR_hypo = 1 - mean(Prob_D_given_E_hypo)/mean(Prob_D_data_B)
# 0.4202686
PAR_MR = 1 - mean(Prob_D_given_E_MR_corrected)/mean(Prob_D_data_B)
#0.4205321
mean(Prob_D_data_B)
#0.1573797
AR_MR = mean(Prob_D_given_E_MR_corrected)
AR_hypo = mean(Prob_D_given_E_hypo)
AR_observed =  mean(Prob_D_given_E_obs)


#E continuous, U_1,U_2,U_3 are binary, U_4, U_5 are continuous
n_sim = 1000
Prob_D_given_E_hypo = c()
Prob_D_data_B = c()
Prob_D_given_E_obs = c()
Prob_D_given_E_MR_corrected = c()
CRR = c()
n_sample = 100000
no_of_confounders = 5
marginal = list(c(0.8),c(0.6),c(0.2))
mu_param = rep(0, no_of_confounders+1)
offdiag = -0.5*c(c(0.2,0.2,0.2,0.2,0.2), c(0.2,0.2,0.2,0.2), c(0.2, 0.2, 0.2), c(0.2, 0.2), rep(0.2,1))
sigma_param = matrix(NA, ncol = length(mu_param), nrow = length(mu_param))
sigma_param[lower.tri(sigma_param)] <- offdiag
sigma_param[upper.tri(sigma_param)] <- t(sigma_param)[upper.tri(t(sigma_param))]
diag(sigma_param) <- rep(1, length(mu_param))
beta_effect = rep(log(1.65), no_of_confounders)
#beta_effect[3] = -beta_effect[3]
#beta_effect[4] = -log(1.8)
for(sim in 1:n_sim)
{
  j = 3*(sim-1)
  set.seed(j+1)
  mm.cmat = cmat.star(plist = marginal, CorrMat = sigma_param, no.ord = 3, no.norm = 3)
  set.seed(j+1)
  U_variables = genOrdNor(n = n_sample, plist = marginal, cmat.star = mm.cmat, mean.vec = c(0,0,0), sd.vec = c(1,1,1), no.ord = 3, no.norm = 3)
  U_variables[, 1:3] = U_variables[,1:3]-1
  #E = U_variables[,5]
  #U_variables = U_variables[,-5]
  U = as.matrix(U_variables[, -6]) %*% as.numeric(beta_effect)
  
  X = cbind(1, U_variables[, 6], U)
  set.seed(j+2)
  beta_MR = rnorm(1, log(1.4), 0.01)
  beta_true = c(-2.8,log(1.4), 1)
  
  #p_true = locfit::expit(as.numeric(X%*%beta_true))
  p_true = exp(as.numeric(X%*%beta_true))
  set.seed(j+1)
  D = rbinom(n_sample, 1, p_true)
  data_B = as.data.frame(cbind(D, X))
  colnames(data_B)[2:4] = c("V", "E", "U")
  if(sum(is.na(D)) > 0)
  {
    data_B = data_B[-which(is.na(data_B$D) == T),]
  }
  
  #theta_observed = glm(D ~ E , data = data_B, family = poisson())$coefficients
  #theta_observed
  #glm(D ~ E + U , data = data_B, family = poisson())$coefficients
  theta_observed = glm(D ~ E , data = data_B, family = poisson())$coefficients
  #theta_observed
  #theta_observed = glm(D ~ E , data = data_B, family = poisson())$coefficients
  #theta_observed
  temp = glm(D ~ E + U , data = data_B, family = poisson())$coefficients
  CRR = c(CRR, ((theta_observed[2] - temp[2])/temp[2])*100)
  
  Prob_D_data_B = c(Prob_D_data_B, mean(data_B$D))
  data_B$E = quantile(data_B$E, probs = 0.05)
  obs_prob = mean(exp(as.matrix(data_B)[,c(2:3)] %*% theta_observed))
  Prob_D_given_E_obs = c(Prob_D_given_E_obs, obs_prob)
  rho_hat = theta_observed[2] - beta_MR
  Prob_D_given_E_MR_corrected = c(Prob_D_given_E_MR_corrected, obs_prob * exp((rho_hat^2)/2 - (rho_hat*unique(data_B$E))))
  if(sum(is.na(D)) > 0)
  {
    X = X[-which(is.na(D) == T), ]
    D = D[-which(is.na(D) == T)]
  }
  
  X[,2] = quantile(X[,2], probs = 0.05)
  
  data_C = as.data.frame(cbind(D, X))
  colnames(data_C)[2:4] = c("V", "E", "U")
  Prob_D_given_E_hypo = c(Prob_D_given_E_hypo, mean(exp(as.matrix(data_C)[,2:4] %*% beta_true)))
  print(sim)
}
PAR_observed = 1 - mean(Prob_D_given_E_obs)/mean(Prob_D_data_B)
# 0.2515574
PAR_hypo = 1 - mean(Prob_D_given_E_hypo)/mean(Prob_D_data_B)
#0.4247392
PAR_MR = 1 - mean(Prob_D_given_E_MR_corrected)/mean(Prob_D_data_B)
#0.4251202
mean(Prob_D_data_B)
#0.1516833

AR_MR = mean(Prob_D_given_E_MR_corrected)
AR_hypo = mean(Prob_D_given_E_hypo)
AR_observed =  mean(Prob_D_given_E_obs)



#E continuous, U  binary
n_sim = 1000
Prob_D_given_E_hypo = c()
Prob_D_data_C = c()
Prob_D_data_B = c()
Prob_D_given_E_obs = c()
Prob_D_given_E_MR_corrected = c()
CRR = c()

marginal = list(c(0.1))
sigma_param <- matrix(c(1,-0.56,-0.56,1), no_of_confounders+1,no_of_confounders+1)
n_sample = 10000
no_of_confounders = 1
#beta_effect = rep(log(1.6), no_of_confounders)
beta_effect = rep(1, no_of_confounders)
for(sim in 1:n_sim)
{
  j = 3*(sim-1)
  set.seed(j+1)
  mm.cmat = cmat.star(plist = marginal, CorrMat = sigma_param, no.ord = 1, no.norm = 1)
  set.seed(j+1)
  U_variables = genOrdNor(n = n_sample, plist = marginal, cmat.star = mm.cmat, mean.vec = c(0), sd.vec = c(1), no.ord = 1, no.norm = 1)
  E = U_variables[,2]
  U_variables[,1] = U_variables[,1] - 1
  
  U = as.matrix(U_variables[, 1]) %*% as.numeric(beta_effect)
  
  X = cbind(1, E, U)
  set.seed(j+3)
  beta_MR = rnorm(1, log(1.4), 0.01)
  beta_true = c(-2.8,log(1.4), 1)
  #p_true = locfit::expit(as.numeric(X%*%beta_true))
  p_true = exp(as.numeric(X%*%beta_true))
  set.seed(j+1)
  D = rbinom(10000, 1, p_true)
  data_B = as.data.frame(cbind(D, X))
  colnames(data_B)[2:4] = c("V", "E", "U")
  if(sum(is.na(D)) > 0)
  {
    data_B = data_B[-which(is.na(data_B$D) == T),]
  }
  
  theta_observed = glm(D ~ E , data = data_B, family = poisson())$coefficients
  #theta_observed
  #theta_observed = glm(D ~ E , data = data_B, family = poisson())$coefficients
  #theta_observed
  temp = glm(D ~ E + U , data = data_B, family = poisson())$coefficients
  CRR = c(CRR, ((theta_observed[2] - temp[2])/temp[2])*100) 
  Prob_D_data_B = c(Prob_D_data_B, mean(data_B$D))
  data_B$E = quantile(data_B$E, probs = 0.05)
  obs_prob = mean(exp(as.matrix(data_B)[,c(2:3)] %*% theta_observed))
  Prob_D_given_E_obs = c(Prob_D_given_E_obs, obs_prob)
  rho_hat = theta_observed[2] - beta_MR
  Prob_D_given_E_MR_corrected = c(Prob_D_given_E_MR_corrected, obs_prob * exp((rho_hat^2)/2 - (rho_hat*unique(data_B$E))))
  if(sum(is.na(D)) > 0)
  {
    X = X[-which(is.na(D) == T), ]
    D = D[-which(is.na(D) == T)]
  }
  
  X[,2] = quantile(X[,2], probs = 0.05)
  
  data_C = as.data.frame(cbind(D, X))
  colnames(data_C)[2:4] = c("V", "E", "U")
  Prob_D_given_E_hypo = c(Prob_D_given_E_hypo, mean(exp(as.matrix(data_C)[,2:4] %*% beta_true)))
  print(sim)
}

PAR_observed = 1 - mean(Prob_D_given_E_obs)/mean(Prob_D_data_B)
#0.2482658
PAR_hypo = 1 - mean(Prob_D_given_E_hypo)/mean(Prob_D_data_B)
#0.4297044
PAR_MR = 1 - mean(Prob_D_given_E_MR_corrected)/mean(Prob_D_data_B)
# 0.4239054
mean(Prob_D_data_B)
#0.1561264

AR_MR = mean(Prob_D_given_E_MR_corrected)
AR_hypo = mean(Prob_D_given_E_hypo)
AR_observed =  mean(Prob_D_given_E_obs)



#---------Categorical E--------#

#E categorical, U_1,U_2,U_3 categorical and U_4, U_5 continuous
n_sample = 10000
n_sim = 1000
Prob_D_given_E_hypo = c()
Prob_D_data_C = c()
Prob_D_data_B = c()
Prob_D_given_E_obs = c()
Prob_D_given_E_MR_corrected = c()
CRR = c()
no_of_confounders = 5
marginal = list(c(0.3),c(0.5),c(0.4), c(0.4))
mu_param = rep(0, no_of_confounders+1)
offdiag = -c(c(0.1,0.1,0.1,0.1,0.1), c(0.1,0.1,0.1,0.1), c(0.1, 0.1, 0.1), c(0.1, 0.1),c(0.1))
sigma_param = matrix(NA, ncol = length(mu_param), nrow = length(mu_param))
sigma_param[lower.tri(sigma_param)] <- offdiag
sigma_param[upper.tri(sigma_param)] <- t(sigma_param)[upper.tri(t(sigma_param))]
diag(sigma_param) <- rep(1, length(mu_param))
beta_effect = rep(log(1.4), no_of_confounders)
beta_effect[5] = -log(1.03)
#beta_effect[4] = -log(1.4)
#beta_effect = rep(1, no_of_confounders)
for(sim in 1:n_sim)
{
  j = 2*(sim-1)
  set.seed(j+1)
  mm.cmat = cmat.star(plist = marginal, CorrMat = sigma_param, no.ord = 4, no.norm = 2)
  set.seed(j+1)
  U_variables = genOrdNor(n = n_sample, plist = marginal, cmat.star = mm.cmat, mean.vec = c(0,0), sd.vec = c(1,1), no.ord = 4, no.norm = 2)
  U_variables[, 1:4] = U_variables[, 1:4]-1
  E = U_variables[,1]
  U = as.matrix(U_variables[, 2:6]) %*% as.numeric(beta_effect)
  
  X = cbind(1, E, U)
  set.seed(j+2)
  beta_MR = rnorm(1, log(1.4), 0.01)
  beta_true = c(-2.8, log(1.4), 1)
  #p_true = locfit::expit(as.numeric(X%*%beta_true))
  p_true = exp(as.numeric(X%*%beta_true))
  set.seed(j+1)
  D = rbinom(10000, 1, p_true)
  data_B = as.data.frame(cbind(D, X))
  colnames(data_B)[2:4] = c("V", "E", "U")
  if(sum(is.na(D)) > 0)
  {
    data_B = data_B[-which(is.na(data_B$D) == T),]
  }
  
  theta_observed = glm(D ~ E , data = data_B, family = poisson())$coefficients
  #theta_observed
  #theta_observed = glm(D ~ E , data = data_B, family = poisson())$coefficients
  #theta_observed
  temp = glm(D ~ E + U , data = data_B, family = poisson())$coefficients
  CRR = c(CRR, ((theta_observed[2] - temp[2])/temp[2])*100) 
  Prob_D_data_B = c(Prob_D_data_B, mean(data_B$D))
  prob_E = prop.table(table(data_B$E)) 
  data_B$E = 0
  obs_prob = mean(exp(as.matrix(data_B)[,c(2:3)] %*% theta_observed))
  Prob_D_given_E_obs = c(Prob_D_given_E_obs, obs_prob)
  rho_hat = theta_observed[2] - beta_MR
  Prob_D_given_E_MR_corrected = c(Prob_D_given_E_MR_corrected, obs_prob * (prob_E[1] + (prob_E[2])*exp(rho_hat)))
  if(sum(is.na(D)) > 0)
  {
    X = X[-which(is.na(D) == T), ]
    D = D[-which(is.na(D) == T)]
  }
  
  X[,2] = 0
  
  data_C = as.data.frame(cbind(D, X))
  colnames(data_C)[2:4] = c("V", "E", "U")
  Prob_D_given_E_hypo = c(Prob_D_given_E_hypo, mean(exp(as.matrix(data_C)[,2:4] %*% beta_true)))
  print(sim)
}

PAR_observed = 1 - mean(Prob_D_given_E_obs)/mean(Prob_D_data_B)
#0.1104026
PAR_hypo = 1 - mean(Prob_D_given_E_hypo)/mean(Prob_D_data_B)
#0.209362
PAR_MR = 1 - mean(Prob_D_given_E_MR_corrected)/mean(Prob_D_data_B)
#0.2091419
mean(Prob_D_data_B)
#0.1469906
AR_MR = mean(Prob_D_given_E_MR_corrected)
AR_hypo = mean(Prob_D_given_E_hypo)
AR_observed =  mean(Prob_D_given_E_obs)


#E categorical, U continuous,   Here U|E is normal

n_sim = 1000
Prob_D_given_E_hypo = c()
Prob_D_data_C = c()
Prob_D_data_B = c()
Prob_D_given_E_obs = c()
Prob_D_given_E_MR_corrected = c()


n_sample = 10000
no_of_confounders = 1
mu_U_given_E_0 = 1.6
mu_U_given_E_1 = 1.1
sigma_U_given_E_0 = 0.1
sigma_U_given_E_1 = 0.8
p_E = 0.65
beta_effect = rep(log(1.62), no_of_confounders)
CRR = c()
#beta_effect = rep(1, no_of_confounders)
for(sim in 1:n_sim)
{
  j = 2*(sim-1)
  set.seed(j+1)
  E = rbinom(n_sample, 1, p_E)
  E = sort(E)
  number_E_0 = length(which(E==0))
  number_E_1 = length(which(E==1))
  set.seed(j+1)
  U_given_E_0  = rnorm(number_E_0, mu_U_given_E_0, sigma_U_given_E_0)
  set.seed(j+1)
  U_given_E_1  = rnorm(number_E_1, mu_U_given_E_1, sigma_U_given_E_1)
  U_variables = c(U_given_E_0, U_given_E_1)
  U = U_variables * as.numeric(beta_effect)
  
  X = cbind(1, E, U)
  set.seed(j+2)
  beta_MR = rnorm(1, log(1.4), 0.01)
  beta_true = c(-2.8,log(1.4), 1)
  #p_true = locfit::expit(as.numeric(X%*%beta_true))
  p_true = exp(as.numeric(X%*%beta_true))
  set.seed(j+1)
  D = rbinom(n_sample, 1, p_true)
  data_B = as.data.frame(cbind(D, X))
  colnames(data_B)[2:4] = c("V", "E", "U")
  if(sum(is.na(D)) > 0)
  {
    data_B = data_B[-which(is.na(data_B$D) == T),]
  }
  
  theta_observed = glm(D ~ E , data = data_B, family = poisson())$coefficients
  #theta_observed
  #theta_observed = glm(D ~ E , data = data_B, family = poisson())$coefficients
  #theta_observed
  temp = glm(D ~ E + U , data = data_B, family = poisson())$coefficients
  CRR = c(CRR, ((theta_observed[2] - temp[2])/temp[2])*100) 
  Prob_D_data_B = c(Prob_D_data_B, mean(data_B$D))
  prob_E = prop.table(table(data_B$E)) 
  data_B$E = 0
  obs_prob = mean(exp(as.matrix(data_B)[,c(2:3)] %*% theta_observed))
  Prob_D_given_E_obs = c(Prob_D_given_E_obs, obs_prob)
  rho_hat = theta_observed[2] - beta_MR
  Prob_D_given_E_MR_corrected = c(Prob_D_given_E_MR_corrected, obs_prob * (prob_E[1] + (prob_E[2])*exp(rho_hat)))
  if(sum(is.na(D)) > 0)
  {
    X = X[-which(is.na(D) == T), ]
    D = D[-which(is.na(D) == T)]
  }
  
  X[,2] = 0
  
  data_C = as.data.frame(cbind(D, X))
  colnames(data_C)[2:4] = c("V", "E", "U")
  Prob_D_given_E_hypo = c(Prob_D_given_E_hypo, mean(exp(as.matrix(data_C)[,2:4] %*% beta_true)))
  print(sim)
}

PAR_observed = 1 - mean(Prob_D_given_E_obs)/mean(Prob_D_data_B)
# 0.1063864
PAR_hypo = 1 - mean(Prob_D_given_E_hypo)/mean(Prob_D_data_B)
#0.1957541
PAR_MR = 1 - mean(Prob_D_given_E_MR_corrected)/mean(Prob_D_data_B)
#0.1962586
mean(Prob_D_data_B)
#0.1473533
AR_MR = mean(Prob_D_given_E_MR_corrected)
AR_hypo = mean(Prob_D_given_E_hypo)
AR_observed =  mean(Prob_D_given_E_obs)


#E categorical, U categorical

n_sim = 1000
Prob_D_given_E_hypo = c()
Prob_D_data_C = c()
Prob_D_data_B = c()
Prob_D_given_E_obs = c()
Prob_D_given_E_MR_corrected = c()

CRR = c()

n_sample = 10000
no_of_confounders = 1
marginal = list(c(0.5),c(0.4))
offdiag = -c(c(0.207,0.1,0.1), c(0.1,0.1), c(0.1))
sigma_param = matrix(NA, ncol = 4, nrow = 4)
sigma_param[lower.tri(sigma_param)] <- offdiag
sigma_param[upper.tri(sigma_param)] <- t(sigma_param)[upper.tri(t(sigma_param))]
diag(sigma_param) <- rep(1, 4)
beta_effect = rep(1, no_of_confounders)
for(sim in 1:n_sim)
{
  j = 2*(sim-1)
  set.seed(j+1)
  j = 2*(sim-1)
  set.seed(j+1)
  mm.cmat = cmat.star(plist = marginal, CorrMat = sigma_param, no.ord = 2, no.norm = 2)
  set.seed(j+1)
  U_variables = genOrdNor(n = n_sample, plist = marginal, cmat.star = mm.cmat, mean.vec = c(0,0), sd.vec = c(1,1), no.ord = 2, no.norm = 2)
  U_variables[, 1:2] = U_variables[, 1:2]-1
  #U_variables = rbinom(n_sample, 1, p_U_given_E)
  E = U_variables[,1]
  U = U_variables[,2] * as.numeric(beta_effect)
  
  
  X = cbind(1, E, U)
  set.seed(j+2)
  beta_MR = rnorm(1, log(1.4), 0.01)
  beta_true = c(-2.8,log(1.4), 1)
  #p_true = locfit::expit(as.numeric(X%*%beta_true))
  p_true = exp(as.numeric(X%*%beta_true))
  set.seed(j+1)
  D = rbinom(n_sample, 1, p_true)
  data_B = as.data.frame(cbind(D, X))
  colnames(data_B)[2:4] = c("V", "E", "U")
  if(sum(is.na(D)) > 0)
  {
    data_B = data_B[-which(is.na(data_B$D) == T),]
  }
  
  theta_observed = glm(D ~ E , data = data_B, family = poisson())$coefficients
  #theta_observed
  #theta_observed = glm(D ~ E , data = data_B, family = poisson())$coefficients
  #theta_observed
  temp = glm(D ~ E + U , data = data_B, family = poisson())$coefficients
  CRR = c(CRR, ((theta_observed[2] - temp[2])/temp[2])*100) 
  
  #print(sim)
  Prob_D_data_B = c(Prob_D_data_B, mean(data_B$D))
  prob_E = prop.table(table(data_B$E)) 
  data_B$E = 0
  obs_prob = mean(exp(as.matrix(data_B)[,c(2:3)] %*% theta_observed))
  Prob_D_given_E_obs = c(Prob_D_given_E_obs, obs_prob)
  rho_hat = theta_observed[2] - beta_MR
  Prob_D_given_E_MR_corrected = c(Prob_D_given_E_MR_corrected, obs_prob * (prob_E[1] + (prob_E[2])*exp(rho_hat)))
  if(sum(is.na(D)) > 0)
  {
    X = X[-which(is.na(D) == T), ]
    D = D[-which(is.na(D) == T)]
  }
  
  X[,2] = 0
  
  data_C = as.data.frame(cbind(D, X))
  colnames(data_C)[2:4] = c("V", "E", "U")
  Prob_D_given_E_hypo = c(Prob_D_given_E_hypo, mean(exp(as.matrix(data_C)[,2:4] %*% beta_true)))
  print(sim)
}

PAR_observed = 1 - mean(Prob_D_given_E_obs)/mean(Prob_D_data_B)
#0.09296446
PAR_hypo = 1 - mean(Prob_D_given_E_hypo)/mean(Prob_D_data_B)
#0.155236
PAR_MR = 1 - mean(Prob_D_given_E_MR_corrected)/mean(Prob_D_data_B)
#0.1558784
mean(Prob_D_data_B)
#0.1462138

AR_MR = mean(Prob_D_given_E_MR_corrected)
AR_hypo = mean(Prob_D_given_E_hypo)
AR_observed =  mean(Prob_D_given_E_obs)




