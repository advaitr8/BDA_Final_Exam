data{
  int<lower = 0> N;
  int<lower = 0, upper = 1> marital[N];
  int<lower = 0, upper = 1> party[N];
  int<lower = 0> state_id[N];
  int<lower = 0> state_id_pred[48];
  real<lower = 0> mar_pred[48];
}

parameters{
  real alpha[48];
  real beta[48];
  real alpha_pool;
  real beta_pool;
  real mu_a;
  real mu_b;
  real<lower = 0> sigma_a;
  real<lower = 0> sigma_b;
}

model{
  //partial pooling model
  for(i in 1:N){
   party[i] ~ bernoulli_logit(alpha[state_id[i]] + beta[state_id[i]]*marital[i]); 
  }
  //complete pooling model
  for(i in 1:N){
    party[i] ~ bernoulli_logit(alpha_pool + beta_pool*marital[i]);  
  }
  alpha ~ normal(mu_a, sigma_a);
  beta ~ normal(mu_b, sigma_b);
  alpha_pool ~ normal(mu_a, sigma_a);
  beta_pool ~ normal(mu_b, sigma_b);
  mu_a ~ normal(0,2);
  mu_b ~ normal(-0.5,2);
  sigma_a ~ normal(3,3);
  sigma_b ~ normal(3,3);
}

generated quantities{
  vector[48] vote_pred;
  vector[48] pool_pred;
  vector[48] gap_pred;
  vector[N] indv_vote;
  real gap_pool_pred;
  //pred partial pooling model
  for(i in 1:48){
    vote_pred[i] = bernoulli_rng(inv_logit(alpha[state_id_pred[i]] 
    + beta[state_id_pred[i]]*mar_pred[i]));
  }
  //gap partial pooling model
  for(i in 1:48){
    gap_pred[i] = inv_logit(alpha[i] + beta[i]) - inv_logit(alpha[i]);
  }
  
  //pred complete pooling model
  for(i in 1:48){
    pool_pred[i] = bernoulli_rng(inv_logit(alpha_pool + beta_pool*mar_pred[i]));
  }
  //gap partial pooling model
  for(i in 1:48){
    gap_pool_pred = inv_logit(alpha_pool + beta_pool) - inv_logit(alpha_pool);
  }
  //posterior predictive check
  for(i in 1:N){
    indv_vote[i] = bernoulli_rng(inv_logit(alpha[state_id[i]] 
    + beta[state_id[i]]*marital[i]));
  }
}
