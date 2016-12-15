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
  real mu_a;
  real mu_b;
  real<lower = 0> sigma_a;
  real<lower = 0> sigma_b;
}

model{
  for(i in 1:N){
   party[i] ~ bernoulli_logit(alpha[state_id[i]] + beta[state_id[i]]*marital[state_id[i]]); 
  }
  alpha ~ normal(mu_a, sigma_a);
  beta ~ normal(mu_b, sigma_b);
  mu_a ~ normal(0,2);
  mu_b ~ normal(-0.5,2);
  sigma_a ~ normal(3,3);
  sigma_b ~ normal(3,3);
}

generated quantities{
  vector[48] vote_pred;
  for(i in 1:48){
    vote_pred[i] = bernoulli_rng(inv_logit(alpha[state_id_pred[i]] 
    + beta[state_id_pred[i]]*mar_pred[i]));
  }
}
