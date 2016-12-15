data{
  int<lower = 0> N;
  int<lower = 0, upper = 1> marital[N];
  int<lower = 0, upper = 1> party[N];
  int<lower = 0> state_id[N];
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
  mu_b ~ normal(-2,2);
  sigma_a ~ normal(0.5,3);
  sigma_b ~ normal(0.5,3);
}
