data{
  int<lower = 0> N;
  int<lower = 0, upper = 1> marital[N];
  int<lower = 0, upper = 1> party[N];
  int<lower = 0> state_id[N];
}

parameters{
  real alpha;
  real beta;
}

model{
  for(i in 1:N){
   party[i] ~ bernoulli_logit(alpha[state_id[i]] + beta*marital[i]) 
  }
}
