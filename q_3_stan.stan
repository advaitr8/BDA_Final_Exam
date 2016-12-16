data{
  real y;
  real sigma;
}
parameters{
  real theta;
}
model{
  y ~ normal(theta, sigma);
  theta ~ uniform(0, 1);
}
