// Simple model to estimate the true number of innovations per species

data {
  int<lower=0> N;
  int x[N];
  vector[N] b;
  vector[N] r;
}

parameters {
  real alpha;
  real beta_brain;
  real beta_effort;
}

model {
  vector[N] lambda;
  alpha ~ normal(2, 1);
  beta_brain ~ normal(0, 0.5);
  beta_effort ~ normal(0, 0.5);
  for(i in 1:N){
    lambda[i] = exp(alpha + beta_brain * b[i] + beta_effort * r[i]);
  } 
  x ~ poisson(lambda);
}

