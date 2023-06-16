// Simple model to estimate the true number of innovations per species

data {
  int<lower=0> N;
  int x[N];
  vector[N] b;
}

parameters {
  real alpha;
  real beta;
}

model {
  vector[N] lambda;
  alpha ~ normal(2, 1);
  beta ~ normal(0, 0.5);
  for(i in 1:N){
    lambda[i] = exp(alpha + beta * b[i]);
  } 
  x ~ poisson(lambda);
}

