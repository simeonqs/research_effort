// Simple model to estimate the true number of innovations per species

data {
  int<lower=0> N;
  int x[N];
  vector[N] r;
}

parameters {
  real<lower=0, upper=1> p;
  real<lower=0> m;
}

model {
  vector[N] lambda;
  p ~ beta(1, 1.5);
  m ~ exponential(0.2);
  for(i in 1:N) lambda[i] = m * (1 - (1-p)^r[i]);
  x ~ poisson(lambda);
}

