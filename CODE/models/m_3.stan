// Model to estimate effect brain on innovation for case 1

data {
  int<lower=0> N_species;
  int innovations_detected_per_species[N_species];
  vector[N_species] relative_brain_per_species;
  vector[N_species] effort_per_species;
}

parameters {
  real alpha;
  real beta_brain_innovation;
  real<lower=0> m;
}

model {
  vector[N_species] lambda;
  vector[N_species] p;
  
  // model effect brain
  alpha ~ normal(-2, 1);
  beta_brain_innovation ~ normal(0, 0.25);
  p = inv_logit(alpha + beta_brain_innovation * relative_brain_per_species);
  
  // model for n innovations
  m ~ exponential(0.2);
  for(i in 1:N_species) lambda[i] = m * (1 - (1-p[i])^effort_per_species[i]) + 0.0001;
  innovations_detected_per_species ~ poisson(lambda);
}

