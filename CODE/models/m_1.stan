// Model to estimate effect brain on innovation for case 1

data {
  int<lower=0> N_species;
  int innovations_detected_per_species[N_species];
  vector[N_species] relative_brain_per_species;
}

parameters {
  real alpha;
  real beta_brain_innovation;
}

model {
  vector[N_species] lambda;
  alpha ~ normal(2, 1);
  beta_brain_innovation ~ normal(0, 0.5);
  for(i in 1:N_species){
    lambda[i] = exp(alpha + 
    beta_brain_innovation * relative_brain_per_species[i]);
  } 
  innovations_detected_per_species ~ poisson(lambda);
}

