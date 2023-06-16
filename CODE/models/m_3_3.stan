// Model to estimate effect brain on innovation.
// This version is corrected, since m_3 mixed p_innovate and p_observed up. 
// This version is even more crrected since m_3_3 mixed up p_observed and 
// R. 

data {
  int<lower=0> N_species;
  int innovations_detected_per_species[N_species];
  vector[N_species] relative_brain_per_species;
  int effort_per_species[N_species];
}

parameters {
  real<lower=0> n;
  real alpha_innovation;
  real beta_brain_innovation;
  real alpha_effort;
  real beta_brain_effort;
  real beta_innovation_effort;
  real<lower=0, upper=1> p_observe;
}

model {
  vector[N_species] lambda_innovation;
  vector[N_species] lambda_effort;
  vector[N_species] p_innovate;
  vector[N_species] innovations_per_species;
  
  // model for innovation 
  n ~ exponential(0.0001);
  alpha_innovation ~ normal(-4, 1);
  beta_brain_innovation ~ normal(0, 0.25);
  p_innovate = inv_logit(alpha_innovation + 
    beta_brain_innovation * relative_brain_per_species);
  innovations_per_species = n * p_innovate;
    
  // model for effort
  alpha_effort ~ normal(1, 1);
  beta_brain_effort ~ normal(0, 0.25);
  beta_innovation_effort ~ normal(0, 0.25);
  for(i in 1:N_species) lambda_effort[i] = exp(alpha_effort +
    beta_brain_effort * relative_brain_per_species[i] +
    beta_innovation_effort * innovations_per_species[i]);
  effort_per_species ~ poisson(lambda_effort);
  
  // model for observed innovations
  p_observe ~ beta(1, 2);
  for(i in 1:N_species) lambda_innovation[i] = innovations_per_species[i] *
    (1 - (1-p_observe)^effort_per_species[i]) + 0.0001;
  innovations_detected_per_species ~ poisson(lambda_innovation);
}

