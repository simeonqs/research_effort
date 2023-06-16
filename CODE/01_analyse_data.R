# Load data
load('RESULTS/simulated_data.RData')

# Set model path
path_model = 'CODE/models/m_3_3.stan'

# Define function to analyse with glm
analyse.glm = function(dat){
  i = dat$innovations_detected_per_species[dat$effort_per_species > 1]
  b = dat$relative_brain_per_species[dat$effort_per_species > 1]
  r = dat$effort_per_species[dat$effort_per_species > 1]
  fit = glm(log(i+1) ~ b + log(r))
  s = summary(fit)
  return(list(b_brain = s$coefficients['b','Estimate'],
              se_brain = s$coefficients['b','Std. Error'],
              b_effort = s$coefficients['log(r)','Estimate'],
              se_effort = s$coefficients['log(r)','Std. Error']))
}

# Analyse all data sets with glm
glm_1 = lapply(data_1, analyse.glm)
glm_2 = lapply(data_2, analyse.glm)
glm_3 = lapply(data_3, analyse.glm)
glm_4 = lapply(data_4, analyse.glm)
glm_5 = lapply(data_5, analyse.glm)
glm_6 = lapply(data_6, analyse.glm)
glm_7 = lapply(data_7, analyse.glm)
glm_8 = lapply(data_8, analyse.glm)

# Analyse with Bayesian model
analyse.bayes = function(dat, path_model){
  model = cmdstan_model(path_model)
  # dat$relative_brain_per_species = dat$relative_brain_per_species[dat$effort_per_species > 1]
  # dat$innovations_per_species = dat$innovations_per_species[dat$effort_per_species > 1]
  # dat$innovations_detected_per_species = dat$innovations_detected_per_species[dat$effort_per_species > 1]
  # dat$effort_per_species = dat$effort_per_species[dat$effort_per_species > 1]
  # dat$N_species = length(dat$innovations_detected_per_species)
  fit = model$sample(data = dat, 
                     seed = 1, 
                     chains = 4, 
                     parallel_chains = 4,
                     refresh = 500) 
  fit_nice = fit$output_files() |>
    rstan::read_stan_csv()
  post = extract.samples(fit_nice)
}

bayes_1 = lapply(data_1, analyse.bayes, path_model)
bayes_2 = lapply(data_2, analyse.bayes, path_model)
bayes_3 = lapply(data_3, analyse.bayes, path_model)
bayes_4 = lapply(data_4, analyse.bayes, path_model)
bayes_5 = lapply(data_5, analyse.bayes, path_model)
bayes_6 = lapply(data_6, analyse.bayes, path_model)
bayes_7 = lapply(data_7, analyse.bayes, path_model)
bayes_8 = lapply(data_8, analyse.bayes, path_model)

# Save
save(glm_1, glm_2, glm_3, glm_4, glm_5, glm_6, glm_7, glm_8, 
     bayes_1, bayes_2, bayes_3, bayes_4, bayes_5, bayes_6, bayes_7, bayes_8,
     file = 'RESULTS/model_results.RData')

# Message
message('All analysis done. Stored summarised results.')
