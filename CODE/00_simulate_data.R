# Load library
library(rethinking)

# Set seed
set.seed(1)

# Define function to simulate data
simulate.data = function(
    N_species = 200, # how many data sets to simulate
    N_behaviours = 100000, # how many species to simulate
    p_observe = 0.1, # probability of observing an innovation
    average_log_effort = 1.8, # average number of studies per species (log scale)
    average_logit_p_innovate = logit(0.0001), # the average probability of innovation 
    b_brain_effort, # the effect of brain on effort
    b_brain_innovate, # the effect of brain on innovation
    b_innovate_effort){ # the effect of innovation on effort
  
  # Simulate brain sizes
  relative_brain_per_species = rnorm(N_species, 0, 1)
  
  # Simulate innovations
  p_innovation = inv_logit(average_logit_p_innovate + 
                             b_brain_innovate * relative_brain_per_species)
  innovations_per_species = vapply(seq_len(N_species), function(i) 
    sum(rbinom(N_behaviours, 1, p_innovation[i])),
    numeric(1))
  
  # Simulate effort per species
  # r = 1/exp(average_log_effort + 
  #             b_brain_effort * relative_brain_per_species +
  #             b_innovate_effort * innovations_per_species)
  # lambda_effort = rexp(N_species, r)
  lambda_effort = exp(average_log_effort + 
                        b_brain_effort * relative_brain_per_species +
                        b_innovate_effort * innovations_per_species)
  effort_per_species = rpois(N_species, lambda_effort)
  
  # Simulate detections of innovations
  ## for each species and for each behaviour test if it is detected at least once, 
  ## then return the summed number of innovations detected per species
  innovations_detected_per_species = vapply(seq_len(N_species), function(i){
    if(innovations_per_species[i] == 0) return(0) else {
      ## for each behaviour in this species repertoire, test if it is observed at 
      ## least once (summing all observations and testing if it is greater than 0)
      observed_repertoire = sum(vapply(seq_len(innovations_per_species[i]), function(j) 
        as.numeric(sum(rbinom(effort_per_species[i], 1, p_observe)) > 0), numeric(1)))
      return(observed_repertoire)
    }
  }, numeric(1))
  
  # Return data
  dat = list(N_species = N_species,
             relative_brain_per_species = relative_brain_per_species,
             innovations_per_species = innovations_per_species,
             effort_per_species = effort_per_species,
             innovations_detected_per_species = innovations_detected_per_species)
  
} # end simulate.data

# How many simulations
N_sim = 3

# Simulate for the different cases
data_1 = lapply(seq_len(N_sim), function(i) 
  simulate.data(b_brain_effort = 0, b_brain_innovate = 0, b_innovate_effort = 0))

data_2 = lapply(seq_len(N_sim), function(i) 
  simulate.data(b_brain_effort = 0, b_brain_innovate = 0.2, b_innovate_effort = 0))

data_3 = lapply(seq_len(N_sim), function(i) 
  simulate.data(b_brain_effort = 0.2, b_brain_innovate = 0, b_innovate_effort = 0))

data_4 = lapply(seq_len(N_sim), function(i) 
  simulate.data(b_brain_effort = 0, b_brain_innovate = 0, b_innovate_effort = 0.2))

data_5 = lapply(seq_len(N_sim), function(i) 
  simulate.data(b_brain_effort = 0.2, b_brain_innovate = 0.2, b_innovate_effort = 0))

data_6 = lapply(seq_len(N_sim), function(i) 
  simulate.data(b_brain_effort = 0, b_brain_innovate = 0.2, b_innovate_effort = 0.2))

data_7 = lapply(seq_len(N_sim), function(i) 
  simulate.data(b_brain_effort = 0.2, b_brain_innovate = 0, b_innovate_effort = 0.2))

data_8 = lapply(seq_len(N_sim), function(i) 
  simulate.data(b_brain_effort = 0.2, b_brain_innovate = 0.2, b_innovate_effort = 0.2))

# # Simulate data for collider bias due to sociality
# 
# # Settings
# N_species = 200 # how many data sets to simulate
# N_behaviours = 1000 # how many species to simulate
# p_observe = 0.5 # probability of observing an innovation
# average_log_effort = 1 # average number of studies per species (log scale)
# average_logit_p_innovate = logit(0.01) # the average probability of innovation 
# b_brain_effort = 0.5 # the effect of brain on effort
# b_brain_innovate = 0 # the effect of brain on innovation
# b_innovate_effort = 0 # the effect of innovation on effort
# b_sociality_effort = 2 # the effect of sociality on effort
# b_sociality_innovation = 2 # the effect of sociality on innovation
#   
# # Simulate brain sizes
# relative_brain_per_species = rnorm(N_species, 0, 1)
# 
# # Simulate sociality
# sociality_per_species = rbeta(N_species, 0.5, 0.5)
# 
# # Simulate innovations
# p_innovation = inv_logit(average_logit_p_innovate + 
#                            b_brain_innovate * relative_brain_per_species +
#                            b_sociality_innovation * sociality_per_species)
# innovations_per_species = vapply(seq_len(N_species), function(i) 
#   sum(rbinom(N_behaviours, 1, p_innovation[i])),
#   numeric(1))
# 
# # Simulate effort per species
# lambda_effort = 1/exp(average_log_effort + 
#                         b_brain_effort * relative_brain_per_species +
#                         b_innovate_effort * innovations_per_species + 
#                         b_sociality_effort * sociality_per_species)
# r = rexp(N_species, lambda_effort)
# effort_per_species = rpois(N_species, r)
# 
# # Simulate detections of innovations
# ## for each species and for each behaviour test if it is detected at least once, 
# ## then return the summed number of innovations detected per species
# innovations_detected_per_species = vapply(seq_len(N_species), function(i){
#   if(innovations_per_species[i] == 0) return(0) else {
#     ## for each behaviour in this species repertoire, test if it is observed at 
#     ## least once (summing all observations and testing if it is greater than 0)
#     observed_repertoire = sum(vapply(seq_len(innovations_per_species[i]), function(j) 
#       as.numeric(sum(rbinom(effort_per_species[i], 1, p_observe)) > 0), numeric(1)))
#     return(observed_repertoire)
#   }
# }, numeric(1))
# 
# # Plot
# par(mfrow = c(2, 2))
# plot(relative_brain_per_species, innovations_detected_per_species)
# plot(sociality_per_species, effort_per_species)
# plot(sociality_per_species, innovations_per_species)
# plot(sociality_per_species, innovations_detected_per_species)
# 
# # Test glm
# summary(glm(log(innovations_detected_per_species[effort_per_species>0]+1) ~ 
#               relative_brain_per_species[effort_per_species>0]))
# summary(glm(log(innovations_detected_per_species[effort_per_species>0]+1) ~ 
#               relative_brain_per_species[effort_per_species>0] + 
#               log(effort_per_species[effort_per_species>0])))
# 
# # Return data
# dat = list(N_species = N_species,
#            relative_brain_per_species = relative_brain_per_species,
#            innovations_per_species = innovations_per_species,
#            effort_per_species = effort_per_species,
#            innovations_detected_per_species = innovations_detected_per_species)

# Save all data
save(data_1, data_2, data_3, data_4, data_5, data_6, data_7, data_8, 
     file = 'RESULTS/simulated_data.RData')

# Message
message('Simulations done. Stored all data.')
