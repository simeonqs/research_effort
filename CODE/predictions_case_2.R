# Plot predictions for case 2 on data

# Load libraries
library(cmdstanr)

# Load data
load('RESULTS/simulated_data.RData')
dat = data_2[[1]]
d = as.data.frame(dat[-1])

# Plot
plot(d)

# Run model
path_model = 'CODE/models/m_3_3.stan'
model = cmdstan_model(path_model)
fit = model$sample(data = dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 500) 
fit_nice = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_nice)
post = extract.samples(fit_nice)

# Plot effort vs observed
plot(d$effort_per_species, d$innovations_detected_per_species)
r = seq(min(d$effort_per_species), max(d$effort_per_species))
m = mean(d$innovations_per_species)
p_observe = 0.1
x = m * (1 - (1-p_observe)^r)
lines(r, x, col = 4, lwd = 3)

# Plot posterior predictions
x = vapply(r, function(ri) 
  vapply(seq_len(length(post$lp__)), function(j){
    m = 1
    p = inv_logit(post$alpha_innovation[j] + post$beta_brain_innovation[j] * 0)
    return(m * (1 - (1-p)^ri) + 0.0001)
  }, numeric(1)) |> mean(), numeric(1))
lines(r, x, col = 2, lwd = 3)

# Plot brain vs innovation
b = seq(min(dat$relative_brain_per_species), max(dat$relative_brain_per_species), 0.1)
average_log_effort = 1
average_logit_p_innovate = logit(0.01)
b_brain_innovate = 0.2
N_behaviours = 1000
plot(d$relative_brain_per_species, d$innovations_per_species)
p_innovation = inv_logit(average_logit_p_innovate + 
                           b_brain_innovate * b)
x = p_innovation * N_behaviours
lines(b, x, col = 4, lwd = 3)

# Plot posterior predictions
x = vapply(r, function(ri) 
  vapply(seq_len(length(post$lp__)), function(j){
    m = post$m[j]
    p = inv_logit(post$alpha[j] + post$beta_brain_innovation[j] * 0)
    return(m * (1 - (1-p)^ri) + 0.0001)
  }, numeric(1)) |> mean(), numeric(1))
lines(r, x, col = 2, lwd = 3)
