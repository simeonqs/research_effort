# Plot figure effect research effort

# Load data
load('RESULTS/simulated_data.RData')

# Settings
p_observe = 0.1

# Open PDF
pdf('RESULTS/figures/research_effort.pdf', 5, 4)

# Plot the effort vs the detected innovations
plot(data_1[[1]]$effort_per_species + rnorm(data_1[[1]]$N_species, 0, 0.1), 
     data_1[[1]]$innovations_detected_per_species + rnorm(data_1[[1]]$N_species, 0, 0.1),
     pch = 16, col = '#979A9A', xlab = 'research effort', ylab = 'observed number innovations',
     xlim = c(0, max(data_1[[1]]$effort_per_species)))
ue = sort(unique(data_1[[1]]$effort_per_species))
# points(ue, 
#        vapply(ue, function(e) 
#          mean(data_1[[1]]$innovations_detected_per_species[data_1[[1]]$effort_per_species == e]), 
#          numeric(1)),
#        pch = 16, col = 6, cex = 2)

# Plot the theoretical relationship
r = seq(min(ue), max(ue))
m = mean(data_1[[1]]$innovations_per_species)
x = m * (1 - (1-p_observe)^r)
lines(r, x, col = 4, lwd = 3)

# Plot a simple log model
I = data_1[[1]]$innovations_detected_per_species[data_1[[1]]$effort_per_species > 0]
R = data_1[[1]]$effort_per_species[data_1[[1]]$effort_per_species > 0]
fit = lm(log(I + 1) ~ log(R))
pred = exp(fit$coefficients[1] + fit$coefficients[2] * log(r[r != 0])) - 1
lines(r[r != 0], pred, col = 2, lwd = 3)

# Close PDF
dev.off()

# Message
message('Plot succesful.')
