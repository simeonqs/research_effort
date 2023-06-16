# Function to plot results
plot.results = function(glm_res, bayes_post){
  plot(NULL, xlim = c(-0.5, 1), ylim = c(0, 2.5), 
       xlab = '', ylab = '', yaxt = 'n', bty = 'n', main = '')
  for(i in seq_len(length(glm_res))){
    d_brain_glm = density(rnorm(1e5, glm_res[[i]]$b_brain, glm_res[[i]]$se_brain))
    d_brain_glm$y = d_brain_glm$y / max(d_brain_glm$y)
    lines(d_brain_glm, pch = 16, cex = 3, col = alpha('#2471A3', 0.5))
    d_brain_bayes = density(bayes_post[[i]]$beta_brain_innovation)
    d_brain_bayes$y = d_brain_bayes$y / max(d_brain_bayes$y)
    d_brain_bayes$y = d_brain_bayes$y + 1.3
    lines(d_brain_bayes, pch = 16, cex = 3, col = alpha('#D68910', 0.5))
  }
}

# Plot results
pdf('RESULTS/figures/model_estimates.pdf', 6.5, 4.5)
par(mfrow = c(2, 4), mar = c(2, 1, 1, 1), oma = c(1, 4, 0, 1))

plot.results(glm_1, bayes_1)
lines(c(0, 0), c(0, 50), lty = 2, lwd = 2)
axis(2, c(0.5, 1.9), c('glm', 'scientific'), las = 2, col = NA)

plot.results(glm_2, bayes_2)
lines(c(0.2, 0.2), c(0, 50), lty = 2, lwd = 2)

plot.results(glm_3, bayes_3)
lines(c(0, 0), c(0, 50), lty = 2, lwd = 2)

plot.results(glm_4, bayes_4)
lines(c(0, 0), c(0, 50), lty = 2, lwd = 2)

plot.results(glm_5, bayes_5)
lines(c(0.2, 0.2), c(0, 50), lty = 2, lwd = 2)
axis(2, c(0.5, 1.9), c('glm', 'scientific'), las = 2, col = NA)

plot.results(glm_6, bayes_6)
lines(c(0.2, 0.2), c(0, 50), lty = 2, lwd = 2)

plot.results(glm_7, bayes_7)
lines(c(0, 0), c(0, 50), lty = 2, lwd = 2)

plot.results(glm_8, bayes_8)
lines(c(0.2, 0.2), c(0, 50), lty = 2, lwd = 2)
dev.off()

message('Plotted all results.')