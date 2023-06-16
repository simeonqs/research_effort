# Calculate bias
extract.bias = function(glm_res, bayes_post, true_value){
  bias_glm = vapply(glm_res, function(res) res$b_brain - true_value, numeric(1))
  bias_bayes = vapply(bayes_post, function(post) 
    mean(post$beta_brain_innovation) - true_value, numeric(1))
  coverage_glm = vapply(glm_res, function(res) 
    true_value > (res$b_brain - 2*res$se_brain) & 
      true_value < (res$b_brain + 2*res$se_brain),
    logical(1))
  coverage_bayes = vapply(bayes_post, function(post) 
    true_value > (mean(post$beta_brain_innovation) - 2*sd(post$beta_brain_innovation)) & 
      true_value < (mean(post$beta_brain_innovation) + 2*sd(post$beta_brain_innovation)),
    logical(1))
  return(data.frame(bias_glm = bias_glm |> mean() |> round(2),
                    bias_bayes = bias_bayes |> mean() |> round(2),
                    sign_glm = sum(bias_glm > 0)/length(bias_glm),
                    sign_bayes = sum(bias_bayes > 0)/length(bias_bayes),
                    coverage_glm = sum(coverage_glm)/length(coverage_glm),
                    coverage_bayes = sum(coverage_bayes)/length(coverage_bayes)))
}

# Compute across cases
bias = rbind(
  extract.bias(glm_1, bayes_1, 0),
  extract.bias(glm_2, bayes_2, 0.2),
  extract.bias(glm_3, bayes_3, 0),
  extract.bias(glm_4, bayes_4, 0),
  extract.bias(glm_5, bayes_5, 0.2),
  extract.bias(glm_6, bayes_6, 0.2),
  extract.bias(glm_7, bayes_7, 0),
  extract.bias(glm_8, bayes_8, 0.2)
)
print(bias)

# Save to csv
write.csv2(bias, 'RESULTS/bias_models.csv', row.names = FALSE)