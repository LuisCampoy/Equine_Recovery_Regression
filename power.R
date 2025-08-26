# Power stats script for the CGS_v_RS study
# This script does power calculation based on Cohen's f2 effect size
# and number of predictors (k)
# Script started 5/21/2025
# Last revision 5/21/2025

calculate_effect_size <- function(model) {
  # Calculate Cohen's F2 effect size
  r2 <- summary(model)$r.squared
  f2 <- r2 / (1 - r2)
  return(f2)
}

power_calculation <- function(effect_size, k, alpha, power) {
  # Calculate the total sample size required for the given power and alpha
  library(pwr)
  result <- pwr.f2.test(u = k, f2 = effect_size, sig.level
                        = alpha, power = power)
  # The total sample size needed is:
  required_n <- ceiling(result$v + k + 1)

  return(required_n)
}
