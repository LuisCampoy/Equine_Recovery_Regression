# Analysis Script
# Script started 7/28/2025
# Last updated 8/20/2025

residuals_analysis <- function(model) {
  library(ggplot2)
  library(plotly)
  library(car)
  library(dplyr)

  fitted_values <- fitted(model)
  residuals <- resid(model)

  # Shapiro-Wilk Test
  # Tests the null hypothesis that the residuals are normally distributed
  # P > 0.05 implies the distribution of the data
  # are not significantly different
  # and therefore we can assume the normality
  shapiro_test <- shapiro.test(residuals)

  # Other diagnostics
  residuals_sqrt_abs <- sqrt(abs(residuals))

  return(list(fitted_values, residuals, # nolint: return_linter.
              shapiro_test,
              residuals_sqrt_abs))
}

# Compare models
compare_models <- function(model_1, model_2, model_3, model_4, model_5, model_6,
                           model_7, model_8, model_9, model_10, model_11) {
  # Higher adjusted R-squared values indicate a better fit
  # Lower Residual Standard Error (RSE) indicates a better fit
  # Lower AIC (Akaike Information Criterion) indicates a better fit
  # Lower BIC (Bayesian Information Criterion) indicates a better fit
  # Lower RMSE (Root Mean Square Error) indicates a better fit
  library("boot")

  sum_model_1 <- summary(model_1)
  sum_model_2 <- summary(model_2)
  sum_model_3 <- summary(model_3)
  sum_model_4 <- summary(model_4)
  sum_model_5 <- summary(model_5)
  sum_model_6 <- summary(model_6)
  sum_model_7 <- summary(model_7)
  sum_model_8 <- summary(model_8)
  sum_model_9 <- summary(model_9)
  sum_model_10 <- summary(model_10)
  sum_model_11 <- summary(model_11)

  aic <- AIC(model_1, model_2, model_3, model_4, model_5, model_6, model_7,
             model_8, model_9, model_10, model_11)
  bic <- BIC(model_1, model_2, model_3, model_4, model_5, model_6, model_7,
             model_8, model_9, model_10, model_11)

  # Calculate RMSE for both models instead of cv.glm
  rmse_1 <- sqrt(mean(residuals(model_1)^2))
  rmse_2 <- sqrt(mean(residuals(model_2)^2))
  rmse_3 <- sqrt(mean(residuals(model_3)^2))
  rmse_4 <- sqrt(mean(residuals(model_4)^2))
  rmse_5 <- sqrt(mean(residuals(model_5)^2))
  rmse_6 <- sqrt(mean(residuals(model_6)^2))
  rmse_7 <- sqrt(mean(residuals(model_7)^2))
  rmse_8 <- sqrt(mean(residuals(model_8)^2))
  rmse_9 <- sqrt(mean(residuals(model_9)^2))
  rmse_10 <- sqrt(mean(residuals(model_10)^2))
  rmse_11 <- sqrt(mean(residuals(model_11)^2))

  rmse_comparison <- data.frame(Model = c("Model_1", "Model_2", "Model_3",
                                          "Model_4", "Model_5", "Model_6",
                                          "Model_7", "Model_8", "Model_9",
                                          "Model_10", "Model_11"),
                                RMSE = c(rmse_1, rmse_2, rmse_3, rmse_4,
                                         rmse_5, rmse_6, rmse_7, rmse_8,
                                         rmse_9, rmse_10, rmse_11))

  return(list(sum_model_1, sum_model_2, sum_model_3, sum_model_4, sum_model_5, # nolint: return_linter, line_length_linter.
              sum_model_6, sum_model_7, sum_model_8, sum_model_9, sum_model_10,
              sum_model_11, aic, bic, rmse_comparison))
}