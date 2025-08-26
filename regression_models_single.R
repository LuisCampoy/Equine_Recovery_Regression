# Regresion_models_single attempt module
# This module contains functions for regression models with single attempts
# Script started: 4/8/2025
# Last revised: 8/20/2025

tests_single_attempt <- function(df_sa) {
  # Simple Linear Regression (model 1)
  sa_slr <- lm(m_cgs ~ sa_accel, data = df_sa)

  # Simple linear Regression w 2axes (model 2)
  sa_slr_2axes <- lm(m_cgs ~ sa_2axes_accel, data = df_sa)

  # Different formula for single attempt (Non Linear Regression) with 3 axes
  # (model 3)
  sa_nls <- nls(m_cgs ~ a * (sa_accel ^ p1), data = df_sa, start
                = list(a = 20, p1 = 0.5))

  # Same formula for single attempt (Non Linear Regression) with 2 axes
  # (model 4)
  sa_nls_2axes <- nls(m_cgs ~ a * (sa_2axes_accel ^ p1), data = df_sa, start
                      = list(a = 20, p1 = 0.5))

  # Polynomial model 2axes (best correlation: 0.672) (model 5)
  sa_pln_2axes <- lm(m_cgs ~ poly(sa_2axes_accel, degree = 2, raw = TRUE), data
                     = df_sa)

  # Polynomial model with sa_2axes and sa_gyro (model 6)
  sa_pln_2axes_gyro <- lm(m_cgs ~ poly(sa_2axes_accel, degree = 2, raw = TRUE)
                          + sa_gyro, data = df_sa)

  # Polynomial model with sa_2axes and sa_gyro_2axes (model 7)
  sa_pln_2axes_gyro_2axes <- lm(m_cgs ~ poly(sa_2axes_accel, degree = 2, raw
                                             = TRUE) + sa_2axes_gyro, data
                                = df_sa)

  # Non Linear Regression with exponential function (model 8)
  sa_nls_exp_2axes <- nls(m_cgs ~ exp(loga * sa_2axes_accel), data
                          = df_sa, start = list(loga = log(1)))

  # Non Linear Regression with exponential function and log (model 9)
  sa_nls_exp_log_2axes <- nls(m_cgs ~ exp(loga * log(sa_2axes_accel)), data
                              = df_sa, start = list(loga = log(1)))

  return(list(sa_slr, sa_slr_2axes, sa_nls, sa_nls_2axes, sa_nls_exp_log_2axes, # nolint: return_linter, line_length_linter.
              sa_pln_2axes, sa_pln_2axes_gyro, sa_pln_2axes_gyro_2axes,
              sa_nls_exp_2axes))
}

create_polynomial_model <- function(df, degree, predictors) {

  # Create polynomial terms for each predictor
  poly_terms <- sapply(predictors, function(p) {
    paste0("poly(", p, ", degree = ", degree, ", raw = TRUE)")
  })

  # Combine terms into a single formula string
  formula_str <- paste("m_cgs ~", paste(poly_terms, collapse = " + "))

  # Fit the polynomial model
  model <- lm(as.formula(formula_str), data = df)

  return(model) # nolint: return_linter.
}