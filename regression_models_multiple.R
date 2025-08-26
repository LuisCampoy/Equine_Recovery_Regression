# Regresion_models_multiple_attempts module
# This module contains functions for regression models with multiple attemps
# Script started: 4/8/2025
# Last revised: 8/20/2025

tests_multiple_attempts <- function(df) {
  # linear model with sa and sumua factors (model 1)
  ua_lr <- lm(m_cgs ~ sa_2axes_accel + sumua_accel, data = df)

  # Linear model with sumua factor only (model 2)
  ua_lr_sumua <- lm(m_cgs ~ sumua_accel, data = df)

  # model with sa and sumua factors (model 3)
  ua_nlr <- nls(m_cgs ~ a * (sa_2axes_accel ^ p1) * (sumua_accel ^ p2), data
                = df, start = list(a = 10, p1 = 0.5, p2 = 0.5))

  # Model with sa and sumua using X, Y only for sa (model 4)
  ua_nlr_2axes <- nls(m_cgs ~ a * (sa_2axes_accel ^ p1) * (sumua_accel ^ p2),
                      data = df, start = list(a = 10, p1 = 0.5, p2 = 0.5))

  # model with sumua only (model 5)
  ua_nlr_1 <- nls(m_cgs ~ exp(b * sumua_accel), data = df, start
                  = c(b = 0), alg = "plinear")

  # Appears as best fit (model 6)
  ua_nlr_2 <- nls(m_cgs ~ a * (sumua_accel ^ p1), data = df, start
                  = list(a = 10, p1 = 0.5))

  # Model with exponential sumua (model 7)
  ua_log_2axes <- nls(m_cgs ~ exp(loga * sumua_accel), data = df, start
                      = list(loga = log(1)))

  # Polynomial model with sa and sumua (model 8)
  ua_polynomial_sumua_lrsa <- lm(m_cgs ~ poly(sumua_accel, degree
                                              = 2, raw = TRUE) + sa_2axes_accel,
                                 data = df)

  # Polynomial model with sa and sumua (model 9)
  ua_polynomial_sa_sumua <- lm(m_cgs ~ poly(sumua_accel, degree = 2, raw = TRUE)
                               + poly(sa_2axes_accel, degree = 2, raw = TRUE),
                               data = df)

  # Plot the models
  return(list(ua_lr, ua_lr_sumua, ua_nlr, ua_nlr_2axes, ua_nlr_1, ua_nlr_2, # nolint: return_linter, line_length_linter.
              ua_log_2axes, ua_polynomial_sumua_lrsa,
              ua_polynomial_sa_sumua))
}
