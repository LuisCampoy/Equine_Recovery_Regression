
plot_sa_model <- function(df, model) {
  # Base scatter plot
  plot(df$sa_2axes, df$m_cgs,
       main = "Best Model Fit: Exponential on sa_2axes",
       xlab = "sa_2axes", ylab = "cgs",
       pch = 19, col = "blue")

  # Predicted values from the model
  sa_range <- seq(min(df$sa_2axes), max(df$sa_2axes), length.out = 100)
  pred_exp <- predict(model, newdata = data.frame(sa_2axes = sa_range))

  # Add best-fit line
  lines(sa_range, pred_exp, col = "red", lwd = 2)
  legend("topleft", legend = c("Data", "Exp Fit"), col
         = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1))
}

plot_residuals <- function(model) {

  # Extract fitted values and residuals
  fitted_values <- fitted(model)
  residuals <- resid(model)

  # Residuals vs Fitted Values Plot
  plot(fitted_values, residuals,
       xlab = "Fitted Values",
       ylab = "Residuals",
       main = "Residuals vs Fitted Values")
  abline(h = 0, col = "red")
}

plot_qq <- function(model) {
  library("ggplot2")
  residuals <- resid(model)
  # Q-Q Plot
  ggplot(data = data.frame(residuals), aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line(col = "red") +
    ggtitle("Q-Q Plot of Residuals")
}

plot_histogram <- function(model) {
  residuals <- resid(model)
  # Histogram of Residuals
  hist(residuals, main = "Histogram of Residuals",
       xlab = "Residuals", breaks = 10, col = "lightblue")
}

# Scale-Location Plot
plot_scale_location <- function(model) {
  fitted_values <- fitted(model)
  residuals <- resid(model)
  scale_location <- sqrt(abs(residuals))

  # Scale-Location Plot
  plot(fitted_values, scale_location,
       xlab = "Fitted Values",
       ylab = "Square Root of |Residuals|",
       main = "Scale-Location Plot")
  abline(h = 2, col = "red")
}

# Cook's Distance Plot
plot_cooks_distance <- function(model) {
  cooks_distance <- cooks.distance(model)

  # Cook's Distance Plot
  plot(cooks_distance, type = "h",
       main = "Cook's Distance",
       xlab = "Index", ylab = "Cook's Distance")
  abline(h = 4 / length(cooks_distance), col = "red")
}