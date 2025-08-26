# Main script for Recovery Score Long Term study
# This script calculates formulae for RS in single attempt
# and several attemps scenarios
# Script started 4/8/2025
# Last updated 8/20/2025

# Import modules
source("analysis.R")
source("config.R")
source("CSV_helper.R")
source("datasets.R")
source("descriptive_stats.R")
source("libraries.R")
source("plots.R")
source("power.R")
source("regression_models_multiple.R")
source("regression_models_single.R")

# Load libraries
load_libraries()

# Load datasets
dataset <- load_datasets(filepath)

# Assign datasets to variables
datasets <- create_datasets(dataset)
df_sa <- datasets[[1]]
df_ua <- datasets[[2]]

View(dataset)
View(df_sa)
View(df_ua)

###############################################################################
##################### Run power calculation ###################################
###############################################################################
# Set parameters for power calculation
tests_sa <- tests_single_attempt(df_sa)
tests_ua <- tests_multiple_attempts(df_ua)
sa_polynomial_sa_sumua <- tests_sa[[9]]
ua_polynomial_sa_sumua <- tests_ua[[9]]

effect_size <- calculate_effect_size(ua_polynomial_sa_sumua)

# Run power calculation
power_calculation <- power_calculation(effect_size, k, alpha, power)

cat(sprintf("Effect size is: %.15f\n", effect_size))
cat("Group size is: ", power_calculation, "\n")

###############################################################################
########### Quick regression on standing and m_cgs ############################
###############################################################################

# This is a quick regression to check the relationship between standing
# and m_cgs linear regression between m_cgs and standing

lm_m_cgs_standing <- lm(m_cgs ~ standing, data = dataset)
summary(lm_m_cgs_standing)

fitted_values <- fitted(lm_m_cgs_standing)
residuals <- resid(lm_m_cgs_standing)

# plot the linear regression and plot regression line
plot(dataset$standing, dataset$m_cgs,
     xlab = "Standing",
     ylab = "m_cgs",
     main = "Linear Regression of m_cgs on Standing")
abline(lm_m_cgs_standing, col = "red")
# save image in current working directory
ggsave("lm_m_cgs_standing.jpeg", plot = last_plot(),
       device = "jpeg",
       width = 8, height = 6, dpi = 300)

# Residual Plot
# Plot the residuals against the fitted values
# This helps you check for:
# **Homoscedasticity**:
# Residuals should have constant variance across all levels of fitted values
# **Linearity**:
# There should be no apparent pattern (e.g., curvature) in the residuals
plot(fitted_values, residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Q-Q Plot (Quantile-Quantile Plot)
# compares the distribution of residuals to a normal distribution
# If residuals follow a normal distribution,
# the points should lie approximately along the 45-degree line
qqPlot(residuals, main = "Q-Q Plot")
qqline(residuals, col = "black")

# save image in curent working directory
ggsave("lm_m_cgs_standing_residuals.jpeg",
       plot = last_plot(),
       device = "jpeg",
       width = 8, height = 6, dpi = 300)

###############################################################################
#################### descriptive statistics ###################################
###############################################################################
# Create Datasets for descriptive statistics
descriptive_dataset_global <- create_descriptive_dataset(dataset)

View(descriptive_dataset_global)

# Check for normality
normality_tests <- check_for_normality(descriptive_dataset_global)

# Check univariate descriptives
check_univariate_descriptives(descriptive_dataset_global)

###############################################################################
##################### Run tests for single attempt ############################
###############################################################################
# Run tests for single attempt
tests_sa <- tests_single_attempt(df_sa)
sa_slr <- tests_sa[[1]]
sa_slr_2axes <- tests_sa[[2]]
sa_nls <- tests_sa[[3]]
sa_nls_2axes <- tests_sa[[4]]
sa_pln_2axes <- tests_sa[[5]]
sa_pln_2axes_gyro <- tests_sa[[6]]
sa_pln_2axes_gyro_2axes <- tests_sa[[7]]
sa_nls_exp_2axes <- tests_sa[[8]]
sa_nls_exp_log_2axes <- tests_sa[[9]]

poly_model_sa <- create_polynomial_model(df_sa, degree, predictors_sa)
poly_model_sa_2axes <- create_polynomial_model(df_sa, degree,
                                               predictors_sa_2axes)



# Analyse Residuals
analysis <- residuals_analysis(poly_model_sa)
analysis[[1]]
analysis[[2]]
analysis[[3]]
analysis[[4]]



# Compare models
comparison <- compare_models(sa_slr, sa_slr_2axes, sa_nls, sa_nls_2axes,
                             sa_pln_2axes, sa_pln_2axes_gyro,
                             sa_pln_2axes_gyro_2axes,
                             sa_nls_exp_2axes, sa_nls_exp_log_2axes,
                             poly_model_sa, poly_model_sa_2axes)

comparison[[1]]
comparison[[2]]
comparison[[3]]
comparison[[4]]
comparison[[5]]
comparison[[6]]
comparison[[7]]
comparison[[8]]
comparison[[9]]
comparison[[10]]
comparison[[11]]
comparison[[12]]
comparison[[13]]
comparison[[14]]

###############################################################################
##################### Run tests for multiple attempts #########################
###############################################################################

# Run tests for multiple attempts
tests_ua <- tests_multiple_attempts(df_ua)
ua_lr <- tests_ua[[1]]
ua_lr_sumua <- tests_ua[[2]]
ua_nlr <- tests_ua[[3]]
ua_nlr_2axes <- tests_ua[[4]]
ua_nlr_1 <- tests_ua[[5]]
ua_nlr_2 <- tests_ua[[6]]
ua_log_2axes <- tests_ua[[7]]
ua_polynomial_sumua_lrsa <- tests_ua[[8]]
ua_polynomial_sa_sumua <- tests_ua[[9]]

# model 10
poly_model_ua <- create_polynomial_model(df_ua, degree, predictors_ua)

# model 11
poly_model_ua_2axes <- create_polynomial_model(df_ua, degree,
                                               predictors_ua_2axes)

# Analyse Residuals
analysis <- residuals_analysis(ua_polynomial_sa_sumua)
analysis[[1]]
analysis[[2]]
analysis[[3]]
analysis[[4]]

# Compare models
comparison <- compare_models(ua_lr, ua_lr_sumua, ua_nlr, ua_nlr_2axes,
                             ua_nlr_1, ua_nlr_2, ua_log_2axes,
                             ua_polynomial_sumua_lrsa, ua_polynomial_sa_sumua,
                             poly_model_ua, poly_model_ua_2axes)

comparison[[1]]
comparison[[2]]
comparison[[3]]
comparison[[4]]
comparison[[5]]
comparison[[6]]
comparison[[7]]
comparison[[8]]
comparison[[9]]
comparison[[10]]
comparison[[11]]
comparison[[12]]
comparison[[13]]
comparison[[14]]

###############################################################################
################################# PLOTS #######################################
###############################################################################
# Plot residuals
# This helps you check for:
# **Homoscedasticity**:
# Residuals should have constant variance across all levels of fitted values
# **Linearity**:
# There should be no apparent pattern (e.g., curvature) in the residuals
plot_residuals(ua_polynomial_sa_sumua)

# Q-Q Plot (Quantile-Quantile Plot)
# compares the distribution of residuals to a normal distribution
# If residuals follow a normal distribution,
# the points should lie approximately along the 45-degree line
plot_qq(ua_polynomial_sa_sumua)

# Histogram of Residuals
# helps visually assess whether they are approximately normally distributed
plot_histogram(ua_polynomial_sa_sumua)

# Scale-Location Plot
# checks the spread of residuals
# Plots the square root of the standardized residuals against
# the fitted values
# Ideally, you should see a horizontal line with equally spread points
plot_scale_location(ua_polynomial_sa_sumua)

# Cook's Distance
plot_cooks_distance(ua_polynomial_sa_sumua)

###############################################################################
##################### Prepare tables for publication ##########################
###############################################################################
prepare_tables(analysis, comparison)
