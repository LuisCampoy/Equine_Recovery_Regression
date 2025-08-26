# Recovery Score Long Term Study. Cofiguration module
# Description: Configuration settings for the analysis script
# Script started: 5/21/2025
# Last revised: 8/20/2025

# Filepath to the ODS file containing the datasets
filepath <- "DataSet_Regression.ods"

# Significance level and power for statistical tests
alpha <- 0.05
power <- 0.95
k <- 2  # Number of predictors

# polynomial model
degree <- 2
predictors_sa <- c("sa_accel", "sa_vel", "sa_gyro")
predictors_sa_2axes <- c("sa_2axes_accel", "sa_2axes_vel", "sa_2axes_gyro")
predictors_ua <- c("sa_accel", "sa_vel", "sa_gyro", "sumua_accel", "sumua_vel",
                   "sumua_gyro")
predictors_ua_2axes <- c("sa_2axes_accel", "sa_2axes_vel", "sa_2axes_gyro",
                         "sumua_accel", "sumua_vel", "sumua_gyro")