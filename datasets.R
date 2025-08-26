# Long term regression model. Datasets module
# Description: Load datasets for analysis
# Script started: 4/8/2025
# Last revised: 8/25/2025

load_datasets <- function(filepath, filepath_gwet) {
  library(readODS)
  # Load the datasets from the ODS file
  df_1 <- read_ods(filepath)
  df_2 <- read_ods(filepath_gwet, sheet = "cgs")

  return(list(df_1, df_2)) # nolint: return_linter.
}

create_datasets <- function(dataset) {
  library(magrittr)
  library(dplyr)  # Ensure dplyr is loaded for select() and other functions

  # Remove nan values
  #df_1 <- dataset[[1]] %>%
  #  filter(!is.na(case) & !is.na(tx) & !is.na(cgs) & !is.na(m_cgs)
  #         & !is.na(sa_accel) & !is.na(sa_2axes_accel) & !is.na(sa_vel)
  #         & !is.na(sa_2axes_vel) & !is.na(sa_gyro) & !is.na(sa_2axes_gyro)
  #         & !is.na(single_attempt))

  #df_2 <- dataset[[2]] %>%
  #  filter(!is.na(CGS_Obs3) & !is.na(CGS_Obs4) & !is.na(CGS_Obs5))

  # Select columns for sa_df
  df_sa <- dataset[[1]] %>%
    select(case, tx, cgs, m_cgs, sa_accel, sa_2axes_accel, sa_vel, sa_2axes_vel,
           sa_gyro, sa_2axes_gyro, single_attempt)

  # Group by single attempt (yes/no)
  df_sa <- df_sa %>%
    group_by(single_attempt)

  # Filter out "no"
  df_sa <- df_sa %>%
    filter(single_attempt == "yes")

  # Select columns for ua df
  df_ua <- dataset[[1]] %>%
    select(case, tx, cgs, m_cgs, sa_accel, sa_2axes_accel, sa_vel, sa_2axes_vel,
           sa_gyro, sa_2axes_gyro, sumua_accel, sumua_vel, sumua_gyro,
           single_attempt)

  # Group by single attempt (yes/no)
  df_ua <- df_ua %>%
    group_by(single_attempt)

  # Filter out "yes"
  df_ua <- df_ua %>%
    filter(single_attempt == "no")

  # Select columns 3, 4, 5 and rows starting on row 1, keep headers

  df_gwet <- dataset[[2]] %>%
    select(4, 5, 6) %>%
    slice(25:76)

  return(list(df_sa, df_ua, df_gwet)) # nolint: return_linter.
}

create_descriptive_dataset <- function(df) {
  # load libraries
  library(magrittr)
  library(dplyr)  # Ensure dplyr is loaded for select() and other functions

  # Select columns for sa df
  dataset <- df %>%
    select(case, tx, age, weight, anesth, standing, attempts)

  # remove nan values
  dataset <- dataset %>%
    filter(!is.na(age) & !is.na(weight) & !is.na(anesth) & !is.na(standing)
           & !is.na(attempts))

  return(dataset) # nolint: return_linter.
}