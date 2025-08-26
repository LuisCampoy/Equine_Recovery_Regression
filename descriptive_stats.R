# Descriptive Stats
# Script started 4/8/2025
# Last updated 7/28/2025

check_for_normality <- function(df) {
  # Perform the Shapiro-Wilk Test for Normality on each group
  cat("P>0.05 implies the distribution of the data
      are not significantly different and therefore we can assume 
      the normality\n")

  subset_df <- df[, c("age", "weight", "anesth", "standing", "attempts")]

  for (j in seq_len(ncol(subset_df))) {
    column_name <- colnames(subset_df)[j]
    shapiro_test <- shapiro.test(subset_df[[j]])
    cat("variable = ", column_name, ":\n")
    cat("p value = ", shapiro_test$p.value, "\n\n")
  }
}

# Check univariate descriptives
check_univariate_descriptives <- function(df) {
  subset_df <- df[, c("age", "weight", "anesth", "standing", "attempts")]

  for (i in seq_len(ncol(subset_df))) {
    column_name <- colnames(subset_df)[i]
    mean <- mean(subset_df[[i]])
    sd <- sd(subset_df[[i]])
    median <- median(subset_df[[i]])
    min <- min(subset_df[[i]], na.rm = TRUE)
    max <- max(subset_df[[i]], na.rm = TRUE)
    cat("variable= ", column_name, ":\n")
    cat("mean = ", mean, ":\n")
    cat("SD = ", sd, ":\n")
    cat("median = ", median, ":\n")
    cat("min = ", min, ":\n")
    cat("max = ", max, ":\n\n")
  }
}

# Summary Satistics
summary_stats <- function(data, group_col, value_col) {
  data %>%
    group_by(.data[[group_col]]) %>%
    summarize(mean = mean(.data[[value_col]], na.rm = TRUE),
              sd = sd(.data[[value_col]], na.rm = TRUE),
              n = n())

  summary(df_sa)
  summary(df_ua)

}