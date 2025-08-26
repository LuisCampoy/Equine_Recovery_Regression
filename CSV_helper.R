prepare_tables <- function(analysis, comparison) {
  # Create a dataframe with diagnostic results

  table_1_df <-
    data.frame(
      Model = c("model 1", "model 2", "model 3", "model 4", "model 5",
                "model 6", "model 7", "model 8", "model 9"),
      AIC = sapply(comparison, function(x) x$aic),
      BIC = sapply(comparison, function(x) x$bic),
      RMSE = sapply(comparison, function(x) x$rmse)
    )

  # Write the tables to CSV files
  write.csv(table_1_df, "table_1_diagnostic_results.csv", row.names = FALSE)

}