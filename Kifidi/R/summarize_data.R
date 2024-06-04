summarize_data <-
function(column_data, group_var1, group_var2 = NULL) {
  if (is.null(group_var2)) {
    # Aggregate the data with one grouping variable
    result <- aggregate(
      column_data,
      by = list(Group = group_var1),
      FUN = function(x) c(
        Mean = mean(x, na.rm = TRUE),
        SD = sd(x, na.rm = TRUE),
        N = sum(!is.na(x)),
        Min = min(x, na.rm = TRUE),
        Max = max(x, na.rm = TRUE),
        Median = median(x, na.rm = TRUE),
        SE = sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
      )
    )
    # Convert the result to a data frame and rename columns
    result_df <- do.call(data.frame, result)
    names(result_df) <- c("Group", "Mean", "SD", "N", "Min", "Max", "Median", "SE")
  } else {
    # Aggregate the data with two grouping variables
    result <- aggregate(
      column_data,
      by = list(Group1 = group_var1, Group2 = group_var2),
      FUN = function(x) c(
        Mean = mean(x, na.rm = TRUE),
        SD = sd(x, na.rm = TRUE),
        N = sum(!is.na(x)),
        Min = min(x, na.rm = TRUE),
        Max = max(x, na.rm = TRUE),
        Median = median(x, na.rm = TRUE),
        SE = sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
      )
    )
    # Convert the result to a data frame and rename columns
    result_df <- do.call(data.frame, result)
    names(result_df) <- c("Group1", "Group2", "Mean", "SD", "N", "Min", "Max", "Median", "SE")
  }

  return(result_df)
}
