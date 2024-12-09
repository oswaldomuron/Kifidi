counts <- function(column_data) {
  # Filter out NA values
  column_data <- column_data[!is.na(column_data)]

  # Get unique group identifiers
  unique_groups <- unique(column_data)

  # Initialize an empty dataframe to store the results
  all_data <- data.frame(group = character(0), counts = integer(0))

  # Loop through unique groups and calculate counts
  for (group in unique_groups) {
    count <- sum(column_data == group)
    all_data <- rbind(all_data, data.frame(group = group, counts = count))
  }
  return (all_data)
}
