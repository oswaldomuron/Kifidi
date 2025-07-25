#' Count Unique Groups in a Column
#'
#' This function calculates the frequency of each unique value in a given column of data, excluding \code{NA} values.
#'
#' @param column_data A vector of data (numeric, character, or factor) from which the unique groups and their frequencies are calculated.
#'
#' @details
#' The function first removes any \code{NA} values from the input data and identifies the unique groups.
#' It then counts the occurrences of each unique value using a loop and returns the results as a data frame with two columns:
#' \code{group} (the unique values) and \code{counts} (their respective frequencies).
#'
#' @return A data frame with:
#' \describe{
#'   \item{group}{The unique values from the input data.}
#'   \item{counts}{The frequency of each unique value.}
#' }
#'
#' @note
#' This implementation uses a loop and may be slower for very large datasets. For faster performance, consider using \code{table()} or \code{dplyr::count()}.
#'
#' @seealso \code{\link[base]{unique}}, \code{\link[base]{table}}, \code{\link[dplyr]{count}}
#'
#' @examples
#' data <- c("A", "B", "A", "C", "B", "B", NA, "A", "C")
#' result <- counts(data)
#' print(result)
#'
#' @author Oswald Omuron
#' @export


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
