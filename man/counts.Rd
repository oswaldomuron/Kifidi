\name{counts}
\alias{counts}
\title{
Count Unique Groups in a Column
}
\description{
This function calculates the frequency of each unique value in a given column of data, excluding \code{NA} values.
}
\usage{
counts(column_data)
}
\arguments{
  \item{column_data}{
A vector of data (numeric, character, or factor) from which the unique groups and their frequencies are calculated.
}
}
\details{
The function first removes any \code{NA} values from the input data and identifies the unique groups.
It then counts the occurrences of each unique value and returns the results as a data frame with two columns: \code{group} (the unique values) and \code{counts} (their respective frequencies).
}
\value{
A data frame with the following components:
\item{group}{The unique values from the input data.}
\item{counts}{The frequency of each unique value.}
}
\references{
For additional information on related functions, see \code{\link{unique}} and \code{\link{table}} in base R.
}
\author{
Your Name Here
}
\note{
This function is intended for small to medium-sized datasets. For larger datasets, consider using \code{table} for better performance.
}
\seealso{
\code{\link{unique}}, \code{\link{table}}
}
\examples{
# Example usage of counts function
data <- c("A", "B", "A", "C", "B", "B", NA, "A", "C")
result <- counts(data)
print(result)

# Output should look like:
#   group counts
# 1     A      3
# 2     B      3
# 3     C      2
}
\keyword{data manipulation}
\keyword{aggregation}
