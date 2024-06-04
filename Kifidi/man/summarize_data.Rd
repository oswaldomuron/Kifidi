\name{summarize_data}
\alias{summarize_data}
\title{
Summarize Data by Groups
}
\description{
This function summarizes the provided data column by one or two grouping variables. It calculates the mean, standard deviation, sample size, minimum, maximum, median, and standard error.
}
\usage{
summarize_data(column_data, group_var1, group_var2 = NULL)
}
\arguments{
  \item{column_data}{
    A numeric vector containing the data to be summarized.
  }
  \item{group_var1}{
    A factor or vector to group the data by.
  }
  \item{group_var2}{
    An optional second factor or vector to group the data by.
  }
}
\details{
If only one grouping variable is provided, the function will summarize the data by that variable. If two grouping variables are provided, it will summarize the data by both variables.
}
\section{Output}{
  A data frame with the following columns:
  \itemize{
    \item{Group1, Group2.}{ The grouping variables.}
    \item{Mean.}{ The mean of the data for each group.}
    \item{SD.}{ The standard deviation of the data for each group.}
    \item{N.}{ The sample size for each group.}
    \item{Min.}{ The minimum value of the data for each group.}
    \item{Max.}{ The maximum value of the data for each group.}
    \item{Median.}{ The median value of the data for each group.}
    \item{SE.}{ The standard error of the mean for each group.}
  }
}

\references{
No references available.
}
\author{
Oswald Omuron
}
\note{
Ensure that the grouping variables and the data column are of the same length.
}
\seealso{
\code{\link{aggregate}}
}
\examples{
# Load the package
library("Kifidi")

# Example data
example_data <- rnorm(100)
example_group1 <- sample(letters[1:4], 100, replace = TRUE)
example_group2 <- sample(LETTERS[1:2], 100, replace = TRUE)

# Summarize by one grouping variable
summary_one_group <- summarize_data(example_data, example_group1)
print(summary_one_group)

# Summarize by two grouping variables
summary_two_groups <- summarize_data(example_data, example_group1, example_group2)
print(summary_two_groups)
}

\keyword{data}
\keyword{summary}
