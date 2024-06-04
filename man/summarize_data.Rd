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
\code{\link{aggregate}},
\code{\link{summarize_data}}
}
\examples{
# Example data
example_data <- c(
  445, 372, 284, 247, 328, 98.8, 108.7, 100.8, 123.6, 129.9, 133.3, 130.1, 123.1, 186.6, 215,
  19.4, 19.3, 27.8, 26, 22, 30.9, 19.8, 16.5, 20.2, 31, 21.1, 16.5, 19.7, 18.9, 27,
  161.8, 117, 94.6, 97.5, 142.7, 109.9, 118.3, 111.4, 96.5, 109, 114.1, 114.9, 101.2, 112.7, 111.1,
  194.8, 169.9, 159.1, 100.8, 130.8, 93.6, 105.7, 178.4, 203, 172.2, 127.3, 128.3, 110.9, 124.1, 179.1,
  293, 197.5, 139.1, 98.1, 84.6, 81.4, 87.2, 71.1, 70.3, 120.4, 194.5, 167.5, 121, 86.5, 81.7
)

example_group1 <- c(
  rep("Palm", 15), rep("Papyrus", 10), rep("Typha", 15), rep("Eucalyptus", 15), rep("Rice farm", 20)
)

example_group2 <- rep(c(50, 40, 30, 20, 10), 15)

# Create dataframe
example_df <- data.frame(
  Vegetation_types = example_group1,
  Depth_revised = example_group2,
  EC_uS_cm = example_data
)

# Load your package (if not already loaded)
library("Kifidi")

# Summarize by one grouping variable
summary_one_group <- summarize_data(example_df$EC_uS_cm, example_df$Vegetation_types)
print(summary_one_group)

# Summarize by two grouping variables
summary_two_groups <- summarize_data(example_df$EC_uS_cm, example_df$Vegetation_types, example_df$Depth_revised)
print(summary_two_groups)
}

\keyword{data}
\keyword{summary}
