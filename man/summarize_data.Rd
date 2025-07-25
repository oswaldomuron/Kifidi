% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_data.R
\name{summarize_data}
\alias{summarize_data}
\title{Summarize Data by Groups}
\usage{
summarize_data(column_data, group_var1, group_var2 = NULL)
}
\arguments{
\item{column_data}{A numeric vector containing the values to summarize.}

\item{group_var1}{A factor or vector to group by (required).}

\item{group_var2}{An optional second grouping factor or vector.}
}
\value{
A data frame containing summary statistics by group(s):
\describe{
\item{Group1}{The first grouping variable.}
\item{Group2}{The second grouping variable (if provided).}
\item{Mean}{Group mean.}
\item{SD}{Standard deviation.}
\item{N}{Sample size.}
\item{Min}{Minimum value.}
\item{Max}{Maximum value.}
\item{Median}{Median value.}
\item{SE}{Standard error of the mean.}
}
}
\description{
This function summarizes a numeric vector by one or two grouping variables.
It calculates mean, standard deviation, sample size, min, max, median, and standard error.
}
\examples{
data <- c(10, 20, 30, 40, 50, 60)
group1 <- c("A", "A", "B", "B", "C", "C")
group2 <- c(1, 1, 2, 2, 3, 3)
summarize_data(data, group1)
summarize_data(data, group1, group2)

}
\author{
Oswald Omuron
}
