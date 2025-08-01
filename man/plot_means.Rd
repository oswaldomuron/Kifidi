% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_means.R
\name{plot_means}
\alias{plot_means}
\title{Plot Mean Values with Error Bars by Group}
\usage{
plot_means(
  summary_df,
  main_title = "Mean Values by Group",
  ylab = NULL,
  xlab = NULL,
  bar_color = "skyblue",
  error_bar_color = "red",
  bar_width = 0.7,
  error_bar_length = 0.1,
  axes = TRUE,
  space = NULL,
  density = NULL,
  angle = 45,
  col = NULL,
  names_arg = NULL,
  xlab_custom = NULL,
  ylab_custom = NULL,
  ann = TRUE,
  xlim = NULL,
  ylim = NULL,
  xaxt = "s",
  las = NULL
)
}
\arguments{
\item{summary_df}{A data frame containing summary statistics including means, standard errors, and group identifiers.}

\item{main_title}{Main title for the plot. Default is "Mean Values by Group".}

\item{ylab}{Deprecated. Label for the y-axis.}

\item{xlab}{Deprecated. Label for the x-axis.}

\item{bar_color}{Color for the bars. Default is "skyblue".}

\item{error_bar_color}{Color for the error bars. Default is "red".}

\item{bar_width}{Width of the bars. Default is 0.7.}

\item{error_bar_length}{Length of the error bar end caps. Default is 0.1.}

\item{axes}{Logical indicating whether axes are drawn. Default is TRUE.}

\item{space}{Numeric or vector indicating spacing between bars.}

\item{density}{Numeric vector for shading density lines on bars.}

\item{angle}{Angle of shading lines on bars.}

\item{col}{Optional colors for shading lines (overrides bar_color).}

\item{names_arg}{Character vector specifying names for x-axis labels. Defaults to group labels in summary_df.}

\item{xlab_custom}{Custom label for the x-axis. Defaults to "Groups".}

\item{ylab_custom}{Custom label for the y-axis. Defaults to "Mean".}

\item{ann}{Logical indicating whether to draw axis labels and titles. Default is TRUE.}

\item{xlim}{Numeric vector of length 2 defining x-axis limits.}

\item{ylim}{Numeric vector of length 2 defining y-axis limits.}

\item{xaxt}{Character specifying x-axis type; "s" for standard, "n" for none. Default is "s".}

\item{las}{Numeric controlling orientation of axis labels.}
}
\value{
Invisibly returns the midpoints of the bars (as from \code{barplot}).
}
\description{
Creates a bar plot of mean values from a summary data frame with optional error bars showing standard errors.
}
\details{
If the input data frame contains two grouping variables (e.g., Group1 and Group2),
these are combined with a hyphen to create the x-axis labels.
The function draws a bar plot of the means with error bars representing Mean ± SE.
}
\note{
This function uses base R graphics and does not depend on external packages.
}
\examples{
example_data <- c(
  445, 372, 284, 247, 328, 98.8, 108.7, 100.8, 123.6, 129.9, 133.3,
  130.1, 123.1, 186.6, 215, 19.4, 19.3, 27.8, 26, 22, 30.9, 19.8,
  16.5, 20.2, 31, 21.1, 16.5, 19.7, 18.9, 27, 161.8, 117, 94.6, 97.5,
  142.7, 109.9, 118.3, 111.4, 96.5, 109, 114.1, 114.9, 101.2, 112.7,
  111.1, 194.8, 169.9, 159.1, 100.8, 130.8, 93.6, 105.7, 178.4, 203,
  172.2, 127.3, 128.3, 110.9, 124.1, 179.1, 293, 197.5, 139.1, 98.1,
  84.6, 81.4, 87.2, 71.1, 70.3, 120.4, 194.5, 167.5, 121, 86.5, 81.7
)

example_group1 <- c(
  rep("Palm", 15), rep("Papyrus", 10), rep("Typha", 15),
  rep("Eucalyptus", 15), rep("Rice farm", 20)
)

example_group2 <- rep(c(50, 40, 30, 20, 10), 15)

example_df <- data.frame(
  Vegetation_types = example_group1,
  Depth_revised = example_group2,
  EC_uS_cm = example_data
)

summary_one_group <- summarize_data(
  example_df$EC_uS_cm,
  example_df$Vegetation_types
)

summary_two_groups <- summarize_data(
  example_df$EC_uS_cm,
  example_df$Vegetation_types,
  example_df$Depth_revised
)

plot_means(
  summary_two_groups,
  ylim = c(0, 350),
  las = 2,
  space = c(0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0)
)
}
\references{
See \code{\link[graphics]{barplot}} and \code{\link[graphics]{arrows}} in base R for details.
}
\seealso{
\code{\link{summary}} for creating summary data frames.
}
\author{
Oswald Omuron
}
