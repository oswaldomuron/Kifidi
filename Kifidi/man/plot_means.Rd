\name{plot_means}
\alias{plot_means}
\title{Plot Means}
\description{
This function plots the means of a summary data frame with optional error bars.
}
\usage{
plot_means(summary_df,
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
           las = NULL)
}
\arguments{
  \item{summary_df}{A summary data frame containing the means and standard errors for each group.}
  \item{main_title}{Main title for the plot. Default is "Mean Values by Group".}
  \item{ylab}{Label for the y-axis.}
  \item{xlab}{Label for the x-axis.}
  \item{bar_color}{Color for the bars. Default is "skyblue".}
  \item{error_bar_color}{Color for the error bars. Default is "red".}
  \item{bar_width}{Width of the bars. Default is 0.7.}
  \item{error_bar_length}{Length of the error bars. Default is 0.1.}
  \item{axes}{Logical value indicating whether to draw axes on the plot. Default is TRUE.}
  \item{space}{Spacing between bars.}
  \item{density}{Density of shading lines.}
  \item{angle}{Angle of shading lines.}
  \item{col}{Color of shading lines.}
  \item{names_arg}{Vector of names for the x-axis.}
  \item{xlab_custom}{Custom label for the x-axis. Default is "Groups".}
  \item{ylab_custom}{Custom label for the y-axis. Default is "Mean".}
  \item{ann}{Logical value indicating whether to draw annotations on the plot. Default is TRUE.}
  \item{xlim}{Limits for the x-axis.}
  \item{ylim}{Limits for the y-axis.}
  \item{xaxt}{Type of x-axis labeling.}
  \item{las}{Style of axis labels.}
}
\details{
If the summary data frame contains two grouping variables (Group1 and Group2), they will be combined to form the x-axis labels.
}
\value{
This function produces a bar plot with optional error bars.
}
\references{
Please refer to the documentation of the \code{barplot} and \code{arrows} functions in the base R package.
}
\author{
Oswald Omuron
}
\note{
Additional notes can be added here.
}

\seealso{
The \code{\link{summarize_data}} function for creating summary data frames.
}

\examples{
# Load the package
library(Kifidi)

##---- Should be DIRECTLY executable !! ----
##-- Define example data
example_data <- rnorm(100)
example_group1 <- sample(letters[1:4], 100, replace = TRUE)
example_group2 <- sample(LETTERS[1:2], 100, replace = TRUE)

# Create summary data frame
summary_df <- data.frame(
  Group1 = example_group1,
  Group2 = example_group2,
  Mean = sample(10:30, 100, replace = TRUE),
  SE = runif(100, min = 0.5, max = 2)
)

# Plot means
plot_means(summary_df)
}

