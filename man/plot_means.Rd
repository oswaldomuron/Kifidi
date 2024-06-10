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
The \code{\link{summary}} function for creating summary data frames.
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
#Load library devtools
library("devtools")

#Install library Kifidi
install_github("OswaldOmuron/Kifidi")

# Load your package (if not already loaded)
library("Kifidi")

# Summarize by one grouping variable
summary_one_group <- summarize_data(example_df$EC_uS_cm, example_df$Vegetation_types)
print(summary_one_group)

# Summarize by two grouping variables
summary_two_groups <- summarize_data(example_df$EC_uS_cm, example_df$Vegetation_types, example_df$Depth_revised)
print(summary_two_groups)

#Plot the outcomes
#Plot the outcomes
par(mar=c(10,5,2,5)) #This sets the margins for better view of the plot area
plot_means(summary_one_group,ylim=c(0,250),las=2)
par(mar=c(10,5,2,5)) #This sets the margins for better view of the plot area
plot_means(summary_two_groups,ylim=c(0,350),las=2,space = c(0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0))
}
}
