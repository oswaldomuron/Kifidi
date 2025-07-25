#' Plot Mean Values with Error Bars by Group
#'
#' Creates a bar plot of mean values from a summary data frame with optional error bars showing standard errors.
#'
#' @param summary_df A data frame containing summary statistics including means, standard errors, and group identifiers.
#' @param main_title Main title for the plot. Default is "Mean Values by Group".
#' @param ylab Deprecated. Label for the y-axis.
#' @param xlab Deprecated. Label for the x-axis.
#' @param bar_color Color for the bars. Default is "skyblue".
#' @param error_bar_color Color for the error bars. Default is "red".
#' @param bar_width Width of the bars. Default is 0.7.
#' @param error_bar_length Length of the error bar end caps. Default is 0.1.
#' @param axes Logical indicating whether axes are drawn. Default is TRUE.
#' @param space Numeric or vector indicating spacing between bars.
#' @param density Numeric vector for shading density lines on bars.
#' @param angle Angle of shading lines on bars.
#' @param col Optional colors for shading lines (overrides bar_color).
#' @param names_arg Character vector specifying names for x-axis labels. Defaults to group labels in summary_df.
#' @param xlab_custom Custom label for the x-axis. Defaults to "Groups".
#' @param ylab_custom Custom label for the y-axis. Defaults to "Mean".
#' @param ann Logical indicating whether to draw axis labels and titles. Default is TRUE.
#' @param xlim Numeric vector of length 2 defining x-axis limits.
#' @param ylim Numeric vector of length 2 defining y-axis limits.
#' @param xaxt Character specifying x-axis type; "s" for standard, "n" for none. Default is "s".
#' @param las Numeric controlling orientation of axis labels.
#'
#' @details
#' If the input data frame contains two grouping variables (e.g., Group1 and Group2),
#' these are combined with a hyphen to create the x-axis labels.
#' The function draws a bar plot of the means with error bars representing Mean ± SE.
#'
#' @return Invisibly returns the midpoints of the bars (as from \code{barplot}).
#'
#' @references
#' See \code{\link[graphics]{barplot}} and \code{\link[graphics]{arrows}} in base R for details.
#'
#' @author Oswald Omuron
#'
#' @note
#' This function uses base R graphics and does not depend on external packages.
#'
#' @seealso
#' \code{\link{summary}} for creating summary data frames.
#'
#' @export
#'
#' @examples
#' example_data <- c(
#'   445, 372, 284, 247, 328, 98.8, 108.7, 100.8, 123.6, 129.9, 133.3,
#'   130.1, 123.1, 186.6, 215, 19.4, 19.3, 27.8, 26, 22, 30.9, 19.8,
#'   16.5, 20.2, 31, 21.1, 16.5, 19.7, 18.9, 27, 161.8, 117, 94.6, 97.5,
#'   142.7, 109.9, 118.3, 111.4, 96.5, 109, 114.1, 114.9, 101.2, 112.7,
#'   111.1, 194.8, 169.9, 159.1, 100.8, 130.8, 93.6, 105.7, 178.4, 203,
#'   172.2, 127.3, 128.3, 110.9, 124.1, 179.1, 293, 197.5, 139.1, 98.1,
#'   84.6, 81.4, 87.2, 71.1, 70.3, 120.4, 194.5, 167.5, 121, 86.5, 81.7
#' )
#'
#' example_group1 <- c(
#'   rep("Palm", 15), rep("Papyrus", 10), rep("Typha", 15),
#'   rep("Eucalyptus", 15), rep("Rice farm", 20)
#' )
#'
#' example_group2 <- rep(c(50, 40, 30, 20, 10), 15)
#'
#' example_df <- data.frame(
#'   Vegetation_types = example_group1,
#'   Depth_revised = example_group2,
#'   EC_uS_cm = example_data
#' )
#'
#' summary_one_group <- summarize_data(
#'   example_df$EC_uS_cm,
#'   example_df$Vegetation_types
#' )
#'
#' summary_two_groups <- summarize_data(
#'   example_df$EC_uS_cm,
#'   example_df$Vegetation_types,
#'   example_df$Depth_revised
#' )
#'
#' plot_means(
#'   summary_two_groups,
#'   ylim = c(0, 350),
#'   las = 2,
#'   space = c(0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0)
#' )




plot_means <-
function(summary_df,
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
                       las = NULL) {

  # Check if there are two groups
  if ("Group2" %in% names(summary_df)) {
    summary_df$Group <- paste(summary_df$Group1, summary_df$Group2, sep = "-")
  }

  # Set names.arg if not provided
  if (is.null(names_arg)) {
    names_arg <- summary_df$Group
  }

  # Set xlab if not provided
  if (is.null(xlab_custom)) {
    xlab_custom <- "Groups"
  }

  # Set ylab if not provided
  if (is.null(ylab_custom)) {
    ylab_custom <- "Mean"
  }

  # Create a bar plot of the means for each group, with axes turned off if specified
  midpoints <- barplot(
    height = summary_df$Mean,
    names.arg = names_arg,
    beside = TRUE,
    col = bar_color,
    density = density, # density parameter
    angle = angle, # angle parameter
    space = space, # Space between bars
    width = bar_width,
    ylim = ylim,  # Set ylim if provided
    xlim = xlim,  # Set xlim if provided
    main = main_title,
    ylab = ylab_custom,
    xlab = xlab_custom,
    axes = axes, # Both x and y axes controlled together
    ann = ann,
    xaxt = xaxt,
    las = las
  )

  # Add error bars for the standard errors
  arrows(
    x0 = midpoints,
    y0 = summary_df$Mean - summary_df$SE,
    x1 = midpoints,
    y1 = summary_df$Mean + summary_df$SE,
    angle = 90,
    code = 3,
    length = error_bar_length,
    col = error_bar_color
  )
}
