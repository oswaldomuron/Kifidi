#' Kifidi: Tools for summarizing and analyzing grouped environmental data
#'
#' The Kifidi package provides tools for summarizing and visualizing grouped numerical data,
#' especially for environmental and ecological datasets. It includes functions for generating
#' statistical summaries, plotting means with error bars, performing grouped regression analysis,
#' and generating frequency counts by group.
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{summarize_data}}}{Provides statistical summaries (mean, SD, N, etc.) of numeric data grouped by one or two categorical variables.}
#'   \item{\code{\link{plot_means}}}{Creates bar plots of means with optional error bars.}
#'   \item{\code{\link{counts}}}{Generates frequency tables or counts of observations by grouping variables.}
#'   \item{\code{\link{plot_group_regressions}}}{Performs and plots linear regressions grouped by a factor variable.}
#'   \item{\code{\link{plot_lmm_regressions}}}{Plots group-level and fixed-effect regression lines from a linear mixed-effects modelin lme4 package with lmer().}
#'   \item{\code{\link{plot_lme_regressions}}}{Plots group-level lines from a linear mixed-effects model in nlme with lme().}
#'   \item{\code{\link{generate_random_points}}}{Generates random (x, y) sampling coordinates within a rectangular plot area and optionally exports them as CSV.}
#' }
#'
#' @author Oswald Omuron
#' @keywords package
"_PACKAGE"
