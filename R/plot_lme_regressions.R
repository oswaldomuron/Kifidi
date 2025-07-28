#' Plot Linear Mixed-Effects Model Regressions by Group
#'
#' Fits a linear mixed-effects model (LME) using the `nlme::lme` function
#' and plots the regression lines for each random effect group along with the data points.
#' Optionally displays a legend with regression equations, R² (marginal and conditional), and AIC.
#'
#' @param formula A two-sided formula specifying the fixed effects (e.g., \code{y ~ x}).
#' @param random A formula or list specifying the random effects (e.g., \code{~ 1 | group}).
#' @param data A data frame containing the variables in the model.
#' @param legend Logical, whether to display the legend with group regression info. Default is \code{TRUE}.
#' @param legend_position Position of the legend. One of \code{"topright"}, \code{"bottomright"}, \code{"bottomleft"}, \code{"topleft"}, \code{"right"}, \code{"left"}, \code{"top"}, \code{"bottom"}. Default is \code{"right"}.
#' @param inset Numeric scalar or vector indicating how much to inset the legend from the margins (in fractions of the plot region). Default is 0.
#' @param return_model Logical, whether to return the fitted \code{nlme::lme} model object. Default is \code{FALSE}.
#' @param lty Line type(s) for regression lines. Can be a vector recycled across groups. Default is \code{NULL} which uses \code{1}.
#' @param pch Plotting character(s) for points. Can be a vector recycled across groups. Default is 16.
#' @param axes Logical, whether to draw axes. Default is \code{TRUE}.
#' @param ann Logical, whether to annotate the plot with titles and axis labels. Default is \code{TRUE}.
#' @param xlim Numeric vector of length 2 specifying x-axis limits. Default is \code{NULL} (automatic).
#' @param ylim Numeric vector of length 2 specifying y-axis limits. Default is \code{NULL} (automatic).
#' @param main Main title for the plot. Default is \code{NULL}, which generates a default title.
#' @param xlab Label for the x-axis. Default is \code{NULL}, which uses the predictor variable name.
#' @param ylab Label for the y-axis. Default is \code{NULL}, which uses the response variable name.
#' @param col Colors for groups. Can be a vector recycled across groups. Default is \code{NULL} which uses \code{rainbow()} palette.
#' @param oma Numeric vector of length 4 giving outer margins in lines (bottom, left, top, right). Default is \code{c(0, 0, 0, 0)}.
#' @param mar Numeric vector of length 4 giving margins in lines (bottom, left, top, right). Default is \code{c(5, 4, 4, 2)}.
#' @param ... Additional arguments passed to \code{nlme::lme} and \code{plot}.
#'
#' @details
#' The function fits a linear mixed-effects model to the data with specified fixed and random effects,
#' extracts group-level regression coefficients (intercept and slope), and plots data points colored and
#' shaped by group. It then overlays regression lines for each group.
#'
#' If the \pkg{MuMIn} package is installed, it also calculates marginal and conditional R² values.
#' The legend shows regression equations for each group, R² values, and the AIC of the fitted model.
#'
#' @return
#' Invisibly returns \code{NULL} by default. If \code{return_model = TRUE}, returns the fitted \code{nlme::lme} model object.
#'
#' @examples
#' \dontrun{
#' library(nlme)
#' data(Orthodont)
#' plot_lme_regressions(distance ~ age, random = ~ age | Subject, data = Orthodont)
#' }
#'
#' @importFrom nlme lme fixef ranef
#' @importFrom graphics plot lines legend layout par box plot.new
#' @importFrom stats AIC complete.cases
#' @export
#'

plot_lme_regressions <- function(formula, random, data,
                                 legend = TRUE,
                                 legend_position = "right",
                                 inset = 0,
                                 return_model = FALSE,
                                 lty = NULL,
                                 pch = 16,
                                 axes = TRUE,
                                 ann = TRUE,
                                 xlim = NULL,
                                 ylim = NULL,
                                 main = NULL,
                                 xlab = NULL,
                                 ylab = NULL,
                                 col = NULL,
                                 oma = c(0, 0, 0, 0),
                                 mar = c(5, 4, 4, 2),
                                 ...) {

  if (!requireNamespace("nlme", quietly = TRUE)) {
    stop("Please install the 'nlme' package to use this function.")
  }

  get_group_var <- function(random_arg) {
    if (is.list(random_arg)) {
      if (length(random_arg) != 1) stop("Only single grouping factor supported.")
      group_var <- names(random_arg)[1]
    } else if (inherits(random_arg, "formula")) {
      random_chr <- deparse(random_arg)
      parts <- strsplit(random_chr, "\\|")[[1]]
      if (length(parts) != 2) stop("Could not parse grouping variable from random formula")
      group_var <- trimws(parts[2])
    } else {
      stop("random argument must be a formula or a list")
    }
    return(group_var)
  }

  response_var <- all.vars(formula)[1]
  predictor_var <- all.vars(formula)[2]
  group_var <- get_group_var(random)

  vars_needed <- c(response_var, predictor_var, group_var)
  data_clean <- data[complete.cases(data[, vars_needed]), vars_needed]
  if (nrow(data_clean) == 0) stop("No complete cases found for the specified variables.")
  data_clean[[group_var]] <- as.factor(data_clean[[group_var]])

  model <- nlme::lme(fixed = formula, random = random, data = data_clean, ...)

  fixed_coef <- nlme::fixef(model)
  re <- nlme::ranef(model)
  groups <- rownames(re)

  # Set styles
  n_groups <- length(groups)
  colors <- if (is.null(col)) rainbow(n_groups) else rep(col, length.out = n_groups)
  ltys   <- if (is.null(lty)) rep(1, n_groups) else rep(lty, length.out = n_groups)
  pchs   <- if (is.null(pch)) rep(1, n_groups) else rep(pch, length.out = n_groups)

  n_re <- ncol(re)
  colnames_re <- colnames(re)

  labels <- character(n_groups)
  for (i in seq_along(groups)) {
    if (n_re == 2) {
      intercept_i <- fixed_coef[1] + re[groups[i], "(Intercept)"]
      slope_i <- fixed_coef[2] + re[groups[i], predictor_var]
    } else if (n_re == 1) {
      if (colnames_re == "(Intercept)") {
        intercept_i <- fixed_coef[1] + re[groups[i], 1]
        slope_i <- fixed_coef[2]
      } else {
        intercept_i <- fixed_coef[1]
        slope_i <- fixed_coef[2] + re[groups[i], 1]
      }
    } else {
      intercept_i <- fixed_coef[1]
      slope_i <- fixed_coef[2]
    }
    labels[i] <- sprintf("%s: y = %.3f %+ .3f x", groups[i], intercept_i, slope_i)
  }

  # R2 calculation
  R2m <- NA
  R2c <- NA
  if (requireNamespace("MuMIn", quietly = TRUE)) {
    r2 <- MuMIn::r.squaredGLMM(model)
    R2m <- r2[1]
    R2c <- r2[2]
  }

  # Save and set layout
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  layout(matrix(c(1, 2), nrow = 1), widths = c(4, 2))
  par(oma = oma, mar = mar)

  # Set labels
  if (is.null(main)) main <- paste("LME Regression:", deparse(formula), "\nRandom effect:", deparse(random))
  if (is.null(xlab)) xlab <- predictor_var
  if (is.null(ylab)) ylab <- response_var

  x_data <- data_clean[[predictor_var]]
  y_data <- data_clean[[response_var]]

  if (is.null(xlim)) xlim <- range(x_data, na.rm = TRUE)
  if (is.null(ylim)) ylim <- range(y_data, na.rm = TRUE)

  # Main plot
  plot(x_data, y_data,
       col = colors[as.numeric(data_clean[[group_var]])],
       pch = pchs[as.numeric(data_clean[[group_var]])],
       xlim = xlim, ylim = ylim,
       main = if (ann) main else NULL,
       xlab = if (ann) xlab else "",
       ylab = if (ann) ylab else "",
       axes = axes,
       ...)

  if (!axes) box()

  for (i in seq_along(groups)) {
    grp <- groups[i]
    x_grp <- data_clean[data_clean[[group_var]] == grp, predictor_var]
    x_vals <- seq(min(x_grp), max(x_grp), length.out = 100)

    if (n_re == 2) {
      intercept_i <- fixed_coef[1] + re[grp, "(Intercept)"]
      slope_i <- fixed_coef[2] + re[grp, predictor_var]
    } else if (n_re == 1) {
      if (colnames_re == "(Intercept)") {
        intercept_i <- fixed_coef[1] + re[grp, 1]
        slope_i <- fixed_coef[2]
      } else {
        intercept_i <- fixed_coef[1]
        slope_i <- fixed_coef[2] + re[grp, 1]
      }
    } else {
      intercept_i <- fixed_coef[1]
      slope_i <- fixed_coef[2]
    }

    y_vals <- intercept_i + slope_i * x_vals
    lines(x_vals, y_vals, col = colors[i], lwd = 2, lty = ltys[i])
  }

  # Get AIC
  model_aic <- AIC(model)


  # Legend panel
  par(mar = c(0, 0, 0, 0))
  plot.new()
  if (legend) {
    combined_legend <- c(labels, "", sprintf("R²m = %.3f", R2m), sprintf("R²c = %.3f", R2c),sprintf("AIC = %.1f", model_aic))
    legend(legend_position,
           legend = combined_legend,
           col = c(colors, rep("black", 4)),
           pch = c(pchs, rep(NA, 4)),
           lty = c(ltys, rep(NA, 4)),
           lwd = c(rep(2, n_groups), rep(NA, 4)),
           bty = "o",
           inset = inset,
           cex = 0.7)
  }

  if (return_model) return(model) else invisible(NULL)
}
