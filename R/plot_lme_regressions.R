#' Plot Linear Mixed-Effects (LME) Regressions
#'
#' This function fits a linear mixed-effects model using \pkg{nlme} and
#' plots the regression lines for each group, including fixed and random
#' effects, R² values, and AIC.
#'
#' @param formula A formula specifying the fixed effects, e.g. `y ~ x`.
#' @param random A random effects formula, e.g. `~ x | group`.
#' @param data A data frame containing the variables in the model.
#' @param legend Logical, whether to display a legend (default = TRUE).
#' @param legend_position Position of the legend ("right", "topright", etc.).
#' @param inset Inset for the legend.
#' @param return_model Logical, if TRUE returns the fitted model.
#' @param lty Line type for regression lines.
#' @param pch Plotting character for data points.
#' @param axes Logical, whether to draw axes (default = TRUE).
#' @param ann Logical, whether to include plot annotations (default = TRUE).
#' @param xlim,ylim Axis limits for the plot.
#' @param main Plot title.
#' @param xlab,ylab Axis labels.
#' @param col Colors for groups.
#' @param oma Outer margin areas.
#' @param mar Margins of the plot.
#' @param ... Additional arguments passed to \code{nlme::lme()} or \code{plot()}.
#'
#' @details
#' The function automatically computes and plots regression lines for each
#' grouping level based on both fixed and random effects. It also displays
#' marginal (R²m) and conditional (R²c) R-squared values and the AIC of the model.
#'
#' @return Invisibly returns `NULL` unless `return_model = TRUE`, in which case
#' it returns the fitted `nlme::lme` model object.
#'
#' @examples
#' \dontrun{
#' library(nlme)
#' data(Orthodont)
#' plot_lme_regressions(distance ~ age, random = ~ age | Subject, data = Orthodont)
#' }
#'
#' @importFrom nlme lme lmList fixef ranef
#' @importFrom MuMIn r.squaredGLMM
#' @export


plot_lme_regressions <- function(model_or_formula,
                                 random = NULL,
                                 data = NULL,
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

  # --- NEW: Detect if user passed a fitted model ---
  if (inherits(model_or_formula, "lme")) {
    model <- model_or_formula
    formula <- formula(model$call$fixed)
    random <- formula(model$modelStruct$reStruct[[1]])
    data <- model$data
  } else {
    # User passed formula, random, and data
    formula <- model_or_formula
    if (is.null(random) || is.null(data)) {
      stop("If you pass a formula, you must also provide random and data arguments.")
    }
    model <- nlme::lme(fixed = formula, random = random, data = data, ...)
  }

  # Extract variable names
  response_var <- all.vars(formula)[1]
  predictor_var <- all.vars(formula)[2]
  group_var <- names(model$groups)

  data_clean <- model$data[complete.cases(model$data[, c(response_var, predictor_var, group_var)]),
                           c(response_var, predictor_var, group_var)]
  data_clean[[group_var]] <- as.factor(data_clean[[group_var]])

  # Fixed and random effects
  fixed_coef <- nlme::fixef(model)
  re <- nlme::ranef(model)
  groups <- rownames(re)

  # Set colors and line styles
  n_groups <- length(groups)
  colors <- if (is.null(col)) rainbow(n_groups) else rep(col, length.out = n_groups)
  ltys   <- if (is.null(lty)) rep(1, n_groups) else rep(lty, length.out = n_groups)
  pchs   <- if (is.null(pch)) rep(1, n_groups) else rep(pch, length.out = n_groups)

  # Labels for legend
  labels <- sapply(seq_along(groups), function(i) {
    intercept_i <- fixed_coef[1] + ifelse("(Intercept)" %in% colnames(re), re[groups[i], "(Intercept)"], 0)
    slope_i <- fixed_coef[2] + ifelse(predictor_var %in% colnames(re), re[groups[i], predictor_var], 0)
    sprintf("%s: y = %.3f %+ .3f x", groups[i], intercept_i, slope_i)
  })

  # R² calculation
  R2m <- R2c <- NA
  if (requireNamespace("MuMIn", quietly = TRUE)) {
    r2 <- MuMIn::r.squaredGLMM(model)
    R2m <- r2[1]; R2c <- r2[2]
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

  # Plot points
  plot(x_data, y_data,
       col = colors[as.numeric(data_clean[[group_var]])],
       pch = pchs[as.numeric(data_clean[[group_var]])],
       xlim = xlim, ylim = ylim,
       main = if (ann) main else NULL,
       xlab = if (ann) xlab else "",
       ylab = if (ann) ylab else "",
       axes = axes,...)

  #if (!axes) box()

  # Plot regression lines
  for (i in seq_along(groups)) {
    grp <- groups[i]
    x_grp <- data_clean[data_clean[[group_var]] == grp, predictor_var]
    x_vals <- seq(min(x_grp), max(x_grp), length.out = 100)
    intercept_i <- fixed_coef[1] + ifelse("(Intercept)" %in% colnames(re), re[grp, "(Intercept)"], 0)
    slope_i <- fixed_coef[2] + ifelse(predictor_var %in% colnames(re), re[grp, predictor_var], 0)
    y_vals <- intercept_i + slope_i * x_vals
    lines(x_vals, y_vals, col = colors[i], lwd = 2, lty = ltys[i])
  }

  # Get AIC
  model_aic <- AIC(model)

  # Legend panel
  par(mar = c(0, 0, 0, 0))
  plot.new()
  if (legend) {
    combined_legend <- c(labels, "", sprintf("R²m = %.3f", R2m), sprintf("R²c = %.3f", R2c), sprintf("AIC = %.1f", model_aic))
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
