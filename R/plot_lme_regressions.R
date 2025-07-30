#' Plot Linear Mixed-Effects (LME) Regressions
#'
#' Fits a linear mixed-effects model using \pkg{nlme} and plots the observed data
#' and regression lines for each group, including fixed and random effects.
#' Optionally plots the overall fixed effect regression line and displays model
#' statistics (R² values and AIC).
#'
#' @param model_or_formula Either a fitted `nlme::lme` model or a formula specifying the fixed effects, e.g. `y ~ x`.
#' @param random A random effects formula, e.g. `~ x | group`. Required only if a formula (not a model) is supplied.
#' @param data A data frame containing the variables in the model. Required only if a formula (not a model) is supplied.
#' @param legend Logical, whether to display a legend (default = TRUE).
#' @param legend_position Position of the legend ("right", "topright", etc.).
#' @param inset Inset for the legend.
#' @param return_model Logical, if TRUE returns the fitted model (default = FALSE).
#' @param lty Line type for group-specific regression lines.
#' @param pch Plotting character for data points.
#' @param lwd Line width for group-specific regression lines (default = 2).
#' @param axes Logical, whether to draw axes (default = TRUE).
#' @param ann Logical, whether to include plot annotations (default = TRUE).
#' @param xlim,ylim Axis limits for the plot.
#' @param main Plot title.
#' @param xlab,ylab Axis labels.
#' @param col Colors for groups. Defaults to distinct colors for each group.
#' @param oma Outer margin areas.
#' @param mar Margins of the plot.
#' @param draw_fixed_effects Logical, whether to plot the fixed-effect regression line (default = FALSE).
#' @param fixed_col Color of the fixed-effect regression line (default = "black").
#' @param fixed_lty Line type of the fixed-effect regression line (default = 2).
#' @param fixed_lwd Line width of the fixed-effect regression line (default = 3).
#' @param ... Additional arguments passed to \code{nlme::lme()} or \code{plot()}.
#'
#' @details
#' The function automatically computes and plots regression lines for each
#' grouping level based on both fixed and random effects. If `draw_fixed_effects = TRUE`,
#' the overall fixed-effect regression line is drawn across the full x-range.
#' The plot legend includes regression equations for each group and, optionally,
#' the fixed effect line. Model performance metrics, including marginal R² (R²m),
#' conditional R² (R²c), and AIC, are displayed in the legend panel.
#'
#' @return Invisibly returns `NULL` unless `return_model = TRUE`, in which case
#' it returns the fitted `nlme::lme` model object.
#'
#' @examples
#' \dontrun{
#' library(nlme)
#' data(Orthodont)
#' plot_lme_regressions(distance ~ age, random = ~ age | Subject, data = Orthodont,
#'                      draw_fixed_effects = TRUE, fixed_col = "red")
#' }
#'
#' @importFrom nlme lme fixef ranef
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
                                 lwd = 2,
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
                                 draw_fixed_effects = FALSE,
                                 fixed_col = "black",
                                 fixed_lty = 2,
                                 fixed_lwd = 3,
                                 fixed_confi = FALSE,      ### NEW: argument to toggle CI
                                 ...) {

  if (!requireNamespace("nlme", quietly = TRUE)) {
    stop("Please install the 'nlme' package to use this function.")
  }

  if (inherits(model_or_formula, "lme")) {
    model <- model_or_formula
    formula <- formula(model$call$fixed)
    random <- formula(model$modelStruct$reStruct[[1]])
    data <- model$data
  } else {
    if (is.null(random) || is.null(data)) {
      stop("If you pass a formula, you must also provide random and data arguments.")
    }
    model <- nlme::lme(fixed = model_or_formula, random = random, data = data, ...)
  }

  response_var <- all.vars(formula)[1]
  predictor_var <- all.vars(formula)[2]
  group_var <- names(model$groups)

  data_clean <- model$data[complete.cases(model$data[, c(response_var, predictor_var, group_var)]),
                           c(response_var, predictor_var, group_var)]

  data_clean[[group_var]] <- factor(data_clean[[group_var]], levels = rownames(nlme::ranef(model)))
  groups <- levels(data_clean[[group_var]])

  fixed_coef <- nlme::fixef(model)
  re <- nlme::ranef(model)
  groups <- rownames(re)

  n_groups <- length(groups)
  colors <- if (is.null(col)) rainbow(n_groups) else rep(col, length.out = n_groups)
  ltys   <- if (is.null(lty)) rep(1, n_groups) else rep(lty, length.out = n_groups)
  pchs   <- if (is.null(pch)) rep(1, n_groups) else rep(pch, length.out = n_groups)
  lwds   <- if (is.null(lwd)) rep(2, n_groups) else rep(lwd, length.out = n_groups)

  labels <- sapply(seq_along(groups), function(i) {
    intercept_i <- fixed_coef[1] + ifelse("(Intercept)" %in% colnames(re), re[groups[i], "(Intercept)"], 0)
    slope_i <- fixed_coef[2] + ifelse(predictor_var %in% colnames(re), re[groups[i], predictor_var], 0)
    sprintf("%s: y = %.3f %+ .3f x", groups[i], intercept_i, slope_i)
  })

  R2m <- R2c <- NA
  if (requireNamespace("MuMIn", quietly = TRUE)) {
    r2 <- MuMIn::r.squaredGLMM(model)
    R2m <- r2[1]; R2c <- r2[2]
  }

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  layout(matrix(c(1, 2), nrow = 1), widths = c(4, 2))
  par(oma = oma, mar = mar)

  if (is.null(main)) main <- paste("LME Regression:", deparse(formula), "\nRandom effect:", deparse(random))
  if (is.null(xlab)) xlab <- predictor_var
  if (is.null(ylab)) ylab <- response_var

  x_data <- data_clean[[predictor_var]]
  y_data <- data_clean[[response_var]]

  if (is.null(xlim)) xlim <- range(x_data, na.rm = TRUE)
  if (is.null(ylim)) ylim <- range(y_data, na.rm = TRUE)

  plot(x_data, y_data,
       col = colors[as.numeric(data_clean[[group_var]])],
       pch = pchs[as.numeric(data_clean[[group_var]])],
       xlim = xlim, ylim = ylim,
       main = if (ann) main else NULL,
       xlab = if (ann) xlab else "",
       ylab = if (ann) ylab else "",
       axes = axes,...)

  # --- NEW: Compute fixed effects line and CI if requested ---
  if (draw_fixed_effects) {
    x_seq <- seq(min(x_data), max(x_data), length.out = 100)
    y_fixed <- fixed_coef[1] + fixed_coef[2] * x_seq

    if (fixed_confi) {
      newdat <- data.frame(x_seq)
      names(newdat) <- predictor_var
      preds <- predict(model, newdata = newdat, level = 0, se.fit = TRUE)
      upper <- preds$fit + 1.96 * preds$se.fit
      lower <- preds$fit - 1.96 * preds$se.fit
      polygon(c(x_seq, rev(x_seq)), c(upper, rev(lower)),
              col = adjustcolor(fixed_col, alpha.f = 0.2), border = NA)
    }
  }
  # ------------------------------------------------------------

  for (i in seq_along(groups)) {
    grp <- groups[i]
    x_grp <- data_clean[data_clean[[group_var]] == grp, predictor_var]
    x_vals <- seq(min(x_grp), max(x_grp), length.out = 100)
    intercept_i <- fixed_coef[1] + ifelse("(Intercept)" %in% colnames(re), re[grp, "(Intercept)"], 0)
    slope_i <- fixed_coef[2] + ifelse(predictor_var %in% colnames(re), re[grp, predictor_var], 0)
    y_vals <- intercept_i + slope_i * x_vals
    lines(x_vals, y_vals, col = colors[i], lwd = lwds[i], lty = ltys[i])
  }

  model_aic <- AIC(model)

  if (draw_fixed_effects) {
    lines(x_seq, y_fixed, col = fixed_col, lwd = fixed_lwd, lty = fixed_lty)
  }

  par(mar = c(0, 0, 0, 0))
  plot.new()
  if (legend) {
    combined_legend <- labels
    combined_col <- colors
    combined_pch <- pchs
    combined_lty <- ltys
    combined_lwd <- lwds

    if (draw_fixed_effects) {
      combined_legend <- c(combined_legend, "Fixed effects")
      combined_col <- c(combined_col, fixed_col)
      combined_pch <- c(combined_pch, NA)
      combined_lty <- c(combined_lty, fixed_lty)
      combined_lwd <- c(combined_lwd, fixed_lwd)
    }

    combined_legend <- c(combined_legend, "",
                         sprintf("R²m = %.3f", R2m),
                         sprintf("R²c = %.3f", R2c),
                         sprintf("AIC = %.1f", model_aic))
    combined_col <- c(combined_col, NA, "black", "black", "black")
    combined_pch <- c(combined_pch, NA, NA, NA, NA)
    combined_lty <- c(combined_lty, NA, NA, NA, NA)
    combined_lwd <- c(combined_lwd, NA, NA, NA, NA)

    legend(legend_position,
           legend = combined_legend,
           col = combined_col,
           pch = combined_pch,
           lty = combined_lty,
           lwd = combined_lwd,
           bty = "o",
           inset = inset,
           cex = 0.7)
  }

  if (return_model) return(model) else invisible(NULL)
}

