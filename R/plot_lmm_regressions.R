#' Plot Linear Mixed Model (LMM) Regressions by Group
#'
#' This function fits a linear mixed-effects model using `lme4::lmer()` and plots
#' the data along with regression lines and equations for each group.
#' It supports models with random intercepts, random slopes, or both,
#' and can label equations using per-group estimates of slope and intercept.
#'
#' @param formula A mixed-effects model formula of the form `y ~ x + (random_effect_structure)`,
#'   such as `y ~ x + (1 | group)` or `y ~ x + (x | group)`. Currently, only one continuous fixed-effect predictor is supported.
#' @param data A data frame containing the variables used in the formula.
#' @param colors Optional vector of colors to use for each group. If not specified, a rainbow palette is used.
#' @param lty Line type(s) for regression lines. Can be a single value or a vector for group-specific styles.
#' @param lwd Line width(s) for regression lines. Can be a single value or a vector for group-specific widths.
#' @param pch Plotting symbol(s) for data points. Can be a single value or a vector for group-specific symbols.
#' @param xlab Label for the x-axis. Defaults to the predictor variable name.
#' @param ylab Label for the y-axis. Defaults to the response variable name.
#' @param main Title for the plot.
#' @param draw_fixed_line Logical. If TRUE, draws the fixed-effects regression line as a dashed black line. Default is TRUE.
#' @param draw_group_lines Logical. If TRUE, draws regression lines for each group using conditional model coefficients. Default is TRUE.
#' @param label_equations Logical. If TRUE, includes each group's regression equation and R² in the legend. Default is TRUE.
#' @param legend_position Position of the legend (e.g., `"topright"`, `"bottomleft"`). Passed to `legend()`.
#' @param legend Logical. If TRUE, displays a legend. Default is TRUE.
#' @param ann Logical. If FALSE, suppresses annotation of axes (titles and labels). Passed to `plot()`. Default is TRUE.
#' @param axes Logical. If FALSE, suppresses drawing of axes. Passed to `plot()`. Default is TRUE.
#' @param return_model Logical. If TRUE, returns the fitted `lmer` model object. Default is FALSE.
#' @param ... Additional graphical parameters passed to `plot()` or `points()`.
#'
#' @details
#' This function fits a linear mixed-effects model using `lme4::lmer()` and
#' extracts group-specific slopes and intercepts using `coef(model)[[group_var]]`,
#' as reported by `lme4`. It assumes only a single continuous fixed-effect predictor.
#'
#' When `label_equations = TRUE`, each group line is labeled in the legend with
#' its regression equation and R², calculated from an ordinary least squares fit to that group.
#' The overall fixed-effects regression line is drawn with a dashed black line if `draw_fixed_line = TRUE`.
#'
#' This visualization is particularly useful for understanding how group-specific trends differ
#' from the overall population trend in mixed-effects models.
#'
#' @return Invisibly returns the model object of class `lmerMod` if `return_model = TRUE`; otherwise, returns `NULL`.
#'
#' @examples
#' \dontrun{
#' library(lme4)
#'
#' # Simulated example
#' set.seed(123)
#' df <- data.frame(
#'   group = rep(LETTERS[1:3], each = 20),
#'   x = rep(rnorm(20, mean = 5, sd = 1), 3),
#'   y = unlist(lapply(1:3, function(i) 2 + i + 0.5 * rnorm(20, x = x, sd = 1)))
#' )
#'
#' # Random intercepts only
#' plot_lmm_regressions(
#'   y ~ x + (1 | group),
#'   data = df,
#'   main = "Random Intercepts Only: Group-specific intercepts, shared slope",
#'   draw_group_lines = TRUE,
#'   label_equations = TRUE
#' )
#'
#' # Random slopes only
#' plot_lmm_regressions(
#'   y ~ x + (x - 1 | group),
#'   data = df,
#'   main = "Random Slopes Only: Group-specific slopes, shared intercept",
#'   draw_group_lines = TRUE,
#'   label_equations = TRUE
#' )
#'
#' # Random intercepts and slopes
#' plot_lmm_regressions(
#'   y ~ x + (x | group),
#'   data = df,
#'   main = "Random Slopes and Intercepts",
#'   draw_group_lines = TRUE,
#'   label_equations = TRUE
#' )
#' }
#'
#' @importFrom lme4 lmer
#' @importFrom stats lm formula
#' @export




plot_lmm_regressions <- function(formula, data,
                                 colors = NULL,
                                 lty = 1, lwd = 2, pch = 16,
                                 xlab = NULL, ylab = NULL, main = NULL,
                                 draw_fixed_line = TRUE, draw_group_lines = TRUE,
                                 label_equations = TRUE, legend_position = "topright",
                                 inset = 0,      # added
                                 xpd = TRUE,     # added
                                 ann = TRUE, axes = TRUE,
                                 legend = TRUE, return_model = FALSE, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Please install the 'lme4' package.")
  }
  library(lme4)

  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(xpd = xpd)

  model <- lmer(formula, data = data)
  mf <- model.frame(model)
  y <- model.response(mf)
  x_var <- all.vars(formula)[2]
  x <- mf[[x_var]]

  group_var <- names(getME(model, "flist"))[1]
  group <- mf[[group_var]]
  levels_group <- levels(group)
  n_groups <- length(levels_group)

  if (is.null(colors)) colors <- rainbow(n_groups)
  colors <- rep(colors, length.out = n_groups)
  lty_vec <- rep(lty, length.out = n_groups)
  lwd_vec <- rep(lwd, length.out = n_groups)
  pch_vec <- rep(pch, length.out = n_groups)

  plot(x, y, type = "n",
       xlab = ifelse(is.null(xlab), x_var, xlab),
       ylab = ifelse(is.null(ylab), all.vars(formula)[1], ylab),
       main = main, ann = ann, axes = axes, ...)

  fe <- fixef(model)
  coef_df <- coef(model)[[group_var]]
  legend_labels <- character()

  for (i in seq_along(levels_group)) {
    g <- levels_group[i]
    idx <- which(group == g)
    xi <- x[idx]; yi <- y[idx]
    points(xi, yi, col = colors[i], pch = pch_vec[i], ...)

    if (draw_group_lines) {
      intercept <- coef_df[g, "(Intercept)"]
      if (ncol(coef_df) > 1) {
        slope <- coef_df[g, x_var]
        x_seq <- seq(min(xi), max(xi), length.out = 100)
        y_seq <- intercept + slope * x_seq
        lines(x_seq, y_seq, col = colors[i], lty = lty_vec[i], lwd = lwd_vec[i])
      } else {
        slope <- NA
      }

      if (label_equations && !is.na(slope)) {
        r2 <- summary(lm(yi ~ xi))$r.squared
        label <- sprintf("%s: y = %.3f x + %.3f, R² = %.3f",
                         g, slope, intercept, r2)
      } else {
        label <- g
      }
      legend_labels[i] <- label
    }
  }

  if (draw_fixed_line && length(fe) > 1) {
    x_seq <- seq(min(x), max(x), length.out = 100)
    y_seq <- fe[1] + fe[2] * x_seq
    lines(x_seq, y_seq, col = "black", lty = 2, lwd = 2)
    if (label_equations) {
      label <- sprintf("Fixed: y = %.3f x + %.3f", fe[2], fe[1])
      legend_labels <- c(legend_labels, label)
      colors <- c(colors, "black")
      lty_vec <- c(lty_vec, 2); lwd_vec <- c(lwd_vec, 2); pch_vec <- c(pch_vec, NA)
    }
  }

  if (legend) {
    legend(legend_position, legend = legend_labels,
           col = colors, lty = lty_vec, lwd = lwd_vec,
           pch = pch_vec, bty = "o", inset = inset)
  }

  if (return_model) return(model) else invisible(NULL)
}
