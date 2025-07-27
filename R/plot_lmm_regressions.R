plot_lmm_regressions <- function(formula, data,
                                 colors = NULL,
                                 lty = 1, lwd = 2, pch = 16,
                                 xlab = NULL, ylab = NULL, main = NULL,
                                 draw_fixed_line = TRUE, draw_group_lines = TRUE,
                                 label_equations = TRUE, legend_position = "topright",
                                 legend = TRUE, return_model = FALSE, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Please install the 'lme4' package.")
  }
  library(lme4)

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
       main = main, ...)

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
           pch = pch_vec, bty = "n")
  }

  if (return_model) return(model) else invisible(NULL)
}
