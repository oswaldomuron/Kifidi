#' Plot Group Regressions with Optional Grouping
#'
#' This function plots x vs y and fits linear models, either by group or for all data.
#'
#' @param x A numeric vector for the x-axis.
#' @param y A numeric vector for the y-axis.
#' @param group Optional factor for grouping. If \code{NULL}, a single regression is drawn.
#' @param colors Named vector of colors for groups or a vector matching number of groups.
#' @param main Main title of the plot.
#' @param xlab Label for x-axis.
#' @param ylab Label for y-axis.
#' @param legend Logical; whether to show the legend.
#' @param legend_position Position of the legend (e.g., "topright").
#' @param return_models Logical; return list of lm models.
#' @param conf.int Logical; whether to draw confidence intervals.
#' @param label_equations Logical; whether to label each group with its regression equation.
#' @param draw_lm Logical; whether to draw the regression line(s).
#' @param add Logical; whether to add to an existing plot.
#' @param theme Plot theme (currently unused).
#' @param lty Line type(s) for regression line.
#' @param lwd Line width(s) for regression line.
#' @param pch Plotting character(s) for points.
#' @param ... Additional plotting parameters passed to \code{points()}.
#'
#' @return Optionally returns a list of lm models if \code{return_models = TRUE}.
#' @export

plot_group_regressions <- function(x, y, group = NULL,
                                   colors = NULL,
                                   main = NULL,
                                   xlab = NULL, ylab = NULL,
                                   legend = TRUE,
                                   legend_position = "topright",
                                   return_models = FALSE,
                                   conf.int = FALSE,
                                   label_equations = FALSE,
                                   draw_lm = TRUE,
                                   add = FALSE,
                                   theme = "default",
                                   lty = 1,
                                   lwd = 2,
                                   pch = 16,
                                   ...) {
  dot_args <- list(...)

  # Check if user passed 'col' explicitly in '...'
  user_col <- NULL
  if ("col" %in% names(dot_args)) {
    user_col <- dot_args$col
    dot_args$col <- NULL
  }

  xlab <- if (is.null(xlab)) deparse(substitute(x)) else xlab
  ylab <- if (is.null(ylab)) deparse(substitute(y)) else ylab

  if (is.null(group)) {
    valid <- complete.cases(x, y)
    x <- x[valid]
    y <- y[valid]

    if (!add) {
      plot(x, y, main = main, xlab = xlab, ylab = ylab, ...)
    }

    point_col <- if (!is.null(user_col)) user_col else "black"
    points(x, y, col = point_col, pch = ifelse(length(pch) == 1, pch, pch[1]))

    model <- lm(y ~ x)

    if (draw_lm) {
      line_col <- if (!is.null(user_col)) user_col else "red"
      abline(model,
             col = line_col,
             lwd = ifelse(length(lwd) == 1, lwd, lwd[1]),
             lty = ifelse(length(lty) == 1, lty, lty[1]))

      if (conf.int) {
        x_seq <- seq(min(x), max(x), length.out = 200)
        pred <- predict(model, newdata = data.frame(x = x_seq), interval = "confidence")
        polygon(c(x_seq, rev(x_seq)),
                c(pred[, "lwr"], rev(pred[, "upr"])),
                col = adjustcolor(line_col, alpha.f = 0.2),
                border = NA)
      }
    }

    coef <- coef(model)
    slope <- round(coef[2], 3)
    intercept <- round(coef[1], 3)
    r2 <- round(summary(model)$r.squared, 3)
    eq_label <- paste0("y = ", slope, "x + ", intercept, ", R² = ", r2)

    if (label_equations) {
      usr <- par("usr")
      x_pos <- usr[1] + 0.05 * (usr[2] - usr[1])
      y_pos <- usr[4] - 0.05 * (usr[4] - usr[3])
      text(x_pos, y_pos, eq_label, adj = c(0, 1), col = "black", cex = 0.9)
    }

    if (legend) {
      legend(legend_position,
             legend = eq_label,
             col = "black",
             lwd = ifelse(length(lwd) == 1, lwd, lwd[1]),
             pch = ifelse(length(pch) == 1, pch, pch[1]),
             lty = ifelse(length(lty) == 1, lty, lty[1]),
             bg = "white",
             cex = 0.8)
    }

    if (return_models) return(list(overall = model))
    return(invisible(NULL))
  }

  # Grouped version
  valid <- complete.cases(x, y, group)
  x <- x[valid]
  y <- y[valid]
  group <- as.factor(group[valid])
  levels_group <- levels(group)
  n_groups <- length(levels_group)

  if (is.null(colors)) {
    colors <- setNames(rainbow(n_groups), levels_group)
  } else {
    if (is.null(names(colors))) {
      if (length(colors) < n_groups) stop("colors vector too short")
      colors <- setNames(colors[1:n_groups], levels_group)
    } else {
      missing_groups <- setdiff(levels_group, names(colors))
      if (length(missing_groups) > 0) stop(paste("Missing colors for groups:", paste(missing_groups, collapse = ", ")))
      colors <- colors[levels_group]
    }
  }

  # Handle group-wise pch
  if (length(pch) == 1) {
    pch_vec <- setNames(rep(pch, n_groups), levels_group)
  } else if (is.null(names(pch))) {
    if (length(pch) < n_groups) stop("pch vector too short")
    pch_vec <- setNames(pch[1:n_groups], levels_group)
  } else {
    if (!all(levels_group %in% names(pch))) stop("pch names don't match all groups")
    pch_vec <- pch[levels_group]
  }

  # Handle group-wise lty
  if (length(lty) == 1) {
    lty_vec <- setNames(rep(lty, n_groups), levels_group)
  } else if (is.null(names(lty))) {
    if (length(lty) < n_groups) stop("lty vector too short")
    lty_vec <- setNames(lty[1:n_groups], levels_group)
  } else {
    if (!all(levels_group %in% names(lty))) stop("lty names don't match all groups")
    lty_vec <- lty[levels_group]
  }

  # Handle group-wise lwd
  if (length(lwd) == 1) {
    lwd_vec <- setNames(rep(lwd, n_groups), levels_group)
  } else if (is.null(names(lwd))) {
    if (length(lwd) < n_groups) stop("lwd vector too short")
    lwd_vec <- setNames(lwd[1:n_groups], levels_group)
  } else {
    if (!all(levels_group %in% names(lwd))) stop("lwd names don't match all groups")
    lwd_vec <- lwd[levels_group]
  }

  if (!add) {
    plot(x, y, type = "n", main = main, xlab = xlab, ylab = ylab, ...)
  }

  legend_labels <- c()
  legend_colors <- c()
  legend_pch <- c()
  legend_lty <- c()
  model_list <- list()

  for (g in levels_group) {
    idx <- group == g
    xi <- x[idx]
    yi <- y[idx]
    if (length(xi) < 2) next

    point_col <- if (!is.null(user_col)) {
      if (length(user_col) == n_groups) user_col[which(levels_group == g)] else user_col
    } else {
      colors[[g]]
    }

    do.call(points, c(list(x = xi, y = yi, col = point_col, pch = pch_vec[g]), dot_args))

    model <- lm(yi ~ xi)
    model_list[[g]] <- model

    if (draw_lm) {
      abline(model,
             col = colors[[g]],
             lwd = lwd_vec[g],
             lty = lty_vec[g])

      if (conf.int) {
        x_seq <- seq(min(xi), max(xi), length.out = 200)
        pred <- predict(model, newdata = data.frame(xi = x_seq), interval = "confidence")
        polygon(c(x_seq, rev(x_seq)),
                c(pred[, "lwr"], rev(pred[, "upr"])),
                col = adjustcolor(colors[[g]], alpha.f = 0.2),
                border = NA)
      }
    }

    coef <- coef(model)
    slope <- round(coef[2], 3)
    intercept <- round(coef[1], 3)
    r2 <- round(summary(model)$r.squared, 3)
    eq_label <- paste0(g, ": y = ", slope, "x + ", intercept, ", R² = ", r2)

    if (label_equations) {
      usr <- par("usr")
      x_pos <- usr[1] + 0.05 * (usr[2] - usr[1])
      y_pos <- usr[4] - (0.05 + 0.05 * which(levels_group == g)) * (usr[4] - usr[3])
      text(x_pos, y_pos, eq_label, adj = c(0, 1), col = colors[[g]], cex = 0.9)
    }

    legend_labels <- c(legend_labels, eq_label)
    legend_colors <- c(legend_colors, colors[[g]])
    legend_pch <- c(legend_pch, pch_vec[g])
    legend_lty <- c(legend_lty, lty_vec[g])
  }

  if (legend) {
    legend(legend_position,
           legend = legend_labels,
           col = legend_colors,
           pch = legend_pch,
           lty = legend_lty,
           lwd = lwd_vec,
           bg = "white",
           cex = 0.8)
  }

  if (return_models) return(model_list)
  invisible(NULL)
}
