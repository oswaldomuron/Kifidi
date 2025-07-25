plot_group_regressions <- function(x, y, group = NULL,
                                   colors = NULL,
                                   main = NULL,
                                   xlab = NULL, ylab = NULL,
                                   legend = TRUE,
                                   legend_position = "topright",
                                   return_models = FALSE,
                                   conf.int = FALSE,
                                   label_equations = FALSE,
                                   add = FALSE,
                                   theme = "default",
                                   ...) {
  dot_args <- list(...)
  
  # Check if user passed 'col' explicitly in '...'
  user_col <- NULL
  if ("col" %in% names(dot_args)) {
    user_col <- dot_args$col
    dot_args$col <- NULL  # remove from dot_args so it doesn't cause conflict
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
    points(x, y, col = point_col, pch = 16)
    
    model <- lm(y ~ x)
    line_col <- if (!is.null(user_col)) user_col else "red"
    abline(model, col = line_col, lwd = 2)
    
    coef <- coef(model)
    slope <- round(coef[2], 3)
    intercept <- round(coef[1], 3)
    r2 <- round(summary(model)$r.squared, 3)
    
    eq_label <- paste0("y = ", slope, "x + ", intercept, ", R² = ", r2)
    
    if (label_equations) {
      usr <- par("usr") # plot coords: c(x1, x2, y1, y2)
      x_pos <- usr[1] + 0.05 * (usr[2] - usr[1])
      y_pos <- usr[4] - 0.05 * (usr[4] - usr[3])
      text(x_pos, y_pos, eq_label, adj = c(0, 1), col = line_col, cex = 0.9)
    }
    
    if (legend) {
      legend(legend_position,
             legend = eq_label,
             col = line_col,
             lwd = 2,
             pch = 16,
             bg = "white",
             cex = 0.8)
    }
    
    if (return_models) return(list(overall = model))
    return(invisible(NULL))
  }
  
  # Grouped data code as before ...
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
  
  if (!add) {
    plot(x, y, type = "n", main = main, xlab = xlab, ylab = ylab, ...)
  }
  
  legend_labels <- c()
  legend_colors <- c()
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
    
    do.call(points, c(list(x = xi, y = yi, col = point_col, pch = 16), dot_args))
    
    model <- lm(yi ~ xi)
    abline(model, col = colors[[g]], lwd = 2)
    
    coef <- coef(model)
    slope <- round(coef[2], 3)
    intercept <- round(coef[1], 3)
    r2 <- round(summary(model)$r.squared, 3)
    
    eq_label <- paste0(g, ": y = ", slope, "x + ", intercept, ", R² = ", r2)
    legend_labels <- c(legend_labels, eq_label)
    legend_colors <- c(legend_colors, colors[[g]])
    
    if (label_equations) {
      usr <- par("usr")
      x_pos <- usr[1] + 0.05 * (usr[2] - usr[1])
      # Spread equations vertically if multiple groups
      y_pos <- usr[4] - (0.05 + 0.05 * which(levels_group == g)) * (usr[4] - usr[3])
      text(x_pos, y_pos, eq_label, adj = c(0, 1), col = colors[[g]], cex = 0.9)
    }
    
    model_list[[g]] <- model
  }
  
  if (legend) {
    legend(legend_position, legend = legend_labels, col = legend_colors,
           lwd = 2, pch = 16, bg = "white", cex = 0.8)
  }
  
  if (return_models) return(model_list)
}
