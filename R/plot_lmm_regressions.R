#' Plot Linear Mixed Model Regressions by Group
#'
#' Fits a linear mixed-effects model (LMM) using the provided formula and data,
#' then plots the data points and regression lines for each group along with
#' an optional overall fixed-effects regression line. Also calculates and displays
#' marginal and conditional R² values.
#'
#' @param formula A formula object specifying the model, including a grouping term
#'                (e.g., \code{y ~ x + (1 | group)}).
#' @param data A data frame containing the variables in the formula.
#' @param colors Named vector of colors for groups. If NULL, colors are generated automatically.
#' @param main Plot title.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param legend Logical; whether to display a legend.
#' @param legend_position Position of the legend (default "topright").
#' @param draw_group_lines Logical; whether to draw regression lines per group.
#' @param draw_overall_line Logical; whether to draw overall fixed-effects regression line.
#' @param fixed_lty Line type for fixed effects line (default 2).
#' @param fixed_col Color for fixed effects line (default "black").
#' @param fixed_lwd Line width for fixed effects line (default 1).
#' @param label_equations Logical; whether to label lines with regression equations.
#' @param conf.int Logical; (Not currently implemented) whether to draw confidence intervals.
#' @param add Logical; whether to add to an existing plot.
#' @param lty Line types for group lines; can be a single value or named vector.
#' @param lwd Line widths for group lines; can be a single value or named vector.
#' @param pch Plotting characters for points; can be single or named vector.
#' @param axes Logical; whether to draw axes.
#' @param ann Logical; whether to annotate plot with labels.
#' @param return_model Logical; whether to return the fitted model object.
#' @param ... Additional graphical parameters passed to plot and points.
#'
#' @return Invisibly returns NULL unless \code{return_model = TRUE}, in which case
#'         the fitted \code{lmer} model object is returned.
#'
#' @details
#' This function requires the \code{lme4} package. It fits a linear mixed model
#' based on the formula provided, where a grouping factor must be included as a
#' random effect term (e.g., (1 | group)). It then plots the raw data points colored
#' by group, overlays fitted regression lines per group, and optionally an overall
#' fixed effects regression line. Marginal and conditional R² are calculated
#' using variance components of the fitted model.
#'
#' @examples
#' \dontrun{
#' library(lme4)
#' data(sleepstudy)
#' plot_lmm_regressions(Reaction ~ Days + (1 | Subject), sleepstudy)
#' }
#'
#' @importFrom lme4 lmer findbars fixef ranef VarCorr
#' @importFrom stats predict sigma
#' @export



plot_lmm_regressions <- function(formula, data,
                                 colors = NULL,
                                 main = NULL,
                                 xlab = NULL,
                                 ylab = NULL,
                                 legend = TRUE,
                                 legend_position = "topright",
                                 draw_group_lines = TRUE,
                                 draw_overall_line = TRUE,
                                 fixed_lty = 2,
                                 fixed_col = "black",
                                 fixed_lwd = 1,
                                 label_equations = FALSE,
                                 conf.int = FALSE,
                                 add = FALSE,
                                 lty = 2,
                                 lwd = 2,
                                 pch = 16,
                                 axes = TRUE,
                                 ann = TRUE,
                                 return_model = FALSE,
                                 ...) {
  # function body here ...
}


plot_lmm_regressions <- function(formula, data,
                                 colors = NULL,
                                 main = NULL,
                                 xlab = NULL,
                                 ylab = NULL,
                                 legend = TRUE,
                                 legend_position = "topright",
                                 draw_group_lines = TRUE,
                                 draw_overall_line = TRUE,
                                 fixed_lty = 2,
                                 fixed_col = "black",
                                 fixed_lwd = 1,
                                 label_equations = FALSE,
                                 conf.int = FALSE,
                                 add = FALSE,
                                 lty = 2,
                                 lwd = 2,
                                 pch = 16,
                                 axes = TRUE,
                                 ann = TRUE,
                                 return_model = FALSE,
                                 ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("The lme4 package is required but not installed.")
  }
  library(lme4)

  .remodmat.merMod <- function(object) {
    rval <- do.call("cbind", model.matrix(object, type = "randomListRaw"))
    rval[, !duplicated(colnames(rval)), drop = FALSE]
  }

  .varRESum <- function(vc, X) {
    if (is.null(vc)) return(0)
    n <- nrow(X)
    sum(sapply(vc, function(sig) {
      mm1 <- X[, rownames(sig), drop = FALSE]
      sum(rowSums((mm1 %*% sig) * mm1)) / n
    }))
  }

  # Parse formula
  re_term <- lme4::findbars(formula)
  if (length(re_term) == 0) stop("Formula must contain a grouping term (e.g., (1 | group)).")
  group_var <- as.character(re_term[[1]][[3]])

  # Extract variable names
  vars <- all.vars(formula)
  y_var <- vars[1]
  x_var <- vars[2]

  x <- data[[x_var]]
  y <- data[[y_var]]
  group <- factor(data[[group_var]])

  valid <- complete.cases(x, y, group)
  x <- x[valid]
  y <- y[valid]
  group <- droplevels(group[valid])
  data <- data[valid, ]
  data[[group_var]] <- group

  levels_group <- levels(group)
  n_groups <- length(levels_group)

  # Handle colors
  if (is.null(colors)) {
    colors <- setNames(rainbow(n_groups), levels_group)
  } else {
    if (is.null(names(colors))) {
      if (length(colors) < n_groups) stop("colors vector too short")
      colors <- setNames(colors[1:n_groups], levels_group)
    } else {
      missing <- setdiff(levels_group, names(colors))
      if (length(missing)) stop("Missing colors for groups: ", paste(missing, collapse = ", "))
      colors <- colors[levels_group]
    }
  }

  # Handle styles helper
  style_helper <- function(arg, name) {
    if (length(arg) == 1) return(setNames(rep(arg, n_groups), levels_group))
    if (is.null(names(arg))) {
      if (length(arg) < n_groups) stop(paste(name, "vector too short"))
      return(setNames(arg[1:n_groups], levels_group))
    } else {
      if (!all(levels_group %in% names(arg))) stop(name, " names don't match all groups")
      return(arg[levels_group])
    }
  }
  lty_vec <- style_helper(lty, "lty")
  lwd_vec <- style_helper(lwd, "lwd")
  pch_vec <- style_helper(pch, "pch")

  # Fit model
  model <- lme4::lmer(formula, data = data)

  # Improved R² calculations
  fixed_pred <- predict(model, re.form = NA)
  var_fixed <- var(fixed_pred)

  vc <- VarCorr(model)
  X_re <- .remodmat.merMod(model)
  var_random <- .varRESum(vc, X_re)

  var_resid <- sigma(model)^2

  r2_marginal <- round(var_fixed / (var_fixed + var_random + var_resid), 3)
  r2_conditional <- round((var_fixed + var_random) / (var_fixed + var_random + var_resid), 3)

  # Compute conditional R² per group
  group_r2 <- sapply(levels_group, function(g) {
    idx <- which(group == g)
    y_g <- y[idx]
    newdata_g <- data[idx, ]
    y_pred_g <- predict(model, newdata = newdata_g, re.form = NULL)
    ss_res <- sum((y_g - y_pred_g)^2)
    ss_tot <- sum((y_g - mean(y_g))^2)
    if (ss_tot == 0) return(NA) else round(1 - ss_res / ss_tot, 3)
  })
  names(group_r2) <- levels_group

  # Prepare plot
  if (!add) {
    plot(x, y, type = "n",
         main = if (ann) main else NULL,
         xlab = if (ann) if (is.null(xlab)) x_var else xlab else "",
         ylab = if (ann) if (is.null(ylab)) y_var else ylab else "",
         axes = FALSE,
         ...)

    if (axes) {
      axis(1)
      axis(2)
      box()
    }

    if (!ann) {
      title(xlab = "", ylab = "")
    }
  }

  legend_labels <- c()

  # Plot groups
  for (g in levels_group) {
    idx <- which(group == g)
    xi <- x[idx]
    yi <- y[idx]

    points(xi, yi, col = colors[[g]], pch = pch_vec[[g]], ...)

    if (draw_group_lines) {
      x_seq <- seq(min(xi), max(xi), length.out = 100)
      newdata <- data.frame(x_seq)
      names(newdata) <- x_var
      newdata[[group_var]] <- g

      other_covs <- setdiff(all.vars(formula), c(y_var, x_var, group_var))
      for (cov in other_covs) {
        if (is.numeric(data[[cov]])) {
          newdata[[cov]] <- mean(data[[cov]], na.rm = TRUE)
        } else {
          newdata[[cov]] <- levels(data[[cov]])[1]
        }
      }

      pred <- predict(model, newdata = newdata, re.form = NULL)
      lines(x_seq, pred,
            col = colors[[g]],
            lty = lty_vec[[g]],
            lwd = lwd_vec[[g]])

      if (label_equations) {
        fe <- fixef(model)
        re <- ranef(model)[[group_var]][g, ]
        intercept <- round(fe[1] + re[1], 3)
        slope <- if (length(fe) > 1) round(fe[2], 3) else NA
        r2_g <- group_r2[g]

        label <- if (!is.na(slope)) {
          paste0(g, ": y = ", slope, "x + ", intercept, ", R² = ", r2_g)
        } else {
          paste0(g, ": intercept = ", intercept, ", R² = ", r2_g)
        }

        legend_labels <- c(legend_labels, label)
      } else {
        legend_labels <- c(legend_labels, g)
      }
    }
  }

  # Fixed effects line and legend entry
  if (draw_group_lines && draw_overall_line) {
    x_seq <- seq(min(x), max(x), length.out = 100)
    newdata <- data[1, , drop = FALSE]
    newdata <- newdata[rep(1, 100), ]
    newdata[[x_var]] <- x_seq

    other_covs <- setdiff(all.vars(formula), c(y_var, x_var, group_var))
    for (cov in other_covs) {
      if (is.numeric(data[[cov]])) {
        newdata[[cov]] <- mean(data[[cov]], na.rm = TRUE)
      } else {
        newdata[[cov]] <- levels(data[[cov]])[1]
      }
    }

    pred_fixed <- predict(model, newdata = newdata, re.form = NA)
    lines(x_seq, pred_fixed, col = fixed_col, lwd = fixed_lwd, lty = fixed_lty)

    if (legend) {
      fe <- fixef(model)
      if (length(fe) > 1) {
        eq_terms <- paste0(round(fe[-1], 3), " * ", names(fe)[-1])
        eq_string <- paste0("y = ", round(fe[1], 3), " + ", paste(eq_terms, collapse = " + "))
      } else {
        eq_string <- paste0("y = ", round(fe[1], 3))
      }
      eq_string <- paste0(eq_string, ", R²m = ", r2_marginal, ", R²c = ", r2_conditional)

      legend_labels <- c(legend_labels, eq_string)
      colors <- c(colors, fixed_col)
      lty_vec <- c(lty_vec, fixed_lty)
      lwd_vec <- c(lwd_vec, fixed_lwd)
      pch_vec <- c(pch_vec, NA)
      names(colors)[length(colors)] <- "Fixed effect"
      names(lty_vec)[length(lty_vec)] <- "Fixed effect"
      names(lwd_vec)[length(lwd_vec)] <- "Fixed effect"
    }
  }

  if (legend) {
    legend(legend_position,
           legend = legend_labels,
           col = colors,
           pch = unname(pch_vec),
           lty = unname(lty_vec),
           lwd = unname(lwd_vec),
           bg = "white",
           cex = 0.85)
  }

  if (return_model) return(model)
  invisible(NULL)
}
