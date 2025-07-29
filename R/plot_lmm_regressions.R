#' Plot Linear Mixed Model Regressions by Group
#'
#' This function fits a linear mixed-effects model using `lmer()` and plots group-level
#' regression lines and optionally the fixed effect regression line. It includes group-specific
#' points and regression lines, and can display model statistics such as Nakagawa R² values and AIC.
#'
#' @param formula A formula specifying the model (e.g., `y ~ x + (x | group)`).
#' @param data A data frame containing the variables in the model.
#' @param colors A vector of colors for each group. Defaults to rainbow colors if not specified.
#' @param lty Line type(s) for regression lines. Can be a single value or vector.
#' @param lwd Line width(s) for regression lines. Can be a single value or vector.
#' @param pch Point character(s) for data points. Can be a single value or vector.
#' @param xlab Label for the x-axis. If `NULL`, uses the predictor variable name.
#' @param ylab Label for the y-axis. If `NULL`, uses the response variable name.
#' @param main Plot title.
#' @param draw_fixed_line Logical; if `TRUE`, adds the fixed-effect regression line.
#' @param draw_group_lines Logical; if `TRUE`, draws regression lines for each group.
#' @param label_equations Logical; if `TRUE`, includes regression equations in the legend.
#' @param legend_position Position of the legend (default is `"topright"`; not currently used—legend is placed in right margin).
#' @param inset Inset spacing for the legend; default is 0.
#' @param xpd Logical; whether to allow plotting outside the plot region. Defaults to `TRUE`.
#' @param ann Logical; whether to annotate the axes (titles, labels).
#' @param axes Logical; whether to draw axes.
#' @param legend Logical; whether to display the legend.
#' @param return_model Logical; if `TRUE`, returns the fitted `lmer` model object.
#' @param ... Additional graphical parameters passed to `plot()` or `points()`.
#'
#' @details
#' This function plots both the individual group-level data and their corresponding regression lines
#' from a linear mixed model. It optionally adds the fixed-effect regression line (representing
#' population-level trend), and can annotate the plot with R² statistics (marginal and conditional) and AIC.
#'
#' It uses `lme4::lmer()` to fit the model and `MuMIn::r.squaredGLMM()` to compute Nakagawa's R².
#'
#' @return
#' If `return_model = TRUE`, the fitted `lmer` model object is returned. Otherwise, the function returns `NULL` invisibly.
#'
#' @importFrom lme4 lmer fixef ranef
#' @importFrom MuMIn r.squaredGLMM
#' @export
#'
#' @examples
#' \dontrun{
#'   library(lme4)
#'   library(MuMIn)
#'   data(sleepstudy)
#'   plot_lmm_regressions(Reaction ~ Days + (Days | Subject),
#'                        data = sleepstudy,
#'                        draw_fixed_line = TRUE,
#'                        label_equations = TRUE,
#'                        show_aic = TRUE)
#' }




plot_lmm_regressions <- function(formula, data,
                                 colors = NULL,
                                 lty = 1, lwd = 2, pch = 16,
                                 xlab = NULL, ylab = NULL, main = NULL,
                                 draw_fixed_line = FALSE, draw_group_lines = TRUE,
                                 label_equations = FALSE, legend_position = "topright",
                                 inset = 0,
                                 xpd = TRUE,
                                 ann = TRUE, axes = TRUE,
                                 legend = TRUE, return_model = FALSE,
                                 mar = c(5,4,4,15),
                                 oma = c(0,0,0,4),
                                 xlim = NULL,
                                 ylim = NULL,
                                 ...) {
  # Check that required packages are installed; stop if not
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Please install the 'lme4' package.")
  }
  if (!requireNamespace("MuMIn", quietly = TRUE)) {
    stop("Please install the 'MuMIn' package.")
  }

  # Load packages
  library(lme4)
  library(MuMIn)

  # Save current graphical parameters and restore them on function exit
  op <- par(no.readonly = TRUE)
  on.exit(par(op))

  # Set margins to the defaults: extra right margin (15) to make space for legend outside plot area
  par(mar = mar, oma = oma)
  # Allow plotting outside the plot region (for legend positioning)
  par(xpd = xpd)

  # Fit linear mixed model with specified formula and data
  model <- lmer(formula, data = data)

  # Extract model frame, response variable (y), and predictor variable (x)
  mf <- model.frame(model)
  y <- model.response(mf)
  x_var <- all.vars(formula)[2]    # Get name of predictor variable (assumes two-variable formula)
  x <- mf[[x_var]]

  # Extract grouping factor (random effect grouping variable)
  group_var <- names(getME(model, "flist"))[1]  # first grouping factor name
  group <- mf[[group_var]]
  levels_group <- levels(group)
  n_groups <- length(levels_group)

  # Set colors for groups; if not provided, use rainbow palette
  if (is.null(colors)) colors <- rainbow(n_groups)
  colors <- rep(colors, length.out = n_groups) # repeat if not enough colors

  # Set line types, widths, and point characters for groups, repeating if needed
  lty_vec <- rep(lty, length.out = n_groups)
  lwd_vec <- rep(lwd, length.out = n_groups)
  pch_vec <- rep(pch, length.out = n_groups)

  # Create an empty plot with proper labels and axes
  plot(x, y, type = "n",
       xlab = ifelse(is.null(xlab), x_var, xlab),
       ylab = ifelse(is.null(ylab), all.vars(formula)[1], ylab),
       main = main, ann = ann, axes = axes,
       xlim = xlim,ylim = ylim, ...)

  # Extract fixed effect coefficients
  fe <- fixef(model)
  # Extract group-level coefficients (random intercepts and slopes)
  coef_df <- coef(model)[[group_var]]

  # Initialize vectors to store legend information
  legend_labels <- character(0)
  legend_colors <- character(0)
  legend_lty <- numeric(0)
  legend_lwd <- numeric(0)
  legend_pch <- numeric(0)

  # Loop over each group to plot data points and regression lines
  for (i in seq_along(levels_group)) {
    g <- levels_group[i]
    idx <- which(group == g)    # indices for current group
    xi <- x[idx]; yi <- y[idx]
    # Plot points for group
    points(xi, yi, col = colors[i], pch = pch_vec[i], ...)

    # Plot group regression lines if requested
    if (draw_group_lines) {
      intercept <- coef_df[g, "(Intercept)"]
      # If slope exists for predictor variable, calculate line points
      if (ncol(coef_df) > 1 && x_var %in% colnames(coef_df)) {
        slope <- coef_df[g, x_var]
        x_seq <- seq(min(xi), max(xi), length.out = 100) # fine grid for line
        y_seq <- intercept + slope * x_seq
        # Draw group regression line
        lines(x_seq, y_seq, col = colors[i], lty = lty_vec[i], lwd = lwd_vec[i])
      } else {
        slope <- NA
      }

      # Create legend label with equation if requested
      if (label_equations && !is.na(slope)) {
        lbl <- sprintf("%s: y = %.3f x + %.3f", g, slope, intercept)
      } else {
        lbl <- g
      }

      # Append legend info for group
      legend_labels <- c(legend_labels, lbl)
      legend_colors <- c(legend_colors, colors[i])
      legend_lty <- c(legend_lty, lty_vec[i])
      legend_lwd <- c(legend_lwd, lwd_vec[i])
      legend_pch <- c(legend_pch, pch_vec[i])
    }
  }

  # Optionally draw fixed effect regression line (population average)
  if (draw_fixed_line && length(fe) > 1) {
    x_seq <- seq(min(x), max(x), length.out = 100)
    y_seq <- fe[1] + fe[2] * x_seq
    lines(x_seq, y_seq, col = "black", lty = 2, lwd = 2)  # dashed black line

    # Label for fixed effect line
    fixed_label <- if (label_equations) {
      sprintf("Fixed: y = %.3f x + %.3f", fe[2], fe[1])
    } else {
      "Fixed effect"
    }

    # Append legend info for fixed effect
    legend_labels <- c(legend_labels, fixed_label)
    legend_colors <- c(legend_colors, "black")
    legend_lty <- c(legend_lty, 2)
    legend_lwd <- c(legend_lwd, 2)
    legend_pch <- c(legend_pch, NA)
  }

  # Calculate Nakagawa's R² (marginal and conditional) for mixed models
  R2 <- MuMIn::r.squaredGLMM(model)
  R2m <- R2[1, "R2m"]  # marginal R² (fixed effects only)
  R2c <- R2[1, "R2c"]  # conditional R² (fixed + random effects)

  # Calculate AIC for model
  aic_value <- AIC(model)

  # Add R² values and AIC as additional legend entries (with empty line for spacing)
  legend_labels <- c(legend_labels, "",
                     sprintf("Nakagawa R² (c): %.3f", R2c),
                     sprintf("Nakagawa R² (m): %.3f", R2m),
                     sprintf("AIC: %.1f", aic_value))
  legend_colors <- c(legend_colors, NA, NA, NA, NA)
  legend_lty <- c(legend_lty, NA, NA, NA, NA)
  legend_lwd <- c(legend_lwd, NA, NA, NA, NA)
  legend_pch <- c(legend_pch, NA, NA, NA, NA)


  # Add legend to the right of the plot (in extended margin)
  if (legend) {
    usr <- par("usr")  # get plot coordinates: c(xmin, xmax, ymin, ymax)
    # Place legend just outside right edge (2% offset)
    x_legend <- usr[2] + 0.02 * (usr[2] - usr[1])
    y_legend <- usr[4]  # top of y-axis

    legend(x = x_legend, y = y_legend, legend = legend_labels,
           col = legend_colors, lty = legend_lty, lwd = legend_lwd,
           pch = legend_pch, bty = "o", xpd = TRUE,
           y.intersp = 1.2, adj = c(0, 1)) # left-top aligned
  }

  # Optionally return fitted model object
  if (return_model) return(model) else invisible(NULL)
}
