#getwd()
rm(list = ls())
library("devtools")
use_git()
summarize_data <- function(column_data, group_var1, group_var2 = NULL) {
  if (is.null(group_var2)) {
    # Aggregate the data with one grouping variable
    result <- aggregate(
      column_data,
      by = list(Group = group_var1),
      FUN = function(x) c(
        Mean = mean(x, na.rm = TRUE),
        SD = sd(x, na.rm = TRUE),
        N = sum(!is.na(x)),
        Min = min(x, na.rm = TRUE),
        Max = max(x, na.rm = TRUE),
        Median = median(x, na.rm = TRUE),
        SE = sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
      )
    )
    # Convert the result to a data frame and rename columns
    result_df <- do.call(data.frame, result)
    names(result_df) <- c("Group", "Mean", "SD", "N", "Min", "Max", "Median", "SE")
  } else {
    # Aggregate the data with two grouping variables
    result <- aggregate(
      column_data,
      by = list(Group1 = group_var1, Group2 = group_var2),
      FUN = function(x) c(
        Mean = mean(x, na.rm = TRUE),
        SD = sd(x, na.rm = TRUE),
        N = sum(!is.na(x)),
        Min = min(x, na.rm = TRUE),
        Max = max(x, na.rm = TRUE),
        Median = median(x, na.rm = TRUE),
        SE = sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
      )
    )
    # Convert the result to a data frame and rename columns
    result_df <- do.call(data.frame, result)
    names(result_df) <- c("Group1", "Group2", "Mean", "SD", "N", "Min", "Max", "Median", "SE")
  }

  return(result_df)
}

plot_means <- function(summary_df,
                       main_title = "Mean Values by Group",
                       ylab = NULL,
                       xlab = NULL,
                       bar_color = "skyblue",
                       error_bar_color = "red",
                       bar_width = 0.7,
                       error_bar_length = 0.1,
                       axes = TRUE,
                       space = NULL,
                       density = NULL,
                       angle = 45,
                       col = NULL,
                       names_arg = NULL,
                       xlab_custom = NULL,
                       ylab_custom = NULL,
                       ann = TRUE,
                       xlim = NULL,
                       ylim = NULL,
                       xaxt = "s",
                       las = NULL) {

  # Check if there are two groups
  if ("Group2" %in% names(summary_df)) {
    summary_df$Group <- paste(summary_df$Group1, summary_df$Group2, sep = "-")
  }

  # Set names.arg if not provided
  if (is.null(names_arg)) {
    names_arg <- summary_df$Group
  }

  # Set xlab if not provided
  if (is.null(xlab_custom)) {
    xlab_custom <- "Groups"
  }

  # Set ylab if not provided
  if (is.null(ylab_custom)) {
    ylab_custom <- "Mean"
  }

  # Create a bar plot of the means for each group, with axes turned off if specified
  midpoints <- barplot(
    height = summary_df$Mean,
    names.arg = names_arg,
    beside = TRUE,
    col = bar_color,
    density = density, # density parameter
    angle = angle, # angle parameter
    space = space, # Space between bars
    width = bar_width,
    ylim = ylim,  # Set ylim if provided
    xlim = xlim,  # Set xlim if provided
    main = main_title,
    ylab = ylab_custom,
    xlab = xlab_custom,
    axes = axes, # Both x and y axes controlled together
    ann = ann,
    xaxt = xaxt,
    las = las
  )

  # Add error bars for the standard errors
  arrows(
    x0 = midpoints,
    y0 = summary_df$Mean - summary_df$SE,
    x1 = midpoints,
    y1 = summary_df$Mean + summary_df$SE,
    angle = 90,
    code = 3,
    length = error_bar_length,
    col = error_bar_color
  )
}

#Create the package skeleton
#package.skeleton(name = "Kifidi")
