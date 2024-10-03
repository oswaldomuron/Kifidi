**1. Overview**

The Kifidi package provides tools for summarizing and analyzing environmental data based on grouping variables. It is designed to facilitate statistical summaries of data sets containing numeric measurements and associated categorical grouping factors. The package is particularly useful for environmental and ecological studies where data is often collected across different categories, such as vegetation types, depth, or sampling locations.

**2. Key Functions**

**2.1 summarize_data**

Description:

The `summarize_data` function provides statistical summaries (mean, standard deviation, sample size, etc.) of a numeric column grouped by one or two categorical variables.

Usage:
```r
summarize_data(column_data, group_var1, group_var2 = NULL)
```
Arguments:
- column_data: A numeric vector containing the data to be summarized.
- group_var1: A factor or vector to group the data by (e.g., vegetation type).
- group_var2: (Optional) A second factor or vector to group the data by (e.g., depth).

Output:
A data frame containing the following columns:
- Group1: The first grouping variable (values from group_var1).
- Group2: The second grouping variable (values from group_var2), if provided.
- Mean: The mean of the data for each group.
- SD: The standard deviation for each group.
- N: The sample size for each group.
- Min: The minimum value for each group.
- Max: The maximum value for each group.
- Median: The median value for each group.
- SE: The standard error of the mean for each group.


```r
  # Example data
example_data <- c(
  445, 372, 284, 247, 328, 98.8, 108.7, 100.8, 123.6, 129.9, 133.3,
  130.1, 123.1, 186.6, 215, 19.4, 19.3, 27.8, 26, 22, 30.9, 19.8,
  16.5, 20.2, 31, 21.1, 16.5, 19.7, 18.9, 27, 161.8, 117, 94.6, 97.5,
  142.7, 109.9, 118.3, 111.4, 96.5, 109, 114.1, 114.9, 101.2, 112.7,
  111.1, 194.8, 169.9, 159.1, 100.8, 130.8, 93.6, 105.7, 178.4, 203,
  172.2, 127.3, 128.3, 110.9, 124.1, 179.1, 293, 197.5, 139.1, 98.1,
  84.6, 81.4, 87.2, 71.1, 70.3, 120.4, 194.5, 167.5, 121, 86.5, 81.7
)

example_group1 <- c(
  rep("Palm", 15), rep("Papyrus", 10), rep("Typha", 15),
  rep("Eucalyptus", 15), rep("Rice farm", 20)
)

example_group2 <- rep(c(50, 40, 30, 20, 10), 15)

# Create dataframe
example_df <- data.frame(
  Vegetation_types = example_group1,
  Depth_revised = example_group2,
  EC_uS_cm = example_data
)

# Summarize by one grouping variable
summary_one_group <- summarize_data(
  example_df$EC_uS_cm,
  example_df$Vegetation_types
)
print(summary_one_group)

# Summarize by two grouping variables
summary_two_groups <- summarize_data(
  example_df$EC_uS_cm,
  example_df$Vegetation_types,
  example_df$Depth_revised
)
print(summary_two_groups)

```

**2.2 plot_means**
The `plot_means` function creates a bar plot of means with optional error bars but based on the output of the first function: the `summarize_data` function.

Usage:
```r
plot_means(summary_df,
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
           las = NULL)
```
Arguments:

- summary_df: A summary data frame containing the means and standard errors for each group.
- main_title: Main title for the plot. Default is "Mean Values by Group".
- ylab: Label for the y-axis.
- xlab: Label for the x-axis.
- bar_color: Color for the bars. Default is "skyblue".
- error_bar_color: Color for the error bars. Default is "red".
- bar_width: Width of the bars. Default is 0.7.
- error_bar_length: Length of the error bars. Default is 0.1.
- axes: Logical value indicating whether to draw axes on the plot. Default is TRUE.
- space: Spacing between bars.
- density: Density of shading lines.
- angle: Angle of shading lines.
- col: Color of shading lines.
- names_arg: Vector of names for the x-axis.
- xlab_custom: Custom label for the x-axis. Default is "Groups".
- ylab_custom: Custom label for the y-axis. Default is "Mean".
- ann: Logical value indicating whether to draw annotations on the plot. Default is TRUE.
- xlim: Limits for the x-axis.
- ylim: Limits for the y-axis.
- xaxt: Type of x-axis labeling.
- las: Style of axis labels.

Details:

If the summary data frame contains two grouping variables (Group1 and Group2), they will be combined to form the x-axis labels.

Example:
```r
# Plotting the summarized data
plot_means(summary_two_groups, ylim=c(0,350), las=2,
  space = c(0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0))
```
