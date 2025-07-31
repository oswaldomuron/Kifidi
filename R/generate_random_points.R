#' Generate Random Sampling Points in a Plot
#'
#' This function generates random (x, y) coordinates within a rectangular plot area 
#' for biomass or other sampling. It can plot the points and optionally export them as CSV.
#'
#' @param plot_length Numeric. Length of the plot in meters. Default is 3.
#' @param plot_width Numeric. Width of the plot in meters. Default is 3.
#' @param n_points Integer. Number of random points to generate. Default is 5.
#' @param seed Integer or NULL. Seed for random number generator to reproduce results. Default is NULL (random every run).
#' @param export_csv Logical. Whether to export the coordinates as a CSV file. Default is FALSE.
#' @param filename Character. Name of the CSV file to export if `export_csv = TRUE`. Default is "random_coordinates.csv".
#'
#' @return A data.frame with columns: Point, X_meters, Y_meters.
#' @examples
#' # Generate 5 random points in a 3x3 m plot, plot and export csv
#' generate_random_points(plot_length = 3, plot_width = 3, n_points = 5, seed = 42,
#'                        export_csv = TRUE, filename = "points.csv")
#' 
#' # Generate random points without fixed seed (different each run)
#' generate_random_points(n_points = 10)
#' 
#' @export
generate_random_points <- function(plot_length = 3, plot_width = 3,
                                   n_points = 5, seed = NULL,
                                   export_csv = FALSE,
                                   filename = "random_coordinates.csv") {
  if(!is.null(seed)) set.seed(seed)
  
  x_coords <- runif(n_points, min = 0, max = plot_length)
  y_coords <- runif(n_points, min = 0, max = plot_width)
  
  sampling_points <- data.frame(
    Point = 1:n_points,
    X_meters = round(x_coords, 2),
    Y_meters = round(y_coords, 2)
  )
  
  print(sampling_points)
  
  plot(x_coords, y_coords, pch = 19, col = "blue",
       xlim = c(0, plot_length), ylim = c(0, plot_width),
       xlab = "X (m)", ylab = "Y (m)",
       main = paste("Random Sampling Points (", plot_length, "x", plot_width, "m plot)", sep=""))
  
  rect(0, 0, plot_length, plot_width, border = "red", lwd = 2)
  text(x_coords, y_coords, labels = 1:n_points, pos = 3)
  
  if(export_csv){
    write.csv(sampling_points, file = filename, row.names = FALSE)
    message(paste("Coordinates exported to:", filename))
  }
  
  return(sampling_points)
}
