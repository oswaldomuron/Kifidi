#' Correlation Matrix and Variance Inflation Factors (VIFs) for a Set of Variables
#'
#' Computes the Pearson correlation matrix for a set of numeric variables and calculates
#' Variance Inflation Factors (VIFs) to assess multicollinearity. All variables are included
#' in the VIF calculation using a dummy response variable in an additive linear model.
#'
#' @param data A data frame containing the variables of interest.
#' @param vars A character vector specifying the names of the numeric variables to include.
#'             All specified variables must exist in `data`.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{correlations}{A numeric matrix of pairwise Pearson correlations among the selected variables.}
#'   \item{VIF}{A data frame with columns `Variable` and `GVIF`, giving the variance inflation factor for each variable.}
#' }
#'
#' @details
#' - The correlation matrix shows pairwise linear associations between variables.
#' - VIFs are computed using a linear model with all variables as predictors and a dummy response.
#' - The VIF calculation assumes an **additive linear model**: each variable is included as a main effect only, 
#'   and no interaction terms or higher-order terms are included.
#' - The function automatically removes rows with missing values (NA) in the selected variables.
#' - VIFs reflect multicollinearity of each variable with respect to all other variables in the set.
#'
#' @examples
#' # Create example data frame
#' set.seed(123)
#' Z <- data.frame(
#'   L.AREA  = rnorm(20, mean = 50, sd = 10),
#'   L.DIST  = rnorm(20, mean = 30, sd = 5),
#'   L.LDIST = rnorm(20, mean = 15, sd = 3),
#'   YR.ISOL = rnorm(20, mean = 10, sd = 2),
#'   ALT     = rnorm(20, mean = 100, sd = 20),
#'   GRAZE   = rnorm(20, mean = 5, sd = 1)
#' )
#' 
#' # Select variables to analyze
#' vars <- c("L.AREA", "L.DIST", "L.LDIST", "YR.ISOL", "ALT", "GRAZE")
#' 
#' # Run the correlation and VIF function
#' result <- cor_vif_table(Z, vars)
#' 
#' # View the correlation matrix
#' result$correlations
#' 
#' # View the variance inflation factors (VIFs)
#' result$VIF
#'
#' @import car
#' @export



cor_vif_table <- function(data, vars) {
  if(!require(car, quietly = TRUE)) install.packages("car", dependencies = TRUE)
  library(car)
  
  # Subset data and ensure it's a data frame
  df <- as.data.frame(na.omit(data[ , vars, drop = FALSE]))
  
  # Correlation matrix
  cor_matrix <- cor(df, use = "pairwise.complete.obs")
  cat("Correlations of the variables\n")
  print(round(cor_matrix, 6))
  
  # For VIF: create a dummy response
  dummy <- rnorm(nrow(df))  # random response
  model <- lm(dummy ~ ., data = df)  # all vars as predictors
  
  vif_values <- vif(model)
  cat("\nVariance inflation factors\n")
  vif_table <- data.frame(Variable = names(vif_values), GVIF = round(vif_values, 6))
  print(vif_table, row.names = FALSE)
  
  invisible(list(correlations = cor_matrix, VIF = vif_table))
}
