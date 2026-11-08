#' Linear Regression Fit
#'
#' This function fits a linear regression model for continuous response.
#'
#' If the data is high-dimensional and p >> n, one should run screening() before fitting.
#'
#' @param X A matrix or data frame of predictors.
#' @param y A continuous vector.
#'
#' @return A list containing the fitted linear regression model, predictor names,
#' and response type.
#'
#' @examples
#' data(example_data)
#' linear_fit(example_data$X, example_data$y)
#'
#' @export
#' 
linear_fit <- function(X, y){
  
  dat_info <- input_data(X, y)
  
  X <- dat_info$X
  y <- dat_info$y
  
  if (dat_info$y.type != "continuous"){
    stop("Linear regression requires continuous y, try other.")
  }
  
  if (ncol(X) >= nrow(X)){
    stop("n must be larger than p, run screening to select predictors.")
  }
  
  dat <- data.frame(y = y, X)
  
  return(lm(y ~ ., data = dat))
}
