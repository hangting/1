#' Logistic Regression Fit
#'
#' This function fits a logistic regression model for binary response.
#'
#' If the data is high-dimensional and p >> n, one should run screening() before fitting.
#'
#' @param X A matrix or data frame of predictors.
#' @param y A binary response vector.
#'
#' @return A fitted logistic regression model.
#'
#' @examples
#' data(example_data)
#' logistic_fit(example_data$X, example_data$y_binary)
#'
#' @export
#' 
logistic_fit <- function(X, y){
  
  dat_info <- input_data(X, y)
  
  X <- dat_info$X
  y <- dat_info$y
  
  if (dat_info$y.type != "binary"){
    stop("Logistic regression requires binary y, try other.")
  }
  
  if (ncol(X) >= nrow(X)){
    stop("n must be larger than p, run screening to select predictors.")
  }
  
  y <- as.factor(y)
  
  dat <- data.frame(y = y, X)
  
  return(glm(y ~ ., family = binomial, data = dat))
}
