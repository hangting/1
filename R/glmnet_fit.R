#' Ridge and Lasso Regression Fit
#'
#' This function fits a ridge regression or lasso regression model.
#'
#' Ridge regression uses alpha = 0.
#' Lasso regression uses alpha = 1.
#'
#' Ridge regression can be used for continuous response or binary response.
#' Lasso regression can be used for continuous response or binary response.
#'
#' For small betas with constraint, ridge is recommended.
#' If some betas should be 0, lasso is recommended.
#'
#' @param X A matrix or data frame of predictors.
#' @param y A continuous or binary response vector.
#' @param alpha A number between 0 and 1. Use 0 for ridge regression and 1 for lasso regression.
#' @param nfolds The number of folds used in cross-validation. The default is 5.
#'
#' @return A fitted ridge or lasso regression model.
#'
#' @examples
#' data(example_data)
#' glmnet_fit(example_data$X, example_data$y, alpha = 0)
#' glmnet_fit(example_data$X, example_data$y, alpha = 1)
#'
#' @export
#' 
glmnet_fit <- function(X, y, alpha = 1, nfolds = 5){
  
  dat_info <- input_data(X, y)
  
  X <- dat_info$X
  y <- dat_info$y
  
  if (alpha < 0 | alpha > 1){
    stop("alpha must be between 0 and 1.")
  }
  
  if (nfolds < 2 | floor(nfolds) != nfolds){
    stop("nfolds must be an integer larger than 1.")
  }
  
  X <- model.matrix(~ . - 1, data = X)
  
  if (dat_info$y.type == "continuous"){
    fit <- glmnet::cv.glmnet(X, y, alpha = alpha, family = "gaussian", nfolds = nfolds)
  }
  
  if (dat_info$y.type == "binary"){
    y <- as.factor(y)
    fit <- glmnet::cv.glmnet(X, y, alpha = alpha, family = "binomial", nfolds = nfolds)
  }
  
  return(fit)
}
