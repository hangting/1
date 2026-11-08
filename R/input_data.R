#' Input Data
#'
#' This function checks whether the input data is valid for the package, and will
#' aggregate data if it satisfies the requirements.
#'
#' The predictor data X must be a matrix or data frame with columns of continuous,
#' discrete, or binary values.
#' 
#' The response y must be a vector with continuous or binary values.
#'
#' @param X A matrix or data frame of predictors.
#' @param y A response vector.
#'
#' @return A list containing predictor X, response y, predictor type, and response type.
#'
#' @examples
#' data(example_data)
#' input_data(example_data$X, example_data$y)
#'
#' @export
#' 
input_data <- function(X, y){
  
  if (!is.matrix(X) & !is.data.frame(X)){
    stop("X must be a matrix or data frame.")
  }
  
  if (!is.vector(y) & !is.factor(y)){
    stop("y must be a vector.")
  }
  
  if (nrow(X) != length(y)){
    stop("The number of rows of X must match the length of y.")
  }
  
  if (any(is.na(X)) | any(is.na(y))){
    stop("Dataset contains missing values.")
  }
  
  check_x <- function(x){
    
    if (is.list(x)){
      stop("Each column of X must be continuous, discrete, or binary.")
    }
    
    if (length(unique(x)) == 2){
      return("binary")
    }
    
    if (is.integer(x)){
      return("discrete")
    }
    
    if (is.numeric(x)){
      return("continuous")
    }
    
    if (is.factor(x) | is.character(x) | is.logical(x)){
      return("discrete")
    }
    
    stop("Each column of X must be continuous, discrete, or binary.")
  }
  
  convert_x <- function(x, x.type){
    
    if (x.type == "continuous"){
      return(as.numeric(x))
    }
    
    if (x.type == "discrete"){
      return(as.factor(x))
    }
    
    if (x.type == "binary"){
      return(as.factor(x))
    }
  }
  
  X <- as.data.frame(X)
  
  x.type <- sapply(X, check_x)
  
  X <- data.frame(
    mapply(convert_x, x = X, x.type = x.type, SIMPLIFY = FALSE)
  )
  
  names(X) <- names(x.type)
  
  if (length(unique(y)) == 2){
    y.type <- "binary"
    y <- as.factor(y)
  } else if (is.numeric(y)){
    y.type <- "continuous"
    y <- as.numeric(y)
  } else {
    stop("y must be continuous or binary.")
  }
  
  return(list(
    y = y,
    X = X,
    y.type = y.type,
    x.type = x.type
  ))
}
