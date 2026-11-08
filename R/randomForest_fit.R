#' Random Forest Fit
#'
#' This function fits a random forest model.
#'
#' Random forest can be used for continuous response or binary response.
#'
#' For continuous response, the function fits random forest regression.
#' For binary response, the function fits random forest classification.
#'
#' @param X A matrix or data frame of predictors.
#' @param y A continuous or binary response vector.
#' @param ntree Number of trees to grow. The default is 500.
#' @param mtry Number of variables randomly sampled as candidates at each split.
#' The default is NULL, so the function uses the randomForest default.
#' @param weights A vector of positive weights with the same length as y.
#' These weights are used only in sampling data to grow each tree. The default is NULL.
#' @param replace A Boolean indicating whether sampling of cases is done with
#' replacement. The default is TRUE.
#' @param classwt A named vector of prior weights for different classes.
#' It is only used for classification. The default is NULL.
#' @param cutoff A numeric vector used for classification. The winning class is
#' the class with the maximum ratio of vote proportion to cutoff. The default is NULL.
#' @param sampsize Size of sample to draw. The default is NULL, so the function
#' uses the randomForest default.
#' @param nodesize Minimum size of terminal nodes. The default is NULL, so the
#' function uses the randomForest default.
#' @param maxnodes Maximum number of terminal nodes trees in the forest can have.
#' The default is NULL.
#' @param importance A Boolean indicating whether variable importance is computed.
#' The default is FALSE.
#'
#' @return A fitted random forest model.
#'
#' @examples
#' data(example_data)
#' randomForest_fit(example_data$X, example_data$y)
#'
#' @export
#' 
randomForest_fit <- function(X, y, ntree = 500, mtry = NULL, weights = NULL,
                             replace = TRUE, classwt = NULL, cutoff = NULL,
                             sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                             importance = FALSE){
  
  dat_info <- input_data(X, y)
  
  X <- dat_info$X
  y <- dat_info$y
  y.type <- dat_info$y.type
  
  if (is.null(mtry)){
    
    if (y.type == "continuous"){
      mtry <- max(floor(ncol(X) / 3), 1)
    }
    
    if (y.type == "binary"){
      mtry <- floor(sqrt(ncol(X)))
    }
  }
  
  if (is.null(sampsize)){
    
    if (replace == TRUE){
      sampsize <- nrow(X)
    } else {
      sampsize <- ceiling(0.632 * nrow(X))
    }
  }
  
  if (is.null(nodesize)){
    
    if (y.type == "continuous"){
      nodesize <- 5
    }
    
    if (y.type == "binary"){
      nodesize <- 1
    }
  }
  
  if (y.type == "binary" & is.null(cutoff)){
    cutoff <- rep(1 / length(levels(y)), length(levels(y)))
    names(cutoff) <- levels(y)
  }
  
  fit <- randomForest::randomForest(x = X, y = y, ntree = ntree, mtry = mtry,
                                    weights = weights, replace = replace,
                                    classwt = classwt, cutoff = cutoff,
                                    sampsize = sampsize, nodesize = nodesize,
                                    maxnodes = maxnodes, importance = importance)
  
  return(fit)
}
