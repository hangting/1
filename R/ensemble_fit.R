#' Ensemble Fit
#'
#' This function fits multiple models on the same dataset.
#'
#' The available models are:
#'  1. linear regression,
#'  2. logistic regression,
#'  3. ridge regression,
#'  4. lasso regression,
#'  5. random forest.
#'
#' Linear regression analyses continuous response y.
#' Logistic regression analyses binary response y.
#' Ridge regression, lasso regression, and random forest can be used for
#' continuous or binary y.
#'
#' The ensemble prediction can be computed by a weighted average of model
#' predictions. If porp is not specified, all models receive equal weights.
#'
#' @param X A matrix or data frame of predictors.
#' @param y A continuous or binary response vector.
#' @param models A vector of models used in ensemble learning. Choices are
#' "linear", "logistic", "ridge", "lasso", and "randomForest".
#' @param porp A numeric vector of weights for combining model predictions. The
#' length must be the same as models and the sum must be 1. The default is NULL,
#' so all models receive equal weights.
#' @param nfolds The number of folds used in glmnet cross-validation. The default
#' is 5.
#' @param s The lambda used in glmnet prediction. Choices are "lambda.min" and
#' "lambda.1se". The default is "lambda.min".
#' @param ntree Number of trees to grow for random forest. The default is 500.
#' @param mtry Number of variables randomly sampled as candidates at each split.
#' The default is NULL.
#' @param weights A vector of positive weights with the same length as y.
#' The default is NULL.
#' @param replace A Boolean indicating whether sampling of cases is done with
#' replacement in random forest. The default is TRUE.
#' @param classwt A named vector of prior weights for different classes.
#' It is only used for classification. The default is NULL.
#' @param cutoff A numeric vector used for classification. The default is NULL.
#' @param sampsize Size of sample to draw. The default is NULL.
#' @param nodesize Minimum size of terminal nodes. The default is NULL.
#' @param maxnodes Maximum number of terminal nodes trees in the forest can have.
#' The default is NULL.
#' @param importance A Boolean indicating whether random forest variable
#' importance is computed. The default is FALSE.
#'
#' @return A list containing fitted models, model names, model weights, response
#' type, and lambda choice.
#'
#' @examples
#' data(example_data)
#' ensemble_fit(example_data$X, example_data$y,
#'              models = c("linear", "ridge", "lasso"),
#'              porp = c(0.4, 0.3, 0.3))
#'
#' @export
#' 
ensemble_fit <- function(X, y,
                         models = c("linear", "logistic", "ridge", "lasso",
                                    "randomForest"),
                         porp = NULL,
                         nfolds = 5,
                         s = c("lambda.min", "lambda.1se"),
                         ntree = 500, mtry = NULL, weights = NULL,
                         replace = TRUE, classwt = NULL, cutoff = NULL,
                         sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                         importance = FALSE){
  
  dat_info <- input_data(X, y)
  
  X <- dat_info$X
  y <- dat_info$y
  y.type <- dat_info$y.type
  
  s <- match.arg(s)
  
  model.all <- c("linear", "logistic", "ridge", "lasso", "randomForest")
  
  if (any(!models %in% model.all)){
    stop("models must contain only linear, logistic, ridge, lasso, or randomForest.")
  }
  
  if (length(unique(models)) != length(models)){
    stop("models cannot contain repeated model names.")
  }
  
  if (y.type == "continuous" & "logistic" %in% models){
    stop("Logistic regression uses binary y.")
  }
  
  if (y.type == "binary" & "linear" %in% models){
    stop("Linear regression uses continuous y.")
  }
  
  if (is.null(porp)){
    porp <- rep(1 / length(models), length(models))
  }
  
  if (!is.numeric(porp)){
    stop("porp must be a numeric vector.")
  }
  
  if (length(porp) != length(models)){
    stop("The length of porp must match the length of models.")
  }
  
  if (any(porp < 0)){
    stop("porp must contain nonnegative values.")
  }
  
  if (abs(sum(porp) - 1) > 1e-8){
    stop("porp must sum to 1.")
  }
  
  if ("ridge" %in% models | "lasso" %in% models){
    
    if (nfolds < 2 | floor(nfolds) != nfolds){
      stop("nfolds must be an integer larger than 1.")
    }
  }
  
  if ("randomForest" %in% models){
    
    if (ntree < 1 | floor(ntree) != ntree){
      stop("ntree must be a positive integer.")
    }
    
    if (!is.null(weights) & length(weights) != length(y)){
      stop("weights must have the same length as y.")
    }
    
    if (!is.logical(replace) | length(replace) != 1){
      stop("replace must be TRUE or FALSE.")
    }
    
    if (!is.logical(importance) | length(importance) != 1){
      stop("importance must be TRUE or FALSE.")
    }
  }
  
  ensemble_model_fit <- function(model){
    
    if (model == "linear"){
      fit <- linear_fit(X, y)
    }
    
    if (model == "logistic"){
      fit <- logistic_fit(X, y)
    }
    
    if (model == "ridge"){
      fit <- glmnet_fit(X, y, alpha = 0, nfolds = nfolds)
    }
    
    if (model == "lasso"){
      fit <- glmnet_fit(X, y, alpha = 1, nfolds = nfolds)
    }
    
    if (model == "randomForest"){
      fit <- randomForest_fit(X, y, ntree = ntree, mtry = mtry,
                              weights = weights, replace = replace,
                              classwt = classwt, cutoff = cutoff,
                              sampsize = sampsize, nodesize = nodesize,
                              maxnodes = maxnodes, importance = importance)
    }
    
    return(fit)
  }
  
  fitted.models <- lapply(models, ensemble_model_fit)
  
  return(list(
    models = fitted.models,
    model.names = models,
    porp = porp,
    y.type = y.type,
    s = s
  ))
}
