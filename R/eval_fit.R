#' Evaluate Fit
#'
#' This function evaluates a model by K-fold cross-validation.
#'
#' For continuous response, the function computes RMSE.
#' For binary response, the function computes AUC.
#'
#' The available models are:
#'  1. linear regression,
#'  2. logistic regression,
#'  3. ridge regression,
#'  4. lasso regression,
#'  5. random forest,
#'  6. bagging,
#'  7. ensemble.
#'
#' Bagging can use:
#'  1. linear regression,
#'  2. logistic regression,
#'  3. ridge regression,
#'  4. lasso regression.
#'
#' Ensemble can use:
#'  1. linear regression,
#'  2. logistic regression,
#'  3. ridge regression,
#'  4. lasso regression,
#'  5. random forest.
#'
#' @param X A matrix or data frame of predictors.
#' @param y A continuous or binary response vector.
#' @param model The model used in evaluation. Choices are "linear", "logistic",
#' "ridge", "lasso", "randomForest", "bagging", and "ensemble".
#' @param K The number of folds used in cross-validation. The default is 5.
#' @param nfolds The number of folds used in glmnet cross-validation. The default is 5.
#' @param s The lambda used in glmnet prediction. Choices are "lambda.min" and
#' "lambda.1se". The default is "lambda.min".
#' @param R The number of bootstrap samples for bagging. The default is 100.
#' @param bagging.model The model used inside bagging. Choices are "linear",
#' "logistic", "ridge", and "lasso".
#' @param ensemble.models A vector of models used in ensemble fitting. Choices are
#' "linear", "logistic", "ridge", "lasso", and "randomForest".
#' @param porp A numeric vector of weights for combining ensemble predictions.
#' The length must be the same as ensemble.models and the sum must be 1.
#' The default is NULL, so all ensemble models receive equal weights.
#' @param ntree Number of trees to grow for random forest. The default is 500.
#' @param mtry Number of variables randomly sampled as candidates at each split.
#' The default is NULL.
#' @param weights A vector of positive weights with the same length as y.
#' The default is NULL.
#' @param replace A Boolean indicating whether sampling of cases is done with
#' replacement in random forest. The default is TRUE.
#' @param classwt A named vector of prior weights for different classes.
#' The default is NULL.
#' @param cutoff A numeric vector used for classification. The default is NULL.
#' @param sampsize Size of sample to draw. The default is NULL.
#' @param nodesize Minimum size of terminal nodes. The default is NULL.
#' @param maxnodes Maximum number of terminal nodes trees in the forest can have.
#' The default is NULL.
#' @param importance A Boolean indicating whether random forest variable
#' importance is computed. The default is FALSE.
#'
#' @return A list containing the model name, number of folds, metric, and value.
#'
#' @examples
#' data(example_data)
#' eval_fit(example_data$X, example_data$y, model = "ridge", K = 5)
#'
#' @export
#' 
eval_fit <- function(X, y,
                     model = c("linear", "logistic", "ridge", "lasso",
                               "randomForest", "bagging", "ensemble"),
                     K = 5,
                     nfolds = 5,
                     s = c("lambda.min", "lambda.1se"),
                     R = 100,
                     bagging.model = c("linear", "logistic", "ridge", "lasso"),
                     ensemble.models = c("linear", "logistic", "ridge", "lasso",
                                         "randomForest"),
                     porp = NULL,
                     ntree = 500, mtry = NULL, weights = NULL,
                     replace = TRUE, classwt = NULL, cutoff = NULL,
                     sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                     importance = FALSE){
  
  dat_info <- input_data(X, y)
  
  X <- dat_info$X
  y <- dat_info$y
  y.type <- dat_info$y.type
  
  model <- match.arg(model)
  s <- match.arg(s)
  bagging.model <- match.arg(bagging.model)
  
  if (K < 2 | floor(K) != K){
    stop("K must be an integer larger than 1.")
  }
  
  if (K > length(y)){
    stop("K must be smaller than or equal to the sample size.")
  }
  
  if (nfolds < 2 | floor(nfolds) != nfolds){
    stop("nfolds must be an integer larger than 1.")
  }
  
  if (model == "linear" & y.type != "continuous"){
    stop("Linear regression evaluation requires continuous y.")
  }
  
  if (model == "logistic" & y.type != "binary"){
    stop("Logistic regression evaluation requires binary y.")
  }
  
  if (model == "bagging"){
    
    if (bagging.model == "linear" & y.type != "continuous"){
      stop("Linear regression bagging requires continuous y.")
    }
    
    if (bagging.model == "logistic" & y.type != "binary"){
      stop("Logistic regression bagging requires binary y.")
    }
  }
  
  if (model == "ensemble"){
    
    model.choice <- c("linear", "logistic", "ridge", "lasso", "randomForest")
    
    if (any(!ensemble.models %in% model.choice)){
      stop("ensemble.models must contain linear, logistic, ridge, lasso, or randomForest.")
    }
    
    if (length(unique(ensemble.models)) != length(ensemble.models)){
      stop("ensemble.models cannot contain repeated model names.")
    }
    
    if ("linear" %in% ensemble.models & y.type != "continuous"){
      stop("Linear regression ensemble requires continuous y.")
    }
    
    if ("logistic" %in% ensemble.models & y.type != "binary"){
      stop("Logistic regression ensemble requires binary y.")
    }
    
    if (!is.null(porp)){
      
      if (!is.numeric(porp)){
        stop("porp must be a numeric vector.")
      }
      
      if (length(porp) != length(ensemble.models)){
        stop("The length of porp must match the length of ensemble.models.")
      }
      
      if (any(porp < 0)){
        stop("porp must contain nonnegative values.")
      }
      
      if (abs(sum(porp) - 1) > 1e-8){
        stop("porp must sum to 1.")
      }
    }
  }
  
  if (R < 1 | floor(R) != R){
    stop("R must be a positive integer.")
  }
  
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
  
  fit_base_model <- function(X.train, y.train, model.now, weights.train){
    
    if (model.now == "linear"){
      fit <- linear_fit(X.train, y.train)
    }
    
    if (model.now == "logistic"){
      fit <- logistic_fit(X.train, y.train)
    }
    
    if (model.now == "ridge"){
      fit <- glmnet_fit(X.train, y.train, alpha = 0, nfolds = nfolds)
    }
    
    if (model.now == "lasso"){
      fit <- glmnet_fit(X.train, y.train, alpha = 1, nfolds = nfolds)
    }
    
    if (model.now == "randomForest"){
      fit <- randomForest_fit(X.train, y.train, ntree = ntree, mtry = mtry,
                              weights = weights.train, replace = replace,
                              classwt = classwt, cutoff = cutoff,
                              sampsize = sampsize, nodesize = nodesize,
                              maxnodes = maxnodes, importance = importance)
    }
    
    return(fit)
  }
  
  predict_base_model <- function(fit, X.test, model.now){
    
    if (model.now == "linear"){
      pred <- predict(fit, newdata = X.test)
    }
    
    if (model.now == "logistic"){
      pred <- predict(fit, newdata = X.test, type = "response")
    }
    
    if (model.now == "ridge" | model.now == "lasso"){
      
      X.test.new <- model.matrix(~ . - 1, data = X.test)
      
      if (y.type == "continuous"){
        pred <- as.numeric(predict(fit, newx = X.test.new, s = s))
      }
      
      if (y.type == "binary"){
        pred <- as.numeric(predict(fit, newx = X.test.new, s = s,
                                   type = "response"))
      }
    }
    
    if (model.now == "randomForest"){
      
      if (y.type == "continuous"){
        pred <- predict(fit, newdata = X.test)
      }
      
      if (y.type == "binary"){
        pred <- predict(fit, newdata = X.test, type = "prob")[, 2]
      }
    }
    
    return(as.numeric(pred))
  }
  
  fit_one_model <- function(X.train, y.train, weights.train){
    
    if (model == "linear" | model == "logistic" | model == "ridge" |
        model == "lasso" | model == "randomForest"){
      
      fit <- fit_base_model(X.train, y.train, model, weights.train)
    }
    
    if (model == "bagging"){
      fit <- bagging_fit(
        X = X.train,
        y = y.train,
        model = bagging.model,
        R = R,
        nfolds = nfolds,
        s = s
      )
    }
    
    if (model == "ensemble"){
      fit <- ensemble_fit(
        X = X.train,
        y = y.train,
        models = ensemble.models,
        porp = porp,
        nfolds = nfolds,
        s = s,
        ntree = ntree,
        mtry = mtry,
        weights = weights.train,
        replace = replace,
        classwt = classwt,
        cutoff = cutoff,
        sampsize = sampsize,
        nodesize = nodesize,
        maxnodes = maxnodes,
        importance = importance
      )
    }
    
    return(fit)
  }
  
  predict_one_model <- function(fit, X.test){
    
    if (model == "linear" | model == "logistic" | model == "ridge" |
        model == "lasso" | model == "randomForest"){
      
      pred <- predict_base_model(fit, X.test, model)
    }
    
    if (model == "bagging"){
      
      pred.mat <- sapply(fit$models, predict_base_model, X.test = X.test,
                         model.now = bagging.model)
      
      if (is.null(dim(pred.mat))){
        pred.mat <- matrix(pred.mat, ncol = 1)
      }
      
      pred <- apply(pred.mat, 1, mean)
    }
    
    if (model == "ensemble"){
      
      pred.mat <- mapply(predict_base_model, fit = fit$models,
                         model.now = fit$model.names,
                         MoreArgs = list(X.test = X.test),
                         SIMPLIFY = FALSE)
      
      pred.mat <- do.call(cbind, pred.mat)
      
      pred <- as.numeric(pred.mat %*% fit$porp)
    }
    
    return(as.numeric(pred))
  }
  
  fold.id <- sample(rep(1:K, length.out = length(y)))
  
  fit_predict_one_fold <- function(k){
    
    train.id <- which(fold.id != k)
    test.id <- which(fold.id == k)
    
    X.train <- X[train.id, , drop = FALSE]
    y.train <- y[train.id]
    
    X.test <- X[test.id, , drop = FALSE]
    
    if (is.null(weights)){
      weights.train <- NULL
    } else {
      weights.train <- weights[train.id]
    }
    
    if (y.type == "binary" & length(unique(y.train)) < 2){
      stop("Each training fold must contain both response levels.")
    }
    
    fit <- fit_one_model(X.train, y.train, weights.train)
    pred <- predict_one_model(fit, X.test)
    
    return(list(
      fit = fit,
      test.id = test.id,
      pred = pred
    ))
  }
  
  cv.fit <- lapply(1:K, fit_predict_one_fold)
  
  pred <- rep(NA, length(y))
  
  for (k in 1:K){
    pred[cv.fit[[k]]$test.id] <- cv.fit[[k]]$pred
  }
  
  if (any(is.na(pred))){
    stop("Some observations are missing predictions.")
  }
  
  if (y.type == "continuous"){
    
    value <- sqrt(mean((y - pred)^2))
    metric <- "RMSE"
    
  } else {
    
    value <- as.numeric(pROC::auc(y, pred))
    metric <- "AUC"
  }
  
  return(list(
    model = model,
    K = K,
    metric = metric,
    value = value
  ))
}
