#' Bagging Fit
#'
#' This function fits a bagging model.
#'
#' It draws bootstrap samples with replacement, fits a model on each bootstrap 
#' sample, and aggregates predictions from all bootstrap models.
#'
#' The available models are:
#'  1. linear regression, 
#'  2. logistic regression, 
#'  3. ridge regression, 
#'  4. lasso regression.
#'
#' Linear regression analyses continuous response y.
#' Logistic regression analyses binary response y.
#' Ridge regression and lasso regression can be used for continuous or binary y.
#'
#' @param X A matrix or data frame of predictors.
#' @param y A continuous or binary response vector.
#' @param model The model used in bagging. Choices are "linear", "logistic",
#' "ridge", and "lasso".
#' @param R The number of bootstrap samples. The default is 100.
#' @param nfolds The number of folds used in glmnet cross-validation. The default
#' is 5.
#' @param s The lambda used in glmnet prediction. Choices are "lambda.min" and
#' "lambda.1se". The default is "lambda.min".
#'
#' @return For continuous response, a list containing fitted bootstrap models,
#' prediction matrix, and final predictions. For binary response, the list also
#' contains final probabilities.
#'
#' @examples
#' data(example_data)
#' bagging_fit(example_data$X, example_data$y, model = "linear", R = 100)
#'
#' @export
#' 
bagging_fit <- function(X, y, model = c("linear", "logistic", "ridge", "lasso"),
                        R = 100, nfolds = 5,
                        s = c("lambda.min", "lambda.1se")){
  
  dat_info <- input_data(X, y)
  
  X <- dat_info$X
  y <- dat_info$y
  y.type <- dat_info$y.type
  
  model <- match.arg(model)
  s <- match.arg(s)
  
  if (R < 1 | floor(R) != R){
    stop("R must be a positive integer.")
  }
  
  if (nfolds < 2 | floor(nfolds) != nfolds){
    stop("nfolds must be an integer larger than 1.")
  }
  
  if (model == "linear" & y.type != "continuous"){
    stop("Linear regression uses continuous y.")
  }
  
  if (model == "logistic" & y.type != "binary"){
    stop("Logistic regression uses binary y.")
  }
  
  bagging_model_fit <- function(X, y, model, nfolds){
    
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
    
    return(fit)
  }
  
  id <- replicate(R, sample(1:length(y), size = length(y), replace = TRUE))
  
  boot_by_id <- function(j){
    
    id.j <- id[, j]
    
    X.boot <- X[id.j, , drop = FALSE]
    y.boot <- y[id.j]
    
    if (y.type == "binary"){
      
      while (length(unique(y.boot)) < 2){
        
        id.j <- sample(1:length(y), size = length(y), replace = TRUE)
        
        X.boot <- X[id.j, , drop = FALSE]
        y.boot <- y[id.j]
      }
    }
    
    fit <- bagging_model_fit(X.boot, y.boot, model, nfolds)
    
    return(fit)
  }
  
  bagging_model_predict <- function(fit, X, model, y.type, s){
    
    if (model == "linear"){
      pred <- predict(fit, newdata = X)
    }
    
    if (model == "logistic"){
      pred <- predict(fit, newdata = X, type = "response")
    }
    
    if (model == "ridge" | model == "lasso"){
      
      X.new <- model.matrix(~ . - 1, data = X)
      
      if (y.type == "continuous"){
        pred <- as.numeric(predict(fit, newx = X.new, s = s))
      }
      
      if (y.type == "binary"){
        pred <- as.numeric(predict(fit, newx = X.new, s = s,
                                   type = "response"))
      }
    }
    
    return(pred)
  }
  
  models <- vector("list", R)
  
  for (j in 1:R){
    models[[j]] <- boot_by_id(j)
  }
  
  pred.mat <- matrix(NA, nrow = length(y), ncol = R)
  
  for (j in 1:R){
    
    pred.mat[, j] <- bagging_model_predict(
      fit = models[[j]],
      X = X,
      model = model,
      y.type = y.type,
      s = s
    )
  }
  
  if (y.type == "continuous"){
    
    pred <- apply(pred.mat, 1, mean)
    
    return(list(
      models = models,
      pred.matrix = pred.mat,
      pred = pred
    ))
  }
  
  if (y.type == "binary"){
    
    prob <- apply(pred.mat, 1, mean)
    y.level <- levels(as.factor(y))
    pred <- ifelse(prob >= 0.5, y.level[2], y.level[1])
    
    return(list(
      models = models,
      pred.matrix = pred.mat,
      pred = pred,
      prob = prob
    ))
  }
}
