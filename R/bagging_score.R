#' Bagging Score
#'
#' This function computes a naive variable importance score from bagging.
#'
#' The predictors are first converted to a numeric design matrix and scaled.
#' Then bootstrap samples are drawn with replacement, a model is fitted on each
#' bootstrap sample, and coefficients are extracted from all bootstrap models.
#'
#' The score of each predictor is the absolute value of the mean bootstrap
#' coefficient divided by the bootstrap standard deviation of the coefficient.
#'
#' The available models are:
#'  1. linear regression, 
#'  2. logistic regression, 
#'  3. ridge regression, 
#'  4. lasso regression.
#'
#' @param X A matrix or data frame of predictors.
#' @param y A continuous or binary response vector.
#' @param model The model used in bagging. Choices are "linear", "logistic",
#' "ridge", and "lasso".
#' @param R The number of bootstrap samples. Must be larger than 1. The default is 100.
#' @param nfolds The number of folds used in glmnet cross-validation. The default
#' is 5.
#' @param s The lambda used in glmnet coefficient extraction. Choices are
#' "lambda.min" and "lambda.1se". The default is "lambda.min".
#'
#' @return A list containing ranked predictors and scores.
#'
#' @examples
#' data(example_data)
#' bagging_score(example_data$X, example_data$y, model = "linear", R = 100)
#'
#' @export
#' 
bagging_score <- function(X, y, model = c("linear", "logistic", "ridge", "lasso"),
                          R = 100, nfolds = 5,
                          s = c("lambda.min", "lambda.1se")){
  
  dat_info <- input_data(X, y)
  
  X <- dat_info$X
  y <- dat_info$y
  y.type <- dat_info$y.type
  
  model <- match.arg(model)
  s <- match.arg(s)
  
  if (R < 2 | floor(R) != R){
    stop("R must be an integer larger than 1.")
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
  
  X <- model.matrix(~ . - 1, data = X)
  
  X.sd <- apply(X, 2, sd)
  
  if (any(X.sd == 0)){
    stop("X contains predictors with zero standard deviation.")
  }
  
  X <- scale(X)
  
  if (model == "linear" | model == "logistic"){
    
    if (ncol(X) >= nrow(X)){
      stop("n must be larger than p, run screening to select predictors.")
    }
  }
  
  fit_one_model <- function(X, y, model, y.type, nfolds){
    
    if (model == "linear"){
      
      dat <- data.frame(y = y, X)
      fit <- lm(y ~ ., data = dat)
    }
    
    if (model == "logistic"){
      
      y <- as.factor(y)
      dat <- data.frame(y = y, X)
      fit <- glm(y ~ ., family = binomial, data = dat)
    }
    
    if (model == "ridge"){
      
      if (y.type == "continuous"){
        fit <- glmnet::cv.glmnet(X, y, alpha = 0, family = "gaussian",
                                 nfolds = nfolds)
      }
      
      if (y.type == "binary"){
        y <- as.factor(y)
        fit <- glmnet::cv.glmnet(X, y, alpha = 0, family = "binomial",
                                 nfolds = nfolds)
      }
    }
    
    if (model == "lasso"){
      
      if (y.type == "continuous"){
        fit <- glmnet::cv.glmnet(X, y, alpha = 1, family = "gaussian",
                                 nfolds = nfolds)
      }
      
      if (y.type == "binary"){
        y <- as.factor(y)
        fit <- glmnet::cv.glmnet(X, y, alpha = 1, family = "binomial",
                                 nfolds = nfolds)
      }
    }
    
    return(fit)
  }
  
  gcoef <- function(fit, model, s){
    
    if (model == "linear" | model == "logistic"){
      b <- coef(fit)[-1]
    }
    
    if (model == "ridge" | model == "lasso"){
      b <- as.numeric(coef(fit, s = s))[-1]
    }
    
    b[is.na(b)] <- 0
    
    return(b)
  }
  
  id <- mapply(sample, size = rep(length(y), R),
               MoreArgs = list(x = 1:length(y), replace = TRUE))
  
  id <- matrix(id, nrow = length(y), ncol = R)
  
  coef.boot <- function(j){
    
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
    
    fit <- fit_one_model(X.boot, y.boot, model, y.type, nfolds)
    b <- gcoef(fit, model, s)
    
    return(b)
  }
  
  b.mat <- sapply(1:R, coef.boot)
  
  if (is.null(dim(b.mat))){
    b.mat <- matrix(b.mat, nrow = 1)
  }
  
  b.bar <- apply(b.mat, 1, mean)
  b.se <- apply(b.mat, 1, sd)
  b.se[b.se == 0] <- 1e-8
  
  score <- abs(b.bar) / b.se
  
  rank.id <- order(score, decreasing = TRUE)
  
  return(list(
    predictors = colnames(X)[rank.id],
    scores = score[rank.id]
  ))
}
