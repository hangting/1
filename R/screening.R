#' Predictor Screening
#'
#' This function screens predictors for high-dimensional data (p >> n).
#'
#' The algorithm first screens predictors by marginal association test between
#' each predictor and the response y.
#'
#' Since different predictor types use different tests, the preprocess takes the
#' p-value, and the pre-score is -log(p-value).
#'
#' For each combination of {x, y}, it uses:
#'  1. {continuous, continuous}, correlation test;
#'  2. {binary/discrete, continuous}, ANOVA test;
#'  3. {continuous, binary}, ANOVA test;
#'  4. {binary/discrete, binary}, chi-square test, or Fisher exact test if needed.
#'
#' If vif.check = TRUE, the function sorts the scores and chooses predictors with
#' low VIF. If a predictor violates the VIF cutoff, the algorithm moves to the
#' next top predictor.
#'
#' If K.full = TRUE and fewer than K predictors satisfy the VIF cutoff, the
#' remaining predictors are selected by the lowest VIF.
#'
#' @param X A matrix or data frame of predictors.
#' @param y A response vector.
#' @param K The number of predictors. The default is 1.
#' @param vif.check A Boolean to check VIF. The default is FALSE.
#' @param vif.max The largest allowed VIF. The default is 4.
#' @param K.full A Boolean to keep K predictors if vif.max is violated.
#'
#' @return A list containing screened predictor X, selected predictors, scores,
#' indices of predictors, predictor type, and VIFs.
#'
#' @examples
#' data(example_data)
#' screening(example_data$X, example_data$y, K = 5)
#'
#' @export
#' 
screening <- function(X, y, K = 1, vif.check = FALSE, vif.max = 4, K.full = FALSE){
  
  dat_info <- input_data(X, y)
  
  X <- dat_info$X
  y <- dat_info$y
  x.type <- dat_info$x.type
  y.type <- dat_info$y.type
  
  if (K < 1 | floor(K) != K){
    stop("K must be a positive integer.")
  }
  
  if (K > ncol(X)){
    stop("K must be smaller than or equal to the number of predictors.")
  }
  
  if (!is.logical(vif.check) | length(vif.check) != 1){
    stop("vif.check must be TRUE or FALSE.")
  }
  
  if (vif.max <= 1){
    stop("vif.max must be larger than 1.")
  }
  
  if (!is.logical(K.full) | length(K.full) != 1){
    stop("K.full must be TRUE or FALSE.")
  }
  
  if (is.null(colnames(X))){
    colnames(X) <- paste0("X", 1:ncol(X))
  }
  
  score <- function(x, y, x.type, y.type){
    
    if (y.type == "continuous"){
      
      if (x.type == "continuous"){
        return(-log(cor.test(x, y)$p.value))
      }
      
      if (x.type == "binary" | x.type == "discrete"){
        fit <- aov(y ~ as.factor(x))
        return(-log(summary(fit)[[1]][["Pr(>F)"]][1]))
      }
    }
    
    if (y.type == "binary"){
      
      y <- as.factor(y)
      
      if (x.type == "continuous"){
        fit <- aov(x ~ y)
        return(-log(summary(fit)[[1]][["Pr(>F)"]][1]))
      }
      
      if (x.type == "binary" | x.type == "discrete"){
        tab <- table(x, y)
        
        pv <- tryCatch(
          chisq.test(tab)$p.value,
          warning = function(w) fisher.test(tab)$p.value,
          error = function(e) fisher.test(tab)$p.value
        )
        
        return(-log(pv))
      }
    }
  }
  
  vif_x <- function(X){
    
    if (ncol(X) == 1){
      VIF <- 1
      names(VIF) <- colnames(X)
      return(VIF)
    }
    
    dat <- data.frame(y.vif = 1:nrow(X), X)
    fit <- lm(y.vif ~ ., data = dat)
    
    VIF <- car::vif(fit)
    
    if (is.matrix(VIF)){
      VIF <- VIF[, ncol(VIF)]
    }
    
    return(VIF)
  }
  
  scores <- mapply(score, x = X, x.type = x.type,
                   MoreArgs = list(y = y, y.type = y.type))
  
  rank.id <- order(scores, decreasing = TRUE)
  
  if (vif.check == FALSE){
    
    id <- rank.id[1:K]
    vifs <- rep(NA, K)
    names(vifs) <- colnames(X)[id]
  }
  
  if (vif.check == TRUE){
    
    id <- c()
    
    for (j in rank.id){
      
      id.2 <- c(id, j)
      vif.2 <- vif_x(X[, id.2, drop = FALSE])
      
      if (max(vif.2) <= vif.max){
        id <- id.2
      }
      
      if (length(id) == K){
        break
      }
    }
    
    vif_candidate <- function(j, id, X){
      
      id.2 <- c(id, j)
      vif.2 <- vif_x(X[, id.2, drop = FALSE])
      
      return(max(vif.2))
    }
    
    if (length(id) < K){
      
      if (K.full == TRUE){
        
        for (i in 1:(K - length(id))){
          
          rest.id <- setdiff(rank.id, id)
          
          rest.vif <- mapply(vif_candidate, rest.id,
                             MoreArgs = list(id = id, X = X))
          
          id <- c(id, rest.id[which.min(rest.vif)])
        }
        
      } else {
        warning("Fewer than K predictors were selected because of the VIF threshold.")
      }
    }
    
    if (length(id) == 0){
      stop("No predictors were selected because of the VIF threshold.")
    }
    
    vifs <- vif_x(X[, id, drop = FALSE])
    
    if (max(vifs) > vif.max){
      warning("Some selected predictors have VIF larger than the threshold.")
    }
  }
  
  X.name <- colnames(X)[id]
  
  return(list(
    X = X[, id, drop = FALSE],
    X.name = X.name,
    X.id = id,
    scores = scores[id],
    x.type = x.type[id],
    VIF = vifs
  ))
}
