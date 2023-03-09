
lasso <- function(y, x, lambda = lambda, intercept = TRUE) {
  beta <- coef(glmnet:::glmnet.fit(x, y, weights = rep(1, length(y)), lambda = lambda, intercept = intercept))
  if(intercept) {
    X <- cbind(1, x)
  }
  p <- ncol(X)
  M <- beta@i
  s <- sign(as.vector(beta))
  
  # noM <- (1:p)[-M]
  X.M <- X[,M]
  X.noM <- X[,-M]
}

