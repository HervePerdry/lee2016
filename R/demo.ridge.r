#' A small demo of what Ridge does
#' 
#' @param x
#' @param y
#' @param lambda

#' @examples set.seed(3)
#' x <- cbind( c(rep(-1, 20), rep(1,20)), runif(40, -1, 1) )
#' x[,2] <- x[,2] + 0.4 * x[,1] 
#' y <- x %*% c(1,2) + rnorm(40)
#' y <- y - mean(y)
#' demo.ridge(x, y, 25)
#' @export
demo.ridge <- function(x, y, lambda, xlim = c(-5,5), ylim = c(-5,5), add.grad = FALSE) {
  if(ncol(x) != 2) 
    stop("x should have two columns")

  # les coeff de la forme quadratique 0.5 || y - X beta ||
  A <- 0.5 * crossprod(x)
  u <- -as.vector(crossprod(x, y))
  c <- 0.5 * sum(y**2)
  
  # beta ridge
  beta <- solve(2*A + lambda*diag(2), -u)

  # la valeur prise en beta
  v <- 0.5 * sum( (y - x %*% beta)**2 )
  # la valeur min 
  v.min <- sum(lm(y ~ x - 1)$residuals**2)/2; 

  if(lambda == 0) v <- v * 1.001 # pour éviter que contour.fq refuse le tracé
  plot(contour.fq(v, c, u, A), type = "l", xlim = xlim, ylim = ylim, asp = 1, col = "red", 
       xlab = expression(beta[1]), ylab = expression(beta[2]))
  # le repère
  abline(h = 0, v = 0, lty = 2)
  # for(val in v.min * c(1.001, 1.01, 1.2, 1.4, 2, 4, 8, 14))
  for(val in v.min * c(1.001, 1.8**(1:10) ))
    lines(contour.fq(val, c, u, A))
  points( beta[1], beta[2], pch = 16 )
  # la boule l2
  if(lambda > 0) {
    t <- seq(0, 2*pi, length = 501)
    r <- sqrt(sum(beta**2))
    lines(r*cos(t), r*sin(t))
  } 
  # le gradient en beta
  if(add.grad & lambda > 0) {
    grad <- 2*A %*% beta + u
    grad <- grad/sqrt(sum(grad**2))
    le <- diff(xlim)/10
    arrows( beta[1], beta[2], beta[1] - grad[1]*le, beta[2] - grad[2]*le, 0.2)
  }
}

