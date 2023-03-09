#' Contour plot for a quadratic form
#'
#' @param x value of the quadratic form
#' @param c constant term
#' @param u linear term
#' @param A symmetric 2x2 matrix
#' @param n number of points
#' @param branch which branch to trace (in the elliptic case)
#'
#' @return a list with components x, y
#' @examples A <- matrix(c(1,0.5,0.5,1), 2, 2)
#' plot( contour.fq(1, A = A), type = "l", xlab = "x", ylab = "y")
#' for(v in 0.8**(1:40)) lines( contour.fq(v, A = A) )
contour.fq <- function(x, c = 0, u = c(0, 0), A, n = 201, branche = c(-1,1))
{
  if(nrow(A) != 2 | ncol(A) != 2)
    stop("Please give a 2x2 matrix")
  if(det(A) == 0)
    stop("Singular matrix\n")
  center <- as.vector(-0.5*solve(A)%*%u)
  e <- eigen(A)
  la1 <- e$values[1]; la2 <- e$values[2]
  c1 <- as.vector(c + u %*% center/2)
  if( la1*la2 > 0 )
  {
    if(x-c1 > 0 & la1 < 0)
      stop("Valeur maximale ", c1)
    if(x-c1 < 0 & la1 > 0)
      stop("Valeur minimale ", c1)
    t <- seq(0, 2*pi, length=n)
    xx <- center + e$vectors %*% matrix( c(cos(t)*sqrt((x-c1)/la1), sin(t)*sqrt((x-c1)/la2)), nrow=2, byrow=TRUE)
  }
  if( la1*la2 < 0)
  {
    m <- round(n/2)
    xx <- NULL
    for(b in branche) {
      b <- sign(b)
      t <- seq(-10,10,length=m)
      if((x-c1)*la1>0)
        xx <- cbind(xx, center + e$vectors %*% matrix( c(b*cosh(t)*sqrt((x-c1)/la1), sinh(t)*sqrt(-(x-c1)/la2)), nrow=2, byrow=TRUE))
      else
        xx <- cbind(xx, center + e$vectors %*% matrix( c(sinh(t)*sqrt(-(x-c1)/la1), b*cosh(t)*sqrt((x-c1)/la2)), nrow=2, byrow=TRUE))
      xx <- cbind(xx, NA)
    }
  }
  return(list(x=xx[1,], y=xx[2,]))
}

