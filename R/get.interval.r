#' Get a truncation interval
#' 
#' @param eta direction to project y
#' @param y value of the vector y
#' @param A matrix
#' @param B vector
#' @param lambda regularization parameter
#'
#' @details One should have \eqn{A y <= lambda B} for this to make sense.
#' Gives the bounds on eta' y, when y verifies this inequality. The bounds depend on y through z
#' which is the residual of the projection of y on eta. Cf lemma 5.1.
#'
#' @export
get.interval <- function(eta, y, A, B, lambda) {
  c <- eta / sum(eta**2)
  z <- y - sum(eta*y)/sum(eta**2) * eta
  Ac <- A %*% c
  Bz <- lambda*B - A %*% z
  
  U <- min( (Bz/Ac)[Ac > 0] )
  L <- max( (Bz/Ac)[Ac < 0] )
  POS <- min( Bz[Ac == 0] )
  list(L = L, U = U, POS = POS)
} 
