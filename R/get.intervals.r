#' Get a truncation interval
#' 
#' @param eta direction to project y
#' @param y value of the vector y
#' @param linear.constraints As sent back by \code{get.linear.constraints}
#' @param lambda regularization parameter
#'
#' @details Gives a list of bounds on eta' y, when y verifies this inequality. The bounds depend on y through z
#' which is the residual of the projection of y on eta. Cf lemma 5.1.
#'
#' @export
get.intervals <- function(eta, y, linear.constraints, lambda, sanitize = TRUE) {
  if(class(linear.constraints) == "list") {
    LI <- lapply(linear.constraints, \(lc) get.i(eta, y, lc, lambda))
    LI <- LI[!sapply(LI, is.null)]
    if(sanitize)
      sanitize.intervals(LI)
    else 
      LI
  } else { 
    LI <- get.i(eta, y, lc, lambda)
    if(is.null(LI)) 
      list()
    else
      LI
  }
}

get.i <- function(eta, y, lc, lambda) {
  if(class(lc) != "linear.constraints")
    stop("Is this a linear constraints object?")
  A <- lc$A
  B <- lc$B
  c <- eta / sum(eta**2)
  z <- y - sum(eta*y)/sum(eta**2) * eta
  Ac <- A %*% c
  Bz <- lambda*B - A %*% z
  
  U <- suppressWarnings(min( (Bz/Ac)[Ac > 0] ))
  L <- suppressWarnings(max( (Bz/Ac)[Ac < 0] ))
  # POS <- min( Bz[Ac == 0] )
  # list(L = L, U = U, POS = POS)
  if(L < U) # les conditions calculées peuvent être contradictoire !
    c(L,U)
  else
    NULL
} 
