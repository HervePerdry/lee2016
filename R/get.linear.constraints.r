#' Linear constraints for model choice
#'
#' @param X matrix of covariates
#' @param M indices of covariates kept in the model
#' @param S signs of the effects
#'
#' @details Corresponds to prop 4.2. \code{S} can contain zeros for unpenalized variables.
#' the corresponding model is selected if \eqn{A y <= lambda B}. 
#' @details If S is missing or contains \code{NA}s, then a list of linear constraints 
#' will be returned.
#'
#'
#' @export
get.linear.constraints <- function(X, M, S) {
  if(missing(S))
    S <- rep(NA_integer_, length(M))
  S <- sign.list(S)
  lapply(S, \(s) get.lc(X, M, s))
}

# version où S est un vecteur et n'est pas manquant
get.lc <- function(X, M, S) {

  M <- as.integer(M)
  S <- as.integer(S)
  if(length(M) != length(S)) 
    stop("M and S should have the same length")
  if(!all(S %in% -1:1))
    stop("S should contain only -1, 0, 1")

  # cas particulier du modèle vide
  if(length(M) == 0) {
    a <- t(X)
    A0 <- rbind(a, -a)
    B0 <- rep(1, 2*ncol(X))
    return( structure(list( A = A0, B = B0 ), class = "linear.constraints") )
  } 

  # cas général
  XM <- X[,M, drop = FALSE]
  XN <- X[,-M, drop = FALSE]

  crXM <- crossprod(XM)
  XMP <- solve( crXM, t(XM) )
  PM <- XM %*% XMP 
  ImPM <- -PM; diag(ImPM) <- 1 + diag(ImPM); # I - PM

  a <- crossprod(XN, ImPM) 
  A0 <- rbind(a, -a)
  b <- crossprod(XN, crossprod(XMP, S))
  B0 <- rbind(1 - b, 1 + b)

  A1 <- -(S * XMP)
  B1 <- matrix(-S * solve(crXM, S), ncol = 1)
  # les endroits où S = 0 produisent des conditions vides ici
  # mais on doit quand même les garder dans S pour calculer (XM' XM)^{-1} S
  # (et les conditions pour B0)
 
  A1 <- A1[ S != 0, , drop = FALSE]
  B1 <- B1[ S != 0, , drop = FALSE]  
  structure( list( A = rbind(A0, A1), B = rbind(B0, B1) ), class = "linear.constraints" )
}


