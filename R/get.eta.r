
#' @export
get.eta <- function(X, M) {
  XM <- X[,M, drop = FALSE]
  crXM <- crossprod(XM)
  XMP <- solve( crXM, t(XM) )
  t(XMP)
}
