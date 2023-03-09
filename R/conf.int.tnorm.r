#' Confidence intervals for truncated normal
#' @param x0 observed value
#' @param level
#' @param alpha
#' @param sd
#' @param low
#' @param upp
#'
#' @details this function needs improvement. 
#' @return a vector of length 2
#' @examples conf.int.tnorm(0)
#' conf.int.tnorm(0, low = -1, upp = +Inf)
#' @export
conf.int.tnorm <- function(x0, level = 0.95, alpha = 1 - level, sd = 1, low = -Inf, upp = +Inf) {
  # on cherche mu tel que ptnorm(x0, mu, ...) = 0.025, 0.975
  f <- \(mu, alpha) ptnorm(x0, mu, sd, low, upp) - alpha
  U <- uniroot(f, x0 + c(-5, 5)*sd, alpha = alpha/2)$root
  L <- uniroot(f, x0 + c(-5, 5)*sd, alpha = 1 - alpha/2)$root
  c(L, U)
}
