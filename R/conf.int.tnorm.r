#' Confidence intervals for truncated normal
#' @param x0 observed value
#' @param level
#' @param alpha
#' @param sd
#' @param low
#' @param upp
#'
#' @details this function finds a confidence interval by solving the equation
#' \code{ptnorm(x0, mu, sd, low, upp) = alpha/2} or \code{1 - alpha/2}, \code{mu}
#' being the unknown. 
#' @details It corresponds to Theorem 6.1.
#' @return a vector of length 2
#' @examples conf.int.tnorm(0)
#' conf.int.tnorm(0, low = -1, upp = +Inf)
#' @export
conf.int.tnorm <- function(x0, level = 0.95, alpha = 1 - level, sd = 1, low = -Inf, upp = +Inf) {
  f <- function(mu, alpha) ptnorm(x0, mu, sd, low, upp) - alpha
  sol <- function(a) { 
    k <- 4;
    repeat {
      if(is.infinite(k))
        stop("Failed") 
      R <- try( uniroot(f, x0 + c(-1, 1)*k, alpha = a)$root, TRUE )
      if(class(R) != "numeric") 
        k <- k*2
      else
        break
    }
    R
  }
  U <- sol(alpha/2)
  L <- sol(1 - alpha/2)
  c(L, U)
}

