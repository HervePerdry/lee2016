#' @name TNorm
#' @title Truncated normal
#' @param x
#' @param q
#' @param p
#' @param n
#' @param mean
#' @param sd
#' @param low
#' @param upp
NULL

#' @rdname TNorm
#' @export
dtnorm <- function(x, mean = 0, sd = 1, low = -Inf, upp = +Inf) {
  check.interval(low, upp)
  A <- sum(pnorm(upp, mean, sd) - pnorm(low, mean, sd))
  I <- find.interval(x, low, upp)
  ifelse( is.na(I), 0, dnorm(x, mean, sd)/A)
}

#' @rdname TNorm
#' @export
ptnorm <- function(q, mean = 0, sd = 1, low = -Inf, upp = +Inf) {
  check.interval(low, upp)
  a <- pnorm(upp, mean, sd) - pnorm(low, mean, sd)
  A <- sum(a)
  we <- sapply(q, \(x) sum(a[upp < x]))
  I <- find.interval(q, low, upp)
  (we + ifelse(is.na(I), 0, pnorm(q, mean, sd) - pnorm(low[I], mean, sd)))/A
}

#' @rdname TNorm
#' @export
qtnorm <- function(p, mean = 0, sd = 1, low = -Inf, upp = +Inf) {
  check.interval(low, upp)
  a <- pnorm(upp, mean, sd) - pnorm(low, mean, sd)
  A <- sum(a)
  csa <- c(0, cumsum(a)/A)
  f0 <- function(x) {
    I <- which(x <= csa)[1]
    qnorm( pnorm(low[I-1], mean, sd) + A * (x - csa[I-1]), mean, sd )
  }
  sapply(p, f0)
}

#' @rdname TNorm
#' @export
rtnorm <- function(n, mean = 0, sd = 1, low = -Inf, upp = +Inf) {
  qtnorm(runif(n), mean, sd, low, upp)
}

check.interval <- function(low, upp) {
  if(!all(low < upp))
    stop("lower bounds shoud be inferior to upper bounds")
  n <- length(low)
  if(n > 1) {
    if(any(diff(low) < 0) | any(diff(upp) < 0))
     stop("specify intervals from left to right")
    if(any(upp[-n] >= low[-1]))
     stop("specify disjoint intervals")
  }
}

find.interval <- function(x, low, upp) {
  # fonction non vectorisée
  f0 <- function(x) {
    w <- which(low <= x & x <= upp)
    if(length(w) == 0) 
      NA
    else if(length(w) == 1)
      w
    else
      stop("low / upp should specify disjoint intervals")
  }  
  sapply(x, f0)
}

