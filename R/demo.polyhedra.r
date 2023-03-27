#' A small demo of the polyhedra constraint result
#' 
#' @param x
#' @param y
#' @param lambda

#' @examples set.seed(3)
#' x <- cbind( c(rep(-1, 20), rep(1,20)), runif(40, -1, 1) )
#' x[,2] <- x[,2] + 0.4 * x[,1] 
#' y <- x %*% c(1,2) + rnorm(40)
#' y <- y - mean(y)
#' demo.polyhedra(x, y, 25)
#' @export
demo.polyhedra <- function(x, y, lambda, xlim, ylim, plot = TRUE) {

  p <- 2
  # faire le dessin dans un repère orthonormé
  fit <- lm(x[,2] ~ x[,1] - 1)

  sx1 <- sqrt(sum(x[,1]**2))
  sx2 <- sqrt(sum(fit$residuals**2))
  XX <- cbind(x[,1]/sx1, fit$residuals/sx2)
  if(!plot) return(invisible(XX))

  a <- lm(x[,2] ~ XX - 1)$coeff
  # dans le plan < XX >
  # coordonnees de x[,1] = (sx1, 0)
  # coordonnees de x[,2] = (a1, a2)
  m <- max(sx1, a)

  if(missing(xlim)) xlim <- c(-5,5)*m
  if(missing(ylim)) ylim <- c(-5,5)*m
  plot(0, 0, type = "n", xlim = xlim, ylim = ylim, xlab = "", ylab = "")

  # les deux vecteurs x1, x2
  arrows(0, 0, sx1, 0, length = 0.1) # x1
  arrows(0, 0, a[1], a[2], length = 0.1) # x2

  # la projection de y
  yhat <- lm(y ~ XX - 1)$coeff
  points(yhat[1], yhat[2])

  # enumeration des modèles
  list.M <- model.list(p)
  for(M in list.M) {
    for(S in sign.list( rep(NA, length(M)) )) {
      # on trace le polyhedre qui correspond à la contrainte M, S
      lic <- get.linear.constraints(x, M, S)
      A <- lic[[1]]$A %*% XX
      B <- lic[[1]]$B * lambda
      draw.borders(A, B, lty = 2, 100*m)
    }
  }
  return(invisible(XX))
}
