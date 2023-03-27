#' @export
demo.tnorm <- function(x0, mu, sd, low, upp, CI = round(conf.int.tnorm(x0, sd = sd, low = low, upp = upp ),2)) {
  x1 <- min(CI[1] - .5*diff(CI), x0 - 5*sd)
  x2 <- max(CI[2] + .5*diff(CI), x0 + 5*sd)
  x <- seq(x1, x2, length = 501)
  y <- dtnorm(x, mean = mu, sd = sd, low = low, upp = upp)
  plot( x, dtnorm(x, mean = mu, sd = sd, low = low, upp = upp), type = "n", ylab = "density" )
  lines( CI, c(0,0), lwd = 10, col = "lightblue")
  lines(x, y)
  points(mu, 0, pch = 2, cex = 1.4)
  segments( x0, 0, x0, dtnorm(x0, mean = mu, sd = sd, low = low, upp = upp), lty = 2)
  if(mu == CI[1]) {
    x <- seq(x0, 5, length = 101) 
    y <- dtnorm(x, mean = mu, sd = sd, low = low, upp = upp)
    polygon( c(x, rev(x)), c(y, rep(0,101)), border = NA, col = "red")
  } 
  if(mu == CI[2]) {
    x <- seq(-5, x0, by = 0.01)
    y <- dtnorm(x, mean = mu, sd = sd, low = low, upp = upp)
    polygon( c(x, rev(x)), c(y, rep(0,length(x))), border = NA, col = "red")
  } 
}

