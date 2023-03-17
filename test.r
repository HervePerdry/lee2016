require(lee2016)
set.seed(3)
x <- cbind( c(rep(-1, 20), rep(1,20)), runif(40, -1, 1) )
x[,2] <- x[,2] + 0.4 * x[,1] 
y <- x %*% c(1,2) + rnorm(40)
y <- y - mean(y)

L <- get.linear.constraints(x, c(1,2))
eta <- get.eta(x, c(1,2))
get.intervals(eta[,1], y, L, 30)
get.intervals(eta[,2], y, L, 30)

# faire le dessin dans un repère orthonormé
fit <- lm(x[,2] ~ x[,1] - 1)

sx1 <- sqrt(sum(x[,1]**2))
sx2 <- sqrt(sum(fit$residuals**2))
XX <- cbind(x[,1]/sx1, fit$residuals/sx2)
a <- lm(x[,2] ~ XX - 1)$coeff

# dans le plan < XX >
# coordonnees de x[,1] = (sx1, 0)
# coordonnees de x[,2] = (a1, a2)
m <- max(sx1, a)

plot(0, 0, type = "n", xlim = c(-5,5)*m, ylim = c(-5,5)*m)

arrows( 0, 0, sx1, 0, length = 0.1) # x1
arrows( 0, 0, a[1], a[2], length = 0.1) # x2

f <- function(M, S, lambda) {
  lic <- get.linear.constraints(x, M, S)
  A <- lic[[1]]$A %*% XX
  B <- lic[[1]]$B * lambda 
  lee2016:::draw.borders(A, B, lty = 2, 100*m)
}

lambda <- 15 
f( c(), c(), lambda)
f( c(1,2), c( 1,  1), lambda)
f( c(1,2), c( 1, -1), lambda)
f( c(1,2), c(-1,  1), lambda)
f( c(1,2), c(-1, -1), lambda)

yhat <- lm(y ~ XX - 1)$coeff
points(yhat[1], yhat[2])

plot.eta <- function(eta, ...) {
  cx <- lm(eta ~ XX - 1)$coeff
  arrows(0, 0, cx[1], cx[2], length = 0.1)
  if(cx[1] == 0)
    abline(v = 0, ...)
  else 
    abline(0, cx[2]/cx[1], ...)
}

pr.eta <- lm(eta[,1] ~ XX - 1)$coeff
gamma <- c(pr.eta[2], -pr.eta[1]) # orthogonal pr.eta
kappa <- sum(yhat * gamma)/sum(gamma**2)
LI <- get.intervals(eta[,1], y, get.linear.constraints(x, c(1,2)), lambda )

for(I in LI) {
  I <- I / sum(pr.eta**2)
  I[ I == Inf ] <- 100*m
  I[ I == -Inf ] <- -100*m
  lines(kappa * gamma[1] + pr.eta[1] * I, kappa * gamma[2] + pr.eta[2] * I)
}

