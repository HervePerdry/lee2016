% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demo.polyhedra.r
\name{demo.polyhedra}
\alias{demo.polyhedra}
\title{A small demo of the polyhedra constraint result}
\usage{
demo.polyhedra(x, y, lambda, xlim, ylim, plot = TRUE)
}
\description{
A small demo of the polyhedra constraint result
}
\examples{
set.seed(3)
x <- cbind( c(rep(-1, 20), rep(1,20)), runif(40, -1, 1) )
x[,2] <- x[,2] + 0.4 * x[,1] 
y <- x \%*\% c(1,2) + rnorm(40)
y <- y - mean(y)
demo.polyhedra(x, y, 25)
# an example with colinearity
x <- cbind(x, x[,1] - x[,2])
demo.polyhedra(x, y, 25)
}
