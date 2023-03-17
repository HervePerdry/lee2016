

draw.borders <- function(A, B, MAX = 100, ...) {
  for(i in 1:nrow(A))
    draw.borders.i(A, B, i, MAX, ...)
}

draw.borders.i <- function(A, B, i, MAX = 100, ...) {
  u <- A[i,]
  b <- B[i]
  v <- c(-u[2], u[1]) # orthogonal à u
  # les solution à (u x = b) sont de la forme
  # x = b / ||u||^2 t(u) + T * v

  # inégalités sur T
  Ac <- A[-i,,drop = FALSE] %*% v
  B2 <- B[-i] - b * A[-i,,drop = FALSE] %*% u / sum(u**2)
  
  if(any(Ac == 0))
    if( B2[Ac == 0] < 0 ) stop("problème ?")
  LT <- suppressWarnings(max( (B2/Ac)[Ac < 0] ))
  UT <- suppressWarnings(min( (B2/Ac)[Ac > 0] ))
  LT <- ifelse(is.infinite(LT), -MAX, LT)
  UT <- ifelse(is.infinite(UT),  MAX, UT)
  beg <- b / sum(u**2) * u + LT * v
  end <- b / sum(u**2) * u + UT * v
  lines(x = c(beg[1], end[1]), y = c(beg[2], end[2]), ...)
}
