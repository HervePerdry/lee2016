

sanitize.intervals <- function(LI) {
  if(length(LI) == 0) return(LI)
  low <- sapply(LI, \(x) x[1])
  upp <- sapply(LI, \(x) x[2])
  x <- sanitize.low.upp(low, upp)
  mapply(c, x$low, x$upp, SIMPLIFY = FALSE)
}

sanitize.low.upp <- function(low, upp) {
  fl <- TRUE
  while(fl) {
    fl <- FALSE
    n <- length(low)
    for(i in 1:n) {
      # les indices des intervales dont une borne au moins tombe dans le i-eme intervale
      J <- which((low[i] <= low & low <= upp[i]) | (low[i] <= upp & upp <= upp[i]))
      if(length(J) == 1) next # il n'y a que i
      L <- min(low[J])
      U <- max(upp[J])
      low <- c(L, low[-J])
      upp <- c(U, upp[-J])
      fl <- TRUE
      break
    }
  }
  o <- order(low)
  list(low = low[o], upp = upp[o])
}
