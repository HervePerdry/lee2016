
# p : le nombre de variables
model.list <- function(p) {
  I <- 1:p
  lapply(seq(0, 2^p-1), \(i) I[int.to.logical(i,p)])
}

int.to.logical <- function(n, p) {
  if(n == 0L) 
    return(rep(FALSE, p))
  if(n %% 2L) 
    c(TRUE, int.to.logical(n %/% 2L, p - 1L))
  else
    c(FALSE, int.to.logical(n %/% 2L, p - 1L))
}
