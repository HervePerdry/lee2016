
# celle ci prend un vecteur et renvoie une liste
sign.list.0 <- function(S) {
  S <- as.integer(S)
  ina <- is.na(S)
  if(all(!ina)) 
    return(list(S))
  i <- which(ina)[1]
  S[i] <- -1L
  Lm <- sign.list(S)
  S[i] <- +1L
  Lp <- sign.list(S)
  c(Lm, Lp) 
}

# cette version prend un vecteur OU une liste et renvoie une liste
sign.list <- function(S) {
  if(!is.list(S))
    S <- list(S)
  Reduce(c, lapply(S, sign.list.0))
}
