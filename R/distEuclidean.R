distEuclidean <-
function(x,y) {
  z <- matrix(0, nrow = nrow(x), ncol = nrow(y))
  for (k in 1:nrow(y)) {
    z[, k] <- sqrt(colSums((t(x) - y[k, ])^2))
  }
  return(z)
}
