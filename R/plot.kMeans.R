plot.kMeans <-
function(X, col, pch, ...) {
  if (missing("col")) {
    col <- X$Cbest
  }
  if (missing("pch")) {
    pch <- X$Cbest
  }
  plot(X$Y, col=col, pch=pch, ...)
}
