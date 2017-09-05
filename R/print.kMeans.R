print.kMeans <-
function(X) {
  cat("K-Means clustering for", X$Xname, "\n\n")
  
  cat("Number of runs:", X$m, "\n")
  cat("Status of best run:", X$StatusAll[X$Best])
}
