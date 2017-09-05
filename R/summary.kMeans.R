summary.kMeans <-
function(X) {
  cat("K-Means clustering for", X$Xname, "\n\n")
  cat("Clusters to be detected:", X$k, "\n")
  cat("Cluster sizes detected:", table(X$Cbest), "\n\n") 
  
  cat("Number of runs:", X$m, "\n")
  cat("Status of best run:", X$StatusAll[X$Best], "\n\n")
  
  cat("Criterion value:", X$ObjBest, "\n")
  cat("Summary of criterion values:\n")
  cat("Min:", min(X$ObjAll), "\n")
  cat("Q1:", quantile(X$ObjAll)[2], "\n")
  cat("Mean:", mean(X$ObjAll), "\n")
  cat("Q3:", quantile(X$ObjAll)[4], "\n")
  cat("Max:", max(X$ObjAll))
}
